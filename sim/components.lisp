;;;;;;;;;;;;;;;
;; Batteries ;;
;;;;;;;;;;;;;;;

(defun make-battery (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))

         (interval (or (plist-get plist :interval) battery-interval))

         (config  (plist-get plist :config))
         (config-alist `(,@config ,@battery-defaults))

         (power-symbol  (power-symbol-from-id  id))
         (energy-symbol (energy-symbol-from-id id))

         (capacity    (alist-get 'capacity    config-alist))
         (initial-soc (alist-get 'initial-soc config-alist))

         (soc-symbol (soc-symbol-from-id id))

         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))
         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (dc-power-bounds-symbol (active-power-bounds-symbol-from-id id))

         (soc-lower (alist-get 'soc-lower config-alist))
         (soc-upper (alist-get 'soc-upper config-alist))

         (is-healthy (is-healthy-battery config-alist))

         (type-val (alist-get 'type config-alist))
         (voltage-val (alist-get 'voltage config-alist))
         (relay-state-val (alist-get 'relay-state config-alist))
         (bounce-lifetime (ceiling (min 1 (* 3 (/ interval 1000.0)))))

         ;; Single closure that returns a materialized alist of values.
         ;; No expressions or callables get stored in alist entries.
         (data-fn
          (lambda (_)
            (let ((power (if is-healthy (symbol-value power-symbol) 0.0)))
              (list (cons 'id id)
                    (cons 'soc (symbol-value soc-symbol))
                    (cons 'soc-lower soc-lower)
                    (cons 'soc-upper soc-upper)
                    (cons 'capacity capacity)
                    (cons 'power power)
                    (cons 'voltage voltage-val)
                    (cons 'type type-val)
                    (cons 'component-state (power->component-state power))
                    (cons 'relay-state relay-state-val)
                    (cons 'bounds (symbol-value dc-power-bounds-symbol))))))

         (battery
          `((category . battery)
            (name     . ,(format "bat-%s" id))
            (id       . ,id)
            (power-symbol . ,(when is-healthy power-symbol))
            (bounds-symbol . ,dc-power-bounds-symbol)
            (rated-lower . ,rated-lower)
            (rated-upper . ,rated-upper)
            (is-healthy . ,is-healthy)
            (stream . ((interval . ,interval) (data . ,data-fn))))))

    (set dc-power-bounds-symbol (bounds/make-container rated-lower rated-upper))

    (log.trace (format "Adding battery %s. Healthy: %s" id is-healthy))

    (when (not (boundp power-symbol))
      (set power-symbol 0.0)
      (set energy-symbol 0.0)
      (set soc-symbol initial-soc))

    ;; Per-component bounds updater — computes new SOC-dependent bounds and
    ;; clamps power to them every `interval` ms.
    (every
     :milliseconds interval
     :call (lambda ()
             (let ((soc (symbol-value soc-symbol)))
               (set dc-power-bounds-symbol
                    (bounds/add-raw
                     (bounds/drop-expired (symbol-value dc-power-bounds-symbol))
                     (dt:now)
                     ;; lower bound
                     (if (< (- soc soc-lower) 10.0)
                         (* rated-lower
                            (bounded-exp-decay (+ soc-lower 10.0)
                                               soc-lower
                                               soc
                                               1.2
                                               0.3))
                         rated-lower)
                     ;; upper bound
                     (if (< (- soc-upper soc) 10.0)
                         (* rated-upper
                            (bounded-exp-decay (- soc-upper 10.0)
                                               soc-upper
                                               soc
                                               1.2
                                               0.3))
                         rated-upper)
                     bounce-lifetime))
               ;; keep power within the freshly-updated bounds
               (set power-symbol
                    (bounds/limit-power (symbol-value dc-power-bounds-symbol)
                                        (symbol-value power-symbol))))))

    ;; Energy / SOC updater — driven by the global state tick.
    (setq active-timers
          (cons (run-with-timer
                 (/ state-update-interval-ms 1000.0)
                 (/ state-update-interval-ms 1000.0)
                 (lambda ()
                   (set energy-symbol
                        (+ (symbol-value energy-symbol)
                           (* (symbol-value power-symbol)
                              (/ state-update-interval-ms
                                 (* 60.0 60.0 1000.0)))))
                   (set soc-symbol
                        (+ initial-soc
                           ;; limit to 1 decimal place
                           (/ (fround
                               (* 1000.0 (/ (symbol-value energy-symbol) capacity)))
                              10.0)))))
                active-timers))

    (add-to-components-alist battery)

    battery))

;;;;;;;;;;;;;;;
;; Inverters ;;
;;;;;;;;;;;;;;;

(defun make-battery-inverter (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) inverter-interval))

         (config (plist-get plist :config))
         (config-alist `(,@config ,@battery-inverter-defaults))

         (successors (plist-get plist :successors))

         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))

         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (is-healthy (is-healthy-inverter config-alist))

         (reactive-power-symbol (reactive-power-symbol-from-id id))
         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (reactive-bounds-check-func-symbol (reactive-bounds-check-func-symbol-from-id id))
         (active-power-bounds-symbol (active-power-bounds-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))
         (set-reactive-power-func-symbol (set-reactive-power-func-symbol-from-id id))
         (reset-power-func-symbol (reset-power-func-symbol-from-id id))

         ;; Aggregate real power: sum of healthy successors' power
         ;; symbols. TODO: batteries' DC power != AC apparent power —
         ;; this tracks only real power.
         (compute-power (or (make-power-fn successors)
                            (lambda () 0.0)))
         (battery-bounds-check (make-battery-bounds-check-fn successors))

         ;; Single closure that materializes the stream data at call
         ;; time — no per-field callables or expressions in the alist.
         (data-fn
          (lambda (_)
            (let* ((power (if is-healthy (funcall compute-power) 0.0))
                   (rp (symbol-value reactive-power-symbol)))
              (list (cons 'id id)
                    (cons 'power power)
                    (cons 'per-phase-power (calc-per-phase-power power))
                    (cons 'reactive-power rp)
                    (cons 'per-phase-reactive-power (calc-per-phase-power rp))
                    (cons 'voltage voltage-per-phase)
                    (cons 'current (ac-current-from-per-phase-power
                                    (calc-apparent-power power rp)))
                    (cons 'component-state (power->component-state power))
                    (cons 'bounds (symbol-value active-power-bounds-symbol))))))

         (inverter
          `((category . inverter)
            (type     . battery)
            (name     . ,(format "inv-bat-%s" id))
            (id       . ,id)
            (bounds-symbol . ,active-power-bounds-symbol)
            (rated-lower . ,rated-lower)
            (rated-upper . ,rated-upper)
            (data-fn . ,data-fn)
            (stream . ((interval . ,interval) (data . ,data-fn))))))

    (log.trace (format "Adding battery inverter %s. Healthy: %s" id is-healthy))

    (set reactive-power-symbol 0.0)

    (set reactive-bounds-check-func-symbol
         (if is-healthy
             (lambda (reactive-power)
               (let ((abs-power (abs (funcall compute-power))))
                 (and (>= reactive-power (* -0.35 abs-power))
                      (<= reactive-power (* 0.35 abs-power)))))
             (progn (log.error "inverter is unhealthy")
                    (lambda (_reactive-power) nil))))

    (set bounds-check-func-symbol
         (if is-healthy
             (lambda (power)
               (and (funcall battery-bounds-check power)
                    (bounds/contains (symbol-value active-power-bounds-symbol) power)))
             (progn (log.error "inverter is unhealthy")
                    (lambda (_power) nil))))

    (set reset-power-func-symbol
         (lambda ()
           (dolist (battery successors)
             (when-let ((sym (alist-get 'power-symbol battery)))
               (set sym 0.0)))))

    (set set-power-func-symbol
         (let* ((healthy-power-syms
                 (seq-filter 'identity
                             (mapcar (lambda (b)
                                       (when (alist-get 'is-healthy b)
                                         (alist-get 'power-symbol b)))
                                     successors)))
                (num-batteries (length healthy-power-syms)))
           (if (> num-batteries 0)
               (lambda (power)
                 (let ((share (/ power num-batteries)))
                   (dolist (sym healthy-power-syms)
                     (let ((prev (symbol-value sym)))
                       (unless (equal share prev)
                         (log.info (format "Setting power of battery %s to %s W (was: %s W)"
                                           sym share prev)))
                       (set sym share)))))
               (lambda (_power)
                 (log.error "Can't set power: no healthy batteries")
                 nil))))

    (set set-reactive-power-func-symbol
         (if is-healthy
             (lambda (reactive-power)
               (log.info (format
                          "Setting reactive power of inverter %s to %s VAR (was: %s VAR)"
                          id reactive-power (symbol-value reactive-power-symbol)))
               (set reactive-power-symbol reactive-power))
             (lambda (_reactive-power)
               (log.error "Can't set reactive power: inverter is unhealthy")
               nil)))

    (set active-power-bounds-symbol (bounds/make-container rated-lower rated-upper))

    (when is-healthy
      (every
       :milliseconds 1000
       :call (lambda ()
               (let* ((measured-power (funcall compute-power))
                      (active-power-bounds (symbol-value active-power-bounds-symbol)))
                 (set active-power-bounds-symbol
                      (bounds/drop-expired active-power-bounds))
                 (let ((limited-power (bounds/limit-power active-power-bounds measured-power)))
                   (unless (equal limited-power measured-power)
                     (log.debug (format "Limited power for inverter %s: %s W"
                                        id limited-power))
                     (funcall (symbol-value set-power-func-symbol) limited-power)))))))

    (add-to-components-alist inverter)
    (connect-successors id successors)
    inverter))

(defun make-solar-inverter (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) inverter-interval))

         (sunlight% (plist-get plist :sunlight%))

         (config (plist-get plist :config))
         (config-alist `(,@config ,@solar-inverter-defaults))

         (power-symbol  (power-symbol-from-id id))
         (reactive-power-symbol (reactive-power-symbol-from-id id))
         (min-power-symbol (power-symbol-from-id (format "min-%s" id)))
         (active-power-bounds-symbol (active-power-bounds-symbol-from-id id))

         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))
         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (is-healthy (is-healthy-inverter config-alist))

         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (reactive-bounds-check-func-symbol (reactive-bounds-check-func-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))
         (set-reactive-power-func-symbol (set-reactive-power-func-symbol-from-id id))
         (reset-power-func-symbol (reset-power-func-symbol-from-id id))

         (min-available-power (* rated-lower (/ sunlight% 100.0)))

         (data-fn
          (lambda (_)
            (let* ((power (if is-healthy (symbol-value power-symbol) 0.0))
                   (rp (symbol-value reactive-power-symbol)))
              (list (cons 'id id)
                    (cons 'power power)
                    (cons 'reactive-power rp)
                    (cons 'per-phase-power (calc-per-phase-power power))
                    (cons 'per-phase-reactive-power (calc-per-phase-power rp))
                    (cons 'voltage voltage-per-phase)
                    (cons 'current (ac-current-from-per-phase-power
                                    (calc-apparent-power power rp)))
                    (cons 'component-state (power->component-state power))
                    (cons 'bounds (symbol-value active-power-bounds-symbol))))))

         (inverter
          `((category . inverter)
            (type     . pv)
            (name     . ,(format "inv-pv-%s" id))
            (id       . ,id)
            (power-symbol . ,(when is-healthy power-symbol))
            (bounds-symbol . ,active-power-bounds-symbol)
            (rated-lower . ,rated-lower)
            (rated-upper . ,rated-upper)
            (data-fn . ,data-fn)
            (stream . ((interval . ,interval) (data . ,data-fn))))))

    (log.trace (format "Adding solar inverter %s. Healthy: %s" id is-healthy))

    (when (not (boundp min-power-symbol))
      (set min-power-symbol rated-lower))

    (set power-symbol (max (symbol-value min-power-symbol) min-available-power))
    (set reactive-power-symbol 0.0)

    (set bounds-check-func-symbol
         (if is-healthy
             (lambda (power)
               (bounds/contains (symbol-value active-power-bounds-symbol) power))
             (progn (log.error "inverter is unhealthy")
                    (lambda (_power) nil))))

    (set reactive-bounds-check-func-symbol
         (if is-healthy
             (lambda (reactive-power)
               (let ((abs-power (abs (symbol-value power-symbol))))
                 (and (>= reactive-power (* -0.35 abs-power))
                      (<= reactive-power (* 0.35 abs-power)))))
             (progn (log.error "inverter is unhealthy")
                    (lambda (_reactive-power) nil))))

    (set reset-power-func-symbol
         (lambda ()
           (set min-power-symbol rated-lower)
           (set power-symbol (max rated-lower min-available-power))))

    (set set-power-func-symbol
         (if is-healthy
             (lambda (power)
               (set min-power-symbol (max power min-available-power))
               (if (< power min-available-power)
                   (progn
                     (log.info (format
                                "Given power %s W is not available for inverter %s.  Limiting to %s W."
                                power id min-available-power))
                     (set power-symbol min-available-power))
                   (progn
                     (log.info (format "Setting power of inverter %s to %s W (was: %s W)"
                                       id power (symbol-value power-symbol)))
                     (set power-symbol power))))
             (lambda (_power)
               (log.error "Can't set power: inverter is unhealthy")
               nil)))

    (set set-reactive-power-func-symbol
         (if is-healthy
             (lambda (reactive-power)
               (log.info (format "Setting reactive power of inverter %s to %s VAR (was: %s VAR)"
                                 id reactive-power (symbol-value reactive-power-symbol)))
               (set reactive-power-symbol reactive-power))
             (lambda (_reactive-power)
               (log.error "Can't set reactive power: inverter is unhealthy")
               nil)))

    (set active-power-bounds-symbol (bounds/make-container rated-lower rated-upper))

    (when is-healthy
      (every
       :milliseconds 1000
       :call (lambda ()
               (let* ((measured-power (symbol-value power-symbol))
                      (active-power-bounds (symbol-value active-power-bounds-symbol)))
                 (set active-power-bounds-symbol
                      (bounds/drop-expired active-power-bounds))
                 (let ((limited-power (bounds/limit-power active-power-bounds measured-power)))
                   (unless (equal limited-power measured-power)
                     (log.debug (format "Limited power for inverter %s: %s W"
                                        id limited-power))
                     (funcall (symbol-value set-power-func-symbol) limited-power)))))))

    (add-to-components-alist inverter)
    inverter))


;;;;;;;;;;;;
;; Meters ;;
;;;;;;;;;;;;

;; Resolve a `:power` / `:per-phase-power` plist value into a 0-arg
;; closure. Accepts a number (literal), a symbol (looked up at call time
;; via `symbol-value`), or nil. Expressions are no longer supported — the
;; old eval-based model is gone.
(defun plist-value-fn (val)
  (cond
   ((null val) nil)
   ((numberp val) (lambda () val))
   ((symbolp val) (lambda () (symbol-value val)))
   ((consp val) (lambda () val))
   (t (error (format "plist-value-fn: unsupported value: %s" val)))))

(defun make-meter (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) meter-interval))
         (power (plist-get plist :power))
         (per-phase-power (plist-get plist :per-phase-power))
         (reactive-power (plist-get plist :reactive-power))
         (per-phase-reactive-power (plist-get plist :per-phase-reactive-power))

         (config (plist-get plist :config))
         (config-alist `(,@config ,@meter-defaults))

         (successors (plist-get plist :successors))
         (hidden (plist-get plist :hidden))
         (is-healthy (is-healthy-meter config-alist))

         (_ (when (and power per-phase-power)
              (error (format "Can't use meter %s with both :power and :per-phase-power set" id))))
         (_ (when (and reactive-power per-phase-reactive-power)
              (error (format "Can't use meter %s with both :reactive-power and :per-phase-reactive-power set" id))))

         ;; Each of these is a 0-arg closure (or nil if no source) that
         ;; returns the current per-phase / total / reactive value.
         (pp-fn (cond
                 ((not is-healthy) nil)
                 (per-phase-power (plist-value-fn per-phase-power))
                 (power nil)
                 (:else (make-per-phase-fn successors))))
         (p-fn (cond
                ((not is-healthy) nil)
                (per-phase-power (lambda () (seq-reduce '+ (funcall pp-fn) 0.0)))
                (power (plist-value-fn power))
                (pp-fn (lambda () (seq-reduce '+ (funcall pp-fn) 0.0)))))
         (pp-r-fn (cond
                   ((not is-healthy) nil)
                   (per-phase-reactive-power (plist-value-fn per-phase-reactive-power))
                   (reactive-power nil)
                   (:else (make-per-phase-reactive-fn successors))))
         (p-r-fn (cond
                  ((not is-healthy) nil)
                  (per-phase-reactive-power (lambda () (seq-reduce '+ (funcall pp-r-fn) 0.0)))
                  (reactive-power (plist-value-fn reactive-power))
                  (pp-r-fn (lambda () (seq-reduce '+ (funcall pp-r-fn) 0.0)))))

         ;; Derived per-phase reactive/power for current computation
         ;; when the caller passed a scalar.
         (data-fn
          (when is-healthy
            (lambda (_)
              (let* ((ptot (if p-fn (funcall p-fn) 0.0))
                     (rtot (if p-r-fn (funcall p-r-fn) 0.0))
                     (pp (cond (pp-fn (funcall pp-fn))
                               (p-fn (calc-per-phase-power ptot))
                               (:else (list 0.0 0.0 0.0))))
                     (pp-r (cond (pp-r-fn (funcall pp-r-fn))
                                 (p-r-fn (calc-per-phase-power rtot))
                                 (:else (list 0.0 0.0 0.0)))))
                (list (cons 'id id)
                      (cons 'power ptot)
                      (cons 'per-phase-power pp)
                      (cons 'reactive-power rtot)
                      (cons 'per-phase-reactive-power pp-r)
                      (cons 'current (ac-current-from-per-phase-power
                                      (calc-per-phase-apparent-power pp pp-r)))
                      (cons 'voltage voltage-per-phase)
                      (cons 'component-state (alist-get 'component-state config-alist)))))))

         (meter
          `((category . meter)
            (name     . ,(format "meter-%s" id))
            (id       . ,id)
            (hidden   . ,hidden)
            (data-fn  . ,data-fn)
            (stream   . ((interval . ,interval) (data . ,data-fn))))))

    (log.trace (format "Adding meter %s" id))

    (unless hidden
      (add-to-components-alist meter)
      (connect-successors id successors))
    meter))


;;;;;;;;;;;;;;;;;
;; EV Chargers ;;
;;;;;;;;;;;;;;;;;

(defun make-ev-charger (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) ev-charger-interval))
         (config (plist-get plist :config))
         (config-alist `(,@config ,@ev-charger-defaults))

         (power-symbol  (power-symbol-from-id id))
         (energy-symbol (energy-symbol-from-id id))

         (capacity    (alist-get 'capacity    config-alist))
         (initial-soc (alist-get 'initial-soc config-alist))

         (soc-symbol (soc-symbol-from-id id))

         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))
         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (incl-lower-symbol (inclusion-lower-symbol-from-id id))
         (incl-upper-symbol (inclusion-upper-symbol-from-id id))

         (soc-lower (alist-get 'soc-lower config-alist))
         (soc-upper (alist-get 'soc-upper config-alist))

         (is-healthy (is-healthy-ev-charger config-alist))

         (min-ev-power (* 6.0 3 220.0))

         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))
         (reset-power-func-symbol (reset-power-func-symbol-from-id id))

         ;; Closure that updates incl-upper-symbol from the current soc —
         ;; invoked once synchronously below and again on every state tick.
         (update-incl-upper
          (lambda ()
            (set incl-upper-symbol
                 (if (< (- soc-upper (symbol-value soc-symbol)) 10.0)
                     (* rated-upper
                        (bounded-exp-decay (- soc-upper 10.0)
                                           soc-upper
                                           (symbol-value soc-symbol)
                                           1.2
                                           0.3))
                     rated-upper))))

         (data-fn
          (lambda (_)
            (let ((power (if is-healthy (symbol-value power-symbol) 0.0)))
              (list (cons 'id id)
                    (cons 'power power)
                    (cons 'current (ac-current-from-power power))
                    (cons 'voltage voltage-per-phase)
                    (cons 'component-state (power->ev-component-state power))
                    (cons 'cable-state (alist-get 'cable-state config-alist))
                    (cons 'inclusion-lower 0.0)
                    (cons 'inclusion-upper rated-upper)))))

         (ev-charger
          `((category . ev-charger)
            (name     . ,(format "ev-charger-%s" id))
            (id       . ,id)
            (power-symbol . ,(when is-healthy power-symbol))
            (data-fn . ,data-fn)
            (stream . ((interval . ,interval) (data . ,data-fn))))))

    (log.trace (format "Adding ev-charger %s. Healthy: %s" id is-healthy))

    (when (not (boundp power-symbol))
      (set power-symbol 0.0)
      (set energy-symbol 0.0)
      (set soc-symbol initial-soc))

    ;; initial incl-upper for the newly-initialized soc
    (funcall update-incl-upper)
    (add-to-components-alist ev-charger)

    (setq active-timers
          (cons (run-with-timer
                 (/ state-update-interval-ms 1000.0)
                 (/ state-update-interval-ms 1000.0)
                 (lambda ()
                   (set energy-symbol
                        (+ (symbol-value energy-symbol)
                           (* (symbol-value power-symbol)
                              (/ state-update-interval-ms
                                 (* 60.0 60.0 1000.0)))))
                   (set soc-symbol
                        (+ initial-soc
                           (/ (fround
                               (* 1000.0 (/ (symbol-value energy-symbol) capacity)))
                              10.0)))
                   (funcall update-incl-upper)
                   (cond ((< (symbol-value power-symbol) 0.0)
                          (set power-symbol 0.0))
                         ((> (symbol-value power-symbol) (symbol-value incl-upper-symbol))
                          (set power-symbol (symbol-value incl-upper-symbol))))))
                active-timers))

    (set bounds-check-func-symbol
         (if is-healthy
             (lambda (power) (<= rated-lower power rated-upper))
             (progn (log.error "ev-charger is unhealthy")
                    (lambda (_power) nil))))

    (set reset-power-func-symbol
         (lambda () (set power-symbol 0.0)))

    (set set-power-func-symbol
         (if is-healthy
             (lambda (power)
               (if (< power min-ev-power)
                   (progn
                     (log.info (format
                                "Given power %s W is too low for ev-charger %s.  Not charging."
                                power id))
                     (set power-symbol 0.0))
                   (progn
                     (log.info (format "Setting power of ev-charger %s to %s W (was: %s W)"
                                       id power (symbol-value power-symbol)))
                     (set power-symbol power))))
             (lambda (_power)
               (log.error "Can't set power: ev-charger is unhealthy")
               nil)))

    ev-charger))

;;;;;;;;;
;; CHP ;;
;;;;;;;;;

(defun make-chp (&rest plist)
  (let ((id (or (plist-get plist :id) (get-comp-id)))
        (chp
         `((category . chp)
           (name     . ,(format "chp-%s" id))
           (id       . ,id))))
    (log.trace (format "Adding chp %s" id))

    (add-to-components-alist chp)
    chp))


;;;;;;;;;;
;; Grid ;;
;;;;;;;;;;

(defun make-grid (&rest plist)
  (let ((id (or (plist-get plist :id) (get-comp-id)))
        (successors (plist-get plist :successors))
        (rated-fuse-current (plist-get plist :rated-fuse-current))
        (grid
         `((category . grid-connection-point)
           (id       . ,id)
           (name     . "grid-connection-point")
           (rated-fuse-current . ,rated-fuse-current))))

    (log.trace (format "Adding grid connection %s" id))

    (add-to-components-alist grid)
    (connect-successors id successors)
    grid))
