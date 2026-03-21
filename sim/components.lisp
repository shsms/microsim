;;;;;;;;;;;;;;;
;; Batteries ;;
;;;;;;;;;;;;;;;

(defmacro battery-data-maker (data-alist defaults-alist)
  (component-data-maker data-alist
                        defaults-alist
                        '(id soc soc-upper soc-lower
                          capacity power voltage type
                          component-state relay-state
                          inclusion-lower inclusion-upper
                          exclusion-lower exclusion-upper)))

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
         (soc-expr `(setq ,soc-symbol
                              (+ ,initial-soc
                                 ;; limit to 1 decimal place
                                 (/ (fround
                                     (* 1000.0 (/ ,energy-symbol ,capacity)))
                                    10.0))))

         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))
         (excl-bounds (or (alist-get 'exclusion-bounds config-alist) '(0.0 0.0)))

         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (excl-lower (car excl-bounds))
         (excl-upper (cadr excl-bounds))

         (incl-lower-symbol (inclusion-lower-symbol-from-id id))
         (incl-upper-symbol (inclusion-upper-symbol-from-id id))

         (soc-lower (alist-get 'soc-lower config-alist))
         (soc-upper (alist-get 'soc-upper config-alist))

         (incl-lower-expr `(setq ,incl-lower-symbol
                                 (if (< (- ,soc-symbol ,soc-lower) 10.0)
                                     (* ,rated-lower
                                        (bounded-exp-decay ,(+ soc-lower 10.0)
                                                           ,soc-lower
                                                           ,soc-symbol
                                                           1.2
                                                           0.3))
                                     ,rated-lower)))
         (incl-upper-expr `(setq ,incl-upper-symbol
                                 (if (< (- ,soc-upper ,soc-symbol) 10.0)
                                     (* ,rated-upper
                                        (bounded-exp-decay ,(- soc-upper 10.0)
                                                           ,soc-upper
                                                           ,soc-symbol
                                                           1.2
                                                           0.3))
                                     ,rated-upper)))

         (is-healthy (is-healthy-battery config-alist))

         (power-expr (when is-healthy
                       `((power . ,power-symbol)
                         (`component-state . (power->component-state ,power-symbol)))))

         (soc-bounds-expr `((soc . ,soc-symbol)
                            (inclusion-lower . ,incl-lower-symbol)
                            (inclusion-upper . ,incl-upper-symbol)
                            (exclusion-lower . ,excl-lower)
                            (exclusion-upper . ,excl-upper)))
         (battery
          `((category . battery)
            (name     . ,(format "bat-%s" id))
            (id       . ,id)
            ,@power-expr
            ,@soc-bounds-expr
            (is-healthy . ,is-healthy)
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(battery-data-maker
                                        `((id    . ,id)
                                          ,@soc-bounds-expr
                                          ,@power-expr)
                                        config-alist))))))))

    (log.trace (format "Adding battery %s. Healthy: %s" id is-healthy))

    (when (not (boundp power-symbol))
      (set power-symbol 0.0)
      (set energy-symbol 0.0)
      (set soc-symbol (eval initial-soc)))

    (setq state-update-functions
          (cons (eval (list 'lambda '(ms-since-last-call)
                            `(setq ,energy-symbol
                                   (+ ,energy-symbol ;; ->> ?
                                      (* ,power-symbol
                                         (/ ms-since-last-call
                                            ,(* 60.0 60.0 1000.0)))))
                            soc-expr
                            incl-lower-expr
                            incl-upper-expr
                            `(cond ((< ,power-symbol ,incl-lower-symbol)
                                    (setq ,power-symbol ,incl-lower-symbol))
                                   ((> ,power-symbol ,incl-upper-symbol)
                                    (setq ,power-symbol ,incl-upper-symbol)))))
                state-update-functions))

    (eval incl-lower-expr)
    (eval incl-upper-expr)

    (add-to-components-alist battery)

    battery))

;;;;;;;;;;;;;;;
;; Inverters ;;
;;;;;;;;;;;;;;;

(defmacro inverter-data-maker (data-alist defaults-alist)
  (component-data-maker data-alist
                        defaults-alist
                        '(id power current voltage component-state reactive-power
                          per-phase-reactive-power per-phase-power inclusion-lower
                          inclusion-upper)))

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

         (power-expr (when is-healthy
                       `(;; TODO; change batteries DC power to match
                         ;; AC apparent power. With below approach,
                         ;; battery power only corresponds to the real
                         ;; power.
                         (power . ,(make-power-expr successors))
                         (per-phase-power . (calc-per-phase-power ,(make-power-expr successors)))
                         (reactive-power . ,reactive-power-symbol)
                         (per-phase-reactive-power . (calc-per-phase-power ,reactive-power-symbol))
                         (voltage . voltage-per-phase)
                         (current . (ac-current-from-per-phase-power
                                     (calc-apparent-power
                                      ,(make-power-expr successors)
                                      ,reactive-power-symbol)))
                         (component-state . (power->component-state
                                             ,(make-power-expr successors))))))
         (bounds-expr `((inclusion-lower . ,rated-lower)
                        (inclusion-upper . ,rated-upper)))
         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (reactive-bounds-check-func-symbol (reactive-bounds-check-func-symbol-from-id id))
         (active-power-bounds-symbol (active-power-bounds-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))
         (set-reactive-power-func-symbol (set-reactive-power-func-symbol-from-id id))
         (reset-power-func-symbol (reset-power-func-symbol-from-id id))

         (inverter
          `((category . inverter)
            (type     . battery)
            (name     . ,(format "inv-bat-%s" id))
            (id       . ,id)
            ,@power-expr
            ,@bounds-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(inverter-data-maker
                                        `((id . ,id)
                                          ,@bounds-expr
                                          ,@power-expr)
                                        config-alist))))))))

    (log.trace (format "Adding battery inverter %s. Healthy: %s" id is-healthy))

    (set reactive-power-symbol 0.0)

    (set reactive-bounds-check-func-symbol
         (if is-healthy
             (eval (list 'lambda '(reactive-power)
                         `(let ((abs-power (abs ,(make-power-expr successors))))
                            (and
                             (>= reactive-power (* -0.35 abs-power))
                             (<= reactive-power (* 0.35 abs-power))))))
             (eval (list 'lambda '(reactive-power)
                         (log.error "inverter is unhealthy")
                         nil))))

    (set bounds-check-func-symbol
         (if is-healthy
             (eval (list 'lambda '(power)
                         `(and
                           (,(make-battery-bounds-check-expr successors) power)
                           (bounds/contains ,active-power-bounds-symbol power))))
             (eval (list 'lambda '(power)
                         (log.error "inverter is unhealthy")
                         nil))))

    (set reset-power-func-symbol
         `(lambda ()
            (dolist (battery (quote ,successors))
              (set (power-symbol-from-id (alist-get 'id battery)) 0.0))))

    (set set-power-func-symbol
         (let* ((healthy-batteries (seq-filter
                                    (lambda (b) (alist-get 'is-healthy b))
                                    successors))
                (num-batteries (length healthy-batteries))
                (expr ()))
           (dolist (battery healthy-batteries)
             (setq expr
                   (cons `(let ((power (/ power ,num-batteries)))
                            (if (not (equal
                                      power
                                      ,(power-symbol-from-id (alist-get 'id battery))))
                                (log.info (format "Setting power of battery %s to %s W (was: %s W)"
                                                  ,(alist-get 'id battery)
                                                  power
                                                  ,(power-symbol-from-id (alist-get 'id battery)))))
                            (setq ,(power-symbol-from-id (alist-get 'id battery))
                                  power)
                            )
                         expr)))
           (if (> num-batteries 0)
               (eval `(lambda (power)
                        ,@expr))
               (lambda (power)
                  (log.error "Can't set power: no healthy batteries")
                  nil))))

    (set set-reactive-power-func-symbol
         (if is-healthy
             (eval `(lambda (reactive-power)
                      (log.info (format
                                 "Setting reactive power of inverter %s to %s VAR (was: %s VAR)"
                                 ,id
                                 reactive-power
                                 ,reactive-power-symbol))
                      (setq ,reactive-power-symbol reactive-power)))
             (lambda (reactive-power)
               (log.error "Can't set reactive power: inverter is unhealthy")
               nil)))

    (set active-power-bounds-symbol (bounds/make-container rated-lower rated-upper))

    (when is-healthy
      (every
       :milliseconds 1000
       :call `(lambda ()
                (let ((measured-power ,(alist-get 'power power-expr))
                      (active-power-bounds ,active-power-bounds-symbol))
                  (set active-power-bounds-symbol
                       (bounds/drop-expired active-power-bounds))
                  (let ((limited-power (bounds/limit-power active-power-bounds measured-power)))
                    (unless (equal limited-power measured-power)
                      (log.debug (format "Limited power for inverter %s: %s W"
                                         ,id limited-power))
                      (,set-power-func-symbol limited-power)))))))

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

         (power-expr (when is-healthy
                       `((power . ,power-symbol)
                         (reactive-power . ,reactive-power-symbol)
                         (per-phase-power . (calc-per-phase-power ,power-symbol))
                         (per-phase-reactive-power . (calc-per-phase-power ,reactive-power-symbol))
                         (voltage . voltage-per-phase)
                         (current . (ac-current-from-per-phase-power
                                     (calc-apparent-power
                                      ,power-symbol
                                      ,reactive-power-symbol)))
                         (component-state . (power->component-state
                                             ,power-symbol)))))

         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (reactive-bounds-check-func-symbol (reactive-bounds-check-func-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))
         (set-reactive-power-func-symbol (set-reactive-power-func-symbol-from-id id))
         (reset-power-func-symbol (reset-power-func-symbol-from-id id))

         (inverter
          `((category . inverter)
            (type     . pv)
            (name     . ,(format "inv-pv-%s" id))
            (id       . ,id)
            (inclusion-lower . ,rated-lower)
            (inclusion-upper . ,rated-upper)
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(inverter-data-maker
                                        `((id . ,id)
                                          (inclusion-lower . ,rated-lower)
                                          (inclusion-upper . ,rated-upper)
                                          ,@power-expr)
                                        config-alist))))))))

    (log.trace (format "Adding solar inverter %s. Healthy: %s" id is-healthy))

    (when (not (boundp min-power-symbol))
      (set min-power-symbol rated-lower))

    (set power-symbol (max (eval min-power-symbol) (* rated-lower (/ sunlight% 100.0))))
    (set reactive-power-symbol 0.0)

    (set bounds-check-func-symbol
         (if is-healthy
             (list 'lambda '(power)
                   `(bounds/contains ,active-power-bounds-symbol power))
             (list 'lambda '(power)
                   (log.error "inverter is unhealthy")
                   nil)))

    (set reactive-bounds-check-func-symbol
         (if is-healthy
             (list 'lambda '(reactive-power)
                   `(and (>= reactive-power (* -0.35 (abs ,power-symbol)))
                         (<= reactive-power (* 0.35 (abs ,power-symbol)))))
             (list 'lambda '(reactive-power)
                   (log.error "inverter is unhealthy")
                   nil)))

    (set reset-power-func-symbol
         `(lambda ()
            (setq ,min-power-symbol ,rated-lower)
            (setq ,power-symbol (max ,rated-lower ,(* rated-lower (/ sunlight% 100))))))

    (set set-power-func-symbol
         (if is-healthy
             (eval `(lambda (power)
                      (let ((min-power ,(* rated-lower (/ sunlight% 100.0))))
                        (setq ,min-power-symbol (max power min-power))
                        (if (< power min-power)
                            (progn
                              (log.info
                               (format "Given power %s W is not available for inverter %s.  Limiting to %s W."
                                       power ,id min-power))
                              (setq ,power-symbol min-power))
                            (log.info (format "Setting power of inverter %s to %s W (was: %s W)"
                                              ,id
                                              power
                                              ,(power-symbol-from-id id)))
                            (setq ,power-symbol power)))))
             (lambda (power)
               (log.error "Can't set power: inverter is unhealthy")
               nil)))

    (set set-reactive-power-func-symbol
         (if is-healthy
             (eval `(lambda (reactive-power)
                      (log.info (format "Setting reactive power of inverter %s to %s VAR (was: %s VAR)"
                                        ,id
                                        reactive-power
                                        ,reactive-power-symbol))
                      (setq ,reactive-power-symbol reactive-power)))
             (lambda (reactive-power)
               (log.error "Can't set reactive power: inverter is unhealthy")
               nil)))

    (set active-power-bounds-symbol (bounds/make-container rated-lower rated-upper))

    (when is-healthy
      (every
       :milliseconds 1000
       :call `(lambda ()
                (let ((measured-power ,(alist-get 'power power-expr))
                      (active-power-bounds ,active-power-bounds-symbol))
                  (set active-power-bounds-symbol
                       (bounds/drop-expired active-power-bounds))
                  (let ((limited-power (bounds/limit-power active-power-bounds measured-power)))
                    (unless (equal limited-power measured-power)
                      (log.debug (format "Limited power for inverter %s: %s W"
                                         ,id limited-power))
                      (,set-power-func-symbol limited-power)))))))

    (add-to-components-alist inverter)
    inverter))


;;;;;;;;;;;;
;; Meters ;;
;;;;;;;;;;;;

(defmacro meter-data-maker (data-alist defaults-alist)
  (component-data-maker data-alist
                        defaults-alist
                        '(id power per-phase-power reactive-power
                          per-phase-reactive-power current voltage
                          component-state)))



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
         (power-expr
          (when is-healthy
            (cond
              ((and power per-phase-power)
               (error (format "Can't use meter %s with both :power and :per-phase-power set" id)))
              (per-phase-power
               `((power . (seq-reduce '+ ,per-phase-power 0.0))
                 (per-phase-power . ,per-phase-power)))
              (power
               `((power . ,power)
                 (per-phase-power . (calc-per-phase-power ,power))))
              (:else (if-let ((per-phase-power (make-per-phase-power-expr successors)))
                         `((power . (seq-reduce '+ ,per-phase-power 0.0))
                           (per-phase-power . ,per-phase-power)))))
            ))
         (reactive-power-expr
          (when is-healthy
            (cond
              ((and reactive-power per-phase-reactive-power)
               (error (format "Can't use meter %s with both :reactive-power and :per-phase-reactive-power set" id)))
              (per-phase-reactive-power
               `((reactive-power . (seq-reduce '+ ,per-phase-reactive-power 0.0))
                 (per-phase-reactive-power . ,per-phase-reactive-power)))
              (reactive-power
               `((reactive-power . ,reactive-power)
                 (per-phase-reactive-power . (calc-per-phase-power ,reactive-power))))
              (:else (if-let ((per-phase-reactive-power (make-per-phase-reactive-power-expr successors)))
                         `((reactive-power . (seq-reduce '+ ,per-phase-reactive-power 0.0))
                           (per-phase-reactive-power . ,per-phase-reactive-power)))))))
         (current-expr (when power-expr
                         `((current . (ac-current-from-per-phase-power
                                       (calc-per-phase-apparent-power
                                        ,(alist-get 'per-phase-power power-expr)
                                        ,(alist-get 'per-phase-reactive-power
                                                    reactive-power-expr))))
                           (voltage . voltage-per-phase))))
         (meter
          `((category . meter)
            (name     . ,(format "meter-%s" id))
            (id       . ,id)
            (hidden   . ,hidden)
            ,@current-expr
            ,@power-expr
            ,@reactive-power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(meter-data-maker
                                               `((id    . ,id)
                                                 ,@current-expr
                                                 ,@power-expr
                                                 ,@reactive-power-expr)
                                               config-alist))))))))

    (log.trace (format "Adding meter %s" id))

    (unless hidden
      (add-to-components-alist meter)
      (connect-successors id successors))
    meter))


;;;;;;;;;;;;;;;;;
;; EV Chargers ;;
;;;;;;;;;;;;;;;;;

(defmacro ev-charger-data-maker (data-alist defaults-alist)
  (component-data-maker data-alist
                        defaults-alist
                        '(id power current voltage component-state
                          cable-state inclusion-lower inclusion-upper)))

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
         (soc-expr `(setq ,soc-symbol
                          (+ ,initial-soc
                             ;; limit to 1 decimal place
                             (/ (fround
                                 (* 1000.0 (/ ,energy-symbol ,capacity)))
                                10.0))))


         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))
         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (incl-lower-symbol (inclusion-lower-symbol-from-id id))
         (incl-upper-symbol (inclusion-upper-symbol-from-id id))

         (soc-lower (alist-get 'soc-lower config-alist))
         (soc-upper (alist-get 'soc-upper config-alist))

         (incl-lower 0.0)
         (incl-upper-expr `(setq ,incl-upper-symbol
                                 (if (< (- ,soc-upper ,soc-symbol) 10.0)
                                     (* ,rated-upper
                                        (bounded-exp-decay ,(- soc-upper 10.0)
                                                           ,soc-upper
                                                           ,soc-symbol
                                                           1.2
                                                           0.3))
                                     ,rated-upper)))

         (is-healthy (is-healthy-ev-charger config-alist))

         (power-expr (when is-healthy
                       `((power . ,power-symbol)
                         (current . (ac-current-from-power ,power-symbol))
                         (component-state . (power->ev-component-state ,power-symbol)))))

         (bounds-expr `((inclusion-lower . 0.0)
                        (inclusion-upper . ,rated-upper)))
         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))
         (reset-power-func-symbol (reset-power-func-symbol-from-id id))

         (ev-charger
          `((category . ev-charger)
            (name     . ,(format "ev-charger-%s" id))
            (id       . ,id)
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(ev-charger-data-maker
                                               `((id . ,id)
                                                 ,@bounds-expr
                                                 ,@power-expr)
                                               config-alist))))))))

    (log.trace (format "Adding ev-charger %s. Healthy: %s" id is-healthy))

    (when (not (boundp power-symbol))
      (set power-symbol 0.0)
      (set energy-symbol 0.0)
      (set soc-symbol (eval initial-soc)))

    (eval incl-upper-expr)
    (add-to-components-alist ev-charger)

    (setq state-update-functions
          (cons (list 'lambda '(ms-since-last-call)
                      `(eval (setq ,energy-symbol
                                   (+ ,energy-symbol ;; ->> ?
                                      (* ,power-symbol
                                         (/ ms-since-last-call
                                            ,(* 60.0 60.0 1000.0))))))
                      `(eval ,soc-expr)
                      `(eval ,incl-upper-expr)
                      `(cond ((< ,power-symbol 0.0)
                              (setq ,power-symbol 0.0))
                             ((> ,power-symbol ,incl-upper-symbol)
                              (setq ,power-symbol ,incl-upper-symbol))))
                state-update-functions))

    (set bounds-check-func-symbol
         (if is-healthy
             (list 'lambda '(power)
                   `(<= ,rated-lower
                        power
                        ,rated-upper))
             (list 'lambda '(power)
                   (log.error "ev-charger is unhealthy")
                   nil)))

    (set reset-power-func-symbol
         `(lambda ()
            (setq ,power-symbol 0.0)))

    (set set-power-func-symbol
         (if is-healthy
             `(lambda (power)
                (if (< power ,(* 6.0 3 220.0))
                    (progn
                      (log.info
                       (format "Given power %s W is too low for ev-charger %s.  Not charging."
                               power ,id))
                      (setq ,power-symbol 0.0))
                    (log.info (format "Setting power of ev-charger %s to %s W (was: %s W)"
                                      ,id
                                      power
                                      ,(power-symbol-from-id id)))
                    (setq ,(power-symbol-from-id id) power)))
           '(lambda (power)
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
