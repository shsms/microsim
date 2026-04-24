;; Reset variables to their initial state everytime the script is
;; reloaded, so that the same components and connections are not added
;; multiple times.
(defun reset-state ()
  (setq comp--id--counter 1000)
  (setq connections-alist nil)
  (setq components-alist nil)
  ;; Stop any timers from the previous load so they don't keep mutating
  ;; now-orphaned state symbols.
  (when (boundp 'active-timers)
    (dolist (tm active-timers)
      (cancel-timer tm)))
  (setq active-timers nil)
  (setq metadata nil))

(defun identity (x) x)

(defun get-comp-id ()
  (setq comp--id--counter (+ comp--id--counter 1)))


(defun reactive-power-symbol-from-id (id)
  (intern (format "component-reactive-power-%s" id)))


(defun active-power-bounds-symbol-from-id (id)
  (intern (format "component-active-power-bounds-%s" id)))


(defun power-symbol-from-id (id)
  (intern (format "component-power-%s" id)))


(defun energy-symbol-from-id (id)
  (intern (format "component-energy-%s" id)))


(defun soc-symbol-from-id (id)
  (intern (format "component-soc-%s" id)))


(defun inclusion-upper-symbol-from-id (id)
  (intern (format "component-inclusion-upper-%s" id)))


(defun inclusion-lower-symbol-from-id (id)
  (intern (format "component-inclusion-lower-%s" id)))


(defun bounds-check-func-symbol-from-id (id)
  (intern (format "component-bounds-check-func-%s" id)))


(defun reactive-bounds-check-func-symbol-from-id (id)
  (intern (format "component-reactive-bounds-check-func-%s" id)))


(defun set-reactive-power-func-symbol-from-id (id)
  (intern (format "component-set-reactive-power-func-%s" id)))


(defun set-power-func-symbol-from-id (id)
  (intern (format "component-set-power-func-%s" id)))


(defun reset-power-func-symbol-from-id (id)
  (intern (format "component-reset-power-func-%s" id)))


(defun add-to-connections-alist (id-from id-to)
  (setq connections-alist (cons (cons id-from id-to)
                                connections-alist)))


(defun add-to-components-alist (alist)
  (setq components-alist (cons alist
                               components-alist)))


(defun connect-components (alist-from alist-to)
  (let ((id-from (alist-get 'id alist-from))
        (id-to (alist-get 'id alist-to)))
    (add-to-connections-alist id-from id-to)))


(defun connect-successors (id successors)
  (dolist (successor successors)
    (unless (alist-get 'hidden successor)
      (add-to-connections-alist id (alist-get 'id successor)))))


(defun sum-symbol-values (syms)
  (let ((total 0.0))
    (dolist (sym syms)
      (setq total (+ total (symbol-value sym))))
    total))

;; `(mapcar 'symbol-value …)` double-evaluates under the current
;; defspecial/defun wrapper (the element is a symbol; the wrapper
;; evaluates it again, producing the symbol's value, which is then
;; passed to symbol-value). Use this helper in hot paths instead.
(defun symbol-values (syms)
  (let ((result nil))
    (dolist (sym syms)
      (setq result (cons (symbol-value sym) result)))
    (reverse result)))

;; Returns a 0-arg closure summing successors' current power via their
;; `'power-symbol` entries, or nil if none of them expose one.
(defun make-power-fn (successors)
  (let ((syms (seq-filter 'identity
                          (mapcar (lambda (s) (alist-get 'power-symbol s)) successors))))
    (when syms
      (lambda () (sum-symbol-values syms)))))


;; Given successors that expose a `'data-fn`, returns a 0-arg closure
;; returning the elementwise sum (a 3-element list) of their ALIST-KEY
;; field (defaulting to `per-phase-power`). Each successor is queried
;; at call time.
(defun make-per-phase-fn (successors &optional alist-key)
  (let ((alist-key (or alist-key 'per-phase-power))
        (data-fns (seq-filter 'identity
                              (mapcar (lambda (s) (alist-get 'data-fn s)) successors))))
    (when data-fns
      (lambda ()
        (let ((p1 0.0) (p2 0.0) (p3 0.0))
          (dolist (fn data-fns)
            (when-let ((pp (alist-get alist-key (funcall fn 0))))
              (setq p1 (+ p1 (car pp)))
              (setq p2 (+ p2 (cadr pp)))
              (setq p3 (+ p3 (caddr pp)))))
          (list p1 p2 p3))))))


(defun make-per-phase-reactive-fn (successors)
  (make-per-phase-fn successors 'per-phase-reactive-power))


(defun make-current-fn (successors)
  (make-per-phase-fn successors 'current))

(defun make-battery-bounds-check-fn (successors)
  (let ((all-bounds (seq-filter 'identity
                                (mapcar (lambda (s) (alist-get 'bounds-symbol s))
                                        successors))))
    (lambda (power)
      (bounds/contains-in-sum power (symbol-values all-bounds)))))


(defun set-power-active (id power)
  (let* (;; TODO: drop unused? power-symbol
         (power-symbol (power-symbol-from-id id))
         (bounds-check-func (symbol-value (bounds-check-func-symbol-from-id id)))
         (set-power-func (symbol-value (set-power-func-symbol-from-id id)))
         (power (ftruncate power)))

    (if (funcall bounds-check-func power)
        (progn
          (funcall set-power-func power)
          nil)
        (let ((err (format "Requested power %f is out of bounds for component id %d" power id)))
          (log.warn err)
          ;; TODO: switch to throw instead of returning error string
          err))))


(defun set-power-reactive (id reactive-power)
  (let* ((reactive-bounds-check-func (symbol-value (reactive-bounds-check-func-symbol-from-id id)))
         (set-reactive-power-func (symbol-value (set-reactive-power-func-symbol-from-id id)))
         (reactive-power (ftruncate reactive-power)))

    (if (funcall reactive-bounds-check-func reactive-power)
        (progn
          (funcall set-reactive-power-func reactive-power)
          nil)
        (let ((err (format "Requested reactive power %f is out of bounds for component id %d" reactive-power id)))
          (log.warn err)
          ;; TODO: switch to throw instead of returning error string
          err))))


(defun reset-power-active (id)
  (let* ((reset-power-func (symbol-value (reset-power-func-symbol-from-id id))))
    (if reset-power-func
        (funcall reset-power-func)
      (log.warn "No reset power function found for component id %d" id))))


(defun augment-active-power-bounds (id create-ts bounds lifetime-secs)
  (let* ((active-power-bounds-symbol (active-power-bounds-symbol-from-id id)))
    (set active-power-bounds-symbol
         (bounds/add (symbol-value active-power-bounds-symbol) create-ts bounds lifetime-secs))))

(defun component-data-maker (data-alist defaults-alist keys)
  (let ((args-alist))
    (dolist (key keys)
      (if-let ((val (alist-get key data-alist)))
          (setq args-alist (cons (cons key val) args-alist))
        (if-let ((val (alist-get key defaults-alist)))
            (setq args-alist (cons (cons key `(quote ,val)) args-alist)))))
    (lambda (_) args-alist)))


(defun ac-current-from-power (power)
  (if (numberp power)
      (let ((sum-voltage (seq-reduce '+ voltage-per-phase 0.0))
            (vp1 (car voltage-per-phase))
            (vp2 (cadr voltage-per-phase))
            (vp3 (caddr voltage-per-phase)))
        (list (/ (* power (/ vp1 sum-voltage)) vp1)
              (/ (* power (/ vp2 sum-voltage)) vp2)
              (/ (* power (/ vp3 sum-voltage)) vp3)))
    '(0.0 0.0 0.0)))


(defun ac-current-from-per-phase-power (per-phase-power)
  (if (consp per-phase-power)
        (list (/ (car per-phase-power) (car voltage-per-phase))
              (/ (cadr per-phase-power) (cadr voltage-per-phase))
              (/ (caddr per-phase-power) (caddr voltage-per-phase)))
      '(0.0 0.0 0.0)))


(defun calc-apparent-power (power reactive-power)
  (if (and (numberp power)
           (numberp reactive-power))
        (let ((per-phase-power (calc-per-phase-power power))
              (per-phase-reactive-power (calc-per-phase-power reactive-power)))
          (calc-per-phase-apparent-power per-phase-power per-phase-reactive-power))
        '(0.0 0.0 0.0)))


(defun calc-per-phase-apparent-power (per-phase-power per-phase-reactive-power)
  (if (and (consp per-phase-power)
           (consp per-phase-reactive-power))
      (list (sqrt (+ (expt (car per-phase-power) 2)
                     (expt (car per-phase-reactive-power) 2)))
            (sqrt (+ (expt (cadr per-phase-power) 2)
                     (expt (cadr per-phase-reactive-power) 2)))
            (sqrt (+ (expt (caddr per-phase-power) 2)
                     (expt (caddr per-phase-reactive-power) 2))))
    '(0.0 0.0 0.0)))


(defun calc-per-phase-power (power)
  (if (numberp power)
      (let ((total-voltage (seq-reduce '+ voltage-per-phase 0.0)))
        (mapcar (lambda (voltage) (* power (/ voltage total-voltage))) voltage-per-phase))
    '(0.0 0.0 0.0)))


(defun is-healthy-battery (bat)
  (let ((comp-state (alist-get 'component-state bat))
        (relay-state (alist-get 'relay-state bat)))
    (and (or (eq comp-state 'ready)
             (eq comp-state 'charging)
             (eq comp-state 'discharging))
         (eq relay-state 'relay-closed))))

(defun is-healthy-meter (met)
  (eq (alist-get 'component-state met) 'ready))

(defun is-healthy-inverter (inv)
  (let ((comp-state (alist-get 'component-state inv)))
    (or (eq comp-state 'ready)
        (eq comp-state 'charging)
        (eq comp-state 'discharging))))

(defun is-healthy-ev-charger (ev)
  (let ((comp-state (alist-get 'component-state ev))
        (cable-state (alist-get 'cable-state ev)))
    (and (or (eq comp-state 'ready)
             (eq comp-state 'charging)
             (eq comp-state 'discharging))
         (eq cable-state 'ev-charging-cable-locked-at-ev))))

(defun power->component-state (power)
  (cond
    ((not (numberp power)) 'error)
    ((> power 0.0) 'charging)
    ((< power 0.0) 'discharging)
    (:else         'ready)))

(defun power->ev-component-state (power)
  (cond
    ((not (numberp power)) 'error)
    ((> power 0.0) 'charging)
    ((< power 0.0) 'discharging)
    (:else         'ready)))

(defun bounded-exp-decay (start stop val base min_val)
  (let* ((base (max base 1.1))
         (factor (/ 10.0 (- stop start)))
         (stop (+ start (* (- stop start) factor)))
         (val (+ start (* (- val start) factor)))
         (shift (- min_val (expt base (- start stop 1)))))
    (cond
      ((>= val stop) 0.0)
      ((< val start) 1.0)
      (t (+ shift (* (- 1.0 shift)
                     (expt base (- start val))))))))

;; Call `:call` once now, then every `:milliseconds` ms. Handle is
;; pushed onto `active-timers` so `reset-state` can cancel it on config
;; reload.
(defun every (&rest plist)
  (let* ((milliseconds (plist-get plist :milliseconds))
         (func (plist-get plist :call))
         (secs (/ milliseconds 1000.0)))
    (funcall func)
    (setq active-timers
          (cons (run-with-timer secs secs func) active-timers))))

