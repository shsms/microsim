;; Reset variables to their initial state everytime the script is
;; reloaded, so that the same components and connections are not added
;; multiple times.
(defun reset-state ()
  (setq comp--id--counter 1000)
  (setq connections-alist nil)
  (setq components-alist nil)
  (setq state-update-functions nil)
  (setq metadata nil))

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


(defun make-power-expr (successors)
  (let ((expr ()))
    (dolist (successor successors)
      (if-let ((power (alist-get 'power successor)))
          (setq expr (cons power expr))))
    (when expr (cons '+ expr))))


(defun make-per-phase-power-expr (successors &optional alist-key)
  (let ((p1-expr ())
        (p2-expr ())
        (p3-expr ())
        (alist-key (or alist-key 'per-phase-power)))
    (dolist (successor successors)
      (when-let ((per-phase-power (alist-get alist-key successor)))
        (setq p1-expr (cons `(car ,per-phase-power) p1-expr))
        (setq p2-expr (cons `(cadr ,per-phase-power) p2-expr))
        (setq p3-expr (cons `(caddr ,per-phase-power) p3-expr))))
    (when p1-expr
      (setq p1-expr (cons '+ p1-expr))
      (setq p2-expr (cons '+ p2-expr))
      (setq p3-expr (cons '+ p3-expr))
      (list 'list p1-expr p2-expr p3-expr))))


(defun make-per-phase-reactive-power-expr (successors)
  (make-per-phase-power-expr successors 'per-phase-reactive-power))


(defun make-current-expr (successors)
  (let ((p1-expr ())
        (p2-expr ())
        (p3-expr ()))
    (dolist (successor successors)
      (if-let ((current (alist-get 'current successor)))
          (progn
            (setq p1-expr (cons `(car ,current) p1-expr))
            (setq p2-expr (cons `(cadr ,current) p2-expr))
            (setq p3-expr (cons `(caddr ,current) p3-expr)))))
    (when p1-expr (list 'list
                        (setq p1-expr (cons '+ p1-expr))
                        (setq p2-expr (cons '+ p2-expr))
                        (setq p3-expr (cons '+ p3-expr))))))

(defun make-battery-bounds-check-expr (successors)
  (let ((sum-incl-lower-expr ())
        (sum-incl-upper-expr ())
        (sum-excl-lower-expr ())
        (sum-excl-upper-expr ()))
    (dolist (successor successors)
      (when-let ((is-healthy (alist-get 'is-healthy successor))
               (incl-lower (alist-get 'inclusion-lower successor))
               (incl-upper (alist-get 'inclusion-upper successor)))
        (setq sum-incl-lower-expr (cons incl-lower sum-incl-lower-expr))
        (setq sum-incl-upper-expr (cons incl-upper sum-incl-upper-expr)))
      (when-let ((excl-lower (alist-get 'exclusion-lower successor))
               (excl-upper (alist-get 'exclusion-upper successor)))
        (setq sum-excl-lower-expr (cons excl-lower sum-excl-lower-expr))
        (setq sum-excl-upper-expr (cons excl-upper sum-excl-upper-expr))))

    (when sum-incl-lower-expr
      (setq sum-incl-lower-expr (cons '+ sum-incl-lower-expr))
      (setq sum-incl-upper-expr (cons '+ sum-incl-upper-expr))
      )

    (when sum-excl-lower-expr
      (setq sum-excl-lower-expr (cons '+ sum-excl-lower-expr))
      (setq sum-excl-upper-expr (cons '+ sum-excl-upper-expr))
      )

    (eval (list 'lambda '(power)
                (when sum-incl-lower-expr
                  `(and (<= ,sum-incl-lower-expr power ,sum-incl-upper-expr)
                        (or (equal power 0.0)
                            ,(when sum-excl-lower-expr
                               `(or (<= power ,sum-excl-lower-expr)
                                    (<= ,sum-excl-upper-expr power))))))))))


(defun set-power-active (id power)
  (let* (;; TODO: drop unused? power-symbol
         (power-symbol (power-symbol-from-id id))
         (bounds-check-func (eval (bounds-check-func-symbol-from-id id)))
         (set-power-func (eval (set-power-func-symbol-from-id id)))
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
  (let* ((reactive-bounds-check-func (eval (reactive-bounds-check-func-symbol-from-id id)))
         (set-reactive-power-func (eval (set-reactive-power-func-symbol-from-id id)))
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
  (let* ((reset-power-func (eval (reset-power-func-symbol-from-id id))))
    (if reset-power-func
        (funcall reset-power-func)
      (log.warn "No reset power function found for component id %d" id))))


(defun augment-active-power-bounds (id create-ts bounds)
  (let* ((active-power-bounds-symbol (active-power-bounds-symbol-from-id id)))
    (set active-power-bounds-symbol
         (bounds/add (eval active-power-bounds-symbol) create-ts bounds))))

(defun component-data-maker (data-alist defaults-alist keys)
  (let ((data-alist (eval data-alist))
        (defaults-alist (eval defaults-alist))
        (args-alist))

    (dolist (key keys)
      (if-let ((val (alist-get key data-alist)))
          (setq args-alist (cons (cons key val) args-alist))
        (if-let ((val (alist-get key defaults-alist)))
            (setq args-alist (cons (cons key `(quote ,val)) args-alist)))))

    (eval (list 'lambda '(_) `(quote ,args-alist)))))


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
        (mapcar '(lambda (voltage) (* power (/ voltage total-voltage))) voltage-per-phase))
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

(defun repeat-every-impl (counter every-ms action ms-since-last-call)
  (let ((count (+ (eval counter) ms-since-last-call)))
    (set counter count)
    (when (> count every-ms)
      (funcall action)
      (set counter 0))))

(defun every (&rest plist)
  (let* ((milliseconds (plist-get plist :milliseconds))
         (action (plist-get plist :call))
         (timer (gensym "timer-")))
    (funcall action)     ;; call once at the start
    (set timer 0)
    (setq state-update-functions
          (cons (eval (list 'lambda '(ms-since-last-call)
                            `(repeat-every-impl
                              (quote ,timer)
                              ,milliseconds
                              ,action
                              ms-since-last-call)))
                state-update-functions))
    ))
