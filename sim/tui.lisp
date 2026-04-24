;; Minimal TUI for microsim. Loaded by the `microsim-tui` binary, which
;; repeatedly invokes `(tui/frame)` between tokio yields. `tui/frame`
;; must return non-nil when the user wants to quit.

;; defvar preserves live state across config reloads — plain setq would
;; reset these every time config.lisp is saved, killing the TUI or the
;; user's selection.
(defvar tui/running  t)
(defvar tui/term     nil)
(defvar tui/selected 0)

;; Tree structure cache: prefixes (tree-drawing chars) and components in
;; display order, plus the components-alist size the cache was built
;; against. Invalidated automatically when the count changes (config
;; reload).
(defvar tui/-cached-prefixes nil)
(defvar tui/-cached-comps    nil)
(defvar tui/-cached-count    -1)

(defun tui/-ensure-structure ()
  (let ((n (length components-alist)))
    (unless (= tui/-cached-count n)
      (tui/-build-structure)
      (setq tui/-cached-count n))))

;; Mouse support: remember the visible region of the components list so
;; clicks can be translated into indices.
(setq tui/-list-x      0)
(setq tui/-list-y      0)
(setq tui/-list-w      0)
(setq tui/-list-h      0)
(setq tui/-list-offset 0)

;; Colour scheme inspired by the doom-monokai-machine Emacs theme.
;; https://github.com/doomemacs/themes/blob/master/themes/doom-monokai-machine-theme.el
;; Named per their role in doom-themes so face → TUI mapping stays obvious.
(setq tui/-dmm-fg       "#f2fffc")  ;; primary foreground (variables)
(setq tui/-dmm-fg-alt   "#c6c6c6")  ;; secondary foreground
(setq tui/-dmm-base3    "#3a4449")  ;; selection / region background
(setq tui/-dmm-base5    "#5a6568")  ;; low-contrast text
(setq tui/-dmm-base6    "#6b7678")  ;; comments (non-bright)
(setq tui/-dmm-base7    "#8b9798")  ;; borders / subtle ui
(setq tui/-dmm-red      "#ff6d7e")  ;; errors, operators
(setq tui/-dmm-orange   "#ffb270")  ;; warnings
(setq tui/-dmm-yellow   "#ffed72")  ;; strings
(setq tui/-dmm-green    "#a2e57b")  ;; functions
(setq tui/-dmm-cyan     "#7cd5f1")  ;; keywords, types
(setq tui/-dmm-violet   "#baa0f8")  ;; numbers, constants, builtins

;; Shared style palettes for all four widgets. The list widget leaves
;; highlight-* unset so the ratatui side falls back to REVERSED, which swaps
;; fg/bg on whatever per-span colour each field already has.
(setq tui/-header-style
      `((fg . ,tui/-dmm-orange)
        (modifier . bold)
        (border-fg . ,tui/-dmm-cyan)
        (title-fg . ,tui/-dmm-cyan)
        (title-modifier . bold)))
(setq tui/-list-style
      `((border-fg . ,tui/-dmm-cyan)
        (title-fg . ,tui/-dmm-cyan)
        (title-modifier . bold)
        ;; Mirror doom's `region' face: a dark bg bar that leaves the row's
        ;; per-span fg colours untouched instead of inverting them.
        (highlight-bg . ,tui/-dmm-base3)
        (highlight-modifier . bold)))
(setq tui/-details-style
      `((fg . ,tui/-dmm-fg-alt)
        (border-fg . ,tui/-dmm-violet)
        (title-fg . ,tui/-dmm-violet)
        (title-modifier . bold)))
(setq tui/-log-style
      `((fg . ,tui/-dmm-base6)
        (border-fg . ,tui/-dmm-base7)
        (title-fg . ,tui/-dmm-yellow)
        (title-modifier . bold)))

;; Per-field colour palettes for the components list. Bright variants map to
;; doom faces (id → numbers/violet, category → types/cyan, name → functions/
;; green); dim variants fall back to comment-grey so the eye lands on the
;; selected subtree without losing structure.
(setq tui/-prefix-style     `((fg . ,tui/-dmm-base7)))
(setq tui/-id-style         `((fg . ,tui/-dmm-violet)))
(setq tui/-cat-style        `((fg . ,tui/-dmm-cyan)))
(setq tui/-name-style       `((fg . ,tui/-dmm-green) (modifier . bold)))
(setq tui/-dim-prefix-style `((fg . ,tui/-dmm-base5)))
(setq tui/-dim-id-style     `((fg . ,tui/-dmm-base5)))
(setq tui/-dim-cat-style    `((fg . ,tui/-dmm-base6)))
(setq tui/-dim-name-style   `((fg . ,tui/-dmm-base6)))

;; Details panel palette. Labels borrow the comment colour; values follow
;; doom's face roles — text uses the main fg (variables), enum → types/cyan,
;; state → functions/green, numerics → strings/yellow.
(setq tui/-detail-label-style `((fg . ,tui/-dmm-base7)))
(setq tui/-detail-text-style  `((fg . ,tui/-dmm-fg) (modifier . bold)))
(setq tui/-detail-enum-style  `((fg . ,tui/-dmm-cyan)))
(setq tui/-detail-state-style `((fg . ,tui/-dmm-green) (modifier . bold)))
(setq tui/-detail-num-style   `((fg . ,tui/-dmm-yellow)))
(setq tui/-detail-empty-style `((fg . ,tui/-dmm-base6) (modifier . italic)))

(defun tui/-init-once ()
  (unless tui/term
    (setq tui/term (tui/init))))

(defun tui/-find-comp (id)
  (let ((found nil))
    (dolist (comp components-alist)
      (when (and (not found) (= (alist-get 'id comp) id))
        (setq found comp)))
    found))

(defun tui/-children-of (id)
  ;; connections-alist is built bottom-up and each pair is prepended, so
  ;; consing matches as we walk front-to-back yields declaration order.
  (let ((result nil))
    (dolist (pair connections-alist)
      (when (= (car pair) id)
        (setq result (cons (cdr pair) result))))
    result))

(defun tui/-root-ids ()
  (let ((roots nil))
    (dolist (comp components-alist)
      (let ((id (alist-get 'id comp))
            (is-child nil))
        (dolist (pair connections-alist)
          (when (= (cdr pair) id)
            (setq is-child t)))
        (unless is-child
          (setq roots (cons id roots)))))
    (reverse roots)))

(setq tui/-tree-prefix-acc nil)
(setq tui/-tree-comps-acc  nil)

(defun tui/-tree-walk (id prefix branch child-prefix)
  (let ((comp (tui/-find-comp id)))
    (when comp
      (setq tui/-tree-prefix-acc
            (cons (concat prefix branch) tui/-tree-prefix-acc))
      (setq tui/-tree-comps-acc
            (cons comp tui/-tree-comps-acc))
      (let* ((children (tui/-children-of id))
             (n        (length children))
             (i        0))
        (dolist (cid children)
          (let ((is-last (= i (- n 1))))
            (tui/-tree-walk
             cid
             child-prefix
             (if is-last "└─ " "├─ ")
             (concat child-prefix (if is-last "   " "│  "))))
          (setq i (+ i 1)))))))

(defun tui/-build-structure ()
  (setq tui/-tree-prefix-acc nil)
  (setq tui/-tree-comps-acc  nil)
  (dolist (root-id (tui/-root-ids))
    (tui/-tree-walk root-id "" "" ""))
  (setq tui/-cached-prefixes (reverse tui/-tree-prefix-acc))
  (setq tui/-cached-comps    (reverse tui/-tree-comps-acc)))

;; Collect the ID of COMP plus every descendant, as a flat list.
(setq tui/-subtree-acc nil)

(defun tui/-collect-subtree (id)
  (setq tui/-subtree-acc (cons id tui/-subtree-acc))
  (dolist (cid (tui/-children-of id))
    (tui/-collect-subtree cid)))

(defun tui/-subtree-ids (id)
  (setq tui/-subtree-acc nil)
  (when id (tui/-collect-subtree id))
  tui/-subtree-acc)

;; Build the styled list items for `tui/list`. Each item is a list of
;; (TEXT . STYLE-ALIST) spans so the ratatui side can colour each field
;; independently. Components in SEL-ID's subtree are coloured with the bright
;; palette; everything else gets the dim palette.
(defun tui/-build-items (sel-id)
  (let ((subtree  (tui/-subtree-ids sel-id))
        (items    nil)
        (prefixes tui/-cached-prefixes)
        (comps    tui/-cached-comps))
    (while comps
      (let* ((prefix (car prefixes))
             (comp   (car comps))
             (id     (alist-get 'id comp))
             (in-sub (memql id subtree))
             (p-st   (if in-sub tui/-prefix-style tui/-dim-prefix-style))
             (i-st   (if in-sub tui/-id-style     tui/-dim-id-style))
             (c-st   (if in-sub tui/-cat-style    tui/-dim-cat-style))
             (n-st   (if in-sub tui/-name-style   tui/-dim-name-style)))
        (setq items
              (cons (list (cons prefix p-st)
                          (cons (format "%d " id) i-st)
                          (cons (format "%s " (alist-get 'category comp)) c-st)
                          (cons (format "%s" (alist-get 'name comp)) n-st))
                    items))
        (setq prefixes (cdr prefixes))
        (setq comps    (cdr comps))))
    (reverse items)))

(defun tui/-resolve (expr)
  "Return EXPR's display value: bare unbound symbols stay literal (they are
enum-ish values like `pv' or `ready'); everything else is evaluated."
  (cond
   ((null expr) nil)
   ((not (symbolp expr)) (eval expr))
   ((boundp expr) (eval expr))
   (t expr)))

(defun tui/-detail-value-style (kind)
  (cond
   ((eq kind 'text)  tui/-detail-text-style)
   ((eq kind 'enum)  tui/-detail-enum-style)
   ((eq kind 'state) tui/-detail-state-style)
   (t                tui/-detail-num-style)))

(defun tui/-detail-line (label kind expr)
  (let ((val (tui/-resolve expr)))
    (when val
      (list (cons (format "%-10s " label) tui/-detail-label-style)
            (cons (format "%s" val) (tui/-detail-value-style kind))))))

;; SPEC is a list of (LABEL KIND FIELD) triples; returns a list of styled
;; detail lines with nil (missing-value) rows dropped.
(defun tui/-comp-details (comp)
  (if (null comp)
      (list (list (cons "(no component selected)" tui/-detail-empty-style)))
    (let ((specs '((id       num   id)
                   (name     text  name)
                   (category enum  category)
                   (type     enum  type)
                   (state    state component-state)
                   (relay    state relay-state)
                   (cable    state cable-state)
                   (soc      num   soc)
                   (capacity num   capacity)
                   (voltage  num   voltage)
                   (current  num   current)
                   (power    num   power)
                   (reactive num   reactive-power)
                   (bounds   num   bounds)))
          (lines nil))
      (dolist (spec specs)
        (let* ((label (car   spec))
               (kind  (car (cdr spec)))
               (field (car (cdr (cdr spec))))
               (line  (tui/-detail-line label kind (alist-get field comp))))
          (when line
            (setq lines (cons line lines)))))
      (reverse lines))))

(defun tui/-tail-lines (n lst)
  "Return the last N elements of LST (fewer if LST is shorter)."
  (seq-drop lst (max 0 (- (length lst) n))))

(defun tui/-render ()
  (tui/-ensure-structure)
  (let* ((size    (tui/size tui/term))
         (cols    (car size))
         (rows    (cdr size))
         (header  3)
         (log-h   (min 10 (max 3 (floor (/ rows 4)))))
         (body-y  header)
         (body-h  (- rows header log-h))
         (log-y   (+ body-y body-h))
         (left    (floor (/ cols 2)))
         (right   (- cols left))
         (comps   tui/-cached-comps)
         (count   (length comps))
         (idx     (if (> count 0)
                      (max 0 (min tui/selected (- count 1)))
                    0))
         (current (nth idx comps))
         (sel-id  (when current (alist-get 'id current)))
         (items   (tui/-build-items sel-id))
         ;; log-h includes the 2-row border; leave the rest for text.
         (log-body-h (max 0 (- log-h 2)))
         (log-text (string-join (tui/-tail-lines log-body-h (tui/log-lines)) "\n"))
         ;; list viewport: inside the border. Remember for mouse hit-tests.
         (list-inner-y (+ body-y 1))
         (list-inner-h (max 0 (- body-h 2)))
         ;; Keep the selected row visible (ratatui's ListState does the same
         ;; thing internally; we mirror it so clicks can be back-projected).
         (offset (cond
                  ((< idx tui/-list-offset) idx)
                  ((and (> list-inner-h 0)
                        (>= idx (+ tui/-list-offset list-inner-h)))
                   (- idx (- list-inner-h 1)))
                  (t tui/-list-offset))))
    (setq tui/selected idx)
    (setq tui/-list-x      0)
    (setq tui/-list-y      list-inner-y)
    (setq tui/-list-w      left)
    (setq tui/-list-h      list-inner-h)
    (setq tui/-list-offset offset)
    (list
     (tui/paragraph 0    0      cols  header "microsim"
                    "q/C-c quit   C-n/C-p move   scroll or click to select"
                    tui/-header-style)
     (tui/list      0    body-y left  body-h "components" items idx
                    tui/-list-style)
     (tui/paragraph left body-y right body-h "details"
                    (tui/-comp-details current)
                    tui/-details-style)
     (tui/paragraph 0    log-y  cols  log-h  "log" log-text
                    tui/-log-style))))

(defun tui/-in-list-area (x y)
  (and (>= x tui/-list-x)
       (< x (+ tui/-list-x tui/-list-w))
       (>= y tui/-list-y)
       (< y (+ tui/-list-y tui/-list-h))))

(defun tui/-handle-mouse (kind x y)
  (cond
   ((eq kind 'mouse-scroll-up)
    (setq tui/selected (max 0 (- tui/selected 1))))
   ((eq kind 'mouse-scroll-down)
    (setq tui/selected (+ tui/selected 1)))
   ((and (eq kind 'mouse-left) (tui/-in-list-area x y))
    (setq tui/selected (+ tui/-list-offset (- y tui/-list-y))))))

(defun tui/-handle (ev)
  (cond
   ;; Mouse events arrive as (KIND X Y).
   ((listp ev)
    (tui/-handle-mouse (car ev) (car (cdr ev)) (car (cdr (cdr ev)))))
   ((or (eq ev 'char-q) (eq ev 'C-char-c))
    (setq tui/running nil))
   ((or (eq ev 'up) (eq ev 'C-char-p))
    (setq tui/selected (max 0 (- tui/selected 1))))
   ((or (eq ev 'down) (eq ev 'C-char-n))
    (setq tui/selected (+ tui/selected 1)))
   ((or (eq ev 'page-up) (eq ev 'M-char-v))
    (setq tui/selected (max 0 (- tui/selected 10))))
   ((or (eq ev 'page-down) (eq ev 'C-char-v))
    (setq tui/selected (+ tui/selected 10)))
   ((or (eq ev 'home) (eq ev 'C-char-a) (eq ev 'M-char-<))
    (setq tui/selected 0))
   ((or (eq ev 'end) (eq ev 'C-char-e) (eq ev 'M-char->))
    ;; render clamps to (count - 1) and writes back to tui/selected.
    (setq tui/selected 999999))))

(defun tui/frame ()
  (tui/-init-once)
  (tui/draw tui/term (tui/-render))
  ;; Non-blocking poll — the Rust driver sleeps between frames, so
  ;; holding the ctx write-lock inside a blocking poll would starve
  ;; grpc writers (e.g. set-power-active) that also need the ctx.
  (let ((ev (tui/poll-event 0)))
    (when ev (tui/-handle ev)))
  (not tui/running))
