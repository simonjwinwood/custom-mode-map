;;; custom-mode-map.el --- allow customize to work with mode maps

;; A type for customizable mode maps
(define-widget 'custom-mode-map-type 'lazy
  "A view of mode maps which is editable by customize."
  :type '(alist :key-type 'key-sequence
                ;; Must be a command
                :value-type (restricted-sexp :match-alternatives (commandp) )))


(defun unfold-keymap-entry (prefix binding)
  (let ((empty-result '(nil . nil)))
    (cond
     ;; FIXME: unhandled
     ;; * CHAR-TABLE
     ((or (char-table-p event) ;; CHAR-TABLE
          (and (consp binding) (booleanp (car binding)) (car binding))) ;; t . ...
      (error "Unhandled keymap entry `%s' in unfold-keymap-entry" binding))

   ;; FIXME: menu stuff
     ((stringp binding) empty-result)
     ((and (consp binding)
           (eq (car binding) 'menu-bar))
      (message "Ignoring menubar %s" binding)
      empty-result)
   
   ;; I assume nested key bindings are the parent, which are handled at the top level.
   ((keymapp binding) empty-result)

   ;; Those starting with (TYPE . ...), i.e.
   ;; * (TYPE menu-item . DETAILS)
   ;; * (TYPE . KEYMAP)
   ;; * (TYPE . BINDING)
   ;; * (TYPE ITEM-NAME . BINDING)'
   ;; * (TYPE ITEM-NAME HELP-STRING . BINDING)'   
   ((and (consp binding)
         (eventp (car binding)))
    (let* ((event (vector (car binding)))
           (new-prefix (vconcat prefix event)))
      (cond ((and (consp (cdr binding)) 
                  (eq (car (cdr binding)) 'menu-item)) ;; menu-item . DETAILS
             (message "Ignoring menu-item %s" binding)
             empty-result)
            ((keymapp (cdr binding))  ;; . KEYMAP
             (expand-keymap (cdr binding) new-prefix))
            ((commandp (cdr binding)) ;; . BINDING
             (list (cons new-prefix (cdr binding))))
            ((and (consp (cdr binding)) (commandp (cdr (cdr binding)))) ;; ITEM-NAME . BINDING
             (list (cons new-prefix (cdr (cdr binding)))))
            ((and (consp (cdr binding))
                  (consp (nthcdr 2 binding))
                  (commandp (nthcdr 3 binding))) ;; ITEM-NAME HELP-STRING . BINDING
             (list (cons new-prefix (nthcdr 3 binding))))))
    )))

;; (defun unfold-keymap-entry (prefix event binding)
;;   (let ((new-prefix (vconcat prefix (vector event))))
;;     (cond
;;      ((and (eq event 'menu-bar))
;;       (message "Ignoring menubar %s" binding)
;;       (list nil nil nil))
   
;;      ;; Nested keymaps are simply expanded
;;      ((keymapp binding) (expand-keymap binding new-prefix))

;;      ((commandp binding) (cons
;;                           (list (cons new-prefix binding))
;;                           nil))

;;      (t '(nil . nil))))) ;; shouldn't happen?
      
;; (defun expand-keymap (keymap &optional prefix)
;;   "Expand out KEYMAP into a list of (keycode . binding) pairs."
;;   (unless (keymapp keymap)
;;     (error "Not a keymap"))
;;   ;; wtf emacs lisp.
;;   (let ((result (list nil nil nil)))
;;     (map-keymap (lambda (event binding)
;;                   (let* ((expanded (unfold-keymap-entry prefix event binding))
;;                          (new-keymap (append (nth 0 result) (car expanded)))
;;                          (new-menu   (append (nth 1 result) (cdr expanded))))
;;                     (setq result (list new-keymap new-menu nil))))
;;                 keymap)
;;     result))

(defun expand-keymap (keymap &optional prefix)
  "Expand out KEYMAP into a list of (keycode . binding) pairs."
  (unless (keymapp keymap)
    (error "Not a keymap"))
  ;; wtf emacs lisp.
  (let ((result (list nil nil nil)))
    (map-keymap (lambda (event binding)
                  (let* ((expanded (unfold-keymap-entry prefix event binding))
                         (new-keymap (append (nth 0 result) (car expanded)))
                         (new-menu   (append (nth 1 result) (cdr expanded))))
                    (setq result (list new-keymap new-menu nil))))
                keymap)
    result))

(custom-set-variables '(foo-bar (cons nil (cons "hello" "---"))))

(defun custom-get-map-type (symbol)
  (message "get %s %s" symbol (symbol-value symbol))
  (expand-keymap (symbol-value symbol)))

(defun custom-set-map-type (symbol value)
  (message "set %s %s" symbol value)
  (let ((new-map (make-sparse-keymap))
        (key-bindings (nth 0 value))
        (menu-items   (nth 1 value)))
    (mapcar (lambda (binding) (define-key new-map (car binding) (cdr binding))) key-bindings)    
    (set symbol new-map)))

(define-widget 'custom-menu-item-type 'lazy
  "A binary tree made of cons-cells and strings."
  :tag "Item"
  :format "%v"
  :type '(choice :format "%[Type%] %v"
                 (list :tag "Command"                       
                       :value (:command "" self-insert-command)
                       (const :format "" :command) (string :tag "Entry") (function :tag "Command"))
                 (list :tag "Sub-menu"
                       :format "%v"
                       (const :format "" :sub-menu) (custom-menu-bar-type :tag "Sub-menu" :format "%v"))
                 (list :tag "Separator" :format "%v" (const :format "" :separator) (const "---"))))

(define-widget 'custom-menu-bar-type 'lazy
  "A binary tree made of cons-cells and strings."
  :offset 2
  :type '(cons :format "%v"
               (string :tag "Menu")
               (repeat :format "\n%v%i\n" custom-menu-item-type)))
                       
(defcustom foo-bar (cons nil '("hello" . nil))
  "Sample variable holding a mode map."
  :get 'custom-get-map-type
  :set 'custom-set-map-type
  ;; :initialize
  :type '(list (alist :tag "Key map" :key-type key-sequence
                      ;; Must be a command
                      :value-type function)
               (repeat :tag "Menus" (custom-menu-bar-type :format "%v"))
               (choice :tag "Parent"
                       (const :tag "None" nil)
                       (sexp :tag "Parent")))
               ) ;; (restricted-sexp :match-alternatives (commandp) )))

(cdr (let ((p (make-sparse-keymap))
                (me (make-sparse-keymap)))
            me))
  
(keymap-parent (make-sparse-keymap))

(keymap
 (C-M-S-mouse-3 . haskell-doc-ask-mouse-for-type)
 (27 keymap
     (115 . ghc-sort-lines)
     (116 . ghc-insert-template-or-signature)
     (63 . ghc-display-errors)
     (110 . ghc-goto-next-error)
     (112 . ghc-goto-prev-error)
     (13 . ghc-import-module)
     (4 . ghc-browse-document)
     (9 . ghc-complete)
     (46 . haskell-mode-tag-find))
 (67108960 . haskell-interactive-bring)
 (f5 . haskell-process-load-file)
 (menu-bar keymap
           (Haskell menu-item "Haskell"
                    (keymap "Haskell"
                            (Indent\ line menu-item "Indent line" indent-according-to-mode)
                            (Indent\ region menu-item "Indent region" indent-region :enable mark-active)
                            (\(Un\)Comment\ region menu-item "(Un)Comment region" comment-region :enable mark-active)
                            (nil menu-item "---")
                            (Start\ interpreter menu-item "Start interpreter" haskell-interactive-switch)
                            (Load\ file menu-item "Load file" haskell-process-load-file)
                            (nil-6 menu-item "---")
                            (Load\ tidy\ core menu-item "Load tidy core" ghc-core-create-core)
                            (nil-8 menu-item "---")
                            (Doc\ mode menu-item "Doc mode" eldoc-mode :button
                                       (:toggle bound-and-true-p eldoc-mode))
                            (Customize menu-item "Customize" menu-function-10 :key-sequence nil))))
 (remap keymap
        (delete-indentation . haskell-delete-indentation))
 (3 keymap
    (1 . ghc-auto)
    (6 . ghc-refine)
    (62 . ghc-make-indent-deeper)
    (60 . ghc-make-indent-shallower)
    (8 . haskell-hoogle)
    (11 . ghc-kill-process)
    (13 . ghc-insert-module)
    (10 . ghc-jump-file)
    (5 . ghc-expand-th)
    (27 keymap
        (110 . ghc-goto-next-hole)
        (112 . ghc-goto-prev-hole)
        (47 . haskell-doc-check-active))
    (99 . haskell-process-cabal)
    (3 . ghc-toggle-check-command)
    (26 . haskell-interactive-switch)
    (9 . ghc-show-info)
    (20 . ghc-show-type)
    (22 . haskell-mode-enable-process-minor-mode)
    (2 . haskell-mode-enable-process-minor-mode)
    (12 . haskell-process-load-file)
    (67108910 . haskell-mode-format-imports))
 keymap
 (27 keymap
     (17 . prog-indent-sexp)))
                    

key             binding
---             -------

^C              Prefix Command
ESC             Prefix Command
^`              haskell-interactive-bring
<C-M-S-mouse-3>                 haskell-doc-ask-mouse-for-type
<f5>            haskell-process-load-file
<remap>         Prefix Command

^⌥D             ghc-browse-document
^⌥i             ghc-complete
⌥RET            ghc-import-module
⌥.              haskell-mode-tag-find
⌥?              ghc-display-errors
⌥N              ghc-goto-next-error
⌥P              ghc-goto-prev-error
⌥S              ghc-sort-lines
  (that binding is currently shadowed by another mode)
⌥T              ghc-insert-template-or-signature

<remap> <delete-indentation>    haskell-delete-indentation

^C ^A           ghc-auto
^C ^B           haskell-mode-enable-process-minor-mode
^C ^C           ghc-toggle-check-command
^C ^E           ghc-expand-th
  (that binding is currently shadowed by another mode)
^C ^F           ghc-refine
^C ^H           haskell-hoogle
^C TAB          ghc-show-info
^C ^J           ghc-jump-file
  (that binding is currently shadowed by another mode)
^C ^K           ghc-kill-process
^C ^L           haskell-process-load-file
^C RET          ghc-insert-module
^C ^T           ghc-show-type
^C ^V           haskell-mode-enable-process-minor-mode
^C ^Z           haskell-interactive-switch
^C ESC          Prefix Command
^C <            ghc-make-indent-shallower
^C >            ghc-make-indent-deeper
^C C            haskell-process-cabal
^C ^.           haskell-mode-format-imports

^⌥Q             prog-indent-sexp

^C ⌥/           haskell-doc-check-active
^C ⌥N           ghc-goto-next-hole
^C ⌥P           ghc-goto-prev-hole
