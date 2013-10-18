;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun custom-autoload (&rest args))
;; Define the load-path
(setq load-path (cons "~/.emacs.d/" load-path))
(let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Some Basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Turn on font-lock mode for Emacs
(global-font-lock-mode t)

;; Don't show the startup screen
;; Don't show the startup screen and disable menu
(setq inhibit-startup-message t)
(menu-bar-mode -1)

;; Invoke M-X without alt key
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; utf-8 preferred
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Turn off backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

;; remove trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; Line-wrapping
(set-default 'fill-column 75)

;; No tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Turn on line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Gotta see matching parens
(show-paren-mode t)

;; No Scroll Bar
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; No beep
;; (setq visible-bell t)

;; Mark region when selecting
(setq transient-mark-mode t)

;; Show file size
(setq size-indication-mode t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Always start server
(load "server")
(unless (server-running-p) (server-start))

;; ido mode
(require 'ido)
(ido-mode t)

;; Highlight changes
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#303030")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#303030")
(add-hook 'after-save-hook
          (lambda ()
            (highlight-changes-remove-highlight (point-min) (point-max))))
(set-background-color "black")

;; cscope support
(require 'xcscope)
(setq cscope-do-not-update-database t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ibuffer Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ibuffer)
(require 'ibuffer-vc)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

(setq ibuffer-saved-filter-groups
      '(("home"
         ;; Home Base
         ("MAIN3" (filename . "work/core.main3"))
         ("TOOLS" (filename . "work/tools"))
         ("THIRDPARTY" (filename . "work/thirdparty"))
         ("BUILDBOT" (filename . "work/buildbot"))
         ("WORK" (filename . "work/"))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*"))))))

(add-hook 'ibuffer-hook
     (lambda ()
       (ibuffer-auto-mode 1)
       (ibuffer-switch-to-saved-filter-groups "home")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun das-mail-mode-hook ()
  (turn-on-auto-fill) ;;; Auto-Fill is necessary for mails
  (flyspell-mode)
  (setq-default fill-column 79)
  (turn-on-font-lock) ;;; Font-Lock is always cool *g*
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*")
                      ;;; Kills quoted sigs.
  (not-modified)      ;;; We haven't changed the buffer, haven't we? *g*
  (mail-text)         ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil)
                      ;;; No backups necessary.
)

(or (assoc "mutt-" auto-mode-alist)
    (setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist)))

(add-hook 'mail-mode-hook 'das-mail-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remind Macro
(defun insert-remind-entry ()
  ""
  (interactive)
  (insert (format-time-string "rem %Y %b %d at %H:00 +5 duration 0:30 tag none msg "
(current-time))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python-mode)
(add-hook 'python-mode-hook
          (lambda ()
	      (require 'python-pep8)
	      (require 'python-pylint)
	      (require 'virtualenv)
	      (progn
                (setq py-shell-name "ipython")
                (setq py-python-command "ipython")
                (setq py-indent-offset 4)
                (set-variable 'indent-tabs-mode nil)
                (setq py-smart-indentation nil))))
