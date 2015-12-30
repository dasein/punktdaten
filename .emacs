;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Custom Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun custom-autoload (&rest args))
;; Define the load-path
(let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(require 'cl)

(setq package-user-dir "~/.emacs.d/packages")

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(defvar preinstall-packages
  '(
    python-pep8
    python-pylint
    python-mode
    jedi
    helm
    xcscope
    virtualenv
    zenburn-theme)
    "A list of packages to ensure are installed at launch.")

(defun preinstall-packages-installed-p ()
  (loop for p in preinstall-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (preinstall-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p preinstall-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'preinstall-packages)

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

;; load my theme
(load-theme 'zenburn t)

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
;; Helm mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)

(if (eq system-type 'darwin) (setq helm-locate-fuzzy-match nil))
(setq helm-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -onlyin ~ %s %s")
        (t "locate %s")))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cscope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All keybindings use the "C-c s" prefix:

;;  | C-c s s | Find symbol.                                                   |
;;  | C-c s = | Find assignments to this symbol                                |
;;  | C-c s d | Find global definition.                                        |
;;  | C-c s g | Find global definition (alternate binding).                    |
;;  | C-c s G | Find global definition without prompting.                      |
;;  | C-c s c | Find functions calling a function.                             |
;;  | C-c s C | Find called functions (list functions called from a function). |
;;  | C-c s t | Find text string.                                              |
;;  | C-c s e | Find egrep pattern.                                            |
;;  | C-c s f | Find a file.                                                   |
;;  | C-c s i | Find files #including a file.                                  |

;;  | C-c s a | Set initial directory.   |
;;  | C-c s A | Unset initial directory. |

;; These pertain to navigation through the search results:

;;  | C-c s b | Display *cscope* buffer.             |
;;  | C-c s B | Auto display *cscope* buffer toggle. |
;;  | C-c s n | Next symbol.                         |
;;  | C-c s N | Next file.                           |
;;  | C-c s p | Previous symbol.                     |
;;  | C-c s P | Previous file.                       |
;;  | C-c s u | Pop mark.                            |

;; These pertain to cscope database maintenance:

;;  | C-c s L | Create list of files to index.                                              |
;;  | C-c s I | Create list and index.                                                      |
;;  | C-c s E | Edit list of files to index.                                                |
;;  | C-c s W | Locate this buffer's cscope directory ("W" --> "where").                    |
;;  | C-c s S | Locate this buffer's cscope directory. (alternate binding: "S" --> "show"). |
;;  | C-c s T | Locate this buffer's cscope directory. (alternate binding: "T" --> "tell"). |
;;  | C-c s D | Dired this buffer's directory.                                              |

(require 'xcscope)
(cscope-minor-mode t)
(setq cscope-close-window-after-select t)
(setq cscope-do-not-update-database t)
