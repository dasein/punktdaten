;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Custom Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun custom-autoload (&rest args))
;; Define the load-path
(let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-user-dir "~/.emacs.d/packages")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (progn (message "installing %s" package)
            (package-refresh-contents)
            (package-install package))))
 '(
   auto-complete
   blacken
   counsel
   gptel
   go-autocomplete
   go-mode
   go-eldoc
   elpy
   flycheck
   forge
   ivy
   magit
   forge
   flycheck
   rego-mode
   swiper
   smex
   tramp
   use-package
   virtualenv
   yasnippet
   which-key
   zenburn-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Some Basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Remap forward and backward word
(global-set-key (kbd "C-b") 'backward-word)
(global-set-key (kbd "C-f") 'forward-word)

;; Refresh buffer if file change on disk
(global-auto-revert-mode t)

;; Turn on font-lock mode for Emacs
(global-font-lock-mode t)

;; Invoke M-X without alt key
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; utf-8 preferred
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Use emacs ls
(setq dired-use-ls-dired nil)

;; Turn off backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; remove trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; Line-wrapping
(set-default 'fill-column 75)

;; No tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Fix copy/paste
(electric-indent-mode 0)

;; Turn on line and column numbers
;; (setq line-number-mode t)
(setq column-number-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)

;; Gotta see matching parens
(show-paren-mode t)

;; No Scroll Bar
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; No beep
(setq visible-bell t)

;; Mark region when selecting
(setq transient-mark-mode t)

;; Show file size
(setq size-indication-mode t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Save registers etc.
(desktop-save-mode)

;; Always start server
(load "server")
(unless (server-running-p) (server-start))

;; Where I keep secrets for emacs
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

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
(defvar zenburn-override-colors-alist
  '(("zenburn-bg" . "#505050")
    ("zenburn-fg" . "#DCDCCC")))
(load-theme 'zenburn t)

;; Split windows evenly for multiple file visits
(setq split-height-threshold 0)
(setq split-width-threshold nil)

(defadvice server-visit-files
  (around server-visit-files-custom-find
      activate compile)
  "Maintain a counter of visited files from a single client call."
  (let ((server-visit-files-custom-find:buffer-count 0))
    ad-do-it))

(defun server-visit-hook-custom-find ()
  "Arrange to visit the files from a client call in separate windows."
  (if (zerop server-visit-files-custom-find:buffer-count)
      (progn
    (delete-other-windows)
    (switch-to-buffer (current-buffer)))
    (let ((buffer (current-buffer))
      (window (split-window-sensibly)))
      (switch-to-buffer buffer)
      (balance-windows)))
  (setq server-visit-files-custom-find:buffer-count
    (1+ server-visit-files-custom-find:buffer-count)))
(add-hook 'server-visit-hook 'server-visit-hook-custom-find)

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
;; Help modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show the help buffer after startup
(add-hook 'after-init-hook 'help-quick)

;; shows a popup of available keybindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completions-max-height 10)                     ; This is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)
(setq completion-auto-select 'second-tab)            ; Much more eager
;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
   ("C-'" . ivy-avy))
  (:map ivy-minibuffer-map
   ("C-o" . ivy-dispatching-done))
  :config
  (ivy-mode 1)
  ;; Wrap around search results
  (setq ivy-wrap t)
  ;; Current and total number in collection prompt
  (setq ivy-count-format "(%d/%d) ")
  ;; add `recentf-mode` and bookmarks to `ivy-switch-buffer`
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 20)
  ;; does not count candidates
  ;;(setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))))


(defun counsel-lookup-point ()
  (interactive)
  (ivy-lookup-point 'counsel-ag))

(defun relative-counsel-ag ()
  (interactive)
  (counsel-ag "" default-directory))

(defun gitroot-counsel-fzf ()
  (interactive)
  (let ((git-dir (locate-dominating-file default-directory ".git")))
    (if git-dir
        (counsel-fzf "" git-dir "")
      (counsel-fzf))))

(defun ivy-lookup-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

(defun ivy-lookup-repos ()
  (interactive)
  (let* ((repos-dir (read-directory-name "Search repos in directory: " "~/work"))
         (default-directory repos-dir)
         (git-dirs (split-string
                   (shell-command-to-string
                    "find . -maxdepth 5 -name .git -type d -prune -print")
                   "\n" t))
         (repo-dirs (mapcar (lambda (git-dir)
                             (file-name-directory git-dir))
                           git-dirs)))
    (if repo-dirs
        (ivy-read "Select repository: " repo-dirs
                  :action '(1
                            ("o" (lambda (dir)
                                   (let ((repo-path (expand-file-name dir repos-dir)))
                                     (dired repo-path)
                                     (magit-status repo-path)))
                             "open dired and magit status")
                            ("d" (lambda (dir)
                                   (dired (expand-file-name dir repos-dir)))
                             "open dired only")
                            ("m" (lambda (dir)
                                   (let ((repo-path (expand-file-name dir repos-dir)))
                                     (magit-status repo-path)))
                             "open magit status only")
                            ("f" (lambda (dir)
                                   (let ((repo-path (expand-file-name dir repos-dir)))
                                     (counsel-find-file "" repo-path)))
                             "find file in repository")))
      (message "No git repositories found in %s" repos-dir))))


;;(setenv "FZF_DEFAULT_COMMAND" "ag --ignore .git/ -Ul")
(setenv "FZF_DEFAULT_COMMAND" "rg --files --hidden --follow --glob '!.git'")

(global-set-key (kbd "C-c m") 'counsel-M-x)
(global-set-key (kbd "C-l") 'ivy-backward-delete-char)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c a") 'counsel-rg)
(global-set-key (kbd "C-c s") 'counsel-ag)
(global-set-key (kbd "C-c d") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'relative-counsel-ag)
(global-set-key (kbd "C-c j") 'counsel-lookup-point)
(global-set-key (kbd "C-c l") 'gitroot-counsel-fzf)
(global-set-key (kbd "C-c r") 'ivy-lookup-repos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; Enable Flycheck
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;(add-hook 'elpy-mode-hook (lambda () (add-hook 'before-save-hook 'blacken-buffer nil t)))
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

(setq python-shell-interpreter "ipython3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'yasnippet)

(add-hook 'go-mode-hook 'auto-complete-mode
          (lambda ()
            (ac-go-expand-arguments-into-snippets "yes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :config
  (setq magit-define-global-key-bindings 'recommended)
  (setq magit-push-current-set-remote-if-missing nil)
  (setq magit-branch-pull-margin nil)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit
  :config
  ;; Go back 90 days for default forge PR history
  (setq forge--initial-topic-until "2025-05-01")

  ;; (setq forge--initial-topic-until
  ;;       (format-time-string "%Y-%m-%d"
  ;;                           (time-subtract (current-time)
  ;;                                        (days-to-time 90))))

  ;; Filter PRs in magit status buffer to show only those assigned to me
  (let ((topic-filter (forge--topics-spec :type 'topic
                                          :active t
                                          :state 'open
                                          :assignee "hpfennig"
                                          :order 'recently-updated)))
  (setq forge-status-buffer-default-topic-filters topic-filter
        forge-list-buffer-default-topic-filters topic-filter))

  ;; Remove other sections from status buffer if desired
  (remove-hook 'magit-status-sections-hook 'forge-insert-issues)
  (remove-hook 'magit-status-sections-hook 'forge-insert-notifications)

  ;; Customize PR format to show author and assignee
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title nil)
          ("State" 6 t nil state nil)
          ("Author" 15 t nil author nil)
          ("Assignee" 15 t nil assignee nil)
          ("Updated" 10 t nil updated nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c C-c") 'gptel)
(global-set-key (kbd "C-c i") 'gptel-menu)

(use-package gptel
  :config
  (define-key gptel-mode-map (kbd "C-c RET") 'gptel-menu)
  (setq gptel-model 'claude-sonnet-4
        gptel-backend (gptel-make-gh-copilot "Copilot")))

(gptel-make-ollama "local-llama"
  :host "hpfennig-dt.internal:11434"
  :stream t
  :models '("llama3.1:8b"))

(gptel-make-ollama "local-mistral"
  :host "hpfennig-dt.internal:11434"
  :stream t
  :models '("mistral:latest"))

(gptel-make-gh-copilot "Copilot")


;; Preset prompts
(gptel-make-preset 'c-refactor
  :description "Refactor code to improve readability and performance"
  :system "Refactor the provided code to improve readability, maintainability, and performance. Apply best practices and design patterns appropriate for the language. Explain your key changes using code comments inline with the code."
  :temperature 0.2)

(gptel-make-preset 'c-comment
  :description "Generate comments for code"
  :system "Generate comprehensive comments for the provided code to explain what the code is doing. The goal is not to be verbose, but to provide clarity for complex code sections and to provide a general overview when read, as to what the code is doing. Follow documentation best practices for the language."
  :temperature 0.3)

(gptel-make-preset 'c-document
  :description "Generate documentation for code"
  :system "Generate comprehensive documentation for the provided code. Include function descriptions, parameter explanations, return values, and usage examples. Follow documentation best practices for the language."
  :temperature 0.3)

(gptel-make-preset 'c-architect
  :description "Design code architecture"
  :system "Design a clean, maintainable architecture for the described programming task. Include component breakdown, interfaces, data flow, and design patterns. Consider scalability and future extensibility."
  :temperature 0.7)

(gptel-make-preset 'c-optimize
  :description "Optimize code for performance"
  :system "Analyze the provided code for performance bottlenecks and optimize it. Focus on algorithmic improvements, memory usage, and execution speed. Explain your optimization strategy."
  :temperature 0.2)

(gptel-make-preset 'c-debug
  :description "Debug problematic code"
  :system "Analyze the provided code for bugs, edge cases, or potential issues. Suggest fixes and explain the reasoning behind each problem identified."
  :temperature 0.3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remind Macro
(defun insert-remind-entry ()
  ""
  (interactive)
  (insert (format-time-string "rem %Y %b %d at %H:00 +5 duration 0:30 tag none msg "
(current-time))))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))
