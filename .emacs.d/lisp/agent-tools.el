;;; agent-tools.el --- GPtel agent tools for coding assistance -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides agent tools for GPtel to enable LLM-based coding assistance.
;; To register tools, call (agent-tools-register-all) after gptel is loaded.

;;; Code:


(require 'cl-lib)
(require 'gptel nil t) ; Optional require - will be checked at runtime
(require 'counsel nil t)

;;; Tool Definitions

(defvar agent-tools-definitions
  '(
    ;; Test tools for development and validation
    (:name "hello_world"
     :description "A simple test tool that returns a greeting message"
     :function (lambda () 
                 (format "Hello, World! Current time: %s" (current-time-string)))
     :args nil
     :category "test")
    
    (:name "hello_world_with_name"
     :description "A test tool that greets a specific person"
     :function (lambda (name)
                 (format "Hello, %s! Current time: %s" name (current-time-string)))
     :args ((:name "name" :type "string" :description "Name to greet"))
     :category "test")
    
    (:name "gptel-list-files"
     :description "Fuzzy/regex file search using ripgrep and fzf filtering. Uses project git root if available."
     :function (lambda (search)
                 (let* ((git-dir (or (and (fboundp 'locate-dominating-file)
                                         (locate-dominating-file default-directory ".git"))
                                    default-directory))
                        (default-directory git-dir)
                        ;; Use ripgrep to list files, then filter with fzf
                        (rg-cmd "rg --files --hidden --follow --glob '!.git' --glob '!node_modules' --glob '!.DS_Store'")
                        (filter-cmd (format "fzf --no-sort --filter='%s'" (shell-quote-argument search)))
                        (full-cmd (concat rg-cmd " | " filter-cmd))
                        (res (shell-command-to-string full-cmd)))
                   ;; Split output into lines, remove empty, ensure we return a list (not nil)
                   (let ((lines (split-string (string-trim res) "\n")))
                     (or (cl-remove-if #'string-empty-p lines) '()))))
     :args ((:name "search" :type "string" :description "Fuzzy or regex search string for files"))
     :category "project")
    )
  "List of agent tool definitions. Each entry is a plist with tool properties.")

;;; Tool Registration

(defun agent-tools-clear-category (category)
  "Clear all tools in CATEGORY from gptel--known-tools."
  (when (boundp 'gptel--known-tools)
    (setq gptel--known-tools 
          (assq-delete-all category gptel--known-tools))))

(defun agent-tools-register-all (&optional force)
  "Register all agent tools with gptel.
If FORCE is non-nil, clear existing tools before registering."
  (interactive "P")
  (unless (featurep 'gptel)
    (user-error "GPtel is not loaded. Please load gptel first"))
  
  (unless (fboundp 'gptel-make-tool)
    (user-error "gptel-make-tool function not available"))
  
  (when force
    (message "Clearing existing test tools...")
    (agent-tools-clear-category "test"))
  
  (message "Registering %d agent tools..." (length agent-tools-definitions))
  
  (let ((registered-count 0))
    (dolist (tool-def agent-tools-definitions)
      (condition-case err
          (progn
            (apply #'gptel-make-tool tool-def)
            (cl-incf registered-count)
            (message "✓ Registered: %s" (plist-get tool-def :name)))
        (error 
         (message "✗ Failed to register %s: %s" 
                  (plist-get tool-def :name) 
                  (error-message-string err)))))
    
    (message "Agent tools registration complete: %d/%d tools registered"
             registered-count (length agent-tools-definitions))
    
    ;; Return count for programmatic use
    registered-count))

;;; Utility Functions

(defun agent-tools-list-tools ()
  "List all registered agent tools."
  (interactive)
  (unless (boundp 'gptel--known-tools)
    (user-error "gptel--known-tools not available"))
  
  (let ((tool-list '()))
    (dolist (category gptel--known-tools)
      (dolist (tool-entry (cdr category))
        (push (format "%s (%s)" (car tool-entry) (car category)) tool-list)))
    
    (if (called-interactively-p 'any)
        (message "Registered tools: %s" 
                 (if tool-list 
                     (mapconcat #'identity (reverse tool-list) ", ")
                   "None"))
      (reverse tool-list))))

(defun agent-tools-info ()
  "Display information about agent tools system."
  (interactive)
  (message "=== Agent Tools System Info ===")
  (message "GPtel loaded: %s" (featurep 'gptel))
  (message "Tool definitions: %d" (length agent-tools-definitions))
  (message "Registered tools: %d" 
           (if (boundp 'gptel--known-tools)
               (apply #'+ (mapcar (lambda (cat) (length (cdr cat))) gptel--known-tools))
             0))
  (when (called-interactively-p 'any)
    (agent-tools-list-tools)))

(provide 'agent-tools)
;;; agent-tools.el ends here
