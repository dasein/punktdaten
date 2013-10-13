;;; $Id$ 

(defun remind-mode ()
  "Mode for highlighting remind files"
  (interactive)
  (setq mode-name "remind"
	major-mode 'remind)

  (require 'font-lock)
  (make-local-variable 'font-lock-defaults)
  (setq remind-font-lock-keywords
	(list 
	 ; basic
	 '("#.*"       
	   . font-lock-comment-face)

	 ; rem
	 '("\\([Rr][Ee][Mm]\\) \\(.*\\) \\([Mm][Ss][Gg]\\) \\(.+\\)"
	   (1 font-lock-keyword-face)
	   (2 font-lock-variable-name-face)
	   (3 font-lock-keyword-face)
	   (4 font-lock-string-face)
           )

	 ))
  (setq font-lock-defaults '(remind-font-lock-keywords t))
  (font-lock-mode t)
  (setq comment-start "# "))
