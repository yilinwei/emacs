;; -*- lexical-binding: t; -*-

(cl-defun site:mu4e--set-gmail ()
  "Set `mu4e' variables for gmail."
  (cl-letf*
      ((prefix "/[Gmail]")
       ((symbol-function 'set-folder!)
	(lambda (folder gmail-folder)
	  (let
	      ((sym
		(intern
		 (concat "mu4e-" (symbol-name folder) "-folder"))))
	    (set sym (concat prefix "." (capitalize gmail-folder)))))))
    (set-folder! 'trash "trash")
    (set-folder! 'refile "archive")))

(provide 'site:mu4e)
