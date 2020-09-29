;; -*- lexical-binding: t; -*-

(require 'rx)

(defgroup rackunit
  nil
  "Rackunit customization."
  ;; TODO: Proper category
  )

(defcustom rackunit-program "raco"
  "Program to run for rackunit"
  :type 'string
  :group 'rackunit)

(defun rackunit-goto ()
  (interactive)
  (let
      ((marker
	(get-text-property (point) 'file-pos)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(defun rackunit--process-filter (proc str)
  (let
      ((lines (split-string str "\n"))
       (location (rx
		  line-start
		  "location:"
		  (0+ whitespace)
		  (group (1+ (or alnum ?\. ?\-)))
		  ?\:
		  (group (1+ num))
		  ?\:
		  (group (1+ num)))))
      (cl-loop
       for line in lines
       do
       (with-current-buffer (process-buffer proc)
	 (cond ((string-match location line)
		(cl-flet
		    ((group (num) (match-string-no-properties num line)))
		  (let*
	 	      ((file (group 1))
	 	       (line-pos (string-to-number (group 2)))
	 	       (column-pos (string-to-number (group 3)))
	 	       (*buffer* (get-file-buffer (concat default-directory file)))
		       ;;TODO: Is there a better method?
	 	       (pos (save-excursion
	 		      (with-current-buffer *buffer*
	 			(goto-char (point-min))
	 			(forward-line line-pos)
	 			(forward-char column-pos)
	 			(point))))
	 	       (marker (set-marker
	 			(make-marker)
	 			pos
	 			*buffer*)))
		    (add-text-properties
	 	     (point)
	 	     (progn
	 	       (insert (format "%s:%s:%s" file line-pos column-pos))
	 	       (point))
	 	     (list 'file-pos marker)))))
	       (t (insert line)))
	 (newline)))))

;;;###autoload
(defun rackunit-test ()
  "Run the rackunit tests in the `current-buffer'."
  (interactive)
  (let
      ((*buffer* (get-buffer-create "*raco test*"))
       (file-name buffer-file-name)
       (dir default-directory))
    (with-current-buffer *buffer*
      (erase-buffer)
      (setq default-directory dir)
      (let
	  ((proc
	    (make-process
	     :name rackunit-program
	     :buffer *buffer*
	     :command
	     (list rackunit-program "test" file-name)
	     :filter #'rackunit--process-filter))))
      (switch-to-buffer-other-window *buffer*))))

(provide 'rackunit)
