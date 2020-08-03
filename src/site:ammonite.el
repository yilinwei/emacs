;;; -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'rx)

(defvar site:ammonite-buffer "*ammonite*")
(defvar site:ammonite-completion-buffer "*ammonite-completion*")
(defvar site:ammonite-bin "amm")

(defvar site:comint--redirect-stack 0)

(defconst site:ammonite--ansi-re
  '(group (or ?\233 (and ?\e ?\[))
      (zero-or-more (char (?0 . ?\?)))
      (zero-or-more (char ?\s ?- ?\/))
      (char (?@ . ?~))))

(defconst site:ammonite--prompt-re
  (rx
   line-start
   (zero-or-more (eval site:ammonite--ansi-re))
   "@ "
   ))

(cl-defun site:comint-redirect-filter (&rest args)
  (if (<= 0 site:comint--redirect-stack)
      (apply 'comint-redirect-filter args)
    (setq site:comint--redirect-stack (1- site:comint--redirect-stack))))

(cl-defun site:ammonite--completion-candidates (str)
  (cl-letf*
      ((comint-buffer (current-buffer))
       (proc (get-buffer-process comint-buffer))
       (candidate-buffer (get-buffer-create site:ammonite-completion-buffer))
       ;; If we use minibuffer completion
       (inhibit-quit nil))
    (with-current-buffer candidate-buffer
      (erase-buffer)
      (comint-redirect-setup
       ;; Bit of a hack because redirect doesn't run the ansi escape function
       (current-buffer) comint-buffer site:ammonite--prompt-re nil)

      (setq site:comint--redirect-stack 1)
      (add-function :around (process-filter proc) #'comint-redirect-filter)

      ;; tab complete
      (process-send-string
       proc
       (concat str "\t"))

      (set-buffer comint-buffer)
      (while (and (null comint-redirect-completed)
      		  (accept-process-output proc)))
      (set-buffer candidate-buffer)
      (ansi-color-apply-on-region (point-min) (point-max))

      (previous-line)
      (split-string
       (buffer-substring-no-properties (line-beginning-position) (line-end-position))
       " "
       t))))

(cl-defun site:ammonite ()
  (interactive)
  (switch-to-buffer-other-window
   (with-current-buffer
       (get-buffer-create site:ammonite-buffer)
     (site:ammonite-mode)
     (current-buffer))))

(defvar site:ammonite-mode-map
  (let
      ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-i") 'complete-symbol)
    map))

(cl-defun site:ammonite-completion-function ()
  (let*
      ((bounds (bounds-of-thing-at-point 'word))
       (start (car bounds))
       (end (cdr bounds))
       (str (buffer-substring-no-properties start end)))
    (list start end (site:ammonite--completion-candidates str))))

(define-derived-mode site:ammonite-mode comint-mode
  "amm"
  :after-hook
  (progn
    (setq-local comint-process-echoes t)
    (setq-local ansi-color-for-comint-mode t)
    (add-to-list 'comint-dynamic-complete-functions 'site:ammonite-completion-function)
    (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
    (setq-local comint-prompt-regexp (rx line-start "@ "))
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-read-only t)
    ;;(add-to-list 'completion-at-point-functions 'site:ammonite-completion-at-point-cached)
    (make-comint-in-buffer
     "amm"
     (current-buffer)
     site:ammonite-bin)))

(provide 'site:ammonite)
