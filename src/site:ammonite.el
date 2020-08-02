;;; -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'rx)

(defvar site:ammonite-buffer "*ammonite*")
(defvar site:ammonite-bin "amm")

(define-derived-mode site:ammonite-mode comint-mode
  "amm"
  :after-hook
  (progn
    (setq-local ansi-color-for-comint-mode t)
    (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
    (setq-local comint-prompt-regexp (rx line-start "@ "))
    (setq-local comint-use-prompt-regexp t)
    (make-comint-in-buffer
     "amm"
     (current-buffer)
     site:ammonite-bin)))

(cl-defun site:ammonite ()
  (interactive)
  (switch-to-buffer-other-window
   (with-current-buffer
       (get-buffer-create site:ammonite-buffer)
     (site:ammonite-mode)
     (current-buffer))))
