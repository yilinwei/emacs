;; -*- lexical-binding: t; -*-

(require 'smtpmail)

(defconst site:smtpmail-gmail-server "smtp.gmail.com")
(defconst site:smtpmail-gmail-port 587)

(cl-defun site:smtpmail--set-gmail (mail-address)
  "Set the `smtpmail' settings to gmail, with using MAIL-ADDRESS."
  (let
      ((server site:smtpmail-gmail-server)
       (port site:smtpmail-gmail-port))
    (setq
     message-send-mail-function 'smtpmail-send-it
     starttls-use-gnutls t
     smtpmail-starttls-credentials `((,server ,port nil nil))
     smtpmail-auth-credentials
     `((,server ,port ,mail-address nil))
     smtpmail-smtp-server ,server
     smtpmail-smtp-service ,port)))

(provide 'site:smtpmail)
