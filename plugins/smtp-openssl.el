;;; smtp-open-ssl.el --- Allow smptmail to use openssl

;; This file is not part of Emacs

;; Author: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
;; Website: http://www.russet.org.uk

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. 

;;; Commentary:
;;
;; Many SMTP servers now require a secure channel of access for
;; interaction. Currently, smtpmail supports connection via a TLS
;; channel using the external gnutls package. This is opposed to Gnus
;; IMAP support which uses openssl. 
;;
;; Unfortunately, gnutls uses process signalling for out-of-band
;; communication. So, for example, the cygwin gnutls won't work with
;; NTEmacs, while openssl works fine. I attempting to hack this in (by
;; calling cygwin "kill" to do process handling), but failed. 
;;
;; This file adds support to smptmail.el to use openssl instead of
;; gnutls. 
;;
;; NOTE that this file overwrites existing smtpmail code and so should
;; be classed as a hack. There's also a dodgy bit in the middle where
;; I wait for an openssl response to have happened, rather than
;; checking it. This should integrate directly into smptmail.el then
;; it should work fine.

;;; Usage
;;
;; Put (require 'smtp-openssl) into your .emacs. Then configure smtpmail as normal. Say..

;; (setq smtpmail-smtp-server "smtp.domain.com")
;; (setq smtpmail-smtp-service 465)
;; (setq send-mail-function 'smtpmail-send-it)


;; (setq smtpmail-auth-credentials  ; or use ~/.authinfo
;;      '(("smtp.domain.com" 465 "username" "password")))


;;; Code

(require 'smtpmail)

;; options
(defvar smtp-openssl-active t
  "Use openssl as opposed to gnutls for TLS communication")


(defvar smtp-openssl-wait-delay 1
  "Arbitrary hack wait for openssl response to return")

;; we can advice around smtpmail-open-stream, so do this rather than overwrite it. 

(defadvice smtpmail-open-stream
  (around smtp-openssl-advice-open activate)
  
  ;; we want to use openssl and for this stream we should be using
  ;; ssl. 
  (let ((process-buffer (ad-get-arg 0))
        (host (ad-get-arg 1))
        (port (ad-get-arg 2)))

    (if (and smtp-openssl-active
             (smtpmail-find-credentials
              smtpmail-starttls-credentials host port))
        ;; check return to see if process is sane. 
        (progn 
          (setq ad-return-value
                (start-process 
                 ;; open the process a buffer
                 "SMTP" process-buffer "openssl"
                 ;; use client mode for tunnelling
                 "s_client" 
                 ;; be quiet about it, or any ssl communication stuff will
                 ;; happen in band
                 "-quiet" 
                 ;; send the starttls negotation stuff with smtp protocol
                 "-starttls" "smtp" 
                 ;; use tls1 and/or connection
                 "-tls1" "-connect" 
                 (format "%s:%s" host port)))
          ;; dodgy bit -- wait for a short time for response to
          ;; complete.  otherwise, we get what appears to be a race
          ;; condition, as I've hacked out the wait in the
          ;; smtpmail-via-smtp function
          (sit-for smtp-openssl-wait-delay)
          (save-excursion 
            (set-buffer process-buffer)
            (erase-buffer)))
      ;; just do the normal thing instead. 
      ad-do-it)))

 

;; unfortunately, we can't advice around smtpmail-via-smtp. It is
;; expecting to process the HELO. But openssl has already done this
;; piece of negotation for us. So it hangs, waiting for a reply which
;; is not going to come.

;; this is the bit which is going to go into smtpmail-via-smtp

;; So, we need to hack out the bit which is waiting for it. 
(defun smtpmail-via-smtp (recipient smtpmail-text-buffer)
  (let ((process nil)
	(host (or smtpmail-smtp-server
		  (error "`smtpmail-smtp-server' not defined")))
	(port smtpmail-smtp-service)
        ;; smtpmail-mail-address should be set to the appropriate
        ;; buffer-local value by the caller, but in case not:
        (envelope-from (or smtpmail-mail-address
                           (and mail-specify-envelope-from
                                (mail-envelope-from))
                           user-mail-address))
	response-code
	greeting
	process-buffer
	(supported-extensions '()))
    (unwind-protect
	(catch 'done
	  ;; get or create the trace buffer
	  (setq process-buffer
		(get-buffer-create (format "*trace of SMTP session to %s*" host)))

	  ;; clear the trace buffer of old output
	  (with-current-buffer process-buffer
	    (setq buffer-undo-list t)
	    (erase-buffer))

	  ;; open the connection to the server
	  (setq process (smtpmail-open-stream process-buffer host port))
	  (and (null process) (throw 'done nil))

	  ;; set the send-filter
	  (set-process-filter process 'smtpmail-process-filter)

	  (with-current-buffer process-buffer
	    (set-buffer-process-coding-system 'raw-text-unix 'raw-text-unix)
	    (make-local-variable 'smtpmail-read-point)
	    (setq smtpmail-read-point (point-min))
            
            
            ;;; PWL, this is the hacked bit
            
            ;; check to see if we are using openssl
            (unless (and smtp-openssl-active
                         (smtpmail-find-credentials
                          smtpmail-starttls-credentials host port))
              ;; read the response as normal
              (if (or (null (car (setq greeting (smtpmail-read-response process))))
                      (not (integerp (car greeting)))
                      (>= (car greeting) 400))
                  (throw 'done nil)
                ))
            

	    (let ((do-ehlo t)
		  (do-starttls t))
	      (while do-ehlo
	    ;; EHLO
	    (smtpmail-send-command process (format "EHLO %s" (smtpmail-fqdn)))

	    (if (or (null (car (setq response-code
				     (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(progn
                  ;; HELO
		  (smtpmail-send-command
		   process (format "HELO %s" (smtpmail-fqdn)))
                  
                  
		  (if (or (null (car (setq response-code
					   (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil)))

              (dolist (line (cdr (cdr response-code)))
		(let ((name
		       (with-case-table ascii-case-table
			 (mapcar (lambda (s) (intern (downcase s)))
				 (split-string (substring line 4) "[ ]")))))
		  (and (eq (length name) 1)
		       (setq name (car name)))
		  (and name
		       (cond ((memq (if (consp name) (car name) name)
	 			    '(verb xvrb 8bitmime onex xone
					   expn size dsn etrn
					   enhancedstatuscodes
					   help xusr
					   auth=login auth starttls))
			      (setq supported-extensions
				    (cons name supported-extensions)))
			     (smtpmail-warn-about-unknown-extensions
			      (message "Unknown extension %s" name)))))))

	    (if (and do-starttls
		     (smtpmail-find-credentials smtpmail-starttls-credentials host port)
		     (member 'starttls supported-extensions)
		     (numberp (process-id process)))
		(progn
		  (smtpmail-send-command process (format "STARTTLS"))
		  (if (or (null (car (setq response-code (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))
		  (starttls-negotiate process)
		  (setq do-starttls nil))
	      (setq do-ehlo nil))))

	    (smtpmail-try-auth-methods process supported-extensions host port)

	    (if (or (member 'onex supported-extensions)
		    (member 'xone supported-extensions))
		(progn
		  (smtpmail-send-command process (format "ONEX"))
		  (if (or (null (car (setq response-code (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    (if (and smtpmail-debug-verb
		     (or (member 'verb supported-extensions)
			 (member 'xvrb supported-extensions)))
		(progn
		  (smtpmail-send-command process (format "VERB"))
		  (if (or (null (car (setq response-code (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    (if (member 'xusr supported-extensions)
		(progn
		  (smtpmail-send-command process (format "XUSR"))
		  (if (or (null (car (setq response-code (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    ;; MAIL FROM:<sender>
	    (let ((size-part
		   (if (or (member 'size supported-extensions)
			   (assoc 'size supported-extensions))
		       (format " SIZE=%d"
			       (with-current-buffer smtpmail-text-buffer
				 ;; size estimate:
				 (+ (- (point-max) (point-min))
				    ;; Add one byte for each change-of-line
				    ;; because of CR-LF representation:
				    (count-lines (point-min) (point-max)))))
		     ""))
		  (body-part
		   (if (member '8bitmime supported-extensions)
		       ;; FIXME:
		       ;; Code should be added here that transforms
		       ;; the contents of the message buffer into
		       ;; something the receiving SMTP can handle.
		       ;; For a receiver that supports 8BITMIME, this
		       ;; may mean converting BINARY to BASE64, or
		       ;; adding Content-Transfer-Encoding and the
		       ;; other MIME headers.  The code should also
		       ;; return an indication of what encoding the
		       ;; message buffer is now, i.e. ASCII or
		       ;; 8BITMIME.
		       (if nil
			   " BODY=8BITMIME"
			 "")
		     "")))
;	      (smtpmail-send-command process (format "MAIL FROM:%s@%s" (user-login-name) (smtpmail-fqdn)))
	      (smtpmail-send-command process (format "MAIL FROM:<%s>%s%s"
                                                     envelope-from
						     size-part
						     body-part))

	      (if (or (null (car (setq response-code (smtpmail-read-response process))))
		      (not (integerp (car response-code)))
		      (>= (car response-code) 400))
		  (throw 'done nil)
		))

	    ;; RCPT TO:<recipient>
	    (let ((n 0))
	      (while (not (null (nth n recipient)))
		(smtpmail-send-command process (format "RCPT TO:<%s>" (smtpmail-maybe-append-domain (nth n recipient))))
		(setq n (1+ n))

		(setq response-code (smtpmail-read-response process))
		(if (or (null (car response-code))
			(not (integerp (car response-code)))
			(>= (car response-code) 400))
		    (throw 'done nil)
		  )
		))

	    ;; DATA
	    (smtpmail-send-command process "DATA")

	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;; Mail contents
	    (smtpmail-send-data process smtpmail-text-buffer)

	    ;;DATA end "."
	    (smtpmail-send-command process ".")

	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;;QUIT
;	    (smtpmail-send-command process "QUIT")
;	    (and (null (car (smtpmail-read-response process)))
;		 (throw 'done nil))
	    t ))
      (if process
	  (with-current-buffer (process-buffer process)
	    (smtpmail-send-command process "QUIT")
	    (smtpmail-read-response process)

;	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
;		    (not (integerp (car response-code)))
;		    (>= (car response-code) 400))
;		(throw 'done nil)
;	      )
	    (delete-process process)
	    (unless smtpmail-debug-info
	      (kill-buffer process-buffer)))))))

                           


(provide 'smtp-openssl)


