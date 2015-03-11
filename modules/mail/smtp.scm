(define-module mail.smtp
  (use gauche.net) ; socket
  (use rfc.mime)
  (export send-mail))

(select-module mail.smtp)

;; (send-mail "localhost" 25 "from@localhost" "to@localhost" "subject" "Hello!")
(define (send-mail host port mail-from recipients subject body)
  (with-error-handler
   (lambda (e) (errorf "send-mail failed: ~a" (slot-ref e 'message)))
   (lambda ()
     (call-with-client-socket
      (make-client-socket 'inet host port)
      (lambda (in out)
        (let ((send-command 
               (lambda (command code)
                 (when command (format out "~a\r\n" command) (flush out))
                 (let* ((line (read-line in))
                        (return-code (string->number (substring line 0 3))))
                   (if (eq? return-code code)
                       line
                       (errorf "smtp-error: ~a => ~a" command line)))))
              (string-iport (open-input-string body)))
          (send-command #f 220)
          (send-command (format "HELO ~a" (sys-gethostname)) 250)
          (send-command (format "MAIL FROM: <~a>" mail-from) 250)
          (for-each (lambda (rcpt)
                      (send-command (format "RCPT TO: <~a>" rcpt) 250))
                    (if (string? recipients) (list recipients) recipients))
          (send-command "DATA" 354)
          (format out "From: ~a\r\n" mail-from);
          (format out "To: ~a\r\n" recipients);
          (format out "Subject: ~a\r\n" (mime-encode-word subject));
          (format out "X-Mailer: dog3\r\n");
          (format out "\r\n")
          (port-for-each (lambda (line)
                           (if (equal? "." line)
                               (format out "..\r\n")
                               (format out "~a\r\n" line)))
                         (lambda () (read-line string-iport)))
          (send-command "." 250)
          (send-command "QUIT" 221)))))))

;; Epilogue
(provide "mail.smtp")
