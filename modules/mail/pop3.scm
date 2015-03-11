;; -*- coding: utf-8 -*-
;; pop3.scm -
;; 
;; Copyright (c) 2009 kobapan <kobapan<at>gmail.com>
;; 
;;   Redistribution and use in source and binary forms, with or without
;;   modification, are permitted provided that the following conditions
;;   are met:
;; 
;;   1. Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;; 
;;   2. Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;; 
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;; 
;;  $Id: pop3.scm 10 2009-03-13 16:12:51Z kobapan $
;; 

;; USAGE
;; (add-load-path "..")
;; (use mail.pop3)
;; (define mp3 (pop3-connect "host" 110))
;; (pop3-auth mp3 "user" "pass")
;; (pop3-list mp3) => (("1" "125") ("2" "1048") ...)
;; (pop3-extract-one mp3 1 10)
;; (pop3-extract-many mp3 '(1 2) 10)
;;
;; (pop3-read-one mp3 1)
;;
;; (pop3-delete-one mp3 1)
;; (pop3-delete-many mp3 '(1 2))
;; (slot-ref mp3 'deleted-mail-numbers)
;;
;; (pop3-close mp3)


(define-module mail.pop3
  (use gauche.net) ; socket
  (use srfi-1) ; list
  (use srfi-13) ; string
  (use rfc.822) ; mail format
  (use rfc.mime) ; mime header
  (use gauche.charconv)

  (use logger)

  (export pop3-connect  pop3-close
          pop3-auth pop3-list
          pop3-read-one pop3-read-many
          pop3-store-one
          pop3-extract-one pop3-extract-many %send-command %read-all-lines %b-handler
          pop3-delete-one pop3-delete-many))
(select-module mail.pop3)

(define-class <pop3-connection> ()
  ((socket :getter socket-of
           :init-keyword :socket)
   (input-port :getter input-port-of
               :init-keyword :input-port)
   (output-port :getter output-port-of
                :init-keyword :output-port)
   (deleted-mail-numbers :accessor dele-mail-no-of
                         :init-value '())))
;; connection

(define (pop3-connect host port)
  (let ((socket (make-client-socket 'inet host port)))
    (make <pop3-connection>
      :socket socket
      :input-port (socket-input-port socket)
      :output-port (socket-output-port socket))))

(define-method pop3-close ((conn <pop3-connection>))
  "dele command will confirmed and update server status"
  (%send-command conn "QUIT")
  (set! (dele-mail-no-of conn) '())
  (socket-close (socket-of conn)))

(define-method pop3-auth ((conn <pop3-connection>) (user <string>) (pass <string>))
  (%send-command conn #f)
  (%send-command conn #`"USER ,user")
  (%send-command conn #`"PASS ,pass"))

;; list

(define-method pop3-list ((conn <pop3-connection>))
  "get return of LIST
\(\(mail-number mail-size) (mail-number mail-size) ...)"
  (let ((iport (input-port-of conn))
        (how-many-mail (x->integer (cadr (string-split (%send-command conn "STAT") " ")))))
    (%send-command conn "LIST")
    (%read-all-lines iport '())))

;; read mail

(define-method pop3-read-one ((conn <pop3-connection>) mail-number)
  "get whole one mail with mail-number"
  (let ((iport (input-port-of conn))
        (mail-number (%x->string mail-number)))
    (%send-command conn #`"RETR ,mail-number")
    (let* ((headers (rfc822-read-headers iport)))
      (list (%h "message-id" headers) (%h "from" headers) (%h "subject" headers) (%h "date" headers) (%b headers iport)))))

(define-method pop3-read-many ((conn <pop3-connection>) mail-numbers)
  "get mail headers with envelopes list"
  (let ((iport (input-port-of conn)))
    (map (lambda (mail-number)
           (pop3-read-one conn mail-number))
         mail-numbers)))

;; store

(define-method pop3-store-one ((conn <pop3-connection>) mail-number)
  "get whole one mail with mail-number"
  (let ((iport (input-port-of conn))
        (mail-number (%x->string mail-number)))
    (%send-command conn #`"RETR ,mail-number")
    (let* ((str (%read-all-lines iport ""))
           (id (cadr (assoc "message-id" (rfc822-read-headers (open-input-string str))))))
      (call-with-output-file #`"../dat/,id"
        (cut display str <>)))))


;; delete mail

(define-method pop3-delete-one ((conn <pop3-connection>) mail-number)
  "delete one mail with mail-number"
  (let ((iport (input-port-of conn))
        (mail-number (%x->string mail-number)))
    (%send-command conn #`"DELE ,mail-number")
    (set! (dele-mail-no-of conn) (cons mail-number (dele-mail-no-of conn)))))

(define-method pop3-delete-many ((conn <pop3-connection>) (mail-numbers <list>))
  "delete one mail with mail-numbers list"
  (let ((iport (input-port-of conn)))
    (map (cut pop3-delete-one conn <>) mail-numbers)))

;; header 

(define-method pop3-extract-one ((conn <pop3-connection>) mail-number . how-many-lines)
  "get one mail header with mail-number"
  (let ((iport (input-port-of conn))
        (mail-number (%x->string mail-number))
        (how-many-lines (if (null? how-many-lines) "6" ; at least 6 lines are needed for mime-part
                            (%x->string (car how-many-lines)))))
    (%send-command conn #`"TOP ,mail-number ,how-many-lines")
    (let* ((headers (rfc822-read-headers iport)))
      (list (%h "message-id" headers) (%h "from" headers) (%h "subject" headers) (%h "date" headers) (%b headers iport)))))

(define-method pop3-extract-many ((conn <pop3-connection>) (mail-numbers <list>) . how-many-lines)
  "get mail headers with mail-numbers list"
  (let ((iport (input-port-of conn)))
    (map (lambda (mail-number)
           (pop3-extract-one conn mail-number (if (null? how-many-lines) "6" (car how-many-lines))))
         mail-numbers)))

;; private

(define-method %send-command ((conn <pop3-connection>) command)
  "send COMMAND to out port and read from in port "
  (let ((iport (input-port-of conn))
        (oport (output-port-of conn)))
    (when command (format oport "~a\r\n" command) (flush oport))
    (let* ((line (read-line iport)))
      (if (#/^\+OK/ line)
          line
          (errorf "pop3-error: ~a => ~a" command line)))))

(define-method %read-all-lines (iport (lis <list>))
  (let* ((line (read-line iport)))
    (if [#/^\.$/ line] lis [%read-all-lines iport (cons (string-split line " ") lis)])))

(define-method %read-all-lines (iport (str <string>))
  (let* ((line (read-line iport)))
    (if [#/^\.$/ line] str [%read-all-lines iport #`",|str|,|line|\r\n"])))

(define (%x->string str)
  (if (string? str) str (x->string str)))

(define (%x->number num)
  (if (number? num) num (x->integer num)))

(define (%read-up iport)
  (if [#/^\.$/ (read-line iport)] #t [%read-up iport]))

(define (%h id headers)
  (cons id (cond [(assoc id headers) => (lambda (x) (mime-decode-text (second x)))] [else ""])))





;; TODO 一件のメールを受け取って、添付がある場合も含めてパースする手続きを作る
;; どういう形で返すのがいいか？	

(define (%b headers iport)
  (let* ((str (%read-all-lines iport ""))
         (body '()))
    (if (mime-parse-version (rfc822-header-ref headers "mime-version"))
        (and (set! body (ref (mime-parse-message (open-input-string str) headers %b-handler) 'content))
             (if (string? body) (cons "body" body) 

                 (and 
                  (%logger::out (ref (second body) 'content)) ;; DEBUG
                  (cons "body" (ref (first body) 'content))
                  )
))
        (cons "body" str))))

(define (%b-handler part-info xport)
  (if (string=? "text" (slot-ref part-info 'type))
      (ces-convert (mime-body->string part-info xport) "ISO2022JP")
      (call-with-output-string
          (cut mime-retrieve-body part-info xport <>))))

;; Epilogue
(provide "mail.pop3")

;; pop3.scm -

