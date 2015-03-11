;; engine.scm -
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
;;  $Id:$
;; 

(define-module core.engine
  (use www.cgi)
  (use text.html-lite)
  (export run))
(select-module core.engine)

(define (run params)
     (let* ((controller (cgi-get-parameter "controller" params :default "inbox"))
            (action (cgi-get-parameter "action" params :default "indexAction"))
            (error-if-not-found #f))
       (load #`",|CONTROLLER-DIR|,|controller|")
       `(,(cgi-header
           :content-type #`"text/html; char-set=,(gauche-character-encoding)")
         ,(html:html
           (html:head (html:meta :http-equiv "Content-type"
                                 :content "text/html"
                                 :charset #`",(gauche-character-encoding)")
                      (html:title "INBOX")
                      (html:link :type "text/css" :rel "stylesheet" :media "screen" :href "css/style.css"))
           (html:body (eval (read-from-string #`"(,|action| (quote ,|params|))") #t))))))

(provide "core.engine")

;; engine.scm -
