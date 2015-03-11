#!/usr/local/bin/gosh

(add-load-path "../modules/")

(define-constant CONTROLLER-DIR "../app/controller/")
(define-constant VIEW-DIR "../app/view/")

(use www.cgi)
(use text.html-lite)
(use template)
(use logger)

;;;
;;; Main entry
;;;
(define (main args)
  (cgi-main
   (lambda (params)
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
           (html:body ((eval (read-from-string action) (interaction-environment)) params))))))))

;; Local variables:
;; mode: gauche
;; end:

