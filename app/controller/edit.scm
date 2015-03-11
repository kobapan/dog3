(use srfi-19) ; sxml:string->xml
(use srfi-13) ; string-take
(use sxml.tools)
(use util.list)
(use mail.smtp)

(define host "")
(define user "")
(define pass "")

(define (indexAction params)
  (render #`",|VIEW-DIR|edit.tmpl" '()))

(define (send params)
  (let* ((from (cgi-get-parameter "from" params :default ""))
         (to (cgi-get-parameter "to" params :default ""))
         (subject (cgi-get-parameter "subject" params :default ""))
         (body (cgi-get-parameter "body" params :default "")))
    (send-mail host 25 from to subject body)
    (render #`",|VIEW-DIR|sended.tmpl" '())))


;; Local variables:
;; mode: gauche
;; end:
