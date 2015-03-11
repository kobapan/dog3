(use srfi-19) ; sxml:string->xml
(use srfi-13) ; string-take
(use sxml.tools)
(use util.list)
(use mail.pop3)

(define host "")
(define user "")
(define pass "")

(define (indexAction params)
  (define mp3 (pop3-connect host 110))
  (pop3-auth mp3 user pass)
;;  (let ((headers (pop3-extract-many mp3 (map (cut car <>) (pop3-list mp3)))))
  (let ((headers (pop3-read-many mp3 (map (cut car <>) (pop3-list mp3)))))

    (pop3-store-one mp3 1)

    (pop3-close mp3)
    (render #`",|VIEW-DIR|inbox.tmpl" headers)))

(define (hoge params)
  "hoge")

;; Local variables:
;; mode: gauche
;; end:





