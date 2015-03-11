(define-module db.user
  (use orm)
  (export <user>))
(select-module user)

(define-class <user> (<orm>) ())

(provide "db.user")
