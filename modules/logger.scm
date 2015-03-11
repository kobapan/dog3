(define-module logger
  (use gauche.logger)
  (export %logger::out))
(select-module logger)

(define (%logger::out in . args)
  (let-keywords args ((prefix "")
                      (surfix ""))
    (let ((log (make <log-drain>
                 :path "../log/log.txt" )))
      (log-format log "~a~a~a\n" prefix in surfix)
      in)))
