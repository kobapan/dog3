(define-module orm
  (use dbi)
  (use gauche.collection)
  (use gauche.interactive)
  (export <orm> <orm-meta> make all find save set-params))
(select-module orm)

(define *data-source-name* "dbi:sqlite3:shrimp.db")

(define-class <orm-meta> (<class>) ())

(define-class <orm> ()
  ((initialized  :allocation :each-subclass)
   (db-conn :allocation :class)
   (table-name :allocation :each-subclass)
   (column-names :allocation :each-subclass))
  :metaclass <orm-meta>)

(define-method setup-slots ((class <orm-meta>))
  (if (class-slot-bound? class 'initialized)
      class
      (let* ((dummy (make class))
	     (db-conn (or (class-slot-bound? class 'db-conn) (db-connect)))
	     (table-name (class-name->table-name class))
	     (column-names (db-column-name-list db-conn table-name))
	     (new-class #f))
	(redefine-slots class column-names)
	(set! new-class (slot-ref class 'redefined))
	(set! (class-slot-ref new-class 'db-conn) db-conn) 
	(set! (class-slot-ref new-class 'table-name) table-name) 
	(set! (class-slot-ref new-class 'column-names) column-names)
	(set! (class-slot-ref new-class 'initialized)  #t)
	new-class)))

(define-method class-name->table-name ((class <orm-meta>))
  (string-append (regexp-replace #/<(.+)>/ (symbol->string (class-name class)) "\\1") "s"))

(define-method redefine-slots ((class <orm-meta>) slots-names)
  (let1 slots (map (lambda(n)
		     `(,(string->symbol n) :init-keyword ,(make-keyword n)
		       :accessor ,(string->symbol #`",|n|-of")))
		   slots-names)
    (eval `(define-class ,(class-name class) 
	     ( ,(class-name (car (class-direct-supers class))) )
	      ,slots)
	  (interaction-environment))))

(define-method make-with-db-values ((class <orm-meta>) row column-getter)
  (let1 init-values (fold (lambda(c r)
			    (list* (make-keyword c) (column-getter row c) r))
			  ()
			  (class-slot-ref class 'column-names))
    (apply make (cons class init-values))))

(define-method all ((class <orm-meta>))
  (set! class (setup-slots class))
  (let* ((result (db-select (class-slot-ref class 'db-conn)
			    (class-slot-ref class 'table-name)))
	 (column-getter (relation-accessor result)))
    (map (lambda(row) (make-with-db-values class row column-getter))
	 result)))

(define-method find ((class <orm-meta>)(id <integer>))
  (set! class (setup-slots class))
  (let* ((result (db-select-by-key (class-slot-ref class 'db-conn)
				   (class-slot-ref class 'table-name)
				   "id" id))
	 (column-getter (relation-accessor result)))
    (make-with-db-values class (first-element result) column-getter)))

(define-method first-element ((coll <collection>))
  (find (lambda(el) #t) coll))

(define-method set-params ((object <orm>) params)
  (for-each 
   (lambda(p)
     (let1 col (string->symbol (car p))
       (if (slot-exists? object col)
	   (slot-set! object col (cadr p)))))
   params))

(define-method save ((object <orm>)) 
  (if (slot-bound? object 'id)
      (db-update (slot-ref object 'db-conn)
		 (slot-ref object 'table-name)
		 (column-slot-hash object)
		 "id"
		 (slot-ref object 'id))
      (db-insert (slot-ref object 'db-conn)
		 (slot-ref object 'table-name)
		 (column-slot-hash object)))
  #t)

(define-method column-slot-hash ((object <orm>))
  (let1 hash (make-hash-table 'string=?)
    (for-each
     (lambda(col)
       (if (slot-bound? object (string->symbol col))
	   (set! (ref hash col)
	         (slot-ref object (string->symbol col)))))
       (slot-ref object 'column-names))
    hash))

(define (db-connect)
  (dbi-connect *data-source-name*))

(define (db-column-name-list db-conn table-name)
  (let1 result (dbi-do db-conn 
		       #`"SELECT * FROM ,table-name LIMIT 1")
     (vector->list (relation-column-names result))))

(define (db-select db-conn table-name)
  (dbi-do db-conn #`"SELECT * FROM ,table-name"))

(define (db-select-by-key db-conn table-name key-column key-value)
  (let* ((sql #`"SELECT * FROM ,table-name WHERE ,key-column = ?")
	 (query (dbi-prepare db-conn sql)))
    (dbi-execute query key-value)))

(define (db-update db-conn table-name slot-hash key-column key-value)
  (let* ((assign (string-join (map (lambda(c) #`",c = ?") (hash-table-keys slot-hash)) ","))
	 (sql #`"UPDATE ,table-name SET ,assign WHERE ,key-column = ?")
	 (query (dbi-prepare db-conn sql)))
    (apply dbi-execute (cons query (append (hash-table-values slot-hash) (list key-value))))))
	 

(define (db-insert db-conn table-name slot-hash)
  (let* ((places (map (lambda(c) "?") (hash-table-keys slot-hash)))
	 (sql #`"INSERT INTO ,table-name (,(string-join (hash-table-keys slot-hash) \",\")) VALUES (,(string-join places \",\"))")
	 (query (dbi-prepare db-conn sql)))
    (apply dbi-execute (cons query (hash-table-values slot-hash)))))


(provide "orm")
