(defun atom? (x)
  (not (listp x)))

(defun lat? (l)
  (cond
  ((null l) t)
  ((atom? (car l)) (lat? (cdr l)))
  (t nil)))

(defun member? (a lat)
  (cond
    ((null lat) nil)
    ((or (eq (car lat) a) (member? a (cdr lat))))
    ))

(defun rember (a lat)
  (cond
    ((null lat) (quote()))
    ((eq (car lat) a) (cdr lat))
    ((rember a (cdr lat))
    ))
  )

(defun rembercons (a lat)
  (cond
    ((null lat) (quote()))
    ((eq (car lat) a) (cdr lat))
    ((cons (car lat) (rember a (cdr lat))))
    )
  )

(defun firsts (l)
  (cond
    ((null l) (quote()))
    ((cons (car (car l)) (firsts (cdr l))))
    )
  )

(defun insertR (new old lat)
  (cond
    ((null lat) (quote()))
    ((eq (car lat) old) (cons old (cons new (cdr lat))))
    ((cons (car lat) (insertR new old (cdr lat))))
))

(defun insertL (new old lat)

  (cond
    ((null lat) (quote()))
    ((eq (car lat) old) (cons new lat))
    ((cons (car lat) (insertL new old (cdr lat))))
    )
  )

(defun subst2 (new old lat)
  (cond
    ((null lat) (quote()))
    ((eq (car lat) old) (cons new (cdr lat)))
    ((cons (car lat) (subst2 new old (cdr lat))))
    )
  )

(defun subst3 (new o1 o2 lat)
  (cond
    ((null lat) (quote()))
    ((or (eq (car lat) o1) (eq (car lat) o2)) (cons new (cdr lat)))
    ((cons (car lat) (subst3 new o1 o2 (cdr lat))))
    )
  )

(defun multirember (a lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) a) (multirember a (cdr lat)))
    ((cons (car lat) (multirember a (cdr lat))))
    )
  )

(defun multiinsertR (new old lat)
  (cond
    ((null lat) (quote()))
    ((eq (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
    ((cons (car lat) (multiinsertR new old (cdr lat))))
    )
  )

(defun multiinsertL (new old lat)
  (cond
    ((null lat) (quote()))
    ((eq (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
    ((cons (car lat) (multiinsertL new old (cdr lat))))
    )
  )

(defun fact (n)
  (cond
    ((< n 0) nil)
    ((= n 0) 1)
    ((* n (fact (- n 1))))
    )
  )

(defun distro ()
  (quote("Arch Linux")))
