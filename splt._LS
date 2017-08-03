(defun c:splt( / ss sl f i name ent a)
;Работает только в СПЛИТ
;Ищет объекты с записью об вертикальном угле на профиле и дописывает их в конец файла d:\\split-ugol.csv
(setq ss (ssget "_X" '((0 . "MNP_CROSSDRAW_PROFIL_VYNOSKA_TEMPLATE"))))
(if (= nil ss)
  (progn (print "Ничего не найдено! Возможно СПЛИТ не загружен"))
  (progn
	(setq sl (sslength ss) i 0)
	(setq f (open "d:\\split-ugol.csv" "w"))
	  (while (< i sl)
	    (setq
	      name (ssname ss i)
	      ent (entget name)
	      i (+ 1 i)
	      a (strcat "ПК" (cdr (assoc 302 ent)) ";" (cdr (assoc 303 ent)) ";" (cdr (assoc 1 ent)) ";" (cdr (assoc 2 ent)) ";" (cdr (assoc 3 ent)) ";" (cdr (assoc 4 ent)) ";" (cdr (assoc 304 ent)) ";" (cdr (assoc 300 ent)) ";" (cdr (assoc 301 ent)) )
	    );setq
	    (write-line a f)
	    (print a)
	  );while
	(setq ss nil)
	(close f)
  )
)
(print (strcat "Итого: " (itoa sl)))
(princ)
)

(defun c:splt2( / ss sl f i name ent a direct one)
;Работает только в СПЛИТ
;Ищет объекты с записью об горизонтальном угле на плане и дописывает их в конец файла d:\\plan-ugol.csv
(setq ss (ssget "_X" '((0 . "MNP_CROSSDRAW_PLANE_VYNOSKA_TEMPLATE"))))
(if (= nil ss)
  (progn (print "Ничего не найдено! Возможно СПЛИТ не загружен"))
  (progn
	(setq sl (sslength ss) i 0)
	(setq f (open "d:\\plan-ugol.csv" "w"))
	  (while (< i sl)
	    (setq
	      name (ssname ss i)
	      ent (entget name)
	      i (+ 1 i)
	      direct "no dir"
	    );setq
	      (foreach one (massoc 1 ent)
		(if (eq one "dпр. <<Угол>>") (setq direct "право"))
		(if (eq one "dлев. <<Угол>>") (setq direct "лево"))
		)
	    (setq a (strcat (cdr (assoc 1 ent)) ";" "ПК" (cdr (assoc 4 ent)) ";" direct ";" (cdr (assoc 2 ent)) ";" (cdr (assoc 3 ent)) ";" (cdr (assoc 300 ent)) ";" (cdr (assoc 301 ent))  ))
	    (write-line a f)
	    (print a)
	  );while
	(setq ss nil)
	(close f)
  )
)
(print (strcat "Итого: " (itoa sl)))
(princ)
)

(defun massoc (key alist / x nlist)
(foreach x alist
(if (eq key (car x))
(setq nlist (cons (cdr x) nlist))
)
)
(reverse nlist)
)
