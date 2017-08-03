;;; Utils for AutoCAD 14
;;; leaders functions
;;; (C) Milkov Dmitry, 2001-2008
;;; <Last edition 30.02.02>

;;; установить горизонтальный и вертикальный уровень выносок
(defun c:lset ()
(setq *LEADERS_LEVEL* (getpoint "Укажите уровень выносок: "))
);end lset

;;; отрисовать выноску от заданной точке к горизонтальному уровню под углом в 80 градусов
(defun c:lhor( / p1 p2 dy num)
(if (= *LEADERS_LEVEL* nil)
    (c:lset)
)
(setq 
    p1 (getpoint "Укажите точку: ")
    num (getstring "\nВведите номер: ")
    dy (- (cadr p1) (cadr *LEADERS_LEVEL*))
    p2 (list (- (car p1) (* dy (cos (rad 80)))) (- (cadr *LEADERS_LEVEL*) (getvar "dimgap") ) 0 )
);setq
    (command "_leader" p1 p2 "" num "")
);end lhor

;;; отрисовать выноску от заданной точке к вертикальному уровню под углом в 80 градусов
(defun c:lver( / p1 p2 dx num)
(if (= *LEADERS_LEVEL* nil)
    (c:lset)
)
(setq 
    p1 (getpoint "Укажите точку: ")
    num (getstring "\nВведите номер: ")
    dx (- (car p1) (car *LEADERS_LEVEL*))
    p2 (list (car *LEADERS_LEVEL*) (+ (cadr p1) (* dx (cos (rad 45)))) 0)
);setq
    (command "_leader" p1 p2 "" num "")
);end lver

(defun c:leaders ( / 
    dx
    ar
    ent
    name
    lmin
    lmax
    mtent
    textstr
);params
(setq
   ar (ssget "X" '((0 . "LEADER")) )
   lmin (getint "Минимальное значение выноски<1>: ")
   lmax (getint "Максимальное значение выноски<100>: ")
   dx (getint "Приращение для выносок<1>: ")
);setq
(if (= dx nil)
    (setq dx 1)
);if
(if (= lmin nil)
    (setq lmin 1)
);if
(if (= lmax nil)
    (setq lmax 100)
);if
(while
  
    (setq
        name (ssname ar 0) 
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
        mtent (entget (cdr (assoc 340 ent) ) )
        textstr (cdr(assoc 1 mtent))
    );setq
    (princ "\nнайдена метка: ")
    (princ textstr )
    (if 
        (and (/= textstr nil) (/= (setq num (atoi(remove_a_tag textstr))) 0) (<= num lmax) (>= num lmin))
        (progn 
            (setq        
                mtent
                (subst 
                    (cons 1 (itoa (+ num dx)))
                    (assoc 1 mtent)           
                    mtent                     
                );subst
            );setq
            (entmod mtent)
            (princ (strcat "\nвыноска: " (itoa num) " изменена на: " (itoa (+ num dx))))
        );progn
;        (princ ", оставлена без изменений.")
    );if
);while
(princ)
);leaders
