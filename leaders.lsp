;;; Utils for AutoCAD 14
;;; leaders functions
;;; (C) Milkov Dmitry, 2001-2008
;;; <Last edition 30.02.02>

;;; ���������� �������������� � ������������ ������� �������
(defun c:lset ()
(setq *LEADERS_LEVEL* (getpoint "������� ������� �������: "))
);end lset

;;; ���������� ������� �� �������� ����� � ��������������� ������ ��� ����� � 80 ��������
(defun c:lhor( / p1 p2 dy num)
(if (= *LEADERS_LEVEL* nil)
    (c:lset)
)
(setq 
    p1 (getpoint "������� �����: ")
    num (getstring "\n������� �����: ")
    dy (- (cadr p1) (cadr *LEADERS_LEVEL*))
    p2 (list (- (car p1) (* dy (cos (rad 80)))) (- (cadr *LEADERS_LEVEL*) (getvar "dimgap") ) 0 )
);setq
    (command "_leader" p1 p2 "" num "")
);end lhor

;;; ���������� ������� �� �������� ����� � ������������� ������ ��� ����� � 80 ��������
(defun c:lver( / p1 p2 dx num)
(if (= *LEADERS_LEVEL* nil)
    (c:lset)
)
(setq 
    p1 (getpoint "������� �����: ")
    num (getstring "\n������� �����: ")
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
   lmin (getint "����������� �������� �������<1>: ")
   lmax (getint "������������ �������� �������<100>: ")
   dx (getint "���������� ��� �������<1>: ")
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
    (princ "\n������� �����: ")
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
            (princ (strcat "\n�������: " (itoa num) " �������� ��: " (itoa (+ num dx))))
        );progn
;        (princ ", ��������� ��� ���������.")
    );if
);while
(princ)
);leaders
