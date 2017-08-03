;;; Utils for AutoCAD 14
;;; draw utils
;;; (C) Milkov Dmitry, 2001-2008
;;; <Last edition 01.07.02>

;;; ����� ������ ������������ �� �������
(defun c:trassa ( /
    pt  ; ����� �� ����������� �����
    oldpt; ���������� �����
    m   ; ������� ������� �� ��������� (1:m)
    hold;
    h   ; ������� ��������� ��  
    oldsnap
    next
    p1
    p2
);params

(if (null *SCALE*)
    (setq *SCALE* 200)
);if

(initget 6)   
(setq
   m (getint (strcat "������� ������� �� ��������� <" (rtos *SCALE*)  "> : "))
);setq

(if (= nil m)
    (setq m *SCALE*)
);if

(setq
   oldpt (getpoint "First point: ")
   h (getreal "������� ������� � �����, �: ")
   next 1
   hold h
);setq

(setq
    oldpt
    (list
        (nth 0 oldpt)
        (- (nth 1 oldpt) (/ (* h 1000) m) )
        0
    );list
);setq

(while (= next 1)
  (initget "eXit")
  (setq pt (getpoint "������� ��������� ����� (X-���������): ") )
  (if (= pt "eXit")
    (setq next 0)
    (progn
      (setq h (getreal (strcat "������� ������� � ����� <" (rtos hold)  "> : ")))
      (if (= nil h)
        (setq h hold)
      );if
      (setq hold h)
      (setq oldsnap (getvar "OSMODE"))
      (setvar "OSMODE" 0)

    (setq
        pt
        (list
            (nth 0 pt)
            (- (nth 1 pt) (/ (* h 1000) m) )
            0
        );list
    );setq

      (command "_pline" oldpt "_w" "0.5" "0.5" pt "")

      (setq oldpt pt)

      (setvar "OSMODE" oldsnap)
    );progn
  );if
);while

(setq *SCALE* m)
    
);end trassa 

;;; ������ ������
(defun c:flag ( / fp cp a)
(setq 
    fp (getpoint "\n��������� ����� �����: ")
    cp (getpoint "\n�������� ����� �����: " fp)
    a (angle fp cp)
    oldsnap (getvar "OSMODE")
);setq
(if (and (> a (/ pi 2)) (< a (/ (* 3 pi) 2)))
    (setq dx -7)
    (setq dx 7)
)
(if (and (> a 0) (< a pi))
    (setq dy 4)
    (setq dy -4)
)    
(setvar "OSMODE" 0)
(command "_pline" fp cp
                     (setq cp (polar cp 0 dx)) 
                     (setq cp (polar cp (- a pi) (/ dy (sin a))))
                     (polar cp (rad 180) dx)
                     ""
);command
(setvar "OSMODE" oldsnap)
);end

;;; ������ ������� ������������� ����
(defun ugol ( mode / fp p1 )
(setq fp (getpoint "\n������� �����: "))
(setq oldsnap (getvar "OSMODE"))
(setvar "OSMODE" 0)
(command "_line" fp (setq fp (polar fp (rad 90) 30))
    (polar fp (rad 0) 20)
    ""
);command
(setq p1 (list (+(nth 0 fp)1.5) (-(nth 1 fp)4)))
(style1)
(mktext (strcat "��" (getstring T "������� ����(�������): ") "%%d" (getstring T "������� ����(������): ") "'" ) p1 2.5 0 "style1" 0 0 7)
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "R=" (getstring T "������� R(XXXX): ") "�") p1 2.5 0 "style1" 0 0 7)
(if (= mode 2)
    (progn
        (setq p1 (incr p1 1 -4.5))
        (mktext (strcat "T1=" (getstring T "������� T1(XX.XX): ") "�") p1 2.5 0 "style1" 0 0 7)
        (setq p1 (incr p1 1 -4.5))
        (mktext (strcat "T2=" (getstring T "������� T2(XX.XX): ") "�") p1 2.5 0 "style1" 0 0 7)
    )
    (progn
        (setq p1 (incr p1 1 -4.5))
        (mktext (strcat "T=" (getstring T "������� T(XX.XX): ") "�") p1 2.5 0 "style1" 0 0 7)
    )
);if    
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "�=" (getstring T "������� �(X.XX): ") "�") p1 2.5 0 "style1" 0 0 7)
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "���=" (getstring T "������� ���(XXX.XX): ")) p1 2.5 0 "style1" 0 0 7)
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "��" (getstring T "������� ��(XX+XX.X): ")) p1 2.5 0 "style1" 0 0 7)
(setvar "OSMODE" oldsnap)
);end

;;; ������ ������� ������������� ����
(defun ugol2 ( fp a1 a2 a3 a4 a5 a6 a7 / p1 sp)
(setq oldsnap (getvar "OSMODE"))
(setvar "OSMODE" 0)

(if (< (atof a5) 0) 
  (command "_line" fp (setq sp (polar fp (rad 90) 30))
      (polar sp (rad 0) 20)
      ""
  );command
  (command "_line" (polar fp (rad 90) 30) fp
      (polar fp (rad 0) 20)
      ""
  );command
)

(setq fp (polar fp (rad 90) 30))

(setq p1 (list (+(nth 0 fp)1.5) (-(nth 1 fp)4)))
(mktext (strcat "��" a1 "%%d" a2 "'" ) p1 2.5 0 "style1" 0 0 7)
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "R=" a3 "�") p1 2.5 0 "style1" 0 0 7)
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "T=" a4 "�") p1 2.5 0 "style1" 0 0 7)
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "�=" a5 "�") p1 2.5 0 "style1" 0 0 7)
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "���=" a6) p1 2.5 0 "style1" 0 0 7)
(setq p1 (incr p1 1 -4.5))
(mktext (strcat "��" a7) p1 2.5 0 "style1" 0 0 7)
(setvar "OSMODE" oldsnap)
);end

;;; ������ ���������� ������ ����� �� ���������� �������
(defun c:truba ( / land toptrub diametr mx my pt ptt dy dx oldsnap)
;   (if (= nil *NOZ*) (setq *NOZ* 100))
(setq
    land (getreal "\n������� �����: ")
    toptrub (getreal "���� �����: ")
    diametr (getreal "������� �����: ")
    mx (getreal "�������������� �������: ")
    my (getreal "������������ �������: ")
    pt (getpoint "������� ������� �����: ")
    dy (* (+ (- land toptrub) (/ diametr 2)) (/ 1000 my))
    pt (incr pt 1 (- dy) )
    ptt (incr pt 1 (* (/ diametr 2) (/ 1000 my)))
    dx (/ (* diametr 1000) mx 2)
);setq
(setq oldsnap (getvar "OSMODE"))
(setvar "OSMODE" 0)
(command "_ellipse" "_c" pt ptt dx)
(setvar "OSMODE" oldsnap)
);end

;;; ������ ��������� ������ �� ���������� �������
(defun c:cabel ( / deep  my oldsnap pt)
(setq
    deep (getreal "\n������� ��������� ������: ")
    pt (getpoint "������� ������� �����: ")
    my (/ 1000 (getreal "\n������������ �������: "))
    pt (incr pt 1 (-(* deep my)))
);setq
(setq oldsnap (getvar "OSMODE"))
(setvar "OSMODE" 0)
(command "_circle" pt 0.5)
(setvar "OSMODE" oldsnap)
);end

;;== �������
(defun c:trinity (/ pt0 pt1 ug1 pt2 pt3 pt4 pt5 pt6 ptr1 ptr2 ptr3 ptr4 wi2 l1)
(if (= os1 1) (command "_Osnap" "_nea"))
(if (= os1 2) (command "_Osnap" "_int"))
(setq wi1 (getreal "\n������� W1 <mm>.."))
(setq wi2 (getreal "\n������� W2 <mm>.."))
(setq L1 (getreal "\n����� �������� L <mm>.."))
(setq h1 (getreal "\n������ �������� H <mm>.."))
(setq r (getreal "\n������ ����������� r <mm>.."))
(setq pt1 (getpoint "\n����� ������� (x,y) : "))
(command "_osnap" "_none")

(setq pt2 (polar pt1 0 l1))
(setq pt3 (polar pt1 pi l1))

(setq pt5 (polar pt2 ( / pi 2) (/ wi1 2)))
(setq pt6 (polar pt2 ( / pi -2) (/ wi1 2)))
(setq pt7 (polar pt3 ( / pi 2) (/ wi1 2)))
(setq pt8 (polar pt3 ( / pi -2) (/ wi1 2)))
(setq pt1a (polar pt1 ( / pi 2) h1) )
(setq pt1b (polar pt1 ( / pi -2) (/ wi1 2)) )

(setq pt11 (polar pt1 ( / pi 2) (/ wi1 2)))
(setq pt9 (polar pt11 0 (/ wi2 2)))
(setq pt10 (polar pt11 pi (/ wi2 2)))

(setq pt13 (polar pt1a 0 (/ wi2 2)))
(setq pt14 (polar pt1a pi (/ wi2 2)))
(setq pt11a (polar pt11 ( / pi -2) (/ wi2 4)) )
(setq ptr1 (polar pt10 pi r))
(setq ptr2 (polar pt10 ( / pi 2 ) r))
(setq ptr4 (polar pt9 0 r))
(setq ptr3 (polar pt9 ( / pi 2 ) r))

; (command "_pline" pt7 pt10 pt14 pt13 pt9 pt5 pt6 pt8 "_cl")
(command "_pline" ptr2 pt14 pt13 ptr3 "")
(command "_pline" ptr4 pt5 pt6 pt8 pt7 ptr1"")
(command "_arc" ptr1 "_e" ptr2 "_r" r)
(command "_arc" ptr3 "_e" ptr4 "_r" r)
; (if (= wi2 wi1) (command "_pline" pt10 pt1 pt9 ""))
; (if (< wi2 wi1) (command "_arc" pt10 pt11a pt9 ""))

(command "_color" "1")
(command "_linetype" "_s" "������" "")
(command "_line" (polar pt1b (/ pi -2)  1) (polar pt1a (/ pi 2)  1) "")
(command "_line" (polar pt2 0 1) (polar pt3 pi 1) "")
(command "_linetype" "_s" "_bylayer" "")
(command "_color" "_bylayer")
)

;;; ����������� ����. ����� (�������)
(defun c:relay( / t1 t2 p1 p2 oldsnap)
  (setq p1 (getpoint "\nEnter 1 corner: "))
  (setq p2 (getpoint p1 "\nEnter 2 corner: "))
  (setq oldsnap (getvar "OSMODE"))
  (setvar "OSMODE" 0)
; �������������� �����
  (setq t1 (list ( - (car p1) 5.0) (cadr p1) ))
  (setq t2 (list ( + (car p2) 5.0) (cadr p1) ))
  (command "_line" t1 t2 "")
; ������������ ������������
  (setq t1 (list (car p1) (+ (cadr p1) 5) ))
  (setq t2 (list (car p1) (- (cadr p1) 20) ))
  (command "_line" t1 t2 "")
  (setq t1 (list (car p2) (+ (cadr p1) 5) ))
  (setq t2 (list (car p2) (- (cadr p1) 20) ))
  (command "_line" t1 t2 "")
; ������� �� ������������
  (setq t1 (list ( - (car p1) 1.41) (- (cadr p1) 1.41) ))
  (setq t2 (list ( + (car p1) 1.41) (+ (cadr p1) 1.41) ))
  (command "_line" t1 t2 "")
  (setq t1 (list ( - (car p2) 1.41) (- (cadr p1) 1.41) ))
  (setq t2 (list ( + (car p2) 1.41) (+ (cadr p1) 1.41) ))
  (command "_line" t1 t2 "")
; ������� �������
  (setq t1 (list ( / ( + (car p1) (car p2)) 2) ( + (cadr p1) 2) ))
  (command "_text" "j" "m" t1 "" "" "����������� ������������ �����")(princ)
  (setvar "OSMODE" oldsnap)
)
         
;;; ��������� �����
(defun c:piket( / p1 p2 p3 p4 l a1 a2)
  (setq p1 (getpoint "\nEnter 1 corner: "))
  (setq p2 (getpoint p1 "\nEnter 2 corner: "))
  (command "_pline" p1 p2 
    (setq p3 (polar p2 (rad 270) 10))
    (setq p4 (polar p1 (rad 270) 10))
    "_c"
  )
  (setq 
    l 
    (/ 
      (distance p1 p3) 
      3 
    )
    a1
    (angle p1 p3)
    a2
    (angle p4 p2)
  )
  (command "_pline" p1 (polar p1 a1 l) "")  
  (command "_pline" p4 (polar p4 a2 l) "")  
  (command "_pline" p2 (polar p2 (- a2 pi) l) "")  
  (command "_pline" p3 (polar p3 (- a1 pi) l) "")  
)
