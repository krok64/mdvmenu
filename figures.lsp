(defun c:h ( /
    pt  ; начальная точка
    len ; длина трапеции
    d   ; ширина основания 1
    d2  ; ширина основания 2
    m   ; масштаб (1:m)
    dy
    dy2
    oldsnap
);params

(setq m (getvar "dimlfac"))
(print (strcat "Линейный масштаб: " (rtos m 2 0)))

(if (null *DIAMETR*)
    (setq *DIAMETR* 0)
);if
(if (null *DLINA*)
    (setq *DLINA* 0)
);if

(setq
   d (getreal (strcat "\nПервое основание трапеции <" (rtos *DIAMETR*)  "> : "))
);setq

(if (= nil d)
    (setq d *DIAMETR*)
);if

(setq
   d2 (getreal (strcat "Второе основание трапеции <" (rtos d)  "> : "))
   tlen (getreal (strcat "Длина трапеции <" (rtos *DLINA*)  "> : "))
   pt (getpoint "Точка середины первого основания трапеции: ")
);setq

(if (= nil d2)
    (setq d2 d)
);if
(if (= nil tlen)
    (setq tlen *DLINA*)
);if

(setq oldsnap (getvar "OSMODE"))
(setvar "OSMODE" 0)

(setq 
    dy (/ d 2 m)
    dy2 (/ d2 2 m)
    p1 (incr pt 1 dy)
    p2 (incr pt 1 (- dy))
    p3 (incr pt 0 (/ tlen m))
    p4 (incr p3 1 dy2)
    p3 (incr p3 1 (- dy2))
);setq
(command "_line" p1 p2 p3 p4 "_c")

(setvar "OSMODE" oldsnap)

(setq *DIAMETR* d)
(setq *DLINA* tlen)
    
);end ht

;;; рисует вертикальную трапецию в масштабе
(defun c:v ( /
    pt  ; начальная точка
    len ; длина трапеции
    d   ; ширина основания 1
    d2  ; ширина основания 2
    m   ; масштаб (1:m)
    dx
    dx2
    oldsnap
);params

(setq m (getvar "dimlfac"))
(print (strcat "Линейный масштаб: " (rtos m 2 0)))

(if (null *DIAMETR*)
    (setq *DIAMETR* 0)
);if
(if (null *DLINA*)
    (setq *DLINA* 0)
);if

(setq
   d (getreal (strcat "\nПервое основание трапеции <" (rtos *DIAMETR*)  "> : "))
);setq

(if (= nil d)
    (setq d *DIAMETR*)
);if

(setq
   d2 (getreal (strcat "Второе основание трапеции <" (rtos d)  "> : "))
   tlen (getreal (strcat "Длина трапеции <" (rtos *DLINA*)  "> : "))
   pt (getpoint "Точка середины первого основания трапеции: ")
);setq

(if (= nil d2)
    (setq d2 d)
);if
(if (= nil tlen)
    (setq tlen *DLINA*)
);if

(setq oldsnap (getvar "OSMODE"))
(setvar "OSMODE" 0)

(setq 
    dx (/ d 2 m)
    dx2 (/ d2 2 m)
    p1 (incr pt 0 dx)
    p2 (incr pt 0 (- dx))
    p3 (incr pt 1 (/ tlen m))
    p4 (incr p3 0 dx2)
    p3 (incr p3 0 (- dx2))
);setq
(command "_line" p1 p2 p3 p4 "_c")

(setvar "OSMODE" oldsnap)

(setq *DIAMETR* d)
(setq *DLINA* tlen)
    
);end vt

;;; построить дугу по 3 точкам в масштабе
(defun c:d (/ p1 p2 p3 dir dx1 dx3 scale t1 t2 t3 oldvar m)
(setq scale (getvar "dimlfac"))
(print (strcat "Линейный масштаб: " (rtos scale 2 0)))
(if (= *RSCALE* nil)
    (setq *RSCALE* 1)
)
(setq
    p1 (getpoint "\nначальная точка проекции дуги на прямую: ")
    p2 (getpoint "\nконечная точка проекции дуги на прямую: ")
    p3 (list (/ (+ (nth 0 p1) (nth 0 p2)) 2) (/ (+ (nth 1 p1) (nth 1 p2)) 2))
    dir (angle p3 (getpoint p3 "\nнаправление: "))
    m (getreal (strcat "\nмасштаб в котором были измерены расстояния<" (rtos *RSCALE* 2 0) ">: " ))
);setq
(if (= m nil)
    (setq m *RSCALE*)
)    
(setq    
    dx3 (* (/ (getreal "\nрасстояние от центра дуги до проекции: ") scale) m)
    dx1 (* (/ (getreal "\nрасстояние от края дуги до проекции: ") scale) m)
    t1 (polar p1 dir dx1)
    t2 (polar p2 dir dx1)
    t3 (polar p3 dir dx3)
)
(setq oldvar (getvar "OSMODE"))
(setvar "OSMODE" 0)
(command "_arc" t1 t3 t2)
(setvar "OSMODE" oldvar)
(setq *RSCALE* m)
);end duga3p

(defun c:o (/ x1 x mas1)
(setvar "cmdecho" 0)
(setq mas1 (getvar "dimlfac"))
(print (strcat "Линейный масштаб: " (rtos mas1 2 0)))
(setq x1 (getreal "Подобие < MM >: "))
(if (> x1 nil) (setq x (/ x1 mas1) ))
(command "_offset" x)
)

;;; переместить объекты от базовой точки на заданное растояние в масштабе
(defun c:mf (/ scale ar p p1 dx name oldvar)
(setq scale (getvar "dimlfac"))
(print (strcat "Линейный масштаб: " (rtos scale 2 0)))
(setq 
    ar (ssget) ; выбор
    p (getpoint "\nТочка на объекте")
    p1 (getpoint "\nБазовая точка")
    dx (getreal "\nПеремещение< MM >: ")
);setq
(if (> dx nil) 
(progn
    (setq oldvar (getvar "OSMODE"))
    (setvar "OSMODE" 0)
    (setq 
        p (polar p1 (angle p1 p) (- (/ dx scale) (distance p p1)) )
    );setq
    (while (setq name (ssname ar 0))
        (command "_move" name "" p1 p)
        (setq ar (ssdel name ar))
    );while
    (setvar "OSMODE" oldvar)
);progn
);if
);end move_from

;;; переместить объекты с учетом масштаба
(defun c:m (/ scale ar p1 p2 dx name oldvar)
(setq scale (getvar "dimlfac"))
(print (strcat "Линейный масштаб: " (rtos scale 2 0)))
(setq 
    ar (ssget) ; выбор
    p1 (getpoint "\nБазовая точка")
    oldvar (getvar "ORTHOMODE")
);setq
(setvar "ORTHOMODE" 1)
(setq    
    p2 (getpoint p1 "\nСторона...")
    dx (getreal "\nПеремещение< MM >: ")
);setq
(setvar "ORTHOMODE" oldvar)
(if (> dx nil) 
(progn
    (setq oldvar (getvar "OSMODE"))
    (setvar "OSMODE" 0)
    (setq 
        p2 (polar p1 (angle p1 p2) (/ dx scale ))
    );setq
    (while (setq name (ssname ar 0))
        (command "_move" name "" p1 p2)
        (setq ar (ssdel name ar))
    );while
    (setvar "OSMODE" oldvar)
);progn
);if
);end move_scale

;;; копировать объекты с учетом масштаба
(defun c:c (/ scale ar p1 p2 dx name oldvar)
(setq scale (getvar "dimlfac"))
(print (strcat "Линейный масштаб: " (rtos scale 2 0)))
(setq 
    ar (ssget) ; выбор
    p1 (getpoint "\nБазовая точка")
    oldvar (getvar "ORTHOMODE")
);setq
(setvar "ORTHOMODE" 1)
(setq    
    p2 (getpoint p1 "\nСторона...")
    dx (getreal "\nПеремещение< MM >: ")
);setq
(setvar "ORTHOMODE" oldvar)
(if (> dx nil) 
(progn
    (setq oldvar (getvar "OSMODE"))
    (setvar "OSMODE" 0)
    (setq 
        p2 (polar p1 (angle p1 p2) (/ dx scale ))
    );setq
    (while (setq name (ssname ar 0))
        (command "_copy" name "" p1 p2)
        (setq ar (ssdel name ar))
    );while
    (setvar "OSMODE" oldvar)
);progn
);if
);end move_scale

;;= длина линии
(defun c:l (/ mas1 a1 a)
(setq mas1 (getvar "dimlfac"))
(print (strcat "Линейный масштаб: " (rtos mas1 2 0)))
(setq a1 (getreal "\nДлина :"))
 (if (> a1 nil) (setq a (/ a1 mas1) ))
(command "_lengthen" "_t" a )
)

;;=линия разрыва 
(defun c:as1 (/)
(command "_color" "1")
(setq t1 (getpoint  "\nPoint 1"))
(setq t2 (getpoint t1 "\nPoint 2"))
(setq di (distance t1 t2))
(setq ug (angle t1 t2))
(command "_pline" t1 (polar t1 ug (-(/ di 2)1))
    (polar (polar t1 ug (/ di 2)) (+ ug (/ pi 2)) 1.5)
    (polar (polar t1 ug (/ di 2)) (+ ug (/ pi 2)) -1.5)
    (polar t1 ug (+(/ di 2)1)) 
     t2
     "")
(command "_color" "_bylayer")
)
