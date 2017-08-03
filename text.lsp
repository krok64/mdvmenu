;;; Utils for AutoCAD 
;;; text editing utils
;;; (C) Milkov Dmitry, 2001-2016
;;; <Last edition 01.10.2016>

;;; poz - Изменение нумераций позиций {{{1
(defun c:poz ( / ar ent name num min dpoz)
(princ "Выберите текст: ")
(setq  
    ar (ssget '((0 . "TEXT")))
    min (getint "Введите минимальную изменяемую позицию<1>: ")
    dpoz (getint "Введите приращение для изменяемых позиций<1>: ")
);setq
(if (= min nil) (setq min 1))
(if (= dpoz nil) (setq dpoz 1))
(while
    (setq
        name (ssname ar 0)  
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
        num (atoi (cdr (assoc 1 ent))) 
    );setq
    ( if (and (/= num 0 ) (>= num min))
        (progn
            (setq 
                ent
                (subst (cons 1 (itoa(+ num dpoz))) 
                    (assoc 1 ent)           
                    ent                     
                );subst
            );setq
            (entmod ent);
        );progn
    );if
);while
(princ)
);end poz

;;; poz2 - Изменение числовых значений текстовых элементов {{{1
(defun c:poz2 ( / ar ent name num dpoz prec)
(princ "Выберите текст: ")
(setq  
    ar (ssget '((0 . "TEXT")))
    dpoz (getreal "Введите приращение для изменяемых позиций<1>: ")
    prec (getint "Введите точность (число знаков после запятой)<0>: ")
);setq
(if (= dpoz nil) (setq dpoz 1))
(if (= prec nil) (setq prec 0))
(while
    (setq
        name (ssname ar 0)  
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
        num (atof (strchg (cdr (assoc 1 ent)) "," "." )) 
        ent
        (subst (cons 1 (rtos (+ num dpoz) 2 prec)) 
            (assoc 1 ent)           
            ent                     
        );subst
    );setq
    (entmod ent);
);while
(princ)
);end poz2

;;; MakeSeq - проставление нумерации текстовых отметках по вертикали
(defun c:MakeSeq ( / ar num1 name obj)
(princ "Выберите текст: ")
(setq  
    ar (ssget '((0 . "TEXT")))
    num1 (getint "Введите номер первого пункта<1>: ")
);setq
(if (= num1 nil) (setq num1 1))
(while (> (sslength ar) 0)
    (setq
	;;; ВНИМАНИЕ!!! строки идут не по порядку.
; ищем строчку с наибольшей координатой 'y' - самую верхнюю
        name (findmaxy ar) 
        ent (entget name)
        ar (ssdel name ar)
	ent
	(subst (cons 1 (itoa num1))
            (assoc 1 ent)           
            ent                     
        );subst
	num1 (+ num1 1)
    );setq
    (entmod ent)
);while
(princ) 
)       

;;; MakeSeq - проставление нумерации текстовых отметках по горизонтали
(defun c:MSX ( / ar num1 name obj)
(princ "Выберите текст: ")
(setq  
    ar (ssget '((0 . "TEXT")))
    num1 (getint "Введите номер первого пункта<1>: ")
);setq
(if (= num1 nil) (setq num1 1))
(while (> (sslength ar) 0)
    (setq
	;;; ВНИМАНИЕ!!! строки идут не по порядку.
; ищем строчку с наибольшей координатой 'y' - самую верхнюю
        name (findminx ar) 
        ent (entget name)
        ar (ssdel name ar)
	ent
	(subst (cons 1 (itoa num1))
            (assoc 1 ent)           
            ent                     
        );subst
	num1 (+ num1 1)
    );setq
    (entmod ent)
);while
(princ) 
)       

;;; se - Экспортирование таблицы спецификации в текстовый файл {{{1
(defun c:se (/
    ar
    name
    ent
    ytop
    strlist
    FH
    FileName
);params

(princ "Select text:\n")
(setq  
    ar (ssget '((0 . "TEXT"))) 
    strlist nil
    FileName (getstring "\nEnter output file name: ")
    FH (open FileName "w")
)

;пока не пересмотрим все захваченые элементы
(while (/= (ssname ar 0) nil)
; ищем строку (элементы в строке должны лежать в пределах y+-4)
    (setq name (findmaxy ar))
    (if (= strlist nil)
; если нашли первый элемент в строке (строка была пустая)
        (setq 
            ytop (egety name)
            strlist (list name)
            ar (ssdel name ar)
        );setq
        (if (<= (abs (- ytop (egety name) ) ) 4)
            (setq
                strlist (append strlist (list name))
                ar (ssdel name ar)
            )
            (progn 
                (writestrtofile strlist FH)
                (setq strlist nil)
            )
        );if
    );if
);while
(if (/= strlist nil)
    (writestrtofile strlist FH)
);if
(close FH)
);end se

;;; setx - Выравнивание х координаты для столбца строк {{{1
(defun c:setx ( /
    xact    ; x - координата выравнивания
    ar
    name
    startpt
;    ent
);params
(setq
   ar (ssget '((0 . "TEXT")) )
   startpt (getpoint "\nЛевая граница выравнивания: ")
   xact (nth 0 startpt)
);setq
(while
    (setq
        name (ssname ar 0)  
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
        
        ent
        (subst (list 10 xact (nth 2 (assoc 10 ent)) 0) 
            (assoc 10 ent)           
            ent                     
        );subst
        
        ent
        (subst (cons 73 0) 
            (assoc 73 ent)           
            ent                     
        );subst
        
        ent
        (subst (cons 72 0) 
            (assoc 72 ent)           
            ent                     
        );subst
    );setq
    (entmod ent)                 
);while
(princ)
);end setx

;;; merge - Склеивание нескольких строк в одну {{{1
(defun c:merge( /
   ar
   name
   ent
   entfirst
   textstr
);params
(setq
   ar (ssget '((0 . "TEXT")) )
   name (ssname ar 0)  
   entfirst (entget name)
   ar (ssdel name ar)
   textstr (cdr (assoc 1 entfirst))
);setq
(while
    (setq
        name (ssname ar 0)   
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
        textstr (strcat textstr (cdr (assoc 1 ent)))
    );setq
    (entdel name)                 
);while
(setq
   entfirst
   (subst (cons 1 textstr)
      (assoc 1 entfirst)           
      entfirst                     
   );subst
);setq
(entmod entfirst)
);end merge

;;; textdel - Удалить текстовые строки {{{1
(defun c:textdel ( / ar name ent )
(setq
   ar (ssget '((0 . "TEXT")) )
);setq
(while
    (setq
        name (ssname ar 0)   
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
    );setq
    (entdel name)                 
);while
)

;;; c:myalign - Выравнивание межстрочных интервалов {{{1
(defun c:myalign( /
   ar         
   name
   ent
   startpt
   MDROWHIGH
);params
(setq MDROWHIGH (getint "\nВведите расстояние между строками: "))
(setq
   ar (ssget '((0 . "TEXT")) )
   startpt (getpoint "\nНачальная точка: ")
);setq
(while
    (setq
        name (ssname ar 0)  ;;; ВНИМАНИЕ!!! строки идут не по порядку.
; ищем строчку с наибольшей координатой 'y' - самую верхнюю
        name (findmaxy ar) 
        ent (entget name)
        ar (ssdel name ar)
        ent
        (subst (cons 10 startpt)
            (assoc 10 ent)           
            ent                     
        );subst
        startpt (incr startpt 1 (- MDROWHIGH))
    );setq
    (entmod ent)                 
);while
);end align
    
;;; al2 - Выравнивание х координаты для столбца строк {{{1
(defun al2 (
    xact    ; x - координата выравнивания
    ar
    name
    startpt
);params
(if (null *ROWHIGH*)
    (setq *ROWHIGH* (getint "\nВведите расстояние между строками: "))
);if
(setq
   ar (ssget '((0 . "TEXT")) )
   startpt (getpoint "\nНачальная точка: ")
   xact (nth 0 startpt)
);setq
(while
    (setq
        name (ssname ar 0)  ;;; ВНИМАНИЕ!!! строки идут не по порядку.
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
        ent
        (subst (cons 10 (list xact (nth 1 (assoc 10 ent)) 0) )
            (assoc 10 ent)           
            ent                     
        );subst
    );setq
    (entmod ent)                 
);while
);end al2

;;; c:textadd - добавляет в начало текста текст MDTEXT1 {{{1
(defun c:textadd ( / ar name ent )
(if (null MDTEXT1)
    (setq MDTEXT1 (getstring "\nВведите строку: "))
);if
(setq ar (ssget '((0 . "TEXT")) )
);setq
(while
    (setq
        name (ssname ar 0)
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
    );setq
    (setq ent
        (subst (cons 1 (strcat MDTEXT1 (cdr (assoc 1 ent))))
            (assoc 1 ent)           
            ent                     
        );subst
    );setq
    (entmod ent)                 
);while
(princ)
);end

;;; c:pk - добавление букв ПК а также изменение высоты текста до 2.5 и сжатия до 1 {{{1
(defun c:pk ( / ar name ent )
(setq 
    ar (ssget '((0 . "TEXT")) )
);setq
(style1)
(while
    (setq
        name (ssname ar 0)
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
    );setq
    (setq 
        ent
        (subst (cons 1 (strcat "ПК" (cdr (assoc 1 ent))))
            (assoc 1 ent)           
            ent                     
        );subst
        ent
        (subst (cons 40 '2.5)
            (assoc 40 ent)           
            ent                     
        );subst
        ent
        (subst (cons 7 "style1")
            (assoc 7 ent)           
            ent                     
        );subst
        ent
        (subst (cons 41 '1)
            (assoc 41 ent)           
            ent                     
        );subst
;        ent
;        (subst (cons 50 '0)
;            (assoc 50 ent)           
;            ent                     
;        );subst                            
        ent
        (subst (cons 51 '0)
            (assoc 51 ent)           
            ent                     
        );subst
    );setq
    (entmod ent)                 
);while
(princ)
);end

;;; c:strim - убирает впереди идущие пробелы из текста {{{1
(defun c:strim ()
(setq ar (ssget '((0 . "TEXT")) )
);setq
(while
    (setq
        name (ssname ar 0)
        ent (entget name)
        ar (ssdel name ar)
    );setq
    (setq ent 
        (subst (cons 1 (ltrim (cdr (assoc 1 ent))))
            (assoc 1 ent)           
            ent                     
        );subst
    ) ;setq
    (entmod ent)                 
);while
);end

;;; c:textgrad - заменяет в тексте символ ? на %%d {{{1
(defun c:textgrad( / ar name ent)
(setq ar (ssget '((0 . "TEXT")) )
);setq
(while
    (setq
        name (ssname ar 0)
        ent (entget name)
        ar (ssdel name ar)
    );setq
    (setq ent 
        (subst (cons 1 (strchg (cdr (assoc 1 ent)) "Є" "%%d" ))
            (assoc 1 ent)           
            ent                     
        );subst@
    ) ;setq
    (entmod ent)                 
);while
);end

;;; c:tc - заменяет в тексте строку 1 на строку 2 {{{1
(defun c:tc( / ar name ent str1 str2)
(setq 
    str1 (getstring ("Введите исходную строку"))
    str2 (getstring ("Введите новую строку"))
    ar (ssget '((0 . "TEXT")) )
);setq
(while
    (setq
        name (ssname ar 0)
        ent (entget name)
        ar (ssdel name ar)
    );setq
    (setq ent 
        (subst (cons 1 (strchg (cdr (assoc 1 ent)) "Є" "%%d" ))
            (assoc 1 ent)           
            ent                     
        );subst@
    ) ;setq
    (entmod ent)                 
);while
);end

;;; summ - суммирует числовые значения {{{1
(defun c:summ(); / ar name ent sum)
(setq 
    ar (ssget 
    '(
        (-4 . "<OR")  
        (0 . "TEXT")
        (0 . "MTEXT")
        (-4 . "OR>")  
     ))
    sum 0
    num 0
);setq
(while
    (setq
        name (ssname ar 0)
    );setq
    (setq
        ent (entget name)
        ar (ssdel name ar)
        sum (+ sum (atof (strchg (cdr (assoc 1 ent)) "," "." )))
        num (+ num 1)
    ) ;setq
);while
(princ (strcat "Sum=" (rtos sum 2 2) " Num=" (rtos num 2 0) " Avg=" (rtos (/ sum num) 2 2)))
(princ)
);end


;;; ni - прога для проставления нумерации в спецификации {{{1
(defun c:ni ( 
    /
    dy          ; шаг м/у строками
    centerp     ; точка центра поля 
    item_no     ; текущий номер в спецификации 
    exit_flag   ; флаг выхода из цикла
    usrin       ; ввод юзера
    ulpoint     ; левая верхняя точка первой ячейки
    drpoint     ; правая нижняя точка первой ячейки
    h           ; высота шрифта
    )
(setq 
    ulpoint (getpoint "\nУкажите левую верхнюю точку первой ячейки: ")
    drpoint (getpoint "\nУкажите правую нижнюю точку первой ячейки: ")
    h (getreal "\nВысота шрифта: ")
    exit_flag nil
    dy (- (nth 1 ulpoint) (nth 1 drpoint))
    dx (- (nth 0 ulpoint) (nth 0 drpoint))
    centerp (list (- (nth 0 ulpoint) (/ dx 2)) (- (nth 1 ulpoint) (/ dy 2)) 0)
);setq
(initget 5)
(setq
    item_no (getint "\nВведите номер пункта в первой ячейке: ")
);setq    
(while (= exit_flag nil)
    (initget 4 "Yes No Point End poS")
    (setq usrin (getkword (strcat "\nМетка: " (itoa item_no) ". ставить тут ?(<Yes>/No/Point/End/poS): ")))
    (cond
        ((= usrin "Point")
            (setq 
                ulpoint (getpoint "Укажите левую верхнюю точку первой ячейки: ")
                centerp (list (- (nth 0 ulpoint) (/ dx 2)) (- (nth 1 ulpoint) (/ dy 2)) 0)
            );setq
        );Point
        ((= usrin "End")
            (setq exit_flag T)
        );End
        ((= usrin "poS")
            (setq item_no (getint "Введите номер пункта: "))
        );Position
        ((= usrin "No")
            (setq centerp (incr centerp 1 (- dy)))
        );No
        (T
            (mktext (itoa item_no) centerp h 0 "style1" 1 2 5)
            (setq item_no (1+ item_no))
            (setq centerp (incr centerp 1 (- dy)))
        );Yes or nil
    );cond
);while
);end

;;; c:txt - надпись в центре 4-х угольника {{{1
(defun c:txt ( / p1 p2 )
  (setq p1 (getpoint "\nEnter 1 corner: "))
  (setq p2 (getpoint "\nEnter 2 corner: "))
  (drawtextcenter p1 p2)
)

;;; c:txt2 - двойная надпись в центре 4-х угольника {{{1
(defun c:txt2 ( / p1 p2 p3 p4 dy)
  (setq p1 (getpoint "\nEnter 1 corner: "))
  (setq p2 (getpoint "\nEnter 2 corner: "))
  (setq dy (/ (- (cadr p1) (cadr p2)) 3))
  (setq p3 
    (list
      (car p2)
      (+ (cadr p2) dy)
      0
    );list
  );setq
  (setq p4
    (list
      (car p1)
      (- (cadr p1) dy)
      0
    );list
  );setq
  (drawtextcenter p1 p3)
  (drawtextcenter p4 p2)
);end

;;; c:txt3 тройная надпись в центре 4-х угольника {{{1
(defun c:txt3 ( / p1 p2 p3 p4 p5 p6)
  (setq p1 (getpoint "\nEnter 1 corner: "))
  (setq p6 (getpoint "\nEnter 2 corner: "))
  (setq 
    p2 
    (list 
        (nth 0 p6)
        (- (nth 1 p1) (/ (- (nth 1 p1) (nth 1 p6)) 3))
        0
    );list          
    p3
    (list
        (nth 0 p1)
        (nth 1 p2)
        0
    );list
    p4 
    (list 
        (nth 0 p6)
        (+ (nth 1 p6) (/ (- (nth 1 p1) (nth 1 p6)) 3))
        0
    );list          
    p5
    (list
        (nth 0 p1)
        (nth 1 p4)
        0
    );list
  );setq
  (drawtextcenter p1 p2)
  (drawtextcenter p3 p4)
  (drawtextcenter p5 p6)
);txt3
   
;;; drawtextcenter - отрисовать текст в центре прямоугольника {{{1
(defun drawtextcenter ( p1 p2 / p3 color)
  (setq
     p3
     (list
        (/
            (+ (car p1) (car p2))
            2
        );/
        (/
            (+ (cadr p1) (cadr p2))
            2
        );/
        0.0
     );list
     color (getvar "CECOLOR")
     color  
        (cond
            ((= color "BYLAYER") 256)
            ((= color "BYBLOCK") 0)
            (t (atoi color))
        );cond
  );setq
  (mktext "text" p3 (getvar "TEXTSIZE") 0 (getvar "TEXTSTYLE") 1 2 color)
);end

;;; mktext - нарисовать текст {{{1
; 73  Vertical text justification type (optional, default = 0): integer codes (not bit- coded)
; 0 = Baseline; 1 = Bottom; 2 = Middle; 3 = Top
; 72  Horizontal text justification type (optional, default = 0) integer codes (not bit-coded)
;0 = Left;1= Center; 2 = Right
;3 = Aligned (if vertical alignment = 0)
;4 = Middle (if vertical alignment = 0)
;5 = Fit (if vertical alignment = 0)
(defun mktext( txt point height rotation style hjustify vjustify color / EL) 
(setq
   EL
   (list
      '(0 . "TEXT")       ;type of entity
      (cons 10 point)     ;base point
      (cons 11 point)     ;2 base point
      (cons 1  txt)       ;text
      (cons 40 height)    ;высота
      (cons 50 rotation)  ;rotation
      (cons 7  style)     ;style
      (cons 72 hjustify)  ;horizont justify
      (cons 73 vjustify)   ;vert justify
      (cons 62 color)
   );list
);setq
(princ txt)
(if (= (entmake EL) nil)
    (progn 
        (princ "\nError: can't draw text!")
        (princ EL)
    );progn
);if
);end

;;; c:textout - Экспорт натурных отметок земли и растояния м/у ними в текстовый файл {{{1
(defun c:textout ( / ar status FH name)
(princ "Выберите текстовые отметками:")
(setq ar (ssget '((0 . "TEXT"))))
(setq FH (open "d:/test" "w"))
(while (/= (ssname ar 0) nil)
    ; ищем значения растояний м/у отметками - у них максимальное значение y
    (setq name (findminx ar))
    (setq ar (ssdel name ar))
    (write-line (strchg (egettext name) "," ".") FH)
)
(close FH)
(princ)
);end textout

;;; style1 - Создать стиль style1 {{{1
(defun style1 ()
(command "_-style" "style1" "gost_sng" "" "1" "" "" "")
)
