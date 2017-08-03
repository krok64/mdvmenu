;;; Utils for AutoCAD 14-2002
;;; common functions
;;; (C) Milkov Dmitry, 2001-2008
;;; <Last edition 05.04.05>

;;; initmdv - загрузка модулей {{{
(load "config" "error! can't load config.lsp\n")
(load "text" "error! can't load text.lsp\n")
(load "draw" "error! can't load draw.lsp\n")
(load "keys" "error! can't load keys.lsp\n")
(load "leaders" "error! can't load leaders.lsp\n")
(load "land" "error! can't load land.lsp\n")
(load "textutil" "error! can't load textutil.lsp\n")
(load "figures" "error! can't load figures.lsp\n")
(load "dlg" "error! can't load dlg.lsp\n")
(load "vbasc" "error! can't load vbasc.lsp\n")
(load "vedom" "error! can't load vedom.lsp\n")
;(load "spec" "error! can't load spec.lsp\n")
;(load "spec1" "error! can't load spec1.lsp\n")
(princ "\nMDV utils loaded.")
(princ)

;;; egetx - возвратить начение X для объекта заданного именем {{{1
(defun egetx (name)
(nth 1 (assoc 10 (entget name)))
);egetx

;;; egety - возвратить начение Y для объекта заданного именем {{{1
(defun egety (name)
(nth 2 (assoc 10 (entget name)))
);egetx

;;; egettext - возвратить текстовое поле объекта {{{1
(defun egettext (name)
(cdr (assoc 1 (entget name)))
);egettext

;;; findmaxy - в списке объектов полученных по (ssget) находит имя объекта с максимальным значением y {{{1
(defun findmaxy ( arr / maxy y namemaxy idx len ent name)
(setq
   idx 0
   len (sslength arr)
   maxy -3000.
);setq
(while (< idx len)
   (setq
      name (ssname arr idx)
      ent (entget name)
      idx (1+ idx)
   );setq
   (if (< maxy (setq y (nth 2 (assoc 10 ent) )))
      (setq
         maxy y
         namemaxy name
      );setq
   );if
);while
namemaxy
);end findmaxy

;;; findminx - в списке объектов полученных по (ssget) находит имя объекта с min значением x {{{1
(defun findminx ( arr / minx x namenimx idx len ent name)
(setq
   idx 0
   len (sslength arr)
   minx 300000.
);setq
(while (< idx len)
   (setq
      name (ssname arr idx)
      ent (entget name)
      idx (1+ idx)
   );setq
   (if (> minx (setq x (nth 1 (assoc 10 ent) )))
      (setq
         minx x
         nameminx name
      );setq
   );if
);while
nameminx
);end findmaxy

;;; c:ftext1 - поворот осей UCS {{{1
(defun c:ftext1 (/ t)
(setq t (car (entsel "\nUSC ugol..")))
(command "_Ucs" "_e" t)
);end

;;; c:findk - расчет кривой (К) {{{1
(defun c:findk ( / R GR M )
 (setq R (getreal "\nRadius: "))
 (setq GR (getreal "\nGradus: "))
 (setq M (getreal "\nMinutes: "))
 (princ "\nK=") 
 (* R (rad ( + GR ( / M 60. ) ) ) )
)

;;; rad - переводит градусы в радианы {{{1
(defun rad( ugol )
  (/ (* pi ugol) 180)
)

;;; incr - увеличить i-тый эл-т списка на n {{{1
(defun incr (ls i n)
(setq k 0 len (length ls) newl nil
);setq
(while (< k len)
    (if (= k i)
    (setq newl (append newl (list(+ (nth k ls) n))))
    (setq newl (append newl (list(nth k ls))))
    );if
    (setq k (1+ k))
);while
newl
);end

;;; writestrtofile - запись строки состоящей из списка слов(в виде ent) в файл {{{1
(defun writestrtofile (slist FH /
    ar
    name
    str
);params
(setq
   str ""
   ar (ssadd)
);setq
(while (/= slist nil)
    (setq 
        ar (ssadd (car slist) ar )
        slist (cdr slist)
    )
);while
(while
    (ssname ar 0)
      
    (setq name (findminx ar))
    (ssdel name ar)
    (setq str (strcat 
        str
        (egettext name)
        "@"))
);while
(write-line str FH)
);writestrtofile

;;; write-strlist-to-file - запись списка строк в текстовый файл {{{1
(defun write-strlist-to-file ( file-name lst / FH str )
(if (setq  FH (open file-name "w"))
    (progn
        (foreach str lst (write-line str FH))
        (close FH)
    )
    (princ (strcat "\nОшибка при записи файла: " file-name))
) 
)

;;; export-table - экспорт таблицы в текстовый файл {{{1
(defun export-table ( / ar lst str FileName x-epsilon y-epsilon str1 )
(princ "\nВыделите текстовую таблицу для экспорта:")
(setq  
    ar (ssget '((0 . "TEXT"))) 
;   FileName (getstring "\nВведите название файла для экспорта: ")
;   str1 (getstring "\nВведите обозначение объекта: ")
    x-epsilon 5
    y-epsilon 4
)
(foreach str (split-horizontal ar y-epsilon)
    (princ str)
    (princ "\n")
)
)

;;; split-horizontal - разбивка захваченных по ssget эл-ов построчно с максимальныйм расстоянием м/у строками=y-max {{{1
(defun split-horizontal ( ar y-max / entlist entstr )
(setq entstr nil)
;пока не пересмотрим все захваченые элементы
(while (/= (ssname ar 0) nil)
; ищем строку (элементы в строке должны лежать в пределах y-max)
    (setq name (findmaxy ar))
    (if (= entstr nil)
; если нашли первый элемент в строке (строка была пустая)
        (setq
            ytop (egety name)
            entstr (list name)
            ar (ssdel name ar)
        );setq
        (if (<= (abs (- ytop (egety name) ) ) y-max)
            (setq
                entstr (append entstr (list name))
                ar (ssdel name ar)
            )
            (setq 
                entlist (append entlist (list entstr))
                entstr nil
            )
        );if
    );if
);while
(setq 
    entlist (append entlist (list entstr))
)
entlist
)

;;; LoadData - загрузка данных из файла в список построчно, возвращает список строк {{{1
(defun LoadData ( DataFileName
   /
   FH    ;описатель файла данных
   LN    ;строка
   ArmList
   )
(setq FH (open DataFileName "r"))
(if FH
(progn
   (while (setq LN (read-line FH))
      (setq 
        ArmList (append ArmList (list LN))
      );setq
   );while
   (close FH)
);progn
(princ (strcat "Ошибка открытия файла: " DataFileName ))   
);if
ArmList
);end

;;; c:lay_on - включить все слои {{{1
(defun c:lay_on ()
    (Command "_.-LAYER" "_ON" "*" "")
);end lay_on

;;; c:lay_off - выключить все слои {{{1
(defun c:lay_off ()
    (Command "_.-LAYER" "_OFF" "*" "_N" "")
);end lay_off

;;; getintdef - получить числовую часть из текстового блока {{{1
(defun getintdef (txt def / a)
(setq a (getint txt))
(if (= a nil) (setq a def))
a
)

;;; sortlst - Сортировка списка {{{1
(defun sortlst (lst / i idx newlst val)
(setq newlst nil)
(while (> (length lst) 0)
    (setq idx 0 val (car (nth idx lst)) i (1+ idx) len (length lst))
    (while (< i len)
        (if (< (car (nth i lst)) val)
            (setq idx i val (car (nth i lst)))
        )
        (setq i (1+ i))
    )    
    (setq lst (lstchg lst 0 idx) newlst (append newlst (list (car lst))) lst (cdr lst))
)
newlst
)

;;; lstchg - обмен местами 2 элементов списка {{{1
(defun lstchg (lst el1 el2 / len tmp newlst idx )
(if (= el1 el2) 
(setq newlst lst)
(progn
(setq len (length lst) newlst nil)
(if (or (< el1 0) (< el2 0) (>= el1 len) (>= el2 len))
    (print "The index is out of range")
    (progn
        (if (> el1 el2) (setq tmp el1 el1 el2 el2 tmp))
        (setq idx 0)
        (while (< idx len)
            (if (= idx el1) (setq tmp (nth idx lst) newlst (append newlst (list (nth el2 lst)))))
            (if (= idx el2) (setq newlst (append newlst (list tmp))))
            (if (and (/= idx el1) (/= idx el2)) (setq newlst (append newlst (list (nth idx lst)))))
            (setq idx (1+ idx))
        )
    )
)
)
)
newlst
)

;;; osmode_off - отключение объектной привязки {{{1
(defun osmode_off (/ mode)
    (setq mode (logior (getvar "osmode") 16384))
    (setvar "osmode" mode)
)

;;; osmode_on - включение объектной привязки {{{1
(defun osmode_on (/ mode)
    (setq mode (logand (getvar "osmode") 49151))
    (setvar "osmode"  mode)
)
