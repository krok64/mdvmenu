;;; Utils for AutoCAD
;;; profile generating utils
;;; (C) Milkov Dmitry, 2001-2013
;;; <Last edition 29.04.13>

;;; ImportMarks - импорт расчитанной трассы из экселя в автокад {{{1
(defun c:ImportMarks (/ mylist dlgdata vscale hscale OKload OKdlg
                      OKscale OKpoint ISel IStrassa ISpk ISdist ISland
                      ISpipe ISi ISdeep shift xcold truba ymas idx pugol
                      p_land sp ycoord
                     )

  (if (setq mylist (LoadData (strcat *MDVTEMPDIR* "xlsout")))
    (setq OKload T)
    (progn
      (princ "\nОшибка при загрузке файла данных.")
      (setq OKload nil)
    )
  )

  (if OKload
    (if (setq dlgdata (mdv:getDialogInput))
      (setq OKdlg T)
      (progn
        (princ "\nФункция прервана пользователем.")
        (setq OKdlg nil)
      )
    )
  )

  (if (and OKLoad OKdlg)
    (progn
      (setq
        hscale (/ (float (cdr (assoc 1 dlgdata))) 1000)
        vscale (/ (float (cdr (assoc 2 dlgdata))) 1000)
      )
      (if (or (= 0 hscale) (= 0 vscale))
        (progn
          (princ "\nМасштаб не может быть равен 0.")
          (setq OKscale nil)
        )
        (setq OKscale T)
      )
    )
  )

  (if (and OKLoad OKdlg OKscale)
    (progn
      (setq
        ISel     (if (= (cdr (assoc 3 dlgdata)) "1")
                   T
                   nil
                 )
        IStrassa (if (= (cdr (assoc 4 dlgdata)) "1")
                   T
                   nil
                 )
        ISpk     (if (= (cdr (assoc 5 dlgdata)) "1")
                   T
                   nil
                 )
        ISdist   (if (= (cdr (assoc 6 dlgdata)) "1")
                   T
                   nil
                 )
        ISland   (if (= (cdr (assoc 7 dlgdata)) "1")
                   T
                   nil
                 )
        ISpipe   (if (= (cdr (assoc 8 dlgdata)) "1")
                   T
                   nil
                 )
        ISi      (if (= (cdr (assoc 9 dlgdata)) "1")
                   T
                   nil
                 )
        ISdeep   (if (= (cdr (assoc 10 dlgdata)) "1")
                   T
                   nil
                 )
        shift    0
        xcold    -1000000
        truba    nil
        ymas     (list 19 16.75 14.5)
        idx      0
        OKpoint  T
      )
      (if Isel
        (if (setq pugol
                   (getpoint "\nУкажите нижний уровень выносок углов: ")
            )
          T
          (setq OKpoint nil)
        )
      )
      (if (and OKpoint IStrassa)
        (if (setq p_land
                   (getpoint
                     "\nУкажите на профиле отмеку земли в первой точке: "
                   )
            )
          T
          (setq OKpoint nil)
        )
      )
      (if (and OKpoint (or ISpk ISdist ISland ISpipe ISi ISdeep))
        (if (setq sp
                   (getpoint "\nУкажите левый верхний угол графы ПИКЕТАЖ: ")
            )
          (setq ycoord (nth 1 sp))
          (setq OKpoint nil)
        )
        (setq sp (list 0 0 0))
      )
      (if (not OKpoint)
        (princ "\nФункция прервана.")
      )
    )
  )

  (if (and OKLoad OKdlg OKscale OKpoint)
    (progn
      (setvar "DIMZIN" 0)
      (osmode_off)
      (style1)
      ;; в цикле отрисовываем: пикетаж, растояния между отметками, НОЗ, верх трубы, глубину траншеи.
      (while (/= mylist nil)
        (setq
          str    (car mylist)
          mylist (cdr mylist)
          items  (strtok str ",")
        )                               ;setq
        ;; если первый элемент -1 значит идут доп данные
        (if (= (nth 0 items) "-1")
          (progn
            (if ISi
              (progn
                ;; перемычка участков с одинаковым уклоном
                (setq cp (list xcoord (- ycoord 50) 0))
                (setq cp2 (list xcoord (- ycoord 60) 0))
                (command "_line" cp cp2 "")
                ;; наклонная линия разделящая уклон и расстояние
                (if (> (atof (nth 2 items)) 0)
                  (progn
                    ;; рисуем уклон слева  и растояние слева от точки
                    (setq cp
                           (list
                             (+ (- xcoord (/ (atof (nth 1 items)) hscale))
                                2
                             )
                             (- ycoord 54)
                             0
                           )
                    )
                    (mktext (okrugl (nth 2 items) 4)
                            cp
                            2.5
                            0
                            "style1"
                            0
                            0
                            7
                    )
                    (setq cp (list (- xcoord 2) (- ycoord 59) 0))
                    (mktext (okrugl (nth 1 items) 1)
                            cp
                            2.5
                            0
                            "style1"
                            2
                            0
                            7
                    )
                    (setq cp (list xcoord (- ycoord 50) 0))
                    (setq
                      cp2 (list
                            (- xcoord (/ (atof (nth 1 items)) hscale))
                            (- ycoord 60)
                            0
                          )
                    )
                  )                     ;progn
                )                       ;if
                (if (< (atof (nth 2 items)) 0)
                  (progn
                    ;; рисуем уклон слева  и растояние слева от точки
                    (setq cp (list (- xcoord 2) (- ycoord 54) 0))
                    (mktext (rtos (abs (atof (nth 2 items))) 2 4)
                            cp
                            2.5
                            0
                            "style1"
                            2
                            0
                            7
                    )
                    (setq cp
                           (list
                             (+ (- xcoord (/ (atof (nth 1 items)) hscale))
                                2
                             )
                             (- ycoord 59)
                             0
                           )
                    )
                    (mktext (okrugl (nth 1 items) 1)
                            cp
                            2.5
                            0
                            "style1"
                            0
                            0
                            7
                    )
                    (setq cp (list xcoord (- ycoord 60) 0))
                    (setq
                      cp2 (list
                            (- xcoord (/ (atof (nth 1 items)) hscale))
                            (- ycoord 50)
                            0
                          )
                    )
                  )                     ;progn
                )                       ;if
                (if (= (atof (nth 2 items)) 0)
                  (progn
                    ;; рисуем уклон слева  и растояние слева от точки
                    (setq cp
                           (list
                             (- xcoord (/ (atof (nth 1 items)) hscale 2))
                             (- ycoord 54)
                             0
                           )
                    )
                    (mktext (okrugl (nth 2 items) 4)
                            cp
                            2.5
                            0
                            "style1"
                            1
                            0
                            7
                    )
                    (setq cp
                           (list
                             (- xcoord (/ (atof (nth 1 items)) hscale 2))
                             (- ycoord 59)
                             0
                           )
                    )
                    (mktext (okrugl (nth 1 items) 1)
                            cp
                            2.5
                            0
                            "style1"
                            1
                            0
                            7
                    )
                    (setq cp (list xcoord (- ycoord 55) 0))
                    (setq
                      cp2 (list
                            (- xcoord (/ (atof (nth 1 items)) hscale))
                            (- ycoord 55)
                            0
                          )
                    )
                  )                     ;progn
                )                       ;if
                (command "_line" cp cp2 "")
              )
            )
            (if ISel
              (progn
                                        ; отрисовываем выноску угла
                (setq
                  cp (list xcoord (nth 1 pugol) 0)
                  u1 (nth 4 items)      ;угол
                  u2 (nth 5 items)      ;минуты
                  u3 (nth 3 items)      ;радиус
                  u4 (okrugl (nth 6 items) 2) ;тангенс
                  u5 (okrugl (nth 7 items) 2) ;биссектриса
                  u7 (okrugl (nth 8 items) 3) ;пикет
                  u7 (strchg u7 "." "+")
                  u7 (strcat (substr u7 1 (- (strlen u7) 1))
                             "."
                             (substr u7 (strlen u7))
                     )
                )                       ;setq
                (if (= (strlen u2) 1) (setq u2 (strcat "0" u2)))
                (ugol2 cp u1 u2 u3 u4 u5 u6 u7)
              )
            )
            ;; следующая точка трубы 
            (if IStrassa
              (setq
                truba
                 (append truba
                         (list (list xcoord
                                     (+ (nth 1 p_land)
                                        (/ (- (atof (nth 2 itold))
                                              (atof (nth 4 itold))
                                              land_first
                                           )
                                           vscale
                                        )
                                     )
                               )
                         )
                 )
              )
            )
          )
          (progn
            (setq
              dxold  dx
              dx     (atof (nth 1 items))
              shift  (+ shift dx)
              xcoord (+ (car sp) (/ shift hscale))
            )                           ;setq
            ;; первая точка трубы
            (if IStrassa
              (if (= truba nil)
                (setq
                  truba      (list (list (nth 0 p_land)
                                         (- (nth 1 p_land)
                                            (/ (atof (nth 4 items)) vscale)
                                         )
                                   )
                             )
                  land_first (atof (nth 2 items))
                )
              )
            )                           ;if
            ;; если первый эл-т не -2 - рисуем номер пикета
            (if (and ISpk (/= (nth 0 items) "-2"))
              (progn
                (setq cp (list xcoord (- ycoord 8) 0))
                (mktext (nth 0 items) cp 2.5 0 "style1" 1 0 7)
              )                         ;progn
            )                           ;if
            ;; рисуем расстояние м/у отметками
            (if (and ISdist (/= shift 0) (/= dx 100))
              (progn
                (if (<= (/ (+ dx dxold) hscale 2) 4)
                  (progn
                    (setq idx (+ idx 1))
                    (if (>= idx 3)
                      (setq idx 0)
                    )
                  )
                  (setq idx 0)
                )
                (setq cp (list (- xcoord (/ dx (* hscale 2)))
                               (- ycoord (nth idx ymas))
                               0
                         )
                )
                (mktext (okrugl (nth 1 items) 2)
                        cp
                        2.5
                        0
                        "style1"
                        1
                        0
                        7
                )
              )                         ;progn
              (setq idx 0)
            )                           ; if
            ;; раздвигаем близлежащие отметки
            (setq xnew xcoord)
            (if (< (- xnew xcold) 2.55)
              (setq xnew (+ xcold 2.55))
            )                           ;if
            ;; рисуем отметки НОЗ, верх трубы и глубину траншеи
            (if ISland
              (progn
                (setq cp (list xnew (- ycoord 33) 0))
                (mktext (okrugl (nth 2 items) 2)
                        cp
                        2.5
                        (rad 90)
                        "style1"
                        0
                        2
                        7
                )
              )
            )
            (if ISpipe
              (progn
                (setq cp (list xnew (- ycoord 48) 0))
                (mktext (okrugl (nth 3 items) 2)
                        cp
                        2.5
                        (rad 90)
                        "style1"
                        0
                        2
                        7
                )
              )
            )
            (setq u6 (okrugl (nth 3 items) 2))
            (if ISdeep
              (progn
                (setq cp (list xnew (- ycoord 67) 0))
                (mktext (okrugl (nth 4 items) 2)
                        cp
                        2.5
                        (rad 90)
                        "style1"
                        0
                        2
                        7
                )
              )
            )

            (setq xcold xnew)
            (setq itold items)
          )                             ;progn
        )                               ;if
      )                                 ;while
      (if IStrassa
        (draw_poly truba)
      )
      (osmode_on)
    )
  )
  (princ)
)

;;; c:otvod_list - загрузка и отрисовка таблицы гнутых отводов {{{1
(defun c:otvod_list (/ filename pt mylist xcoord ycoord str items)
  (setq
    filename "c:\\temp\\xoutot"
    pt       (getpoint "Укажите точку")
    mylist   (LoadData filename)
    xcoord   (nth 0 pt)
    ycoord   (nth 1 pt)
  )
  (setvar "DIMZIN" 0)
  (osmode_off)
  (style1)
  (while (/= mylist nil)
    (setq
      str    (car mylist)
      mylist (cdr mylist)
      items  (strtok str ",")
    )
    (setq cp (list xcoord ycoord))
    (mktext (nth 0 items) cp 3 0 "style1" 0 0 7)
    (setq cp (list (+ xcoord 30) ycoord))
    (mktext (nth 1 items) cp 3 0 "style1" 0 0 7)
    (setq cp (list (+ xcoord 60) ycoord))
    (mktext (if (= (nth 2 items) "0")
              "---"
              (nth 2 items)
            )
            cp
            3
            0
            "style1"
            0
            0
            7
    )
    (setq cp (list (+ xcoord 90) ycoord))
    (mktext (if (= (nth 3 items) "0")
              "---"
              (nth 3 items)
            )
            cp
            3
            0
            "style1"
            0
            0
            7
    )
    (setq cp (list (+ xcoord 120) ycoord))
    (mktext (strchg (nth 4 items) "\"" "")
            cp
            3
            0
            "style1"
            0
            0
            7
    )
    (setq ycoord (- ycoord 8))
  )
  (osmode_on)
)

;;; exportmarks - Экспорт натурных отметок земли и растояния м/у ними в текстовый файл {{{1
;;; TODO: изменить алгоритм определения типа отметок на проверку угла наклона отметки (0-растояния, 90-земля)
(defun c:exportmarks (/ ar status dxlist nlist name)
  (princ
    "Выберите все значения в строках НОЗ и растояния м/у отметками:"
  )
  (setq ar (ssget '((0 . "TEXT"))))
  (setq status 0
        dxlist nil
        nlist nul
  )
  (while (/= (ssname ar 0) nil)
    ;; ищем значения растояний м/у отметками - у них максимальное значение y
    (setq name (findmaxy ar))
    (setq ar (ssdel name ar))
    (cond
      ((= status 0)
       (progn (setq status 1
                    yfirst (egety name)
                    dxlist (append dxlist
                                   (list (cons (egetx name) (egettext name)))
                           )
              )
       )
      )
      ((= status 1)
       (if (> 8 (abs (- yfirst (egety name))))
         (setq
           dxlist (append dxlist
                          (list (cons (egetx name) (egettext name)))
                  )
         )
         (setq status 2
               nlist  (append nlist
                              (list (cons (egetx name) (egettext name)))
                      )
         )
       )
      )
      ((= status 2)
       (setq nlist
              (append nlist (list (cons (egetx name) (egettext name))))
       )
      )
    )
  )
                                        ; (princ dxlist)
                                        ; (princ nlist)
  (lsttofile (sortlst dxlist) (strcat *MDVTempDir* "dx"))
  (lsttofile (sortlst nlist) (strcat *MDVTempDir* "n"))
  (princ)
)                                       ;end exportmarks

;;; lsttofile - записать список в текстовый файл {{{1
(defun lsttofile (lst f / FH n old tmp)
  (setq FH (open f "w"))
  ;; (foreach n lst (write-line (strcat (strchg (car n) "." ",") " " (strchg (cdr n) "." ",")) FH))
  (setq tmp nil)
  (foreach n lst
    (if (/= (car n) (car tmp))
      (progn
        (write-line (strchg (cdr n) "," ".") FH)
                                        ;        (write-line (cdr n) FH)
        (setq tmp n)
      )
    )
  )
  (close FH)
)


;;; draw_poly - отрисовать полилинию по полученному списку точек {{{1
(defun draw_poly (lst / pold pnew)
;  (setvar "plinewid" 0.5)
  (entmake '((0 . "POLYLINE")    ; Object type
    (40 . 0.5)             ; start wide
    (41 . 0.5)             ; end wide
    (62 . 7)             ; Color
    (6 . "continuous")       ; Linetype
    (66 . 1)             ; Vertices follow
  ))
  
;  (setq
;    pold (car lst)
;    lst  (cdr lst)
; )
 
  (foreach n lst
;    (progn
;      (command "_pline" pold n "")
;      (setq pold n)
;    )
    (setq 
      EL 
      (list 
        '(0 . "VERTEX") 
        (cons 10 n)
      )
    )
    (entmake EL)
  )
  
  (entmake '((0 . "SEQEND")))    ; Sequence end 
)
