;;; Utils for AutoCAD 14
;;; text editing functions
;;; (C) Milkov Dmitry, 2001-2008
;;; <Last edition 12.03.02>

;;; �������� ��� ��������� ������� FR � ������ TX �� ������ TO
(defun strchg (TX FR TO)
(apply 'strcat (subst TO FR (strtolist TX)))
);end strchg

;;; ������������� ������ � ������ �������� � ������� ���
(defun strtolist (TX / LST)
(setq LST nil)
(while (> (strlen TX) 0)
    (setq LST (append LST (list (substr TX 1 1))) TX (substr TX 2))
);while
LST
);end strtolist

;;; ��������� ������ �� ������������
(defun strtok (str razd / retlist item CN Ch)
(setq CN 1 retlist nil item "")
(while (<= CN (strlen str))
   (setq Ch (substr str CN 1) CN (1+ CN))
   (cond
      ((= Ch razd)
         (setq retlist (append retlist (list item)) item "")
      ); razd
      (T
         (setq item (strcat item Ch))   
      ); T
   );cond
);while
(if (/= item "")
   (setq retlist (append retlist (list item)) item "")
);if
retlist
);end

;;; ������� ������� ������ ������� �� ������
(defun ltrim (str / CN FLAG)
(setq CN 1 FLAG 1 SL (strlen str))
(while (and (<= CN SL) (= FLAG 1))
   (if (= " " (substr str CN 1))
      (setq CN (1+ CN))
      (setq FLAG 0)   
   );if
);while
(substr str CN)
);end

;;; ������� �������� ������� �� ������
(defun rtrim (str / CN FLAG)
(setq CN (strlen str) FLAG 1)
(while (and (< 0 CN) (= FLAG 1))
    (if (= " " (substr str CN 1))
        (setq CN (1- CN))
        (setq FLAG 0)
    );if
);while
(substr str 1 CN)
);end strim

;;; ������� ��������� � �������� ������� �� ��������� ������
(defun trim (str)
(ltrim (rtrim str))
);end trim

(defun remove_a_tag (
    string
);params
    (if 
        (wcmatch string "\\A#;*") 
        (substr string 5)
        string
    );if
);remove_text_Ateg

;;; ��������� �����, �������������� � ��������� ���� �� �������� ��������
(defun okrugl (str prec)
(rtos (atof str) 2 prec)
);end nulls