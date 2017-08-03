MDVMENU - это куча полезного стаффа для проектирования линейной части 
магистральных трубопроводов.

Пакет состоит из следующих частей:

mdv5??.xls     - лист для расчета положения трубы в земле
calculator.xls - куча всяких дополнительных расчетов
info.xls       - куча полезной инфы по проектированию
*.lsp, mdv.dvb - движок программы, для работы в системе Автокад, инструкцию по 
                 установке смотри в файле install.txt
*.bmp          - иконки для тулбаров
.hg/           - файлы системы контроля версий mercurial
dx,n,xlsout    - файлы для обмена данными между vba и vlisp
gost_*,TU*     - база с размерами трубопроводной арматуры
mdv.mnr,mdv.mnu - файлы меню
mdv.cuix
mdv.dcl        - файл описания диалога
config.ini     - общий файл настроек
SO_VR_GEN.mdb  - генератор спецификаций и объемов работ (устарел, используйте веб версию)
GOST_SNG.ttf   - шрифт, используемый по умолчанию во ВНИИГАЗЕ
*.txt          - файлы описания, включая текущий

Установка:

English

1. Copy directory MDVMENU to the root of disk D:\
2. Run AutoCAD, print _appload <ENTER>, select AutoLoad
and add files mdv.lsp and mdv.dvb.
3. Print _options <ENTER>, select tab FILES, select PATH FOR SUPPORT FILES, 
press Add, Browse, select d:\mdvmenu
4.1. If AutoCad version is 2006 print _cui and add mdv.mnu
4.2. If AutoCad version is 2004 or early print _menuload and add mdv.mnu
5. It's all, enjoy! :)

(с) Dmitry Milkov, send wishes and bugreports to krok64@mail.ru

Русский

1. Скопируйте папку MDVMENU в корень диска D:\
2. Запустите Автокад, наберите _appload <ENTER>, нажмите кнопку ПРИЛОЖЕНИЯ в
области АВТОЗАГРУЗКА. Нажмите кнопку ДОБАВИТЬ и выберите файлы mdv.lsp и mdv.dvb
из папки MDVMENU.
3. Наберите _options <ENTER>, выберите вкладку ФАЙЛЫ, пункт ПУТЬ ДОСТУПА К
   ВОСПОМОГАТЕЛЬНЫМ ФАЙЛАМ, нажмите ДОБАВИТЬ, нажмите ОБЗОР, выберите папку 
   MDVMENU.
4. Добавьте файл меню mdv.mnu к меню автокада.
5. Усе! :)

Пожелания, отчеты о найденных ошибках пишете мне на адрес krok64@mail.ru
Милькову Дмитрию.

(с) Dmitry Milkov, 2001-2016
