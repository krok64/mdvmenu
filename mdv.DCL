mdv_mainDialog : dialog {
    label = "Параметры вывода"; 
    : boxed_column {
      label="Масштаб трассы газопровода";
      : edit_box {
	label="По горизонтали (1:Х), где Х=";
	key="mhor";
	value="1000";
	edit_width=6;	
	alignment="rigth";
	}
      : edit_box {
	label="По вертикали (1:Х), где Х=";
	key="mver";
	value="200";
	edit_width=6;
	alignment="rigth";
	}
}
    : boxed_column {
      label="Элементы";
      : toggle {
	label="Выноски вертикальных углов";
	key="md_el";
	value="1";
      }
      : toggle {
	label="Трасса трубопровода";
	key="md_trassa";
	value="1";
      }
      : toggle {
	label="Пикетаж";
	key="md_pk";
	value="1";
      }
      : toggle {
	label="Расстояния между отметками";
	key="md_dist";
	value="1";
      }
      : toggle {
	label="Натурные отметки земли";
	key="md_land";
	value="1";
      }
      : toggle {
	label="Проектные отметки (верх трубы)";
	key="md_pipe";
	value="1";
      }
      : toggle {
	label="Длина участка/Уклон";
	key="md_i";
	value="1";
      }
      : toggle {
	label="Глубина траншеи";
	key="md_deep";
	value="1";
      }

     }
    : row {          // defines the OK/Cancel button row
        : spacer { width = 1; }
        : button {    // defines the OK button
            label = "OK";
            is_default = true;
            key = "accept";
            width = 12;
            fixed_width = true;
        }
        : button {    // defines the Cancel button
            label = "Отмена";
            is_cancel = true;
            key = "cancel";
            width = 12;
            fixed_width = true;
        }
        : spacer { width = 1;}
    }
}