mdv_mainDialog : dialog {
    label = "��������� ������"; 
    : boxed_column {
      label="������� ������ �����������";
      : edit_box {
	label="�� ����������� (1:�), ��� �=";
	key="mhor";
	value="1000";
	edit_width=6;	
	alignment="rigth";
	}
      : edit_box {
	label="�� ��������� (1:�), ��� �=";
	key="mver";
	value="200";
	edit_width=6;
	alignment="rigth";
	}
}
    : boxed_column {
      label="��������";
      : toggle {
	label="������� ������������ �����";
	key="md_el";
	value="1";
      }
      : toggle {
	label="������ ������������";
	key="md_trassa";
	value="1";
      }
      : toggle {
	label="�������";
	key="md_pk";
	value="1";
      }
      : toggle {
	label="���������� ����� ���������";
	key="md_dist";
	value="1";
      }
      : toggle {
	label="�������� ������� �����";
	key="md_land";
	value="1";
      }
      : toggle {
	label="��������� ������� (���� �����)";
	key="md_pipe";
	value="1";
      }
      : toggle {
	label="����� �������/�����";
	key="md_i";
	value="1";
      }
      : toggle {
	label="������� �������";
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
            label = "������";
            is_cancel = true;
            key = "cancel";
            width = 12;
            fixed_width = true;
        }
        : spacer { width = 1;}
    }
}