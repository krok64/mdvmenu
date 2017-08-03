Attribute VB_Name = "NewMacros"
Sub Макрос2()
Attribute Макрос2.VB_ProcData.VB_Invoke_Func = "Normal.NewMacros.Макрос2"
'
' Макрос2 Макрос
'
'
For Each atable In ActiveDocument.Tables
    Dim i1 As Double, i2 As Double, i3 As Double
    For j = 3 To atable.Rows.Count
        atable.Cell(j, 2).Select
        If Left(Selection.Text, 3) = "н/д" Then
            atable.Cell(j - 1, 4).Select
            StrToDouble Selection.Text, i1
            atable.Cell(j, 4).Select
            StrToDouble Selection.Text, i2
            If (i1 > 0 And i2 > 0) Or (i1 < 0 And i2 < 2) Then
                znak = -1
            Else
                znak = 1
            End If
            i3 = Abs(i1 + i2 * znak)
            gr = Int(RadToGrad(Atn(i3)))
            mi = (RadToGrad(Atn(i3)) - Int(RadToGrad(Atn(i3)))) * 60
            atable.Cell(j, 2).Select
            Selection.Delete
            Selection.TypeText Text:=Format(gr) + "°" + Format(mi, "0") + "'"
        End If
    Next j
Next atable

End Sub

Sub tablenum()
Dim tb As Table, BeginVal As Integer, k As String, first As Boolean
Set tb = ActiveDocument.Tables.Item(1)
BeginVal = 1
first = True
For i = 1 To tb.Rows.Count
    tb.Cell(i, 1).Select
    k = Trim(Selection.Text)
    If Len(k) > 2 Then
        If first Then
            BeginVal = CInt(Mid(k, 1, Len(k) - 2))
            first = False
        End If
        Selection.Text = Format(BeginVal)
        BeginVal = BeginVal + 1
    End If
Next i
End Sub

Function RadToGrad(rad As Double) As Double
    RadToGrad = rad * 57.2957795
End Function

Function StrToDouble(s As String, ByRef d As Double) As Boolean
' переводит строку в число, разделителем целой и дробной части может быть как . так и ,
' в случае успешного перевода возвращает true иначе false
Dim i As Integer, ch, dflag As Boolean, k As Integer
dflag = False: k = 0: s = Trim(s): flagminus = False: d = 0
For i = 1 To Len(s)
    ch = Mid(s, i, 1)
    Select Case ch
        Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
            d = d * 10 + CInt(ch)
            If dflag Then k = k + 1
        Case ",", "."
            If Not dflag Then
                dflag = True
            Else
                StrToDouble = False
                Exit For
            End If
        Case "-"
            flagminus = True
'        Case Else
'            StrToDouble = False
'            Exit For
    End Select
    StrToDouble = True
Next i
If k > 0 Then d = d / 10 ^ k
If flagminus Then d = d * (-1)
End Function
