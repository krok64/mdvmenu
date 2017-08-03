Attribute VB_Name = "MDVvbALL"
Option Explicit
Option Base 0
'18.08.2013 Общий модуль для всех vb приложений

Public Function achar(str As String, chnum As Integer) As String
'18.08.2013 возвращает символ строки под заданным номером (номер начинается с 1)
    achar = Mid(str, chnum, 1)
End Function

Function ArcCos(X As Double) As Double
'18.08.2013 вычисляет арккосинус. параметр задавать в радианах
    ArcCos = Atn(-X / Sqr(-X * X + 1)) + 2 * Atn(1)
End Function

Function Arcsin(X As Double) As Double
'18.08.2013 вычисляет арксинуса. параметр задавать в радианах
    Arcsin = Atn(X / Sqr(-X * X + 1))
End Function

Public Function dtr(a As Double) As Double
'18.08.2013 Convert angle in degrees to radians
    dtr = (a / 180) * pi
End Function

Function Gradus(Ugol As Double)
'18.08.2013 переводит радианы в градусы. параметр задавать в радианах
    Gradus = 180 * Ugol / pi
End Function

Function MtoPK(m As Double) As String
'конвертит метраж в ПК. например 172010,5 в ПК1720+10,5
'07.09.2013 f округление до сотых метра
Dim pk As Long, met As Double
    pk = Int(m / 100)
    met = m - pk * 100
    MtoPK = "ПК" + format(pk) + "+" + format(met, "00.00")
End Function

Function Rad(Ugol As Double)
'18.08.2013 переводит градусы в радианы. параметр задавать в градусах
'TODO - использовать dtr
    Rad = pi * Ugol / 180
End Function

Public Function RegExp(strData As String, strPattern As String, Optional bolIgnoreCase As Boolean = False)
'18.08.2013 тестирует строку на регулярное выражение
Dim re As Object
    Set re = CreateObject("vbscript.regexp")
    re.Pattern = strPattern
    re.IgnoreCase = bolIgnoreCase
    If re.test(strData) Then
        RegExp = True
    Else
        RegExp = False
    End If
End Function

Public Function RegExpReplace(strData As String, strFrom As String, strTo As String, Optional bolIgnoreCase As Boolean = False, Optional bolGlobal As Boolean = True) As String
'18.08.2013 выпоняет замену в строке с помошью регулярного выражения
Dim re As Object
    Set re = CreateObject("vbscript.regexp")
    re.Pattern = strFrom
    re.IgnoreCase = bolIgnoreCase
    re.Global = bolGlobal
    RegExpReplace = re.Replace(strData, strTo)
End Function

Function StrChg(str As String, sfrom As String, sto As String) As String
'18.08.2013 выпоняет замену в строке одного символа на другой 1 раз
Dim pos As Long
    pos = InStr(str, sfrom)
    If pos = 0 Then
        StrChg = str
        Exit Function
    End If
    StrChg = Left(str, pos - 1) + sto + Mid(str, pos + Len(sfrom))
End Function

Function StrToDouble(s As String, ByRef D As Double) As Boolean
'18.08.2013 переводит строку в число, разделителем целой и дробной части может быть как . так и ,
'в случае успешного перевода возвращает true иначе false
Dim i As Integer, ch, dflag As Boolean, k As Integer, minus As Integer
dflag = False: k = 0: s = Trim(s): minus = 1: D = 0
If Left(s, 1) = "-" Then
    s = Mid(s, 2)
    minus = -1
End If
For i = 1 To Len(s)
    ch = Mid(s, i, 1)
    Select Case ch
        Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
            D = D * 10 + CInt(ch)
            If dflag Then k = k + 1
        Case ",", "."
            If Not dflag Then
                dflag = True
            Else
                StrToDouble = False
                Exit For
            End If
        Case Else
            StrToDouble = False
            Exit For
    End Select
    StrToDouble = True
Next i
If k > 0 Then D = D / 10 ^ k
D = D * minus
End Function

