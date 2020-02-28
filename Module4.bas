Attribute VB_Name = "Module4"
Dim stamp As String

Sub calc()
'
' calc Macro
'

Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim l As Integer

Dim cars As Integer
Dim tate As Integer, yoko As Integer
Dim segments As Integer, raw_routes As Integer
Dim kappa As Double, lamb As Double
Dim element As Integer
Dim pass As Range

cars = Range("W2")
tate = Range("W3")
yoko = Range("W4")
kappa = Range("W6")
lambda = Range("W7")

segments = Range("W10")
raw_routes = Range("W11")

For i = 0 To cars - 1
cnt = 0
    For j = 0 To raw_routes - 1
        Set pass = Range(Cells(13 + j, 11), Cells(13 + j, 11 + segments)).Find(What:=Cells(14 + i, 24))
'        Set pass = Range(Cells(13 + j, 11), Cells(13 + j, 11 + segments)).Find(What:=Range(Cells(14 + i, 24), Cells(14 + i, 22 + segments)))
        If Not pass Is Nothing Then
            cnt = cnt + 1
        End If
    Next j
    Cells(14 + i, 23 + segments).Value = cnt
Next i


'
End Sub
Sub save()
'
' save Macro
'

Dim cars As Integer
Dim tate As Integer, yoko As Integer
Dim segments As Integer, routes As Integer
Dim i As Integer, j As Integer, k As Integer

Worksheets("params").Cells.Clear
        
Worksheets("params").Cells(1, 1).Value = Worksheets(1).Range("W2")
Worksheets("params").Cells(1, 2).Value = Worksheets(1).Range("W3")
Worksheets("params").Cells(1, 3).Value = Worksheets(1).Range("W4")

Worksheets("params").Cells(1, 4).Value = Worksheets(1).Range("W6")
Worksheets("params").Cells(1, 5).Value = Worksheets(1).Range("W7")

cars = Worksheets(1).Range("W2")
For i = 1 To cars
    Worksheets("params").Cells(4, i).Value = Worksheets(1).Cells(13 + i, 24)
Next i

tate = Worksheets(1).Range("W3")
yoko = Worksheets(1).Range("W4")
segments = tate + yoko
routes = fact(segments) / (fact(tate) * fact(yoko))
k = 1
For i = 0 To routes - 1
    For j = 0 To segments - 1
        Worksheets("params").Cells(2, k).Value = Worksheets(1).Cells(13 + i, 6 + j)
        k = k + 1
    Next j
Next i

k = 1
For i = 0 To routes - 1
    For j = 0 To segments
        Worksheets("params").Cells(3, k).Value = Worksheets(1).Cells(13 + i, 7 + segments + j)
        k = k + 1
    Next j
Next i

Application.DisplayAlerts = False
    
stamp = Format(Now, "mmdd_hhmm")
Worksheets("params").Copy
ActiveWorkbook.SaveAs FileName:="C:\Users\watanabe.kevin\Desktop\d-wave_traffic\params\prm" + stamp + ".csv", _
    FileFormat:=xlCSV
ActiveWindow.Close
    
Application.DisplayAlerts = True


'
End Sub

Public Sub python_exe()
'
' python_exe Macro
'
     Call RunPython("import traffic; traffic.callDwave('" & stamp & "')")
'
End Sub


Sub output()
'
' output Macro
'

Dim n As Integer, i As Integer, j As Integer, k As Integer, ind As Integer, ind2 As Integer, count As Integer
Dim str1 As String, str2 As String, str3 As String, str4 As String, str5 As String, str6 As String
Dim buf As Integer, name As String
Dim cars As Integer, segments As Integer, max_routes As Integer
Dim kappa As Double, lambda As Double
Dim binary() As Variant, routes() As Variant
Dim str_lambda As String, str_kappa As String

cars = Range("W2")
segments = Range("W10")
'max_routes = Range("W11")
kappa = Range("W6")
lambda = Range("W7")

lambda = lambda * cars * segments
kappa = kappa * cars * segments

If lambda Mod 1 = 0 Then
    str_lambda = Format(lambda, "##########.0")
End If
If kappa Mod 1 = 0 Then
    str_kappa = Format(kappa, "##########.0")
End If
If lambda = 0 Then
    str_lambda = Format(lambda, "0.0")
End If
If kappa = 0 Then
    str_kappa = Format(kappa, "0.0")
End If

For i = 0 To cars - 1
    ReDim Preserve routes(i)
    routes(i) = Cells(14 + i, 23 + segments)
Next i

max_routes = WorksheetFunction.Max(routes)
n = 1
ind = 1
name = "C:\Users\watanabe.kevin\Desktop\d-wave_traffic\Dwave" & stamp & "_" & cars & "_" & max_routes & "_" & str_kappa & "_" & str_lambda & ".csv"
Open name For Input As #1
  
'    Do While Not EOF(n)
'        i = i + 1
    Input #n, str1, str2, str3, str4, str5

    j = 0
    str6 = Mid(str1, 2, InStr(str1, "}") - 2)
        
    Do While ind <> 0
        s = CStr(j)
        t = "x" + s
        If j < 10 Then
            buf = Mid(str1, InStr(1, str1, t) + 5, 1)
        ElseIf j < 100 Then
            buf = Mid(str1, InStr(1, str1, t) + 6, 1)
        Else
            buf = Mid(str1, InStr(1, str1, t) + 7, 1)

        End If
                
        ReDim Preserve binary(j)
        binary(j) = buf
        
        j = j + 1
        ind = InStr(str1, j)
    Loop
    
    ind = 0
    For i = 0 To cars - 1
        count = 1
        ind2 = 0
        For k = 0 To routes(i) - 1
            If binary(ind) = 1 Then
'                Cells(14 + i, 24 + segments + ind2).Value = "route " & count
                Cells(14 + i, 24 + segments + ind2).Value = count
                ind2 = ind2 + 1
            End If
            ind = ind + 1
            count = count + 1
        Next k
    Next i
'    Loop
    
    'End If
    
    'If ind <> j Then
    'Cells(9, 18).Value = "dif " & (ind - j)
    
    Close #n
'
End Sub

Sub exe()
'
' exe Macro
'
Call save

Call calc

Call python_exe

Call output
'
End Sub


Function fact(n As Integer) As Integer
Dim i As Integer

    If n = 1 Then
        fact = 1
    Else
        fact = n * fact(n - 1)
    End If
 
End Function

