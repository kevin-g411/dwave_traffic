Attribute VB_Name = "Module1"
Sub start()
Attribute start.VB_ProcData.VB_Invoke_Func = " \n14"
'
' start Macro
'

Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim l As Integer

Dim cars As Integer
Dim y As Integer, x As Integer
Dim segments As Integer, raw_routes As Integer
Dim kappa As Double, lamb As Double
Dim elements As Integer
Dim seg_arr() As Variant

cars = Range("W2")
y = Range("W3")
x = Range("W4")
kappa = Range("W6")
lambda = Range("W7")

ReDim Preserve seg_arr(2 * y + 1, x + 1)
segments = y + x
raw_routes = fact(segments) / (fact(y) * fact(x))

Range(Cells(13, 23), Cells(33, 25 + segments)).Select
Selection.UnMerge
Selection.ClearContents

Range("A1:S11").Select
Selection.UnMerge
Selection.ClearContents
Selection.Borders(xlDiagonalDown).LineStyle = xlNone
Selection.Borders(xlDiagonalUp).LineStyle = xlNone
Selection.Borders(xlEdgeLeft).LineStyle = xlNone
Selection.Borders(xlEdgeTop).LineStyle = xlNone
Selection.Borders(xlEdgeBottom).LineStyle = xlNone
Selection.Borders(xlEdgeRight).LineStyle = xlNone
Selection.Borders(xlInsideVertical).LineStyle = xlNone
Selection.Borders(xlInsideHorizontal).LineStyle = xlNone

Range("X13").Value = "経由地点"
Range(Cells(13, 24), Cells(13, 22 + segments)).Select
With Selection
            .HorizontalAlignment = xlCenter
            .VerticalAlignment = xlCenter
End With
Selection.Merge

For i = 24 To 22 + segments
    Columns(i).ColumnWidth = 3.63
Next i

Cells(13, 23 + segments).Value = "該当ルート数"
Columns(23 + segments).EntireColumn.AutoFit

Cells(13, 24 + segments).Value = "提案ルート"
Columns(24 + segments).EntireColumn.AutoFit


Range("B2").Value = "A"
For i = 0 To y - 1
    For j = 0 To x - 1
        
    Range(Cells(3 + i * 4, 3 + j * 4), Cells(6 + i * 4, 6 + j * 4)).Select
        Selection.Borders(xlDiagonalDown).LineStyle = xlNone
        Selection.Borders(xlDiagonalUp).LineStyle = xlNone
            With Selection.Borders(xlEdgeLeft)
                .LineStyle = xlContinuous
                .ColorIndex = 0
                .TintAndShade = 0
                .Weight = xlMedium
            End With
            With Selection.Borders(xlEdgeTop)
                .LineStyle = xlContinuous
                .ColorIndex = 0
                .TintAndShade = 0
                .Weight = xlMedium
            End With
            With Selection.Borders(xlEdgeBottom)
                .LineStyle = xlContinuous
                .ColorIndex = 0
                .TintAndShade = 0
                .Weight = xlMedium
            End With
            With Selection.Borders(xlEdgeRight)
                .LineStyle = xlContinuous
                .ColorIndex = 0
                .TintAndShade = 0
                .Weight = xlMedium
            End With
            Selection.Borders(xlInsideVertical).LineStyle = xlNone
            Selection.Borders(xlInsideHorizontal).LineStyle = xlNone
            
    If j = 0 Then
        Range(Cells(4 + i * 4, 2 + j * 4), Cells(5 + i * 4, 2 + j * 4)).Select
        With Selection
            .HorizontalAlignment = xlCenter
            .VerticalAlignment = xlCenter
        End With
        Selection.Merge
    End If
    
    If i = y - 1 Then
        Range(Cells(6 + i * 4, 4 + j * 4), Cells(6 + i * 4, 5 + j * 4)).Select
        With Selection
            .HorizontalAlignment = xlCenter
            .VerticalAlignment = xlCenter
        End With
        Selection.Merge
    End If

    Range(Cells(2 + i * 4, 4 + j * 4), Cells(2 + i * 4, 5 + j * 4)).Select
    With Selection
        .HorizontalAlignment = xlCenter
        .VerticalAlignment = xlCenter
    End With
    Selection.Merge
    Range(Cells(4 + i * 4, 6 + j * 4), Cells(5 + i * 4, 6 + j * 4)).Select
    With Selection
        .HorizontalAlignment = xlCenter
        .VerticalAlignment = xlCenter
    End With
    Selection.Merge
    Next j
Next i

elements = x * (y + 1) + y * (x + 1)
For i = 0 To x - 1
    Cells(2, 4 + i * 4).Value = i
    Cells(2, 6 + i * 4).Value = Chr(i + 66)
    
Next i
k = x
l = 0
For i = 0 To y - 1
    For j = 0 To x
        Cells(4 + i * 4, 2 + j * 4).Value = k
        k = k + 1
    Next j
    For j = 0 To x - 1
        Cells(6 + i * 4, 4 + j * 4).Value = k
        Cells(6 + i * 4, 2 + j * 4).Value = Chr(l + 66 + x)
        k = k + 1
        l = l + 1
    Next j
    l = l + 1
Cells(6 + i * 4, 2 + j * 4).Value = Chr(l + 65 + x)
Next i

Range(Cells(13, 6 + segments), Cells(13 + raw_routes, 8 + 2 * segments)).Select
Selection.ClearContents

For i = 0 To raw_routes - 1
    Cells(13 + i, 7 + segments).Value = Chr(65)
Next i
ind = 0
For i = 0 To (x + 1) * (2 * y + 1) - (y + 2)
    For j = 0 To raw_routes - 1
        Set pass = Range(Cells(13 + j, 6), Cells(13 + j, 5 + segments)).Find(i, LookAt:=xlWhole)
        If Not pass Is Nothing Then
            If i = 0 Or i = x Then
                Cells(13 + j, 8 + segments).Select
            Else
                Cells(13 + j, 7 + segments).End(xlToRight).Offset(0, 1).Select
            End If
            Selection = Chr(66 + ind)
        End If
    Next j
    ind = ind + 1
    If (i Mod (2 * x + 1)) = (2 * x) Then
        ind = ind - x
    End If
Next i

Range("W10").Value = segments
Range("W11").Value = raw_routes

For i = 1 To cars
    Cells(13 + i, 23).Value = "car " & i
Next i

End Sub

Function fact(n As Integer) As Integer
Dim i As Integer

    If n = 1 Then
        fact = 1
    Else
        fact = n * fact(n - 1)
    End If
 
End Function
