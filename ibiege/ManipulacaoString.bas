Attribute VB_Name = "ManipulacaoString"
Option Explicit

Public Function ContaItens(ByVal Lista As String, ByVal separador As String) As Long
  Dim i As Long
  Dim iCont As Long
  iCont = 1
  For i = 1 To Len(Lista)
    If Mid(Lista, i, Len(separador)) = separador Then
      iCont = iCont + 1
      i = i + Len(separador) - 1
    End If
  Next
  ContaItens = iCont
End Function

Public Function ContaItens2(ByVal Lista As String, ByVal separador As String) As Long
  Dim aTemp  As Variant
  aTemp = Split(Lista, separador)
  ContaItens2 = UBound(aTemp) + 1
End Function

Public Function PegaItem(ByVal Lista As String, ByVal separador As String, ByVal Posicao As Long) As String
    Dim i As Long
    Dim iCont As Long
    Dim sTemp As String
    iCont = 1
    For i = 1 To Len(Lista)
        If Mid(Lista, i, Len(separador)) = separador Then
            iCont = iCont + 1
        End If
        If iCont = Posicao Then
            sTemp = sTemp & Mid(Lista, i, 1)
        Else
            If iCont > Posicao Then Exit For
        End If
    Next
    If Posicao > 1 Then
        PegaItem = Mid(sTemp, 1 + Len(separador))
    Else
        PegaItem = sTemp
    End If
End Function

Public Function PegaItem2(ByVal Lista As String, ByVal separador As String, ByVal Posicao As Long) As String
    Dim aTemp  As Variant
    aTemp = Split(Lista, separador)
    PegaItem2 = aTemp(Posicao - 1)
End Function

Public Function RetiraItem(ByVal Lista As String, ByVal separador As String, ByVal Posicao As Long) As String
    Dim i As Long
    Dim iCont As Long
    Dim sTemp As String
    iCont = 1
    For i = 1 To Len(Lista)
        If Mid(Lista, i, Len(separador)) = separador Then
            iCont = iCont + 1
        End If
        If Not iCont = Posicao Then
            sTemp = sTemp & Mid(Lista, i, 1)
        End If
    Next
    If Left(sTemp, 1) = separador Then
        sTemp = Mid(sTemp, 1 + Len(separador))
    End If
    
    If Posicao = 1 Then
        sTemp = Mid(sTemp, 1 + Len(separador))
    End If
    
    RetiraItem = sTemp
End Function

Public Function RetiraItem2(ByVal Lista As String, ByVal separador As String, ByVal Posicao As Long) As String
    Dim i As Long
    Dim aTemp As Variant
    Dim sTemp As String
    
    aTemp = Split(Lista, separador)
    For i = LBound(aTemp) To UBound(aTemp)
        If i <> (Posicao - 1) Then
            sTemp = sTemp & separador & aTemp(i)
        End If
    Next
    
    RetiraItem2 = Mid(sTemp, Len(separador) + 1)
End Function

Public Function SubstituiItem(ByVal Lista As String, ByVal separador As String, ByVal Posicao As Long, ByVal ItemNovo As String) As String
    Dim i As Long
    Dim iCont As Long
    Dim sTemp As String
    Dim bInseriuItem As Boolean
    bInseriuItem = False
    iCont = 1
    For i = 1 To Len(Lista)
        If Mid(Lista, i, Len(separador)) = separador Then
            iCont = iCont + 1
        End If
        If Not iCont = Posicao Then
            sTemp = sTemp & Mid(Lista, i, 1)
        Else
            If Not bInseriuItem Then
                sTemp = sTemp & separador & ItemNovo
                bInseriuItem = True
            End If
        End If
    Next
    If Left(sTemp, 1) = separador Then
        sTemp = Mid(sTemp, 1 + Len(separador))
    End If
    
    If Posicao = 1 Then
        sTemp = Mid(sTemp, 1 + Len(separador))
    End If
    
    SubstituiItem = sTemp
End Function

Public Function SubstituiItem2(ByVal Lista As String, ByVal separador As String, ByVal Posicao As Long, ByVal ItemNovo As String) As String
    Dim i As Long
    Dim aTemp As Variant
    Dim sTemp As String
    
    aTemp = Split(Lista, separador)
    For i = LBound(aTemp) To UBound(aTemp)
        If i <> (Posicao - 1) Then
            sTemp = sTemp & separador & aTemp(i)
        Else
            sTemp = sTemp & separador & ItemNovo
        End If
    Next
    
    SubstituiItem2 = Mid(sTemp, Len(separador) + 1)
End Function
