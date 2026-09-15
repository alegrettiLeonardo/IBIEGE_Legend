Attribute VB_Name = "MFind"
'===============================================================================
' Name: MFind
' Purpose:
' Functions:
' Properties:
' Methods:
' Author: mpiazera
' Start:
' Modified:
'===============================================================================
Option Explicit

'===============================================================================
' Name: USEB
' Input:
' Output:
' Purpose: usar buffer
' Remarks:
' Author:
'===============================================================================
Private Const USEB As Boolean = False

'===============================================================================
' Name: MAXB
' Input:
' Output:
' Purpose: numero de variaveis no buffer
' Remarks:
' Author:
'===============================================================================
Private Const MAXB As Integer = 10

'===============================================================================
' Name: Buf
' Input:
'   ByRef Nom As String  - nome
'   ByRef Row As Integer - linha
'   ByRef Nxt As Integer - ocorrencia
'   ByRef Vlr As String  - valor
' Output:
' Purpose: guarda informacoes da variavel
' Remarks:
' Author:
'===============================================================================
Private Type Buf  '
   Nom As String  'nome
   Row As Integer 'linha
   Nxt As Integer 'ocorrencia
   Vlr As String  'valor
End Type

'Public aLin() As String 'vetor de linhas do arquivo
'===============================================================================
' Name: aBuf
' Input:
' Output:
' Purpose: vetor de variaveis do buffer
' Remarks:
' Author:
'===============================================================================
Private aBuf(MAXB) As Buf

'===============================================================================
' Name: lElm
' Input:
' Output:
' Purpose: ultimo valor no buffer
' Remarks:
' Author:
'===============================================================================
Private lElm As Integer

'===============================================================================
' Name: getBuf
' Input:
'   ByVal Name As String
'   ByVal Optional lin As Integer = 1
'   ByVal Optional Nxt As Integer = 1
' Output:
' Purpose:
' Remarks:
' Author:
'===============================================================================
Private Function getBuf(ByVal Name As String, Optional ByVal lin As Integer = 1, Optional ByVal Nxt As Integer = 1) As String
  'procura variavel no buffer
  Dim Idx As Integer
  For Idx = 0 To lElm
     If ((aBuf(Idx).Nom = UCase$(Name)) And (aBuf(Idx).Nxt = Nxt) And (aBuf(Idx).Row = lin)) Then
        getBuf = aBuf(Idx).Vlr
        Exit Function
     End If
  Next
  'nao achou
End Function
'
'===============================================================================
' Name: putBuf
' Input:
'   ByVal Vlr As String
'   ByVal Name As String
'   ByVal Optional lin As Integer = 1
'   ByVal Optional Nxt As Integer = 1
' Output:
' Purpose:
' Remarks:
' Author:
'===============================================================================
Private Function putBuf(ByVal Vlr As String, ByVal Name As String, Optional ByVal lin As Integer = 1, Optional ByVal Nxt As Integer = 1) As Boolean
  'coloca variavel no bufer
  Dim Idx As Integer, nIt As Integer
  '
  nIt = IIf(lElm + 1 > MAXB, MAXB, lElm + 1)
  For Idx = lElm To 1 Step -1
    aBuf(Idx) = aBuf(Idx - 1)
  Next
  'guarda informacoes na primeira posicao do buffer
  aBuf(0).Vlr = Vlr
  aBuf(0).Nom = UCase$(Name)
  aBuf(0).Nxt = Nxt
  aBuf(0).Row = lin
  lElm = nIt
  putBuf = True
  '
End Function
'
'===============================================================================
' Name: Find
' Input:
'   ByRef aLin As String
'   ByVal Name As String
'   ByVal Optional lin As Integer = 1
'   ByVal Optional Nxt As Integer = 1
'   ByRef Optional RetLinha As Integer = 0
'   ByRef Optional LinIni As Integer = 0
' Output:
' Purpose:
' Remarks:
' Author:
'===============================================================================
Public Function Find(aLin() As String, ByVal Name As String, Optional ByVal lin As Integer = 1, _
                Optional ByVal Nxt As Integer = 1, Optional RetLinha As Integer = 0, _
                Optional LinIni As Integer = 0) As String
  'procura por variavel
  Dim Tmp As String
  Dim Idx As Integer, Id1 As Integer, Id2 As Integer, Id3 As Integer
  Dim Tam As Integer, Cnt As Integer

  '
  If USEB Then Find = getBuf(Name, lin, Nxt): If Find > "" Then Exit Function
  '
  Tam = Len(Name)
  For Idx = LinIni To UBound(aLin)
    Id1 = InStr(UCase$(aLin(Idx)), " " & UCase$(Name) & " ")
    If Id1 = 0 Then Id1 = InStr(UCase$(aLin(Idx)), " " & UCase$(Name) & ":")
    If Id1 = 0 Then Id1 = InStr(UCase$(aLin(Idx)), " " & UCase$(Name) & "=")
    If Id1 = 0 Then Id1 = InStr(UCase$(aLin(Idx)), " " & UCase$(Name))
    If Id1 = 0 Then Id1 = InStr(" " & UCase$(aLin(Idx)), " " & UCase$(Name))
    If Id1 > 0 Then
      Cnt = Cnt + 1
      If (InStr(aLin(Idx), ":") = 0) And (InStr(aLin(Idx), "=") = 0) Then
        If (Cnt = Nxt) Then
          Id2 = ((Id1) \ 7) * 7 + 2   'alterado por marcos
          Find = Trim(Mid$(aLin(Idx + lin), Id2, 7))
          If USEB Then putBuf Find, Name, lin, Nxt
          RetLinha = Idx
          Exit Function
        End If
      ElseIf (Cnt = Nxt) Then
        Tmp = RemEsp(UCase$(aLin(Idx)))
        Id1 = InStr(Tmp, UCase$(Name) & " ")
        If Id1 = 0 Then Id1 = InStr(Tmp, UCase$(Name) & ":")
        If Id1 = 0 Then Id1 = InStr(Tmp, UCase$(Name) & "=")
        If Id1 > 0 Then Id2 = InStr(Id1 + 1, Tmp, ":")
        If Id2 = 0 Then
          If Id1 > 0 Then Id2 = InStr(Id1 + 1, Tmp, "=")
        End If
        If Id2 > 0 Then
          Id3 = InStr(Id2 + 2, Tmp, " ")
          If Id3 = 0 Then Id3 = Len(Tmp) + 1
        End If
        If Id3 > 0 And Id2 > 0 Then
          Find = Trim(Mid$(Tmp, Id2 + 1, Id3 - Id2 - 1))
          If USEB Then putBuf Find, Name, lin, Nxt
          RetLinha = Idx
          Exit Function
        End If
      End If
    End If
  Next
  '
End Function
'
'===============================================================================
' Name: RemEsp
' Input:
'   ByVal Str As String
' Output:
' Purpose:
' Remarks:
' Author:
'===============================================================================
Private Function RemEsp(ByVal Str As String) As String
  'elimina espacos em branco duplos
  Do
    Str = Replace(Str, "  ", " ")
  Loop Until InStr(Str, "  ") = 0
  RemEsp = Trim$(Str)
  '
End Function
'
'===============================================================================
' Name: ChkArq
' Input:
'   ByVal NomArq As String
'   ByRef Optional ErrMsg As String
' Output:
' Purpose:
' Remarks:
' Author:
'===============================================================================
Private Function ChkArq(ByVal NomArq As String, Optional ErrMsg As String) As Boolean
  'verifica arquivo
  On Local Error GoTo TrtErrForm1ChkArq
  Dim fHnd As Integer
  If Dir$(NomArq) = "" Then 'verifica se arquivo existe
    ErrMsg = "Arquivo " & LCase$(NomArq) & " não encontrado"
    Exit Function
  End If
  fHnd = FreeFile() 'obtem file handdle
  Open NomArq For Input Shared As #fHnd
  'verifica se arquivo esta vazio
  If LOF(fHnd) = 0 Then
    ErrMsg = "Arquivo " & LCase$(NomArq) & " está vazio"
  Else
    ChkArq = True
  End If
  Close #fHnd
  Exit Function
  'saindo
TrtErrForm1ChkArq:
  Close #fHnd
  ErrMsg = Error$
End Function
'
'===============================================================================
' Name: LeMsk
' Input:
'   ByVal NomArq As String
'   ByRef Optional ErrMsg As String
' Output:
' Purpose:
' Remarks:
' Author:
'===============================================================================
Public Function LeMsk(ByVal NomArq As String, Optional ErrMsg As String) As String()
  'le arquivo de entrada
  On Local Error GoTo TrtErrForm1LeMsk
  Dim lin As String, Buf As String
  Dim Tmp() As String, Tst() As String
  Dim fHnd As Integer, Cnt As Integer
  Dim nOk As Boolean
  '
  ReDim LeMsk(-1 To -1)
  'verifica arquivo
  If Not ChkArq(NomArq, ErrMsg) Then Exit Function
  fHnd = FreeFile() 'obtem file handdle
  Open NomArq For Input Shared As #fHnd
  Do While Not EOF(fHnd) 'le as linhas do arquivo
    Line Input #fHnd, lin
    Tst = Split(RemEsp(lin), " ")
    If (InStr(lin, "(") = 0 And InStr(lin, "---") = 0 And InStr(lin, "***") = 0 And InStr(lin, "===") = 0 And UBound(Tst) > 0 And _
        Len(Trim$(lin)) > 0 And Left$(Trim$(lin), 1) <> Chr(12)) Or (InStr(lin, "=") > 0 And InStr(lin, "===") = 0) Then
      ReDim Preserve Tmp(Cnt)
      Tmp(Cnt) = lin
      Cnt = Cnt + 1
    End If
  Loop
  Close #fHnd 'fecha
  LeMsk = Tmp 'atribui vetor lido
  Exit Function 'ok, retorna
  '
TrtErrForm1LeMsk:
  Close #fHnd
  ErrMsg = Error$
End Function
