Attribute VB_Name = "Publico"
Option Explicit

Public Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Private Declare Function WNetGetUser Lib "mpr.dll" Alias "WNetGetUserA" (ByVal lpName As String, ByVal lpUserName As String, lpnLength As Long) As Long
Private Declare Function GetUserName Lib "advapi32.dll" Alias "GetUserNameA" (ByVal lpBuffer As String, nSize As Long) As Long

Private Const CKEY As String = "WeGiNdUsTrIaSdIvMaQuInAs", SSG As String = "ungesichert"
Public Const NMI As String = "iBiege.ini" 'nome do arquivo ini
'
Private Type Ini 'buffer ini
  Sec As String 'secao
  Chv As String 'chave
  Vlr() As String 'valor
End Type
Private Type GENPAR 'parametros genericos
  TmpPth As String  'path drive temporario
  LocPth As String 'path para o aplicativo
  NomIni As String 'nome do arquivo ini
  AppIdf As Long 'id gerado em init para identificacao do aplicativo
End Type
Private Type ODBC
  Or As Boolean 'oracle?
  Pr As String 'schema
  C1 As String 'carcater de substituicao like uma posical
  Ct As String 'carcater de substituicao like tudo
End Type
Private Type USUPAR 'parqametros usuario
  Nom As String  'nome do usuario
End Type
'
Public ParIni() As Ini 'buffer arquivo ini
Public AppCfg As GENPAR 'parametros do aplicativo
Public ParUsu As USUPAR 'parametros usuario
Public ParDB  As ODBC 'parametros banco dados

Public AdoCon As Object
Public IDConE As Long

Public tem_costelado As Boolean
Public linha_costelado As Integer

Public tem_oco As Boolean
Public linha_oco As Integer

Public tem_con As Boolean
Public linha_con As Integer

Public Const ind_red = 0
Public Const ind_cost = 1
Public Const ind_oco = 2

Public Const BkgPck = &HC0FFFF
Public Const BckNml = &H80000005

Public sgmntpdr As Integer

Public RegAdocn As String

Public Function StrIni(ByVal Arq As String, ByVal Sec As String, ByVal Chv As String, Optional Vlr As String) As String
  'le parametros do ini
  Dim Ret As Long
  Vlr = Space$(512)
  Ret = GetPrivateProfileString(Sec, Chv, "", Vlr, 512, Arq)
  If Ret > 0 Then Vlr = Trim$(Left$(Vlr, Ret)): StrIni = Vlr Else Vlr = ""
  '
End Function

'===============================================================================
' Name: Function GetNom
' Input:
'   ByRef Optional ErrMsg As String - Retorna texto da mensagem de erro
' Output:
'   String - Retorna o nome do usuario
' Purpose: Obtem nome do usuario do sistema operacional
' Remarks: None
'===============================================================================
Public Function GetNom(Optional ErrMsg As String) As String
  '
  Dim User As String, Dummy As String
  Dim Idx As Long
  '
  User = Space$(12)
  Idx = GetUserName(User, 12) 'tenta advpai
  If Idx <> 0 And Trim$(User) > "" Then
    Idx = InStr(User, Chr(0))
    If Idx > 0 Then User = Left$(User, Idx - 1) 'ok
  Else 'agora tenta mpr
    Idx = WNetGetUser(Dummy, User, 12)
    If Idx = 0 And Trim$(User) > "" Then
      Idx = InStr(User, Chr(0))
      If Idx > 0 Then User = Left$(User, Idx - 1) 'ok
    Else 'nada...
      User = InputBox$("Nome do usuário:", "EE1 : GetNom")
    End If
  End If
  'tenta de novo
  If User = "" Then User = InputBox$("Nome do usuário:", "FdeRot9 : GetNom")
  'usuario atribuido
  If User = "" Then User = "SemNom"
  GetNom = UCase$(User)  'saindo
  Exit Function
  '
End Function

' Name: Function GetTmp
' Input:
' Output:
'   String - Retorna pasta de arquivos temporarios
' Purpose: Obtem pasta temporarios
' Remarks: None
'===============================================================================
Public Function GetTmp() As String
  Dim Tmp As String
  '
  Tmp = Environ$("temp") 'obtem variavel ambiente
  If (Tmp = "") Then
    Tmp = Environ$("tmp")
    If (Tmp = "") Then Tmp = Environ$("windir")
    If (Tmp = "") Then Tmp = "C:" 'ultima chance
  End If
  GetTmp = VrfPth(Tmp) 'verifica \
  'saindo
End Function

'===============================================================================
' Name: Function VrfPth
' Input:
'   ByVal NomPth As String - Caminho a ser verificado
' Output:
'   String - Retorna o caminho com a contrabarra final
' Purpose: Verifica se caminho ja tem contrabarra no fim
' Remarks: None
'===============================================================================
Public Function VrfPth(ByVal NomPth As String) As String
  '
  Dim Idx As Integer
  '
  Idx = InStr(3, NomPth & "\", "\\")
  If (Idx > 0) Then VrfPth = Left$(NomPth, Idx)
  If Right$(VrfPth, 1) <> "\" Then VrfPth = NomPth & "\"
  '
End Function

Public Function Usuario() As String
    Usuario = GetNom
End Function

Public Function RecGrd(ByVal iD As Long, ByVal Grd As MSFlexGrid, ByVal NmCol As String, ByRef Linhas As Integer, Optional ErrMsg As String) As Boolean
  'recupera grids
  'tratamento de erro?????
  '
  Dim Idx As Integer, Idy As Integer, nRw As Integer
  Dim Sql As String, Tmp As String
  Dim Vl1() As String, Nmc() As String
  '
  'nmcol="x1,x2,x3,x4,x5"
  'nome das caracteristicas
  Nmc = Split(NmCol, ",") 'obtem vetor com nome das colunas
  '
  Linhas = 0
  Grd.Rows = 2
  For Idy = 0 To Grd.Cols - 1
    Grd.TextMatrix(1, Idy) = ""
  Next
  For Idy = 0 To UBound(Nmc) 'numero fixo de colunas??? (alterar por grid)
    Sql = "Select VL_CARACT,VL_LINHA From " & ParDB.Pr & "d_carbiege Where ID_D_IDNBIEGE=" & CStr(iD) & " And DS_CARACT='" & Nmc(Idy) & "' Order by VL_LINHA"
    If (AdoCon.AdoSQ(Tmp, IDConE, Sql, True, 999, ErrMsg) = 0) Then
      Vl1 = Split(Tmp, Chr(0)) 'separa linhas
      If (nRw = 0) Then 'seta linhas
        nRw = UBound(Vl1)
        If (nRw >= 0) Then Grd.Rows = nRw + 2
      End If
      If (UBound(Vl1) <> nRw) Then
        'loop pelas linhas da coluna
        For Idx = 0 To UBound(Vl1)
          If (Idy >= 2) And (Idy <= 6) Then
            tem_costelado = True
            linha_costelado = PegaItem(Vl1(Idx), Chr(1), 2)
            Grd.TextMatrix(linha_costelado, Idy) = PegaItem(Vl1(Idx), Chr(1), 1)
          ElseIf (Idy = 7) Then 'eixo oco
            tem_oco = True
            linha_oco = PegaItem(Vl1(Idx), Chr(1), 2)
            Grd.TextMatrix(linha_oco, Idy) = PegaItem(Vl1(Idx), Chr(1), 1)
          ElseIf (Idy = 8) Then 'eixo cônico
            tem_con = True
            linha_con = PegaItem(Vl1(Idx), Chr(1), 2)
            Grd.TextMatrix(linha_con, Idy) = PegaItem(Vl1(Idx), Chr(1), 1)
          End If
        Next
      Else
        'loop pelas linhas da coluna
        For Idx = 1 To nRw + 1
          Grd.TextMatrix(Idx, Idy) = PegaItem(Vl1(Idx - 1), Chr(1), 1)
          Linhas = Idx
        Next
      End If
    Else
      ErrMsg = "EE4 : RecGrd-> Erro executando SQL" & IIf(ErrMsg > "", vbLf & ErrMsg, "")
      Exit Function
    End If
  Next
  '
  RecGrd = True
  '
End Function

Public Function Delete(ByVal iD As Long, Optional ByVal Ref As String, Optional All As Boolean, Optional Trn As Boolean, Optional ErrMsg As String) As Boolean
  'deleta
  Dim Sql As String, r As String
  Dim OK As Boolean
  '
  'inicia transacao
  If (Trn) Then 'delecao avulsa
    OK = (AdoCon.AdoBT(IDConE, ErrMsg) = 0)
    If (Not OK) Then MsgBox ErrMsg: Exit Function
  End If
  '
  If (Ref <> "") Then
    Sql = "select id from " & ParDB.Pr & "d_idnbiege where nr_ref='" & Ref & "'"
    OK = (AdoCon.AdoSQ(r, IDConE, Sql, , , ErrMsg) = 0)
    If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrDel
    iD = CLng(r)
  End If
  '
  Sql = "Delete From " & ParDB.Pr & "d_carbiege Where ID_D_IDNBIEGE=" & CStr(iD)
  OK = (AdoCon.AdoEX(IDConE, Sql, ErrMsg) = 0)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrDel
  '
  If (All) Then
    Sql = "Delete From " & ParDB.Pr & "d_idnbiege Where ID=" & CStr(iD)
    OK = (AdoCon.AdoEX(IDConE, Sql, ErrMsg) = 0)
    If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrDel
  End If
  '
  If (Trn) Then
    'comita tudo
    OK = (AdoCon.AdoCT(IDConE, ErrMsg) = 0)
    If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrDel
  End If
  '
  Delete = True
  '
  Exit Function
  '
TrtErrDel:
  '
  If (Trn) Then Call AdoCon.AdoRT(IDConE, ErrMsg)  'rolbeca transacao
  ErrMsg = "EE4 : Delete-> " & IIf(ErrMsg > "", vbLf & ErrMsg, "")
  '
End Function

Public Function SlvGrd(ByVal iD As Long, ByVal Grd As MSFlexGrid, ByVal NmCol As String, Optional ErrMsg As String) As Boolean
  'salva grid
  'tratamento de erro?????
  '
  Dim Idx As Integer, Idy As Integer
  Dim Sq1 As String, Sql As String, Nmc() As String
  Dim OK As Boolean
  '
  'nmcol="x1,x2,x3,x4,x5"
  'nome das caracteristicas
  Nmc = Split(NmCol, ",") 'obtem vetor com nome das colunas
  '
  Sq1 = "Insert Into " & ParDB.Pr & "d_carbiege (ID_D_IDNBIEGE,VL_LINHA,DS_CARACT,VL_CARACT) Values (" & CStr(iD) & ","
  '
  For Idy = 0 To UBound(Nmc) 'numero fixo de colunas??? (alterar por grid)
    For Idx = 1 To Grd.Rows - 1
      If (Grd.TextMatrix(Idx, Idy) <> "") Then
        Sql = Sq1 & CStr(Idx) & ",'" & Nmc(Idy) & "'," & Grd.TextMatrix(Idx, Idy) & ")"
        OK = (AdoCon.AdoEX(IDConE, Sql, ErrMsg) = 0)
        If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrSlv
      End If
    Next
  Next
  '
  SlvGrd = True
  '
  Exit Function
 '
TrtErrSlv:
  '
  ErrMsg = "EE4 : SlvGrd-> " & IIf(ErrMsg > "", vbLf & ErrMsg, "")
  '
End Function

Public Function GetId(ByVal NomTbS As String, Optional ByVal ColId As String = "Id", Optional ErrMsg As String) As Long
  'obtem novo id
  Dim Sql As String, Tmp As String
  Dim i As Integer
  '
  GetId = -1
  If Not ParDB.Or Then 'nao oracle, max da tabela
    i = InStr(NomTbS, "_seq")
    If (i > 0) Then NomTbS = Left(NomTbS, i - 1)
    Sql = "Select (Max(" & ColId & ")+1) as nId From " & ParDB.Pr & NomTbS
  Else 'sequencia oracle
    Sql = "Select " & ParDB.Pr & NomTbS & ".nextval From Dual"
  End If
  If (AdoCon.AdoSQ(Tmp, IDConE, Sql, , , ErrMsg) = 0) Then 'sql ok
    GetId = Val(Tmp) 'retorna Id
    If (GetId = 0) Then GetId = 1
  Else
    ErrMsg = "EE3 : GetId-> Erro obtendo sequencia" & IIf(ErrMsg > "", vbLf & ErrMsg, "")
  End If
  'saindo
End Function

Public Function DoConn(Optional Nom As String, Optional ErrMsg As String) As Boolean
  'faz a conexao com o banco de dados EE
  'Nom: retorna nome indicativo da conexao
  Dim ConStr As String
  Dim Idx As Integer
  '
  On Local Error GoTo ErDoConn
  ParDB.Pr = GetStD("Odbc", "Admin") 'schema
  ParDB.Or = IIf(Val(GetStD("Odbc", "NaoOra")) <> 0, False, True)  'nao oracle
  ParDB.C1 = GetStD("Ado", "Sub1Str")  'parametros de subs sql like 1 posicao
  ParDB.Ct = GetStD("Ado", "SubStr")  'parametros de subs sql like toda string
  If (ParDB.C1 = "") Then ParDB.C1 = IIf(ParDB.Or, "_", "?") 'oracle/access
  If (ParDB.Ct = "") Then ParDB.Ct = IIf(ParDB.Or, "%", "*")
  '
  ConStr = "Provider=" & GetStD("Ado", "Provider") & ";Dsn=" & GetStD("Odbc", "dsn")
  ConStr = ConStr & ";uid=" & GetStD("Odbc", "Username") & ";Pwd=" & CryptStringS(GetStD("Odbc", "Password"), CKEY)
  Idx = AdoCon.AdoOP(-1, Nom, ConStr, , , ErrMsg)
  If (Idx = -1) Then
    Nom = ""
    ErrMsg = "EE3 : DoConn-> " & ErrMsg
    Exit Function
  End If
  Nom = IIf(Val(GetStD("Ado", "Nativo")) <> 0, "OleDB", "ODBC") & ":" & Nom
  IDConE = Idx
  DoConn = True
  '
  Exit Function
ErDoConn:
  ErrMsg = Err.Number & " : " & Err.Description
  DoConn = False
End Function

'===============================================================================
' Name: Function CryptStringS
' Input:
'   ByVal strIn As String - String a ser encriptada
'   ByVal crptSECRET_KEY As String - Chave a ser utilizada na encriptacao
'   ByRef Optional CryptString As String - Retorna opcionalmente o texto encriptado
' Output:
'   String - Retorna texto de/encriptado
' Purpose: Simples encriptacao e decriptacao de textos
' Remarks: Utilizar sempre a mesma palavra chave
'===============================================================================
Public Function CryptStringS(ByVal StrIN As String, ByVal crptSECRET_KEY As String, Optional CryptString As String) As String
  '
  Dim strC As String, strO As String, crptCHAR_SET As String
  Dim intX As Integer, lenCHAR_SET As Integer, _
      lenSTRINPUT As Integer, lenSECRET_KEY As Integer, _
      intA  As Integer, intB As Integer, intD As Integer, _
      intE As Integer, intXOR As Integer
  'dicionario
  crptCHAR_SET = "qw+e:rtI<UY@3T,R6]78yu iVC>Xgh[jE?W24;5klz/cDSA)PO90!#vbnmMN-BxZLKop.dfJH|GF$%Q1&(as_"
  lenSTRINPUT = Len(Trim$(StrIN))
  lenCHAR_SET = Len(Trim$(crptCHAR_SET))
  lenSECRET_KEY = Len(Trim$(crptSECRET_KEY))
  If lenSTRINPUT = 0 Then Exit Function
  '
  intX = 0
  Do
    intX = intX + 1
    strO = Mid$(StrIN, intX, 1)
    intA = InStr(crptCHAR_SET, strO)
    If (intA > 0) Then
      intB = (intX + lenSTRINPUT) Mod (lenSECRET_KEY)
      strC = Mid$(crptSECRET_KEY, intB + 1, 1)
      intD = InStr(crptCHAR_SET, strC)
      intE = (intA + intD) Mod (lenCHAR_SET)
      intXOR = (lenCHAR_SET - intE) Mod (lenCHAR_SET)
      strO = Mid$(crptCHAR_SET, intXOR + 1, 1)
    End If
    CryptString = CryptString & strO
  Loop Until (intX >= lenSTRINPUT)
  CryptStringS = CryptString
  'saindo
End Function

'===============================================================================
' Name: Function GetIni
' Input:
'   ByRef Optional ErrMsg As String - Retorna texto da mensagem de erro
' Output:
'   Boolean - Retorna True para execucao sem erro
' Purpose: Obtem parametros do arquivo ini para uma estrutura em forma de vetor
' Remarks: None
'===============================================================================
Public Function GetIni(Optional ErrMsg As String) As Boolean
  '
  On Local Error GoTo TrtErrEE1GetIni
  Dim Sec As String, Key As String
  Dim Vls() As String, Vlr() As String
  Dim Idx As Integer, Idy As Integer, _
      Cns As Integer, Cnr As Integer
  Dim OK As Boolean
  '
  Vls = Split(StrIni(AppCfg.NomIni, Sec, Key), Chr(0))
  Cns = UBound(Vls)
  If Not (Cns < 0) Then
    ReDim ParIni(Cns)
    For Idx = 0 To Cns
      ParIni(Idx).Sec = Vls(Idx)
      Vlr = Split(StrIni(AppCfg.NomIni, Vls(Idx), Key), Chr(0))
      Cnr = UBound(Vlr)
      If (Not (Cnr < 0)) Then
        ReDim Preserve ParIni(Idx).Vlr(Cnr)
        ParIni(Idx).Chv = Chr(0)
        For Idy = 0 To Cnr
          ParIni(Idx).Vlr(Idy) = StrIni(AppCfg.NomIni, Vls(Idx), Vlr(Idy))
          ParIni(Idx).Chv = ParIni(Idx).Chv & Vlr(Idy) & Chr(1) & Format$(Idy, "00") & Chr(0)
        Next
        OK = True
      Else
        ReDim Preserve ParIni(Idx).Vlr(0)
      End If
    Next
  Else
    ReDim ParIni(0)
  End If
  GetIni = OK
  Exit Function
  'saindo
TrtErrEE1GetIni:
  ErrMsg = "EE1 : GetIni-> " & "Obtendo parametros" & vbLf & _
            LCase$(AppCfg.NomIni) & vbLf & Error$
End Function

'===============================================================================
' Name: Function IniSvr
' Input:
'   ByRef Optional ErrMsg As String - Retorna texto da mensagem de erro
' Output:
'   Boolean - Retorna True para execucao sem erro
' Purpose: 'inicializa o servidor de conexao ao banco de dados
' Remarks: None
'===============================================================================
Public Function IniSvr(Optional ByRef ErrMsg As String) As Boolean
  '
  On Local Error GoTo TrtErrEE1IniSvr
  Dim nOk As Boolean
  Dim NomImp As String
  '
IniSvrDoAgain:
  Call SetaObjConexao
  IniSvr = True
  Exit Function
  '
TrtErrEE1IniSvr:
  'tenta executar o software de impressao activex
  'If ((Err = 429) And (Not nOk)) Then 'error$=ActiveX component can't create object
    'NomImp = GetStD("conexao", "servidor")
    'If (Dir$(NomImp) > "" And NomImp > "") Then
    '  Shell NomImp, vbMinimizedNoFocus: DoEvents
    '  nOk = True
    '  GoTo IniSvrDoAgain
    'Else
    '  ErrMsg = "EE1 : IniSvr-> " & "Arquivo executavel do servidor de conexao nao encontrado!"
     ' Exit Function
    'End If
 ' End If
  'outro erro
  'ErrMsg = Err.Number & " : " & Err.Description
  'bye
End Function

Public Sub SetaObjConexao()
  Dim Registrado As Boolean, ArqRegistro As String
  On Local Error GoTo ErrSetaObjConexao:
IniObjConexao:
  Set AdoCon = CreateObject("AdoCN.AdoCon")
  Exit Sub
ErrSetaObjConexao:
  If (Err.Number = 429) Then
    If Not Registrado Then
      ArqRegistro = RegAdocn
      If Dir$(ArqRegistro) = "" Then
        MsgBox "Arquivo de registro do componente Adocn <" & ArqRegistro & "> não encontrado!"
      End If
      Call Shell(ArqRegistro, vbHide)
      Registrado = True
      GoTo IniObjConexao
    Else
      MsgBox "Erro: Não foi possivel registrar Objeto ADOcn", vbCritical, "Erro"
      End
    End If
  End If
End Sub

'===============================================================================
' Name: Function GetStD
' Input:
'   ByVal Sec As String - Nome da secao
'   ByVal Optional Key As String - Nome da chave
'   ByVal Optional  Vlr As String = "" - Se utilizado, retorna a chave a qual pertence o valor
' Output:
'   String - Retorna o valor ou a chave desejada
' Purpose: 'obtem valor de estruturas dinamicas que mantem opcoes do INI
' Remarks: None
'===============================================================================
Public Function GetStD(ByVal Sec As String, Optional ByVal Key As String, Optional ByVal Vlr As String = "") As String
  '
  Dim Idx As Integer, Idy As Integer
  Dim Tmp() As String
  '
  For Idy = 0 To UBound(ParIni)
    If (UCase$(ParIni(Idy).Sec) = UCase$(Sec)) Then
      If (Vlr = "") Then 'modo retorta valor
        Idx = InStr(UCase$(ParIni(Idy).Chv), (Chr(0) & UCase$(Key) & Chr(1)))
        If Idx > 0 Then GetStD = ParIni(Idy).Vlr(Val(Mid$(ParIni(Idy).Chv, Idx + Len(Key) + 2, 2)))
        Exit For
      Else 'modo retorna chave
        Tmp = Split(ParIni(Idy).Chv, Chr(0))
        For Idx = 0 To UBound(ParIni(Idy).Vlr)
          If Vlr = ParIni(Idy).Vlr(Idx) Then If UBound(Tmp) >= Idx Then Tmp = Split(Tmp(Idx + 1), Chr(1)): GetStD = Tmp(0): Exit For
        Next
        Exit For
      End If
    End If
  Next
  'saindo
End Function

'===============================================================================
' Name: Function GetDat
' Input:
'   Optional ByVal toDb As Boolean = False - Opcao para retono de data formatada para o BD
' Output:
'   String - Retorna data atual formatada
' Purpose: Obtem data atual formatada no estilo dd/mm/yyyy hh:mm:ss
' Remarks: None
'===============================================================================
Public Function GetDat(Optional ByVal toDb As Boolean = False) As String
  '
  GetDat = Format$(Now, "dd/mm/yyyy hh:nn:ss")
  If (toDb) Then If ParDB.Or Then GetDat = "to_date('" & GetDat & "','dd/mm/yyyy hh24:mi:ss')" Else GetDat = "'" & GetDat & "'"
  'saindo
End Function

Public Sub desenha_eixo(ByVal comprimento As Single, ByVal diametro_externo As Single, ByVal afastamento As Single, ByVal X0 As Single, ByVal y0 As Single, ByVal fe As Single, ByVal flag As Byte)
Dim n_saltos As Double, yant As Double, fcor As Double, fcor_aux As Double
Dim ya1 As Double, ya2 As Double, ya3 As Double, ya4 As Double, Y As Double

  If (flag = 2) Then
    frmMain.Picture1.Line (X0 + (afastamento) * fe, y0 - diametro_externo / 2 * fe)-(X0 + (afastamento + comprimento) * fe, y0 + diametro_externo / 2 * fe), QBColor(7), BF
    frmMain.Picture1.Line (X0 + (afastamento) * fe, y0 - diametro_externo / 2 * fe)-(X0 + (afastamento + comprimento) * fe, y0 + diametro_externo / 2 * fe), QBColor(0), B
  Else
    n_saltos = 20
    If (diametro_externo = 0) Then Exit Sub
    yant = 0
    For Y = 0 To Val(diametro_externo) * 1.000001 Step Val(diametro_externo) / n_saltos
      fcor = 255 - 180 * Y / Val(diametro_externo)
      ya1 = y0 - (Y / 2) * fe
      ya2 = y0 - (yant / 2) * fe
      ya3 = y0 + (Y / 2) * fe
      ya4 = y0 + (yant / 2) * fe
  
      If (flag = 0) Then
        fcor_aux = fcor
      Else
        fcor_aux = 0
      End If
      frmMain.Picture1.Line (X0 + (afastamento) * fe, ya1)-(X0 + (afastamento + comprimento) * fe, ya2), RGB(fcor_aux, fcor, fcor_aux), BF
      frmMain.Picture1.Line (X0 + (afastamento) * fe, ya3)-(X0 + (afastamento + comprimento) * fe, ya4), RGB(fcor_aux, fcor, fcor_aux), BF
      yant = Y
    Next Y
    If (flag = 1) Then
      frmMain.Picture1.DrawWidth = 2
      frmMain.Picture1.Line (X0 + (afastamento) * fe, y0 - diametro_externo / 2 * fe)-(X0 + (afastamento + comprimento) * fe, y0 + diametro_externo / 2 * fe), QBColor(0), B
      frmMain.Picture1.DrawWidth = 1
    End If
  End If
End Sub

Public Sub desenha_eixo_conico(ByVal comprimento As Single, ByVal diametro_inicial As Single, ByVal diametro_final As Single, ByVal afastamento As Single, ByVal xIni As Single, ByVal yIni As Single, ByVal fe As Single, ByVal flag As Integer)
  Dim n_saltos As Double, yant As Double, fcor As Double, fcor_aux As Double
  Dim ya1 As Double, ya2 As Double, ya3 As Double, ya4 As Double, Y As Double
  
  Dim y0 As Double
  Dim X0 As Double
  Dim passo As Double
  Dim i As Double
  Dim diam_aux As Double

  y0 = yIni
  'y01 = yIni
  X0 = xIni
  passo = comprimento / 100
  diam_aux = ((diametro_final - diametro_inicial) / 100)
  If (diam_aux = 0) Then diam_aux = 1
  n_saltos = 20
  If (diametro_inicial = 0) Then Exit Sub
  For i = diametro_inicial To (diametro_final - (diam_aux)) Step diam_aux
    'yant = 0
    For Y = 0 To Val(i) * 1.000001 Step Val(i) / n_saltos
      fcor = 255 - 180 * Y / Val(i)
      ya1 = y0 - (Y / 2) * fe
      ya2 = y0 - (yant / 2) * fe
      ya3 = y0 + (Y / 2) * fe
      ya4 = y0 + (yant / 2) * fe
      
      If (flag = 0) Then
        fcor_aux = fcor
      Else
        fcor_aux = 0
      End If
  
      frmMain.Picture1.Line (X0 + (afastamento) * fe, ya1)-(X0 + (afastamento + passo) * fe, ya2), RGB(fcor_aux, fcor, fcor_aux), BF
      frmMain.Picture1.Line (X0 + (afastamento) * fe, ya4)-(X0 + (afastamento + passo) * fe, ya3), RGB(fcor_aux, fcor, fcor_aux), BF
      yant = Y
    Next Y
    'y0 = y0 - diam_aux
    'y01 = y01 + i
    X0 = X0 + (passo * fe)
  Next i
  If (flag = 1) Then
    frmMain.Picture1.DrawWidth = 2
  End If
  frmMain.Picture1.Line (xIni + (afastamento) * fe, y0 + (diametro_inicial / 2) * fe)-(X0 + (afastamento) * fe, y0 + (diametro_final / 2) * fe), QBColor(0)
  frmMain.Picture1.Line (xIni + (afastamento) * fe, y0 + (diametro_inicial / 2) * fe)-(xIni + (afastamento) * fe, y0 - (diametro_inicial / 2) * fe), QBColor(0)
  frmMain.Picture1.Line (xIni + (afastamento) * fe, y0 - (diametro_inicial / 2) * fe)-(X0 + (afastamento) * fe, y0 - (diametro_final / 2) * fe), QBColor(0)
  frmMain.Picture1.Line (X0 + (afastamento) * fe, y0 + (diametro_final / 2) * fe)-(X0 + (afastamento) * fe, y0 - (diametro_final / 2) * fe), QBColor(0)
  frmMain.Picture1.DrawWidth = 1
End Sub

Public Sub desenha_eixo_OLD(comprimento, diametro_externo, diametro_interno, afastamento, X0, y0, fe, flag)
Dim n_saltos As Double, yant As Double, fcor As Double
Dim ya1 As Double, ya2 As Double, ya3 As Double, ya4 As Double, Y As Double
If (flag = 0) Then
  n_saltos = 20
  If (diametro_externo = 0) Then Exit Sub
  yant = 0
  For Y = 0 To Val(diametro_externo) * 1.000001 Step Val(diametro_externo) / n_saltos
    fcor = 255 - 180 * Y / Val(diametro_externo)
    ya1 = y0 - (Y / 2) * fe
    ya2 = y0 - (yant / 2) * fe
    ya3 = y0 + (Y / 2) * fe
    ya4 = y0 + (yant / 2) * fe

    frmMain.Picture1.Line (X0 + (afastamento) * fe, ya1)-(X0 + (afastamento + comprimento) * fe, ya2), RGB(fcor, fcor, fcor), BF
    frmMain.Picture1.Line (X0 + (afastamento) * fe, ya3)-(X0 + (afastamento + comprimento) * fe, ya4), RGB(fcor, fcor, fcor), BF
    yant = Y
  Next Y
End If

If (flag = 1) Then
  n_saltos = 20
  If (diametro_externo = 0) Then Exit Sub
  yant = 0
  For Y = 0 To Val(diametro_externo) * 1.000001 Step Val(diametro_externo) / n_saltos
      fcor = 255 - 180 * Y / Val(diametro_externo)
      ya1 = y0 - (Y / 2) * fe
      ya2 = y0 - (yant / 2) * fe
      ya3 = y0 + (Y / 2) * fe
      ya4 = y0 + (yant / 2) * fe
    
      frmMain.Picture1.Line (X0 + (afastamento) * fe, ya1)-(X0 + (afastamento + comprimento) * fe, ya2), RGB(0, fcor, 0), BF
      frmMain.Picture1.Line (X0 + (afastamento) * fe, ya3)-(X0 + (afastamento + comprimento) * fe, ya4), RGB(0, fcor, 0), BF
      yant = Y
  Next Y
  frmMain.Picture1.DrawWidth = 2
  frmMain.Picture1.Line (X0 + (afastamento) * fe, y0 - diametro_externo / 2 * fe)-(X0 + (afastamento + comprimento) * fe, y0 + diametro_externo / 2 * fe), QBColor(0), B
  frmMain.Picture1.DrawWidth = 1
End If

If (flag = 2) Then
  frmMain.Picture1.Line (X0 + (afastamento) * fe, y0 - diametro_externo / 2 * fe)-(X0 + (afastamento + comprimento) * fe, y0 + diametro_externo / 2 * fe), QBColor(7), BF
  frmMain.Picture1.Line (X0 + (afastamento) * fe, y0 - diametro_externo / 2 * fe)-(X0 + (afastamento + comprimento) * fe, y0 + diametro_externo / 2 * fe), QBColor(0), B
End If
End Sub

Public Function PadStr(ByVal Vlr As String, ByVal Tam As Integer, Optional ByVal Dir As Boolean) As String
  'preenche com brancos
  If Tam - Len(Vlr) <= 0 Then
    PadStr = Left(Vlr, Tam)
  Else
    PadStr = IIf(Dir, String(Tam - Len(Vlr), " ") & Vlr, Vlr & String(Tam - Len(Vlr), " "))
  End If
End Function

Public Function DelClc(ByVal NrMes As Integer) As Boolean
  'remove calculo
  On Local Error GoTo TrtErrForm1DelClc
  Dim Sql As String, SqD As String
  Dim ErrMsg As String
  Dim OK As Boolean
  '
  NrMes = NrMes * -1
  'monta sql delecao por data atual - n meses
  If (NrMes > 0) Then DelClc = True: Exit Function 'sai se usuario colocou periodo negativo no ini
  SqD = Format$(DateAdd("m", NrMes, Now()), "dd/mm/yyyy hh:nn:ss")
  
  OK = (AdoCon.AdoBT(IDConE, ErrMsg) = 0)
  If (Not OK) Then DelClc = False: Exit Function
  
  'delecao
  If ParDB.Or Then
    Sql = "Delete From d_carbiege Where id_d_idnbiege in (select id from d_idnbiege where dt_dat<to_date('" & SqD & "','DD/MM/YYYY hh24:mi:ss'))"
  Else
    Sql = "Delete From d_carbiege Where id_d_idnbiege in (select id from d_idnbiege where cdate(dt_dat)<'" & Format(SqD, "DD/MM/YYYY") & "')"
  End If
  OK = (AdoCon.AdoEX(IDConE, Sql, ErrMsg) = 0)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrForm1DelClc

  If ParDB.Or Then
    Sql = "Delete From d_idnbiege Where dt_dat<to_date('" & SqD & "','DD/MM/YYYY hh24:mi:ss')"
  Else
    Sql = "Delete From d_idnbiege Where cdate(dt_dat)<'" & Format(SqD, "DD/MM/YYYY") & "'"
  End If
  
  OK = (AdoCon.AdoEX(IDConE, Sql, ErrMsg) = 0)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrForm1DelClc
  
  OK = (AdoCon.AdoCT(IDConE, ErrMsg) = 0)
  If (Not OK) Then DelClc = False: Exit Function
  
  DelClc = True
  Exit Function
TrtErrForm1DelClc:
Resume Next
  'rolbeca se possivel
  OK = (AdoCon.AdoRT(IDConE, ErrMsg) = 0)
  DelClc = False
  '
End Function

'===============================================================================
' Name: EnSc
' Input:
'   ByRef Frm As Object   - formulário
'   ByVal flag As Boolean
' Output:
' Purpose: Habilitar/Desabilitar componentes de tela
' Remarks:
' Author: mpiazera
'===============================================================================
Public Sub EnSc(ByRef Frm As Object, ByVal flag As Boolean)
  Dim component As Object
  Dim i As Long
  For i = 0 To Frm.Count - 1
    Set component = Frm.Controls(i)
    If (TypeName(component) <> "Line") Then
      component.Enabled = flag
    End If
    Set component = Nothing
  Next
End Sub

