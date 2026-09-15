VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   3810
   ClientLeft      =   1605
   ClientTop       =   1545
   ClientWidth     =   6540
   LinkTopic       =   "Form1"
   ScaleHeight     =   3810
   ScaleWidth      =   6540
   Begin VB.CommandButton Command2 
      Caption         =   "Salva"
      Height          =   375
      Left            =   3960
      TabIndex        =   2
      Top             =   720
      Width           =   855
   End
   Begin VB.CommandButton Command1 
      Caption         =   "PreGrd"
      Height          =   375
      Left            =   3960
      TabIndex        =   1
      Top             =   240
      Width           =   855
   End
   Begin MSFlexGridLib.MSFlexGrid MSFlexGrid1 
      Height          =   2895
      Left            =   120
      TabIndex        =   0
      Top             =   240
      Width           =   3735
      _ExtentX        =   6588
      _ExtentY        =   5106
      _Version        =   393216
      Cols            =   5
      FixedCols       =   0
      FormatString    =   "^x1        |^x2        |^x3        |^x4        |^x5        "
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'
Option Explicit
'
'
Private Const CKEY As String = "WeGiNdUsTrIaSdIvMaQuInAs", SSG As String = "ungesichert"
Private Const NMI As String = "iBiege.ini" 'nome do arquivo ini
'
Private Type InI 'buffer ini
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
Private ParIni() As InI 'buffer arquivo ini
Private AppCfg As GENPAR 'parametros do aplicativo
Private ParUsu As USUPAR 'parametros usuario
Private ParDB  As ODBC 'parametros banco dados
Private IdConE As Integer 'numero da conexao para ee
'
Private ConSvr As Object 'servidor de conexao
'
Private Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Private Declare Function WNetGetUser Lib "mpr.dll" Alias "WNetGetUserA" (ByVal lpName As String, ByVal lpUserName As String, lpnLength As Long) As Long
Private Declare Function GetUserName Lib "advapi32.dll" Alias "GetUserNameA" (ByVal lpBuffer As String, nSize As Long) As Long
'
'
'
'
Private Sub pregrd()
  '
  Dim Idx As Integer, Idy As Integer
  '
  MSFlexGrid1.Rows = 10
  '
  For Idx = 1 To 9
    For Idy = 0 To 4
      MSFlexGrid1.TextMatrix(Idx, Idy) = CStr(Idx) & CStr(Idy)
    Next
  Next
  '
End Sub
'
Private Function Delete(ByVal Id As Long, Optional All As Boolean, Optional Trn As Boolean, Optional ErrMsg As String) As Boolean
  'deleta
  Dim Sql As String
  Dim OK As Boolean
  '
  'inicia transacao
  If (Trn) Then 'delecao avulsa
    OK = (ConSvr.AdoBT(IdConE, ErrMsg) <> 0)
    If (Not OK) Then MsgBox ErrMsg: Exit Function
  End If
  '
  Sql = "Delete From " & ParDB.Pr & "d_carbiege Where ID_D_IDNBIEGE=" & CStr(Id)
  OK = (ConSvr.AdoEx(IdConE, Sql, ErrMsg) = 0)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrDel
  '
  If (All) Then
    Sql = "Delete From " & ParDB.Pr & "d_idnbiege Where ID=" & CStr(Id)
    OK = (ConSvr.AdoEx(IdConE, Sql, ErrMsg) = 0)
    If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrDel
  End If
  '
  If (Trn) Then
    'comita tudo
    OK = (ConSvr.AdoCT(IdConE, ErrMsg) = 0)
    If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrDel
  End If
  '
  Delete = True
  '
  Exit Function
  '
TrtErrDel:
  '
  If (Trn) Then Call ConSvr.AdoRT(IdConE, ErrMsg)  'rolbeca transacao
  ErrMsg = "EE4 : Delete-> " & IIf(ErrMsg > "", vbLf & ErrMsg, "")
  '
End Function
'
Private Function RecGrd(ByVal Id As Long, ByVal Grd As MSFlexGrid, ByVal NmCol As String, Optional ErrMsg As String) As Boolean
  'recupera grids
  'tratamento de erro?????
  '
  Dim Idx As Integer, Idy As Integer, nRw As Integer
  Dim Sql As String, Tmp As String, Sq1 As String
  Dim Vl1() As String, Nmc() As String
  '
  'nmcol="x1,x2,x3,x4,x5"
  'nome das caracteristicas
  Nmc = Split(NmCol, ",") 'obtem vetor com nome das colunas
  '
  For Idy = 0 To UBound(Nmc) 'numero fixo de colunas??? (alterar por grid)
    Sql = "Select VL_CARACT From " & ParDB.Pr & "d_carbiege Where ID_D_IDNBIEGE=" & CStr(Id) & " And DS_CARACT='" & Nmc(Idy) & "' Order by VL_LINHA"
    If (ConSvr.AdoSQ(Tmp, IdConE, Sql, True, 999, ErrMsg) = 0) Then
      Vl1 = Split(Tmp, Chr(0)) 'separa linhas
      If (nRw = 0) Then 'seta linhas
        nRw = UBound(Vl1)
        Grd.Rows = nRw + 2
      Else 'checa se sempre tem o mesmo numero de linhas
        If (UBound(Vl1) <> nRw) Then
          ErrMsg = "EE4 : RecGrd-> Número de linhas inconsistente!" & IIf(ErrMsg > "", vbLf & ErrMsg, "")
          Exit Function
        End If
      End If
      'loop pelas linhas da coluna
      For Idx = 1 To nRw + 1
        Grd.TextMatrix(Idx, Idy) = Vl1(Idx - 1)
      Next
    Else
      ErrMsg = "EE4 : RecGrd-> Erro executando SQL" & IIf(ErrMsg > "", vbLf & ErrMsg, "")
      Exit Function
    End If
  Next
  '
  RecGrd = True
  '
End Function
'
Private Function SlvGrd(ByVal Id As Long, ByVal Grd As MSFlexGrid, ByVal NmCol As String, Optional ErrMsg As String) As Boolean
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
  Sq1 = "Insert Into " & ParDB.Pr & "d_carbiege (ID_D_IDNBIEGE,VL_LINHA,DS_CARACT,VL_CARACT) Values (" & CStr(Id) & ","
  '
  For Idy = 0 To UBound(Nmc) 'numero fixo de colunas??? (alterar por grid)
    For Idx = 1 To Grd.Rows - 1
      Sql = Sq1 & CStr(Idx) & ",'" & Nmc(Idy) & "'," & Grd.TextMatrix(Idx, Idy) & ")"
      OK = (ConSvr.AdoEx(IdConE, Sql, ErrMsg) = 0)
      If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrSlv
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
'
Private Function Salva(ByVal NrRef As String, ByVal DsClc As String, Optional Id As Long, Optional ErrMsg As String) As Boolean
  'salva tudo
  'tratamento de erro?????
  '
  Dim Id As Long
  Dim Sql As String, Nmc As String
  Dim OK As Boolean
  '
  'inicia transacao
  OK = (ConSvr.AdoBT(IdConE, ErrMsg) = 0)
  If (Not OK) Then Exit Function
  '
  'verifica update
  Sql = "Select id From " & ParDB.Pr & "d_idnbiege Where NR_REF=" & NrRef
  If (ConSvr.AdoSQ(Tmp, IdConE, Sql, , , ErrMsg) = 0) Then
    Id = Val(Tmp)
    If (Id > 0) Then
      If (Not Delete(Id, True, False, ErrMsg)) Then GoTo TrtErrIns
    Else
      'obtem id novo da sequencia
      Id = GetId("d_idnbiege_seq", , ErrMsg)
      If (Id < 0) Then MsgBox ErrMsg: Exit Function
    End If
  Else
    ErrMsg = "EE4 : Salva-> Erro executando SQL" & IIf(ErrMsg > "", vbLf & ErrMsg, "")
    Exit Function
  End If
  '
  'prepara insert
  Sql = "Insert Into " & ParDB.Pr & "d_idnbiege (ID,NR_REF,DS_CALC,NM_USU,DT_DAT) Values ("
  Sql = Sql & CStr(Id) & "," & NrRef & ",'" & DsClc & "','" & UCase$(ParUsu.Nom) & "'," & GetDat(True) & ")"
  '
  'executa insert na identificacao
  OK = (ConSvr.AdoEx(IdConE, Sql, ErrMsg) = 0)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  'salva grid
  OK = SlvGrd(Id, MSFlexGrid1, "x1,x2,x3,x4,x5", ErrMsg)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  '...
  '...
  '
  'comita tudo
  OK = (ConSvr.AdoCT(IdConE, ErrMsg) = 0)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  Salva = True
  '
  Exit Function
  '
TrtErrIns:
  '
  Call ConSvr.AdoRT(IdConE, ErrMsg) 'rolbeca transacao
  ErrMsg = "EE4 : RemTmp-> " & IIf(ErrMsg > "", vbLf & ErrMsg, "")
  '
End Function
'
Private Sub Command1_Click()
  '
  pregrd
  '
End Sub

Private Sub Command2_Click()
  '
  Salva
  '
End Sub
'
Private Sub Form_Load()
  '
  Dim ErrMsg As String
  '
  AppCfg.LocPth = App.Path 'caminho atual
  AppCfg.NomIni = AppCfg.LocPth & "\" & NMI 'nome do ini
  ParUsu.Nom = GetNom() 'nome do usuario
  AppCfg.TmpPth = GetTmp() 'pasta temporarios
  If Not GetIni(ErrMsg) Then
    MsgBox ErrMsg
  End If
  If Not IniSvr(ErrMsg) Then
    MsgBox ErrMsg
  End If
  If Not DoConn(, ErrMsg) Then   'conexao
     MsgBox ErrMsg
  End If
  '
End Sub
'
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
'
'===============================================================================
' Name: Function GetId
' Input:
'   ByVal NomTbS As String - Nome tabela ou sequencia
'   ByRef Optional ColId As String = "Id" - nome da coluna indice
'   ByRef Optional ErrMsg As String - Retorna texto da mensagem de erro
' Output:
'   Long - retorna o valor do identificador
' Purpose: obtem novo identificador de uma sequencia ou coluna de chave promaria (id)
' Remarks: Retorna -1 em caso de errro
'===============================================================================
Public Function GetId(ByVal NomTbS As String, Optional ByVal ColId As String = "Id", Optional ErrMsg As String) As Long
  'obtem novo id
  Dim Sql As String, Tmp As String
  '
  GetId = -1
  If Not ParDB.Or Then 'nao oracle, max da tabela
    Sql = "Select (Max(" & ColId & ")+1) nId From " & ParDB.Pr & NomTbS
  Else 'sequencia oracle
    Sql = "Select " & ParDB.Pr & NomTbS & ".nextval From Dual"
  End If
  If ConSvr.AdoSQ(Tmp, IdConE, Sql, , , ErrMsg) = 0 Then 'sql ok
    GetId = Val(Tmp) 'retorna Id
  Else
    ErrMsg = "EE3 : GetId-> Erro obtendo sequencia" & IIf(ErrMsg > "", vbLf & ErrMsg, "")
  End If
  'saindo
End Function
'
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
'
'===============================================================================
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
'
'===============================================================================
' Name: PrmMai
' Input:
'   ByVal Txt As String - Texto a ser processado
'   ByVal Optional All As Boolean = False - Processa todo texto
' Output:
'   String - Retora o texto com a(s) primeira(s) letra(s) maiuscula(s)
' Purpose: Deixa a(s) primeira(s) letra(s) maiuscula(s) as demais minusculas
' Remarks: None
'===============================================================================
Public Function PrmMai(ByVal Txt As String, Optional ByVal All As Boolean = False) As String
  '
  Dim Idx As Integer
  Dim Vlr() As String
  'separa texto
  If (All) Then Vlr = Split(Txt, " ") Else ReDim Vlr(0): Vlr(0) = Txt
  For Idx = 0 To UBound(Vlr)
    'checa o tamanho, so faz para palavras maiores que 2 letras
    If ((Len(Vlr(Idx)) > 2) Or (Idx = 0)) Then Vlr(Idx) = UCase$(Left$(Vlr(Idx), 1)) & LCase$(Mid$(Vlr(Idx), 2)) Else Vlr(Idx) = LCase$(Vlr(Idx))
  Next
  PrmMai = Join(Vlr, " ") 'junta outra vez para o retorno
  'saindo
End Function

'
'===============================================================================
' Name: Function GetNom
' Input:
'   ByRef Optional ErrMsg As String - Retorna texto da mensagem de erro
' Output:
'   String - Retorna o nome do usuario
' Purpose: Obtem nome do usuario do sistema operacional
' Remarks: None
'===============================================================================
Private Function GetNom(Optional ErrMsg As String) As String
  '
  Dim User As String, Dummy As String
  Dim Idx As Long
  '
  User = Space$(12)
  Idx = GetUserName(User, 12) 'tenta advpai
  If ((Idx <> 0) And (Trim$(User) > "")) Then
    Idx = InStr(User, Chr(0))
    If (Idx > 0) Then User = Left$(User, Idx - 1) 'ok
  Else 'agora tenta mpr
    Idx = WNetGetUser(Dummy, User, 12)
    If Idx = 0 And Trim$(User) > "" Then
      Idx = InStr(User, Chr(0))
      If (Idx > 0) Then User = Left$(User, Idx - 1) 'ok
    Else 'nada...
      User = InputBox$("SemNome", "EE1 : GetNom")
    End If
  End If
  'tenta de novo
  If (User = "") Then User = InputBox$("SemNOme", "FdeRot9 : GetNom")
  'usuario atribuido
  If (User = "") Then User = "SemNom"
  GetNom = PrmMai(User)  'saindo
  Exit Function
  '
End Function
'
'===============================================================================
' Name: Function StrIni
' Input:
'   ByVal Arq As String - Nome completo do arquivo INI
'   ByVal Sec As String - Nome da secao entre []
'   ByVal Key As String - Nome da chave
'   Optional ByRef Vlr As String - Retorna o valor da chave
' Output:
'   String - Retora valor da chave
' Purpose: Le valores de arquivos no padrao INI do windows
' Remarks: None
'===============================================================================
Private Function StrIni(ByVal Arq As String, ByVal Sec As String, ByVal Key As String, Optional Vlr As String) As String
  '
  Dim Ret As Long
  '
  Vlr = Space$(512) 'dimensiona maxima string que pode ser lida
  Ret = GetPrivateProfileString(Sec, Key, "", Vlr, Len(Vlr), Arq)
  Ret = Ret - IIf(Sec = "" Or Key = "", 1, 0) 'checa tamanho do texto retornado
  If (Ret > 0) Then Vlr = Trim$(Left$(Vlr, Ret)): StrIni = Vlr Else Vlr = ""
  'saindo
End Function
'
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
  If Not Cns < 0 Then
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
'
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
'
'===============================================================================
' Name: Function IniSvr
' Input:
'   ByRef Optional ErrMsg As String - Retorna texto da mensagem de erro
' Output:
'   Boolean - Retorna True para execucao sem erro
' Purpose: 'inicializa o servidor de conexao ao banco de dados
' Remarks: None
'===============================================================================
Public Function IniSvr(Optional ByVal ErrMsg As String) As Boolean
  '
  On Local Error GoTo TrtErrEE1IniSvr
  Dim nOk As Boolean
  Dim NomImp As String
  '
IniSvrDoAgain:
  Set ConSvr = CreateObject("AdoCn.AdoCon")
  IniSvr = True
  Exit Function
  '
TrtErrEE1IniSvr:
  'tenta executar o software de impressao activex
  If ((Err = 429) And (Not nOk)) Then 'error$=ActiveX component can't create object
    NomImp = GetStD("conexao", "servidor")
    If (Dir$(NomImp) > "" And NomImp > "") Then
      Shell NomImp, vbMinimizedNoFocus: DoEvents
      nOk = True
      GoTo IniSvrDoAgain
    Else
      ErrMsg = "EE1 : IniSvr-> " & "Arquivo executavel do servidor de conexao nao encontrado!"
      Exit Function
    End If
  End If
  'outro erro
  ErrMsg = "EE1 : IniSvr-> " & Error$
  'bye
End Function
'
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
'
Private Function DoConn(Optional Nom As String, Optional ErrMsg As String) As Boolean
  'faz a conexao com o banco de dados EE
  'Nom: retorna nome indicativo da conexao
  Dim ConStr As String
  Dim Idx As Integer
  '
  ParDB.Pr = GetStD("Odbc", "Admin") 'schema
  ParDB.Or = IIf(Val(GetStD("Odbc", "NaoOra")) <> 0, False, True) 'nao oracle
  ParDB.C1 = GetStD("Ado", "Sub1Str") 'parametros de subs sql like 1 posicao
  ParDB.Ct = GetStD("Ado", "SubStr") 'parametros de subs sql like toda string
  If (ParDB.C1 = "") Then ParDB.C1 = IIf(ParDB.Or, "_", "?") 'oracle/access
  If (ParDB.Ct = "") Then ParDB.Ct = IIf(ParDB.Or, "%", "*")
  '
  ConStr = "Provider=" & GetStD("Ado", "Provider") & ";Dsn=" & GetStD("Odbc", "dsn")
  ConStr = ConStr & ";uid=" & GetStD("Odbc", "Username") & ";Pwd=" & CryptStringS(GetStD("Odbc", "Password"), CKEY)
  Idx = ConSvr.AdoOP(-1, Nom, ConStr, , , ErrMsg)
  If Idx = -1 Then
    Nom = ""
    ErrMsg = "EE3 : DoConn-> " & ErrMsg
    Exit Function
  End If
  Nom = IIf(Val(GetStD("Ado", "Nativo")) <> 0, "OleDB", "ODBC") & ":" & Nom
  IdConE = Idx
  DoConn = True
  '
End Function

