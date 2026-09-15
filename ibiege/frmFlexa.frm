VERSION 5.00
Begin VB.Form frmFlexa 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Informações complementares"
   ClientHeight    =   5565
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   13470
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmFlexa.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5565
   ScaleWidth      =   13470
   StartUpPosition =   1  'CenterOwner
   Begin VB.Frame Frame4 
      Height          =   5640
      Left            =   1320
      TabIndex        =   46
      Top             =   5100
      Visible         =   0   'False
      Width           =   8595
      Begin VB.CommandButton Command4 
         Height          =   380
         Left            =   6030
         Picture         =   "frmFlexa.frx":014A
         Style           =   1  'Graphical
         TabIndex        =   22
         ToolTipText     =   "Visualizar gráfico de deflexão"
         Top             =   5190
         Width           =   380
      End
      Begin VB.CommandButton Command3 
         Height          =   380
         Left            =   6510
         Picture         =   "frmFlexa.frx":04D2
         Style           =   1  'Graphical
         TabIndex        =   23
         ToolTipText     =   "Visualizar Relatório"
         Top             =   5190
         Width           =   380
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Voltar"
         Height          =   435
         Left            =   7020
         TabIndex        =   24
         Top             =   5160
         Width           =   1515
      End
      Begin VB.ListBox List1 
         BeginProperty Font 
            Name            =   "Courier"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   4935
         ItemData        =   "frmFlexa.frx":085F
         Left            =   60
         List            =   "frmFlexa.frx":0861
         TabIndex        =   47
         ToolTipText     =   "Para imprimir o relatório de saída clique no botão com uma impressora abaixo"
         Top             =   180
         Width           =   8475
      End
   End
   Begin VB.Frame Frame6 
      Height          =   5625
      Left            =   4860
      TabIndex        =   52
      Top             =   -90
      Width           =   8595
      Begin VB.PictureBox Picture1 
         Height          =   5235
         Left            =   60
         Picture         =   "frmFlexa.frx":0863
         ScaleHeight     =   5000
         ScaleMode       =   0  'User
         ScaleWidth      =   8445
         TabIndex        =   53
         Top             =   360
         Width           =   8505
         Begin VB.CommandButton Command2 
            Caption         =   "Resultado"
            Height          =   435
            Left            =   6930
            TabIndex        =   21
            Top             =   4740
            Width           =   1515
         End
      End
      Begin VB.Label Label4 
         Alignment       =   2  'Center
         BackColor       =   &H80000018&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "SISTEMA DE COORDENADAS"
         Height          =   255
         Left            =   60
         TabIndex        =   54
         Top             =   150
         Width           =   8505
      End
   End
   Begin VB.Frame Frame3 
      Height          =   495
      Left            =   0
      TabIndex        =   39
      Top             =   -90
      Width           =   4845
      Begin VB.OptionButton Option2 
         Caption         =   "MSS"
         Height          =   255
         Index           =   1
         Left            =   3690
         TabIndex        =   1
         Top             =   180
         Width           =   645
      End
      Begin VB.OptionButton Option2 
         Caption         =   "MIT"
         Height          =   255
         Index           =   0
         Left            =   2940
         TabIndex        =   0
         Top             =   180
         Value           =   -1  'True
         Width           =   645
      End
      Begin VB.CommandButton cmdBotao 
         Height          =   315
         Index           =   0
         Left            =   1950
         Picture         =   "frmFlexa.frx":8EA45
         Style           =   1  'Graphical
         TabIndex        =   42
         TabStop         =   0   'False
         ToolTipText     =   "Carregar informações do Fde"
         Top             =   150
         Width           =   380
      End
      Begin VB.TextBox Text2 
         Alignment       =   2  'Center
         BackColor       =   &H80000000&
         Enabled         =   0   'False
         Height          =   315
         Left            =   600
         TabIndex        =   40
         TabStop         =   0   'False
         Top             =   150
         Width           =   1305
      End
      Begin VB.Label Label2 
         Caption         =   "Nº Fde"
         Height          =   225
         Left            =   60
         TabIndex        =   41
         Top             =   210
         Width           =   495
      End
   End
   Begin VB.Frame Frame1 
      Height          =   5235
      Left            =   0
      TabIndex        =   25
      Top             =   300
      Width           =   4845
      Begin VB.Frame Frame5 
         Caption         =   "Forças concentradas na ponta de eixo (Ex: Empuxo Hidr.)"
         Height          =   1305
         Left            =   90
         TabIndex        =   48
         ToolTipText     =   "Forças devidas a empuxo hidráulico, engrenamento, acoplamento, etc."
         Top             =   3210
         Width           =   4695
         Begin VB.TextBox Text1 
            Alignment       =   1  'Right Justify
            Height          =   315
            Index           =   6
            Left            =   3510
            TabIndex        =   19
            Top             =   930
            Width           =   795
         End
         Begin VB.TextBox Text1 
            Alignment       =   1  'Right Justify
            Height          =   315
            Index           =   5
            Left            =   3510
            TabIndex        =   18
            Top             =   585
            Width           =   795
         End
         Begin VB.TextBox Text1 
            Alignment       =   1  'Right Justify
            Height          =   315
            Index           =   4
            Left            =   3510
            TabIndex        =   17
            Top             =   240
            Width           =   795
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "mm"
            Height          =   210
            Index           =   20
            Left            =   4350
            TabIndex        =   57
            Top             =   960
            Width           =   240
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "N"
            Height          =   210
            Index           =   19
            Left            =   4350
            TabIndex        =   56
            Top             =   630
            Width           =   105
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "N"
            Height          =   210
            Index           =   18
            Left            =   4350
            TabIndex        =   55
            Top             =   300
            Width           =   105
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "Dist. início eixo - ponto aplicação forças (Z,Y)"
            Height          =   210
            Index           =   17
            Left            =   90
            TabIndex        =   51
            Top             =   975
            Width           =   3315
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "Força Vertical (Y - Negativo para baixo)"
            Height          =   210
            Index           =   13
            Left            =   90
            TabIndex        =   50
            Top             =   630
            Width           =   2895
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "Força Horizontal (Z)"
            Height          =   210
            Index           =   12
            Left            =   90
            TabIndex        =   49
            Top             =   285
            Width           =   1455
         End
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   13
         Left            =   3660
         TabIndex        =   12
         Top             =   2130
         Width           =   900
      End
      Begin VB.CommandButton cmdBotao 
         Height          =   380
         Index           =   1
         Left            =   4380
         Picture         =   "frmFlexa.frx":8EDDD
         Style           =   1  'Graphical
         TabIndex        =   20
         ToolTipText     =   "Executa cálculo da flecha"
         Top             =   4785
         Width           =   380
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   12
         Left            =   2340
         TabIndex        =   11
         Top             =   2130
         Width           =   900
      End
      Begin VB.Frame Frame2 
         Caption         =   "Tipo Rotor"
         Height          =   615
         Index           =   1
         Left            =   60
         TabIndex        =   37
         Top             =   2550
         Width           =   2355
         Begin VB.OptionButton Option1 
            Caption         =   "Gaiola"
            Height          =   285
            Index           =   3
            Left            =   270
            TabIndex        =   13
            Top             =   240
            Value           =   -1  'True
            Width           =   1035
         End
         Begin VB.OptionButton Option1 
            Caption         =   "Anéis"
            Height          =   285
            Index           =   2
            Left            =   1320
            TabIndex        =   14
            Top             =   240
            Width           =   885
         End
      End
      Begin VB.Frame Frame2 
         Caption         =   "Fator Amplificação"
         Height          =   615
         Index           =   0
         Left            =   2430
         TabIndex        =   36
         Top             =   2550
         Width           =   2355
         Begin VB.OptionButton Option1 
            Caption         =   "Impacto"
            Height          =   285
            Index           =   1
            Left            =   1320
            TabIndex        =   16
            Top             =   240
            Width           =   885
         End
         Begin VB.OptionButton Option1 
            Caption         =   "Vibração"
            Height          =   285
            Index           =   0
            Left            =   270
            TabIndex        =   15
            Top             =   240
            Value           =   -1  'True
            Width           =   1035
         End
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Efeito Cisalhamento"
         Height          =   345
         Left            =   660
         TabIndex        =   35
         Top             =   1650
         Value           =   1  'Checked
         Width           =   1725
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   11
         Left            =   3990
         TabIndex        =   8
         Top             =   930
         Width           =   795
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   10
         Left            =   3990
         TabIndex        =   7
         Top             =   570
         Width           =   795
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   9
         Left            =   3990
         TabIndex        =   6
         Top             =   210
         Width           =   795
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   8
         Left            =   3990
         TabIndex        =   10
         Top             =   1650
         Width           =   795
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   7
         Left            =   3990
         TabIndex        =   9
         Top             =   1290
         Width           =   795
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   3
         Left            =   1560
         TabIndex        =   5
         Top             =   1290
         Width           =   795
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   2
         Left            =   1560
         TabIndex        =   4
         Top             =   930
         Width           =   795
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   1
         Left            =   1560
         TabIndex        =   3
         Top             =   570
         Width           =   795
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Height          =   345
         Index           =   0
         Left            =   1560
         TabIndex        =   2
         Top             =   210
         Width           =   795
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Pacote de chapas do rotor"
         Height          =   210
         Index           =   16
         Left            =   120
         TabIndex        =   45
         Top             =   2190
         Width           =   1920
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "LC"
         Height          =   210
         Index           =   15
         Left            =   3420
         TabIndex        =   44
         Top             =   2190
         Width           =   195
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "AGUARDE"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000000FF&
         Height          =   210
         Left            =   120
         TabIndex        =   43
         Top             =   4870
         Visible         =   0   'False
         Width           =   765
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Xi"
         Height          =   210
         Index           =   14
         Left            =   2130
         TabIndex        =   38
         Top             =   2190
         Width           =   135
      End
      Begin VB.Line Line1 
         BorderColor     =   &H80000014&
         Index           =   1
         X1              =   0
         X2              =   4830
         Y1              =   4725
         Y2              =   4725
      End
      Begin VB.Line Line1 
         BorderColor     =   &H80000015&
         Index           =   0
         X1              =   0
         X2              =   4830
         Y1              =   4710
         Y2              =   4710
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Inclinação Eixo"
         Height          =   210
         Index           =   11
         Left            =   2460
         TabIndex        =   34
         Top             =   990
         Width           =   1065
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Comp Pct. Chp. (mm)"
         Height          =   210
         Index           =   10
         Left            =   2460
         TabIndex        =   33
         Top             =   630
         Width           =   1500
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Diâm Chp Rot. (mm)"
         Height          =   210
         Index           =   9
         Left            =   2460
         TabIndex        =   32
         Top             =   270
         Width           =   1410
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Indução Média"
         Height          =   210
         Index           =   8
         Left            =   2430
         TabIndex        =   31
         Top             =   1710
         Width           =   1035
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Entreferro"
         Height          =   210
         Index           =   7
         Left            =   2430
         TabIndex        =   30
         Top             =   1350
         Width           =   735
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Nº de Pólos"
         Height          =   210
         Index           =   3
         Left            =   150
         TabIndex        =   29
         Top             =   1350
         Width           =   840
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "RPM"
         Height          =   210
         Index           =   2
         Left            =   150
         TabIndex        =   28
         Top             =   990
         Width           =   315
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "kW ou kVA"
         Height          =   210
         Index           =   1
         Left            =   150
         TabIndex        =   27
         Top             =   630
         Width           =   810
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Carcaça"
         Height          =   210
         Index           =   0
         Left            =   150
         TabIndex        =   26
         Top             =   270
         Width           =   615
      End
   End
End
Attribute VB_Name = "frmFlexa"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'===============================================================================
' Name: frmFlexa
' Purpose: dados de entrada/saída - cálculo da flecha
' Functions:
' Properties:
' Methods:
' Author: mpiazera
' Start:
' Modified:
'===============================================================================
Option Explicit

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Private Const pthmit As String = "c:\tmp\"

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Private Const pthvpmss As String = "c:\tmp\vp\aus\"

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Private Const pthspmss As String = "c:\tmp\sp\aus\"

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Private Type TpBur
  'eixo liso/costelado
  D_E As Double
  D_I As Double
  l As Double
  c As Double
  E_F As Double
  'eixo costelado
  Lg_A As Double
  At_A As Double
  Lg_F As Double
  At_F As Double
  Nr_C As Integer
End Type

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Dim iAx As Integer

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Dim PcLmDvSgt As Double

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Dim bLd As Boolean

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Dim bAct As Boolean

'===============================================================================
' Name: cmdBotao_Click
' Input:
'   Index As Integer
' Output:
' Purpose: Invocar rotinas associadas aos botões
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub cmdBotao_Click(Index As Integer)
  Select Case Index
    Case 0
      Call LdVls
    Case 1
      Call ExFl
  End Select
End Sub

'===============================================================================
' Name: LdVls
' Input:
' Output:
' Purpose: Fazer carga de dados do Fde
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub LdVls()
  Dim bMit As Boolean
  Dim b1 As Boolean, b2 As Boolean
  b1 = Option2(0).Value
  b2 = Option2(1).Value
  Call EnSc(Me, False)
  bMit = CBool(Option2(0).Value)
  If bMit Then
    Call LdVlsM
  Else
    If (Text2.Text <> "") Then
      Call LdVlsS
    Else
      MsgBox "Nº do Fde não informado!", vbCritical, "Atenção!"
    End If
  End If
  Call EnSc(Me, True)
  Option2(0).Value = b1
  Option2(1).Value = b2
End Sub

'===============================================================================
' Name: LdVlsM
' Input:
' Output:
' Purpose: Fazer carga de dados do FdeMit
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub LdVlsM()
  Dim sfl As String, sTp As String, Er As String
  Dim aln() As String, ef As String
  Dim i As Integer, pol As Integer
  Dim aTp() As String
  sfl = pthmit & "asyn.aus"
  If (Dir(sfl) = "") Then
    MsgBox "Não foi possível encontrar o arquivo de saída do Fde!", vbCritical, "Erro!"
    Exit Sub
  End If
  sTp = ""
  aln = LeMsk(sfl, Er)
  If (Er <> "") Then Call MsgBox(Er, vbCritical, "Atenção!"): Exit Sub
  sTp = Find(aln, "RECHNUNGSKENNUNG", , , i)
  'tipo de rotor
  Option1(3).Value = True
  If (Mid(sTp, 2, 1) = "A") Then Option1(2).Value = True
  'carcaça
  Text1(0).Text = Mid(sTp, 4, (Len(sTp) - 3) - 1)
  'potência
  Text1(1).Text = Find(aln, "PN")
  'rotação
  pol = (Val(Find(aln, " P")) * 2)
  Text1(2).Text = (120 * Val(Find(aln, "FN"))) / pol
  'nº de pólos
  Text1(3).Text = pol
  'diâmetro chp rotor
  ef = Val(Find(aln, "DELTAG"))
  Text1(9).Text = Val(Find(aln, "DI1")) - (ef * 2)
  'comprimento pct chp
  Text1(10).Text = Val(Find(aln, " L"))
  'entreferro
  Text1(7).Text = ef
  'indução média entreferro
  'Text1(8).Text = Round((Val(Find(aln, "BLM")) * Pi) / 2, 3)
  Text1(8).Text = Val(Find(aln, "BLM"))
  aTp = Split(aln(i), " ")
  'nº fde
  Text2.Text = aTp(UBound(aTp))
  Option2(0).Value = True
End Sub

'===============================================================================
' Name: LdVlsS
' Input:
' Output:
' Purpose: Fazer carga dos dados do FdeMss
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub LdVlsS()
  Dim sflv As String, sfls As String, sfl As String
  Dim sTp As String, Er As String
  Dim aln() As String, ef As String
  Dim i As Integer, pol As Integer
  Dim X As Date, Y As Date
  sfls = pthspmss & "spsyn.syn"
  sflv = pthvpmss & "vpsyn.syn"
  If (Dir(sfls) <> "") Then Kill (sfls)
  If (Dir(sflv) <> "") Then Kill (sflv)
  sTp = StrIni(App.Path & "\" & NMI, "CAMINHOS", "FdeMss")
  Call ExecCmd(sTp & " /WRAPED /FDE " & Text2.Text & " /EXIT YES /MIN YES", 1000, True)
  'aguardar criação de arquivo de saída do fde
  X = Now
  Do
    DoEvents
    If (Dir(sfls) <> "") Then sfl = sfls
    If (Dir(sflv) <> "") Then sfl = sflv
    Y = Now
    If (DateDiff("s", X, Y) > 30) Then Exit Do
  Loop Until ((sfl <> ""))
  'PiD = -1
  If (Dir(sfl) = "") Then
    MsgBox "Não foi possível encontrar o arquivo de saída do Fde!", vbCritical, "Erro!"
    Exit Sub
  End If
  sTp = ""
  aln = LeMsk(sfl, Er)
  If (Er <> "") Then Call MsgBox(Er, vbCritical, "Atenção!"): Exit Sub
  sTp = Find(aln, "RECHNUNGSKENNUNG", , , i)
  'carcaça
  Text1(0).Text = Mid(sTp, 4, (Len(sTp) - 3) - 1)
  'potência
  i = 0
  Find aln, "M O T O R I S C H E R", , , i, 0
  Text1(1).Text = Find(aln, IIf(i > 0, "PN", "SN"))
  'rotação
  pol = (Val(Find(aln, " P ")) * 2)
  Text1(2).Text = (120 * Val(Find(aln, "F1"))) / pol
  'nº de pólos
  Text1(3).Text = pol
  'diâmetro cho rotor
  ef = Val(Find(aln, "DELTAG"))
  Text1(9).Text = Val(Find(aln, "DI1")) - (ef * 2)
  'comprimento pct chp
  Text1(10).Text = Val(Find(aln, IIf(sfl = sfls, "LJL", "LA1")))
  'entreferro
  Text1(7).Text = ef
  'indução média entreferro
  Text1(8).Text = "0"
  On Local Error GoTo FimLdVlsS
  Call Kill(pthvpmss & "*.*")
  Call Kill(pthspmss & "*.*")
FimLdVlsS:
End Sub

'===============================================================================
' Name: ExFl
' Input:
' Output:
' Purpose: Calcular flecha
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub ExFl()
  Dim Er As String
  Dim Ex_C() As TpBur
  Dim o1 As Boolean, o2 As Boolean
  Call EnSc(Me, False)
  MousePointer = vbHourglass
  Label3.Caption = ""
  Label3.Enabled = True
  Label3.Visible = True
  o1 = Option2(0).Value: o2 = Option2(1).Value
  'valida preenchimento de campos
  If Not Vld(Er) Then GoTo ErExFl
  'verifica se precisa subdividir sessões com carga
  If Not GtAxDt(Ex_C, Er) Then GoTo ErExFl
  'gera arquivo texto
  If Not WrFl(Ex_C, Er) Then GoTo ErExFl
  'chama programa para calcular flecha
  If Not DoCl(Er) Then GoTo ErExFl
  'verifica geração do arquivo
  If Not VfFl(Er) Then GoTo ErExFl
  Label3.Visible = False
  Call EnSc(Me, True)
  'apresenta valores
  If Not SwRs(Er) Then GoTo ErExFl
  'gera relatório
  If Not WrRl(Ex_C, Er) Then GoTo ErExFl
  Option2(0).Value = o1: Option2(1).Value = o2
  MousePointer = vbNormal
  Exit Sub
ErExFl:
  Call MsgBox(Er, vbCritical, "Atenção!")
  Call EnSc(Me, True)
  Label3.Visible = False
  Option2(0).Value = o1: Option2(1).Value = o2
  MousePointer = vbNormal
End Sub

'===============================================================================
' Name: Vld
' Input:
'   ByRef Warnings As String - mensagens de alerta, caso houver
' Output:
' Purpose: Validar conteúdo dos camps da tela
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function Vld(ByRef Warnings As String) As Boolean
  Dim b As Boolean, i As Integer
  On Local Error GoTo ErVld
  For i = Text1.LBound To Text1.UBound 'preenche campos com zero
    If (Text1(i).Text = "") Then Text1(i).Text = "0"
  Next
  b = True
  If (Val(frmMain.lblComp(1).Caption) = 0) Then
    Warnings = "Não há segmentos de eixo informado!"
    b = False
  End If
  If (Val(frmMain.txtEntrada(12).Text) = 0) Then
    Warnings = Warnings & vbCr & "Mancal 1 não informado!"
    b = False
  End If
  If (Val(frmMain.txtEntrada(13).Text) = 0) Then
    Warnings = Warnings & vbCr & "Mancal 2 não informado!"
    b = False
  End If
  If (Val(Text1(12).Text) = 0) Then
    Warnings = Warnings & vbCr & "Início (Xi) do pacote de chapas não informado!"
    b = False
  End If
  If (Val(Text1(13).Text) = 0) Then
    Warnings = Warnings & vbCr & "Comprimento (LC) do pacote de chapas não informado!"
    b = False
  End If
  If ((Val(Text1(4).Text) > 0) Or (Val(Text1(5).Text) > 0)) Then
    If (Val(Text1(6).Text) = 0) Then
      Warnings = Warnings & vbCr & "Posição da força na ponta de eixo não informada!"
      b = False
    End If
  End If
  Vld = b
  Exit Function
ErVld:
  If (Warnings = "") Then Warnings = "Erro validando informações do eixo ->" & vbCr & Err.Number & " : " & Err.Description
  Vld = False
End Function

'===============================================================================
' Name: GtAxDt
' Input:
'   ByRef Axis() As TpBur - saída, informações do eixo
'   ByRef Errs As String  - mensagem de erro caso houver
' Output:
' Purpose: Extrair/Adaptar informações para as entradas do cálculo da flecha
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function GtAxDt(ByRef Ax() As TpBur, ByRef Errs As String) As Boolean
  Dim i As Integer, bg As Double, en As Double, IPc As Double
  Dim dE As Double, dI As Double, ef As Double, i_c() As Integer
  Dim lA As Double, aA As Double, lF As Double, aF As Double, nC As Double
  Dim dv As Boolean
  Dim GrdS As Object
  On Local Error GoTo ErGtAxDt
  Label3.Caption = "PREPARANDO DADOS..."
  PcLmDvSgt = Val(StrIni(App.Path & "\" & NMI, "FLECHA", "PERC_LIM_DIV_SEGM"))
  bg = 0: en = 0: iAx = 0
  ef = Val(Text1(7).Text) 'entreferro
  Set GrdS = frmMain.grdDimensoes
  IPc = Val(Text1(12).Text) 'segmento do pacote de chapas
  For i = 1 To (GrdS.Rows - 1)
    dE = Val(Val(GrdS.TextMatrix(i, 1))) 'diâmetro externo
    dI = Val(Val(GrdS.TextMatrix(i, 7))) 'diâmetro interno
    lA = Val(Val(GrdS.TextMatrix(i, 3))) 'largura alma/espessura costela
    If (lA > 0) Then
      aA = Val(Val(GrdS.TextMatrix(i, 2))) - _
           Val(Val(GrdS.TextMatrix(i, 1))) - _
           (2 * Val(Val(GrdS.TextMatrix(i, 4)))) 'altura alma
      lF = Val(Val(GrdS.TextMatrix(i, 5))) 'largura flange/largura martelo
      aF = Val(Val(GrdS.TextMatrix(i, 4))) 'altura flange/espessura martelo
      nC = Val(Val(GrdS.TextMatrix(i, 6)))
    Else
      aA = 0: lF = 0: aF = 0: nC = 0
    End If
    bg = en
    en = en + GrdS.TextMatrix(i, 0)
    'verifica se há carga(s) no segmento
    If (SgtCKg(bg, en, i_c, dv)) Then
      If (dv) Then
        'segmento com carga(s) menor(es) do que seu comprimento
        If Not AdDvSgt(Ax, bg, en, dE, dI, lA, aA, lF, aF, nC, ef, i_c, Errs) Then
          GoTo ErGtAxDt
        End If
      Else
        'segmento com carga ao longo de todo comprimento
        If Not AdSgt(Ax, bg, en, dE, dI, lA, aA, lF, aF, nC, IIf(Val(IPc) >= Val(bg) And Val(IPc) <= Val(en), ef, 0), i_c(0), Errs) Then
          GoTo ErGtAxDt
        End If
      End If
    Else
      'segmento sem carga
      If Not AdSgt(Ax, bg, en, dE, dI, lA, aA, lF, aF, nC, 0, 0, Errs) Then
        GoTo ErGtAxDt
      End If
    End If
  Next
  GtAxDt = True
  Exit Function
ErGtAxDt:
  If (Errs = "") Then Errs = "Erro processando dados do eixo -> " & vbCr & Err.Number & " : " & Err.Description
  GtAxDt = False
End Function

'===============================================================================
' Name: AdDvSgt
' Input:
'   ByRef Ax() As TpBur     - Vetor segmentos de eixo
'   ByVal bg As Double      - início do segmento
'   ByVal en As Double      - fim do segmento
'   ByVal dE As Double      - diametro externo
'   ByVal dI As Double      - diametro interno
'   ByVal lA As Double      - largura alma/espessura costela
'   ByVal aA As Double      - altura da alma
'   ByVal lF As Double      - largura do flange/largura do martelo
'   ByVal aF As Double      - altura do flange/espessura do martelo
'   ByVal nC As Double      - nº de costelas
'   ByVal ef As Double      - entreferro
'   ByRef iDc() As Integer  - vetor cargas do segmento
'   ByRef Errs As String    - mensagem de erro, caso houver
' Output:
' Purpose: Subdividir segmento de eixo que contem carga menor do que o seu tamanho
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function AdDvSgt(ByRef Ax() As TpBur, ByVal bg As Double, ByVal en As Double, ByVal dE As Double, ByVal dI As Double, ByVal lA As Double, ByVal aA As Double, ByVal lF As Double, ByVal aF As Double, ByVal nC As Double, ByVal ef As Double, ByRef iDc() As Integer, ByRef Errs As String) As Boolean
  Dim j As Integer, bgc As Double, edc As Double
  On Local Error GoTo ErAdDvSgt
  j = 0
  Do
    ReDim Preserve Ax(iAx)
    Ax(UBound(Ax)).D_E = dE
    Ax(UBound(Ax)).D_I = dI
    Ax(UBound(Ax)).Lg_A = lA
    Ax(UBound(Ax)).At_A = aA
    Ax(UBound(Ax)).At_F = aF
    Ax(UBound(Ax)).Lg_F = lF
    Ax(UBound(Ax)).Nr_C = nC
    If (j <= UBound(iDc)) Then 'ainda tem cargas nesse segmento?
      bgc = Val(frmMain.grdCargas.TextMatrix(iDc(j), 0))
      edc = bgc + Val(frmMain.grdCargas.TextMatrix(iDc(j), 1))
      'iT = IIf(EmptyArray(Ax), -1, UBound(Ax))
      If (Val(bg) < Val(bgc)) Then ''sub-segmento' antes de uma carga
        Ax(UBound(Ax)).l = bgc - bg
        bg = bgc
      Else ''sub-segmento' de uma carga (distribuída)
        Ax(UBound(Ax)).l = edc - bg
        Ax(UBound(Ax)).c = Val(frmMain.grdCargas.TextMatrix(iDc(j), 2))
        If (CDbl(Val(Text1(12).Text)) = CDbl(Val(bg))) Then
          Ax(UBound(Ax)).E_F = ef
        End If
        bg = edc
        j = j + 1
      End If
    Else 'último 'sub-segmento'
      Ax(UBound(Ax)).l = en - bg
      bg = en
    End If
    iAx = iAx + 1
  Loop While (Val(bg) < Val(en))
  AdDvSgt = True
  Exit Function
ErAdDvSgt:
  If (Errs = "") Then Errs = "Erro dividindo segmento de eixo -> " & vbCr & Err.Number & " : " & Err.Description
  AdDvSgt = False
End Function

'===============================================================================
' Name: AdSgt
' Input:
'   ByRef Ax() As TpBur     - Vetor segmentos de eixo
'   ByVal bg As Double      - início do segmento
'   ByVal en As Double      - fim do segmento
'   ByVal dE As Double      - diametro externo
'   ByVal dI As Double      - diametro interno
'   ByVal lA As Double      - largura alma/espessura costela
'   ByVal aA As Double      - altura da alma
'   ByVal lF As Double      - largura do flange/largura do martelo
'   ByVal aF As Double      - altura do flange/espessura do martelo
'   ByVal nC As Double      - nº de costelas
'   ByVal ef As Double      - entreferro
'   ByRef iDc() As Integer  - vetor cargas do segmento
'   ByRef Errs As String    - mensagem de erro, caso houver
' Output:
' Purpose: Adicionar informações do segmento de eixo
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function AdSgt(ByRef Ax() As TpBur, ByVal bg As Double, ByVal en As Double, ByVal dE As Double, ByVal dI As Double, ByVal lA As Double, ByVal aA As Double, ByVal lF As Double, ByVal aF As Double, ByVal nC As Double, ByVal ef As Double, ByRef iC As Integer, ByRef Errs As String) As Boolean
  On Local Error GoTo ErAdSgt
  ReDim Preserve Ax(iAx)
  Ax(UBound(Ax)).D_E = dE
  Ax(UBound(Ax)).D_I = dI
  Ax(UBound(Ax)).Lg_A = lA
  Ax(UBound(Ax)).At_A = aA
  Ax(UBound(Ax)).At_F = aF
  Ax(UBound(Ax)).Lg_F = lF
  Ax(UBound(Ax)).E_F = ef
  Ax(UBound(Ax)).Nr_C = nC
  Ax(UBound(Ax)).l = en - bg
  Ax(UBound(Ax)).c = Val(frmMain.grdCargas.TextMatrix(iC, 2))
  iAx = iAx + 1
  AdSgt = True
  Exit Function
ErAdSgt:
  Errs = "Erro adicionando informações de segmento de eixo ->" & vbCr & Err.Number & " : " & Err.Description
  AdSgt = False
End Function

'===============================================================================
' Name: SgtCKg
' Input:
'   ByVal bg As Double      - início do segmento
'   ByVal ed As Double      - fim do segmento
'   ByRef i_c() As Integer  - arrays com índices das cargas do segmento
'   ByRef dv As Boolean     - segmento deve ser sub-dividido??
' Output:
' Purpose: Verificar se existe(m) carga(s) no segmento e se deve ser sub-dividido
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function SgtCKg(ByVal bg As Double, ByVal ed As Double, ByRef id_kg() As Integer, ByRef dv As Boolean) As Boolean
  Dim j As Integer, bgc As Double, lnc As Double
  Dim b As Boolean, nC As Byte
  nC = 0
  b = False
  dv = False
  For j = 1 To (frmMain.grdCargas.Rows - 1)
    bgc = Val(Val(frmMain.grdCargas.TextMatrix(j, 0)))
    lnc = frmMain.grdCargas.TextMatrix(j, 1)
    If (Val(bgc + lnc) <= Val(ed)) And (Val(bgc) >= Val(bg)) Then
      b = True
      ReDim Preserve id_kg(nC)
      id_kg(nC) = j
      nC = nC + 1
      If (Val(lnc) < ((Val(ed) - Val(bg)) * Val(PcLmDvSgt))) Then dv = True
    ElseIf (bgc > ed) Then
      Exit For
    End If
  Next
  SgtCKg = b
End Function

'===============================================================================
' Name: WrFl
' Input:
'  ByRef Ax() As TpBur  - array com informações
'  ByRef Errs As String - mensagem de erro, caso houver
' Output:
' Purpose: Escrever arquivo com informções para cálculo da flecha
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function WrFl(ByRef Ax() As TpBur, ByRef Errs As String) As Boolean
  Dim i As Integer, Fl As String
  Dim m§x As Integer 'nº máximo (fixo) de escalonamentos
  Dim GrdS As Object
  Dim dst As Double
  On Local Error GoTo ErWrFl
  Label3.Caption = "GERANDO ARQUIVO..."
  Set GrdS = frmMain.grdDimensoes
  m§x = CInt(Val(StrIni(App.Path & "\" & NMI, "FLECHA", "NRESC")))
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "OUT")
  Open Fl For Output As #1
  Print #1, Trim(Text1(0).Text) 'tamanho carcaça
  Print #1, Trim(Text1(1).Text) 'potência [Kw]
  Print #1, Trim(Text1(2).Text) 'rotação  [RPM]
  Print #1, Trim(Text1(3).Text) 'nº de pólos
  Print #1, 2 'nº de mancais
  Print #1, Trim(frmMain.txtEntrada(12).Text) 'mancal 1
  Print #1, Trim(frmMain.txtEntrada(13).Text) 'mancal 2
  Print #1, "0" 'mancal 3
  Print #1, "0" 'mancal 4
  Print #1, "7850" 'massa específica
  Print #1, "207"  'módulo elasticidade aço
  Print #1, "0.3"  'coeficiente de poisson
  Print #1, (UBound(Ax) + 1) 'nº de escalonamentos
  For i = 1 To m§x
    If (UBound(Ax) >= (i - 1)) Then
      Print #1, Ax(i - 1).D_E 'diâmetro externo seção
      Print #1, Ax(i - 1).D_I 'diâmetro interno
      Print #1, Ax(i - 1).l 'comprimento seção
      Print #1, Ax(i - 1).c 'massa distribuída seção
      Print #1, Ax(i - 1).E_F 'entreferro
    Else
      Print #1, "0" & Chr(13) & Chr(10) & "0" & Chr(13) & Chr(10) & "0" & Chr(13) & Chr(10) & "0" & Chr(13) & Chr(10) & "0"
    End If
  Next
  For i = 1 To m§x
    If (UBound(Ax) >= (i - 1)) Then
      Print #1, Ax(i - 1).Lg_A 'largura alma/costela
      Print #1, Ax(i - 1).At_A 'altura alma
      Print #1, Ax(i - 1).Lg_F 'largura flange/martelo
      Print #1, Ax(i - 1).At_F 'altura flange/espessura martelo
      Print #1, Ax(i - 1).Nr_C 'nº costelas
    Else
      Print #1, "0" & Chr(13) & Chr(10) & "0" & Chr(13) & Chr(10) & "0" & Chr(13) & Chr(10) & "0" & Chr(13) & Chr(10) & "0"
    End If
  Next
  Print #1, "0"   'nº de componentes massa concentrada
  For i = 1 To 10
    Print #1, "0" 'massa
    Print #1, "0" 'posição da massa
  Next
  Print #1, Trim(Text1(7).Text) 'entreferro
  Print #1, Trim(Text1(8).Text) 'indução média no entreferro
  Print #1, Trim(Text1(9).Text) 'diâmetro chapas rotor
  Print #1, IIf(Option1(2).Value, "1", "2") 'tipo rotor
  Print #1, Trim(Text1(10).Text) 'comprimento pacote rotor
  dst = Val(Text1(12).Text) + (Val(Text1(13).Text) / 2)
  Print #1, dst 'distancia: início eixo -> centro pct rotor
  Print #1, Trim(Text1(11).Text) 'inclinação do eixo
  Print #1, "0" 'aceleração direção X
  Print #1, "0" 'aceleração direção Y
  Print #1, "0" 'aceleração direção Z
  Print #1, IIf(Option1(0).Value, "1", "2") 'fator amplificação din.
  Print #1, "0" 'nº cargas concentradas direção X
  Print #1, IIf(Val(Text1(5).Text) <> 0, "1", "0") 'nº cargas concentradas direção Y
  Print #1, IIf(Val(Text1(4).Text) <> 0, "1", "0") 'nº cargas concentradas direção Z
  For i = 1 To 10 'cargas concentradas (X)
    Print #1, "0" 'intensidade
    Print #1, "0" 'posição
  Next
  Print #1, Val(Text1(5).Text)
  Print #1, IIf(Val(Text1(5).Text) <> 0, Val(Text1(6).Text), "0")
  For i = 1 To 9 'cargas concentradas (Y)
    Print #1, "0" 'intensidade
    Print #1, "0" 'posição
  Next
  Print #1, Val(Text1(4).Text)
  Print #1, IIf(Val(Text1(4).Text) <> 0, Val(Text1(6).Text), "0")
  For i = 1 To 9 'cargas concentradas (Z)
    Print #1, "0" 'intensidade
    Print #1, "0" 'posição
  Next
  Print #1, Check1.Value 'efeito cisalhamento
  Close #1
  WrFl = True
  Exit Function
ErWrFl:
  Errs = "Erro gerando arquivo para flecha -> " & vbCr & Err.Number & " : " & Err.Description & vbCr & "[" & Fl & "]"
  WrFl = False
End Function

'===============================================================================
' Name: VfFl
' Input:
'   ByRef Er As String - mensagem de erro, caso houver
' Output:
' Purpose: Verificar a criação do arquivo pelo programa da flecha
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function VfFl(ByRef Er As String) As Boolean
  Dim Fl As String
  Dim X As Date, Y As Date
  Dim m§x As Double
  Dim df As Integer, bdf As Integer, iPt As Integer
  m§x = StrIni(App.Path & "\" & NMI, "FLECHA", "WAIT")
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "IN")
  X = Now
  iPt = 1
  Do
    DoEvents
    Y = Now
    df = DateDiff("s", X, Y)
    If (df > m§x) Then Exit Do
    If (df > bdf) Then
      bdf = df
      Label3.Caption = "AGUARDE" & String(iPt, ".")
      iPt = iPt + 1
      If (iPt > 3) Then iPt = 0
    End If
  Loop Until (Dir(Fl) <> "")
  'PiD = -1
  If (Dir(Fl) = "") Then
    Er = "Arquivo de saída do cálculo da flecha não encontrado!" & _
         "[" & Fl & "]"
    VfFl = False
  Else
    Call MsgBox("Flecha executada com sucesso!" & vbCr & "Tempo:" & DateDiff("s", X, Y) & " segundos.", vbInformation, "Atenção!")
    VfFl = True
  End If
End Function

'===============================================================================
' Name: SwRs
' Input:
'   ByRef Errs As String - mensagem de erro caso houver
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function SwRs(ByRef Errs As String) As Boolean
  Dim Fl As String, sTp As String, aDs() As String
  Dim i As Byte
  On Local Error GoTo ErSwRs
  
  aDs = Split("Reação Mancal Dianteiro - direção Y (Vertical)(kN)|" & _
              "Reação Mancal Dianteiro - direção Z (Horizontal)(kN)|" & _
              "Reação Mancal Traseiro - direção Y (Vertical)(kN)|" & _
              "Reação Mancal Traseiro - direção Z (Horizontal)(kN)|" & _
              "Flecha Percentual Máxima em Relação ao Entreferro(%)|" & _
              "Posição Flecha Máxima em relação ao entreferro(mm)|" & _
              "Flecha Dianteira Eixo(mm)|" & _
              "Flecha Traseira Eixo(mm)", "|")
  Frame4.Top = Frame6.Top
  Frame4.Left = Frame6.Left
  Frame4.Visible = True
  'Frame1.Visible = False
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "IN")
  i = 0
  List1.Clear
  Open Fl For Input As #1
  Do While Not EOF(1)
    Line Input #1, sTp
    List1.AddItem PadStr(aDs(i), 53) & " -> " & Trim(sTp)
    i = i + 1
  Loop
  Close #1
  SwRs = True
  Exit Function
ErSwRs:
  Errs = "Erro recuperando saída do cálculo da flecha ->" & vbCr & Err.Number & " : " & Err.Description & vbCr & "[" & Fl & "]"
  SwRs = False
End Function

'===============================================================================
' Name: DoCl
' Input:
'   ByRef Errs As String - mensagem de erro caso houver
' Output:
' Purpose:
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function DoCl(ByRef Errs As String) As Boolean
  Dim Fl As String, flrs As String
  On Local Error GoTo ErDoCl
  'Label3.Caption = "Executando 'Flecha.exe', aguarde..."
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "EXE")
  flrs = StrIni(App.Path & "\" & NMI, "FLECHA", "IN")
  If (Dir(flrs) <> "") Then Call Kill(flrs)
  Call ExecCmd(Fl & " F", 0, True)
  DoCl = True
  Exit Function
ErDoCl:
  If (Errs = "") Then Errs = "Erro executando programa de flecha ->" & vbCr & Err.Number & " : " & Err.Description & vbCr & "[" & Fl & "]"
  DoCl = False
End Function

'===============================================================================
' Name: Command1_Click
' Input:
' Output:
' Purpose: Invocar rotina associada a Command1 (Voltar)
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub Command1_Click()
  Frame4.Visible = False
  Label3.Visible = False
End Sub

'===============================================================================
' Name: Command2_Click
' Input:
' Output:
' Purpose: Invocar rotina associada a Command2 (Resultado)
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub Command2_Click()
  Frame4.Top = Frame6.Top
  Frame4.Left = Frame6.Left
  Frame4.Visible = True
End Sub

'===============================================================================
' Name: Command3_Click
' Input:
' Output:
' Purpose: Invocar rotina associada a Command3 (Relatório - saída)
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub Command3_Click()
  Dim Fl As String
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "REL")
  If (Dir(Fl) <> "") Then
    Dim frms As Object
    Set frms = New frmSaida
    frms.inicializa Fl
    Set frms = Nothing
  Else
    MsgBox "Não foi possível encontrar arquivo relatório do cálculo da flecha!" & vbCr & vbCr & Fl, vbCritical, "Atenção!"
  End If
End Sub

'===============================================================================
' Name: Command4_Click
' Input:
' Output:
' Purpose: Invocar rotina associada a Command4 (Gráfico da flecha)
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub Command4_Click()
  frmGphFlc.Show
  Call frmGphFlc.Desenha_Flecha(0)
  Call frmGphFlc.Desenha_Flecha(1)
End Sub

'===============================================================================
' Name: Form_Activate
' Input:
' Output:
' Purpose: Verificar se pacote do rotor foi definido
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub Form_Activate()
  If Not bAct Then
    If Not bLd Then
      Call MsgBox("Carga que representa pacote do rotor não definida, duplo clique na carga para indicar!", vbExclamation, "Atenção!")
      Unload Me
    Else
      bAct = True
    End If
  End If
End Sub

'===============================================================================
' Name: Form_Load
' Input:
' Output:
' Purpose: Invocar rotina para obter informações do pacote do rotor
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub Form_Load()
  bLd = LdPc()
End Sub

'===============================================================================
' Name: LdPc
' Input:
' Output:
' Purpose: Obter informações do pacote do rotor
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function LdPc() As Boolean
  Dim i As Integer, bF As Boolean
  Dim Grd As Object
  bF = False
  Set Grd = frmMain.grdCargas
  For i = 1 To (frmMain.grdCargas.Rows - 1)
    frmMain.grdCargas.Row = i: frmMain.grdCargas.Col = 0
    If (frmMain.grdCargas.CellBackColor = BkgPck) Then
      Text1(12).Text = frmMain.grdCargas.TextMatrix(i, 0)
      Text1(13).Text = frmMain.grdCargas.TextMatrix(i, 1)
      bF = True
    End If
  Next
  LdPc = bF
End Function

'===============================================================================
' Name: Form_Unload
' Input:
' Output:
' Purpose: Eliminar arquivos gerados
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub Form_Unload(Cancel As Integer)
  Dim Fl As String
  On Local Error GoTo FimUl
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "REL")
  Call Kill(Fl)
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "GPH1")
  Call Kill(Fl)
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "GPH2")
  Call Kill(Fl)
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "GPH3")
  Call Kill(Fl)
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "OUT")
  Call Kill(Fl)
FimUl:
  bAct = False
End Sub

'===============================================================================
' Name: Option2_Click
' Input:
'   Index As Integer
' Output:
' Purpose: Habilitar/Desabilitar caixa de texto (fde)
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub Option2_Click(Index As Integer)
  Select Case Index
    Case 0 'MIT
      Text2.Enabled = False
      Text2.BackColor = &H80000000
    Case 1 'MSS
      Text2.Enabled = True
      Text2.BackColor = &H80000005
      Text2.SetFocus
  End Select
End Sub

'===============================================================================
' Name: WrRl
' Input:
'  ByRef Ax() As TpBur  - array com informações
'  ByRef Errs As String - mensagem de erro, caso houver
' Output:
' Purpose: Escrever arquivo relatório
' Remarks:
' Author: mpiazera
'===============================================================================
Private Function WrRl(ByRef Ax() As TpBur, ByRef Errs As String) As Boolean
  Dim i As Integer, Fl As String
  Dim m§x As Integer 'nº máximo (fixo) de escalonamentos
  Dim GrdS As Object
  Dim dst As Double
  Dim k As Integer, j As Integer, l As Integer, ul As Integer
  On Local Error GoTo ErWrFl
  j = 12: k = 40: l = 8: ul = 97
  Set GrdS = frmMain.grdDimensoes
  m§x = CInt(Val(StrIni(App.Path & "\" & NMI, "FLECHA", "NRESC")))
  Fl = StrIni(App.Path & "\" & NMI, "FLECHA", "REL")
  Open Fl For Output As #1
  Print #1, PadStr("IBiege", j) & ":" & frmMain.txtEntrada(0).Text
  Print #1, PadStr("Usuário", j) & ":" & frmMain.lblUsuario.Caption
  Print #1, PadStr("Data", j) & ":" & frmMain.lblData.Caption
  Print #1, String(ul, "_")
  Print #1, "DADOS DE ENTRADA"
  Print #1, PadStr("Carcaça", j) & ":" & Trim(Text1(0).Text)
  Print #1, PadStr("Potência", j) & ":" & Trim(Text1(1).Text) & " kW/kVA"
  Print #1, PadStr("Rotação", j) & ":" & Trim(Text1(2).Text) & " rpm"
  Print #1, PadStr("Polaridade", j) & ":" & Trim(Text1(3).Text)
  Print #1, "Distancia a partir da ponta do eixo"
  Print #1, PadStr("Mancal D.", j) & ":" & Trim(frmMain.txtEntrada(j).Text) & Chr(9) & _
            PadStr("Mancal T.", j) & ":" & Trim(frmMain.txtEntrada(13).Text)
  Print #1, PadStr("Indução média entref.", k) & ":" & Trim(Text1(8).Text) & " tesla"
  Print #1, PadStr("Diâm. Pct. Rotor", k) & ":" & Trim(Text1(9).Text) & " mm"
  Print #1, PadStr("Comp. Pct. Rotor", k) & ":" & Trim(Text1(10).Text) & " mm"
  Print #1, PadStr("Tipo Rotor", k) & ":" & IIf(Option1(2).Value, "Anéis", "Gaiola")
  dst = Val(Text1(j).Text) + (Val(Text1(13).Text) / 2)
  Print #1, PadStr("Dist. Centro Pct a Partir Início Eixo", k) & ":" & dst & " mm"
  Print #1, PadStr("Inclinação do eixo", k) & ":" & Trim(Text1(11).Text) & " °"
  Print #1, PadStr("Fator amplificação", k) & ":" & IIf(Option1(0).Value, "Vibração", "Impacto")
  Print #1, PadStr("Efeito cisalhamento", k) & ":" & IIf(Check1.Value, "Sim", "Não")
  Print #1, PadStr("Força Horizontal (Z)", k) & ":" & Val(Text1(4).Text) & " N"
  Print #1, PadStr("Força Vertical (Y)", k) & ":" & Val(Text1(5).Text) & " N"
  Print #1, PadStr("Dist. Aplic. Força a Partir Início Eixo", k) & ":" & Val(Text1(6).Text) & " mm"
  Print #1, ""
  Print #1, "DIMENSÕES DO EIXO EM 'mm'"
  Print #1, PadStr("D.Ext", l) & "  " & PadStr("D.Int", l) & "  " & _
            PadStr("Comp.", l) & "  " & PadStr("Massa", l) & "  " & _
            PadStr("Entref.", l) & "  " & PadStr("Larg.Alm", l) & "  " & _
            PadStr("Alt.Alm", l) & "  " & PadStr("Larg.Fla", l) & "  " & _
            PadStr("Alt.Fla", l) & "  " & PadStr("NºCost.", l)
  For i = 1 To m§x
    If (UBound(Ax) >= (i - 1)) Then
      Print #1, PadStr(Ax(i - 1).D_E, l) & "  " & PadStr(Ax(i - 1).D_I, l) & "  " & _
              PadStr(Ax(i - 1).l, l) & "  " & PadStr(Ax(i - 1).c, l) & "  " & _
              PadStr(Ax(i - 1).E_F, l) & "  " & PadStr(Ax(i - 1).Lg_A, l) & "  " & _
              PadStr(Ax(i - 1).At_A, l) & "  " & PadStr(Ax(i - 1).Lg_F, l) & "  " & _
              PadStr(Ax(i - 1).At_F, l) & "  " & PadStr(Ax(i - 1).Nr_C, l)
    End If
  Next
  Print #1, ""
  Print #1, String(ul, "_")
  Print #1, "DADOS DE SAÍDA"
  For i = 0 To (List1.ListCount - 1)
    Print #1, List1.List(i)
  Next
  Close #1
  WrRl = True
  Exit Function
ErWrFl:
  Errs = "Erro gerando arquivo para flecha -> " & vbCr & Err.Number & " : " & Err.Description & vbCr & "[" & Fl & "]"
  WrRl = False
End Function
