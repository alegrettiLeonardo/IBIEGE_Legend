VERSION 5.00
Begin VB.Form frmCalculos 
   Caption         =   "Lista de Cálculos"
   ClientHeight    =   5055
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   6900
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmCalculos.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   5055
   ScaleWidth      =   6900
   StartUpPosition =   1  'CenterOwner
   Begin VB.Frame Frame2 
      Height          =   645
      Left            =   0
      TabIndex        =   5
      Top             =   0
      Width           =   6885
      Begin VB.CommandButton cmdChoice 
         Height          =   435
         Index           =   0
         Left            =   90
         Picture         =   "frmCalculos.frx":014A
         Style           =   1  'Graphical
         TabIndex        =   14
         ToolTipText     =   "Abrir cálculo selecionado"
         Top             =   150
         Width           =   380
      End
      Begin VB.CommandButton cmdChoice 
         Height          =   435
         Index           =   1
         Left            =   480
         Picture         =   "frmCalculos.frx":04D1
         Style           =   1  'Graphical
         TabIndex        =   13
         ToolTipText     =   "Cancelar"
         Top             =   150
         Width           =   380
      End
      Begin VB.TextBox Text1 
         Height          =   375
         Index           =   0
         Left            =   1920
         TabIndex        =   12
         Text            =   "Text1"
         Top             =   195
         Width           =   1455
      End
      Begin VB.TextBox Text1 
         Height          =   375
         Index           =   1
         Left            =   4275
         TabIndex        =   11
         Text            =   "Text1"
         Top             =   195
         Width           =   1455
      End
      Begin VB.CommandButton Command1 
         Caption         =   "+"
         Height          =   420
         Index           =   0
         Left            =   3360
         TabIndex        =   10
         Top             =   165
         Width           =   165
      End
      Begin VB.CommandButton Command1 
         Caption         =   "-"
         Height          =   420
         Index           =   1
         Left            =   3510
         TabIndex        =   9
         Top             =   165
         Width           =   165
      End
      Begin VB.CommandButton Command1 
         Caption         =   "+"
         Height          =   420
         Index           =   2
         Left            =   5760
         TabIndex        =   8
         Top             =   165
         Width           =   165
      End
      Begin VB.CommandButton Command1 
         Caption         =   "-"
         Height          =   420
         Index           =   3
         Left            =   5910
         TabIndex        =   7
         Top             =   165
         Width           =   165
      End
      Begin VB.CommandButton Command1 
         Height          =   420
         Index           =   4
         Left            =   6270
         Picture         =   "frmCalculos.frx":083D
         Style           =   1  'Graphical
         TabIndex        =   6
         Top             =   165
         Width           =   525
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Período de "
         Height          =   210
         Index           =   0
         Left            =   1080
         TabIndex        =   16
         Top             =   270
         Width           =   810
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "até"
         Height          =   210
         Index           =   1
         Left            =   3915
         TabIndex        =   15
         Top             =   270
         Width           =   225
      End
   End
   Begin VB.Frame Frame1 
      Height          =   4425
      Left            =   0
      TabIndex        =   0
      Top             =   630
      Width           =   6885
      Begin VB.ListBox lstCalculos 
         BeginProperty Font 
            Name            =   "Courier New"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   3840
         ItemData        =   "frmCalculos.frx":0BE6
         Left            =   30
         List            =   "frmCalculos.frx":0BE8
         TabIndex        =   4
         Top             =   540
         Width           =   6795
      End
      Begin VB.CommandButton cmdOrd 
         Caption         =   "Ordenar"
         Height          =   375
         Index           =   0
         Left            =   30
         Style           =   1  'Graphical
         TabIndex        =   3
         Top             =   150
         Width           =   3345
      End
      Begin VB.CommandButton cmdOrd 
         Caption         =   "Ordenar"
         Height          =   375
         Index           =   1
         Left            =   3570
         Style           =   1  'Graphical
         TabIndex        =   2
         Top             =   150
         Width           =   1365
      End
      Begin VB.CommandButton cmdOrd 
         Caption         =   "Ordenar"
         Height          =   375
         Index           =   2
         Left            =   5130
         Style           =   1  'Graphical
         TabIndex        =   1
         Top             =   150
         Width           =   1125
      End
   End
End
Attribute VB_Name = "frmCalculos"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim bUP As Boolean
Dim IndAnt As Integer
Dim DifW As Double, DifH As Double
Dim DifWf As Double, DifHf As Double

Private Sub cmdChoice_Click(Index As Integer)
  If (Index = 0) Then
    If (lstCalculos.ListIndex = -1) Then MsgBox "Selecione um Item", vbCritical, "Atenção!": Exit Sub
    frmMain.txtEntrada(0).Text = Trim(Mid(lstCalculos.Text, 1, 31))
  End If
  Unload Me
End Sub

Private Sub cmdOrd_Click(Index As Integer)
  Dim i As Integer
  For i = cmdOrd.LBound To cmdOrd.UBound
    cmdOrd(i).Caption = "Ordenar"
    Set cmdOrd(i).Picture = Nothing
  Next
  
  cmdOrd(Index).Caption = ""
  
  If (IndAnt = Index) Then
    If bUP Then
      Set cmdOrd(Index).Picture = LoadResPicture(114, 0)
      bUP = False
    Else
      Set cmdOrd(Index).Picture = LoadResPicture(113, 0)
      bUP = True
    End If
  Else
    Set cmdOrd(Index).Picture = LoadResPicture(114, 0)
    bUP = False
  End If
  IndAnt = Index
  
  Call GetClcs(Format(CDate(Text1(0).Text) - 1, "DD/MM/YYYY"), Format(CDate(Text1(1).Text) + 1, "DD/MM/YYYY"), Index, bUP)
End Sub

Private Sub Command1_Click(Index As Integer)
  Select Case Index
    Case 0
      If (Text1(0).Text <> "") Then
        Text1(0).Text = Format(CDate(Text1(0).Text) + 1, "dd/mm/yyyy")
      End If
    Case 1
      If (Text1(0).Text <> "") Then
        Text1(0).Text = Format(CDate(Text1(0).Text) - 1, "dd/mm/yyyy")
      End If
    Case 2
      If (Text1(1).Text <> "") Then
        Text1(1).Text = Format(CDate(Text1(1).Text) + 1, "dd/mm/yyyy")
      End If
    Case 3
      If (Text1(1).Text <> "") Then
        Text1(1).Text = Format(CDate(Text1(1).Text) - 1, "dd/mm/yyyy")
      End If
    Case 4
      Call GetClcs(Format(CDate(Text1(0).Text) - 1, "DD/MM/YYYY"), Format(CDate(Text1(1).Text) + 1, "DD/MM/YYYY"), 0, True)
      
  End Select
End Sub

Private Sub Form_Load()
  DifW = Me.Width - lstCalculos.Width
  DifH = Me.Height - lstCalculos.Height
  DifWf = Me.Width - Frame1.Width
  DifHf = Me.Height - Frame1.Height
  Text1(0).Text = Format(Date - 15, "dd/mm/yyyy")
  Text1(1).Text = Format(Date, "dd/mm/yyyy")
  Call GetClcs(Format(CDate(Text1(0).Text) - 1, "DD/MM/YYYY"), Format(CDate(Text1(1).Text) + 1, "DD/MM/YYYY"), 0, True)
  bUP = True
End Sub

Private Function GetClcs(ByVal Ini As String, ByVal Fim As String, ByVal OrdInd As Integer, ByVal AscOrd As Boolean) As Boolean
  Dim Sql As String, Tmp As String, ErrMsg As String, linha As String
  Dim i As Integer
  Dim SqOrd As String, SqCnd As String
  '
  On Error GoTo TrtErrGetClcs
  If (Ini <> "") Then SqCnd = " dt_dat>=to_date('" & Ini & "','DD/MM/YYYY')"
  If (Fim <> "") Then
    If (Ini <> "") Then Tmp = " and"
    SqCnd = SqCnd & Tmp & " dt_dat<=to_date('" & Fim & "','DD/MM/YYYY')"
    Tmp = ""
  End If
  If (SqCnd <> "") Then SqCnd = "Where " & SqCnd
  Select Case OrdInd
    Case 0  'coluna zero
        SqOrd = " order by nr_ref"
    Case 1  'coluna um
        SqOrd = " order by nm_usu"
    Case 2  'coluna dois
        SqOrd = " order by dt_dat"
  End Select
  If AscOrd Then
    SqOrd = SqOrd & " ASC"
  Else
    SqOrd = SqOrd & " DESC"
  End If
  Sql = "select id, nr_ref, nm_usu, dt_dat, ds_calc " & _
        "from " & ParDB.Pr & "d_idnbiege " & _
        SqCnd & " " & _
        SqOrd
  AdoCon.AdoSQ Tmp, IDConE, Sql, True, 999, ErrMsg
  '
  lstCalculos.Clear
  If (Tmp <> "") Then
    For i = 1 To ContaItens(Tmp, Chr(0))
      linha = PegaItem(Tmp, Chr(0), i)
      lstCalculos.AddItem PadStr(PegaItem(linha, Chr(1), 2), 31) & "- " & PadStr(PegaItem(linha, Chr(1), 3), 13) & "- " & Format(PegaItem(linha, Chr(1), 4), "DD/MM/YYYY") & " - " & PadStr(PegaItem(linha, Chr(1), 5), 50)
      lstCalculos.ItemData(lstCalculos.NewIndex) = PegaItem(linha, Chr(1), 1)
    Next
  End If
  '
  GetClcs = True
  Exit Function
  '
TrtErrGetClcs:
  GetClcs = False
End Function

Private Sub Form_Resize()
  Frame1.Width = Me.Width - DifWf
  Frame1.Height = Me.Height - DifHf
  Frame2.Width = Me.Width - DifWf
  lstCalculos.Width = Me.Width - DifW
  lstCalculos.Height = Me.Height - DifH
End Sub

Private Sub lstCalculos_DblClick()
  cmdChoice_Click (0)
End Sub
