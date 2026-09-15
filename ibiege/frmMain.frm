VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form frmMain 
   Caption         =   "Definição Eixo"
   ClientHeight    =   9210
   ClientLeft      =   165
   ClientTop       =   450
   ClientWidth     =   11325
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   9210
   ScaleWidth      =   11325
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdBotao 
      Enabled         =   0   'False
      Height          =   375
      Index           =   7
      Left            =   3090
      Picture         =   "frmMain.frx":014A
      Style           =   1  'Graphical
      TabIndex        =   93
      ToolTipText     =   "Executar cálculo da flecha"
      Top             =   60
      Width           =   380
   End
   Begin VB.CommandButton cmdBotao 
      Height          =   380
      Index           =   3
      Left            =   4215
      Picture         =   "frmMain.frx":04C8
      Style           =   1  'Graphical
      TabIndex        =   92
      ToolTipText     =   "Excluir Cálculo"
      Top             =   60
      Visible         =   0   'False
      Width           =   380
   End
   Begin VB.CheckBox Check2 
      Height          =   380
      Left            =   2700
      Picture         =   "frmMain.frx":0834
      Style           =   1  'Graphical
      TabIndex        =   91
      ToolTipText     =   "Gerar Arquivo de Plotagem"
      Top             =   60
      Width           =   380
   End
   Begin VB.CommandButton cmdBotao 
      Height          =   380
      Index           =   6
      Left            =   2310
      Picture         =   "frmMain.frx":097E
      Style           =   1  'Graphical
      TabIndex        =   83
      ToolTipText     =   "Visualizar Gráfico"
      Top             =   60
      Width           =   380
   End
   Begin VB.CommandButton cmdBotao 
      Height          =   380
      Index           =   5
      Left            =   1920
      Picture         =   "frmMain.frx":0D06
      Style           =   1  'Graphical
      TabIndex        =   82
      ToolTipText     =   "Visualizar Arquivo de Saída"
      Top             =   60
      Width           =   380
   End
   Begin VB.CommandButton cmdBotao 
      Height          =   375
      Index           =   4
      Left            =   120
      Picture         =   "frmMain.frx":1093
      Style           =   1  'Graphical
      TabIndex        =   70
      ToolTipText     =   "Cria novo Cálculo"
      Top             =   60
      Width           =   380
   End
   Begin VB.Frame frmCargas 
      Caption         =   "Cargas"
      Height          =   2235
      Left            =   5760
      TabIndex        =   32
      Top             =   6480
      Width           =   5475
      Begin VB.CheckBox Check1 
         Caption         =   "Empuxo Magnético"
         Height          =   255
         Left            =   570
         TabIndex        =   22
         Top             =   1470
         Width           =   1665
      End
      Begin VB.CommandButton cmdGrid 
         Enabled         =   0   'False
         Height          =   345
         Index           =   4
         Left            =   1800
         Picture         =   "frmMain.frx":140E
         Style           =   1  'Graphical
         TabIndex        =   25
         ToolTipText     =   "Alterar"
         Top             =   1785
         Width           =   380
      End
      Begin VB.CommandButton cmdGrid 
         Height          =   345
         Index           =   3
         Left            =   960
         Picture         =   "frmMain.frx":1795
         Style           =   1  'Graphical
         TabIndex        =   23
         ToolTipText     =   "Incluir"
         Top             =   1785
         Width           =   380
      End
      Begin VB.CommandButton cmdGrid 
         Height          =   345
         Index           =   5
         Left            =   1380
         Picture         =   "frmMain.frx":180C
         Style           =   1  'Graphical
         TabIndex        =   24
         ToolTipText     =   "Excluir"
         Top             =   1785
         Width           =   380
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   11
         Left            =   960
         TabIndex        =   21
         ToolTipText     =   "Carga"
         Top             =   1110
         Width           =   1215
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   10
         Left            =   960
         TabIndex        =   20
         ToolTipText     =   "Comprimento da área de aplicação da Carga"
         Top             =   750
         Width           =   1215
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   9
         Left            =   960
         TabIndex        =   19
         ToolTipText     =   "Posição inicial da aplicação da Carga"
         Top             =   390
         Width           =   1215
      End
      Begin MSFlexGridLib.MSFlexGrid grdCargas 
         Height          =   1815
         Left            =   2280
         TabIndex        =   47
         TabStop         =   0   'False
         ToolTipText     =   "Duplo clique define pacote de chapas para o cálculo da flecha"
         Top             =   360
         Width           =   3015
         _ExtentX        =   5318
         _ExtentY        =   3201
         _Version        =   393216
         Cols            =   4
         FixedCols       =   0
         FocusRect       =   0
         AllowUserResizing=   1
      End
      Begin VB.Label Label12 
         AutoSize        =   -1  'True
         Caption         =   "Lista de Cargas"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Left            =   2325
         TabIndex        =   41
         Top             =   150
         Width           =   1290
      End
      Begin VB.Label Label11 
         AutoSize        =   -1  'True
         Caption         =   "Carga (Kg)"
         Height          =   210
         Left            =   120
         TabIndex        =   40
         Top             =   1155
         Width           =   795
      End
      Begin VB.Label Label10 
         AutoSize        =   -1  'True
         Caption         =   "LC"
         Height          =   210
         Left            =   720
         TabIndex        =   39
         Top             =   795
         Width           =   195
      End
      Begin VB.Label Label9 
         AutoSize        =   -1  'True
         Caption         =   "Xi"
         Height          =   210
         Left            =   720
         TabIndex        =   38
         Top             =   435
         Width           =   135
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   " Desenho "
      Height          =   1680
      Left            =   9240
      TabIndex        =   66
      Top             =   4800
      Width           =   1995
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   2
         Left            =   1320
         TabIndex        =   29
         ToolTipText     =   "Fator de Escala"
         Top             =   1320
         Width           =   495
      End
      Begin VB.CommandButton Command2 
         Height          =   380
         Left            =   1320
         Picture         =   "frmMain.frx":187A
         Style           =   1  'Graphical
         TabIndex        =   26
         ToolTipText     =   "Desenha"
         Top             =   180
         Width           =   495
      End
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   0
         Left            =   1320
         TabIndex        =   27
         ToolTipText     =   "Fator de Escala"
         Top             =   600
         Width           =   495
      End
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   1
         Left            =   1320
         TabIndex        =   28
         ToolTipText     =   "Posição X0"
         Top             =   960
         Width           =   495
      End
      Begin VB.Label Label14 
         Caption         =   "Altura da Carga"
         Height          =   255
         Left            =   120
         TabIndex        =   69
         Top             =   1350
         Width           =   1215
      End
      Begin VB.Label Label15 
         Caption         =   "Fator de Escala"
         Height          =   255
         Left            =   120
         TabIndex        =   68
         Top             =   630
         Width           =   1215
      End
      Begin VB.Label Label16 
         Caption         =   "Posição X0"
         Height          =   255
         Left            =   120
         TabIndex        =   67
         Top             =   990
         Width           =   1095
      End
   End
   Begin VB.CommandButton cmdBotao 
      Height          =   380
      Index           =   1
      Left            =   900
      Picture         =   "frmMain.frx":1C0C
      Style           =   1  'Graphical
      TabIndex        =   65
      ToolTipText     =   "Salva Cálculo"
      Top             =   60
      Width           =   380
   End
   Begin VB.CommandButton cmdBotao 
      Height          =   380
      Index           =   2
      Left            =   1530
      Picture         =   "frmMain.frx":1F93
      Style           =   1  'Graphical
      TabIndex        =   64
      ToolTipText     =   "Executa software BIEGE"
      Top             =   60
      Width           =   380
   End
   Begin VB.CommandButton cmdBotao 
      Height          =   380
      Index           =   0
      Left            =   510
      Picture         =   "frmMain.frx":22D5
      Style           =   1  'Graphical
      TabIndex        =   63
      ToolTipText     =   "Busca Cálculo"
      Top             =   60
      Width           =   380
   End
   Begin VB.PictureBox Picture3 
      Align           =   2  'Align Bottom
      BorderStyle     =   0  'None
      Height          =   375
      Left            =   0
      ScaleHeight     =   375
      ScaleWidth      =   11325
      TabIndex        =   57
      Top             =   8835
      Width           =   11325
      Begin VB.Label lblStatus 
         BorderStyle     =   1  'Fixed Single
         Height          =   285
         Left            =   0
         TabIndex        =   58
         Top             =   0
         Width           =   11310
      End
   End
   Begin VB.Timer tmr 
      Left            =   10440
      Top             =   6240
   End
   Begin VB.Frame frmOP 
      Caption         =   "Dados Cálculo"
      Height          =   3975
      Left            =   120
      TabIndex        =   50
      Top             =   600
      Width           =   2655
      Begin VB.TextBox lblDT_EXEC 
         BackColor       =   &H8000000F&
         BorderStyle     =   0  'None
         Height          =   315
         Left            =   1230
         Locked          =   -1  'True
         TabIndex        =   85
         Text            =   "Text2"
         Top             =   3630
         Width           =   1380
      End
      Begin VB.TextBox txtEntrada 
         BackColor       =   &H80000000&
         BorderStyle     =   0  'None
         Enabled         =   0   'False
         Height          =   255
         Index           =   18
         Left            =   960
         MaxLength       =   50
         TabIndex        =   81
         Text            =   "(Empuxo Magnético)"
         ToolTipText     =   "Descrição do Cálculo"
         Top             =   2520
         Visible         =   0   'False
         Width           =   1575
      End
      Begin VB.TextBox txtEntrada 
         BackColor       =   &H80000000&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Index           =   17
         Left            =   1545
         Locked          =   -1  'True
         MaxLength       =   50
         TabIndex        =   80
         TabStop         =   0   'False
         Top             =   3285
         Width           =   735
      End
      Begin VB.TextBox txtEntrada 
         BackColor       =   &H80000000&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Index           =   15
         Left            =   1560
         Locked          =   -1  'True
         MaxLength       =   50
         TabIndex        =   79
         TabStop         =   0   'False
         Top             =   2760
         Width           =   735
      End
      Begin VB.TextBox txtEntrada 
         BackColor       =   &H80000000&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Index           =   16
         Left            =   105
         Locked          =   -1  'True
         MaxLength       =   50
         TabIndex        =   78
         TabStop         =   0   'False
         Top             =   3285
         Width           =   975
      End
      Begin VB.TextBox txtEntrada 
         BackColor       =   &H80000000&
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Index           =   14
         Left            =   120
         Locked          =   -1  'True
         MaxLength       =   50
         TabIndex        =   77
         TabStop         =   0   'False
         Top             =   2760
         Width           =   975
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   0
         Left            =   120
         MaxLength       =   30
         TabIndex        =   0
         ToolTipText     =   "Código Alfa-Numérico de Referência"
         Top             =   480
         Width           =   2415
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   1
         Left            =   120
         MaxLength       =   50
         TabIndex        =   1
         ToolTipText     =   "Descrição do Cálculo"
         Top             =   975
         Width           =   2415
      End
      Begin VB.Label Label23 
         Caption         =   "Data execução:"
         ForeColor       =   &H00800000&
         Height          =   255
         Left            =   75
         TabIndex        =   84
         Top             =   3645
         Width           =   1215
      End
      Begin VB.Line Line7 
         BorderColor     =   &H80000005&
         X1              =   10
         X2              =   2625
         Y1              =   2485
         Y2              =   2485
      End
      Begin VB.Line Line6 
         BorderColor     =   &H80000003&
         X1              =   -15
         X2              =   2625
         Y1              =   2475
         Y2              =   2475
      End
      Begin VB.Label Label22 
         AutoSize        =   -1  'True
         Caption         =   "Hz"
         Height          =   210
         Left            =   2310
         TabIndex        =   76
         Top             =   3315
         Width           =   195
      End
      Begin VB.Label Label21 
         AutoSize        =   -1  'True
         Caption         =   "RPM"
         Height          =   210
         Left            =   1110
         TabIndex        =   75
         Top             =   3315
         Width           =   315
      End
      Begin VB.Label Label20 
         AutoSize        =   -1  'True
         Caption         =   "Rot. Critica"
         Height          =   210
         Left            =   120
         TabIndex        =   74
         Top             =   3090
         Width           =   780
      End
      Begin VB.Label Label19 
         AutoSize        =   -1  'True
         Caption         =   "Hz"
         Height          =   210
         Left            =   2325
         TabIndex        =   73
         Top             =   2790
         Width           =   195
      End
      Begin VB.Label Label18 
         AutoSize        =   -1  'True
         Caption         =   "RPM"
         Height          =   210
         Left            =   1125
         TabIndex        =   72
         Top             =   2790
         Width           =   315
      End
      Begin VB.Label Label17 
         AutoSize        =   -1  'True
         Caption         =   "Rot. Critica"
         Height          =   210
         Left            =   120
         TabIndex        =   71
         Top             =   2535
         Width           =   780
      End
      Begin VB.Label lblData 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "data"
         Height          =   285
         Left            =   120
         TabIndex        =   56
         Top             =   2055
         Width           =   2415
      End
      Begin VB.Label Label8 
         AutoSize        =   -1  'True
         Caption         =   "Data"
         Height          =   210
         Left            =   120
         TabIndex        =   55
         Top             =   1860
         Width           =   330
      End
      Begin VB.Label lblUsuario 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "usuário"
         Height          =   285
         Left            =   120
         TabIndex        =   54
         Top             =   1530
         Width           =   2415
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         Caption         =   "Usuário"
         Height          =   210
         Left            =   120
         TabIndex        =   53
         Top             =   1320
         Width           =   555
      End
      Begin VB.Label Label6 
         AutoSize        =   -1  'True
         Caption         =   "Descrição"
         Height          =   210
         Left            =   120
         TabIndex        =   52
         Top             =   795
         Width           =   735
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "REFERÊNCIA"
         Height          =   210
         Left            =   120
         TabIndex        =   51
         Top             =   240
         Width           =   930
      End
   End
   Begin VB.Frame frmMancais 
      Caption         =   "Mancais"
      Height          =   1680
      Left            =   5760
      TabIndex        =   43
      Top             =   4800
      Width           =   3465
      Begin VB.ComboBox cmbEntrada 
         Height          =   330
         Index           =   1
         ItemData        =   "frmMain.frx":2657
         Left            =   840
         List            =   "frmMain.frx":2659
         TabIndex        =   18
         ToolTipText     =   "Constante do 2º Mancal"
         Top             =   960
         Width           =   2535
      End
      Begin VB.TextBox txtEntrada 
         Height          =   285
         Index           =   13
         Left            =   120
         TabIndex        =   17
         ToolTipText     =   "Posição do 1º Mancal"
         Top             =   990
         Width           =   645
      End
      Begin VB.ComboBox cmbEntrada 
         Height          =   330
         Index           =   0
         ItemData        =   "frmMain.frx":265B
         Left            =   840
         List            =   "frmMain.frx":265D
         TabIndex        =   16
         ToolTipText     =   "Constante do 1º Mancal"
         Top             =   600
         Width           =   2535
      End
      Begin VB.TextBox txtEntrada 
         Height          =   285
         Index           =   12
         Left            =   120
         TabIndex        =   15
         ToolTipText     =   "Posição do 1º Mancal"
         Top             =   630
         Width           =   645
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "K (10e9 N/m)"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Left            =   840
         TabIndex        =   45
         Top             =   360
         Width           =   1005
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Pos (X)"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Left            =   120
         TabIndex        =   44
         Top             =   345
         Width           =   585
      End
   End
   Begin VB.Frame frmDimensoes 
      Caption         =   "Dimensões"
      Height          =   3915
      Left            =   120
      TabIndex        =   31
      Top             =   4800
      Width           =   5535
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   20
         Left            =   1250
         TabIndex        =   4
         ToolTipText     =   "Diâmetro Final do Segmento de Eixo"
         Top             =   600
         Visible         =   0   'False
         Width           =   735
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   19
         Left            =   2805
         TabIndex        =   11
         ToolTipText     =   "Diâmetro do Furo"
         Top             =   600
         Visible         =   0   'False
         Width           =   735
      End
      Begin VB.ComboBox cmbEntrada 
         Height          =   330
         Index           =   2
         ItemData        =   "frmMain.frx":265F
         Left            =   480
         List            =   "frmMain.frx":2661
         TabIndex        =   5
         ToolTipText     =   "Constante do 1º Mancal"
         Top             =   1080
         Width           =   1455
      End
      Begin VB.CommandButton cmdGrid 
         Enabled         =   0   'False
         Height          =   380
         Index           =   1
         Left            =   4920
         Picture         =   "frmMain.frx":2663
         Style           =   1  'Graphical
         TabIndex        =   13
         ToolTipText     =   "Alterar"
         Top             =   1230
         Width           =   380
      End
      Begin VB.CommandButton cmdGrid 
         Height          =   380
         Index           =   0
         Left            =   4095
         Picture         =   "frmMain.frx":29EA
         Style           =   1  'Graphical
         TabIndex        =   12
         ToolTipText     =   "Incluir"
         Top             =   1230
         Width           =   380
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   7
         Left            =   4545
         TabIndex        =   9
         ToolTipText     =   "Largura do Martelo"
         Top             =   600
         Visible         =   0   'False
         Width           =   735
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   8
         Left            =   2805
         TabIndex        =   10
         ToolTipText     =   "Número de Costelas"
         Top             =   960
         Visible         =   0   'False
         Width           =   735
      End
      Begin MSFlexGridLib.MSFlexGrid grdDimensoes 
         Height          =   2175
         Left            =   180
         TabIndex        =   46
         TabStop         =   0   'False
         Top             =   1680
         Width           =   5175
         _ExtentX        =   9128
         _ExtentY        =   3836
         _Version        =   393216
         Cols            =   9
         FixedCols       =   0
         FocusRect       =   0
         SelectionMode   =   1
         AllowUserResizing=   1
         FormatString    =   "^L           |^D           |^DPCT    |^A         |^B          |^C          |^NR_COST|^D_INT|^D_F"
      End
      Begin VB.CommandButton cmdGrid 
         Height          =   380
         Index           =   2
         Left            =   4500
         Picture         =   "frmMain.frx":2A61
         Style           =   1  'Graphical
         TabIndex        =   14
         ToolTipText     =   "Excluir"
         Top             =   1230
         Width           =   380
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   6
         Left            =   4545
         TabIndex        =   8
         ToolTipText     =   "Espessura/Altura do Martelo"
         Top             =   240
         Visible         =   0   'False
         Width           =   735
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   5
         Left            =   2805
         TabIndex        =   7
         ToolTipText     =   "Espessura da Costela"
         Top             =   600
         Visible         =   0   'False
         Width           =   735
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   4
         Left            =   2805
         TabIndex        =   6
         ToolTipText     =   "Diâmetro do Pacote"
         Top             =   240
         Visible         =   0   'False
         Width           =   735
      End
      Begin VB.CheckBox chkConico 
         Caption         =   "Cônico"
         Height          =   285
         Left            =   1250
         TabIndex        =   86
         Top             =   255
         Width           =   855
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   3
         Left            =   480
         TabIndex        =   3
         ToolTipText     =   "Diâmetro do segmento de eixo"
         Top             =   600
         Width           =   735
      End
      Begin VB.TextBox txtEntrada 
         Height          =   315
         Index           =   2
         Left            =   480
         TabIndex        =   2
         ToolTipText     =   "Comprimento do segmento de eixo"
         Top             =   240
         Width           =   735
      End
      Begin VB.Label Label25 
         AutoSize        =   -1  'True
         Caption         =   "Diam. Int."
         Height          =   210
         Left            =   2100
         TabIndex        =   88
         Top             =   645
         Visible         =   0   'False
         Width           =   645
      End
      Begin VB.Label Label24 
         AutoSize        =   -1  'True
         Caption         =   "Tipo"
         Height          =   210
         Left            =   120
         TabIndex        =   87
         Top             =   1125
         Width           =   300
      End
      Begin VB.Label lblC 
         AutoSize        =   -1  'True
         Caption         =   "LM"
         Height          =   210
         Left            =   4200
         TabIndex        =   49
         Top             =   652
         Visible         =   0   'False
         Width           =   210
      End
      Begin VB.Label lblNR 
         AutoSize        =   -1  'True
         Caption         =   "Nº Cost."
         Height          =   210
         Left            =   2100
         TabIndex        =   48
         Top             =   1012
         Visible         =   0   'False
         Width           =   600
      End
      Begin VB.Label Label13 
         AutoSize        =   -1  'True
         Caption         =   "Segmentos do Eixo"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Left            =   255
         TabIndex        =   42
         Top             =   1440
         Width           =   1590
      End
      Begin VB.Label lblB 
         AutoSize        =   -1  'True
         Caption         =   "EM"
         Height          =   210
         Left            =   4200
         TabIndex        =   37
         Top             =   292
         Visible         =   0   'False
         Width           =   210
      End
      Begin VB.Label lblA 
         AutoSize        =   -1  'True
         Caption         =   "EC"
         Height          =   210
         Left            =   2085
         TabIndex        =   36
         Top             =   652
         Visible         =   0   'False
         Width           =   195
      End
      Begin VB.Label lblDC 
         AutoSize        =   -1  'True
         Caption         =   "D. pct"
         Height          =   210
         Left            =   2100
         TabIndex        =   35
         Top             =   292
         Visible         =   0   'False
         Width           =   420
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "D"
         Height          =   210
         Left            =   300
         TabIndex        =   34
         Top             =   652
         Width           =   105
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "L"
         Height          =   210
         Left            =   300
         TabIndex        =   33
         Top             =   292
         Width           =   90
      End
   End
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00FFFFFF&
      Height          =   3855
      Left            =   2880
      ScaleHeight     =   3795
      ScaleWidth      =   8235
      TabIndex        =   30
      Top             =   600
      Width           =   8295
      Begin VB.CommandButton cmdImg 
         Height          =   345
         Index           =   3
         Left            =   7860
         Picture         =   "frmMain.frx":2ACF
         Style           =   1  'Graphical
         TabIndex        =   62
         Top             =   3450
         Width           =   390
      End
      Begin VB.CommandButton cmdImg 
         Height          =   345
         Index           =   2
         Left            =   7455
         Picture         =   "frmMain.frx":2E42
         Style           =   1  'Graphical
         TabIndex        =   61
         Top             =   3450
         Width           =   390
      End
      Begin VB.CommandButton cmdImg 
         Height          =   345
         Index           =   1
         Left            =   0
         Picture         =   "frmMain.frx":31B4
         Style           =   1  'Graphical
         TabIndex        =   60
         Top             =   3465
         Width           =   390
      End
      Begin VB.CommandButton cmdImg 
         Height          =   345
         Index           =   0
         Left            =   0
         Picture         =   "frmMain.frx":356E
         Style           =   1  'Graphical
         TabIndex        =   59
         Top             =   3120
         Width           =   390
      End
   End
   Begin VB.Label lblComp 
      AutoSize        =   -1  'True
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   210
      Index           =   1
      Left            =   5040
      TabIndex        =   90
      Top             =   4560
      Width           =   90
   End
   Begin VB.Label lblComp 
      AutoSize        =   -1  'True
      Caption         =   "COMPRIMENTO TOTAL EIXO: "
      Height          =   210
      Index           =   0
      Left            =   2880
      TabIndex        =   89
      Top             =   4560
      Width           =   2130
   End
   Begin VB.Line Line5 
      BorderColor     =   &H00808080&
      X1              =   11280
      X2              =   11280
      Y1              =   0
      Y2              =   480
   End
   Begin VB.Line Line4 
      BorderColor     =   &H00FFFFFF&
      X1              =   0
      X2              =   11280
      Y1              =   0
      Y2              =   0
   End
   Begin VB.Line Line3 
      BorderColor     =   &H00FFFFFF&
      X1              =   0
      X2              =   0
      Y1              =   0
      Y2              =   480
   End
   Begin VB.Line Line2 
      X1              =   0
      X2              =   3000
      Y1              =   0
      Y2              =   0
   End
   Begin VB.Line Line1 
      BorderColor     =   &H00808080&
      X1              =   0
      X2              =   11280
      Y1              =   480
      Y2              =   480
   End
   Begin VB.Menu mnuArquivo 
      Caption         =   "&Arquivo"
      Index           =   0
      Begin VB.Menu mnuNovo 
         Caption         =   "&Novo"
         Index           =   1
      End
      Begin VB.Menu mnuAAbrir 
         Caption         =   "A&brir"
         Index           =   2
      End
      Begin VB.Menu mnuSalvar 
         Caption         =   "&Salvar"
         Index           =   3
      End
      Begin VB.Menu mnuSair 
         Caption         =   "Sai&r"
         Index           =   4
      End
   End
   Begin VB.Menu mnuOpcoes 
      Caption         =   "&Opções"
      Index           =   5
      Begin VB.Menu mnuGerar 
         Caption         =   "&Gerar"
         Index           =   6
      End
      Begin VB.Menu mnuCopiar 
         Caption         =   "&Copiar Imagem"
         Index           =   7
      End
   End
   Begin VB.Menu mnuRightBtn 
      Caption         =   ""
      Index           =   8
      Visible         =   0   'False
      Begin VB.Menu mnuCopiar2 
         Caption         =   "Copiar Imagem"
         Index           =   9
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim adiciona_grd1 As Boolean, adiciona_grd2 As Boolean
Dim Largura As Double, Altura As Double
Dim constante(1) As String
Dim ID_CALC As Long
Dim biege As String
Dim file_in_biege As String
Dim file_out_biege As String
Dim file_empuxo As String
Dim command_line As String
Dim h_carga As Double
Dim fator_escala As Double, x_inicial As Double
Dim NrMesIni As Integer
Dim bChanged As Boolean

Private Sub Check1_KeyDown(KeyCode As Integer, Shift As Integer)
  If (KeyCode = 13) Then
    Call cmdGrid_Click(3)
  End If
End Sub

Private Sub mostra_componentes_costelado(ByVal visibilidade As Boolean)
  txtEntrada(4).Visible = visibilidade
  txtEntrada(5).Visible = visibilidade
  txtEntrada(6).Visible = visibilidade
  txtEntrada(8).Visible = visibilidade
  txtEntrada(7).Visible = visibilidade
  lblDC.Visible = visibilidade
  lblA.Visible = visibilidade
  lblB.Visible = visibilidade
  lblNR.Visible = visibilidade
  lblC.Visible = visibilidade
End Sub

Private Sub mostra_componentes_oco(ByVal visibilidade As Boolean)
  txtEntrada(19).Visible = visibilidade
  Label25.Visible = visibilidade
End Sub

Private Sub mostra_componentes_conico(ByVal visibilidade As Boolean)
  txtEntrada(20).Visible = visibilidade
End Sub

Private Sub chkConico_Click()
  If (chkConico.Value = 1) Then
    Call mostra_componentes_conico(True)
  Else
    Call mostra_componentes_conico(False)
  End If
End Sub

Private Sub cmbEntrada_Click(Index As Integer)
  Select Case Index
    Case 2
      Call mostra_componentes_costelado(False)
      Call mostra_componentes_oco(False)
      If (cmbEntrada(Index).ListIndex = ind_cost) Then
        Call mostra_componentes_costelado(True)
      ElseIf (cmbEntrada(Index).ListIndex = ind_oco) Then
        Call mostra_componentes_oco(True)
      End If
    Case Else
      If (cmbEntrada(Index).ItemData(cmbEntrada(Index).ListIndex) <> 0) Then
        If (cmbEntrada(Index).ItemData(cmbEntrada(Index).ListIndex) <> -1) Then
          constante(Index) = cmbEntrada(Index).ItemData(cmbEntrada(Index).ListIndex)
        Else
          constante(Index) = ""
        End If
      Else
        constante(Index) = "Infinito"
      End If
  End Select
  tmr.Interval = 10
  If (Index = 0) Then constante(1) = constante(0)
  bChanged = True
End Sub

Private Sub abre_calculo()
  Dim err_m As String, result As String, linha As String, temp As String
  Dim nrlinhas As Integer, nrlinhas2 As Integer
    
  'busca informações de cálculo
  temp = txtEntrada(0).Text
  result = busca_dados_calc(temp)
  ID_CALC = Val(PegaItem(result, Chr(1), 1))
  If (ID_CALC > 0) Then
    tem_costelado = False
    tem_oco = False
    tem_con = False
    'limpa_tela
    txtEntrada(14).Text = ""
    txtEntrada(15).Text = ""
    txtEntrada(16).Text = ""
    txtEntrada(17).Text = ""
    lblDT_EXEC.Text = ""
    txtEntrada(1).Text = PegaItem(result, Chr(1), 2)
    lblUsuario.Caption = PegaItem(result, Chr(1), 3)
    lblData.Caption = PegaItem(result, Chr(1), 4)
    'busca dimensoes do eixo
    If RecGrd(ID_CALC, grdDimensoes, "L,D,DPCT,A,B,C,NR_COST,D_INT,D_F", nrlinhas, err_m) Then
      If (nrlinhas > 0) Then adiciona_grd1 = True
      grdDimensoes.Row = grdDimensoes.Rows - 1
      'atualiza comprimento eixo
      lblComp(1).Caption = sum_sgmnts
      'busca cargas aplicadas ao eixo
      If RecGrd(ID_CALC, grdCargas, "Xi,LC,KG,Empuxo", nrlinhas2, err_m) Then
        If (nrlinhas2 > 0) Then adiciona_grd2 = True
        grdCargas.Row = grdCargas.Rows - 1
        'busca mancais
        result = busca_mancais(ID_CALC)
        
        linha = PegaItem(result, Chr(0), 1)
        txtEntrada(12).Text = PegaItem(linha, Chr(1), 1)
        linha = PegaItem(result, Chr(0), 2)
        cmbEntrada(0).Text = Left(PegaItem(linha, Chr(1), 1), 1) 'Format(PegaItem(linha, Chr(1), 1), "0.00e+000")
        constante(0) = cmbEntrada(0).Text
        If (Val(constante(0)) = 0) Then constante(0) = "Infinito"
        linha = PegaItem(result, Chr(0), 3)
        txtEntrada(13).Text = PegaItem(linha, Chr(1), 1)
        linha = PegaItem(result, Chr(0), 4)
        cmbEntrada(1).Text = Left(PegaItem(linha, Chr(1), 1), 1) 'Format(PegaItem(linha, Chr(1), 1), "0.00e+000")
        constante(1) = cmbEntrada(1).Text
        If (Val(constante(1)) = 0) Then constante(1) = "Infinito"
        tmr.Interval = 10
        If (nrlinhas > 0) Then
          Command2_Click
        End If
        bChanged = False
      Else
        MsgBox "Erro -> " & err_m, vbCritical, "Atenção!"
      End If
    Else
      MsgBox "Erro -> " & err_m, vbCritical, "Atenção!"
    End If
  Else
  'limpa_tela
  txtEntrada(0).Text = temp
  End If
End Sub

Private Sub AltCrg()
  If Not sobrepoecarga(txtEntrada(9).Text, txtEntrada(10).Text, grdCargas.Row) Then
    If Not ultrapassaescalonamento(txtEntrada(9).Text, txtEntrada(10).Text) Then
      grdCargas.TextMatrix(grdCargas.Row, 0) = txtEntrada(9).Text
      grdCargas.TextMatrix(grdCargas.Row, 1) = txtEntrada(10).Text
      grdCargas.TextMatrix(grdCargas.Row, 2) = txtEntrada(11).Text
      grdCargas.TextMatrix(grdCargas.Row, 3) = Check1.Value
      Call grdCargas_DblClick
    End If
  End If
End Sub

Private Sub AltSgmnt()
  If (grdDimensoes.TextMatrix(grdDimensoes.Row, 0) = "") Then Exit Sub
  grdDimensoes.TextMatrix(grdDimensoes.Row, 2) = ""
  grdDimensoes.TextMatrix(grdDimensoes.Row, 3) = ""
  grdDimensoes.TextMatrix(grdDimensoes.Row, 4) = ""
  grdDimensoes.TextMatrix(grdDimensoes.Row, 5) = ""
  grdDimensoes.TextMatrix(grdDimensoes.Row, 6) = ""
  grdDimensoes.TextMatrix(grdDimensoes.Row, 7) = ""
  grdDimensoes.TextMatrix(grdDimensoes.Row, 8) = ""
  If Not ha_costelado(grdDimensoes.Row) Then tem_costelado = False
  If Not ha_oco(grdDimensoes.Row) Then tem_oco = False
  
    
  If (cmbEntrada(2).ListIndex = ind_cost) Then
    If ha_oco Then MsgBox "Não é possível definir um segmento de eixo costelado pois já existe ao menos um segmento do tipo oco!", vbCritical, "Atenção!": Exit Sub
    grdDimensoes.TextMatrix(grdDimensoes.Row, 2) = txtEntrada(4).Text
    grdDimensoes.TextMatrix(grdDimensoes.Row, 3) = txtEntrada(5).Text
    grdDimensoes.TextMatrix(grdDimensoes.Row, 4) = txtEntrada(6).Text
    grdDimensoes.TextMatrix(grdDimensoes.Row, 5) = txtEntrada(7).Text
    grdDimensoes.TextMatrix(grdDimensoes.Row, 6) = txtEntrada(8).Text
    tem_costelado = True
  ElseIf (cmbEntrada(2).ListIndex = ind_oco) Then
    If ha_costelado Then MsgBox "Não é possível definir um segmento de eixo oco pois já existe ao menos um segmento do tipo costelado!", vbCritical, "Atenção!": Exit Sub
    grdDimensoes.TextMatrix(grdDimensoes.Row, 7) = txtEntrada(19)
    tem_oco = True
  End If
  
  lblComp(1).Caption = CStr(CDbl(lblComp(1).Caption) - CDbl(grdDimensoes.TextMatrix(grdDimensoes.Row, 0)))
  
  grdDimensoes.TextMatrix(grdDimensoes.Row, 0) = txtEntrada(2).Text
  grdDimensoes.TextMatrix(grdDimensoes.Row, 1) = txtEntrada(3).Text
  If (chkConico.Value = 1) Then
    If (Val(txtEntrada(20).Text) <> Val(txtEntrada(3).Text)) Then
      grdDimensoes.TextMatrix(grdDimensoes.Row, 8) = txtEntrada(20).Text
    Else
      chkConico.Value = 0
    End If
  End If
  'atualiza comprimento total
  lblComp(1).Caption = CStr(CDbl(lblComp(1).Caption) + CDbl(txtEntrada(2).Text))
  
End Sub

Private Sub cmbEntrada_KeyDown(Index As Integer, KeyCode As Integer, Shift As Integer)
  If (Index = 2) Then
    If (KeyCode = 13) Then
      Call cmdGrid_Click(0)
    End If
  End If
End Sub

Private Sub cmdBotao_Click(Index As Integer)
  Dim ErrMsg As String
  Select Case Index
    Case 0
      If bChanged Then
        Select Case MsgBox("Deseja salvar alterações??", vbYesNoCancel + vbQuestion, "Atenção!")
          Case vbYes
            Call cmdBotao_Click(3)
            bChanged = False
          Case vbCancel
            Exit Sub
        End Select
      End If
      Call mostra_lista_calculos
      If (txtEntrada(0).Text <> "") Then
        Call abre_calculo
        Call EnSc(Me, True)
      End If
    Case 1
      Call EnSc(Me, False)
      Call salva_calculo
      Call EnSc(Me, True)
      bChanged = False
    Case 2
      Call efetua_calculo
      Call EnSc(Me, True)
    Case 3
      If (ID_CALC > 0) Then
       If (MsgBox("Confirma exclusão de Cálculo " & txtEntrada(0).Text & " ?", vbQuestion + vbYesNoCancel, "Atenção!") = vbYes) Then
         If Not Delete(ID_CALC, "", True, True, ErrMsg) Then
           MsgBox "Erro ao tentar excluir o cálculo: " & vbCr & _
                  ErrMsg, vbCritical, "Erro"
         Else
           Call limpa_tela
         End If
       End If
       Call EnSc(Me, True)
      End If
    Case 4
      Call mnuNovo_Click(1)
    Case 5
      If (Dir(file_out_biege) <> "") Then
        Dim frms As Object
        Set frms = New frmSaida
        frms.inicializa file_out_biege
        Set frms = Nothing
      Else
        MsgBox "Não foi possível encontrar arquivo de saída do BIEGE!" & vbCr & vbCr & file_out_biege, vbCritical, "Atenção!"
      End If
    Case 6
      Call ExecCmdLn(command_line)
    Case 7
      Call ClcFle
  End Select
End Sub

Private Sub ClcFle()
  grdCargas.Col = 0
  grdCargas.Sort = 1
  frmFlexa.Show
End Sub

Private Sub ExecCmdLn(cmd_ln As String)
  On Error GoTo Fim
  Shell cmd_ln
  Exit Sub
Fim:
  MsgBox "Erro ao tentar executar comando configurado no arquivo 'iBiege.ini', favor verificar arquivo!", vbCritical, "Atenção!"
End Sub

Private Sub salva_calculo()
  Dim err_m As String
  If (txtEntrada(0).Text <> "") Then
    If Not Salva(txtEntrada(0).Text, txtEntrada(1).Text, ID_CALC, err_m) Then
      MsgBox "Erro -> " & err_m, vbCritical, "Atenção!"
    Else
      MsgBox "Cálculo Armazenado com Sucesso!", vbInformation, "OK!"
    End If
  Else
    MsgBox "Informe um código de Referência", vbCritical, "Atenção!"
  End If
End Sub

Private Sub mostra_lista_calculos()
  Dim Frm As Object
  Set Frm = New frmCalculos
  frmCalculos.Show vbModal
  Set Frm = Nothing
End Sub

Private Sub ExcCrg()
  If (grdCargas.Row > 0) Then
    If (grdCargas.TextMatrix(grdCargas.Row, 0) <> "") Then
      If (MsgBox("Confirma Exclusão de carga?", vbQuestion + vbYesNo, "Atenção!") = vbYes) Then
        If (grdCargas.Rows > 2) Then
          grdCargas.RemoveItem grdCargas.Row
        Else
          grdCargas.TextMatrix(1, 0) = ""
          grdCargas.TextMatrix(1, 1) = ""
          grdCargas.TextMatrix(1, 2) = ""
          adiciona_grd2 = False
        End If
      End If
    End If
  End If
End Sub

Private Sub ExcSgmnt()
  If (grdDimensoes.Row > 0) Then
    If (grdDimensoes.TextMatrix(grdDimensoes.Row, 0) = "") Then Exit Sub
    If (MsgBox("Confirma Exclusão de segmento?", vbQuestion + vbYesNo, "Atenção!") = vbYes) Then
      'se for costelado, verifica se não há mais segmentos de eixo costelado e seta flags
      If (grdDimensoes.TextMatrix(grdDimensoes.Row, 3) <> "") Then
        If Not ha_costelado(grdDimensoes.Row) Then
          tem_costelado = False
          'linha_costelado = 0
        End If
      End If
      'se for oco, verifica se não há mais segmentos de eixo oco e seta flags
      If (grdDimensoes.TextMatrix(grdDimensoes.Row, 7) <> "") Then
        If Not ha_oco(grdDimensoes.Row) Then
          tem_oco = False
          'linha_oco = 0
        End If
      End If
      'se for cônico, verifica se não há mais segmentos de eixo cônico e seta flags
      If (grdDimensoes.TextMatrix(grdDimensoes.Row, 8) <> "") Then
        If Not ha_conico(grdDimensoes.Row) Then
          tem_con = False
          'linha_con = 0
        End If
      End If
      'atualiza comprimento total
      lblComp(1).Caption = CStr(CDbl(lblComp(1).Caption) - CDbl(grdDimensoes.TextMatrix(grdDimensoes.Row, 0)))
      If (grdDimensoes.Rows > 2) Then
        grdDimensoes.RemoveItem grdDimensoes.Row
      Else
        grdDimensoes.TextMatrix(1, 0) = ""
        grdDimensoes.TextMatrix(1, 1) = ""
        grdDimensoes.TextMatrix(1, 2) = ""
        grdDimensoes.TextMatrix(1, 3) = ""
        grdDimensoes.TextMatrix(1, 4) = ""
        grdDimensoes.TextMatrix(1, 5) = ""
        grdDimensoes.TextMatrix(1, 6) = ""
        grdDimensoes.TextMatrix(1, 7) = ""
        grdDimensoes.TextMatrix(1, 8) = ""
        adiciona_grd1 = False
      End If
    End If
  End If
End Sub

Private Sub efetua_calculo()
  '
  txtEntrada(14).Text = ""
  txtEntrada(15).Text = ""
  txtEntrada(16).Text = ""
  txtEntrada(17).Text = ""
  If (grdDimensoes.TextMatrix(1, 0) = "") Then Exit Sub
  'verifica se arquivo de empuxo existe
  If ha_cargas_com_empuxo_magnetico Then
      If (Dir(file_empuxo) = "") Then MsgBox "Arquivo de empuxo magnético não localizado, favor executar programa FdeMIT", vbCritical, "Atenção!": Exit Sub
  End If
  
  'ordena cargas
  grdCargas.Col = 0
  grdCargas.ColSel = 0
  grdCargas.Sort = 1
      
  exporta_biege file_in_biege
  
  ExecCmd biege & " " & file_in_biege & " " & file_out_biege, 60000, False
  
  'Se Biege não gerou saída informa erro
  If (Dir(file_out_biege) = "") Then
     MsgBox "Erro ao executar software Biege " & vbCr & vbCr & " Arquivo de saída não localizado:(" & file_out_biege & ")", vbCritical, "Atenção!"
     Exit Sub
  Else
    Dim retorno As Double, retorno2 As Double
    Dim frms As Object
    Dim sTmp As String
    
    'pega rotação crítica com empuxo magnético
    retorno = le_NKRIT(file_out_biege, True)
    
    'pega rotação crítica sem empuxo magnético
    retorno2 = le_NKRIT(file_out_biege, False)
    
    If (retorno2 > 0) Then
      txtEntrada(14).Text = CStr(retorno)
      txtEntrada(15).Text = Format(CStr(retorno / 60), "0.00")
      txtEntrada(16).Text = CStr(retorno2)
      txtEntrada(17).Text = Format(CStr(retorno2 / 60), "0.00")
      txtEntrada(18).Visible = True
      'sTmp = "Rotação Crítica (Empuxo Magnético): " & CStr(retorno) & " RPM " & " Frequência : " & Format(CStr(retorno / 60), "0.00") & " Hz"
      'sTmp = sTmp & vbCr & "Rotação Crítica : " & CStr(retorno2) & " RPM " & " Frequência : " & Format(CStr(retorno2 / 60), "0.00") & " Hz"
    Else
      'sTmp = "Rotação Crítica : " & CStr(retorno) & " RPM " & " Frequência : " & Format(CStr(retorno / 60), "0.00") & " Hz"
      txtEntrada(14).Text = CStr(retorno)
      txtEntrada(15).Text = Format(CStr(retorno / 60), "0.00")
      txtEntrada(18).Visible = False
      lblStatus.Caption = "Cálculo Efetuado"
    End If
    lblDT_EXEC.Text = Format(FileDateTime(file_out_biege), "dd/mm/yy hh:mm:ss")
  End If
End Sub

Private Function ha_cargas_com_empuxo_magnetico() As Boolean
  Dim i As Integer
  Dim bTmp As Boolean
  bTmp = False
  For i = 1 To grdCargas.Rows - 1
    If (Val(grdCargas.TextMatrix(i, 3)) = 1) Then bTmp = True: Exit For
  Next
  ha_cargas_com_empuxo_magnetico = bTmp
End Function

Private Function le_NKRIT(file As String, emp_magn As Boolean) As Double
  Dim lin As String, aux As Double, flag As Boolean
  
  Open file For Input As #1
  aux = 0
  flag = False
  Do While Not EOF(1)
    Line Input #1, lin
    If (Trim(PegaItem(lin, "=", 1)) = "NKRIT") Then
      aux = Val(PegaItem(Trim(PegaItem(lin, "=", 2)), " ", 1))
      If ((emp_magn) Or (Not (emp_magn) And flag)) Then Exit Do
      flag = True
      aux = 0
    End If
  Loop
  Close #1
  le_NKRIT = aux
End Function

Private Sub cmdGrid_Click(Index As Integer)
  Select Case Index
    Case 0
      IncSgmnt
    Case 1
      AltSgmnt
    Case 2
      ExcSgmnt
    Case 3
      IncCrg
    Case 4
      AltCrg
    Case 5
      ExcCrg
  End Select
  If (grdDimensoes.TextMatrix(1, 0) <> "") Then
    Command2_Click
  End If
  bChanged = True
End Sub

Private Sub cmdImg_Click(Index As Integer)
  Select Case Index
  Case 0
    Text1(0).Text = Round(Val(Text1(0).Text) * 1.1, 2)
  Case 1
    Text1(0).Text = Round(Val(Text1(0).Text) / 1.1, 2)
  Case 2
    Text1(1).Text = Val(Text1(1).Text) + 500
  Case 3
    Text1(1).Text = Val(Text1(1).Text) - 500
  End Select
  Command2_Click
End Sub

Private Sub IncCrg()
  Dim linha As Integer
  If (grdDimensoes.TextMatrix(1, 0) <> "") Then
    If ((txtEntrada(9).Text <> "") And (txtEntrada(10).Text <> "") And (txtEntrada(11).Text <> "")) Then
      If Not sobrepoecarga(txtEntrada(9).Text, txtEntrada(10).Text, -1) Then
        If Not ultrapassaescalonamento(txtEntrada(9).Text, txtEntrada(10).Text) Then
          If adiciona_grd2 Then
            linha = grdCargas.Row + 1
            grdCargas.AddItem "", linha
          Else
            linha = 1
            adiciona_grd2 = True
          End If
          grdCargas.TextMatrix(linha, 0) = txtEntrada(9).Text
          grdCargas.TextMatrix(linha, 1) = txtEntrada(10).Text
          grdCargas.TextMatrix(linha, 2) = txtEntrada(11).Text
          grdCargas.TextMatrix(linha, 3) = Check1.Value
        End If
        grdCargas.Row = linha
        Call grdCargas_DblClick
      End If
    Else
      MsgBox "Informe os valores para a inserção do segmento!", vbCritical, "Atenção!"
    End If
  Else
    MsgBox "Inclua Segmentos de Eixo!", vbCritical, "Atenção!"
  End If
  txtEntrada(9).SetFocus
End Sub

Private Sub IncSgmnt()
  Dim linha As Integer
  If ((txtEntrada(2).Text <> "") And (txtEntrada(3).Text <> "")) Then
    If adiciona_grd1 Then
      linha = grdDimensoes.Row + 1
      grdDimensoes.AddItem "", linha
    Else
      linha = 1
      adiciona_grd1 = True
    End If
    grdDimensoes.TextMatrix(linha, 0) = txtEntrada(2).Text
    grdDimensoes.TextMatrix(linha, 1) = txtEntrada(3).Text
    If (chkConico.Value = 1) Then grdDimensoes.TextMatrix(linha, 8) = txtEntrada(20).Text
    
    'só pode incluir oco, se não tiver costelado e vice-versa
    If (cmbEntrada(2).ListIndex = ind_cost) Then
      If ha_oco Then MsgBox "Não é possível definir um segmento de eixo costelado pois já existe ao menos um segmento do tipo oco!", vbCritical, "Atenção!": Exit Sub
      tem_costelado = True
      grdDimensoes.TextMatrix(linha, 2) = txtEntrada(4).Text
      grdDimensoes.TextMatrix(linha, 3) = txtEntrada(5).Text
      grdDimensoes.TextMatrix(linha, 4) = txtEntrada(6).Text
      grdDimensoes.TextMatrix(linha, 5) = txtEntrada(7).Text
      grdDimensoes.TextMatrix(linha, 6) = txtEntrada(8).Text
    ElseIf (cmbEntrada(2).ListIndex = ind_oco) Then
      If ha_costelado Then MsgBox "Não é possível definir um segmento de eixo oco pois já existe ao menos um segmento do tipo costelado!", vbCritical, "Atenção!": Exit Sub
      tem_oco = True
      grdDimensoes.TextMatrix(linha, 7) = txtEntrada(19).Text
    End If
    grdDimensoes.Row = linha
    'Atualiza comprimento total
    lblComp(1).Caption = CStr(Val(lblComp(1).Caption) + Val(txtEntrada(2).Text))
  Else
    MsgBox "Informe os valores para a inserção do segmento!", vbCritical, "Atenção!": Exit Sub
  End If
  txtEntrada(2).SetFocus
End Sub

Private Sub Command2_Click()
  Dim X0 As Double
  Dim y0 As Double
  Dim i As Integer
  Dim Dlido As Double
  Dim Llido As Double
  Dim lAcu As Double
  Dim fe As Double
  Dim cor As Integer
  Dim Xcargalido As Double
  Dim Lcargalido As Double
  Dim KGcargalido As Double
  Dim dValido As Double
  Dim xm1 As Double
  Dim xm2 As Double
  Dim triang As Integer
  
  
  If (grdDimensoes.TextMatrix(1, 0) = "") Then Exit Sub
  
  cor = 10
  
  frmMain.Picture1.Cls
  
  fe = Val(frmMain.Text1(0).Text)
  lAcu = 0
  
  X0 = Val(frmMain.Text1(1).Text)
  y0 = frmMain.Picture1.Height / 2
  h_carga = Val(frmMain.Text1(2).Text)
  
  frmMain.Picture1.Circle (X0, y0), 50
  
  ' ---------------------geometria
  For i = 1 To frmMain.grdDimensoes.Rows - 1
    Dlido = Val(grdDimensoes.TextMatrix(i, 1))
    'Dlido = IIf(Val(grdDimensoes.TextMatrix(i, 2)) > 0, Val(grdDimensoes.TextMatrix(i, 2)), Val(grdDimensoes.TextMatrix(i, 1)))
    
    Llido = Val(grdDimensoes.TextMatrix(i, 0))
    
    'representa costelas
    If (Val(grdDimensoes.TextMatrix(i, 2)) > 0) Then Call desenha_eixo(grdDimensoes.TextMatrix(i, 0), Val(grdDimensoes.TextMatrix(i, 2)), lAcu, X0, y0, fe, 2)
    
    '&&&&
    If (Val(grdDimensoes.TextMatrix(i, 8)) > 0) Then
      Call desenha_eixo_conico(Llido, Dlido, Val(grdDimensoes.TextMatrix(i, 8)), lAcu, X0, y0, fe, 0)
    Else
      Call desenha_eixo(Llido, Dlido, lAcu, X0, y0, fe, 0)
      Picture1.Line (X0 + (lAcu) * fe, y0 - (Dlido / 2) * fe)-(X0 + (lAcu + Llido) * fe, y0 + (Dlido / 2) * fe), QBColor(0), B
    End If
    
    lAcu = lAcu + Llido
  Next i
  
  If (grdCargas.TextMatrix(1, 0) <> "") Then
    '----------- cargas
    For i = 1 To frmMain.grdCargas.Rows - 1
      Xcargalido = Val(grdCargas.TextMatrix(i, 0))
      dValido = GetDiamSgmnt(Val(grdCargas.TextMatrix(i, 0)))
      
      Lcargalido = Val(frmMain.grdCargas.TextMatrix(i, 1))
      
      KGcargalido = Val(frmMain.grdCargas.TextMatrix(i, 2))
      
      Picture1.Line (X0 + (Xcargalido) * fe, y0 - (dValido / 2) * fe)-(X0 + (Xcargalido + Lcargalido) * fe, y0 - (dValido / 2 + h_carga) * fe), QBColor(12), BF
      Picture1.Line (X0 + (Xcargalido) * fe, y0 - (dValido / 2) * fe)-(X0 + (Xcargalido + Lcargalido) * fe, y0 - (dValido / 2 + h_carga) * fe), QBColor(0), B
    Next i
  End If
  
  '----------- mancais
  triang = 200
  
  xm1 = Val(frmMain.txtEntrada(12).Text)
  xm2 = Val(frmMain.txtEntrada(13).Text)
  
  If (txtEntrada(12).Text <> "") Then
    dValido = GetDiamSgmnt(xm1)
    frmMain.Picture1.DrawWidth = 2
    frmMain.Picture1.Line (X0 + (xm1) * fe, y0 + (dValido / 2) * fe)-(X0 + (xm1) * fe + triang / 2, y0 + (dValido / 2) * fe + triang), QBColor(9)
    frmMain.Picture1.Line (X0 + (xm1) * fe + triang / 2, y0 + (dValido / 2) * fe + triang)-(X0 + (xm1) * fe - triang / 2, y0 + (dValido / 2) * fe + triang), QBColor(9)
    frmMain.Picture1.Line (X0 + (xm1) * fe, y0 + (dValido / 2) * fe)-(X0 + (xm1) * fe - triang / 2, y0 + (dValido / 2) * fe + triang), QBColor(9)
  End If
  If (txtEntrada(13).Text <> "") Then
    dValido = GetDiamSgmnt(xm2)
    frmMain.Picture1.Line (X0 + (xm2) * fe, y0 + (dValido / 2) * fe)-(X0 + (xm2) * fe + triang / 2, y0 + (dValido / 2) * fe + triang), QBColor(9)
    frmMain.Picture1.Line (X0 + (xm2) * fe + triang / 2, y0 + (dValido / 2) * fe + triang)-(X0 + (xm2) * fe - triang / 2, y0 + (dValido / 2) * fe + triang), QBColor(9)
    frmMain.Picture1.Line (X0 + (xm2) * fe, y0 + (dValido / 2) * fe)-(X0 + (xm2) * fe - triang / 2, y0 + (dValido / 2) * fe + triang), QBColor(9)
    frmMain.Picture1.DrawWidth = 1
  End If
  'frmMain.Picture1.Circle (x0 + (xm2) * fe, y0 + (Dvalido / 2) * fe), 50
End Sub

Private Function GetDiamSgmnt(ByVal inicio As Double) As Double
  Dim dTmp As Double
  Dim i As Integer
  Dim lAcu As Double
  
  dTmp = 0
  lAcu = 0
  'percorre grid de dimensoes para achar o segmento e o diametro de um determinado ponto
  For i = 1 To grdDimensoes.Rows - 1
    lAcu = lAcu + Val(grdDimensoes.TextMatrix(i, 0))
    If (inicio < lAcu) Then
      dTmp = IIf(Val(grdDimensoes.TextMatrix(i, 2)) > 0, grdDimensoes.TextMatrix(i, 2), grdDimensoes.TextMatrix(i, 1))
      Exit For
    End If
  Next
  GetDiamSgmnt = dTmp
End Function

Private Function SelSgmnt(PosX As Double, posY As Double) As Integer
  Dim i As Integer, result As Integer
  Dim sum As Double
  sum = 0
  result = 0
  For i = 1 To grdDimensoes.Rows - 1
    sum = sum + grdDimensoes.TextMatrix(i, 0)
    If ((PosX < sum) And (PosX > sum - grdDimensoes.TextMatrix(i, 0))) Then
      If ((posY >= (0 - grdDimensoes.TextMatrix(i, 1) / 2)) And (posY <= (0 + grdDimensoes.TextMatrix(i, 1) / 2))) Then
        result = i
        Exit For
      End If
    End If
  Next
  SelSgmnt = result
End Function

Private Function SelCrg(PosX As Double, posY As Double) As Integer
  Dim i As Integer, result As Integer
  Dim Ini As Double, diam As Double
  Dim Yi As Double, Yf As Double
  
  result = 0
  For i = 1 To grdCargas.Rows - 1
    Ini = grdCargas.TextMatrix(i, 0)
    If ((PosX >= Ini) And (PosX <= (Ini + Val(grdCargas.TextMatrix(i, 1))))) Then
      diam = GetDiamSgmnt(Ini)
      Yi = -diam / 2
      Yf = Yi - h_carga
      If ((posY < Yi) And (posY > Yf)) Then
        result = i
        Exit For
      End If
    End If
  Next
  SelCrg = result
End Function

Private Sub Form_Activate()
  Text1(0).Text = IIf(Val(Text1(0).Text) = 0, fator_escala, Text1(0).Text)
  Text1(1).Text = IIf(Val(Text1(1).Text) = 0, x_inicial, Text1(1).Text)
  Text1(2).Text = IIf(Val(Text1(2).Text) = 0, h_carga, Text1(2).Text)
End Sub

Private Sub Form_Load()
  Call inicializa_configuracoes
  
  Call inicializa_tela
  
  adiciona_grd1 = False
  adiciona_grd2 = False
  tem_costelado = False
  tem_oco = False
  ID_CALC = 0
  
  Call inicializa_conexao
  
  'exclui registros antigos
  Call DelClc(NrMesIni)
  
  Call EnSc(Me, False)
  
  bChanged = False
  
  Call cmdBotao_Click(4)
End Sub

Private Sub inicializa_configuracoes()
  biege = StrIni(App.path & "\" & NMI, "CAMINHOS", "BIEGE") 'PegaItem(Tmp(0), Chr(0), 2)
  file_in_biege = StrIni(App.path & "\" & NMI, "CAMINHOS", "ENTRADA") 'PegaItem(Tmp(1), Chr(0), 2)
  file_out_biege = StrIni(App.path & "\" & NMI, "CAMINHOS", "SAIDA") 'PegaItem(Tmp(2), Chr(0), 2)
  file_empuxo = StrIni(App.path & "\" & NMI, "CAMINHOS", "ARQUIVO EMPUXO") 'PegaItem(Tmp(3), Chr(0), 2)
  command_line = StrIni(App.path & "\" & NMI, "CAMINHOS", "COMMAND") 'PegaItem(Tmp(4), Chr(0), 2)
  RegAdocn = StrIni(App.path & "\" & NMI, "CAMINHOS", "REGADOCN")
  
  If Not pega_configuracoes_usuario Then
    h_carga = StrIni(App.path & "\" & NMI, "CONFIGURACOES", "ALTURA CARGA") 'PegaItem(Tmp(0), Chr(0), 2)
    fator_escala = StrIni(App.path & "\" & NMI, "CONFIGURACOES", "FATOR ESCALA") 'PegaItem(Tmp(1), Chr(0), 2)
    x_inicial = StrIni(App.path & "\" & NMI, "CONFIGURACOES", "X INICIAL") 'PegaItem(Tmp(2), Chr(0), 2)
  End If
  
  NrMesIni = CInt(StrIni(App.path & "\" & NMI, "TEMPO EXCLUSAO", "MESES")) 'CInt(PegaItem(Tmp(0), Chr(0), 2))
  'elimina arquivos
  If (Dir(file_in_biege) <> "") Then Kill file_in_biege
  If (Dir(file_out_biege) <> "") Then Kill file_out_biege
End Sub

Private Sub inicializa_tela()
    
  'grid das dimensões
  CnfGrdDimensoes
  
  'grid das cargas
  CnfGrdCargas
      
  lblUsuario.Caption = Usuario
  lblData.Caption = Format(Now, "D/M/YYYY HH:MM:SS")

  Dim Tmp() As String, DESC As String, Vlr As String
  Dim i As Integer
  Tmp = GetSec(App.path & "\" & NMI, "CONSTANTES MANCAL")
  For i = LBound(Tmp) To UBound(Tmp)
    DESC = PegaItem(Tmp(i), Chr(0), 1)
    Vlr = Val(PegaItem(Tmp(i), Chr(0), 2))
    
    cmbEntrada(0).AddItem DESC
    cmbEntrada(0).ItemData(cmbEntrada(0).NewIndex) = Vlr

    cmbEntrada(1).AddItem DESC
    cmbEntrada(1).ItemData(cmbEntrada(1).NewIndex) = Vlr
  Next
  
  Tmp = GetSec(App.path & "\" & NMI, "TIPO SEGMENTO")
  For i = LBound(Tmp) To UBound(Tmp)
    DESC = PegaItem(Tmp(i), Chr(0), 1)
    Vlr = Val(PegaItem(Tmp(i), Chr(0), 2))

    cmbEntrada(2).AddItem DESC, CInt(Vlr)
  Next
  
  'tipo de segmento padrão
  Tmp = GetSec(App.path & "\" & NMI, "SEGMENTO PADRAO")
  
  If (UBound(Tmp) > -1) Then sgmntpdr = CInt(PegaItem(Tmp(0), Chr(0), 2))
  
  Largura = Me.Width
  Altura = Me.Height
  
  Call limpa_tela
    
End Sub

Private Sub CnfGrdDimensoes()
  grdDimensoes.TextMatrix(0, 0) = "L"
  grdDimensoes.TextMatrix(0, 1) = "D"
  grdDimensoes.TextMatrix(0, 2) = "DPCT"
  grdDimensoes.TextMatrix(0, 3) = "A"
  grdDimensoes.TextMatrix(0, 4) = "B"
  grdDimensoes.TextMatrix(0, 5) = "C"
  grdDimensoes.TextMatrix(0, 6) = "NR_COST"
  grdDimensoes.TextMatrix(0, 7) = "D_INT"
  grdDimensoes.TextMatrix(0, 8) = "D_F"
  grdDimensoes.ColWidth(0) = 650
  grdDimensoes.ColWidth(1) = 650
  grdDimensoes.ColWidth(2) = 650
  grdDimensoes.ColWidth(3) = 650
  grdDimensoes.ColWidth(4) = 650
  grdDimensoes.ColWidth(5) = 650
  grdDimensoes.ColWidth(6) = 400
  grdDimensoes.ColWidth(7) = 400
  grdDimensoes.ColWidth(8) = 400
End Sub

Private Sub CnfGrdCargas()
  grdCargas.TextMatrix(0, 0) = "Xi"
  grdCargas.TextMatrix(0, 1) = "LC"
  grdCargas.TextMatrix(0, 2) = "Kg"
  grdCargas.TextMatrix(0, 3) = "Empuxo"
  grdCargas.ColWidth(0) = 700
  grdCargas.ColWidth(1) = 700
  grdCargas.ColWidth(2) = 700
  grdCargas.ColWidth(3) = 300
End Sub

Private Sub inicializa_conexao()
  Dim ErrMsg As String
  '
  AppCfg.LocPth = App.path 'caminho atual
  AppCfg.NomIni = AppCfg.LocPth & "\" & NMI 'nome do ini
  ParUsu.Nom = GetNom() 'nome do usuario
  AppCfg.TmpPth = GetTmp() 'pasta temporarios
  If Not GetIni(ErrMsg) Then
    MsgBox ErrMsg
  End If
  If Not IniSvr(ErrMsg) Then
    MsgBox ErrMsg, vbCritical, "Erro!"
  End If
  If Not DoConn(, ErrMsg) Then    'conexao
    MsgBox ErrMsg, vbCritical, "Erro!"
  End If
End Sub

Private Sub Form_Resize()
  If Not (Me.WindowState = vbMinimized) Then
    Me.Width = Largura
    Me.Height = Altura
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
  salva_configuracoes_usuario
  End
End Sub

Private Sub grdCargas_Click()
  If (grdCargas.TextMatrix(grdCargas.Row, 0) <> "") Then
    txtEntrada(9).Text = grdCargas.TextMatrix(grdCargas.Row, 0)
    txtEntrada(10).Text = grdCargas.TextMatrix(grdCargas.Row, 1)
    txtEntrada(11).Text = grdCargas.TextMatrix(grdCargas.Row, 2)
    Check1.Value = Val(grdCargas.TextMatrix(grdCargas.Row, 3))
    cmdGrid(4).Enabled = True

    Command2_Click
    Dim dValido As Double, X0 As Double, y0 As Double, fe As Double
    fe = Val(Text1(0).Text)
    X0 = Val(Text1(1).Text)
    y0 = Picture1.Height / 2
        
    dValido = GetDiamSgmnt(grdCargas.TextMatrix(grdCargas.Row, 0))
    Picture1.Line (X0 + (grdCargas.TextMatrix(grdCargas.Row, 0)) * fe, y0 - (dValido / 2) * fe)-(X0 + (Val(grdCargas.TextMatrix(grdCargas.Row, 0)) + Val(grdCargas.TextMatrix(grdCargas.Row, 1))) * fe, y0 - (dValido / 2 + h_carga) * fe), QBColor(11), BF
    Picture1.Line (X0 + (grdCargas.TextMatrix(grdCargas.Row, 0)) * fe, y0 - (dValido / 2) * fe)-(X0 + (Val(grdCargas.TextMatrix(grdCargas.Row, 0)) + Val(grdCargas.TextMatrix(grdCargas.Row, 1))) * fe, y0 - (dValido / 2 + h_carga) * fe), QBColor(0), B
    
    Call SelRow(grdCargas, grdCargas.Row)
    grdCargas.TopRow = grdCargas.Row
  End If
End Sub

Private Sub grdCargas_DblClick()
  Call DfPcRt
End Sub

'===============================================================================
' Name: DfPcRt
' Input:
' Output:
' Purpose: Definir pacote do rotor
' Remarks:
' Author: mpiazera
'===============================================================================
Private Sub DfPcRt()
  Dim i As Integer, j As Integer, r As Integer, c As Integer
  r = grdCargas.Row: c = grdCargas.Col
  For j = 1 To (grdCargas.Rows - 1)
    grdCargas.Row = j
    For i = 0 To (grdCargas.Cols - 1)
      grdCargas.Col = i
      grdCargas.CellBackColor = BckNml
    Next
  Next
  grdCargas.Row = r
  For i = 0 To (grdCargas.Cols - 1)
    grdCargas.Col = i
    grdCargas.CellBackColor = BkgPck
  Next
  grdCargas.Col = c
End Sub

Private Sub grdDimensoes_Click()
  If (grdDimensoes.TextMatrix(grdDimensoes.Row, 0) <> "") Then
    chkConico.Value = 0
    txtEntrada(2).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 0)
    txtEntrada(3).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 1)
    If (grdDimensoes.TextMatrix(grdDimensoes.Row, 8) <> "") Then
      chkConico.Value = 1
      txtEntrada(20).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 8)
    End If
    If (grdDimensoes.TextMatrix(grdDimensoes.Row, 2) <> "") Then
      cmbEntrada(2).ListIndex = ind_cost
      txtEntrada(4).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 2)
      txtEntrada(5).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 3)
      txtEntrada(6).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 4)
      txtEntrada(7).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 5)
      txtEntrada(8).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 6)
    ElseIf (grdDimensoes.TextMatrix(grdDimensoes.Row, 7) <> "") Then
      cmbEntrada(2).ListIndex = ind_oco
      txtEntrada(19).Text = grdDimensoes.TextMatrix(grdDimensoes.Row, 7)
    Else
      cmbEntrada(2).ListIndex = ind_red
    End If
    cmdGrid(1).Enabled = True
    
    Command2_Click
    Dim lAcu As Double
    Dim i As Integer
    Dim fe As Double, X0 As Double, y0 As Double
    X0 = Val(frmMain.Text1(1).Text)
    y0 = frmMain.Picture1.Height / 2
    fe = Val(Text1(0).Text)
    lAcu = 0
    For i = 1 To grdDimensoes.Row - 1
      lAcu = lAcu + grdDimensoes.TextMatrix(i, 0)
    Next
    'Call desenha_eixo(grdDimensoes.TextMatrix(grdDimensoes.Row, 0), IIf(Val(grdDimensoes.TextMatrix(grdDimensoes.Row, 2)) > 0, Val(grdDimensoes.TextMatrix(grdDimensoes.Row, 2)), Val(grdDimensoes.TextMatrix(grdDimensoes.Row, 1))), 0, lAcu, x0, y0, fe, 1)
    If (grdDimensoes.TextMatrix(grdDimensoes.Row, 8) = "") Then
      Call desenha_eixo(grdDimensoes.TextMatrix(grdDimensoes.Row, 0), grdDimensoes.TextMatrix(grdDimensoes.Row, 1), lAcu, X0, y0, fe, 1)
    Else
      Call desenha_eixo_conico(grdDimensoes.TextMatrix(grdDimensoes.Row, 0), grdDimensoes.TextMatrix(grdDimensoes.Row, 1), grdDimensoes.TextMatrix(grdDimensoes.Row, 8), lAcu, X0, y0, fe, 1)
    End If
    Call SelRow(grdDimensoes, grdDimensoes.Row)
  End If
End Sub

Private Sub mnuAAbrir_Click(Index As Integer)
  If bChanged Then
    Select Case MsgBox("Deseja salvar alterações??", vbYesNoCancel + vbQuestion, "Atenção!")
      Case vbYes
        Call cmdBotao_Click(3)
        bChanged = False
      Case vbCancel
        Exit Sub
    End Select
  End If
  mostra_lista_calculos
  If (txtEntrada(0).Text <> "") Then
    abre_calculo
  End If
End Sub

Private Sub mnuCopiar_Click(Index As Integer)
  Clipboard.Clear
  Clipboard.SetData Picture1.Image
  lblStatus.Caption = "Imagem Copiada"
End Sub

Private Sub mnuCopiar2_Click(Index As Integer)
  mnuCopiar_Click Index
End Sub

Private Sub mnuGerar_Click(Index As Integer)
  cmdBotao_Click 2
End Sub

Private Sub mnuNovo_Click(Index As Integer)
  If bChanged Then
    Select Case MsgBox("Deseja salvar alterações??", vbYesNoCancel + vbQuestion, "Atenção!")
      Case vbYes
        Call cmdBotao_Click(3)
        bChanged = False
      Case vbCancel
        Exit Sub
    End Select
  End If
  limpa_tela
  ID_CALC = 0
  lblData.Caption = Format(Now, "D/M/YYYY HH:MM:SS")
  lblUsuario.Caption = Usuario
  adiciona_grd1 = False
  adiciona_grd2 = False
  tem_costelado = False
  tem_oco = False
  linha_costelado = 0
  linha_oco = 0
  linha_con = 0
  Text1(0).Text = fator_escala
  Text1(1).Text = x_inicial
  Text1(2).Text = h_carga
  Call EnSc(Me, True)
  'txtEntrada(0).SetFocus
  cmbEntrada(2).ListIndex = sgmntpdr
  lblComp(1).Caption = 0
  bChanged = False
  Check2.Value = 1
End Sub

Private Sub mnuSair_Click(Index As Integer)
  If bChanged Then
    Select Case MsgBox("Deseja salvar alterações??", vbYesNoCancel + vbQuestion, "Atenção!")
      Case vbYes
        Call cmdBotao_Click(3)
        bChanged = False
      Case vbCancel
        Exit Sub
    End Select
  End If
  salva_configuracoes_usuario
  End
End Sub

Private Sub mnuSalvar_Click(Index As Integer)
  Call cmdBotao_Click(1)
End Sub

Private Sub Picture1_DblClick()
  Call grdCargas_DblClick
End Sub

Private Sub Picture1_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  Command2_Click
  If (Button = 2) Then
    PopupMenu mnuRightBtn(8)
  End If
  If (Button = 1) Then
    If (grdDimensoes.TextMatrix(1, 0) <> "") Then
      If Not ProcSelSgmnt(Val(X), Val(Y)) Then
        If (grdCargas.TextMatrix(1, 0) <> "") Then
          If Not ProcSelCarga(Val(X), Val(Y)) Then
              'ProcSelMancal Val(X), Val(Y)
          End If
        End If
      End If
    End If
  End If
End Sub

Private Function ProcSelSgmnt(X As Double, Y As Double) As Boolean
  Dim fe As Double, X0 As Double, y0 As Double, lin As Integer
  Dim lAcu As Double
  Dim i As Integer
  fe = Val(frmMain.Text1(0).Text)
  X0 = Val(frmMain.Text1(1).Text)
  y0 = frmMain.Picture1.Height / 2
  lin = SelSgmnt((X - X0) / fe, (Y - y0) / fe)
  If (lin > 0) Then
    grdDimensoes.Row = lin
    grdDimensoes.SetFocus
    grdDimensoes_Click
    lAcu = 0
    For i = 1 To lin - 1
      lAcu = lAcu + grdDimensoes.TextMatrix(i, 0)
    Next
    If (grdDimensoes.TextMatrix(grdDimensoes.Row, 8) = "") Then
      Call desenha_eixo(grdDimensoes.TextMatrix(grdDimensoes.Row, 0), grdDimensoes.TextMatrix(grdDimensoes.Row, 1), lAcu, X0, y0, fe, 1)
    Else
      Call desenha_eixo_conico(grdDimensoes.TextMatrix(grdDimensoes.Row, 0), grdDimensoes.TextMatrix(grdDimensoes.Row, 1), grdDimensoes.TextMatrix(grdDimensoes.Row, 8), lAcu, X0, y0, fe, 1)
    End If
    grdDimensoes.TopRow = grdDimensoes.Row
    ProcSelSgmnt = True
  Else
    ProcSelSgmnt = False
  End If
End Function

Private Function ProcSelCarga(X As Double, Y As Double) As Boolean
  Dim fe As Double, X0 As Double, y0 As Double, lin As Integer
  Dim lAcu As Double
  Dim dValido As Double
  fe = Val(frmMain.Text1(0).Text)
  X0 = Val(frmMain.Text1(1).Text)
  y0 = frmMain.Picture1.Height / 2
  lin = SelCrg((X - X0) / fe, (Y - y0) / fe)
  If (lin > 0) Then
    grdCargas.Row = lin
    grdCargas.SetFocus
    grdCargas_Click
    lAcu = 0
'        For i = 1 To Lin - 1
'            lAcu = lAcu + grdCargas.TextMatrix(i, 0)
'        Next
    'Call desenha_eixo(grdCargas.TextMatrix(grdCargas.Row, 0), grdCargas.TextMatrix(grdCargas.Row, 1), 0, lAcu, x0, y0, fe, 1)
    dValido = GetDiamSgmnt(grdCargas.TextMatrix(grdCargas.Row, 0))
    Picture1.Line (X0 + (grdCargas.TextMatrix(grdCargas.Row, 0)) * fe, y0 - (dValido / 2) * fe)-(X0 + (Val(grdCargas.TextMatrix(grdCargas.Row, 0)) + Val(grdCargas.TextMatrix(grdCargas.Row, 1))) * fe, y0 - (dValido / 2 + h_carga) * fe), QBColor(11), BF
    Picture1.Line (X0 + (grdCargas.TextMatrix(grdCargas.Row, 0)) * fe, y0 - (dValido / 2) * fe)-(X0 + (Val(grdCargas.TextMatrix(grdCargas.Row, 0)) + Val(grdCargas.TextMatrix(grdCargas.Row, 1))) * fe, y0 - (dValido / 2 + h_carga) * fe), QBColor(0), B
    
    grdCargas.TopRow = grdCargas.Row
    ProcSelCarga = True
  Else
    ProcSelCarga = False
  End If
End Function

Private Sub tmr_Timer()
  cmbEntrada(0).Text = constante(0) & IIf(Len(constante(0)) = 1, "E+009", "")
  cmbEntrada(1).Text = constante(1) & IIf(Len(constante(1)) = 1, "E+009", "")
  tmr.Interval = 0
End Sub

Private Sub exporta_biege(path$)
  Dim Ref As Double, posic As Double, diame As Double, diame2 As Double, posi_ant As Double
  
  Dim i As Long

  Open path$ For Output As #1
  Print #1, "BIEGE14"
  Print #1, "''"
  Print #1, "''"
  Print #1, "'MM'  2 2.06E+011 7850 " & IIf(Check2.Value = 1, "'JA'", "'NEIN'")
  
  '---------- comprimento e diametro dos escalonamentos
  Ref = 0
  For i = 1 To grdDimensoes.Rows - 1
    posic = Val(grdDimensoes.TextMatrix(i, 0)) + Ref
        
    Ref = posic
    
    diame = grdDimensoes.TextMatrix(i, 1)
    diame2 = Val(grdDimensoes.TextMatrix(i, 8)) 'eixo cônico
    
'       Print #1, posic & " " & diame & " 0.0"
    Print #1, Format(Val(posic), "######.000000") & Chr(9) & Format(Val(diame), "######.000000") & Chr(9) & IIf(diame2 > 0, diame2, "0.0")
    
    posi_ant = Val(posic)
  Next i
  Print #1, "0.0" & Chr(9) & "0.0" & Chr(9) & "0.0"
  
  '---------- região do pacote
  Dim dpacote As Double, dalma As Double, nC As Double, esp_cost As Double
  Dim esp_mart As Double, larg_mart As Double, h_costela As Double
  '---------- eixo costelado
  If tem_costelado Then
    Print #1, "'ST'"
    For i = 1 To grdDimensoes.Rows - 1
      If (grdDimensoes.TextMatrix(i, 2) <> "") Then
        dpacote = Val(grdDimensoes.TextMatrix(i, 2))
        dalma = Val(grdDimensoes.TextMatrix(i, 1))
        nC = Val(grdDimensoes.TextMatrix(i, 6))
        esp_cost = Val(grdDimensoes.TextMatrix(i, 3))
        esp_mart = Val(grdDimensoes.TextMatrix(i, 4))
        larg_mart = Val(grdDimensoes.TextMatrix(i, 5))
        h_costela = (dpacote - dalma) / 2 - esp_mart
        Print #1, Right$(Str$(h_costela), Len(Str$(h_costela)) - 1) & " " & Str$(esp_cost) & " " & Str$(esp_mart) & " " & Str$(larg_mart) & " " & Str$(nC) & " 0.0 0.0"
      Else
'         Print #1, "0.0 0.0 0.0 0.0 0.0 0.0"
        Print #1, "0.0" & Chr(9) & "0.0" & Chr(9) & "0.0" & Chr(9) & "0.0" & Chr(9) & "0.0" & Chr(9) & "0.0"
      End If
    Next i
  ElseIf tem_oco Then
    Print #1, "'HL'"
    For i = 1 To grdDimensoes.Rows - 1
      If (grdDimensoes.TextMatrix(i, 7) <> "") Then
        Print #1, grdDimensoes.TextMatrix(i, 7)
      Else
        Print #1, "0.0"
      End If
    Next
  Else
    Print #1, "'KEIN'"
  End If
  
  
  '---------- cargas concentradas - posição e peso
  For i = 1 To grdCargas.Rows - 1
    If ((Val(grdCargas.TextMatrix(i, 1)) = 0) And (Val(grdCargas.TextMatrix(i, 0)) > 0)) Then
      'Print #1, grdCargas.TextMatrix(i, 0) & " " & grdCargas.TextMatrix(i, 2)
      Print #1, Format(Val(grdCargas.TextMatrix(i, 0)), "######.000000") & Chr(9) & Format(Val(grdCargas.TextMatrix(i, 2)), "######.000000")
    End If
  Next i
  Print #1, "0.0 0.0"
  
  '---------- cargas distribuidas - posição inicial, posição final, peso
  Dim soma As Double
  Dim posi As Double, compr As Double, peso As Double
  
  
  soma = 0
  For i = 1 To grdCargas.Rows - 1
    If (Val(grdCargas.TextMatrix(i, 1)) > 0) Then
      posi = Val(grdCargas.TextMatrix(i, 0))
      compr = posi + Val(grdCargas.TextMatrix(i, 1))
      peso = Val(grdCargas.TextMatrix(i, 2))
      If (Val(grdCargas.TextMatrix(i, 3)) = 0) Then
'               Print #1, posi & " " & compr & " " & Str$(peso) & " 0.0 0"
        Print #1, Format(Val(posi), "######.000000") & Chr(9) & Format(Val(compr), "######.000000") & Chr(9) & Format(Val(peso), "######.000000") & Chr(9) & "0.0" & Chr(9) & "0"
      Else
'               Print #1, posi & " " & compr & " " & Str$(peso) & " 0.0 1"
        Print #1, Format(Val(posi), "######.000000") & Chr(9) & Format(Val(compr), "######.000000") & Chr(9) & Format(Val(peso), "######.000000") & Chr(9) & "0.0" & Chr(9) & "1"
      End If
    End If
  Next i
  
  'Print #1, "0.0 0.0 0.0 0.0 0"
   Print #1, "0.0" & Chr(9) & "0.0" & Chr(9) & "0.0" & Chr(9) & "0.0" & Chr(9) & "0"
  
  '---------- mancais
  
  Dim tmp2 As String, s As String
  i = 12: s = 1
  If (Val(txtEntrada(12).Text) > Val(txtEntrada(13).Text)) Then
    i = 13: s = -1
  End If
  If (Val(txtEntrada(i).Text) > 0) Then
    tmp2 = cmbEntrada(0).Text
    If ((tmp2 = "Infinito") Or (Val(tmp2) = 0)) Then tmp2 = "0"
    If (tmp2 = "") Then cmbEntrada(0).Text = "0"
    If (tmp2 = "0") Then
      Print #1, Format(Val(txtEntrada(i).Text), "######.000000") & Chr(9); "1" & Chr(9) & "0.0"
    Else
      Print #1, Format(Val(txtEntrada(i).Text), "######.000000") & Chr(9) & "2" & Chr(9) & tmp2
    End If
  End If
  i = i + s
  If Val(txtEntrada(i).Text) Then
    tmp2 = cmbEntrada(1).Text
    If ((tmp2 = "Infinito") Or (Val(tmp2) = 0)) Then tmp2 = "0"
    If (tmp2 = "") Then cmbEntrada(1).Text = "0"
    If (tmp2 = "0") Then
      Print #1, Format(Val(txtEntrada(i).Text), "######.000000") & Chr(9) & "1" & Chr(9) & "0.0"
    Else
      Print #1, Format(Val(txtEntrada(i).Text), "######.000000") & Chr(9) & "2" & Chr(9) & tmp2
    End If
  End If
  Print #1, "0.0" & Chr(9) & "0" & Chr(9) & "0.0"
  Close #1
End Sub

Private Function GetSec(ByVal NomArq As String, NomSec As String) As String()
  '
  Dim Idx As Integer, Cnt As Integer
  Dim Vlr As String
  Dim Tmp() As String, Ret() As String
  ReDim GetSec(-1 To -1)
  '
  Vlr = StrIni(NomArq, NomSec, Vlr)
  Tmp = Split(Vlr, Chr(0))
  '
  For Idx = 0 To UBound(Tmp)
    If (Tmp(Idx) > "") Then
      Vlr = StrIni(NomArq, NomSec, Tmp(Idx))
      If (Tmp(Idx) > "") Then
        ReDim Preserve Ret(Cnt)
        Ret(Cnt) = Tmp(Idx) & Chr(0) & Vlr
        Cnt = Cnt + 1
      End If
    End If
  Next
  If (Cnt > 0) Then GetSec = Ret
  '
End Function

Private Function busca_dados_calc(ByVal nr_ref As String) As String
  Dim Sql As String, Tmp As String, ErrMsg As String
  'dados referentes a OP
  Sql = "select id,ds_calc,nm_usu,dt_dat from d_idnbiege where nr_ref = '" & nr_ref & "'"
  If (AdoCon.AdoSQ(Tmp, IDConE, Sql, True, 999, ErrMsg) < 0) Then Exit Function  'erro executando sql
  busca_dados_calc = Tmp
End Function

Private Function busca_mancais(ByVal id_calculo As Long) As String
  Dim Sql As String, Tmp As String, ErrMsg As String
  'dados referentes a OP
  Sql = "select vl_caract, ds_caract from d_carbiege where id_d_idnbiege=" & id_calculo & " and vl_linha<0 Order by vl_linha desc"
  If (AdoCon.AdoSQ(Tmp, IDConE, Sql, True, 999, ErrMsg) < 0) Then Exit Function  'erro executando sql
  busca_mancais = Tmp
End Function

Private Function salva_mancal(ByVal id_calculo As Long, ByVal Posicao As Double, ByVal constante As Double, ByRef linha As Integer, Optional err_msg As String) As Boolean
  Dim Sql As String
  Dim OK As Boolean
  'posição do mancal
  Sql = "insert into d_carbiege (id_d_idnbiege,vl_linha,ds_caract,vl_caract) " + _
        "values (" & id_calculo & "," & linha & ",'mancal'," & Posicao & ")"
  OK = (AdoCon.AdoEX(IDConE, Sql, err_msg) = 0)
  If OK Then
    linha = linha - 1
    'posição do mancal
    Sql = "insert into d_carbiege (id_d_idnbiege,vl_linha,ds_caract,vl_caract) " + _
          "values (" & id_calculo & "," & linha & ",'constante'," & constante & ")"
    OK = (AdoCon.AdoEX(IDConE, Sql, err_msg) = 0)
  End If
  salva_mancal = OK
End Function

Private Function sobrepoecarga(ByVal inicio As Double, ByVal comprimento As Double, ByVal Indice As Integer) As Boolean
  Dim i As Long
  Dim Ini As Double, Fim As Double, fim2 As Double
  Dim flg As Boolean
  
  flg = False
  For i = 1 To grdCargas.Rows - 1
    If (Indice <> i) Then
      If (grdCargas.TextMatrix(i, 1) <> "") Then
        Ini = Val(grdCargas.TextMatrix(i, 0))
        Fim = Ini + Val(grdCargas.TextMatrix(i, 1))
        
        fim2 = inicio + comprimento
        
        If ((inicio >= Ini) And (inicio <= Fim)) Then flg = True: MsgBox "Cargas sobrepostas!", vbCritical, "Atenção!": Exit For
        If ((fim2 >= Ini) And (fim2 <= Fim)) Then flg = True: MsgBox "Cargas sobrepostas!", vbCritical, "Atenção!": Exit For
      End If
    End If
  Next
  sobrepoecarga = flg
End Function

Private Function ultrapassaescalonamento(ByVal inicio As Double, ByVal comprimento As Double) As Boolean
  Dim i As Long
  Dim inisegmnt As Double, fimsegmnt As Double, fimcarga As Double
  Dim flg As Boolean
  
  flg = True
  
  fimsegmnt = 0
  fimcarga = inicio + comprimento
  For i = 1 To grdDimensoes.Rows - 1
    inisegmnt = fimsegmnt
    fimsegmnt = fimsegmnt + Val(grdDimensoes.TextMatrix(i, 0))
    'carga é nesse segmento??
    If ((Val(inicio) >= Val(inisegmnt)) And (Val(fimcarga) <= Val(fimsegmnt))) Then
      flg = False
      Exit For
    End If
  Next
  If flg Then MsgBox "Carga ultrapassa segmento de eixo!", vbCritical, "Atenção!"
  ultrapassaescalonamento = flg
End Function

Private Function Salva(ByVal NrRef As String, ByVal DsClc As String, Optional iD As Long, Optional ErrMsg As String) As Boolean
  'salva tudo
  'tratamento de erro?????
  '
  'Dim Id As Long
  Dim Sql As String
  Dim OK As Boolean
  Dim Tmp As String
  Dim iDc As Long
  '
  'inicia transacao
  OK = (AdoCon.AdoBT(IDConE, ErrMsg) = 0)
  If (Not OK) Then GoTo TrtErrIns
  '
  'verifica update
  Sql = "Select id From " & ParDB.Pr & "d_idnbiege Where NR_REF='" & NrRef & "'"
  iDc = iD
  If (AdoCon.AdoSQ(Tmp, IDConE, Sql, , , ErrMsg) = 0) Then
    iD = Val(Tmp)
    If (iD > 0) Then
      If (iD = iDc) Then
        If (Not Delete(iD, "", True, False, ErrMsg)) Then GoTo TrtErrIns
      Else
        If (MsgBox("Já existe um cálculo com esse nome, deseja gravar " & _
                  "as informações deste cálculo sobre o já existente?", vbYesNoCancel + vbQuestion, "Atenção") <> vbYes) Then
            ErrMsg = "Informe um nome diferente de um já existente para salvar o cálculo"
            iD = iDc
            GoTo TrtErrIns
        End If
        If (Not Delete(iD, "", True, False, ErrMsg)) Then GoTo TrtErrIns
      End If
    Else
      'obtem id novo da sequencia
      iD = GetId("d_idnbiege_seq", , ErrMsg)
      If (iD < 0) Then MsgBox ErrMsg: GoTo TrtErrIns
    End If
  Else
    ErrMsg = "EE4 : Salva-> Erro executando SQL" & IIf(ErrMsg > "", vbLf & ErrMsg, "")
    GoTo TrtErrIns
  End If
  '
  'prepara insert
  Sql = "Insert Into " & ParDB.Pr & "d_idnbiege (ID,NR_REF,DS_CALC,NM_USU,DT_DAT) Values ("
  Sql = Sql & CStr(iD) & ",'" & NrRef & "','" & DsClc & "','" & UCase$(ParUsu.Nom) & "'," & GetDat(True) & ")"
  '
  'executa insert na identificacao
  OK = (AdoCon.AdoEX(IDConE, Sql, ErrMsg) = 0)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  'salva grid dimensoes
  OK = SlvGrd(iD, grdDimensoes, "L,D,DPCT,A,B,C,NR_COST,D_INT,D_F", ErrMsg)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  'salva grid cargas
  OK = SlvGrd(iD, grdCargas, "Xi,LC,KG,Empuxo", ErrMsg)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  'salva mancais
  Dim linha As Integer
  linha = -1
  OK = salva_mancal(iD, Val(txtEntrada(12).Text), IIf((cmbEntrada(0).Text <> "Infinito"), Val(cmbEntrada(0).Text), 0), linha, ErrMsg)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  '
  linha = linha - 1
  OK = salva_mancal(iD, Val(txtEntrada(13).Text), IIf((cmbEntrada(1).Text <> "Infinito"), Val(cmbEntrada(1).Text), 0), linha, ErrMsg)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  '
  'comita tudo
  OK = (AdoCon.AdoCT(IDConE, ErrMsg) = 0)
  If (Not OK) Then MsgBox ErrMsg: GoTo TrtErrIns
  '
  Salva = True
  '
  Exit Function
  '
TrtErrIns:
  '
  Call AdoCon.AdoRT(IDConE, ErrMsg) 'rolbeca transacao
  ErrMsg = "EE4 : RemTmp-> " & IIf(ErrMsg > "", vbLf & ErrMsg, "")
  '
End Function

Private Sub limpa_tela()
  Dim component As Object
  Dim i As Long
  
  For i = 0 To frmMain.Count - 1
    Set component = frmMain.Controls(i)
    Select Case TypeName(component)
      Case "TextBox"
        component.Text = ""
      Case "CheckBox"
        component.Value = 0
      Case "MSFlexGrid"
        component.Rows = 1
        component.Rows = 2
      Case "ComboBox"
        component.Text = ""
      Case "PictureBox"
        component.Cls
    End Select
  Next
End Sub

Private Sub txtEntrada_Change(Index As Integer)
  Select Case Index
    Case 12 To 13
      bChanged = True
  End Select
End Sub

Private Sub txtEntrada_GotFocus(Index As Integer)
  lblStatus.Caption = txtEntrada(Index).ToolTipText
  txtEntrada(Index).SelStart = 0
  txtEntrada(Index).SelLength = Len(txtEntrada(Index).Text)
End Sub

Private Sub txtEntrada_KeyDown(Index As Integer, KeyCode As Integer, Shift As Integer)
  If (KeyCode = 13) Then
    Select Case Index
      Case 2 To 8
        Call cmdGrid_Click(0)
      Case 9 To 11
        Call cmdGrid_Click(3)
    End Select
  End If
End Sub

Private Sub txtEntrada_LostFocus(Index As Integer)
  txtEntrada(Index).Text = Trim(txtEntrada(Index).Text)
  Select Case Index
    Case 12 To 13
      Call Command2_Click
  End Select
End Sub

Private Sub txtEntrada_Validate(Index As Integer, Cancel As Boolean)
  If (Index > 1) Then
    txtEntrada(Index).Text = Val(txtEntrada(Index).Text)
  End If
End Sub

Private Sub SelRow(ByVal grid As MSFlexGrid, ByVal nRgrow As Integer)
  '
  grid.Row = nRgrow
  grid.Col = grid.Cols - 1
  grid.ColSel = 0
  '
End Sub

Private Sub salva_configuracoes_usuario()
  Dim appname As String
  appname = App.ProductName

  h_carga = Val(Text1(2).Text)
  fator_escala = Val(Text1(0).Text)
  x_inicial = Val(Text1(1).Text)
  SaveSetting appname, "Configuracoes", "Altura Carga", CStr(h_carga)
  SaveSetting appname, "Configuracoes", "Fator Escala", CStr(fator_escala)
  SaveSetting appname, "Configuracoes", "X Inicial", CStr(x_inicial)
End Sub

Private Function pega_configuracoes_usuario() As Boolean
  Dim appname As String
  Dim variant1 As Variant
  Dim bTmp As Boolean
  
  appname = App.ProductName

  bTmp = False
  variant1 = GetSetting(appname, "Configuracoes", "Altura Carga")
  If IsEmpty(variant1) Then GoTo Fim
  If (variant1 = "") Then GoTo Fim
  h_carga = variant1
  variant1 = GetSetting(appname, "Configuracoes", "Fator Escala")
  If IsEmpty(variant1) Then GoTo Fim
  If (variant1 = "") Then GoTo Fim
  fator_escala = variant1
  variant1 = GetSetting(appname, "Configuracoes", "X Inicial")
  If IsEmpty(variant1) Then GoTo Fim
  If (variant1 = "") Then GoTo Fim
  x_inicial = variant1
  bTmp = True
Fim:
  pega_configuracoes_usuario = bTmp
End Function

Private Function ha_costelado(Optional linha As Integer = -1) As Boolean
  'função para verificar se há ao menos um eixo do tipo costelado
  'desconsiderando o indicado pelo parâmetro 'linha'
  Dim i As Integer
  Dim b As Boolean
  b = False
  For i = 1 To grdDimensoes.Rows - 1
    If (i <> linha) Then
      If grdDimensoes.TextMatrix(i, 3) <> "" Then
        b = True
        Exit For
      End If
    End If
  Next
  ha_costelado = b
End Function

Private Function ha_oco(Optional linha As Integer = -1) As Boolean
  'função para verificar se há ao menos um eixo do tipo oco
  'desconsiderando o indicado pelo parâmetro 'linha'
  Dim i As Integer
  Dim b As Boolean
  b = False
  For i = 1 To grdDimensoes.Rows - 1
    If (i <> linha) Then
      If grdDimensoes.TextMatrix(i, 7) <> "" Then
        b = True
        Exit For
      End If
    End If
  Next
  ha_oco = b
End Function

Private Function ha_conico(Optional linha As Integer = -1) As Boolean
  'função para verificar se há ao menos um eixo do tipo cônico
  'desconsiderando o indicado pelo parâmetro 'linha'
  Dim i As Integer
  Dim b As Boolean
  b = False
  For i = 1 To grdDimensoes.Rows - 1
    If (i <> linha) Then
      If (grdDimensoes.TextMatrix(i, 8) <> "") Then
        b = True
        Exit For
      End If
    End If
  Next
  ha_conico = b
End Function

Private Function sum_sgmnts() As Double
  Dim soma As Double
  Dim i As Integer
  soma = 0
  For i = 1 To grdDimensoes.Rows - 1
    soma = soma + Val(grdDimensoes.TextMatrix(i, 0))
  Next
  sum_sgmnts = soma
End Function
