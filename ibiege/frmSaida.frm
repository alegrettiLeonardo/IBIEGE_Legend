VERSION 5.00
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Begin VB.Form frmSaida 
   Caption         =   "Saída do BIEGE"
   ClientHeight    =   8925
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   11730
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmSaida.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   8925
   ScaleWidth      =   11730
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture2 
      Align           =   2  'Align Bottom
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   1
      Left            =   0
      ScaleHeight     =   345
      ScaleWidth      =   11730
      TabIndex        =   3
      Top             =   8580
      Width           =   11730
      Begin VB.Label Label1 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Status"
         Height          =   255
         Left            =   120
         TabIndex        =   4
         Top             =   75
         Width           =   11490
      End
   End
   Begin VB.PictureBox Picture2 
      Align           =   1  'Align Top
      BorderStyle     =   0  'None
      Height          =   495
      Index           =   0
      Left            =   0
      ScaleHeight     =   495
      ScaleWidth      =   11730
      TabIndex        =   1
      Top             =   0
      Width           =   11730
      Begin VB.CommandButton cmdPrint 
         Height          =   300
         Index           =   0
         Left            =   120
         Picture         =   "frmSaida.frx":014A
         Style           =   1  'Graphical
         TabIndex        =   2
         ToolTipText     =   "Imprime"
         Top             =   120
         Width           =   300
      End
   End
   Begin RichTextLib.RichTextBox RichTextBox1 
      Height          =   8115
      Left            =   120
      TabIndex        =   0
      Top             =   480
      Width           =   11490
      _ExtentX        =   20267
      _ExtentY        =   14314
      _Version        =   393217
      Enabled         =   -1  'True
      ReadOnly        =   -1  'True
      ScrollBars      =   3
      TextRTF         =   $"frmSaida.frx":04D7
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
End
Attribute VB_Name = "frmSaida"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Public Sub inicializa(filename As String)
  RichTextBox1.LoadFile filename
  Me.Show
End Sub

Private Sub cmdPrint_Click(Index As Integer)
  RichTextBox1.SelPrint Printer.hDC, True
  Label1.Caption = "Imprimindo..."
End Sub
