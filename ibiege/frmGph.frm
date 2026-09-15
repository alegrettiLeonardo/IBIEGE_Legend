VERSION 5.00
Begin VB.Form frmGphFlc 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   Caption         =   "Gráfico da Flecha do Eixo"
   ClientHeight    =   7425
   ClientLeft      =   3255
   ClientTop       =   2670
   ClientWidth     =   9180
   Icon            =   "frmGph.frx":0000
   LinkTopic       =   "Form7"
   MaxButton       =   0   'False
   ScaleHeight     =   7425
   ScaleWidth      =   9180
   Begin VB.Frame Frame1 
      Height          =   7455
      Index           =   1
      Left            =   0
      TabIndex        =   0
      Top             =   -30
      Width           =   9195
      Begin VB.CommandButton Command1 
         Caption         =   "OK"
         Height          =   345
         Left            =   7980
         TabIndex        =   7
         Top             =   6990
         Width           =   1185
      End
      Begin VB.Frame Frame1 
         Height          =   3405
         Index           =   2
         Left            =   30
         TabIndex        =   4
         Top             =   3510
         Width           =   9135
         Begin VB.PictureBox Picture1 
            AutoRedraw      =   -1  'True
            BackColor       =   &H8000000E&
            Height          =   2625
            Index           =   1
            Left            =   60
            ScaleHeight     =   2565
            ScaleWidth      =   8940
            TabIndex        =   6
            Top             =   420
            Width           =   9000
         End
         Begin VB.TextBox Text1 
            Alignment       =   2  'Center
            Height          =   285
            Index           =   1
            Left            =   60
            Locked          =   -1  'True
            TabIndex        =   5
            Text            =   "4000"
            Top             =   3060
            Width           =   795
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "LINHA ELÁSTICA- PLANO xz"
            Height          =   195
            Index           =   1
            Left            =   60
            TabIndex        =   9
            Top             =   180
            Width           =   2115
         End
      End
      Begin VB.Frame Frame1 
         Height          =   3345
         Index           =   0
         Left            =   30
         TabIndex        =   1
         Top             =   120
         Width           =   9135
         Begin VB.TextBox Text1 
            Alignment       =   2  'Center
            Height          =   285
            Index           =   0
            Left            =   60
            Locked          =   -1  'True
            TabIndex        =   3
            Text            =   "4000"
            Top             =   2970
            Width           =   795
         End
         Begin VB.PictureBox Picture1 
            AutoRedraw      =   -1  'True
            BackColor       =   &H8000000E&
            Height          =   2535
            Index           =   0
            Left            =   60
            ScaleHeight     =   2475
            ScaleWidth      =   8940
            TabIndex        =   2
            Top             =   420
            Width           =   9000
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            Caption         =   "LINHA ELÁSTICA- PLANO xy"
            Height          =   195
            Index           =   0
            Left            =   60
            TabIndex        =   8
            Top             =   180
            Width           =   2115
         End
      End
   End
End
Attribute VB_Name = "frmGphFlc"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Const folga = 400

Public Sub Desenha_Flecha(ByVal Idx As Integer)
  Dim posi() As String, lido As String
  Dim defl() As String, flDf As String
  Dim Largura As Double, Altura As Double, folga As Integer
  Dim cor1 As Double, cor2 As Double, cor3 As Double, cor4 As Double
  Dim Leixo As Double, Lu As Double, FeLu As Double
  Dim Posmd As Double, Posmt As Double, fe As Double
  Dim cont As Integer, i As Integer, j As Integer, salto As Double
  Dim X1 As Double, Y1 As Double, X2 As Double, Y2 As Double
  Dim flPs As String, flVt As String, flHr As String
  On Local Error GoTo Fimgrafico_flecha
  
  flPs = StrIni(App.path & "\IBiege.ini", "FLECHA", "GPH1")
  flVt = StrIni(App.path & "\IBiege.ini", "FLECHA", "GPH2")
  flHr = StrIni(App.path & "\IBiege.ini", "FLECHA", "GPH3")
  
  Largura = Picture1(Idx).Width
  Altura = Picture1(Idx).Height
  
  Picture1(Idx).Cls
  
  'Cor linha de referência
  cor1 = QBColor(5)
  'Cor mancais
  cor2 = QBColor(1)
  'Cor Flecha
  cor3 = QBColor(12)
  'Cor linhas referência valor flecha
  cor4 = QBColor(10)
  
  '--- linhas de referencia
  Picture1(Idx).DrawWidth = 1
  Picture1(Idx).Line (0, Altura / 2)-(Largura, Altura / 2), cor1
  Picture1(Idx).Line (folga, 0)-(folga, Altura), cor1
  Picture1(Idx).Line (Largura - folga, 0)-(Largura - folga, Altura), cor1
  
  'Comprimento do eixo
  'Leixo
  Leixo = Val(frmMain.lblComp(1).Caption)
  
  'Comprimento útil
  Lu = Largura - 2 * folga
  'Fator de escala no comprimento
  FeLu = Lu / Leixo
  
  Posmd = Val(frmMain.txtEntrada(12).Text)
  Posmt = Val(frmMain.txtEntrada(13).Text)
  'Desenha posição dos mancais
  'Mancal Dianteiro
  Picture1(Idx).DrawWidth = 1.5
  Picture1(Idx).Line (Posmd * FeLu + folga, Altura / 2)-(Posmd * FeLu + folga + 200, Altura / 2 + 200), cor2
  Picture1(Idx).Line (Posmd * FeLu + folga + 200, Altura / 2 + 200)-(Posmd * FeLu + folga - 200, Altura / 2 + 200), cor2
  Picture1(Idx).Line (Posmd * FeLu + folga - 200, Altura / 2 + 200)-(Posmd * FeLu + folga, Altura / 2), cor2
  'Mancal Traseiro
  Picture1(Idx).Line (Posmt * FeLu + folga, Altura / 2)-(Posmt * FeLu + folga + 200, Altura / 2 + 200), cor2
  Picture1(Idx).Line (Posmt * FeLu + folga + 200, Altura / 2 + 200)-(Posmt * FeLu + folga - 200, Altura / 2 + 200), cor2
  Picture1(Idx).Line (Posmt * FeLu + folga - 200, Altura / 2 + 200)-(Posmt * FeLu + folga, Altura / 2), cor2
  
  'Busca valores da posição
  If (Dir(flPs) = "") Then
    Exit Sub
  End If
  Open flPs For Input As 3
  cont = 0
  While Not EOF(3)
    Input #3, lido
    ReDim Preserve posi(cont)
    posi(cont) = lido
    cont = cont + 1
  Wend
  Close #3
  
  If (Idx = 0) Then
    flDf = flVt
  Else
    flDf = flHr
  End If
  'Busca valores da deflexão
  If (Dir(flDf) = "") Then
    Exit Sub
  End If
  Open flDf For Input As 4
  cont = 0
  While Not EOF(4)
    Input #4, lido$
    ReDim Preserve defl(cont)
    defl(cont) = -lido$
    cont = cont + 1
  Wend
  Close #4
  
  'Fator de escala para a flecha
  fe = Val(Text1(Idx).Text)
  If (fe < 200) Then
    fe = 200
    Text1(Idx).Text = 200
  End If
  
  'Desenha linhas e valores de referência
  Picture1(Idx).DrawWidth = 1
  
  If (fe >= 1600) Then
    salto = 0.1
  ElseIf (fe > 800) Then
    salto = 0.2
  ElseIf (fe > 400) Then
    salto = 0.4
  Else
    salto = 0.8
  End If
  
  For i = 1 To 20
    Picture1(Idx).CurrentX = 50
    Picture1(Idx).CurrentY = Altura / 2 - 100 + salto * i * fe
    Picture1(Idx).Print (-salto * i)
    Picture1(Idx).CurrentX = 65
    Picture1(Idx).CurrentY = Altura / 2 - 100 - salto * i * fe
    Picture1(Idx).Print (salto * i)
    Picture1(Idx).Line (folga, Altura / 2 + salto * i * fe)-(Largura - folga, Altura / 2 + salto * i * fe), cor4
    Picture1(Idx).Line (folga, Altura / 2 - salto * i * fe)-(Largura - folga, Altura / 2 - salto * i * fe), cor4
  Next i
  
  'Escreve mancais
  Picture1(Idx).CurrentX = Posmd * FeLu + folga - 600
  Picture1(Idx).CurrentY = Altura / 2 + 250
  Picture1(Idx).Print "Mancal Dianteiro"
  Picture1(Idx).CurrentX = Posmt * FeLu + folga - 600
  Picture1(Idx).CurrentY = Altura / 2 + 250
  Picture1(Idx).Print "Mancal Traseiro"
  
  'Desenha flecha
  Picture1(Idx).DrawWidth = 1.5
  For j = 0 To cont - 2
    X1 = Val(posi$(j))
    Y1 = Val(defl$(j))
    X2 = Val(posi$(j + 1))
    Y2 = Val(defl$(j + 1))
    Picture1(Idx).Line (X1 * FeLu + folga, Y1 * fe + Altura / 2)-(X2 * FeLu + folga, Y2 * fe + Altura / 2), cor3
  Next j
  Exit Sub
Fimgrafico_flecha:
  MsgBox "Erro gerando gráfico da flecha:" & vbCr & Err.Description, vbCritical, "Erro!"
End Sub

Private Sub Command1_Click()
  Unload Me
End Sub

Private Sub Picture1_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
  If (Button = 1) Then
    Text1(Index).Text = Val(Text1(Index).Text) + 200
  ElseIf (Button = 2) Then
    Text1(Index).Text = Val(Text1(Index).Text) - 200
  End If
  Call Desenha_Flecha(Index)
End Sub
