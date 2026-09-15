VERSION 5.00
Begin VB.Form Form7 
   Caption         =   "Gráfico da Flecha do Eixo"
   ClientHeight    =   3165
   ClientLeft      =   3255
   ClientTop       =   2670
   ClientWidth     =   9270
   LinkTopic       =   "Form7"
   ScaleHeight     =   3165
   ScaleWidth      =   9270
   Begin VB.TextBox Text1 
      Alignment       =   2  'Center
      Height          =   285
      Left            =   120
      Locked          =   -1  'True
      TabIndex        =   2
      Text            =   "4000"
      Top             =   2670
      Width           =   795
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Height          =   345
      Left            =   7950
      TabIndex        =   1
      Top             =   2730
      Width           =   1185
   End
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      BackColor       =   &H8000000E&
      Height          =   2500
      Left            =   120
      ScaleHeight     =   2445
      ScaleWidth      =   8940
      TabIndex        =   0
      Top             =   90
      Width           =   9000
   End
End
Attribute VB_Name = "Form7"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Public Sub Desenha_Flecha()
Dim posi$(200)
Dim defl$(200)

On Local Error GoTo Fimgrafico_flecha

largura = Form7.Picture1.Width
altura = Form7.Picture1.Height
folga = 400
Form7.Picture1.Cls

'Cor linha de referência
cor1 = QBColor(5)
'Cor mancais
cor2 = QBColor(1)
'Cor Flecha
cor3 = QBColor(12)
'Cor linhas referência valor flecha
cor4 = QBColor(10)

'--- linhas de referencia
Form7.Picture1.DrawWidth = 1
Form7.Picture1.Line (0, altura / 2)-(largura, altura / 2), cor1
Form7.Picture1.Line (folga, 0)-(folga, altura), cor1
Form7.Picture1.Line (largura - folga, 0)-(largura - folga, altura), cor1

'Comprimento do eixo
'Leixo
Leixo = 0

For i = 1 To Form1.Grid4.Rows - 1
  Form1.Grid4.Col = 1
  Form1.Grid4.Row = i
  Leixo = Leixo + Form1.Grid4.Text
Next i

'Comprimento útil
Lu = largura - 2 * folga
'Fator de escala no comprimento
FeLu = Lu / Leixo

'Posição do Mancal dianteiro
Form1.Grid4.Col = 1
Form1.Grid4.Row = 1
Lee1 = Val(Form1.Grid4.Text) 'Dist. da extremidade do eixo ao final do primeiro escalonamento OK

Lcee1 = Lee1 / 2 'Dist. da extremidade do eixo ate o centro do primeiro escalonamento OK

Form1.Grid4.Row = 1
Form1.Grid4.Col = 0
pm0 = Val(Form1.Grid4.Text)

For i = 1 To Form1.Grid4.Rows - 1
    Form1.Grid4.Row = i
    Form1.Grid4.Col = 3
    If Form1.Grid4.Text = "mancalE" Then
        Form1.Grid4.Row = i
        Form1.Grid4.Col = 0
        pm1 = Val(Form1.Grid4.Text)
    End If
    If Form1.Grid4.Text = "mancalD" Then
        Form1.Grid4.Row = i
        Form1.Grid4.Col = 0
        pm2 = Val(Form1.Grid4.Text)
    End If
Next i

Lecm = -pm0 + pm1 + Lcee1 'Dist. da extremidade do eixo ao mancal dianteiro
Lcm = -pm1 + pm2 'Dist. entre mancais OK
Posmd = Lecm
'Posição do Mancal traseiro
Posmt = Lecm + Lcm

'Desenha posição dos mancais
'Mancal Dianteiro
Form7.Picture1.DrawWidth = 1.5
Form7.Picture1.Line (Posmd * FeLu + folga, altura / 2)-(Posmd * FeLu + folga + 200, altura / 2 + 200), cor2
Form7.Picture1.Line (Posmd * FeLu + folga + 200, altura / 2 + 200)-(Posmd * FeLu + folga - 200, altura / 2 + 200), cor2
Form7.Picture1.Line (Posmd * FeLu + folga - 200, altura / 2 + 200)-(Posmd * FeLu + folga, altura / 2), cor2
'Mancal Traseiro
Form7.Picture1.Line (Posmt * FeLu + folga, altura / 2)-(Posmt * FeLu + folga + 200, altura / 2 + 200), cor2
Form7.Picture1.Line (Posmt * FeLu + folga + 200, altura / 2 + 200)-(Posmt * FeLu + folga - 200, altura / 2 + 200), cor2
Form7.Picture1.Line (Posmt * FeLu + folga - 200, altura / 2 + 200)-(Posmt * FeLu + folga, altura / 2), cor2

'Busca valores da posição
If Dir("c:\tmp\posicao.prn") = "" Then
  Exit Sub
End If

Open "c:\tmp\posicao.prn" For Input As 3
cont = 0
While EOF(3) <> True
  Input #3, lido$
  posi$(cont) = lido$
  cont = cont + 1
Wend
Close #3

'Busca valores da deflexão
If Dir("c:\tmp\Deflexao.prn") = "" Then
  Exit Sub
End If

Open "c:\tmp\Deflexao.prn" For Input As 4
cont = 0
While EOF(4) <> True
  Input #4, lido$
  defl$(cont) = -lido$
  cont = cont + 1
Wend
Close #4

'Fator de escala para a flecha
fe = Val(Form7.Text1.Text)

If fe < 200 Then
  fe = 200
  Form7.Text1.Text = 200
End If

'Desenha linhas e valores de referência
Form7.Picture1.DrawWidth = 1

If fe >= 1600 Then
  salto = 0.1
ElseIf fe > 800 And fe < 1600 Then
  salto = 0.2
ElseIf fe > 400 And fe <= 800 Then
  salto = 0.4
Else
  salto = 0.8
End If

For i = 1 To 20
  Form7.Picture1.CurrentX = 50
  Form7.Picture1.CurrentY = altura / 2 - 100 + salto * i * fe
  Form7.Picture1.Print (-salto * i)
  Form7.Picture1.CurrentX = 65
  Form7.Picture1.CurrentY = altura / 2 - 100 - salto * i * fe
  Form7.Picture1.Print (salto * i)
  Form7.Picture1.Line (folga, altura / 2 + salto * i * fe)-(largura - folga, altura / 2 + salto * i * fe), cor4
  Form7.Picture1.Line (folga, altura / 2 - salto * i * fe)-(largura - folga, altura / 2 - salto * i * fe), cor4
Next i

'Escreve mancais
  Form7.Picture1.CurrentX = Posmd * FeLu + folga - 600
  Form7.Picture1.CurrentY = altura / 2 + 250
  Form7.Picture1.Print "Mancal Dianteiro"
  Form7.Picture1.CurrentX = Posmt * FeLu + folga - 600
  Form7.Picture1.CurrentY = altura / 2 + 250
  Form7.Picture1.Print "Mancal Traseiro"


'Desenha flecha
Form7.Picture1.DrawWidth = 1.5
For j = 0 To cont - 2
X1 = posi$(j)
Y1 = defl$(j)
X2 = posi$(j + 1)
Y2 = defl$(j + 1)
  Form7.Picture1.Line (X1 * FeLu + folga, Y1 * fe + altura / 2)-(X2 * FeLu + folga, Y2 * fe + altura / 2), cor3
Next j

Exit Sub
Fimgrafico_flecha:
  MsgBox "Erro gerando gráfico da flecha:" & vbCr & Err.Description, vbCritical, "Erro!"
  Close #3

End Sub

Private Sub Command1_Click()
  Form7.Hide
End Sub


Private Sub Picture1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Button = 1 Then
  Form7.Text1.Text = Val(Form7.Text1.Text) + 200
End If

If Button = 2 Then
  Form7.Text1.Text = Val(Form7.Text1.Text) - 200
End If

Call Desenha_Flecha

End Sub
