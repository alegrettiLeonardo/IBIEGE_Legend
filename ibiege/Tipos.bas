Attribute VB_Name = "Tipos"
Option Explicit

Public Type dado_eixo
    id As Long
    descricao As String
    tipo As Byte
    valor As Long
End Type

Public Type OP
    id As Long
    nr_ref As Long
    descricao As String
    usuario As String
    data As String
    dados_eixo() As dado_eixo
    numero_dados As Integer
End Type
