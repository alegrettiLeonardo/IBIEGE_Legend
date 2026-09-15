Attribute VB_Name = "Module1"
Public Type STARTUPINFO 'exec. programas
  cb As Long
  lpReserved As String
  lpDesktop As String
  lpTitle As String
  dwX As Long
  dwY As Long
  dwXSize As Long
  dwYSize As Long
  dwXCountChars As Long
  dwYCountChars As Long
  dwFillAttribute As Long
  dwFlags As Long
  wShowWindow As Integer
  cbReserved2 As Integer
  lpReserved2 As Long
  hStdInput As Long
  hStdOutput As Long
  hStdError As Long
End Type
Public Type PROCESS_INFORMATION 'exec programas
  hProcess As Long
  hThread As Long
  dwProcessID As Long
  dwThreadID As Long
End Type

Public Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
Public Declare Function CreateProcessA Lib "kernel32" (ByVal lpApplicationName As Long, ByVal lpCommandLine As String, ByVal lpProcessAttributes As Long, ByVal lpThreadAttributes As Long, ByVal bInheritHandles As Long, ByVal dwCreationFlags As Long, ByVal lpEnvironment As Long, ByVal lpCurrentdirectory As Long, lpStartupInfo As STARTUPINFO, lpProcessInformation As PROCESS_INFORMATION) As Long
Public Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long


Public Function ExecCmd(ByVal CmdLine As String, ByVal sWait As Long, Optional HideOut As Boolean) As Long
  'executa programas externos sincronamente
  Dim Proc As PROCESS_INFORMATION
  Dim Start As STARTUPINFO
  Dim RetP As Long
  'Const Normal_Priority_Class = &H20&, High_Priority_Class = &H80
  'mostra ou nao tela do processo
   Start.dwFlags = IIf(Not HideOut, 0, 1)
  'Initialize the STARTUPINFO structure:
  Start.cb = Len(Start)
  'start the shelled application:
  CreateProcessA 0&, CmdLine, 0&, 0&, 1&, &H80, 0&, 0&, Start, Proc
  'Wait sWait miliseconds for the shelled application to finish:
  RetP = WaitForSingleObject(Proc.hProcess, sWait)
  CloseHandle Proc.hProcess
  CloseHandle Proc.hThread
  ExecCmd = RetP
  '
End Function
