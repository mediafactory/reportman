VERSION 5.00
Object = "{D4D26F6B-6564-44F4-A913-03C91CE37740}#1.6#0"; "Reportman.ocx"
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   3195
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   3195
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin Reportman.ReportManX rp 
      Height          =   300
      Left            =   600
      TabIndex        =   1
      Top             =   1920
      Width           =   1125
      filename        =   ""
      Preview         =   0   'False
      ShowProgress    =   0   'False
      ShowPrintDialog =   0   'False
      Title           =   ""
      Language        =   0
      DoubleBuffered  =   0   'False
      Enabled         =   -1  'True
      Object.Visible         =   -1  'True
      Cursor          =   0
      HelpType        =   1
      HelpKeyword     =   ""
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   495
      Left            =   600
      TabIndex        =   0
      Top             =   480
      Width           =   1935
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
 rp.FileName = "c:\prog\toni\cvsroot\reportman\reportman\repman\repsamples\sample4.rep"
 rp.Preview = True
 rp.Execute
 Command1.Caption = Str(rp.Report.Params.Count)
End Sub
