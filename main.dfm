object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'CoPiSave'
  ClientHeight = 49
  ClientWidth = 298
  Color = clBtnFace
  Constraints.MaxHeight = 88
  Constraints.MinHeight = 88
  Constraints.MinWidth = 314
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Consolas'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 298
    Height = 49
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Edit1: TLabeledEdit
      Left = 8
      Top = 20
      Width = 281
      Height = 21
      EditLabel.Width = 282
      EditLabel.Height = 13
      EditLabel.Caption = 'Auto save image in clipboard to: (double-click)'
      TabOrder = 0
      OnChange = Edit1Change
      OnDblClick = Edit1DblClick
    end
  end
  object Panel2: TPanel
    Left = 298
    Top = 0
    Width = 0
    Height = 49
    Align = alRight
    Caption = 'Panel2'
    TabOrder = 1
  end
  object fod: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Left = 16
  end
  object TrayIcon1: TTrayIcon
    Visible = True
    OnDblClick = TrayIcon1DblClick
    Left = 56
  end
end
