object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'CoPiSave'
  ClientHeight = 95
  ClientWidth = 298
  Color = clBtnFace
  Constraints.MaxHeight = 134
  Constraints.MinHeight = 134
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
    Height = 95
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnDblClick = FormDblClick
    ExplicitHeight = 89
    object PathOut: TLabeledEdit
      Left = 8
      Top = 16
      Width = 281
      Height = 21
      EditLabel.Width = 282
      EditLabel.Height = 13
      EditLabel.Caption = 'Auto save image in clipboard to: (double-click)'
      TabOrder = 0
      OnChange = EditChange
      OnDblClick = PathOutDblClick
    end
    object EditSuffix: TLabeledEdit
      Left = 8
      Top = 64
      Width = 281
      Height = 21
      EditLabel.Width = 36
      EditLabel.Height = 13
      EditLabel.Caption = 'Suffix'
      TabOrder = 1
      OnChange = EditChange
    end
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
