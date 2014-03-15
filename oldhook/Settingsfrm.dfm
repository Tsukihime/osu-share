object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 93
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 34
    Top = 11
    Width = 14
    Height = 13
    Caption = 'IP:'
  end
  object Label2: TLabel
    Left = 194
    Top = 11
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object Label3: TLabel
    Left = 8
    Top = 38
    Width = 40
    Height = 13
    Caption = 'osu! dir:'
  end
  object IpEdit: TEdit
    Left = 54
    Top = 8
    Width = 134
    Height = 21
    TabOrder = 0
  end
  object OkButton: TButton
    Left = 191
    Top = 63
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object PortEdit: TEdit
    Left = 224
    Top = 8
    Width = 41
    Height = 21
    ReadOnly = True
    TabOrder = 2
    Text = '778'
  end
  object osuDirEdit: TEdit
    Left = 54
    Top = 35
    Width = 180
    Height = 21
    TabOrder = 3
  end
  object BrowseButton: TButton
    Left = 240
    Top = 34
    Width = 25
    Height = 23
    Caption = '...'
    TabOrder = 4
    OnClick = BrowseButtonClick
  end
  object AutoButton: TButton
    Left = 8
    Top = 62
    Width = 105
    Height = 25
    Caption = 'Auto configuratin'
    TabOrder = 5
    OnClick = AutoButtonClick
  end
  object OpenDialog: TOpenDialog
    Options = [ofEnableSizing]
    Left = 152
    Top = 64
  end
end
