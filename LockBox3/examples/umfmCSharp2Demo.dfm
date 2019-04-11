object mfmCSharp2Demo: TmfmCSharp2Demo
  Left = 0
  Top = 0
  Caption = 'C# Demo 2'
  ClientHeight = 424
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    554
    424)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPlaintext: TLabel
    Left = 210
    Top = 96
    Width = 289
    Height = 13
    Caption = 'Plaintext datum (encoded as UTF-16LE; CR.LF line endings):'
    FocusControl = memoPlaintext
  end
  object edtKey: TLabeledEdit
    Left = 8
    Top = 24
    Width = 538
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 171
    EditLabel.Height = 13
    EditLabel.Caption = 'Key (32 bytes encoded as base64):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '0f2EYYpNRTE16ve7dYxbvGuk+BHR/YRhik1FMTXq97s='
  end
  object edtIV: TLabeledEdit
    Left = 8
    Top = 72
    Width = 538
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 261
    EditLabel.Height = 13
    EditLabel.Caption = 'Initialization Vector (IV; 16 bytes encoded as base64):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = 'kHLzYvYBxooAAAAAAAAAAA=='
  end
  object rdChain: TRadioGroup
    Left = 8
    Top = 99
    Width = 185
    Height = 102
    Caption = 'Chaining mode'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'CBC'
      'CFB'
      'CFB - 8 bit'
      'CTR'
      'OFB')
    TabOrder = 2
  end
  object memoPlaintext: TMemo
    Left = 208
    Top = 112
    Width = 338
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object edtCiphertext: TLabeledEdit
    Left = 8
    Top = 224
    Width = 538
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 399
    EditLabel.Height = 13
    EditLabel.Caption = 
      'Ciphertext datum (encoded as base64; byte count must be a whole ' +
      'multiple of 16):'
    TabOrder = 4
    Text = 'G6DR0607ZXRu24envBxPRA=='
  end
  object Button1: TButton
    Left = 8
    Top = 251
    Width = 273
    Height = 25
    Action = actEncrypt
    TabOrder = 5
  end
  object btnDecrypt: TButton
    Left = 287
    Top = 251
    Width = 259
    Height = 25
    Action = actDecrypt
    DropDownMenu = PopupMenu1
    Style = bsSplitButton
    TabOrder = 6
  end
  object memoResults: TMemo
    Left = 8
    Top = 282
    Width = 538
    Height = 134
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clInfoBk
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object actmngrMainActions: TActionManager
    Left = 256
    Top = 144
    StyleName = 'Platform Default'
    object actEncrypt: TAction
      Caption = 'Encrypt by AES-256 using C# style padding'
      OnExecute = actEncryptExecute
    end
    object actDecrypt: TAction
      Caption = 'Decrypt by AES-256 using C# style padding'
      OnExecute = actDecryptExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 296
    Top = 336
    object mniDecryptDefault: TMenuItem
      Caption = 'Default'
      OnClick = mniDecryptDefaultClick
    end
    object mniRinjandelManagedDemo: TMenuItem
      Caption = 'Rinjandel Managed (Demo)'
      OnClick = mniRinjandelManagedDemoClick
    end
    object mniLockbox2Rinjandel: TMenuItem
      Caption = 'Lockbox 2 Rinjandel'
      OnClick = mniLockbox2RinjandelClick
    end
  end
end
