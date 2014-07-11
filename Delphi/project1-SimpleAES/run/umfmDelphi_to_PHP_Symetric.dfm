object mfmDelphi_to_PHP_Symetric: TmfmDelphi_to_PHP_Symetric
  Left = 0
  Top = 0
  Caption = 'Delphi-to-PHP Symetric Cipher Encoder'
  ClientHeight = 294
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  DesignSize = (
    562
    294)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPassword: TLabel
    Left = 344
    Top = 8
    Width = 90
    Height = 13
    Caption = 'Password (UTF-8):'
    FocusControl = edtPassword
  end
  object lblPlaintext: TLabel
    Left = 255
    Top = 55
    Width = 46
    Height = 13
    Caption = 'Plaintext:'
    FocusControl = memoPlaintext
  end
  object lblSeed: TLabel
    Left = 8
    Top = 95
    Width = 57
    Height = 13
    Caption = 'PRNG seed:'
    FocusControl = edtSeed
  end
  object rgTestVectors: TRadioGroup
    Left = 0
    Top = 0
    Width = 161
    Height = 89
    Caption = 'Test case'
    ItemIndex = 2
    Items.Strings = (
      'Test Vector 1 (non-round)'
      'Test Vector 2 (round)'
      'Custom')
    TabOrder = 0
    OnClick = rgTestVectorsClick
  end
  object rgChainMode: TRadioGroup
    Left = 167
    Top = 0
    Width = 82
    Height = 89
    Caption = 'Chain mode'
    ItemIndex = 0
    Items.Strings = (
      'CFB'
      'CBC'
      'ECB')
    TabOrder = 1
    OnClick = rgChainModeClick
  end
  object edtPassword: TEdit
    Left = 344
    Top = 24
    Width = 217
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = '[Password goes here]'
  end
  object memoPlaintext: TMemo
    Left = 255
    Top = 70
    Width = 306
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      '[Plaintext message goes here]')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object memoOutput: TMemo
    Left = 0
    Top = 168
    Width = 561
    Height = 129
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '[output goes here]')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object btnEncrypt: TButton
    Left = 86
    Top = 137
    Width = 75
    Height = 25
    Action = actEncrypt
    TabOrder = 5
  end
  object edtSeed: TEdit
    Left = 69
    Top = 95
    Width = 100
    Height = 21
    TabOrder = 6
    Text = '1'
  end
  object btnRandomize: TButton
    Left = 175
    Top = 95
    Width = 74
    Height = 25
    Action = actRandomize
    TabOrder = 7
  end
  object rgCipher: TRadioGroup
    Left = 255
    Top = 0
    Width = 83
    Height = 49
    Caption = 'Cipher'
    ItemIndex = 0
    Items.Strings = (
      'AES-128')
    TabOrder = 8
  end
  object cryptoMain: TCryptographicLibrary
    Left = 312
    Top = 104
  end
  object codecAES: TCodec
    AsymetricKeySizeInBits = 1024
    AdvancedOptions2 = []
    CryptoLibrary = cryptoMain
    Left = 400
    Top = 104
    StreamCipherId = 'native.StreamToBlock'
    BlockCipherId = 'native.AES-128'
    ChainId = 'native.CFB'
  end
  object actlstMain: TActionList
    Left = 472
    Top = 104
    object actEncrypt: TAction
      Caption = 'Encrypt'
      OnExecute = actEncryptExecute
      OnUpdate = actEncryptUpdate
    end
    object actRandomize: TAction
      Caption = 'Randomize'
      OnExecute = actRandomizeExecute
      OnUpdate = actRandomizeUpdate
    end
  end
end
