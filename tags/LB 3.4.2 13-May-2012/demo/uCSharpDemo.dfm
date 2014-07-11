object mfmCSharpDemo: TmfmCSharpDemo
  Left = 0
  Top = 0
  Caption = 'CSharp Demo (decryption from AES-128/ECB/Zerofill)'
  ClientHeight = 432
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    457
    432)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TFlowPanel
    Left = 0
    Top = 0
    Width = 458
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'pnlControls'
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 465
    object lblReferencePlaintext: TLabel
      Left = 1
      Top = 1
      Width = 150
      Height = 13
      AutoSize = False
      Caption = 'Reference plaintext file:'
      FocusControl = edtReferencePlaintext
    end
    object edtReferencePlaintext: TButtonedEdit
      Left = 151
      Top = 1
      Width = 300
      Height = 21
      Images = imglstGlyphs16x16
      RightButton.DisabledImageIndex = 2
      RightButton.ImageIndex = 0
      RightButton.PressedImageIndex = 1
      RightButton.Visible = True
      TabOrder = 0
      Text = 'ImpedanceTesting_c.dat'
      OnRightButtonClick = edtReferencePlaintextRightButtonClick
    end
    object lblCiphertext: TLabel
      Left = 1
      Top = 22
      Width = 150
      Height = 13
      AutoSize = False
      Caption = 'Ciphertext file:'
      FocusControl = edtCiphertext
    end
    object edtCiphertext: TButtonedEdit
      Left = 151
      Top = 22
      Width = 300
      Height = 21
      Images = imglstGlyphs16x16
      RightButton.DisabledImageIndex = 2
      RightButton.ImageIndex = 0
      RightButton.PressedImageIndex = 1
      RightButton.Visible = True
      TabOrder = 1
      Text = 'ImpedanceTesting.cdx3'
      OnRightButtonClick = edtReferencePlaintextRightButtonClick
    end
    object lblReconstructedPlaintext: TLabel
      Left = 1
      Top = 43
      Width = 150
      Height = 13
      AutoSize = False
      Caption = 'Reconstructed plaintext file:'
      FocusControl = edtReconstructedPlaintext
    end
    object edtReconstructedPlaintext: TButtonedEdit
      Left = 151
      Top = 43
      Width = 300
      Height = 21
      Images = imglstGlyphs16x16
      RightButton.DisabledImageIndex = 2
      RightButton.ImageIndex = 0
      RightButton.PressedImageIndex = 1
      RightButton.Visible = True
      TabOrder = 2
      Text = 'ImpedanceTesting_lockbox.dat'
      OnRightButtonClick = edtReferencePlaintextRightButtonClick
    end
  end
  object tlbrMain: TActionToolBar
    Left = 0
    Top = 406
    Width = 457
    Height = 26
    ActionManager = actmngrMainActions
    Align = alBottom
    Caption = 'tlbrMain'
    ColorMap.HighlightColor = 15660791
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 15660791
    Spacing = 0
  end
  object memoLog: TMemo
    Left = 0
    Top = 80
    Width = 458
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Color = clInfoBk
    Lines.Strings = (
      '[Text output will go here.]')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object CryptLib: TCryptographicLibrary
    Left = 384
    Top = 104
  end
  object Codec: TCodec
    AsymetricKeySizeInBits = 1024
    CryptoLibrary = CryptLib
    Left = 384
    Top = 160
    StreamCipherId = 'native.StreamToBlock'
    BlockCipherId = 'native.AES-128'
    ChainId = 'native.ECB'
  end
  object actmngrMainActions: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = actTest
            Caption = '&Test'
          end
          item
            Action = FileExit1
            ImageIndex = 3
          end>
        ActionBar = tlbrMain
      end>
    Images = imglstGlyphs16x16
    Left = 384
    Top = 224
    StyleName = 'Platform Default'
    object actTest: TAction
      Caption = 'Test'
      OnExecute = actTestExecute
    end
    object FileExit1: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 3
    end
  end
  object imglstGlyphs16x16: TImageList
    Left = 384
    Top = 296
    Bitmap = {
      494C010104000500040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00000000000000000000000000000000000274AC000274AC000274
      AC000274AC000274AC000274AC000274AC000274AC000274AC000274AC000274
      AC000274AC000274AC0000000000000000000000000092929200929292009292
      9200929292009292920092929200929292009292920092929200929292009292
      9200929292009292920000000000000000008686860086868600868686008686
      8600868686008686860086868600868686008686860086868600FFFFFF00C0C0
      C00086868600C0C0C000FFFFFF00C0C0C000078DBE0025A1D10071C6E80084D7
      FA0066CDF90065CDF90065CDF90065CDF90065CDF80065CDF90065CDF80066CE
      F9003AADD8001999C90000000000000000000274AC0048BCF6000274AC008CD8
      FA004BBFF7004ABFF6004ABFF7004ABFF7004ABFF6004ABFF7004ABFF6004BBF
      F6002398CC0097E0F2000274AC000000000092929200C7C7C70092929200DFDF
      DF00C9C9C900C8C8C800C9C9C900C9C9C900C8C8C800C9C9C900C8C8C800C9C9
      C900AEAEAE00E0E0E00092929200000000008686860086868600868686008686
      860086868600868686008686860086868600868686008686860086868600FFFF
      FF0086868600FFFFFF008686860086868600078DBE004CBCE70039A8D100A0E2
      FB006FD4FA006FD4F9006ED4FA006FD4F9006FD4FA006FD4FA006FD4FA006ED4
      F9003EB1D900C9F0F300078DBE00000000000274AC004FC4F7000274AC0092DD
      FB0054C7F80054C7F70053C7F80054C7F70054C7F80054C7F80054C7F80053C7
      F700279DCE009DE3F2000274AC000000000092929200CACACA0092929200E1E1
      E100CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCCCC00CBCB
      CB00AFAFAF00E2E2E20092929200000000008000000080000000800000008000
      00000000000000000000868686008686860086868600FFFFFF00FFFFFF00FFFF
      FF0080000000800000008000000080000000078DBE0072D6FA00078DBE00AEE9
      FC0079DCFB0079DCFB0079DCFB0079DCFB0079DCFB007ADCFB0079DCFA0079DC
      FA0044B5D900C9F0F300078DBE00000000000274AC0057CAF8000274AC0099E3
      FB005ED1FA005ED1FA005ED1FA005ED1FA005ED1FA005FD1FA005ED1F8005ED1
      F8002CA1CE00A3E9F3000274AC000000000092929200CDCDCD0092929200E3E3
      E300D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000B1B1B100E4E4E40092929200000000000000000000000000000000008000
      0000FF00000080000000000000000000000086868600FFFFFF00FFFFFF00FFFF
      FF0080000000000000000000000000000000078DBE0079DDFB001899C7009ADF
      F30092E7FC0084E4FB0083E4FC0083E4FC0084E4FC0083E4FC0083E4FB0084E5
      FC0048B9DA00C9F0F3001496C400000000000274AC005ED3FA000274AC00A1E9
      FC0069DCFA006ADCFA0069DCFB0069DCFB006ADCFB0069DCFB0069DCFA006ADD
      FB002FA6CF00A9EEF3000274AC000000000092929200D0D0D00092929200E6E6
      E600D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400B3B3B300E5E5E50092929200000000000000000000000000000000008000
      000080000000FF0000008000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0080000000000000000000000000000000078DBE0082E3FC0043B7DC0065C2
      E000ABF0FC008DEBFC008DEBFC008DEBFD008DEBFD008DEBFC008DEBFD008DEB
      FC004CBBDA00C9F0F300C9F0F300078DBE000274AC0068DAFB000274AC00A7EF
      FC0074E5FB0074E5FB0074E5FB0074E5FC0074E5FC0074E5FB0074E5FC0074E5
      FB0033A9CF00ACF0F4000274AC000000000092929200D4D4D40092929200E7E7
      E700D7D7D700D7D7D700D7D7D700D8D8D800D8D8D800D7D7D700D8D8D800D7D7
      D700B5B5B500E7E7E70092929200000000000000000000000000000000008000
      0000FF00000080000000FF00000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0080000000000000000000000000000000078DBE008AEAFC0077DCF300219C
      C700FEFFFF00C8F7FD00C9F7FD00C9F7FD00C9F7FE00C8F7FE00C9F7FD00C8F7
      FE009BD5E600EAFEFE00D2F3F800078DBE000274AC0070E3FB000274AC00FFFF
      FF00BAF4FE00B8F4FE00BAF4FE00BAF4FE00BAF4FE00B8F4FE00BAF4FE00B8F4
      FE0083C9E000D4F7FA000274AC000000000092929200D7D7D70092929200FFFF
      FF00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDEDED00EDED
      ED00D5D5D500F3F3F30092929200000000000000000000000000000000008000
      000080000000FF0000008000000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      000080000000000000000000000000000000078DBE0093F0FE0093F0FD001697
      C500078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE000274AC007AEBFE000274AC000274
      AC000274AC000274AC000274AC000274AC000274AC000274AC000274AC000274
      AC000274AC000274AC000274AC000000000092929200DADADA00929292009292
      9200929292009292920092929200929292009292920092929200929292009292
      9200929292009292920092929200000000000000000000000000000000008000
      0000FF00000080000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF0080000000000000000000000000000000078DBE009BF5FE009AF6FE009AF6
      FE009BF5FD009BF6FE009AF6FE009BF5FE009AF6FD009BF5FE009AF6FE009AF6
      FE000989BA000000000000000000000000000274AC0083F2FE0082F3FE0082F3
      FE0083F2FC0083F3FE0082F3FE0083F2FE0082F3FC0083F2FE0082F3FE0082F3
      FE00036FA70000000000000000000000000092929200DDDDDD00DDDDDD00DDDD
      DD00DDDDDD00DDDDDD00DDDDDD00DDDDDD00DCDCDC00DDDDDD00DDDDDD00DDDD
      DD00929292000000000000000000000000000000000000000000000000008000
      000080000000FF0000008000000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      000080000000000000000000000000000000078DBE00FEFEFE00A0FBFF00A0FB
      FE00A0FBFE00A1FAFE00A1FBFE00A0FAFE00A1FBFE00A1FBFF00A0FBFF00A1FB
      FF000989BA000000000000000000000000000274AC00FEFEFE0089FAFF0089FA
      FE0089FAFE008AF8FE008AFAFE0089F8FE008AFAFE008AFAFF0089FAFF008AFA
      FF00036FA70000000000000000000000000092929200FFFFFF00DFDFDF00DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00E0E0E000DFDFDF00E0E0
      E000929292000000000000000000000000000000000000000000000000008000
      0000FF00000080000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF008000000000000000000000000000000000000000078DBE00FEFEFE00A5FE
      FF00A5FEFF00A5FEFF00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE0000000000000000000000000000000000000000000274AC00FEFEFE008FFE
      FF008FFEFF008FFEFF000274AC000274AC000274AC000274AC000274AC000274
      AC00000000000000000000000000000000000000000092929200FFFFFF00E1E1
      E100E1E1E100E1E1E10092929200929292009292920092929200929292009292
      9200000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000008000000080000000800000008000
      0000800000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000274AC000274
      AC000274AC000274AC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000929292009292
      9200929292009292920000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000800000008000000080000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000080078003800300000003000100010000
      0001000100010000000100010001E007000100010001E007000000010001E007
      000000010001E007000000010001E007000700070007E007000700070007E007
      800F800F800FE007C3FFC3FFC3FFFFFFFFFFFFFFFFFFF81FFFFFFFFFFFFFF81F
      FFFFFFFFFFFFF81FFFFFFFFFFFFFFFFF}
  end
  object dlgOpenReadFile: TOpenDialog
    InitialDir = '..\..\resources\demo'
    Options = [ofReadOnly, ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 288
    Top = 112
  end
  object dlgSelectReconFile: TOpenDialog
    InitialDir = '..\..\resources\demo'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Select Reconstructed plaintext file (output)'
    Left = 288
    Top = 168
  end
end