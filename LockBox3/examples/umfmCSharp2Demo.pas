/// <summary>
///   Based on Demo by Sean B. Durkin
/// </summary>
/// <seealso href="https://stackoverflow.com/questions/27284506/turbopower-lockbox3-can-i-control-initialization-vector-and-padding-for-aes-25">
///   Turbopower Lockbox3 - Can I control initialization vector and padding for AES-256 encryption? Request for encryption equivalent
/// </seealso>
unit umfmCSharp2Demo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ExtCtrls,
  TPLB3.Codec, TPLB3.CryptographicLibrary, TPLB3.BlockCipher, Vcl.Menus;

type
  /// <summary>
  ///   Transformations required to Decrypt from other sources.
  /// </summary>
  /// <seealso href="https://stackoverflow.com/a/38630583/5042682">
  ///   Lockbox 2's RSA encryption is not compatible with anybody else's RSA decryption
  /// </seealso>
  TLB3Transformations = set of (
    /// <summary>
    ///   Prepends 8 bytes of Zero
    /// </summary>
    lbtfPrependFakeIV,
    /// <summary>
    ///   Forces the use of a custom IV
    /// </summary>
    lbtfSetCustomIV,
    /// <summary>
    ///   Decodes as ANSI, default is Unicode
    /// </summary>
    lbtfFinalDecodeANSI,
    /// <summary>
    ///   Decodes as HEX, default is Unicode
    /// </summary>
    lbtfFinalDecodeHex,
    /// <summary>
    ///   Strips the First Block/IV (half of key length) from LB2 decoded result
    /// </summary>
    lbtfDiscardLB2IV);

  TmfmCSharp2Demo = class(TForm)
    edtKey: TLabeledEdit;
    edtIV: TLabeledEdit;
    rdChain: TRadioGroup;
    memoPlaintext: TMemo;
    lblPlaintext: TLabel;
    edtCiphertext: TLabeledEdit;
    actmngrMainActions: TActionManager;
    Button1: TButton;
    btnDecrypt: TButton;
    memoResults: TMemo;
    actEncrypt: TAction;
    actDecrypt: TAction;
    PopupMenu1: TPopupMenu;
    mniDecryptDefault: TMenuItem;
    mniRinjandelManagedDemo: TMenuItem;
    mniLockbox2Rinjandel: TMenuItem;
    procedure actEncryptExecute(Sender: TObject);
    procedure actDecryptExecute(Sender: TObject);
    procedure mniLockbox2RinjandelClick(Sender: TObject);
    procedure Loaded; Override;
    procedure mniDecryptDefaultClick(Sender: TObject);
    procedure mniRinjandelManagedDemoClick(Sender: TObject);
  private
    procedure Put(const Line: string; const Args: array of const);
    procedure Setup(var Codec: TCodec);
    procedure SetIV(var Codec: TCodec);
    procedure Decrypt(ALB3Transformations: TLB3Transformations);
  end;

var
  mfmCSharp2Demo: TmfmCSharp2Demo;

implementation

uses
  TPLB3.Constants, TPLB3.StreamUtils, TPLB3.StreamToBlock, TPLB3.BaseNonVisualComponent;

{$R *.dfm}

type
  TCSharpPadder = class(TTPLb_BaseNonVisualComponent)
  private
    FIV: TBytes;
  public
    constructor Create(AOwner : TComponent; const AIV: TBytes); reintroduce;
    procedure SetIV_For_Decrypt(Value: System.Classes.TMemoryStream);
  end;

function StripSpace(const Spacey: string): string;
var
  Builder: TStringBuilder;
  i: integer;
begin
  Builder := TStringBuilder.Create;
  Builder.Append(Spacey);
  for i := Builder.Length - 1 downto 0 do
    if CharInSet(Builder.Chars[i], [' ', #9, #$A, #$D]) then Builder.Remove(i, 1);
  result := Builder.ToString;
  Builder.Free
end;

procedure TmfmCSharp2Demo.Setup(var Codec: TCodec);
begin
  Codec := TCodec.Create(nil);
  Codec.AdvancedOptions2 := [optSuppressIVSeedPrependage];
  Codec.CryptoLibrary := TCryptographicLibrary.Create(Codec);
  Codec.StreamCipherId := BlockCipher_ProgId;
  Codec.BlockCipherId := Format(AES_ProgId, [256]);
  case rdChain.ItemIndex of
    0: Codec.ChainModeId := CBC_ProgId;
    1: Codec.ChainModeId := CFB_ProgId;
    2: Codec.ChainModeId := CFB8bit_ProgId;
    3: Codec.ChainModeId := CTR_ProgId;
    4: Codec.ChainModeId := OFB_ProgId;
  end
end;

procedure TmfmCSharp2Demo.SetIV(var Codec: TCodec);
var
  IV_AsStream: TStream;
  IV: TBytes;
  CSharpPadder: TCSharpPadder;
begin
  IV_AsStream := TMemoryStream.Create;
  try
    Base64_to_stream(StripSpace(edtIV.Text), IV_AsStream);
    IV_AsStream.Position := 0;
    SetLength(IV, IV_AsStream.Size);
    IV_AsStream.ReadData(IV, Length(IV))
  finally
    IV_AsStream.Free
  end;
  if Length(IV) <> 16 then raise Exception.Create('To encrypt C# style, you must supply a 16 byte IV.');
  CSharpPadder := TCSharpPadder.Create(Codec, IV);
  Codec.OnSetIV := CSharpPadder.SetIV_For_Decrypt;
end;

procedure TmfmCSharp2Demo.actEncryptExecute(Sender: TObject);
var
  Key: TStream;
  Codec: TCodec;
  s: string;
  Builder: TStringBuilder;
  BlockLenInUnits, PadIdx: integer;
  Ciphertext: TStream;
  Temp: TBytes;
  Len: integer;
begin
  try
    // 1. Create the components dynamically.
    // An alternative would be to use design-time properties.
    Setup(Codec);
    Ciphertext := TMemoryStream.Create;
    try

      // 2. Pad the plaintext, C# style.
      // This is an inferior style of padding. Only do it if you need
      // interoperability with C# libraries.
      Builder := TStringBuilder.Create;
      Builder.Append(memoPlaintext.Lines.Text);
      BlockLenInUnits := 16 div SizeOf(Char);
      for PadIdx := 1 to (BlockLenInUnits - (Builder.Length mod BlockLenInUnits)) mod BlockLenInUnits do
        // C# just pads out with zeros.
            Builder.Append(Char(#0));
      s := Builder.ToString;
      Builder.Free;

      // 3. Read in the IV.
      // Normally IV is set automatically by TP Lockbox 3, but C# library is pretty stupid
      // and requires manual management.
      SetIV(Codec);

      // 4. Read in the binary key (instead of setting a string password),
      // and initialize the Codec.
      Key := TMemoryStream.Create;
      try
        Base64_to_stream(StripSpace(edtKey.Text), Key);
        Key.Position := 0;
        Codec.InitFromStream(Key);
      finally
        Key.Free
      end;

      // 5. Encrypt using a UTF-16LE format.
      Codec.Begin_EncryptMemory(Ciphertext);
      if s.Length > 0 then Codec.EncryptMemory(s[Low(s)], s.Length * SizeOf(Char));
      Codec.End_EncryptMemory;

      // 8. TP LockBox 3 automatically seeds (prepend) the cipher stream with
      // the low 8 bytes of the IV. But we don't need this for C# interoperability,
      // so remove these bytes from the stream.
      if Ciphertext.Size <> 0 then begin
        Ciphertext.Position := 8;
        Len := Ciphertext.Size - Ciphertext.Position;
        SetLength(Temp, Len);
        if Len > 0 then Ciphertext.ReadData(Temp, Len);
        Ciphertext.Size := Len;
        Ciphertext.Position := 0;
        Ciphertext.WriteData(Temp, Len)
      end;
      edtCiphertext.Text := Stream_to_Base64(Ciphertext);

      // 9. Tidy up
    finally
      Ciphertext.Free;
      Codec.Free;
    end;
    Put('Encryption done.', [])
  except
    on E: Exception do begin
      Put('Encryption failed: %s: %s', [E.ClassName, E.Message]);
    end
  end;
end;

procedure TmfmCSharp2Demo.actDecryptExecute(Sender: TObject);
begin
  Decrypt([lbtfPrependFakeIV, lbtfSetCustomIV, lbtfFinalDecodeANSI]);
end;

procedure TmfmCSharp2Demo.Decrypt(ALB3Transformations: TLB3Transformations);
var
  Key: TStream;
  Codec: TCodec;
  Plaintext: TStream;
  Padder: TObject;
  Temp, Hex: TBytes;
  Len, BlockSize: integer;
  CipherStream: TMemoryStream;
  idx, Count: integer;
begin
  Padder := nil;
  try
    // 1. Setup
    Setup(Codec);
    Plaintext := TMemoryStream.Create;
    try

      // 2. Read in the IV.
      // Normally IV is set automatically by TP Lockbox 3, but C# library is pretty stupid
      // and requires manual management.
      if lbtfSetCustomIV in ALB3Transformations then
         SetIV(Codec);
      CipherStream := TMemoryStream.Create;
      try
        // 3. Read in the binary key (instead of setting a string password),
        // and initialize the Codec.
        Key := TMemoryStream.Create;
        try
          Base64_to_stream(StripSpace(edtKey.Text), Key);
          Key.Position := 0;
          Codec.InitFromStream(Key);
          BlockSize := Key.Size div 2;
        finally
          Key.Free
        end;

        // 3. Fake a prepended IV. This value doesn't matter any way.
        // It gets overwritten by the Padder object.
        if lbtfPrependFakeIV in ALB3Transformations then begin
          Temp := TBytes.Create(0, 0, 0, 0, 0, 0, 0, 0);
          CipherStream.WriteData(Temp, 8);
        end;

        // 4. Decrypt using a UTF-16LE format.
        Codec.Begin_DecryptMemory(Plaintext);
        Base64_to_stream(StripSpace(edtCiphertext.Text), CipherStream);
        Codec.DecryptMemory(CipherStream.Memory^, CipherStream.Size);
        Codec.End_DecryptMemory;
      finally
        If Assigned(Padder) then FreeAndNil(Padder);
        CipherStream.Free
      end;

      // Discard the Lockbox 2 IV
      if lbtfDiscardLB2IV in ALB3Transformations then
         Plaintext.Position := BlockSize
      else Plaintext.Position := 0;
      // 5. Strip C# style padding.
      SetLength(Temp, Plaintext.Size - Plaintext.Position);
      Plaintext.ReadData(Temp, Length(Temp));
      Count := 0;
      Len := Length(Temp);
      if Len >= 2 then
        for idx := (Len div 2) - 1 downto 0 do begin // Detect how many padding zeroes.
          if (Temp[idx * 2] = 0) and (Temp[idx * 2 + 1] = 0) then continue;
          Count := (idx + 1) * 2;
          break
        end;

      if lbtfFinalDecodeANSI in ALB3Transformations then
        memoPlaintext.Text := TEncoding.ANSI.GetString(Temp, 0, Count)
      else if lbtfFinalDecodeHex in ALB3Transformations then begin
        SetLength(Hex, Count * 2);
        BinToHex(Temp, 0, Hex, 0, Count);
        memoPlaintext.Text := TEncoding.ANSI.GetString(Hex);
      end else memoPlaintext.Text := TEncoding.Unicode.GetString(Temp, 0, Count);

      // 9. Tidy up
    finally
      Plaintext.Free;
      Codec.Free;
    end;
    Put('Decryption done.', [])
  except
    on E: Exception do begin
      Put('Decryption failed: %s: %s', [E.ClassName, E.Message]);
    end
  end;
end;

procedure TmfmCSharp2Demo.Put(const Line: string; const Args: array of const);
var
  sLine: string;
begin
  sLine := Line;
  if Length(Args) > 0 then sLine := Format(sLine, Args);
  memoResults.Lines.Add(sLine)
end;

constructor TCSharpPadder.Create(AOwner : TComponent; const AIV: TBytes);
begin
  inherited Create(AOwner);
  FIV := AIV
end;

procedure TCSharpPadder.SetIV_For_Decrypt(Value: System.Classes.TMemoryStream);
begin
  Value.Size := Length(FIV);
  Value.Position := 0;
  Value.WriteData(FIV, Length(FIV))
end;

procedure TmfmCSharp2Demo.mniLockbox2RinjandelClick(Sender: TObject);
const
  { Lockbox 2 Rinjandel }
  Key: array of byte = [$61,$A6,$02,$D1,$E6,$89,$87,$A3,$7C,$0B,$54,$D2,$64,$7D,$B9,$41,
                        $D0,$E6,$56,$DE,$CF,$A2,$5B,$6C,$76,$4A,$BB,$FA,$DB,$CD,$41,$2D];
  Ciphertext = 'h8MoOCMwzoP0mhSzEJ3NyOVgqNkCRFXnCQ2Td8rjgVzcuM0lSYFEfwT35YkEvedrwc8rBwefgGrSc4pY1Pdqlw==';
var stream : TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    stream.WriteData(key, Length(Key));
    stream.Position    := 0;
    edtKey.Text        := Stream_to_Base64(stream);
  finally
    stream.Free;
  end;
  rdChain.ItemIndex  := 0;
  edtIV.Text         := '';
  edtCiphertext.Text := Ciphertext;
  Put('Lockbox 2 Rinjandel (Demo), Plaintext=%s', ['Your lips are smoother than vasoline.']);
  Decrypt([lbtfPrependFakeIV, lbtfFinalDecodeANSI, lbtfDiscardLB2IV]);
end;

procedure TmfmCSharp2Demo.mniRinjandelManagedDemoClick(Sender: TObject);
{$REGION 'History'}
//  06-Apr-2019 - Quickly Test Decoding from other formats including LB2 Rinjandel
{$ENDREGION}
const
  { Rinjandel Managed: https://www.hanewin.net/encrypt/aes/aes-test.htm
      Key (256):	08090A0B0D0E0F10121314151718191A1C1D1E1F21222324262728292B2C2D2E
      Plaintext:	069A007FC76A459F98BAF917FEDF9521
      Ciphertext:	080e9517eb1677719acf728086040ae3 }
  Key : TBytes   = [8, 9, 10, 11, 13, 14, 15, 16, 18, 19, 20, 21, 23, 24, 25, 26, 28, 29, 30, 31, 33, 34, 35, 36, 38, 39, 40, 41, 43, 44, 45, 46];
  Ciphertext     = 'CA6VF+sWd3Gaz3KAhgQK4w==';
var stream : TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    stream.WriteData(key, Length(Key));
    stream.Position    := 0;
    edtKey.Text        := Stream_to_Base64(stream);
  finally
    stream.Free;
  end;
  rdChain.ItemIndex  := 0;
  edtIV.Text         := '';
  edtCiphertext.Text := Ciphertext;
  Put('Rinjandel Managed (Demo), Plaintext=%s', ['069A007FC76A459F98BAF917FEDF9521']);
  Decrypt([lbtfPrependFakeIV, lbtfFinalDecodeHex]);
end;

procedure TmfmCSharp2Demo.mniDecryptDefaultClick(Sender: TObject);
const
  {https://stackoverflow.com/questions/27284506/turbopower-lockbox3-can-i-control-initialization-vector-and-padding-for-aes-25}
  Key: array of byte = [$61,$A6,$02,$D1,$E6,$89,$87,$A3,$7C,$0B,$54,$D2,$64,$7D,$B9,$41,
                        $D0,$E6,$56,$DE,$CF,$A2,$5B,$6C,$76,$4A,$BB,$FA,$DB,$CD,$41,$2D];
  IV: array of byte  = [$86,$78,$1C,$D2,$66,$91,$F7,$91,$3B,$2A,$44,$10,$DF,$38,
                        $E4,$47];
  Ciphertext         = 'G6DR0607ZXRu24envBxPRA==';
var stream : TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    stream.WriteData(key, Length(Key));
    stream.Position    := 0;
    edtKey.Text        := Stream_to_Base64(stream);

    stream.size        := 0;
    stream.WriteData(IV, Length(IV));
    stream.Position    := 0;
    edtIV.Text         := Stream_to_Base64(stream);
  finally
    stream.Free
  end;
  rdChain.ItemIndex  := 0;
  edtCiphertext.Text := Ciphertext;
  Put('CSharp (Demo), Plaintext=%s', ['1']);
  Decrypt([lbtfPrependFakeIV, lbtfSetCustomIV, lbtfFinalDecodeANSI]);
end;

procedure TmfmCSharp2Demo.Loaded;
begin
  Inherited;
  mniDecryptDefault.Click;
end;

initialization
  System.ReportMemoryLeaksOnShutdown := True;

end.
