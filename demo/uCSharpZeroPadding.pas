unit uCSharpZeroPadding;
interface
uses uTPLb_StreamCipher, uTPLb_BlockCipher, Classes, uTPLb_Decorators;

type

TCSharpZeroPadding = class( TInterfacedObject,
    IStreamCipher, ICryptoGraphicAlgorithm, IControlObject)
  private
    FBlockCipher: IBlockCipher;
    FChaining: IBlockChainingModel;

    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  Parameterize( const Params: IInterface): IStreamCipher;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;

    //  IControlObject = interface
    function ControlObject: TObject;

  public
    constructor Create;
  end;



implementation













uses SysUtils, Math, uTPLb_StreamUtils, uTPLb_CFB_Block, uTPLb_Constants,
     uTPLb_CFB_8Bit, uTPLb_PointerArithmetic, uTPLb_I18n;





type
TEncryptorDecryptor = class( TInterfacedObject)
  protected
    FChaining: IBlockChainingModel;
    FEmitBuffer: TMemoryStream;
    FBuffer: TMemoryStream;
    FBlockLen: integer;
    FCodec: IBlockCodec;
    FBlockCipher: IBlockCipher;
    FLenBuffer: integer;
    FChainState: TBlockChainLink;
    FIV: TMemoryStream;
    FDataCount: int64;
    FisAutoXOR: boolean;
    Fis8bitMode: boolean;
    FisEncrypting: boolean;
    FEmissionStream: TStream;

    procedure Reset;   virtual;
    procedure Emit( SourceBuf: TMemoryStream; EmitLen: integer = -1);
    // ^ Encrypt/Decrypt and then emit.

    procedure Emit_1stBuffer;
    function  ShortNonKeyStreamingChainMode: IBlockChainingModel;
    procedure Chained_Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Chained_Decrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);

  public
    constructor Start_EncDec(
      Key1: TSymetricKey; EmissionStream1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel;
      isEncrypting1: boolean);

    destructor Destroy; override;
  end;




TNoNonceEncryptor = class( TEncryptorDecryptor, IStreamEncryptor)
  private
    FCipherText: TStream;
    FPad: TMemoryStream;

    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;

  protected
    procedure Reset; override;

  public
    constructor Start_Encrypt(
      Key1: TSymetricKey; CipherText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    destructor Destroy; override;
  end;




TNoncibleEncryptor = class( TEncryptorDecryptor, IStreamEncryptor)
  private
    FKey: TSymetricKey;
    FCipherText: TStream;
    F2ndBuffer: TMemoryStream;
    Fis2ndBufferFull: boolean;
    FIVSeedLen: integer;
    FSaltLen: integer;
    FNonce: TMemoryStream;
    FisShortMessage: boolean;
    FisAddingSalt: boolean;

    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;

  protected
    procedure Reset;      override;

  public
    constructor Start_Encrypt(
      Key1: TSymetricKey; CipherText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    destructor Destroy; override;
  end;



TNoncibleDecryptor = class( TEncryptorDecryptor, IStreamDecryptor)
  private
    FKey: TSymetricKey;
    FPlainText: TStream;
    F2ndBuffer: TMemoryStream;
    Fis2ndBufferFull: boolean;
    FSaltAbsorber: TDesalinationWriteStream;
    FDataCount: int64;
    FIVSeedLen: integer;
    FSaltLen: integer;
    FisShortMessage: boolean;

    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;

  protected
    procedure Reset;     override;

  public
    constructor Start_Decrypt(
      Key1: TSymetricKey; PlainText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    destructor Destroy; override;
  end;



TNoNonceDecryptor = class( TEncryptorDecryptor, IStreamDecryptor)
  private
    FPlaintext: TStream;
    FisSecondBufferFull: boolean;
    F2ndBuffer: TMemoryStream;

    procedure Decrypt( const Ciphertext: TStream);
    procedure End_Decrypt;

  protected
    procedure Reset;        override;

  public
    constructor Start_Decrypt(
      Key1: TSymetricKey; PlainText1: TStream;
      const BlockCipher1: IBlockCipher;
      const Chaining1: IBlockChainingModel);

    destructor Destroy; override;
  end;





{ TCSharpZeroPadding }

function TCSharpZeroPadding.ControlObject: TObject;
var
  ControlObject: IControlObject;
begin
if Supports( FBlockCipher, IControlObject, ControlObject) then
    result := ControlObject.ControlObject
  else
    result := self
end;


constructor TCSharpZeroPadding.Create;
begin
end;

function TCSharpZeroPadding.DefinitionURL: string;
begin
result := ''
end;



function TCSharpZeroPadding.DisplayName: string;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.DisplayName
  else
    result := BlockCipher_DisplayName
end;



function TCSharpZeroPadding.Features: TAlgorithmicFeatureSet;
begin
result := [afStar, afOpenSourceSoftware, afBlockAdapter, afDoesNotNeedSalt]
end;


function TCSharpZeroPadding.GenerateKey( Seed: TStream): TSymetricKey;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.GenerateKey( Seed)
  else
    result := nil
end;


function TCSharpZeroPadding.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
if assigned( FBlockCipher) and assigned( Store) then
    result := FBlockCipher.LoadKeyFromStream( Store)
  else
    result := nil
end;


function TCSharpZeroPadding.Parameterize(
  const Params: IInterface): IStreamCipher;
var
  BlockCipherSelector: IBlockCipherSelector;
  BlockCipher: IBlockCipher;
  Chaining: IBlockChainingModel;
  Newbie: TCSharpZeroPadding;
begin
result := nil;
if assigned( FBlockCipher) and assigned( FChaining) then exit;
if Supports( Params, IBlockCipherSelector, BlockCipherSelector) then
  begin
  BlockCipher := BlockCipherSelector.GetBlockCipher;
  Chaining    := BlockCipherSelector.GetChainMode
  end;
if not (assigned( BlockCipher) or assigned( Chaining)) then exit;
Newbie := TCSharpZeroPadding.Create;
Newbie.FBlockCipher := BlockCipher;
Newbie.FChaining    := Chaining;
result := Newbie
end;




function TCSharpZeroPadding.ProgId: string;
begin
result := 'Sedgwick.StreamToBlock'
end;



function TCSharpZeroPadding.SeedByteSize: integer;
begin
if assigned( FBlockCipher) then
    result := FBlockCipher.SeedByteSize
  else
    result := -1
end;



function TCSharpZeroPadding.Start_Decrypt(
  Key: TSymetricKey; PlainText: TStream): IStreamDecryptor;
begin
if cfNoNounce in FChaining.ChainingFeatures then
    result := TNoNonceDecryptor.Start_Decrypt(
      Key, PlainText, FBlockCipher, FChaining)
  else
    result := TNoncibleDecryptor.Start_Decrypt(
      Key, PlainText, FBlockCipher, FChaining)
end;



function TCSharpZeroPadding.Start_Encrypt(
  Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
begin
if cfNoNounce in FChaining.ChainingFeatures then
    result := TNoNonceEncryptor.Start_Encrypt(
      Key, CipherText, FBlockCipher, FChaining)
  else
    result := TNoncibleEncryptor.Start_Encrypt(
      Key, CipherText, FBlockCipher, FChaining)
end;





function TCSharpZeroPadding.WikipediaReference: string;
begin
result := ''
end;





{ TNoNonceEncryptor }

constructor TNoNonceEncryptor.Start_Encrypt( Key1: TSymetricKey;
  CipherText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel);
begin
inherited Start_EncDec( Key1, CipherText1, BlockCipher1, Chaining1, True);
FCipherText  := CipherText1;
FIV.Size := FBlockLen;
ZeroFillStream( FIV);
FChainState := FChaining.Chain_EncryptBlock( Key1, FIV, FCodec);
if cfKeyStream in FChaining.ChainingFeatures then
    FPad := nil
  else
    FPad := TMemoryStream.Create
end;



procedure TNoNonceEncryptor.Encrypt( const Plaintext: TStream);
var
  X: int64;
  Amnt: integer;
begin
Plaintext.Position := 0;
X := Plaintext.Size;
while X > 0 do
  begin
  Amnt := FBlockLen - FLenBuffer;
  if X < FBlockLen then
    Amnt := Min( Amnt, X);
  if Amnt > 0 then
    begin
    ReadMem( Plaintext, FBuffer, FLenBuffer, Amnt);
    Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if FLenBuffer = FBlockLen then
    Emit_1stBuffer
  end
end;



procedure TNoNonceEncryptor.End_Encrypt;
begin
if (FDataCount <> 0) and (not (cfKeyStream in FChaining.ChainingFeatures)) then
  begin
  FPad.Size := FBlockLen - FLenBuffer;
  ZeroFillStream( FPad);
//  PByte( FPad.Memory)^ := PadIntroducer;
  Encrypt( FPad)
  end;
if cfKeyStream in FChaining.ChainingFeatures then
  Emit_1stBuffer;
Reset
end;



procedure TNoNonceEncryptor.Reset;
begin
inherited Reset;
FChainState.Reset( FIV);
if not (cfKeyStream in FChaining.ChainingFeatures) then
  BurnMemoryStream( FPad)
end;



destructor TNoNonceEncryptor.Destroy;
begin
BurnMemoryStream( FPad); FPad.Free;
inherited
end;



{ TNoncibleEncryptor }

constructor TNoncibleEncryptor.Start_Encrypt(
  Key1: TSymetricKey;
  CipherText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel);
begin
inherited Start_EncDec( Key1, Ciphertext1, BlockCipher1, Chaining1, True);
FKey := Key1;
FCipherText  := CipherText1;
FChainState  := nil;
F2ndBuffer       := TMemoryStream.Create;
F2ndBuffer.Size  := FBlockLen;
Fis2ndBufferFull := False;
FIV.Size := FBlockLen;
FIVSeedLen := Min( FBlockLen, 8);
FSaltLen   := 8 - FIVSeedLen;
FNonce := TMemoryStream.Create;
FIV.Size := FBlockLen;
FisShortMessage := True;
FisAddingSalt := False
end;



procedure TNoncibleEncryptor.Encrypt( const Plaintext: TStream);
var
  X: int64;
  Amnt: integer;
  Tmp: TMemoryStream;
begin
Plaintext.Position := 0;
X := Plaintext.Size;
if (X > 0) and (FDataCount = 0) and (not FisAddingSalt) then
  begin
  // 1. Acquire Nonce.
  FNonce.Size := 8;
  RandomFillStream( FNonce);

  // 2. Determine the IV.
  Move( FNonce.Memory^, FIV.Memory^, FIVSeedLen);
  if FBlockLen > FIVSeedLen then
    FillChar( MemStrmOffset( FIV, FIVSeedLen)^,
              FBlockLen - FIVSeedLen, 0);

  // 3. Emit IV seed in the clear.
  if FIVSeedLen > 0 then
    FCiphertext.WriteBuffer( FIV.Memory^, FIVSeedLen);

  // 4. Pre-pend salt.
  if FSaltLen > 0 then
    begin
    FisAddingSalt  := True;
    try
      Move( MemStrmOffset( FNonce, FIVSeedLen)^,
            FNonce.Memory^, FIVSeedLen);
      FNonce.Size := FSaltLen;
      Encrypt( FNonce);
      BurnMemoryStream( FNonce)
    finally
      FisAddingSalt := False
      end
    end
  end;
while (X > 0) or (FLenBuffer = FBlockLen) do
  begin
  Amnt := FBlockLen - FLenBuffer;
  if Amnt > X then
    Amnt := X;
  if Amnt > 0 then
    begin
    Amnt := ReadMem( Plaintext, FBuffer, FLenBuffer, Amnt);
    if Amnt = 0 then
        X := 0
      else
        Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if FLenBuffer >= FBlockLen then
    FisShortMessage := False;
  if (FLenBuffer = FBlockLen) and (not Fis2ndBufferFull) then
    begin
    Tmp := F2ndBuffer;
    F2ndBuffer := FBuffer;
    FBuffer := Tmp;
    Fis2ndBufferFull := True;
    FLenBuffer := 0
    end;
  if (FLenBuffer = FBlockLen) and Fis2ndBufferFull then
    begin
    if not assigned( FChainState) then
      FChainState := FChaining.Chain_EncryptBlock( FKey, FIV, FCodec);
    Emit( F2ndBuffer);
    Fis2ndBufferFull := False
    end
  end
end;


procedure TNoncibleEncryptor.End_Encrypt;
var
  Actual: IBlockChainingModel;
  ChainStateClone: TBlockChainLink;
begin
if FDataCount = 0 then
    begin end

  else
    begin
    if FisShortMessage and (not (cfKeyStream in FChaining.ChainingFeatures)) then
        begin
        Actual := ShortNonKeyStreamingChainMode;
        Fis8bitMode := (cf8bit in Actual.ChainingFeatures) and
                  (cfKeyStream in Actual.ChainingFeatures)
        end
      else
        Actual := FChaining;
    FisAutoXOR := ([cfKeyStream, cfAutoXOR] * Actual.ChainingFeatures) =
                   [cfKeyStream, cfAutoXOR];
    if not assigned( FChainState) then
      FChainState := Actual.Chain_EncryptBlock( FKey, FIV, FCodec);

    if FLenBuffer = 0 then
        begin // Round case
        Assert( Fis2ndBufferFull, AS_BlockToStream_EndEncrypt_InternalMarshalling);
        Emit( F2ndBuffer);
        end

      else if cfKeyStream in Actual.ChainingFeatures then
        begin // KeyStream case
        if Fis2ndBufferFull then
          Emit( F2ndBuffer);
        Emit_1stBuffer
        end

      else
        begin  // Ciphertext stealing
        Assert( Fis2ndBufferFull, AS_BlockToStream_EndEncrypt_InternalMarshalling);
        ChainStateClone := FChainState.Clone;
        try
          Chained_Encrypt_Block( F2ndBuffer, FEmitBuffer);
          Move( MemStrmOffset( FEmitBuffer, FLenBuffer)^,
                MemStrmOffset( FBuffer,     FLenBuffer)^,
                FBlockLen - FLenBuffer); // << The steal!
          FBuffer.Position := 0;
          F2ndBuffer.Position := 0;
          ChainStateClone.Encrypt_Block( FBuffer, F2ndBuffer);
          FCiphertext.WriteBuffer( F2ndBuffer .Memory^, FBlockLen);  // Swap emission order.
          FCiphertext.WriteBuffer( FEmitBuffer.Memory^, FLenBuffer)
        finally
          ChainStateClone.Burn;
          ChainStateClone.Free
          end
        end
    end;
Reset
end;





procedure TNoncibleEncryptor.Reset;
begin
inherited Reset;
BurnMemory( F2ndBuffer .Memory^, FBlockLen);
FreeAndNil( FChainState);
Fis2ndBufferFull := False;
FisShortMessage := True;
BurnMemory( FIV.Memory^, FBlockLen);
BurnMemoryStream( FNonce)
end;



destructor TNoncibleEncryptor.Destroy;
begin
BurnMemoryStream( F2ndBuffer);   FreeAndNil( F2ndBuffer);
BurnMemoryStream( FIV);
BurnMemoryStream( FNonce);       FreeAndNil( FNonce);
FDataCount := 0;
inherited
end;




{ TNoNonceDecryptor }

constructor TNoNonceDecryptor.Start_Decrypt(
  Key1: TSymetricKey; PlainText1: TStream;
  const BlockCipher1: IBlockCipher; const Chaining1: IBlockChainingModel);
begin
inherited Start_EncDec( Key1, Plaintext1, BlockCipher1, Chaining1, False);
FPlainText   := PlainText1;
FIV.Size := FBlockLen;
ZeroFillStream( FIV);
FChainState := FChaining.Chain_DecryptBlock( Key1, FIV, FCodec);
FisSecondBufferFull := False;
F2ndBuffer := TMemoryStream.Create;
F2ndBuffer.Size := FBlockLen
end;


procedure TNoNonceDecryptor.Decrypt( const Ciphertext: TStream);
var
  X: int64;
  Amnt: integer;
  Tmp: TMemoryStream;
begin
Ciphertext.Position := 0;
X := Ciphertext.Size;
while (X > 0) or ((FLenBuffer = FBlockLen) and (not FisSecondBufferFull)) do
  begin
  Amnt := FBlockLen - FLenBuffer;
  if X < FBlockLen then
    Amnt := Min( Amnt, X);
  if Amnt > 0 then
    begin
    ReadMem( Ciphertext, FBuffer, FLenBuffer, Amnt);
    Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if (FLenBuffer = FBlockLen) and (not FisSecondBufferFull) then
    begin
    Tmp := F2ndBuffer;
    F2ndBuffer := FBuffer;
    FBuffer := Tmp;
    FisSecondBufferFull := True;
    FLenBuffer := 0
    end;
  if FisSecondBufferFull and (FLenBuffer > 0) then
    begin
    Emit( F2ndBuffer);
    FisSecondBufferFull := False
    end
  end
end;

procedure TNoNonceDecryptor.End_Decrypt;
var
  Sentinal: PByte;
  j: integer;
begin
if FDataCount = 0 then
    begin end

  else if cfKeyStream in FChaining.ChainingFeatures then
    begin
    if FisSecondBufferFull then
      begin
      Emit( F2ndBuffer);
      FisSecondBufferFull := False
      end;
    Emit_1stBuffer
    end

  else
    begin
    Assert( FisSecondBufferFull and (FLenBuffer = 0),
      AS_BlockPaddingCorrupt);
    Chained_Decrypt_Block( FEmitBuffer, F2ndBuffer);
    FLenBuffer := 0;
    Sentinal   := PByte( MemStrmOffset( FEmitBuffer, FBlockLen - 1));
    for j := FBlockLen - 1 downto 0 do
      begin
//      if Sentinal^ = PadIntroducer then
      if Sentinal^ <> 0 then
        begin
//        FLenBuffer := j;
        FLenBuffer := j + 1;
        break
        end;
      Dec( Sentinal)
      end;
    if FLenBuffer > 0 then
      FPlaintext.WriteBuffer( FEmitBuffer.Memory^, FLenBuffer)
    end;

Reset
end;


procedure TNoNonceDecryptor.Reset;
begin
inherited Reset;
BurnMemory( F2ndBuffer .Memory^, FBlockLen);
FChainState.Reset( FIV);
FisSecondBufferFull := False
end;



destructor TNoNonceDecryptor.Destroy;
begin
BurnMemoryStream( F2ndBuffer); FreeAndNil( F2ndBuffer);
inherited
end;


{ TNoncibleDecryptor }

function TEncryptorDecryptor.ShortNonKeyStreamingChainMode: IBlockChainingModel;
begin
result := TCFB_8Bit.Create
// TCFB_Block is also a reasonable replacement.
end;



constructor TNoncibleDecryptor.Start_Decrypt(
  Key1: TSymetricKey;
  PlainText1: TStream; const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel);
begin
inherited Start_EncDec( Key1, Plaintext1, BlockCipher1, Chaining1, False);
FKey := Key1;

FPlainText   := PlainText1;
FLenBuffer   := 0;

F2ndBuffer       := TMemoryStream.Create;
F2ndBuffer.Size  := FBlockLen;
Fis2ndBufferFull := False;

FIV.Size := 0;

FIVSeedLen := Min( FBlockLen, 8);
FSaltLen   := 8 - FIVSeedLen;
if FSaltLen > 0 then
    begin
    FSaltAbsorber := TDesalinationWriteStream.Create;
    FSaltAbsorber.FreshwaterStream := FEmissionStream;
    FEmissionStream := FSaltAbsorber;
    FSaltAbsorber.SaltVolume := FSaltLen
    end
  else
    FSaltAbsorber := nil;

FisShortMessage := True
end;


procedure TNoncibleDecryptor.Decrypt( const Ciphertext: TStream);
var
  X: int64;
  Amnt: integer;
  Tmp: TMemoryStream;
  OldSize: integer;
begin
Ciphertext.Position := 0;
X := Ciphertext.Size;
while (X > 0) or (FLenBuffer = FBlockLen) do
  begin

  // 1. Read the IV.
  if FIVSeedLen > 0 then
    begin
    Amnt := FIVSeedLen;
    if X < Amnt then
      Amnt := X;
    FIV.CopyFrom( Ciphertext, Amnt);
    Dec( X, Amnt);
    Dec( FIVSeedLen, Amnt);
    OldSize := FIV.Size;
    if (FIVSeedLen = 0) and (OldSize < FBlockLen) then
      begin
      FIV.Size := FBlockLen;
      FillChar( MemStrmOffset( FIV, OldSize)^, FBlockLen - OldSize, 0)
      end;
    continue
    end;

  // 2. Now process the payload
  Amnt := FBlockLen - FLenBuffer;
  if X < Amnt then
    Amnt := X;
  if Amnt > 0 then
    begin
    ReadMem( Ciphertext, FBuffer, FLenBuffer, Amnt);
    Dec( X, Amnt);
    Inc( FLenBuffer, Amnt);
    Inc( FDataCount, Amnt)
    end;
  if FLenBuffer >= FBlockLen then
    FisShortMessage := False;
  if (FLenBuffer = FBlockLen) and (not Fis2ndBufferFull) then
    begin
    Tmp := F2ndBuffer;
    F2ndBuffer := FBuffer;
    FBuffer := Tmp;
    Fis2ndBufferFull := True;
    FLenBuffer := 0
    end;
  if (FLenBuffer = FBlockLen) and Fis2ndBufferFull then
    begin
    if not assigned( FChainState) then
      FChainState := FChaining.Chain_DecryptBlock( FKey, FIV, FCodec);
    Emit( F2ndBuffer);
    Fis2ndBufferFull := False
    end
  end
end;



procedure TNoncibleDecryptor.End_Decrypt;
var
  Actual: IBlockChainingModel;
  ChainStateClone: TBlockChainLink;
begin
if FDataCount = 0 then
    begin end

  else
    begin
    if FisShortMessage and (not (cfKeyStream in FChaining.ChainingFeatures)) then
        begin
        Actual := ShortNonKeyStreamingChainMode;
        Fis8bitMode := (cf8bit in Actual.ChainingFeatures) and
                  (cfKeyStream in Actual.ChainingFeatures)
        end
      else
        Actual := FChaining;
    FisAutoXOR := ([cfKeyStream, cfAutoXOR] * Actual.ChainingFeatures) =
                   [cfKeyStream, cfAutoXOR];
    if not assigned( FChainState) then
      FChainState := Actual.Chain_DecryptBlock( FKey, FIV, FCodec);

    if FLenBuffer = 0 then
        begin // Round case
        Assert( Fis2ndBufferFull, AS_BlockToStream_EndDecrypt_InternalMarshalling);
        Emit( F2ndBuffer)
        end

      else if cfKeyStream in Actual.ChainingFeatures then
        begin // KeyStream case
        if Fis2ndBufferFull then
          Emit( F2ndBuffer);
        Emit_1stBuffer
        end

      else
        begin  // Ciphertext stealing
        Assert( Fis2ndBufferFull, AS_BlockToStream_EndDecrypt_InternalMarshalling);
        ChainStateClone := FChainState.Clone;
        try
          Chained_Decrypt_Block( FEmitBuffer, F2ndBuffer);
          Move( MemStrmOffset( FEmitBuffer, FLenBuffer)^,
                MemStrmOffset( FBuffer,     FLenBuffer)^,
                FBlockLen - FLenBuffer); // << Reverse the steal!
          F2ndBuffer.Position := 0;
          FBuffer.Position := 0;
          ChainStateClone.Decrypt_Block( F2ndBuffer, FBuffer);
          FPlaintext.WriteBuffer( F2ndBuffer .Memory^, FBlockLen);
          FPlaintext.WriteBuffer( FEmitBuffer.Memory^, FLenBuffer)
        finally
          ChainStateClone.Burn;
          ChainStateClone.Free
          end
        end
    end;
Reset
end;



procedure TNoncibleDecryptor.Reset;
begin
inherited Reset;
BurnMemory( F2ndBuffer .Memory^, FBlockLen);
Fis2ndBufferFull := False;
FisShortMessage := True;
BurnMemoryStream( FIV);
if assigned( FChainState) then
  begin
  FChainState.Burn;
  FChainState.Free;
  FChainState := nil
  end;
FIVSeedLen := Min( FBlockLen, 8);
FSaltLen   := 8 - FIVSeedLen;
if FSaltLen > 0 then
  FSaltAbsorber.SaltVolume := FSaltLen
end;



destructor TNoncibleDecryptor.Destroy;
begin
BurnMemoryStream( F2ndBuffer);       FreeAndNil( F2ndBuffer);
FreeAndNil( FSaltAbsorber);
FDataCount := 0;
inherited


end;


{ TEncryptorDecryptor }

constructor TEncryptorDecryptor.Start_EncDec(
  Key1: TSymetricKey; EmissionStream1: TStream;
  const BlockCipher1: IBlockCipher;
  const Chaining1: IBlockChainingModel; isEncrypting1: boolean);
begin
FEmissionStream := EmissionStream1;
FBlockCipher := BlockCipher1;
FChaining    := Chaining1;
FEmitBuffer  := TMemoryStream.Create;
FBuffer   := TMemoryStream.Create;
FBlockLen := FBlockCipher.BlockSize div 8;
FCodec    := FBlockCipher.MakeBlockCodec( Key1);
FBuffer.Size := FBlockLen;
FLenBuffer := 0;
FEmitBuffer.Size := FBlockLen;
FIV := TMemoryStream.Create;
FChainState := nil;
FDataCount := 0;
FisAutoXOR := ([cfKeyStream, cfAutoXOR] * FChaining.ChainingFeatures) =
               [cfKeyStream, cfAutoXOR];
Fis8bitMode := (cf8bit in FChaining.ChainingFeatures) and
               (cfKeyStream in FChaining.ChainingFeatures) and
               (not (cfNoNounce in FChaining.ChainingFeatures));
// A chaining mode with cf8Bit MUST also have
//   cfKeyStream and not cfNoNounce
FisEncrypting := isEncrypting1
end;



procedure TEncryptorDecryptor.Chained_Decrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
begin
Plaintext.Position  := 0;
Ciphertext.Position := 0;
FChainState.Decrypt_Block( Plaintext, Ciphertext)
end;


procedure TEncryptorDecryptor.Chained_Encrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
begin
Plaintext.Position  := 0;
Ciphertext.Position := 0;
FChainState.Encrypt_Block( Plaintext, Ciphertext)
end;



destructor TEncryptorDecryptor.Destroy;
begin
BurnMemoryStream( FEmitBuffer); FEmitBuffer.Free;
BurnMemoryStream( FBuffer);     FBuffer.Free;
FCodec.Burn;
FCodec := nil;
if assigned( FChainState) then
  begin
  FChainState.Burn;
  FChainState.Free
  end;
BurnMemoryStream( FIV);
FIV.Free;
inherited
end;



procedure TEncryptorDecryptor.Emit(
  SourceBuf: TMemoryStream; EmitLen: integer = -1);
var
  PlaintextByte, CiphertextByte: byte;
  P, Q: PByte;
  j: integer;
begin
if EmitLen = -1 then
  EmitLen := FBlockLen;
if FisEncrypting then
    begin
    if Fis8bitMode then
        begin
        P := SourceBuf.Memory;
        Q := FEmitBuffer.Memory;
        for j := 0 to EmitLen - 1 do
          begin
          PlaintextByte := P^;
          FChainState.Encrypt_8bit( PlaintextByte, CiphertextByte);
          Q^ := CiphertextByte;
          Inc( P); Inc( Q)
          end
        end
      else
        Chained_Encrypt_Block( SourceBuf, FEmitBuffer)
    end
  else
    begin
    if Fis8bitMode then
        begin
        P := SourceBuf.Memory;
        Q := FEmitBuffer.Memory;
        for j := 0 to EmitLen - 1 do
          begin
          CiphertextByte := P^;
          FChainState.Decrypt_8bit( PlaintextByte, CiphertextByte);
          Q^ := PlaintextByte;
          Inc( P); Inc( Q)
          end
        end
      else
        Chained_Decrypt_Block( FEmitBuffer, SourceBuf)
    end;
if FisAutoXOR then
  XOR_Streams2( FEmitBuffer, SourceBuf);
FEmissionStream.WriteBuffer( FEmitBuffer.Memory^, EmitLen)
end;




procedure TEncryptorDecryptor.Emit_1stBuffer;
begin
if FLenBuffer = 0 then exit;
Emit( FBuffer, FLenBuffer);
FLenBuffer := 0
end;


procedure TEncryptorDecryptor.Reset;
begin
BurnMemory( FEmitBuffer.Memory^, FBlockLen);
BurnMemory( FBuffer    .Memory^, FBlockLen);
FCodec.Reset;
FLenBuffer := 0;
FDataCount := 0
end;




end.
