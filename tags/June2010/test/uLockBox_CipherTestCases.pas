unit uLockBox_CipherTestCases;
interface

uses TestFramework, uTPLb_Hash, uTPLb_CryptographicLibrary, Classes,
     uTPLb_Codec, uTPLb_StreamCipher, uLockBox_TestCases;

type


THash_TestCase = class( TTestCase)
  protected
    FHash: THash;
    FLib: TCryptographicLibrary;
    FReferenceTestSource: ansistring;
    FReferenceTestRefrnc: ansistring;
    FSource: TMemoryStream;
    FRefValue: TMemoryStream;
    FTestValue: TMemoryStream;

    procedure SetUp; override;
    procedure TearDown; override;
    class function HashId: string; virtual; abstract;

  published
    procedure Test_Reference;
  end;



TMD5_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  end;

TSHA1_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  end;


TBlockSizeCase = (
  scZero,          // size = 0
  scSmall,         // size ~= 0.3 blocks
  scUnit,          // size = 1 block
  scSmallish,      // size = 1.5 blocks
  scRough,         // size = large N + about 0.1 blocks
  scRound);        // size = exactly N blocks. N is a large number.


TBlockMode_TestCase = class( TTestCase)
  protected
    FLib: TCryptographicLibrary;
    FCodec: TCodec;
    FOriginal        : TMemoryStream;
    FCiphertext      : TMemoryStream;
    FKeyStream       : TMemoryStream;
    FReferenceStream : TMemoryStream;
    FBlockSize: integer;
    FFeatures: TAlgorithmicFeatureSet;

    procedure SetUp; override;
    procedure TearDown; override;
    class function CipherId: string; virtual; abstract;

    procedure InversionTest( DataSize: TBlockSizeCase; const ChainMode: string);

  published
    procedure Test_Reference;
    procedure GeneralInversionTests;
  end;


TAES_Reference_TestCase = class( TBlockMode_TestCase)
  protected
    class function KeySize: integer; virtual; abstract;
    class function CipherId: string; override;
  end;


TAES128_RefTestCase = class( TAES_Reference_TestCase)
  protected
    class function KeySize: integer; override;
  end;

TAES192_RefTestCase = class( TAES_Reference_TestCase)
  protected
    class function KeySize: integer; override;
  end;

TAES256_RefTestCase = class( TAES_Reference_TestCase)
  protected
    class function KeySize: integer; override;
  end;



TStreamModeInversion_TestCase = class( TTestCase)
  protected
    FLib: TCryptographicLibrary;
    FCodec: TCodec;
    FOriginal        : TMemoryStream;
    FCiphertext      : TMemoryStream;
    FReconstructed   : TMemoryStream;
    FFeatures: TAlgorithmicFeatureSet;

    procedure SetUp; override;
    procedure TearDown; override;
    class function CipherId: string; virtual; abstract;
    class procedure TestSizeRange( var MinBytes, MaxBytes: integer); virtual; abstract;

  published
    procedure GeneralInversionTest; virtual;
  end;


TBase64Inversion_TestCase = class( TStreamModeInversion_TestCase)
  protected
    class function CipherId: string; override;
    class procedure TestSizeRange( var MinBytes, MaxBytes: integer); override;
  end;




implementation










uses SysUtils, uTPLb_HashDsc, uTPLb_BinaryUtils, uTPLb_StreamUtils,
     uTPLb_ECB, uTPLb_BlockCipher, uTPLb_Random, uTPLb_HugeCardinalUtils,
     uTPLb_IntegerUtils;


{ TTestCaseFirst }


procedure InitUnit_CipherTestCases;
begin
TestFramework.RegisterTest( TMD5_TestCase.Suite);
TestFramework.RegisterTest( TSHA1_TestCase.Suite);
TestFramework.RegisterTest( TAES128_RefTestCase.Suite);
TestFramework.RegisterTest( TAES192_RefTestCase.Suite);
TestFramework.RegisterTest( TAES256_RefTestCase.Suite);
TestFramework.RegisterTest( TBase64Inversion_TestCase.Suite);
end;

procedure DoneUnit_CipherTestCases;
begin
end;




{ THash_TestCase }

procedure THash_TestCase.SetUp;
var
  TestAccess: IHash_TestAccess;
  Hasher: IHasher;
begin
FLib  := TCryptographicLibrary.Create( nil);
FHash := THash.Create( nil);
FHash.CryptoLibrary := FLib;
FHash.HashId := HashId;
FHash.Begin_Hash;
if Supports( FHash, IHash_TestAccess, TestAccess) then
  Hasher := TestAccess.GetHasher;
FHash.End_Hash;
if assigned( Hasher) then
  begin
  FReferenceTestSource := Hasher.SelfTest_Source;
  FReferenceTestRefrnc := Hasher.SelfTest_ReferenceHashValue
  end;
FSource    := TMemoryStream.Create;
FRefValue  := TMemoryStream.Create;
FTestValue := TMemoryStream.Create
end;




procedure THash_TestCase.TearDown;
begin
FHash.Free;
FLib.Free;
FSource.Free;
FRefValue.Free;
FTestValue.Free
end;




procedure THash_TestCase.Test_Reference;
begin
FSource.Write( FReferenceTestSource[1], Length( FReferenceTestSource) * SizeOf( AnsiChar));
{$WARNINGS OFF}
Read_BigEndien_u32_Hex( FReferenceTestRefrnc, FRefValue);
{$WARNINGS ON}
FHash.HashStream( FSource);
FTestValue.CopyFrom( FHash.HashOutputValue, 0);
FHash.Burn;
Check( CompareMemoryStreams( FRefValue, FTestValue), Format(
  'Hash %s failed it''s standard reference test.',[FHash.Hash]))
end;



{ TMD5_TestCase }

class function TMD5_TestCase.HashId: string;
begin
result := 'native.hash.MD5'
end;




{ TSHA1_TestCase }

class function TSHA1_TestCase.HashId: string;
begin
result := 'native.hash.SHA-1'
end;


const sPure_ECB_Id = 'dunit.ECB';

type TPure_ECB = class( TECB)
  protected
    function  ChainingFeatures: TChainingFeatureSet;  override;
    function  ProgId: string;                         override;
  end;

function TPure_ECB.ChainingFeatures: TChainingFeatureSet;
begin
result := inherited ChainingFeatures + [cfKeyStream]
// The inclusion of cfKeyStream disables block padding.
// We can only use this class in situations where be plaintext size
//  is an exact multiple of the block size.
end;

function TPure_ECB.ProgId: string;
begin
result := sPure_ECB_Id
end;



{ TStreamModeReference_TestCase }

procedure TBlockMode_TestCase.GeneralInversionTests;
  procedure TestChainMode( const ChainMode: string);
  var
    j: TBlockSizeCase;
  begin
  for j := Low( TBlockSizeCase) to High( TBlockSizeCase) do
    InversionTest( j, ChainMode)
  end;

begin
TestChainMode( 'native.ECB');
TestChainMode( 'native.CBC');
TestChainMode( 'native.CFB');
TestChainMode( 'native.CTR');
TestChainMode( 'native.OFB');
TestChainMode( 'native.CFB-8bit');
TestChainMode( 'native.PCBC')
end;



procedure TBlockMode_TestCase.InversionTest(
  DataSize: TBlockSizeCase; const ChainMode: string);
var
  TestSize, TestsCount: integer;
  j: integer;
  Ok: boolean;

  function RandomSizableNumber: integer;
  begin
  TRandomStream.Instance.Read( result, SizeOf( result));
  result := (abs( result) mod 200) + 50
  end;

begin
FCodec.ChainModeId := ChainMode;
FCodec.Password := 'Monkey''s uncle';
TestSize   := 0;
TestsCount := 0;
case DataSize of
  scZero:   begin
            TestSize   := 0;
            TestsCount := 1
            end;

  scSmall: begin
           TestSize   := Round( FBlockSize * 0.3);
           TestsCount := 1
           end;

  scUnit: begin
          TestSize   := FBlockSize;
          TestsCount := 1
          end;

  scSmallish: begin
              TestSize   := Round( FBlockSize * 1.5);
              TestsCount := 10
              end;

  scRough: begin
           TestSize   := (RandomSizableNumber * FBlockSize) + RandomSizableNumber;
           TestsCount := 100
           end;

  scRound: begin
           TestSize   := RandomSizableNumber * FBlockSize;
           TestsCount := 10
           end;
  end;
for j := 1 to TestsCount do
  begin
  FOriginal.Size := TestSize;
  RandomFillStream( FOriginal);
  FCiphertext.Size := 0;
  FCodec.EncryptStream( FOriginal, FCiphertext);
  FCodec.Reset;
  FReferenceStream.Size := 0;
  FCodec.DecryptStream( FReferenceStream, FCiphertext);
  FCodec.Reset;
  Ok := CompareMemoryStreams( FOriginal, FReferenceStream);
  Check( Ok,
    Format( '%s fails the general inversion test!', [FCodec.Cipher]));
  if TestSize = 0 then
    Check( FCiphertext.Size = 0,
           Format( '%s fails the zero test!', [FCodec.Cipher]));
  if not Ok then break
  end
end;



procedure TBlockMode_TestCase.SetUp;
var
  Codec_TestAccess: ICodec_TestAccess;
begin
FLib   := TCryptographicLibrary.Create( nil);
FLib.RegisterBlockChainingModel( TPure_ECB.Create as IBlockChainingModel);


FCodec := TCodec.Create( nil);
FCodec.CryptoLibrary  := FLib;
FCodec.StreamCipherId := 'native.StreamToBlock';
FCodec.BlockCipherId  := CipherId;
FCodec.ChainModeId    := sPure_ECB_Id;

FOriginal        := TMemoryStream.Create;
FCiphertext      := TMemoryStream.Create;
FKeyStream       := TMemoryStream.Create;
FReferenceStream := TMemoryStream.Create;

if Supports( FCodec, ICodec_TestAccess, Codec_TestAccess) and
   (Codec_TestAccess.GetCodecIntf.BlockCipher <> nil) then
  with Codec_TestAccess.GetCodecIntf.BlockCipher do
    begin
    FFeatures  := Features;
    FBlockSize := BlockSize div 8;
    {$WARNINGS OFF}
    Read_BigEndien_u32_Hex( SelfTest_Key       , FKeyStream);
    Read_BigEndien_u32_Hex( SelfTest_Plaintext , FOriginal);
    // Original MUST be an exact multiple (probably 1) of the block size.
    Read_BigEndien_u32_Hex( SelfTest_Ciphertext, FReferenceStream)
    {$WARNINGS ON}
    end
  else
    begin
    FFeatures  := [afNotImplementedYet];
    FBlockSize := 1
    end
end;



procedure TBlockMode_TestCase.TearDown;
begin
FCodec.Free;
FLib.Free;
FOriginal.Free;
FCiphertext.Free;
FKeyStream.Free;
FReferenceStream.Free
end;



procedure TBlockMode_TestCase.Test_Reference;
begin
Check( not (afNotImplementedYet in FFeatures),
  Format( '%s is not yet implemented!', [FCodec.Cipher]));
FKeyStream.Position := 0;
FCodec.InitFromStream( FKeyStream);
FCodec.EncryptStream( FOriginal, FCiphertext);
Check( CompareMemoryStreams( FCiphertext, FReferenceStream),
  Format( '%s fails the reference check!', [FCodec.Cipher]))
end;



{ TAES_Reference_TestCase }

class function TAES_Reference_TestCase.CipherId: string;
begin
result := Format( 'native.AES-%d', [KeySize])
end;

{ TAES128_RefTestCase }

class function TAES128_RefTestCase.KeySize: integer;
begin
result := 128
end;

{ TAES192_RefTestCase }

class function TAES192_RefTestCase.KeySize: integer;
begin
result := 192
end;

{ TAES256_RefTestCase }

class function TAES256_RefTestCase.KeySize: integer;
begin
result := 256
end;



{ TStreamModeInversion_TestCase }

procedure TStreamModeInversion_TestCase.SetUp;
var
  Codec_TestAccess: ICodec_TestAccess;
begin
FLib   := TCryptographicLibrary.Create( nil);
FCodec := TCodec.Create( nil);
FCodec.CryptoLibrary  := FLib;
FCodec.StreamCipherId := CipherId;

FOriginal      := TMemoryStream.Create;
FCiphertext    := TMemoryStream.Create;
FReconstructed := TMemoryStream.Create;

if Supports( FCodec, ICodec_TestAccess, Codec_TestAccess) and
   (Codec_TestAccess.GetCodecIntf.StreamCipher <> nil) then
  with Codec_TestAccess.GetCodecIntf.StreamCipher do
    FFeatures := Features
  else
    FFeatures := [afNotImplementedYet];
FCodec.Password := 'Fancy pants';
end;



procedure TStreamModeInversion_TestCase.TearDown;
begin
FLib.Free;
FCodec.Free;
FOriginal.Free;
FCiphertext.Free;
FReconstructed.Free
end;


procedure TStreamModeInversion_TestCase.GeneralInversionTest;
var
  MinBytes, MaxBytes: integer;
  TestSize: integer;
  j: integer;
  Ok: boolean;
begin
TestSizeRange( MinBytes, MaxBytes);
for j := 1 to 100 do
  begin
  TRandomStream.Instance.Read( TestSize, SizeOf( TestSize));
  TestSize := (abs( TestSize) mod (MaxBytes - MinBytes + 1)) + MinBytes;
  FOriginal.Size := TestSize;
  RandomFillStream( FOriginal);
  FCiphertext.Size := 0;
  FCodec.EncryptStream( FOriginal, FCiphertext);
  FCodec.Reset;
  FReconstructed.Size := 0;
  FCodec.DecryptStream( FReconstructed, FCiphertext);
  FCodec.Reset;
  Ok := CompareMemoryStreams( FOriginal, FReconstructed);
  Check( Ok,
    Format( '%s fails the general inversion test!', [FCodec.Cipher]));
  Check( (FCiphertext.Size >= FOriginal.Size) or
         (([afCompressor,afConverter]*FFeatures) <> []),
    'Suspicious size of ciphertext.');
  if not Ok then break
  end
end;


{ TBase64Inversion_TestCase }

class function TBase64Inversion_TestCase.CipherId: string;
begin
result := 'native.base64'
end;

class procedure TBase64Inversion_TestCase.TestSizeRange(
  var MinBytes, MaxBytes: integer);
begin
MinBytes := 0;
MaxBytes := 200
end;



initialization
InitUnit_CipherTestCases;


finalization
DoneUnit_CipherTestCases;


end.
