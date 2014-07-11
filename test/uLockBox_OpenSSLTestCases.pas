unit uLockBox_OpenSSLTestCases;
interface
uses TestFramework, Classes, uTPLb_Signatory, uTPLb_OpenSSL, SysUtils,
     uTPLb_Codec, uTPLb_CryptographicLibrary, uTPLb_Random;

type
TOpenSSL_TestCase = class( TTestCase)
  protected
    FcodecOpenSSL: TOpenSSL_Codec;
    FSig: TOpenSSL_Signatory;
    FKey, FIV: TBytes;
    FBlockSize: integer;
    FPlain, FCipher, FRecon: TMemoryStream;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure OpenSSL_AES_Encryption;
    procedure TwoGenerations;
  end;

implementation





uses uTPLb_StreamUtils, uTPLb_Constants, uTPLb_Asymetric;



procedure InitUnit_OpenSSLTestCases_TestCases;
begin
TestFramework.RegisterTest( TOpenSSL_TestCase.Suite)
end;

procedure DoneUnit_OpenSSLTestCases_TestCases;
begin
end;

{ TOpenSSL_TestCase }
type
TBlock=array[0..15] of byte;
PBlock=^TBlock;
procedure TOpenSSL_TestCase.OpenSSL_AES_Encryption;
var
  I: Integer;
  b: byte;
begin
for I := 0 to 7 do
  begin
  b := i;
  FPlain.Write( b, 1);
  end;
FPlain.Position := 0;
FcodecOpenSSL.Encrypt( FPlain, FCipher);
FCipher.Position := 0;
FcodecOpenSSL.Decrypt( FRecon, FCipher);
Check( CompareMemoryStreams( FPlain, FRecon), 'Failed general inversion')
end;

procedure TOpenSSL_TestCase.SetUp;
var
  i: Integer;
begin
FcodecOpenSSL := TOpenSSL_Codec.Create( nil);
FcodecOpenSSL.RequiredVersion := '0.9.0.0';
FcodecOpenSSL.isLoaded := True;
Check( FcodecOpenSSL.isLoaded, 'Failed to load Libeay32');
FcodecOpenSSL.Cipher := cipher_aes_256_cbc;
FcodecOpenSSL.PaddingScheme := padPKCS;
FBlockSize := 16;
SetLength( FKey, FBlockSize);
SetLength( FIV, FBlockSize);
for i := 0 to FBlockSize - 1 do
  begin
  FKey[i] := 0;
  FIV [i] := 0;
  end;
FPlain  := TMemoryStream.Create;
FCipher := TMemoryStream.Create;
FRecon  := TMemoryStream.Create;
FcodecOpenSSL.SetKey( FKey);
FcodecOpenSSL.SetIV ( FIV);
FSig := TOpenSSL_Signatory.Create( nil);
FSig.RequiredVersion := '0.9.0.0';
FSig.isLoaded := True;
Check( FSig.isLoaded, 'Failed to load Libeay32');
end;

procedure TOpenSSL_TestCase.TearDown;
begin
FcodecOpenSSL.Free;
FPlain.Free;
FCipher.Free;
FRecon.Free;
FSig.Free
end;




procedure TOpenSSL_TestCase.TwoGenerations;
begin
Check( FSig.GenerateKeys);
Check( FSig.GenerateKeys);
end;




initialization
InitUnit_OpenSSLTestCases_TestCases;


finalization
DoneUnit_OpenSSLTestCases_TestCases;

end.
