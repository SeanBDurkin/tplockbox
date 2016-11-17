unit uDemoBlockModeCipher;
interface
uses Classes, TPLB3.StreamCipher, TPLB3.BlockCipher, SysUtils,
     TPLB3.SimpleBlockCipher, TPLB3.CryptographicLibrary;

const

  DemoProgId = 'SBD.Demo1';
  DemoBlockSize = 10;

type
TDemoBlockModeCipher = class( TSimpleBlockCipher)
  protected
    function  Encrypt(
      const Buffer: TBytes;
      Key: TSimpleBlockCipherKey;
      doEncrypt: boolean): TBytes; override;

  public
    class procedure SelfRegister( Lib: TCryptographicLibrary);
  end;




implementation








{ TDemoBlockModeCipher }

function TDemoBlockModeCipher.Encrypt(
  const Buffer: TBytes;
  Key: TSimpleBlockCipherKey; doEncrypt: boolean): TBytes;
var
  j: integer;
begin
SetLength( result, DemoBlockSize);
for j := 0 to DemoBlockSize - 1 do
  result[ j] := byte( Buffer[ j]) xor byte( Key.FKeyData[ j])
end;







class procedure TDemoBlockModeCipher.SelfRegister( Lib: TCryptographicLibrary);
begin
Lib.RegisterSimpleBlockTransform(
  self, DemoProgId, 'Demo block transform',
  [afCryptographicallyWeak, afForTestOnly,
   afForRunTimeOnly, afOpenSourceSoftware], DemoBlockSize)
end;

end.
