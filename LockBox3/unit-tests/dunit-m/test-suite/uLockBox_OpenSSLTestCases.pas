unit uLockBox_OpenSSLTestCases;
interface
uses DUnitM.UnitTestFramework, Classes, TPLB3.Signatory, TPLB3.OpenSSL, SysUtils,
     TPLB3.Codec, TPLB3.CryptographicLibrary, TPLB3.Random, uLockBox_TestCases;

type
{$M+}
[TestFixture('OpenSSL')]
TOpenSSL_TestCase = class( TTestCase)
  public
    FcodecOpenSSL: TOpenSSL_Codec;
    FSig: TOpenSSL_Signatory;
    FKey, FIV: TBytes;
    FBlockSize: integer;
    FPlain, FCipher, FRecon: TMemoryStream;
    [Setup]    procedure SetUp;
    [TearDown] procedure TearDown;

  published
    [Test] procedure OpenSSL_AES_Encryption;

    [IgnoreMemoryLeaks]
    [Test] procedure TwoGenerations;

    [IgnoreMemoryLeaks]
    [Test] procedure Signature;
  end;

implementation





uses TPLB3.StreamUtils, TPLB3.Constants, TPLB3.Asymetric;



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
Assert.IsTrue( CompareMemoryStreams( FPlain, FRecon), 'Failed general inversion')
end;

procedure TOpenSSL_TestCase.SetUp;
var
  i: Integer;
begin
FcodecOpenSSL := TOpenSSL_Codec.Create( nil);
FcodecOpenSSL.RequiredVersion := '0.9.0.0';
FcodecOpenSSL.isLoaded := True;
Assert.IsTrue( FcodecOpenSSL.isLoaded, 'Failed to load Libeay32');
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
Assert.IsTrue( FSig.isLoaded, 'Failed to load Libeay32');
end;

const
  PrivateKey1: utf8string =
    '-----BEGIN RSA PRIVATE KEY-----'#$A +
    'MIIEowIBAAKCAQEAvrcFyDAAU6IyOiXiPaTuAe9CTZtwwLdkDSzb6IzBYwFUH+88'#$A +
    'QAklxLlHSsK3P/UKzDScG1bDHa3Ry8LZcU/LmIuFyoPAx5kJEeP5Auk/NGSsFbIw'#$A +
    '+bYuMXWU2EVMXhJqQpezJKNc+x03dgZLTBgNuH0E1SO1Reu/4FKYA29zSN5qdYeG'#$A +
    '8nrk45e46lm835HYzrDl69IwgB1daZ3/mrvik6qY2JjfiGxHDMqCUtWWuvA6Ayoi'#$A +
    'G4EFpU8ej3MElt7kKjJJrx5A4wx+hbr0BzVPWsIoYtffEhTBnEWR3pGe2R1pKy9l'#$A +
    'zgklFbp7984gqBOXVigHrRXq0pSdVOhW9ZjSqwIDAQABAoIBADPiDKOiU2RtOqbR'#$A +
    'CZRlmw5RrcL5J5p2CbT/4C+Ko02w3db2OXjeRDUZhoiTIlE286IMKe/SEbCwSePZ'#$A +
    '9Ve7MpMkWdh3MWnbezkvwN2G4Nf7D0mzuVkls7lm2IBhkd4BuoC1TloIS1JRZ3Yn'#$A +
    'TjK8VHc9I6RsW3NTKFb5gTtmBYHr7s/wsptT2QNFmK194AbIGAcb4WWtS34YktDT'#$A +
    'gXCdMEqZhASnmShgQhiyL8s5BgacpCBkuVEfBRqrL2MsV2ceSbyuTZUabNFMReOX'#$A +
    'BqrGWGbNebFUce1stFhShi9np6cUj3EL1bErC/RZJuXJONDxNEuVutriglInZ9D+'#$A +
    'g4vY9YkCgYEA/MtEvlO7ptfaw+9Bb/S+aEFYQTo/C8T0ELV3wW8Pj399OVhmxV8s'#$A +
    'ZEUdSYyB6cCJukLPJyGAokSktLBF+sOgxJkGY7dlei9jbUfKa1UGMwKYbxKQX2hA'#$A +
    'UGEbRR1Pt/+pNrNgAfMbiwnz+ir5i2gBRXyZvzbfuzs0RbElqpDH7/0CgYEAwSI0'#$A +
    'nkMEgOXdm/TBPJUEXLv8AjBlBWjI8KxrefGgZNRBYU64rIiKs68w7Suv4hw77fBj'#$A +
    'HIyc1io6W6bOzwx3W9+vEzEG5xHzxcoRESJ0N1195sLuU/F4aCRHioMsSouR02tm'#$A +
    'ea4ltqg2DkHSos1ulbnhWxN5/AFu36roAy/A6ccCgYB7WgiKQrt/VzbFgvrQUYGT'#$A +
    'x8bz1SMscAeUG6h69+GE6PXGxK8pQh6cMulumSRPVoceHzmL45osFAi2rokHKuxI'#$A +
    '4k6u26+lpngCvBQ2uX9T5sFQ+aL/GxS+5BN1by8WHqeILJD9go3/E3U8rjmkX7S3'#$A +
    'Hmy7VGBpsSL5ms5BY3JcVQKBgQCdPWkTlMKc4wkLCTkuRrC3g4FIkvgccFRwxh/2'#$A +
    't0d77+eO/tWR+tTaN/8giVn4QD52mSlIPB8Qqm664dMsdRzUWwgiGt9gz5fl537/'#$A +
    'sUpnLSHs97Wr+EOsniT025j61CkUtTNITAV+cfMYpnSEgbbQBfc4/GFrUAth5LZi'#$A +
    'qcVpTwKBgCfrNoD1KB+hWnJoFtFoObzYz/O1dKO7PbxyeMUEV5QLc96emE5W8p9w'#$A +
    '8r48LhBgq3Ma2ghmnise9zCaNIjb/DTqoyfD+ZdZat6y+bc2zyWu0tZimSGw5rgs'#$A +
    '4XJeUpxr7Gm8p8tS1nUBWmooqiR18ORZEtVxdBAxUl2VRnekYnQG'#$A +
    '-----END RSA PRIVATE KEY-----';

procedure TOpenSSL_TestCase.Signature;
var
  sSecret: utf8string;
  Secret: TStream;
begin
FSig.PrivateKey := PrivateKey1;
FSig.PrivateKeyStoragePassword := 'honey';
FSig.PrivateKeyStorageCipher := cipher_aes_256_cbc;
Secret := TMemoryStream.Create;
FSig.StoreKeysToStream( Secret, [partPrivate]);
Secret.Position := 0;
SetLength( sSecret, Secret.Size);
if Secret.Size > 0 then
  Secret.ReadBuffer( sSecret[1], Secret.Size);
Secret.Position := 0;
Assert.IsTrue( Pos('-----BEGIN RSA PRIVATE KEY-----', sSecret) > 0, 'OpenSSL key not properly stored.');
Assert.IsTrue( Pos('ENCRYPTED', sSecret) > 0, 'OpenSSL key not properly encrypted.');
FSig.LoadKeysFromStream( Secret, [partPrivate]);
Secret.Free;
Assert.IsTrue( FSig.SignVerify_SelfTest);
Assert.Warn( 'TOpenSSL_TestCase.Signature() test has known memory leaks.');
end;

procedure TOpenSSL_TestCase.TearDown;
begin
FcodecOpenSSL.Free;
FPlain.Free;
FCipher.Free;
FRecon.Free;
FSig.Free;
SetLength( FKey, 0);
SetLength( FIV, 0)
end;




procedure TOpenSSL_TestCase.TwoGenerations;
begin
Assert.IsTrue( FSig.GenerateKeys);
Assert.IsTrue( FSig.GenerateKeys);
Assert.Warn( 'TOpenSSL_TestCase.TwoGenerations() test has known memory leaks.');
end;




end.
