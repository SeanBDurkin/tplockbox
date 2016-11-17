unit uLockBox_Hashes;
interface
uses DUnitM.UnitTestFramework, TPLB3.Hash, TPLB3.CryptographicLibrary, Classes,
     TPLB3.MemoryStreamPool, uLockBox_TestCases;

type

{$M+}
THash_TestCase = class( TTestCase)
  protected
    FHash: THash;
    FLib: TCryptographicLibrary;
    FReferenceTestSource: utf8string;
    FReferenceTestRefrnc: utf8string;
    FSource: TMemoryStream;
    FRefValue: TMemoryStream;
    FTestValue: TMemoryStream;

    procedure SetUp;                virtual;
    procedure TearDown;             virtual;
    class function HashId: string;  virtual; abstract;

  published
    procedure ReferenceTestVectors; virtual;
  end;



[TestFixture('Hashes.SHA1')]
TSHA1_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  public
    [Setup]    procedure SetUp;                 override;
    [TearDown] procedure TearDown;              override;
  published
    [Test]     procedure ReferenceTestVectors;  override;
  end;


[TestFixture('Hashes.MD5')]
TMD5_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  public
    [Setup]    procedure SetUp;                 override;
    [TearDown] procedure TearDown;              override;
  published
    [IgnoreMemoryLeaks]
    [Test]     procedure ReferenceTestVectors;  override;
  end;




[TestFixture('Hashes.SHA256')]
TSHA256_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  public
    [Setup]    procedure SetUp;                 override;
    [TearDown] procedure TearDown;              override;
  published
    [Test]     procedure ReferenceTestVectors;  override;
    [Test]     procedure ExtraReferenceTests;
  end;


[TestFixture('Hashes.SHA224')]
TSHA224_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  public
    [Setup]    procedure SetUp;                 override;
    [TearDown] procedure TearDown;              override;
  published
    [Test]     procedure ReferenceTestVectors;  override;
    [Test]     procedure ExtraReferenceTests;
  end;


[TestFixture('Hashes.SHA512')]
TSHA512_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  public
    [Setup]    procedure SetUp;                 override;
    [TearDown] procedure TearDown;              override;
  published
    [Test]     procedure ReferenceTestVectors;  override;
    [Test]     procedure ExtraReferenceTests;
  end;

[TestFixture('Hashes.SHA384')]
TSHA384_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  public
    [Setup]    procedure SetUp;                 override;
    [TearDown] procedure TearDown;              override;
  published
    [Test]     procedure ReferenceTestVectors;  override;
  end;

[TestFixture('Hashes.SHA512.224')]
TSHA512_224_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  public
    [Setup]    procedure SetUp;                 override;
    [TearDown] procedure TearDown;              override;
  published
    [Test]     procedure ReferenceTestVectors;  override;
  end;

[TestFixture('Hashes.SHA512.256')]
TSHA512_256_TestCase = class( THash_TestCase)
  protected
    class function HashId: string; override;
  public
    [Setup]    procedure SetUp;                 override;
    [TearDown] procedure TearDown;              override;
  published
    [Test]     procedure ReferenceTestVectors;  override;
  end;



implementation









uses TPLB3.SHA2, TPLB3.Constants, TPLB3.BinaryUtils, TPLB3.HashDsc,
     SysUtils, TPLB3.StreamUtils;


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


//function MyEncodeUTF8( const ValueUTF16LE: string) : utf8string;
//var
//  B: TBytes;
//begin
//  B      := TEncoding.UTF8.GetBytes( ValueUTF16LE);
//  result := TEncoding.UTF8.GetString( B)
//end;


procedure THash_TestCase.TearDown;
begin
FHash.Free;
FLib.Free;
FSource.Free;
FRefValue.Free;
FTestValue.Free;
FReferenceTestSource := '';
FReferenceTestRefrnc := ''
end;



procedure THash_TestCase.ReferenceTestVectors;
begin
FSource.Write( FReferenceTestSource[low( FReferenceTestSource)], Length( FReferenceTestSource) * SizeOf( AnsiChar));
{$WARNINGS OFF}
Read_BigEndien_u32_Hex( FReferenceTestRefrnc, FRefValue);
{$WARNINGS ON}
FHash.HashStream( FSource);
FTestValue.CopyFrom( FHash.HashOutputValue, 0);
FHash.Burn;
Assert.IsTrue( CompareMemoryStreams( FRefValue, FTestValue), Format(
  'Hash %s failed it''s standard reference test.',[FHash.Hash]))
end;


procedure TSHA256_TestCase.ExtraReferenceTests;
begin
FReferenceTestSource := 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
FReferenceTestRefrnc := '248D6A61 D20638B8 ' +
  'E5C02693 0C3E6039 A33CE459 64FF2167 F6ECEDD4 19DB06C1';
ReferenceTestVectors
end;


procedure TSHA224_TestCase.ExtraReferenceTests;
begin
FReferenceTestSource := 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
FReferenceTestRefrnc := '75388B16 512776CC 5DBA5DA1 FD890150 B0C6455C B4F58B19 52522525';
ReferenceTestVectors
end;




class function TMD5_TestCase.HashId: string;
begin
result := 'native.hash.MD5'
end;





procedure TMD5_TestCase.ReferenceTestVectors;
begin
  inherited;

end;

procedure TMD5_TestCase.SetUp;
begin
  inherited;

end;

procedure TMD5_TestCase.TearDown;
begin
  inherited;

end;

{ TSHA1_TestCase }

class function TSHA1_TestCase.HashId: string;
begin
result := 'native.hash.SHA-1'
end;

// Test vectors from
//  http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA256.pdf

{ TSHA2_TestCase }

class function TSHA256_TestCase.HashId: string;
begin
result := SHA256_ProgId
end;

procedure TSHA256_TestCase.ReferenceTestVectors;
begin
  inherited;

end;

procedure TSHA256_TestCase.SetUp;
begin
  inherited;

end;

procedure TSHA256_TestCase.TearDown;
begin
  inherited;

end;

class function TSHA224_TestCase.HashId: string;
begin
result := SHA224_ProgId
end;


procedure TSHA224_TestCase.ReferenceTestVectors;
begin
  inherited;

end;

procedure TSHA224_TestCase.SetUp;
begin
  inherited;

end;

procedure TSHA224_TestCase.TearDown;
begin
  inherited;

end;

procedure TSHA512_TestCase.ExtraReferenceTests;
begin
FReferenceTestSource := 'abcdefghbcdefghicdefghijdefghijkefghijklfghijk' +
 'lmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu';
FReferenceTestRefrnc := '8E959B75 DAE313DA 8CF4F728 14FC143F ' +
 '8F7779C6 EB9F7FA1 7299AEAD B6889018 501D289E 4900F7E4 ' +
 '331B99DE C4B5433A C7D329EE B6DD2654 5E96E55B 874BE909';
ReferenceTestVectors
end;

class function TSHA512_TestCase.HashId: string;
begin
result := SHA512_ProgId
end;


procedure TSHA512_TestCase.ReferenceTestVectors;
begin
  inherited;

end;

procedure TSHA512_TestCase.SetUp;
begin
  inherited;

end;

procedure TSHA512_TestCase.TearDown;
begin
  inherited;

end;

class function TSHA384_TestCase.HashId: string;
begin
result := SHA384_ProgId
end;


class function TSHA512_224_TestCase.HashId: string;
begin
result := SHA512_224_ProgId
end;

class function TSHA512_256_TestCase.HashId: string;
begin
result := SHA512_256_ProgId
end;


procedure TSHA1_TestCase.ReferenceTestVectors;
begin
  inherited;

end;

procedure TSHA1_TestCase.SetUp;
begin
  inherited;

end;

procedure TSHA1_TestCase.TearDown;
begin
  inherited;

end;

procedure TSHA384_TestCase.ReferenceTestVectors;
begin
  inherited;

end;

procedure TSHA384_TestCase.SetUp;
begin
  inherited;

end;

procedure TSHA384_TestCase.TearDown;
begin
  inherited;

end;

procedure TSHA512_224_TestCase.ReferenceTestVectors;
begin
  inherited;

end;

procedure TSHA512_224_TestCase.SetUp;
begin
  inherited;

end;

procedure TSHA512_224_TestCase.TearDown;
begin
  inherited;

end;

procedure TSHA512_256_TestCase.ReferenceTestVectors;
begin
  inherited;

end;

procedure TSHA512_256_TestCase.SetUp;
begin
  inherited;

end;

procedure TSHA512_256_TestCase.TearDown;
begin
  inherited;

end;



end.
