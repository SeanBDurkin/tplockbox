{* ***** BEGIN LICENSE BLOCK *****
Copyright 2010 Sean B. Durkin

This file is part of TurboPower LockBox.
TurboPower LockBox is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TurboPower LockBox is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the Lesser GNU General Public License
along with TurboPower LockBox.  If not, see <http://www.gnu.org/licenses/>.

The Initial Developer of the Original Code for TurboPower LockBox version 2
and earlier was TurboPower Software.

 * ***** END LICENSE BLOCK ***** *}
unit uTPLb_TwoFish;
interface
uses Classes, uTPLb_BlockCipher, uTPLb_StreamCipher, uTPLb_Decorators;

type
{$IF compilerversion >= 21}
{$RTTI EXPLICIT METHODS([vcPrivate, vcProtected, vcPublic, vcPublished]) PROPERTIES([vcPublished])}
// Exposes the attribute on the SeedByteSize method.
{$IFEND}

{$IF compilerversion >= 21} [DesignDescription(
'From Wikipedia: QUOTE:'#13#10 +
'Twofish is a symmetric key block cipher with a block size of 128 bits and '#13#10 +
'key sizes up to 256 bits. It was one of the five finalists of the Advanced '#13#10 +
'Encryption Standard contest, but was not selected for standardisation. '#13#10 +
'Twofish is related to the earlier block cipher Blowfish.'#13#10 +
'END QUOTE'
)] {$IFEND}
TTwoFish = class( TInterfacedObject,
    IBlockCipher, ICryptoGraphicAlgorithm, IControlObject
{$IF compilerversion < 21}
    ,IVariableSeedSize
{$IFEND}
    )
  private
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  BlockSize: integer;

{$IF compilerversion >= 21}
    [IntegerRange( 192, 256)]
{$IFEND}
    function  KeySize: integer;

    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
    function  SelfTest_Key: ansistring;
    function  SelfTest_Plaintext: ansistring;
    function  SelfTest_Ciphertext: ansistring;
    function ControlObject: TObject;

{$IF compilerversion >= 21}
    [IntegerRange( 1, 32)]
    function  SeedByteSize: integer;
{$ELSE}
    function  SeedByteSize: integer;
    function  MinSeedByteSize: integer;
    function  MaxSeedByteSize: integer;
{$IFEND}

  public
    constructor Create;
  end;


implementation




uses uTPLb_Constants, SysUtils, DCPtwofish_LB3Modified, Math;





type
TTwofishKey = class( TSymetricKey)
  public
    FKeySeed: TBytes;
    FKeySize: integer;
    FSubKeys: TSubKeys;
    FSBox: TSBox;

    constructor GenerateFromSeed( Seed: TStream);
    constructor LoadFromStream( Store: TStream);

    procedure   SaveToStream( Stream: TStream);  override;
    procedure   Burn;                            override;
  end;


TTwofishBlockCodec = class( TInterfacedObject, IBlockCodec)
  private
    FOwner: TTwoFish;
    FKey: TTwofishKey;

    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Reset;
    procedure Burn;

  public
    constructor Create( Owner1: TTwoFish; Key1: TTwofishKey);
  end;


var
  hasPreComp: boolean = False;


procedure InitUnit_TwoFish;
begin
hasPreComp := False
end;


procedure DoneUnit_TwoFish;
begin
end;


procedure CheckPreComp;
begin
if hasPreComp then exit;
hasPreComp := True;
DCP_towfish_Precomp
end;



function TTwoFish.BlockSize: integer;
begin
result := 128
end;


function TTwoFish.ControlObject: TObject;
begin
result := self
end;


constructor TTwoFish.Create;
begin
end;


function TTwoFish.DefinitionURL: string;
begin
result := 'http://www.schneier.com/paper-twofish-paper.pdf'
end;


function TTwoFish.DisplayName: string;
begin
result := 'Twofish'
end;



function TTwoFish.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware]
end;


function TTwoFish.GenerateKey( Seed: TStream): TSymetricKey;
begin
result := TTwofishKey.GenerateFromSeed( Seed)
end;


function TTwoFish.KeySize: integer;
begin
result := 256
// This is a nominal value only. The actual key size will only
//  be known at key generation time. It will deemed either be 128, 192 or 256.
end;


function TTwoFish.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := TTwofishKey.LoadFromStream( Store)
end;


function TTwoFish.MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
begin
result := TTwofishBlockCodec.Create( self, Key as TTwofishKey)
end;


function TTwoFish.ProgId: string;
begin
result := Twofish_ProgId
end;


function TTwoFish.SeedByteSize: integer;
// Variable. Up to 256 bits. In whole number of 8 bits. At least 8.
begin
result := 32
end;

{$IF compilerversion < 21}
// Equivalent to [IntegerRange( 1, 32)]
function TTwoFish.MinSeedByteSize: integer;
begin
result := 1
end;

function TTwoFish.MaxSeedByteSize: integer;
begin
result := 32
end;
{$IFEND}

//from http://www.schneier.com/code/ecb_ival.txt
//KEYSIZE=192
//I=10
//KEY=AE8109BFDA85C1F2C5038B34ED691BFF3AF6F7CE5BD35EF1
//PT=893FD67B98C550073571BD631263FC78
//CT=16434FC9C8841A63D58700B5578E8F67


function TTwoFish.SelfTest_Ciphertext: ansistring;
begin
result := '16434FC9C8841A63D58700B5578E8F67'
end;




function TTwoFish.SelfTest_Key: ansistring;
begin
result := 'AE8109BFDA85C1F2C5038B34ED691BFF3AF6F7CE5BD35EF1'
end;



function TTwoFish.SelfTest_Plaintext: ansistring;
begin
result := '893FD67B98C550073571BD631263FC78'
end;



function TTwoFish.WikipediaReference: string;
begin
result := 'Twofish'
end;



{ TTwofishKey }

constructor TTwofishKey.GenerateFromSeed( Seed: TStream);
var
  L: integer;
begin
L := Seed.Size;
SetLength( FKeySeed, L);
if L > 0 then
  begin
  Seed.Position := 0;
  Seed.Read( FKeySeed[0], L)
  end;

if L < 16 then
    FKeySize := 128

  else if L <= 24 then
    FKeySize := 192

  else
    FKeySize := 256;

CheckPreComp;
DCP_twofish_InitKey( FKeySeed[0], Min( L*8, 256), FSubKeys, FSBox)
end;



constructor TTwofishKey.LoadFromStream( Store: TStream);
begin
CheckPreComp;
GenerateFromSeed( Store)
end;



procedure TTwofishKey.SaveToStream( Stream: TStream);
begin
Stream.Write( FKeySeed[0], Length( FKeySeed))
end;


procedure TTwofishKey.Burn;
var
  L: integer;
begin
L := Length( FKeySeed);
if L > 0 then
  begin
  FillChar( FKeySeed[0], L, 0);
  SetLength( FKeySeed, 0)
  end;
FKeySize := 0;
FillChar( FSubKeys, SizeOf( FSubKeys), 0);
FillChar( FSBox, SizeOf( FSBox), 0)
end;


{ TTwofishBlockCodec }

constructor TTwofishBlockCodec.Create( Owner1: TTwoFish; Key1: TTwofishKey);
begin
CheckPreComp;
FOwner := Owner1;
FKey   := Key1
end;


procedure TTwofishBlockCodec.Encrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
var
  InData, OutData: T128;
begin
Move( Plaintext.Memory^, InData, SizeOf( T128));
DCP_twofish_EncryptECB( FKey.FSubKeys, FKey.FSBox, InData, OutData);
Move( OutData, Ciphertext.Memory^, SizeOf( T128))
end;



procedure TTwofishBlockCodec.Decrypt_Block(
  Plaintext, Ciphertext: TMemoryStream);
var
  InData, OutData: T128;
begin
Move( Ciphertext.Memory^, InData, SizeOf( T128));
DCP_twofish_DecryptECB( FKey.FSubKeys, FKey.FSBox, InData, OutData);
Move( OutData, Plaintext.Memory^, SizeOf( T128))
end;



procedure TTwofishBlockCodec.Reset;
begin
end;


procedure TTwofishBlockCodec.Burn;
begin
end;



initialization
InitUnit_TwoFish;

finalization
DoneUnit_TwoFish;

end.
