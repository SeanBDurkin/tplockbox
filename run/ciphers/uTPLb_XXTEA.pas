unit uTPLb_XXTEA;
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

interface
uses uTPLb_StreamCipher, uTPLb_BlockCipher, Classes, Types;

type

TXXTEA_LargeBlock = class( TInterfacedObject,
    IStreamCipher, ICryptoGraphicAlgorithm)
  private
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  SeedByteSize: integer;
    function  Parameterize( const Params: IInterface): IStreamCipher;
    function  Start_Encrypt( Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
    function  Start_Decrypt( Key: TSymetricKey; PlainText : TStream): IStreamDecryptor;

  public
    constructor Create;
  end;



  TTEA_Key = array[ 0.. 3 ] of longword;

procedure XXTEA_Encrypt(  // Corrected Block TEA encryption primitive.
  const Key: TTEA_Key;
  const Plaintext: TLongWordDynArray;   // At least 2
  var   Ciphertext: TLongWordDynArray);   // Same length as Plaintext

procedure XXTEA_Decrypt(  // Corrected Block TEA encryption primitive.
  const Key: TTEA_Key;
  const Ciphertext: TLongWordDynArray;   // At least 2
  var   Plaintext : TLongWordDynArray);   // Same length as Ciphertext


implementation













uses SysUtils, Math, uTPLb_StreamUtils, uTPLb_Constants,
     uTPLb_PointerArithmetic, uTPLb_I18n;




type
TXXTEA_LE_Key = class( TSymetricKey)
  public
    constructor Create( Seed: TStream; isStore: boolean);
    {Above: False=from password or random seed; True=from SaveToStream}

    procedure   SaveToStream( Stream: TStream);     override;
    procedure   Burn;                               override;
  end;


TXXTEA_LargeBlock_LE_Encryptor = class( TInterfacedObject,
    IStreamEncryptor, IBlockCipherSelector)
  public
    constructor Create( Owner1: TXXTEA_LargeBlock; Key: TXXTEA_LE_Key; CipherText: TStream);
    destructor  Destroy; override;

  private
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;
    procedure Reset;
  end;


TXXTEA_LargeBlock_LE_Decryptor = class( TInterfacedObject,
    IStreamDecryptor, IBlockCipherSelector)
  public
    constructor Create( Owner1: TXXTEA_LargeBlock; Key: TXXTEA_LE_Key; CipherText: TStream);
    destructor  Destroy; override;

  private
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    procedure Decrypt( const PlainTex: TStream);
    procedure End_Decrypt;
    procedure Reset;
  end;


TXXTEA_FixedBlockCipher = class( TInterfacedPersistent,
            IBlockCipher, ICryptoGraphicAlgorithm)
  private
    FBlockSizeInBits: integer;

    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  BlockSize: integer;  // in units of bits. Must be a multiple of 8.
    function  KeySize: integer;  // in units of bits.
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
    function  SelfTest_Key: ansistring; // Hex string; may be oriented into
    function  SelfTest_Plaintext: ansistring; // Hex string;
    function  SelfTest_Ciphertext: ansistring; // Hex string;

  public
    constructor Create( BlockSizeInBits1: integer);
  end;

TXXTEA_FixedBlockCodec = class( TInterfacedObject, IBlockCodec)
  private
    FKey: TXXTEA_LE_Key; // Non-owned
    FCipher: TXXTEA_FixedBlockCipher;

    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Reset;
    procedure Burn;

  public
    constructor Create( Key1: TXXTEA_LE_Key;
                        Cipher1: TXXTEA_FixedBlockCipher);
  end;


//type
//  TTEA_Key = array[ 0.. 3 ] of longword;
//  TLongs = array of longword;

// XXTEA primitives
// In the following comment section, read ")-" as "}".
{ The original formulation of the Corrected Block TEA algorithm, published by
   David Wheeler and Roger Needham, is as follows:

  #define MX (z>>5^y<<2) + (y>>3^z<<4)^(sum^y) + (k[p&3^e]^z);
  long btea(long* v, long n, long* k) {
    unsigned long z=v[n-1], y=v[0], sum=0, e, DELTA=0x9e3779b9;
    long p, q ;
    if (n > 1) {          /* Coding Part */
      q = 6 + 52/n;
      while (q-- > 0) {
        sum += DELTA;
        e = (sum >> 2) & 3;
        for (p=0; p<n-1; p++) y = v[p+1], z = v[p] += MX;
        y = v[0];
        z = v[n-1] += MX;
      )-
      return 0 ;
    )- else if (n < -1) {  /* Decoding Part */
      n = -n;
      q = 6 + 52/n;
      sum = q*DELTA ;
      while (sum != 0) {
        e = (sum >> 2) & 3;
        for (p=n-1; p>0; p--) z = v[p-1], y = v[p] -= MX;
        z = v[n-1];
        y = v[0] -= MX;
        sum -= DELTA;
      )-
      return 0;
    )-
    return 1;
  )-

According to Needham and Wheeler:
    BTEA will encode or decode n words as a single block where n > 1
        * v is the n word data vector
        * k is the 4 word key
        * n is negative for decoding
        * if n is zero result is 1 and no coding or decoding takes place, otherwise the result is zero
        * assumes 32 bit 'long' and same endian coding and decoding
}



const DELTA = $9e3779b9;       // For Little-endien implementations only.

function Mx(
  const z, y, sum: longword;
  const k: TTEA_Key; const p, e: longword): longword;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
result := (((z shr 5) xor (y shl 2)) +
           ((y shr 3) xor (z shl 4))) xor ((sum xor y) +
           (k[(p and 3) xor e] xor z))
end;


procedure XXTEA_Encrypt(  // Corrected Block TEA encryption primitive.
  const Key: TTEA_Key;
  const Plaintext : TLongWordDynArray;   // At least 2
  var   Ciphertext: TLongWordDynArray);   // Same length as Plaintext
var
  n: integer;
  z, sum: longword;
  e, q, p: LongWord;
begin
n := Length( Plaintext);
Assert( n >= 2, 'Plaintext too short');
z := Plaintext[ n-1];
if Length( Ciphertext) <> n then
  SetLength( Ciphertext, n);
Move( Plaintext[0], Ciphertext[0], n * SizeOf( longword));
sum := 0;
for q := 5 + (52 div n) downto 0 do
  begin
  Inc( sum, DELTA);
  e := (sum shr 2) and 3;
  for p := 0 to n - 2 do
    begin
    z := Ciphertext[ p] + MX( z, Ciphertext[ p + 1], sum, Key, p, e);  // z = v[p] += MX
    Ciphertext[ p] := z
    end;
  z := Ciphertext[ n-1] + MX( z, Ciphertext[ 0], sum, Key, n - 1, e);
  Ciphertext[ n-1] := z
  end
end;



procedure XXTEA_Decrypt(  // Corrected Block TEA decryption primitive.
  const Key: TTEA_Key;
  const Ciphertext: TLongWordDynArray;   // At least 2
  var   Plaintext : TLongWordDynArray);   // Same length as Ciphertext
var
  n: Integer;
  y: LongWord;
  sum: LongWord;
  e: LongWord;
  p: LongWord;
begin
n := Length( Ciphertext);
Assert( n >= 2, 'Ciphertext too short');
if Length( Plaintext) <> n then
  SetLength( Plaintext, n);
Move( Ciphertext[0], Plaintext[0], n * SizeOf( longword));
sum := (6 + (52 div n)) * DELTA;
y := Plaintext[0];
while sum <> 0 do
  begin
  e := (sum shr 2) and 3;
  for p := n - 1 downto 1 do
    begin
    y := Plaintext[p] - Mx( Plaintext[p - 1], y, sum, Key, p, e);
    Plaintext[p] := y
    end;
  y := Plaintext[0] - Mx( Plaintext[n-1], y, sum, Key, 0, e);
  Plaintext[0] := y;
  Dec( sum, DELTA)
  end
end;



{ TXXTEA_LargeBlock }

constructor TXXTEA_LargeBlock.Create;
begin
end;


function TXXTEA_LargeBlock.DefinitionURL: string;
begin
result := 'to be developed'
end;


function TXXTEA_LargeBlock.DisplayName: string;
begin
result := 'XXTEA (large block; little-endien)'
end;



function TXXTEA_LargeBlock.Features: TAlgorithmicFeatureSet;
begin
result := [
  afCryptographicallyWeak,
  afNotImplementedYet,
  afOpenSourceSoftware]
end;



function TXXTEA_LargeBlock.GenerateKey( Seed: TStream): TSymetricKey;
begin
result := TXXTEA_LE_Key.Create( Seed, False)
end;


function TXXTEA_LargeBlock.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := TXXTEA_LE_Key.Create( Store, True)
end;


function TXXTEA_LargeBlock.Parameterize(
  const Params: IInterface): IStreamCipher;
begin
result := nil
// tbd
end;



function TXXTEA_LargeBlock.ProgId: string;
begin
result := XXTEA_Large_ProgId
end;


function TXXTEA_LargeBlock.SeedByteSize: integer;
begin
result := 128
end;


function TXXTEA_LargeBlock.Start_Encrypt(
  Key: TSymetricKey; CipherText: TStream): IStreamEncryptor;
begin
result := TXXTEA_LargeBlock_LE_Encryptor
  .Create( self, Key as TXXTEA_LE_Key, Ciphertext)
end;


function TXXTEA_LargeBlock.Start_Decrypt(
  Key: TSymetricKey; PlainText: TStream): IStreamDecryptor;
begin
result := TXXTEA_LargeBlock_LE_Decryptor
  .Create( self, Key as TXXTEA_LE_Key, Plaintext)
end;


function TXXTEA_LargeBlock.WikipediaReference: string;
begin
result := 'XXTEA'
end;


{ TXXTEA_LE_Key }

procedure TXXTEA_LE_Key.Burn;
begin
// tbd
end;

constructor TXXTEA_LE_Key.Create( Seed: TStream; isStore: boolean);
begin
// tbd
end;

procedure TXXTEA_LE_Key.SaveToStream( Stream: TStream);
begin
// tbd
end;

{ TXXTEA_LargeBlock_LE_Encryptor }

constructor TXXTEA_LargeBlock_LE_Encryptor.Create(
  Owner1: TXXTEA_LargeBlock;
  Key: TXXTEA_LE_Key; CipherText: TStream);
begin
// tbd
end;

destructor TXXTEA_LargeBlock_LE_Encryptor.Destroy;
begin
// tbd
inherited
end;

procedure TXXTEA_LargeBlock_LE_Encryptor.Encrypt( const Plaintext: TStream);
begin
// tbd
end;

procedure TXXTEA_LargeBlock_LE_Encryptor.End_Encrypt;
begin
// tbd
end;

function TXXTEA_LargeBlock_LE_Encryptor.GetBlockCipher: IBlockCipher;
begin

end;

function TXXTEA_LargeBlock_LE_Encryptor.GetChainMode: IBlockChainingModel;
begin

end;

procedure TXXTEA_LargeBlock_LE_Encryptor.Reset;
begin
// tbd
end;

{ TXXTEA_LargeBlock_LE_Decryptor }

constructor TXXTEA_LargeBlock_LE_Decryptor.Create(
  Owner1: TXXTEA_LargeBlock;
  Key: TXXTEA_LE_Key; CipherText: TStream);
begin
// tbd
end;

procedure TXXTEA_LargeBlock_LE_Decryptor.Decrypt( const PlainTex: TStream);
begin
// tbd
end;

destructor TXXTEA_LargeBlock_LE_Decryptor.Destroy;
begin
// tbd
inherited
end;

procedure TXXTEA_LargeBlock_LE_Decryptor.End_Decrypt;
begin
// tbd
end;

function TXXTEA_LargeBlock_LE_Decryptor.GetBlockCipher: IBlockCipher;
begin

end;

function TXXTEA_LargeBlock_LE_Decryptor.GetChainMode: IBlockChainingModel;
begin

end;

procedure TXXTEA_LargeBlock_LE_Decryptor.Reset;
begin
// tbd
end;

{ TXXTEA_FixedBlockCodec }

procedure TXXTEA_FixedBlockCodec.Burn;
begin

end;

constructor TXXTEA_FixedBlockCodec.Create(Key1: TXXTEA_LE_Key;
  Cipher1: TXXTEA_FixedBlockCipher);
begin

end;

procedure TXXTEA_FixedBlockCodec.Decrypt_Block(Plaintext,
  Ciphertext: TMemoryStream);
begin

end;

procedure TXXTEA_FixedBlockCodec.Encrypt_Block(Plaintext,
  Ciphertext: TMemoryStream);
begin

end;

procedure TXXTEA_FixedBlockCodec.Reset;
begin

end;

{ TXXTEA_FixedBlockCipher }

function TXXTEA_FixedBlockCipher.BlockSize: integer;
begin

end;

constructor TXXTEA_FixedBlockCipher.Create(BlockSizeInBits1: integer);
begin

end;

function TXXTEA_FixedBlockCipher.DefinitionURL: string;
begin

end;

function TXXTEA_FixedBlockCipher.DisplayName: string;
begin

end;

function TXXTEA_FixedBlockCipher.Features: TAlgorithmicFeatureSet;
begin

end;

function TXXTEA_FixedBlockCipher.GenerateKey(Seed: TStream): TSymetricKey;
begin

end;

function TXXTEA_FixedBlockCipher.KeySize: integer;
begin

end;

function TXXTEA_FixedBlockCipher.LoadKeyFromStream(
  Store: TStream): TSymetricKey;
begin

end;

function TXXTEA_FixedBlockCipher.MakeBlockCodec(Key: TSymetricKey): IBlockCodec;
begin

end;

function TXXTEA_FixedBlockCipher.ProgId: string;
begin

end;

function TXXTEA_FixedBlockCipher.SeedByteSize: integer;
begin

end;

function TXXTEA_FixedBlockCipher.SelfTest_Ciphertext: ansistring;
begin

end;

function TXXTEA_FixedBlockCipher.SelfTest_Key: ansistring;
begin

end;

function TXXTEA_FixedBlockCipher.SelfTest_Plaintext: ansistring;
begin

end;

function TXXTEA_FixedBlockCipher.WikipediaReference: string;
begin

end;

end.
