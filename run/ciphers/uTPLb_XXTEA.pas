unit uTPLb_XXTEA;
{* ***** BEGIN LICENSE BLOCK *****
Copyright 2010 Sean B. Durkin
This file is part of TurboPower LockBox 3. TurboPower LockBox 3 is free
software being offered under a dual licensing scheme: LGPL3 or MPL1.1.

The contents of this file are subject to the Mozilla Public License (MPL)
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Alternatively, you may redistribute it and/or modify it under the terms of
the GNU Lesser General Public License (LGPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

You should have received a copy of the Lesser GNU General Public License
along with TurboPower LockBox 3.  If not, see <http://www.gnu.org/licenses/>.

TurboPower LockBox is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. In relation to LGPL,
see the GNU Lesser General Public License for more details. In relation to MPL,
see the MPL License for the specific language governing rights and limitations
under the License.

The Initial Developer of the Original Code for TurboPower LockBox version 2
and earlier was TurboPower Software.

 * ***** END LICENSE BLOCK ***** *}


interface
uses uTPLb_StreamCipher, uTPLb_BlockCipher, Classes, Types;

type

TXXTEA_LargeBlock = class( TInterfacedObject,
    IStreamCipher, ICryptoGraphicAlgorithm)
  private
    FChaining: IBlockChainingModel;

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













uses SysUtils, Math, uTPLb_StreamUtils, uTPLb_Constants, uTPLb_StreamToBlock,
     uTPLb_PointerArithmetic, uTPLb_I18n, uTPLb_CBC, uTPLb_Random;




type
TXXTEA_LE_Key = class( TSymetricKey)
  public
    FNativeKey: TTEA_Key;
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
    FOwner: TXXTEA_LargeBlock;
    FKey: TXXTEA_LE_Key;
    FCipherText: TStream;
    FBuffer: TBytes;
    FBufLen: integer;
    FisBuffering: boolean;
    FFixedCipher: IStreamCipher;
    FFixedEnc: IStreamEncryptor;

    procedure DelegateEncryptionToAdapter( Ciphertext: TStream);
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;
    procedure Reset;
  end;


TXXTEA_LargeBlock_LE_Decryptor = class( TInterfacedObject,
    IStreamDecryptor, IBlockCipherSelector)
  public
    constructor Create( Owner1: TXXTEA_LargeBlock; Key: TXXTEA_LE_Key; PlainText: TStream);
    destructor  Destroy; override;

  private
    FOwner: TXXTEA_LargeBlock;
    FKey: TXXTEA_LE_Key;
    FPlainText: TStream;

    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    procedure Decrypt( const PlainTex: TStream);
    procedure End_Decrypt;
    procedure Reset;
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
  n: cardinal;
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
var
  BlockCipherSelector: IBlockCipherSelector;
  Chaining: IBlockChainingModel;
  Newbie: TXXTEA_LargeBlock;
begin
result := nil;
if assigned( FChaining) then exit;
if Supports( Params, IBlockCipherSelector, BlockCipherSelector) then
  Chaining    := BlockCipherSelector.GetChainMode;
if not assigned( Chaining) then exit;
Newbie := TXXTEA_LargeBlock.Create;
Newbie.FChaining    := Chaining;
result := Newbie
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
FillChar( FNativeKey, SizeOf( FNativeKey), 0)
end;


constructor TXXTEA_LE_Key.Create( Seed: TStream; isStore: boolean);
begin
Seed.ReadBuffer( FNativeKey, SizeOf( FNativeKey))
end;


procedure TXXTEA_LE_Key.SaveToStream( Stream: TStream);
begin
Stream.WriteBuffer( FNativeKey, SizeOf( FNativeKey))
end;

{ TXXTEA_LargeBlock_LE_Encryptor }

constructor TXXTEA_LargeBlock_LE_Encryptor.Create(
  Owner1: TXXTEA_LargeBlock;
  Key: TXXTEA_LE_Key; CipherText: TStream);
begin
FOwner := Owner1;
FKey   := Key;
FCiphertext := CipherText;
SetLength( FBuffer, 230);
FBufLen := 0;
FisBuffering := True
end;

destructor TXXTEA_LargeBlock_LE_Encryptor.Destroy;
begin
SetLength( FBuffer, 0);
inherited
end;

{
XXTEA is a variable block length cipher. So how does that fit into
our model of block ciphers being of fixed block length? Apply the
following strategy.

Upon encryption buffer up to 209 bytes.
If the input stream is empty, then the output stream is empty, otherwise ...
If the input stream is less than or equal to 199 bytes, then:
  1. Salt with 8 bytes.
  2. Align to quad-byte (adding at least 1 byte)
  3. Encrypt with the block lengh equal to exactly the message length.
      Note: Salting and alignment will add 9 to 12 bytes.
      Note: Chaining mode is irrelevant.
      Note: If the length of the original message m, is n then
        maximum n is:  n + 8 + 1 = 52 * 4
        maximum n = 199 bytes; at which point let length of the ciphertext
      message c, is cl then
        cl @ max n = 208 bytes.

Otherwise if the input stream is more than or equal to 209 bytes, then:
  1. Do not explicitly salt. Salting will be delegated to a StreamToBlock adapter.
  2. Sense the chaining mode from the client.
      Note: ECB and other noNonce modes will supress salting.
  3. Pass the input to the StreamToBlock adapter.
      Note: No explicit quad-byte alignment nor block alignment necessary.
            This work is delegated to the adapter.

Otherwise if the input stream is between 200 and 208 bytes inclusive, then:
  1. Try encrypting the the adapter.
  2. If the adapter's ciphertext was 209 bytes or more, use it.
  3. Otherwise treat as the 199 byte or less case.

Upon decryption buffer up to 208 bytes (52 quad bytes).
If the input stream is empty, then the output stream is empty, otherwise ...
If the input stream is less than or equal to 208 bytes, then:
  1. Decrypt with message as whole block.
  2. Remove 8 prepended bytes of salt.
  3. Remove quad-byte alignment bytes.

Otherwise...
  1. Sense the chaining mode from the client.
      Note: ECB and other noNonce modes will supress salting.
  2. Pass the input to the StreamToBlock adapter.
      Note: No explicit quad-byte alignment nor block alignment necessary.
      Note: No explicit de-salting required.


}
procedure TXXTEA_LargeBlock_LE_Encryptor.DelegateEncryptionToAdapter( Ciphertext: TStream);
var
  Tmp: IStreamCipher;
  PTCopy: TStream;
begin
FFixedCipher := TStreamToBlock_Adapter.Create;
Tmp := FFixedCipher.Parameterize( self);
// The above line casts this to IBlockCipherSelector then calls:
//  GetBlockCipher; and
//  GetChainMode
if assigned( Tmp) then
  FFixedCipher := Tmp;
FFixedEnc := FFixedCipher.Start_Encrypt( FKey, CipherText);
PTCopy := TMemoryStream.Create;
try
  PTCopy.Write( FBuffer[0], FBufLen);
  PTCopy.Position := 0;
  FFixedEnc.Encrypt( PTCopy)
finally
  PTCopy.Free
end end;




procedure TXXTEA_LargeBlock_LE_Encryptor.Encrypt( const Plaintext: TStream);
var
  PlaintextArray, CiphertextArray: TLongWordDynArray;
  Amnt: integer;
  Tmp: IStreamCipher;
  PTCopy: TStream;
begin
Amnt := -1;
if FisBuffering then
  begin
  Amnt := 209 - FBufLen;
  Amnt := Plaintext.Read( FBuffer[FBufLen], Amnt);
  Inc( FBufLen, Amnt);
  if FBufLen >= 209 then
    begin
    DelegateEncryptionToAdapter( FCiphertext);
    FBufLen := 0;
    FisBuffering := False
    end
  end;
if (not FisBuffering) and (Amnt <> 0) then
  FFixedEnc.Encrypt( Plaintext)
end;



procedure TXXTEA_LargeBlock_LE_Encryptor.End_Encrypt;
var
  RequiredSizeIncrease, RequiredSize: integer;
  j: integer;
  doDirect: boolean;
  PlaintextArray, CiphertextArray: TLongWordDynArray;
  Tmp: IStreamCipher;
  PTCopy: TStream;
  TentativeCiphertext: TMemoryStream;
begin
if FisBuffering then
    FFixedEnc.End_Encrypt
  else
    begin
    if FBufLen = 0 then exit;
    doDirect := True;
    if FBufLen >= 200 then
      begin
      TentativeCiphertext := TMemoryStream.Create;
      try
        DelegateEncryptionToAdapter( TentativeCiphertext);
        FFixedEnc.End_Encrypt;
        if TentativeCiphertext.Size >= 209 then
          begin
          FCiphertext.Write( TentativeCiphertext.Memory^, TentativeCiphertext.Size);
          doDirect := False
          end;
      finally
        TentativeCiphertext.Free
        end
      end;
    if doDirect then
      begin
      RequiredSizeIncrease := 4 - (FBufLen mod 4);
      RequiredSize := FBufLen +  RequiredSizeIncrease;
      if RequiredSize < 8 then
        begin
        RequiredSize := 8;
        RequiredSizeIncrease := RequiredSize - FBufLen
        end;
      for j := FBufLen to FBufLen + RequiredSizeIncrease - 1 do
        FBuffer[j] := RequiredSizeIncrease;
      SetLength( PlaintextArray, 8 + RequiredSize);
      TRandomStream.Instance.Write( PlaintextArray[0], 8);
      Move( FBuffer[0], PlaintextArray[2], Length( FBuffer));
      SetLength( CiphertextArray, Length( PlaintextArray));
      XXTEA_Encrypt( FKey.FNativeKey,PlaintextArray, CiphertextArray);
      FCipherText.Write( CiphertextArray[0], Length( CiphertextArray) * 4)
      end
    end;
FFixedEnc := nil;
FFixedCipher := nil
end;




function TXXTEA_LargeBlock_LE_Encryptor.GetBlockCipher: IBlockCipher;
begin

end;

function TXXTEA_LargeBlock_LE_Encryptor.GetChainMode: IBlockChainingModel;
begin
// Called by Parameterize.
result := FOwner.FChaining;
if not assigned( result) then
  result := TCBC.Create
end;

procedure TXXTEA_LargeBlock_LE_Encryptor.Reset;
begin
FBufLen := 0;
FisBuffering := True;
FFixedCipher := nil;
FFixedEnc := nil
end;


{ TXXTEA_LargeBlock_LE_Decryptor }

constructor TXXTEA_LargeBlock_LE_Decryptor.Create(
  Owner1: TXXTEA_LargeBlock;
  Key: TXXTEA_LE_Key; PlainText: TStream);
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


end.
