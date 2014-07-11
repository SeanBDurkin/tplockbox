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
unit uTPLb_RSA_Primitives;
// This unit implements the primitives specified in RFC-3447
// http://www.ietf.org/rfc/rfc3447.txt  &
// http://www.rfc-editor.org/errata_search.php?rfc=3447

interface
uses
  uTPLB_HugeCardinal, Classes, uTPLb_MemoryStreamPool, uTPLb_StreamCipher,
  uTPLb_BlockCipher;

function I2OSP( x: THugeCardinal; xLen: integer;
                XStream: TStream; Pool: TMemoryStreamPool): boolean;
// Integer-to-Octet-String primitive
// I2OSP converts a huge cardinal to a byte stream of the specified length.
// Inputs:
//   x         The number to be converted.
//   xLen      The intended length in bytes of the output octet stream.
//   Pool      Optional memory stream pool for re-cycling memory.
// Outputs:
//   XStream   The corresponding octet stream is written to this TStream
//              at the input position. The stream is not resized or repositioned
//              before output.
//   result    Returns True if the operation was successful;
//              False if xLen was too small for x.


function OS2IP( XStream: TStream; xLen: integer;
                var x: THugeCardinal;
                Pool: TMemoryStreamPool; MaxBits: integer): boolean;
// Octet-String-to-Integer primitive
// OS2IP converts byte stream of the specified length to a new huge cardinal.
// Inputs:
//   XStream   The octet stream to be converted.
//             The stream is not resized or repositioned
//              before reading, but is read from its current (input) position.
//   xLen      The length in bytes of the octet stream to be read. The
//              stream must have this many bytes readable or an exception
//              will be raised. (units are bytes)
//   Pool      Construction parameter for huge cardinal.
//   MaxBits   Construction parameter for huge cardinal. (units are bits)
// Outputs:
//   x         The number converted to. On input this is nil or dangling.
//              On output, a new THugeCardinal is created.
//   result    Returns True if the operation was successful;
//              False if xLen was too large for MaxBits.


procedure MGF1( mgfSeed: TStream; maskLen: cardinal; mask: TStream);
//   MGF1 is a Mask Generation Function based on SHA-1.
// Inputs:
//   mgfSeed   The octet stream which seeds deterministically the output.
//             The seed is the whole stream.
//   maskLen   The intented length in bytes of the output mask.
// Outputs:
//   mask     The mask or output stream created. Must not be nil on input.


function RSAES_OAEP_ENCRYPT( n, e: THugeCardinal; M, C: TMemoryStream): boolean;
//   RSAES-OAEP-ENCRYPT is the RSA encryption primitive.
// Inputs:
//   n         The RSA encryption public key modulus.
//   e         The RSA encryption public key exponent.
//   M         The plaintext to be encrypted.
// Outputs:
//   C         The encrypted ciphertext. This stream is cleared on input.
//              The byte length of C, mlen must not more the byte length of
//              the modulus (k), less twice the hash length (hLen), less 2.
//                          mLen <= k - 2 * hLen - 2
//             The hash used is SHA-1, so hLen = 20.
//   result    True if successful; False if failed.


function RSAES_OAEP_ENCRYPT_MaxByteLen( n: THugeCardinal): integer;
// Computes the maximum byte length for M in the RSAES_OAEP_ENCRYPT function.
// Inputs:
//   n         The RSA encryption public key modulus.
// Outputs:
//   result    The maximum byte length for M. See comments about the
//             RSAES_OAEP_ENCRYPT function.
//                 result := k - 2 * hLen - 2


function RSAES_OAEP_DECRYPT( d, n: THugeCardinal; C, M: TStream): boolean;
//   RSAES-OAEP-DECRYPT is the RSA decryption primitive.
// Inputs:
//   n         The RSA encryption public key modulus.
//   d         The RSA encryption private key exponent.
//   C         The ciphertext to be decrypted. Length = length of n in bytes.
// Outputs:
//   M         The decrypted plaintext. This stream is cleared on input.
//             The hash used is SHA-1, so hLen = 20.
//   result    True if successful; False if failed.


function EMSA_PSS_ENCODE( M: TStream; emBits, sLen: integer; EM: TStream): boolean;
//   EMSA-PSS-ENCODE is the RSA message encoding primitive used by Sign & Verify.
// Inputs:
//   M         message to be encoded, an octet string.
//   emBits    maximal bit length of the integer OS2IP (EM),
//              at least 8hLen + 8sLen + 9
//   sLen      Intended length in octets of the salt
// Outputs:
//   EM       encoded message, an octet string of length emLen = \ceil
//              (emBits/8)
//   result    True if successful; False if failed (due to emBits being too small.)
// Using:
//   Hash     hash function (hLen denotes the length in octets of the hash
//            function output) used is SHA-1.
//   MGF      mask generation function uses is MGF1.


function RSASSA_PSS_SIGN( d, n: THugeCardinal; M, S: TStream): boolean;
//   RSASSA-PSS-SIGN is the RSA signature generation primitive.
// Inputs:
//   d         The signer's RSA private key exponent.
//   n         The signer's RSA public key modulus.
//   M         The message to be signed.
// Outputs:
//   S         The signature.
//   result    True if successful; False if failed.


function EMSA_PSS_VERIFY( M: TStream; emBits, sLen: integer; EM: TStream): boolean;
//   EMSA-PSS-VERIFY is the RSA signature verification primitive.
//   It is the inverse of  EMSA-PSS-ENCODE


function Generate_RSA_SymetricKey(
  n, e: THugeCardinal; CipherStream: TStream;
  const SymetricCipher: IBlockCipher): TSymetricKey;

function Extract_RSA_SymetricKey(
  d, n: THugeCardinal; CipherStream: TStream;
  const SymetricCipher: IBlockCipher): TSymetricKey;


implementation









uses uTPLb_PointerArithmetic, SysUtils, Math, uTPLb_Hash, uTPLb_SHA1,
     uTPLb_HashDsc, uTPLb_StreamUtils, SyncObjs, uTPLb_Random
{$IFDEF SI}
  , SmartInspect,
  SiAuto
{$ENDIF}
     ;


var
  hLen1: integer = -1;
  HashOfNil: TMemoryStream = nil;
  GlobalsGate: TCriticalSection;

function CreateHashDscObject( var Obj: TObject): IHashDsc;
begin
result := nil;
Obj := TSHA1.Create;
Supports( Obj, IHashDsc, result)
end;


function  AcquireHash( var Obj: TObject): IHash;
var
  HashObj: TSimpleHash;
  Dsc: TObject;
begin
result  := nil;
HashObj := TSimpleHash.Create;
Obj     := HashObj;
Supports( Obj, IHash, result);
result.Hash := CreateHashDscObject( Dsc)
end;


procedure ReleaseHash( Obj: TObject; var H: IHash);
begin
H := nil;
Obj.Free
end;


function  hLen: integer;
var
  H: TObject;
  HI: IHashDsc;
begin
if hLen1 = -1 then
  begin
  HI := CreateHashDscObject( H);
  hLen1 := HI.DigestSize div 8;
  HI := nil;
  end;
result := hLen1
end;


procedure ReleaseHashSharedObjects;
begin
hLen1 := -1
end;

procedure InitUnit_RSA_Primitives;
begin
hLen1     := -1;
HashOfNil := nil;
GlobalsGate := TCriticalSection.Create
{$IFDEF RELEASE};
// Release configuration build.
{$ENDIF}
{$IFDEF DEBUG};
// Debug configuration build.
{$ENDIF}
{$IFDEF SI};
Si.Enabled := True;
SiMain.ClearLog;
SiMain.LogVerbose('Hello');
{$ENDIF}
end;


procedure DoneUnit_RSA_Primitives;
begin
FreeAndNil( GlobalsGate);
FreeAndNil( HashOfNil)
end;




function I2OSP( x: THugeCardinal; xLen: integer; XStream: TStream;
                Pool: TMemoryStreamPool): boolean;
begin
result := xLen >= ((x.BitLength + 7) div 8);
if result then
  x.StreamOut( BigEndien, XStream, xLen);
end;



function OS2IP( XStream: TStream; xLen: integer;
                var x: THugeCardinal;
                Pool: TMemoryStreamPool; MaxBits: integer): boolean;
begin
try
  x := THugeCardinal.CreateFromStreamIn( MaxBits, BigEndien, XStream, Pool);
  result := Assigned( x)
except
  x := nil;
  result := False
end end;




procedure MGF1( mgfSeed: TStream; maskLen: cardinal; mask: TStream);
var
  Counter: longword;
  HashObj: TObject;
  Hash: IHash;
  xfer: integer;
  Buffer: array[0..99] of byte;
    // Assume SizeOf( Buffer) >= (TSHA1.DigestSize div 8)
begin
mask.Size := 0;
if maskLen <= 0 then exit;
Hash := AcquireHash( HashObj);
try
Counter := 0;
repeat
  Hash.Begin_Hash;
  Hash.UpdateMemory( Counter, SizeOf(Counter));
  mgfSeed.Position := 0;
  repeat
    xfer := mgfSeed.Read( Buffer, SizeOf( Buffer));
    if xfer > 0 then
      Hash.UpdateMemory( Buffer, xfer);
  until xfer < SizeOf( Buffer);
  Hash.End_Hash;
  xfer := Min( maskLen, Hash.Hash.DigestSize div 8); // = 20 bytes for SHA-1
  Hash.HashOutputValue.ReadBuffer( Buffer, xfer);
  mask.WriteBuffer( Buffer, xfer);
  Dec( maskLen, xfer);
  Inc( Counter)
until maskLen <= 0;
finally
  ReleaseHash( HashObj, Hash)
end end;



function RSAES_OAEP_ENCRYPT_MaxByteLen( n: THugeCardinal): integer;
begin
result := ((n.BitLength - 1) div 8) - (2 * hLen) - 2;
if result < 0 then
  result := 0
end;


procedure CheckHashOfNil;
var
  HI: IHash;
  H: TObject;
begin
GlobalsGate.Enter;
try
if assigned( HashOfNil) then exit;
HashOfNil := TMemoryStream.Create;
HI := AcquireHash( H);
HI.Begin_Hash;
HI.End_Hash;
HashOfNil.CopyFrom( HI.HashOutputValue, 0);
ReleaseHash( H, HI)
finally
GlobalsGate.Leave
end end;


function RSAES_OAEP_ENCRYPT( n, e: THugeCardinal; M, C: TMemoryStream): boolean;
var
  mLen: integer;
  k: integer; // Largest number of bytes that guarentees a number from those bytes is less than n
  j: integer;
  DB, Seed, dbMask, maskedDB, seedMask, maskedSeed, EM: TMemoryStream;
  m1: THugeCardinal;
{$IFDEF SI}
  Ok: boolean;
{$ENDIF}

  function NewMemoryStream: TMemoryStream;
  begin
  if assigned( n.FPool) then
      result := n.FPool.NewMemoryStream( 0)
    else
      result := TMemoryStream.Create
  end;

  procedure PutByte( S: TStream; byte1: byte);
  begin
  S.Write( byte1, 1)
  end;

begin
{$IFDEF SI}
SiMain.EnterMethod( 'RSAES_OAEP_ENCRYPT');
SiMain.LogStream( 'n', n.FValue);
SiMain.LogInteger( 'n.BitLen', n.BitLength);
SiMain.LogInteger( 'n.MaxBits', n.MaxBits);
SiMain.LogStream( 'M', M);
{$ENDIF}
DB := nil; Seed := nil; dbMask := nil; maskedDB := nil; seedMask := nil;
maskedSeed := nil; EM := nil; m1 := nil;
try
C.Size := 0;
mLen := M.Size;
{$IFDEF SI}
SiMain.LogInteger( 'mLen', mLen);
{$ENDIF}

// Step 1.b
// We require Require 0 < mLen <= (k - (2 * hLen) - 2)
//  where k = (n.BitLength - 1) div 8
result := mLen <= RSAES_OAEP_ENCRYPT_MaxByteLen( n);
if not result then exit;

// Step 2.a
CheckHashOfNil; // HashOfNil has hLen = 20 bytes.
k := (n.BitLength - 1) div 8;
{$IFDEF SI}
SiMain.LogInteger( 'k', k);
{$ENDIF}
DB := NewMemoryStream;

// DB = lHash || PS || 0x01 || M.    Len = k - hLen - 1
DB.Write( HashOfNil.Memory^, HashOfNil.Size); // Dont use CopyFrom on a global.
// Step 2.b and 2.c
for j := 0 to k - mLen - (2 * hLen) - 3 do
  PutByte( DB, 0); // PS = stream of zeros.  Len = k - mLen - (2 * hLen) - 2
PutByte( DB, 1);
DB.CopyFrom( M, 0);
{$IFDEF SI}
SiMain.LogStream( 'DB', DB);
{$ENDIF}

// Step 2.d
// seed = random.  len = hLen
Seed := NewMemoryStream;
Seed.Size := hLen;
RandomFillStream( Seed);
{$IFDEF SI}
SiMain.LogStream( 'Seed', Seed);
{$ENDIF}

// Step 2.e
// dbMask = MGF1( seed, k - hLen - 1). Len = k - hLen - 1
dbMask := NewMemoryStream;
MGF1( Seed, k - hLen - 1, dbMask);
{$IFDEF SI}
SiMain.LogStream( 'dbMask', dbMask);
{$ENDIF}

// Step 2.f
//maskedDB = DB xor dbMask. Len = k - hLen - 1
maskedDB := NewMemoryStream;
maskedDB.Size := k - hLen - 1;
XOR_Streams3( maskedDB, DB, dbMask);
{$IFDEF SI}
SiMain.LogStream( 'maskedDB', maskedDB);
{$ENDIF}

// Step 2.g
// seedMask = MGF1( maskedDB, hLen). Len = hLen
seedMask := NewMemoryStream;
MGF1( maskedDB, hLen, seedMask);
{$IFDEF SI}
SiMain.LogStream( 'seedMask', seedMask);
{$ENDIF}

// Step 2.h
// maskedSeed = seed xor seedMask. Len = hLen
maskedSeed := NewMemoryStream;
maskedSeed.Size := hLen;
XOR_Streams3( maskedSeed, Seed, SeedMask);
{$IFDEF SI}
SiMain.LogStream( 'maskedSeed', maskedSeed);
{$ENDIF}

// Step 2.i
// EM = 0x00 || maskedSeed || maskedDB.  Len = k
EM := NewMemoryStream;
PutByte( EM, 0);
EM.CopyFrom( maskedSeed, 0);
EM.CopyFrom( maskedDB, 0);
{$IFDEF SI}
SiMain.LogStream( 'EM', EM);
{$ENDIF}

// Step 3.a
// m = OS2IP (EM).  m:THugeInteger. m.BitLength = k*8
EM.Position := 0;
{$IFDEF SI}
Ok := OS2IP( EM, k, m1, n.FPool, n.bitlength);
SiMain.LogBoolean( 'm = OS2IP( EM) Ok', Ok);
if Ok then
  begin
  Ok := not (m1.Compare( n) in [rGreaterThan, rEqualTo]);
  SiMain.LogStream( 'm', m1.FValue);
  SiMain.LogInteger( 'm.BitLen', m1.BitLength);
  SiMain.LogInteger( 'm.MaxBits', m1.MaxBits);
  SiMain.LogBoolean( 'm < n', Ok);
  end;
if not Ok then
  raise Exception.Create('RSAES_OAEP_ENCRYPT internal error.');
{$ELSE}
if (not OS2IP( EM, k, m1, n.FPool, n.bitlength)) or
   (m1.Compare( n) in [rGreaterThan, rEqualTo]) then
  raise Exception.Create('RSAES_OAEP_ENCRYPT internal error.');
{$ENDIF}

// Step 3.b
// c = m ** e mod n; // RSAEP
m1.PowerMod( e, n, nil);  // 0 <= m1 < n
{$IFDEF SI}
SiMain.LogStream( 'c', m1.FValue);
{$ENDIF}

// Step 3.c
// C = I2OSP (c, (n.BitLength + 7) div 8). // len (n.BitLength + 7) div 8
if not I2OSP( m1, (n.BitLength + 7) div 8, C, n.FPool) then
  raise Exception.Create('RSAES_OAEP_ENCRYPT internal error.')
{$IFDEF SI}
;SiMain.LogStream( 'C', C);
{$ENDIF}

finally
DB.Free; Seed.Free; dbMask.Free; maskedDB.Free; seedMask.Free;
maskedSeed.Free; EM.Free; m1.Free
{$IFDEF SI}
;SiMain.LeaveMethod( 'RSAES_OAEP_ENCRYPT');
{$ENDIF}
end end;






function RSAES_OAEP_DECRYPT( d, n: THugeCardinal; C, M: TStream): boolean;
//   RSAES-OAEP-DECRYPT is the RSA decryption primitive.
// Inputs:
//   n         The RSA encryption public key modulus.
//   d         The RSA encryption private key exponent.
//   C         The ciphertext to be decrypted. Length = length of n in bytes.
// Outputs:
//   M         The decrypted plaintext. This stream is cleared on input.
//             The hash used is SHA-1, so hLen = 20.
//   result    True if successful; False if failed.
var
  mLen: integer;
  k: integer; // Largest number of bytes that guarentees a number from those bytes is less than n
  bytesRead: cardinal;
  DB, Seed, dbMask, maskedDB, seedMask, maskedSeed, EM, reconHash: TMemoryStream;
  m1: THugeCardinal;
  Y: byte;

  function NewMemoryStream: TMemoryStream;
  begin
  if assigned( n.FPool) then
      result := n.FPool.NewMemoryStream( 0)
    else
      result := TMemoryStream.Create
  end;

begin
{$IFDEF SI}
;SiMain.EnterMethod( 'RSAES_OAEP_DECRYPT');
SiMain.LogStream( 'C', C);
SiMain.LogInteger( 'C.Size', C.Size);
SiMain.LogInteger( 'n.BitLength', n.BitLength);
{$ENDIF}
DB := nil; Seed := nil; dbMask := nil; maskedDB := nil; seedMask := nil;
maskedSeed := nil; EM := nil; m1 := nil; reconHash := nil;

try
M.Size := 0;

// Step 1.b
result := C.Size = (n.BitLength + 7) div 8;
if not result then exit;

// Step 2.a
// c2 = I2OSP (C, (n.BitLength + 7) div 8). // len (n.BitLength + 7) div 8
C.Position := 0;
result := OS2IP( C, (n.BitLength + 7) div 8, m1, n.FPool, n.bitlength);
if not result then exit;
{$IFDEF SI}
SiMain.LogStream( 'c', m1.FValue);
{$ENDIF}

// Step 2.b             m = RSADP (K, c).
m1.PowerMod( d, n, nil);  // 0 <= m1 < n
{$IFDEF SI}
SiMain.LogStream( 'm', m1.FValue);
{$ENDIF}

// Step 2.c            EM = I2OSP (m, k).
k := (n.BitLength - 1) div 8;
EM := NewMemoryStream;
result := I2OSP( m1, k, EM, n.FPool);     // EM len = k
if not result then exit;
{$IFDEF SI}
SiMain.LogInteger( 'k', k);
SiMain.LogStream( 'EM', EM);
{$ENDIF}

// Step 3.a
CheckHashOfNil; // HashOfNil has hLen = 20 bytes.
DB := NewMemoryStream;

// Step 3.b  EM = Y || maskedSeed || maskedDB.
EM.Position := 0;
EM.Read( Y, 1);
maskedSeed := NewMemoryStream;
maskedSeed.CopyFrom( EM, hLen);
maskedDB := NewMemoryStream;
maskedDB.CopyFrom( EM, k - hLen - 1);
result := Y = 0;
if not result then exit;

// Step 3.c
// seedMask = MGF(maskedDB, hLen). Len = hLen
seedMask := NewMemoryStream;
MGF1( maskedDB, hLen, seedMask);

// Step 3.d
// Seed = seed = maskedSeed xor seedMask. Len = hLen
Seed := NewMemoryStream;
Seed.Size := hLen;
XOR_Streams3( Seed, maskedSeed, SeedMask);

// Step 3.e
// dbMask = MGF1( seed, k - hLen - 1). Len = k - hLen - 1
dbMask := NewMemoryStream;
MGF1( Seed, k - hLen - 1, dbMask);

// Step 3.f
// DB = maskedDB xor dbMask. Len = k - hLen - 1
DB := NewMemoryStream;
DB.Size := k - hLen - 1;
XOR_Streams3( DB, maskedDB, dbMask);

// Step 3.g
//   DB = lHash' || PS || 0x01 || M.
DB.Position := 0;
reconHash := NewMemoryStream;
reconHash.CopyFrom( DB, hLen);
repeat
  bytesRead := DB.Read( Y, 1)
until (Y <> 0) or (bytesRead = 0);
mLen := DB.Size - DB.Position;
result := CompareMemoryStreams( reconHash, HashOfNil) and
          (Y = 1) and (bytesRead = 1) and (mLen > 0);
if not result then exit;

// Step 4: Output the message M
M.CopyFrom( DB, mLen)

finally
DB.Free; Seed.Free; dbMask.Free; maskedDB.Free; seedMask.Free;
maskedSeed.Free; EM.Free; m1.Free; reconHash.Free
{$IFDEF SI}
;SiMain.LeaveMethod( 'RSAES_OAEP_DECRYPT');
{$ENDIF}
end end;




function Generate_RSA_SymetricKey(
  n, e: THugeCardinal; CipherStream: TStream;
  const SymetricCipher: IBlockCipher): TSymetricKey;
// 1. Make a random Seed stream of size IBlockCipher.SeedByteSize
// 2. Create the key with IBlockCipher.GenerateKey( Seed)
// 3. Create a mememto of the key with TSymetricKey.SaveToStream
// 4. Measure the size of this stream. It needs to be transmitted.
// 6. Prefix the cipherstream with the count of chunks.
// 5. Process this stream RSAES_OAEP_ENCRYPT_MaxByteLen bytes at a time.
//     Each RSAES_OAEP_ENCRYPT_MaxByteLen or part-thereof is a key chunk.
//     In most cases, we will probably only get one key chunk.
// 6. On each key chunk, call function RSAES_OAEP_ENCRYPT( n, e, M=chunk, C=output);
// 7. Append each C to the cipherstream.
var
  SeedStream, M, C: TMemoryStream;
  PayloadSize: integer;
  MaxChunk, Chunk: integer;
  Ok: boolean;

  function NewMemoryStream( Sz: integer): TMemoryStream;
  begin
  if assigned( n.FPool) then
      result := n.FPool.NewMemoryStream( Sz)
    else
      begin
      result := TMemoryStream.Create;
      result.Size := Sz
      end
  end;

begin
result := nil;
SeedStream := NewMemoryStream( SymetricCipher.SeedByteSize);
try
RandomFillStream( SeedStream);
result := SymetricCipher.GenerateKey( SeedStream);
SeedStream.Position := 0;
result.SaveToStream( SeedStream);
PayloadSize := SeedStream.Position;
SeedStream.Size := PayloadSize;
SeedStream.Position := 0;
CipherStream.Write( PayloadSize, 4);
MaxChunk := RSAES_OAEP_ENCRYPT_MaxByteLen( n);
Ok := False;
M := NewMemoryStream( MaxChunk);
C := NewMemoryStream( (n.BitLength + 7) div 8);
try
while (PayloadSize > 0) and (MaxChunk > 0) do
  begin
  Chunk := Min( PayloadSize, MaxChunk);
  M.Size := Chunk;
  SeedStream.Read( M.Memory^, Chunk);
  Ok := RSAES_OAEP_ENCRYPT( n, e, M, C);
  if not Ok then break;
  CipherStream.Write( C.Memory^, C.Size);
  Dec( PayloadSize, Chunk)
  end;
finally
FreeAndNil( M);
FreeAndNil( C);
end
finally
FreeAndNil( SeedStream)
end;
if not Ok then
  FreeAndNil( result)
end;




function Extract_RSA_SymetricKey(
  d, n: THugeCardinal; CipherStream: TStream;
  const SymetricCipher: IBlockCipher): TSymetricKey;
var
  KeyStream, M, C: TMemoryStream;
  PayloadSize: integer;
  MaxChunk, Chunk, CipherChunkSize: integer;
  Ok: boolean;

  function NewMemoryStream( Sz: integer): TMemoryStream;
  begin
  if assigned( n.FPool) then
      result := n.FPool.NewMemoryStream( Sz)
    else
      begin
      result := TMemoryStream.Create;
      result.Size := Sz
      end
  end;

begin
result := nil;
CipherStream.Read( PayloadSize, 4);
MaxChunk := RSAES_OAEP_ENCRYPT_MaxByteLen( n);
Ok := False;
CipherChunkSize := (n.BitLength + 7) div 8;
KeyStream := NewMemoryStream( 0);
try
M := NewMemoryStream( MaxChunk);
C := NewMemoryStream( CipherChunkSize);
try
while (PayloadSize > 0) and (MaxChunk > 0) do
  begin
  Chunk := Min( PayloadSize, MaxChunk);
  C.Size := CipherChunkSize;
  Ok := (CipherStream.Read( C.Memory^, CipherChunkSize) = CipherChunkSize) and
         RSAES_OAEP_DECRYPT( d, n, C, M) and (M.Size = Chunk);
  if not Ok then break;
  KeyStream.Write( M.Memory^, Chunk);
  Dec( PayloadSize, Chunk)
  end;
finally
FreeAndNil( M);
FreeAndNil( C);
end;
KeyStream.Position := 0;
if Ok then
  result := SymetricCipher.LoadKeyFromStream( KeyStream)
finally
FreeAndNil( KeyStream)
end end;


function EMSA_PSS_ENCODE( M: TStream; emBits, sLen: integer; EM: TStream): boolean;
//   EMSA-PSS-ENCODE is the RSA message encoding primitive used by Sign & Verify.
// Inputs:
//   M         message to be encoded, an octet string.
//   emBits    maximal bit length of the integer OS2IP (EM),
//              at least 8hLen + 8sLen + 9
//   sLen      Intended length in octets of the salt
// Outputs:
//   EM       encoded message, an octet string of length emLen = \ceil
//              (emBits/8)
//   result    True if successful; False if failed (due to emBits being too small.)
// Using:
//   Hash     hash function (hLen denotes the length in octets of the hash
//            function output) used is SHA-1.
//   MGF      mask generation function uses is MGF1.
var
  HashObj: TObject;
  Hash: IHash;
  mHash: TStream;
  hLen1, emLen: integer;
  M_Dash: TStream; // M'
  Zero_8Bytes: uint64;
  aByte, ClearFlags: byte;
  SaltP: int64;
  H, DB, maskedDB, dbMask: TMemoryStream;
  j: integer;
begin
//   1.  [Step 1 of the standard unecessary].
//   2.  Let mHash = Hash(M), an octet string of length hLen.
Zero_8Bytes := 0;
emLen := (emBits + 7) div 8;

M_Dash := TMemoryStream.Create;
mHash := TMemoryStream.Create;
H := TMemoryStream.Create;
DB := TMemoryStream.Create;
maskedDB := TMemoryStream.Create;
dbMask := TMemoryStream.Create;
try

M_Dash.Write( Zero_8Bytes, 8);
Hash := AcquireHash( HashObj);
try
  Hash.HashStream( M);
  hLen1 := Hash.HashOutputValue.Size;
  M_Dash.CopyFrom( Hash.HashOutputValue, 0);
  result := not Hash.isUserAborted;

//   3.  If emLen < hLen + sLen + 2, output "encoding error" and stop.
  result := result and (emLen >= (hLen1 + sLen + 2));
  if not result then exit;

//   4.  Generate a random octet string salt of length sLen; if sLen = 0,
//       then salt is the empty string.
//   5.  Let
//         M' = (0x)00 00 00 00 00 00 00 00 || mHash || salt;
//       M' is an octet string of length 8 + hLen + sLen with eight
//       initial zero octets.
  SaltP := M_Dash.Position;
  M_Dash.CopyFrom( TRandomStream.Instance, sLen);

//   6.  Let H = Hash(M'), an octet string of length hLen.
  Hash.HashStream( M_Dash);
  H.CopyFrom( Hash.HashOutputValue, 0);

//   7.  Generate an octet string PS consisting of emLen - sLen - hLen - 2
//       zero octets.  The length of PS may be 0.
  aByte := 0;
  for j := 1 to emLen - sLen - hLen1 - 2 do
    DB.Write( aByte, 1);

//   8.  Let DB = PS || 0x01 || salt; DB is an octet string of length
//       emLen - hLen - 1.
  aByte := $01;
  DB.Write( aByte, 1);
  M_Dash.Position := SaltP;
  DB.CopyFrom( M_Dash, sLen);

//   9.  Let dbMask = MGF(H, emLen - hLen - 1).
  MGF1( H, emLen - hLen -1, dbMask)
finally
  ReleaseHash( HashObj, Hash)
end;

//   10. Let maskedDB = DB \xor dbMask.
maskedDB.Size := DB.Size;
XOR_Streams3( maskedDB, DB, dbMask);

//   11. Set the leftmost 8emLen - emBits bits of the leftmost octet in
//       maskedDB to zero.
if (8 * emLen) > emBits then
  begin
  maskedDB.Position := 0;
  MaskedDB.Read( aByte, 1);
  // For RFC 3447, "left--most" will be read as Most Signficant.
  ClearFlags := $80;
  for j := emBits to 8 * emLen - 2 do
    ClearFlags := (ClearFlags shr 1) + $80;
  aByte := aByte and (not ClearFlags);
  MaskedDB.Write( aByte, 1)
  end;

//   12. Let EM = maskedDB || H || 0xbc.
//   13. Output EM.
EM.CopyFrom( maskedDB, 0);
EM.CopyFrom( H, 0);
aByte := $BC;
EM.Write( aByte, 1)

finally
M_Dash.Free;
mHash.Free;
H.Free;
DB.Free;
maskedDB.Free;
dbMask.Free
end end;



function EMSA_PSS_VERIFY( M: TStream; emBits, sLen: integer; EM: TStream): boolean;
var
  HashObj: TObject;
  Hash: IHash;
  mHash: TStream;
  hLen1, emLen: integer;
  M_Dash: TStream; // M'
  Zero_8Bytes: uint64;
  aByte, ClearFlags: byte;
  SaltP: int64;
  H, DB, maskedDB, dbMask, reconH: TMemoryStream;
  j: integer;
begin
Zero_8Bytes := 0;
emLen := (emBits + 7) div 8;

M_Dash := TMemoryStream.Create;
mHash := TMemoryStream.Create;
reconH := TMemoryStream.Create;
H := TMemoryStream.Create;
DB := TMemoryStream.Create;
maskedDB := TMemoryStream.Create;
dbMask := TMemoryStream.Create;
try

M_Dash.Write( Zero_8Bytes, 8);
Hash := AcquireHash( HashObj);
try
  Hash.HashStream( M);
  hLen1 := Hash.HashOutputValue.Size;
  M_Dash.CopyFrom( Hash.HashOutputValue, 0);
  result := not Hash.isUserAborted;

  result := result and (emLen >= (hLen1 + sLen + 2));
  if not result then exit;

  EM.Position := 0;
  maskedDB.Size := emLen - hLen1 - 1;
  EM.WriteBuffer( maskedDB.Memory^, maskedDB.Size);
  H.Size := hLen1;
  EM.WriteBuffer( H.Memory^, hLen1);
  EM.WriteBuffer( aByte, 1);
  result := aByte = $BC;
  if not result then exit;
  MGF1( H, emLen - hLen -1, dbMask);
  DB.Size := dbMask.Size;
  XOR_Streams3( maskedDB, dbMask, DB);
  DB.Position := 0;
  for j := 0 to emLen - sLen - hLen - 3 do
    begin
    DB.Write( aByte, 1);
    result := aByte = 0;
    if not result then break
    end;
  if not result then exit;
  DB.Write( aByte, 1);
  result := aByte = $01;
  if not result then exit;
  M_Dash.CopyFrom( DB, sLen);
  M_Dash.Position := 0;
  Hash.HashStream( M_Dash);
  reconH.Size := hLen1;
  Hash.HashOutputValue.Write( reconH, hLen1);
finally
  ReleaseHash( HashObj, Hash)
end;
result := CompareMemoryStreams( reconH, H)
finally
M_Dash.Free;
mHash.Free;
H.Free;
DB.Free;
maskedDB.Free;
dbMask.Free;
reconH.Free
end;
end;








function RSASSA_PSS_SIGN ( d, n: THugeCardinal; M, S: TStream): boolean;
//   RSASSA-PSS-SIGN is the RSA signature generation primitive.
// Inputs:
//   d         The signer's RSA private key exponent.
//   n         The signer's RSA public key modulus.
//   M         The message to be signed.
// Outputs:
//   S         The signature.
//   result    True if successful; False if failed.
begin
// TBD !!!

end;




initialization
InitUnit_RSA_Primitives;

finalization
DoneUnit_RSA_Primitives;

end.
