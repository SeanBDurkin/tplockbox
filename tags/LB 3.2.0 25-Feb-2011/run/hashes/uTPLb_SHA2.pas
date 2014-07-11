{* ***** BEGIN LICENSE BLOCK *****
Copyright 2011 Sean B. Durkin
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

unit uTPLb_SHA2;
interface
uses classes, uTPLb_HashDsc, uTPLb_StreamCipher, uTPLb_Decorators;

type

TSHA2FamilyMember = (SHA_224, SHA_256, SHA_348, SHA_512, SHA_512_224, SHA_512_256);

{$IF compilerversion >= 21} [DesignDescription(
'SHA-2 is a family of hashes designed to supersede SHA-1. As of the date ' +
'of this writing, there are no discovered collisions for any SHA-2 member.'
)] {$IFEND}
TSHA2 = class( TInterfacedObject,
    IHashDsc, ICryptoGraphicAlgorithm, IControlObject)
  private
    FAlgorithm: TSHA2FamilyMember;

    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DigestSize: integer;  // in units of bits. Must be a multiple of 8.
    function  UpdateSize: integer; // Size that the input to the Update must be.
    function  MakeHasher( const Params: IInterface): IHasher;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  ControlObject: TObject;

  public
    constructor Create( Algorithm1: TSHA2FamilyMember);
  end;





implementation






uses uTPLb_BinaryUtils, uTPLb_StreamUtils, uTPLb_PointerArithmetic,
     uTPLb_IntegerUtils, uTPLB_Constants, uTPLb_I18n;

type
  TaToh = (a, b, c, d, e, f, g, h);
  TSHA256_M_Array = array[0..15] of uint32;
  TSHA512_M_Array = array[0..15] of uint64;
  TSHA256_W_Array = array[0..63] of uint32;
  TSHA512_W_Array = array[0..79] of uint64;
  TaToh_Array32 = array[ TaToh ] of uint32;
  TaToh_Array64 = array[ TaToh ] of uint64;

TSHA2_BaseHasher = class( TInterfacedObject, IHasher)
  private
    constructor Create( Algorithm1: TSHA2FamilyMember);

  protected
    FAlgorithm: TSHA2FamilyMember;
    FCount: uint64;
    FisEnding: boolean;

  protected
    procedure  InitHashVectors;                   virtual; abstract;
    procedure  Update( Source{in}: TMemoryStream);  virtual; abstract;
    procedure  End_Hash( PartBlock{in}: TMemoryStream; Digest: TStream);  virtual; abstract;
    procedure  Burn;                              virtual; abstract;
    function   SelfTest_Source: ansistring;       virtual; abstract;
    function   SelfTest_ReferenceHashValue: ansistring;  virtual; abstract;
  end;


TSHA256 = class( TSHA2_BaseHasher)
  protected
    FH: TaToh_Array32;

    procedure  InitHashVectors;                    override;
    procedure  Update( Source{in}: TMemoryStream); override;
    procedure  End_Hash( PartBlock{in}: TMemoryStream; Digest: TStream);  override;
    procedure  Burn;                              override;
    function   SelfTest_Source: ansistring;       override;
    function   SelfTest_ReferenceHashValue: ansistring;  override;
  end;


TSHA224 = class( TSHA256)
  protected
    procedure  InitHashVectors;                          override;
    function   SelfTest_Source: ansistring;              override;
    function   SelfTest_ReferenceHashValue: ansistring;  override;
  end;


TSHA512 = class( TSHA2_BaseHasher)
  protected
    FH: TaToh_Array64;

    procedure  InitHashVectors;                    override;
    procedure  Update( Source{in}: TMemoryStream); override;
    procedure  End_Hash( PartBlock{in}: TMemoryStream; Digest: TStream);  override;
    procedure  Burn;                              override;
    function   SelfTest_Source: ansistring;       override;
    function   SelfTest_ReferenceHashValue: ansistring;  override;
  end;


TSHA384 = class( TSHA512)
  protected
    procedure  InitHashVectors;                          override;
    function   SelfTest_Source: ansistring;              override;
    function   SelfTest_ReferenceHashValue: ansistring;  override;
  end;


TSHA512_224 = class( TSHA512)
  protected
    procedure  InitHashVectors;                          override;
    function   SelfTest_Source: ansistring;              override;
    function   SelfTest_ReferenceHashValue: ansistring;  override;
  end;


TSHA512_256 = class( TSHA512)
  protected
    procedure  InitHashVectors;                          override;
    function   SelfTest_Source: ansistring;              override;
    function   SelfTest_ReferenceHashValue: ansistring;  override;
  end;



///////////////////////////////////////
// SHA-2 Primitives

{ 4.2.2 SHA-224 and SHA-256 Constants
  SHA-224 and SHA-256 use the same sequence of sixty-four constant 32-bit words,
  These words represent the first thirty-two bits of the fractional parts of
  the cube roots of the first sixty-four prime numbers.  In hex, these constant
  words are (from left to right). }

const K_Values_32: array[ 0..63 ] of uint32 = (
  $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
  $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
  $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
  $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
  $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
  $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
  $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
  $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);


{ 4.2.3 SHA-384, SHA-512, SHA-512/224 and SHA-512/256 Constants
  SHA-384, SHA-512, SHA-512/224 and SHA-512/256 use the same sequence of eighty
  constant 64-bit words,  .  These words represent the first sixty-four bits of
  the fractional parts of the cube roots of the first eighty prime numbers.
  In hex, these constant words are (from left to right) }

const K_Values_64: array[ 0..79 ] of uint64 = (
  $428a2f98d728ae22, $7137449123ef65cd, $b5c0fbcfec4d3b2f, $e9b5dba58189dbbc,
  $3956c25bf348b538, $59f111f1b605d019, $923f82a4af194f9b, $ab1c5ed5da6d8118,
  $d807aa98a3030242, $12835b0145706fbe, $243185be4ee4b28c, $550c7dc3d5ffb4e2,
  $72be5d74f27b896f, $80deb1fe3b1696b1, $9bdc06a725c71235, $c19bf174cf692694,
  $e49b69c19ef14ad2, $efbe4786384f25e3, $0fc19dc68b8cd5b5, $240ca1cc77ac9c65,
  $2de92c6f592b0275, $4a7484aa6ea6e483, $5cb0a9dcbd41fbd4, $76f988da831153b5,
  $983e5152ee66dfab, $a831c66d2db43210, $b00327c898fb213f, $bf597fc7beef0ee4,
  $c6e00bf33da88fc2, $d5a79147930aa725, $06ca6351e003826f, $142929670a0e6e70,
  $27b70a8546d22ffc, $2e1b21385c26c926, $4d2c6dfc5ac42aed, $53380d139d95b3df,
  $650a73548baf63de, $766a0abb3c77b2a8, $81c2c92e47edaee6, $92722c851482353b,
  $a2bfe8a14cf10364, $a81a664bbc423001, $c24b8b70d0f89791, $c76c51a30654be30,
  $d192e819d6ef5218, $d69906245565a910, $f40e35855771202a, $106aa07032bbd1b8,
  $19a4c116b8d2d0c8, $1e376c085141ab53, $2748774cdf8eeb99, $34b0bcb5e19b48a8,
  $391c0cb3c5c95a63, $4ed8aa4ae3418acb, $5b9cca4f7763e373, $682e6ff3d6b2b8a3,
  $748f82ee5defb2fc, $78a5636f43172f60, $84c87814a1f0ab72, $8cc702081a6439ec,
  $90befffa23631e28, $a4506cebde82bde9, $bef9a3f7b2c67915, $c67178f2e372532b,
  $ca273eceea26619c, $d186b8c721c0c207, $eada7dd6cde0eb1e, $f57d4f7fee6ed178,
  $06f067aa72176fba, $0a637dc5a2c898a6, $113f9804bef90dae, $1b710b35131c471b,
  $28db77f523047d84, $32caab7b40c72493, $3c9ebe0a15c9bebc, $431d67c49c100d4c,
  $4cc5d4becb3e42b6, $597f299cfc657e2a, $5fcb6fab3ad6faec, $6c44198c4a475817);


 // FIPS PUB 180-4 | 4.1.2 | Ch()  function
function Ch32( x, y, z: uint32): uint32;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// Ch(x,y,z) = (x ^ y) xor (not x ^ z)
result := (x and y) xor ((not x) and z)
end;


 // FIPS PUB 180-4 | 4.1.3 | Ch()  function
function Ch64( x, y, z: uint64): uint64;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// Ch(x,y,z) = (x ^ y) xor (not x ^ z)
result := (x and y) xor ((not x) and z)
end;


 // FIPS PUB 180-4 | 4.1.2 | Maj()  function
function Maj32( x, y, z: uint32): uint32;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// Ch(x,y,z) = (x ^ y) xor (x ^ z) xor (y ^ z)
result := (x and y) xor (x and z) xor (y and z)
end;


 // FIPS PUB 180-4 | 4.1.3 | Maj()  function
function Maj64( x, y, z: uint64): uint64;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// Ch(x,y,z) = (x ^ y) xor (x ^ z) xor (y ^ z)
result := (x and y) xor (x and z) xor (y and z)
end;


 // FIPS PUB 180-4 | 4.2.4 | ROTR operation
function Rotr32( Shift: integer; x: uint32): uint32;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
result := (x shr Shift) or (x shl (32 - Shift))
end;


function Rotr64( Shift: integer; x: uint64): uint64;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
result := (x shr Shift) or (x shl (64 - Shift))
end;



 // FIPS PUB 180-4 | 4.1.2 | SIGMA[0,256]()  function
function BigSigma0_256_32( x: uint32): uint32;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// SIGMA[0,256](x) = ROTR[2](x) xor ROTR[13](x) xor ROTR[22](x)
result := Rotr32( 2, x) xor Rotr32( 13, x) xor Rotr32( 22, x)
end;



 // FIPS PUB 180-4 | 4.1.2 | SIGMA[1,256]()  function
function BigSigma1_256_32( x: uint32): uint32;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// SIGMA[1,256](x) = ROTR[6](x) xor ROTR[11](x) xor ROTR[25](x)
result := Rotr32( 6, x) xor Rotr32( 11, x) xor Rotr32( 25, x)
end;



 // FIPS PUB 180-4 | 4.1.3 | SIGMA[0,512]()  function
function BigSigma0_512_64( x: uint64): uint64;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// SIGMA[0,512](x) = ROTR[28](x) xor ROTR[34](x) xor ROTR[39](x)
result := Rotr64( 28, x) xor Rotr64( 34, x) xor Rotr64( 39, x)
end;



 // FIPS PUB 180-4 | 4.1.3 | SIGMA[1,512]()  function
function BigSigma1_512_64( x: uint64): uint64;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// SIGMA[1,512](x) = ROTR[14](x) xor ROTR[18](x) xor ROTR[41](x)
result := Rotr64( 14, x) xor Rotr64( 18, x) xor Rotr64( 41, x)
end;



 // FIPS PUB 180-4 | 4.1.2 | sigma[0,256]()  function
function LittleSigma0_256_32( x: uint32): uint32;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// sigma[0,256](x) = ROTR[7](x) xor ROTR[18](x) xor SHR[3](x)
result := Rotr32( 7, x) xor Rotr32( 18, x) xor (x shr 3)
end;



 // FIPS PUB 180-4 | 4.1.2 | sigma[1,256]()  function
function LittleSigma1_256_32( x: uint32): uint32;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// sigma[1,256](x) = ROTR[17](x) xor ROTR[19](x) xor SHR[10](x)
result := Rotr32( 17, x) xor Rotr32( 19, x) xor (x shr 10)
end;



 // FIPS PUB 180-4 | 4.1.3 | sigma[0,512]()  function
function LittleSigma0_512_64( x: uint64): uint64;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// sigma[0,512](x) = ROTR[1](x) xor ROTR[8](x) xor SHR[7](x)
result := Rotr64( 1, x) xor Rotr64( 8, x) xor (x shr 7)
end;



 // FIPS PUB 180-4 | 4.1.3 | sigma[1,512]()  function
function LittleSigma1_512_64( x: uint64): uint64;
{$IF CompilerVersion >= 17.0} inline; {$IFEND}
begin
// sigma[1,512](x) = ROTR[19](x) xor ROTR[61](x) xor SHR[6](x)
result := Rotr64( 19, x) xor Rotr64( 61, x) xor (x shr 6)
end;


function SHA_StylePadding_64_in_512( MessageLengthInBits: uint64; var Pad): integer;
// The padded message will be 512 bits or 64 bytes.
// Assume Pad is at least that long.
// Return number of bytes of padding.
var
  PadByte: PByte;
  BigEndienLenInBits: uint64;
  Excess, Zeros: integer;
begin
PadByte  := @Pad;
PadByte^ := $80;
Inc( PadByte);
Excess := ((MessageLengthInBits + 7) div 8) mod 64;
Zeros := 55 - Excess;
if Zeros < 0 then
  Inc( Zeros, 64);
if Zeros > 0 then
  begin
  FillChar( PadByte^, Zeros, $00);
  Inc( PadByte, Zeros)
  end;
BigEndienLenInBits := SwapEndien_u64( MessageLengthInBits);
Move( BigEndienLenInBits, PadByte^, 8);
result := Zeros + 9
end;


function SHA_StylePadding_128_in_1024(
  Lo64MessageLengthInBits, Hi64MessageLengthInBits: uint64; var Pad): integer;
// The padded message will be 1024 bits or 128 bytes.
// Assume Pad is at least that long.
// Return number of bytes of padding.
var
  PadByte: PByte;
  BigEndienLenInBits: uint64;
  Excess, Zeros: integer;
begin
PadByte  := @Pad;
PadByte^ := $80;
Inc( PadByte);
Excess := ((Lo64MessageLengthInBits + 7) div 8) mod 128;
Zeros := 111 - Excess;
if Zeros < 0 then
  Inc( Zeros, 128);
if Zeros > 0 then
  begin
  FillChar( PadByte^, Zeros, $00);
  Inc( PadByte, Zeros)
  end;
BigEndienLenInBits := SwapEndien_s64( Hi64MessageLengthInBits);
Move( BigEndienLenInBits, PadByte^, 8);
Inc( PadByte, 8);
BigEndienLenInBits := SwapEndien_s64( Lo64MessageLengthInBits);
Move( BigEndienLenInBits, PadByte^, 8);
result := Zeros + 17
end;

procedure SHA256_Step1(
  const M: TSHA256_M_Array;
  var W: TSHA256_W_Array);
var
  t: integer;
  Temp32: uint32;
begin
for t := 0 to 15 do
  W[t] := M[t];
for t := 16 to 63 do
  begin
  Temp32 := LittleSigma1_256_32( W[t-2]) +
            W[t-7] +
            LittleSigma0_256_32( W[t-15]) +
            W[t-16];
  W[t] := Temp32
  end
end;

procedure SHA512_Step1(
  const M: TSHA512_M_Array;
  var W: TSHA512_W_Array);
var
  t: integer;
  Temp64: uint64;
begin
for t := 0 to 15 do
  W[t] := M[t];
for t := 16 to 79 do
  begin
  Temp64 := LittleSigma1_512_64( W[t-2]) +
            W[t-7] +
            LittleSigma0_512_64( W[t-15]) +
            W[t-16];
  W[t] := Temp64
  end
end;


procedure SHA256_Step3(
  var aa: TaToh_Array32;
  const W: TSHA256_W_Array);
var
  T1, T2: uint32;
  t: integer;
begin
for t := 0 to 63 do
  begin
  T1 := aa[h] + BigSigma1_256_32( aa[e]) + Ch32( aa[e], aa[f], aa[g]) + K_Values_32[t] + W[t];
  T2 := BigSigma0_256_32( aa[a]) + Maj32( aa[a], aa[b], aa[c]);
  aa[h] := aa[g];
  aa[g] := aa[f];
  aa[f] := aa[e];
  aa[e] := aa[d] + T1;
  aa[d] := aa[c];
  aa[c] := aa[b];
  aa[b] := aa[a];
  aa[a] := T1 + T2
  end
end;


procedure SHA512_Step3(
  var aa: TaToh_Array64;
  const W: TSHA512_W_Array);
var
  T1, T2: uint64;
  t: integer;
begin
for t := 0 to 79 do
  begin
  T1 := aa[h] + BigSigma1_512_64( aa[e]) + Ch64( aa[e], aa[f], aa[g]) + K_Values_64[t] + W[t];
  T2 := BigSigma0_512_64( aa[a]) + Maj64( aa[a], aa[b], aa[c]);
  aa[h] := aa[g];
  aa[g] := aa[f];
  aa[f] := aa[e];
  aa[e] := aa[d] + T1;
  aa[d] := aa[c];
  aa[c] := aa[b];
  aa[b] := aa[a];
  aa[a] := T1 + T2
  end
end;


procedure SHA256_Step4(
  var HH: TaToh_Array32;
  const aa: TaToh_Array32);
var
  Letter: TaToh;
begin
for Letter := a to h do
  HH[ Letter] := HH[ Letter] + aa[ Letter]
end;


procedure SHA512_Step4(
  var HH: TaToh_Array64;
  const aa: TaToh_Array64);
var
  Letter: TaToh;
begin
for Letter := a to h do
  HH[ Letter] := HH[ Letter] + aa[ Letter]
end;


procedure SHA256_Step2(
  var aa: TaToh_Array32;
  const HH: TaToh_Array32);
var
  Letter: TaToh;
begin
for Letter := a to h do
  aa[ Letter] := HH[ Letter]
end;


procedure SHA512_Step2(
  var aa: TaToh_Array64;
  const HH: TaToh_Array64);
var
  Letter: TaToh;
begin
for Letter := a to h do
  aa[ Letter] := HH[ Letter]
end;


///////////////////////////////////////
// TSHA-2 Class

function TSHA2.ControlObject: TObject;
begin
result := self
end;


constructor TSHA2.Create( Algorithm1: TSHA2FamilyMember);
begin
FAlgorithm := Algorithm1
end;



function TSHA2.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/drafts/' +
          'fips180-4/Draft-FIPS180-4_Feb2011.pdf'
end;

const DigestSizes: array[ TSHA2FamilyMember] of integer = (
// SHA_224, SHA_256, SHA_384, SHA_512, SHA_512_224, SHA_512_256
       224,     256,     384,     512,         224,         256);
function TSHA2.DigestSize: integer;
begin
result := DigestSizes[ FAlgorithm]
end;


function TSHA2.DisplayName: string;
const Names: array[ TSHA2FamilyMember] of string = (
// SHA_224, SHA_256, SHA_384, SHA_512, SHA_512_224, SHA_512_256
      '224',   '256',   '384',   '512',   '512/224',   '512/256');
begin
result := 'SHA-' + Names[ FAlgorithm]
end;


function TSHA2.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware];
if FAlgorithm in [SHA_512_224, SHA_512_256] then
  Include( result, afStar)
end;



function TSHA2.MakeHasher( const Params: IInterface): IHasher;
type
  TSHA2_BaseHasherClass = class of TSHA2_BaseHasher;
const
  Classes: array[ TSHA2FamilyMember] of TSHA2_BaseHasherClass = (
   TSHA224, TSHA256, TSHA384, TSHA512, TSHA512_224, TSHA512_256);
begin
result := Classes[ Falgorithm].Create( FAlgorithm)
end;


function TSHA2.ProgId: string;
const Names: array[ TSHA2FamilyMember] of string = (
// SHA_224, SHA_256, SHA_348, SHA_512, SHA_512_224, SHA_512_256
 SHA224_ProgId, SHA256_ProgId,
 SHA384_ProgId, SHA512_ProgId, SHA512_224_ProgId, SHA512_256_ProgId);
begin
result := Names[ FAlgorithm]
end;

function TSHA2.UpdateSize: integer;
const Sizes: array[ TSHA2FamilyMember] of integer = (
// SHA_224, SHA_256, SHA_384, SHA_512, SHA_512_224, SHA_512_256
       512,     512,    1024,    1024,        1024,        1024);
begin
result := Sizes[ FAlgorithm]
end;


function TSHA2.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +} 'Sha-2'
end;


{ TSHA2_BaseHasher }

constructor TSHA2_BaseHasher.Create( Algorithm1: TSHA2FamilyMember);
begin
FAlgorithm := Algorithm1;
FCount     := 0;
FisEnding  := False;
InitHashVectors
end;

{ TSHA256 }

procedure TSHA256.InitHashVectors;
begin
FH[a] := $6a09e667;
FH[b] := $bb67ae85;
FH[c] := $3c6ef372;
FH[d] := $a54ff53a;
FH[e] := $510e527f;
FH[f] := $9b05688c;
FH[g] := $1f83d9ab;
FH[h] := $5be0cd19
end;



procedure TSHA256.Update( Source: TMemoryStream);
var
  M: TSHA256_M_Array;
  W: TSHA256_W_Array;
  aa: TaToh_Array32;
  t: integer;
  Temp32: uint32;
begin
Assert( Source.Size = 64, 'TSHA256.Update - Wrong block size.');
if not FisEnding then
  Inc( FCount, 512);
Source.Position := 0;
for t := 0 to 15 do
  begin
  Source.Read( Temp32, 4);
  M[t] := SwapEndien_u32( Temp32)
  end;
SHA256_Step1( M, W);
SHA256_Step2( aa, FH);
SHA256_Step3( aa, W);
SHA256_Step4( FH, aa)
end;


procedure TSHA256.End_Hash( PartBlock: TMemoryStream; Digest: TStream);
var
  MessageLengthInBits, L: uint64;
  Pad: packed array[ 0..71 ] of byte;
  PaddingCountInBytes: integer;
  PadOrigin, Xfer: integer;
  jj: TaToh;
  lwDigest: uint32;
begin
L := PartBlock.Position;
Assert( L <= 64, 'TSHA256.End_Hash - Wrong block size.');
FisEnding  := True;
Inc( FCount, L * 8);
MessageLengthInBits := FCount;
PaddingCountInBytes := SHA_StylePadding_64_in_512( MessageLengthInBits, Pad);
PadOrigin := 0;
Xfer := 64 - L;
if Xfer < 0 then
  Xfer := 0;
if Xfer > PaddingCountInBytes then
  Xfer := PaddingCountInBytes;
if Xfer > 0 then
  begin
  PartBlock.Write( Pad, Xfer);
  Inc( PadOrigin, Xfer);
  Dec( PaddingCountInBytes, Xfer)
  end;
Update( PartBlock);
Xfer := PaddingCountInBytes;
if Xfer > 0 then
  begin
  PartBlock.Position := 0;
  PartBlock.Write( Pad[ PadOrigin], Xfer);
  Assert( Xfer = 64, 'TSHA256.End_Hash - Wrong block size.');
  Update( PartBlock)
  end;
PartBlock.Position := L;
L := DigestSizes[ FAlgorithm] div 8; // digest size in bytes.
Digest.Position := 0;
for jj := a to h do
  begin
  if L <= 0 then break;
  lwDigest := SwapEndien_u32( FH[jj]);
  Xfer     := 4;
  if Xfer > L then
    Xfer := L;
  if Xfer <= 0 then break;
  Digest.WriteBuffer( lwDigest, Xfer);
  Dec( L, Xfer)
  end;
Digest.Position := 0;
FillChar( FH, SizeOf( FH), 0)
end;


procedure TSHA256.Burn;
begin
FCount := 0;
FillChar( FH, SizeOf( FH), 0);
FisEnding := False
end;

function TSHA256.SelfTest_Source: ansistring;
begin
result := 'abc'
end;


function TSHA256.SelfTest_ReferenceHashValue: ansistring;
begin
result := 'BA7816BF 8F01CFEA ' +
          '414140DE 5DAE2223 B00361A3 96177A9C B410FF61 F20015AD'
end;


{ TSHA224 }

procedure TSHA224.InitHashVectors;
begin
FH[a] := $c1059ed8;
FH[b] := $367cd507;
FH[c] := $3070dd17;
FH[d] := $f70e5939;
FH[e] := $ffc00b31;
FH[f] := $68581511;
FH[g] := $64f98fa7;
FH[h] := $befa4fa4
end;


function TSHA224.SelfTest_Source: ansistring;
begin
result := 'abc'
// Two block alternative:
// result := 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'
end;


function TSHA224.SelfTest_ReferenceHashValue: ansistring;
begin
result := '23097D22 3405D822 8642A477 BDA255B3 2AADBCE4 BDA0B3F7 E36C9DA7'
// Two block alternative:
// result := '75388B16 512776CC 5DBA5DA1 FD890150 B0C6455C B4F58B19 52522525'
end;




{ TSHA512 }

procedure TSHA512.InitHashVectors;
begin
FH[a] := $6a09e667f3bcc908;
FH[b] := $bb67ae8584caa73b;
FH[c] := $3c6ef372fe94f82b;
FH[d] := $a54ff53a5f1d36f1;
FH[e] := $510e527fade682d1;
FH[f] := $9b05688c2b3e6c1f;
FH[g] := $1f83d9abfb41bd6b;
FH[h] := $5be0cd19137e2179
end;



procedure TSHA512.Update( Source: TMemoryStream);
var
  M: TSHA512_M_Array;
  W: TSHA512_W_Array;
  aa: TaToh_Array64;
  t: integer;
  Temp64: uint64;
begin
Assert( Source.Size = 128, 'TSHA512.Update - Wrong block size.');
if not FisEnding then
  Inc( FCount, 1024);
Source.Position := 0;
for t := 0 to 15 do
  begin
  Source.Read( Temp64, 8);
  M[t] := SwapEndien_u64( Temp64)
  end;
SHA512_Step1( M, W);
SHA512_Step2( aa, FH);
SHA512_Step3( aa, W);
SHA512_Step4( FH, aa)
end;


procedure TSHA512.End_Hash( PartBlock: TMemoryStream; Digest: TStream);
var
  Lo64MessageLengthInBits, Hi64MessageLengthInBits, L: uint64;
  Pad: packed array[ 0..255 ] of byte;
  PaddingCountInBytes: integer;
  PadOrigin, Xfer: integer;
  jj: TaToh;
  lwDigest: uint64;
begin
L := PartBlock.Position;
Assert( L <= 128, 'TSHA512.End_Hash - Wrong block size.');
FisEnding := True;
Inc( FCount, L * 8);
Lo64MessageLengthInBits := FCount;
Hi64MessageLengthInBits := 0;
PaddingCountInBytes := SHA_StylePadding_128_in_1024(
  Lo64MessageLengthInBits, Hi64MessageLengthInBits, Pad);
PadOrigin := 0;
Xfer := 128 - L;
if Xfer < 0 then
  Xfer := 0;
if Xfer > PaddingCountInBytes then
  Xfer := PaddingCountInBytes;
if Xfer > 0 then
  begin
  PartBlock.Write( Pad, Xfer);
  Inc( PadOrigin, Xfer);
  Dec( PaddingCountInBytes, Xfer)
  end;
Update( PartBlock);
Xfer := PaddingCountInBytes;
if Xfer > 0 then
  begin
  PartBlock.Position := 0;
  PartBlock.Write( Pad[ PadOrigin], Xfer);
  Assert( Xfer = 128, 'TSHA512.End_Hash - Wrong block size.');
  Update( PartBlock)
  end;
PartBlock.Position := L;
L := DigestSizes[ FAlgorithm] div 8; // digest size in bytes.
Digest.Position := 0;
for jj := a to h do
  begin
  if L <= 0 then break;
  lwDigest := SwapEndien_u64( FH[jj]);
  Xfer     := 8;
  if Xfer > L then
    Xfer := L;
  if Xfer <= 0 then break;
  Digest.WriteBuffer( lwDigest, Xfer);
  Dec( L, Xfer)
  end;
Digest.Position := 0;
FillChar( FH, SizeOf( FH), 0)
end;


procedure TSHA512.Burn;
begin
FCount := 0;
FillChar( FH, SizeOf( FH), 0);
FisEnding := False
end;

function TSHA512.SelfTest_Source: ansistring;
begin
result := 'abc'
end;


function TSHA512.SelfTest_ReferenceHashValue: ansistring;
begin
result := 'DDAF35A1 93617ABA CC417349 AE204131 ' +
          '12E6FA4E 89A97EA2 0A9EEEE6 4B55D39A 2192992A 274FC1A8 ' +
          '36BA3C23 A3FEEBBD 454D4423 643CE80E 2A9AC94F A54CA49F'
end;



{ TSHA348 }

procedure TSHA384.InitHashVectors;
begin
FH[a] := $cbbb9d5dc1059ed8;
FH[b] := $629a292a367cd507;
FH[c] := $9159015a3070dd17;
FH[d] := $152fecd8f70e5939;
FH[e] := $67332667ffc00b31;
FH[f] := $8eb44a8768581511;
FH[g] := $db0c2e0d64f98fa7;
FH[h] := $47b5481dbefa4fa4
end;


function TSHA384.SelfTest_Source: ansistring;
begin
result := 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijk' +
 'lmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'

end;


function TSHA384.SelfTest_ReferenceHashValue: ansistring;
begin
result := '09330C33 F71147E8 3D192FC7 82CD1B47 53111B17 3B3B05D2 ' +
          '2FA08086 E3B0F712 FCC7C71A 557E2DB9 66C3E9FA 91746039'
end;


{ TSHA512_224 }

procedure TSHA512_224.InitHashVectors;
begin
FH[a] := $8C3D37C819544DA2;
FH[b] := $73E1996689DCD4D6;
FH[c] := $1DFAB7AE32FF9C82;
FH[d] := $679DD514582F9FCF;
FH[e] := $0F6D2B697BD44DA8;
FH[f] := $77E36F7304C48942;
FH[g] := $3F9D85A86A1D36C8;
FH[h] := $1112E6AD91D692A1
end;



function TSHA512_224.SelfTest_Source: ansistring;
begin
result := 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhi' +
 'jklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
end;

function TSHA512_224.SelfTest_ReferenceHashValue: ansistring;
begin
result := '23FEC5BB 94D60B23 30819264 0B0C4533 35D66473 4FE40E72 68674AF9'
end;



{ TSHA512_256 }

procedure TSHA512_256.InitHashVectors;
begin
FH[a] := $22312194FC2BF72C;
FH[b] := $9F555FA3C84C64C2;
FH[c] := $2393B86B6F53B151;
FH[d] := $963877195940EABD;
FH[e] := $96283EE2A88EFFE3;
FH[f] := $BE5E1E2553863992;
FH[g] := $2B0199FC2C85B8AA;
FH[h] := $0EB72DDC81C52CA2
end;



function TSHA512_256.SelfTest_Source: ansistring;
begin
result := 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijkl' +
  'mnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
end;

function TSHA512_256.SelfTest_ReferenceHashValue: ansistring;
begin
result := '3928E184 FB8690F8 40DA3988 121D31BE 65CB9D3E F83EE614 ' +
          '6FEAC861 E19B563A'
end;


end.
