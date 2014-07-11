(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower LockBox
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s): Sebastian Zierer
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{*                  LBCIPHER.PAS 2.08                    *}
{*     Copyright (c) 2002 TurboPower Software Co         *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I LockBox.inc}

unit LbCipher;
  {-private key encryption/decryption primitives}

interface

uses
  Types, SysUtils, Classes;

const
  { largest structure that can be created }
  MaxStructSize = 1024 * 2000000; {2G}


{ TLbBase - used to force this unit to be added to uses clause }
type
  TLBBase = class(TComponent)
  end;

{$IFDEF LINUX}
  pDword = ^dword;
{$ENDIF}


{ general structures }
type
  pLongIntArray = ^TLongIntArray;
  TLongIntArray = array [0..MaxStructSize div SizeOf(LongInt) - 1] of LongInt;

  TLongIntRec = packed record
    case Byte of
      1: (Lo: Word;
          Hi: Word);
      2: (LoLo: Byte;
          LoHi: Byte;
          HiLo: Byte;
          HiHi: Byte);
  end;

  TInt64 = packed record
    case Byte of
      0: (Lo: LongInt;
          Hi: LongInt);
      1: (LoLo: Word;
          LoHi: Word;
          HiLo: Word;
          HiHi: Word);
      2: (LoLoLo: Byte;
          LoLoHi: Byte;
          LoHiLo: Byte;
          LoHiHi: Byte;
          HiLoLo: Byte;
          HiLoHi: Byte;
          HiHiLo: Byte;
          HiHiHi: Byte);
  end;


{ encryption key types }
type
  PKey64  = ^TKey64;                                                 {!!.03}
  TKey64  = array [0..7] of Byte;

  PKey128 = ^TKey128;                                                {!!.03}
  TKey128 = array [0..15] of Byte;

  PKey192 = ^TKey192;                                                {!!.03}
  TKey192 = array [0..23] of Byte;

  PKey256 = ^TKey256;                                                {!!.03}
  TKey256 = array [0..31] of Byte;


{ encryption block types }
type
  PLBCBlock  = ^TLBCBlock;
  TLBCBlock  = array[0..3] of LongInt;     { LockBox Cipher }
  TLQCBlock  = array[0..1] of LongInt;     { LockBox Quick Cipher }
  TBFBlock   = array[0..1] of LongInt;     { BlowFish }


{ context type constants }
const
  BFRounds = 16;      { 16 blowfish rounds }


{ block cipher context types }
type
  { Blowfish }
  TBFContext = packed record
    PBox    : array[0..(BFRounds+1)] of LongInt;
    SBox    : array[0..3, 0..255] of LongInt;
  end;

  { LockBox Cipher }
  TLBCContext = packed record
    Encrypt : Boolean;
    Dummy   : array[0..2] of Byte; {filler}
    Rounds  : LongInt;
    case Byte of
      0: (SubKeys64   : array [0..15] of TKey64);
      1: (SubKeysInts : array [0..3, 0..7] of LongInt);
  end;


{ stream cipher context types }
type
  { LockBox stream cipher }
  TLSCContext = packed record
    Index       : LongInt;
    Accumulator : LongInt;
    SBox        : array [0..255] of Byte;
  end;

  { random number stream ciphers }
  TRNG32Context = array [0..3] of Byte;
  TRNG64Context = array [0..7] of Byte;


{ message digest context types }
type
  TLMDContext  = array [0..279] of Byte;       { LockBox message digest }

{ Blowfish Cipher }
procedure InitEncryptBF(Key : TKey128;
            var Context : TBFContext);
procedure EncryptBF(const Context : TBFContext;
            var Block : TBFBlock; Encrypt : Boolean);
procedure EncryptBFCBC(const Context : TBFContext;
            const Prev : TBFBlock; var Block : TBFBlock; Encrypt : Boolean);

{ LockBox Cipher }
procedure InitEncryptLBC(const Key : TKey128;
            var Context : TLBCContext; Rounds : LongInt; Encrypt : Boolean); 
procedure EncryptLBC(const Context : TLBCContext;
            var Block : TLBCBlock); 
{ LockBox message digest }
procedure InitLMD(var Context : TLMDContext); 
procedure HashLMD(var Digest; DigestSize : LongInt;
            const Buf; BufSize : LongInt); 
procedure FinalizeLMD(var Context : TLMDContext;
            var Digest; DigestSize : LongInt); 
procedure UpdateLMD(var Context : TLMDContext;
            const Buf; BufSize : LongInt); 

procedure GenerateRandomKey(var Key; KeySize : Integer);

procedure GenerateLMDKey(var Key; KeySize : Integer; const ABytes: TBytes);

procedure XorMem(var Mem1;  const Mem2;  Count : Cardinal);

{ Miscellaneous hash algorithms }
procedure HashELF(var Digest : LongInt; const Buf;  BufSize : LongInt);
procedure StringHashELF(var Digest: LongInt; const Str: string; AEncoding: TEncoding);


implementation

uses
  LbUtils;


{first 2048 bits of Pi in hexadecimal, low to high, without the leading "3"}
const
  Pi2048: array [0..255] of Byte = (
    $24, $3F, $6A, $88, $85, $A3, $08, $D3, $13, $19, $8A, $2E, $03, $70, $73, $44,
    $A4, $09, $38, $22, $29, $9F, $31, $D0, $08, $2E, $FA, $98, $EC, $4E, $6C, $89,
    $45, $28, $21, $E6, $38, $D0, $13, $77, $BE, $54, $66, $CF, $34, $E9, $0C, $6C,
    $C0, $AC, $29, $B7, $C9, $7C, $50, $DD, $3F, $84, $D5, $B5, $B5, $47, $09, $17,
    $92, $16, $D5, $D9, $89, $79, $FB, $1B, $D1, $31, $0B, $A6, $98, $DF, $B5, $AC,
    $2F, $FD, $72, $DB, $D0, $1A, $DF, $B7, $B8, $E1, $AF, $ED, $6A, $26, $7E, $96,
    $BA, $7C, $90, $45, $F1, $2C, $7F, $99, $24, $A1, $99, $47, $B3, $91, $6C, $F7,
    $08, $01, $F2, $E2, $85, $8E, $FC, $16, $63, $69, $20, $D8, $71, $57, $4E, $69,
    $A4, $58, $FE, $A3, $F4, $93, $3D, $7E, $0D, $95, $74, $8F, $72, $8E, $B6, $58,
    $71, $8B, $CD, $58, $82, $15, $4A, $EE, $7B, $54, $A4, $1D, $C2, $5A, $59, $B5,
    $9C, $30, $D5, $39, $2A, $F2, $60, $13, $C5, $D1, $B0, $23, $28, $60, $85, $F0,
    $CA, $41, $79, $18, $B8, $DB, $38, $EF, $8E, $79, $DC, $B0, $60, $3A, $18, $0E,
    $6C, $9E, $0E, $8B, $B0, $1E, $8A, $3E, $D7, $15, $77, $C1, $BD, $31, $4B, $27,
    $78, $AF, $2F, $DA, $55, $60, $5C, $60, $E6, $55, $25, $F3, $AA, $55, $AB, $94,
    $57, $48, $98, $62, $63, $E8, $14, $40, $55, $CA, $39, $6A, $2A, $AB, $10, $B6,
    $B4, $CC, $5C, $34, $11, $41, $E8, $CE, $A1, $54, $86, $AF, $7C, $72, $E9, $93);

type
  TLMDContextEx = packed record
    DigestIndex : LongInt;
    Digest      : array [0..255] of Byte;
    KeyIndex    : LongInt;
    case Byte of
      0: (KeyInts : array [0..3] of LongInt);
      1: (Key     : TKey128);
  end;
  TBlock2048 = array [0..255] of Byte;

type
  {bit mixing types}
  T128Bit     = array [0..3] of DWord;
  T256Bit     = array [0..7] of DWord;                                 

const
  BCSalts: array [0..3] of DWord =                                     
    ($55555555, $AAAAAAAA, $33333333, $CCCCCCCC);

type
  TBCHalfBlock = array [0..1] of LongInt;

  TBFBlockEx = packed record
    Xl : array[0..3] of Byte;
    Xr : array[0..3] of Byte;
  end;

{ Blowfish tables }
{$I LbBF.inc }                                                       {!!.01}

{ ========================================================================== }
procedure EncryptLBC(const Context : TLBCContext; var Block : TLBCBlock);
var
  Blocks    : array[0..1] of TBCHalfBlock;                           {!!.01}
  Work      : TBCHalfBlock;
  Right     : TBCHalfBlock;
  Left      : TBCHalfBlock;
  AA, BB    : LongInt;
  CC, DD    : LongInt;
  R, T      : LongInt;
begin
  Move(Block, Blocks, SizeOf(Blocks));                               {!!.01}
  Right := Blocks[0];
  Left := Blocks[1];

  for R := 0 to Context.Rounds - 1 do begin
    {transform the right side}
    AA := Right[0];
    BB := TBCHalfBlock(Context.SubKeys64[R])[0];
    CC := Right[1];
    DD := TBCHalfBlock(Context.SubKeys64[R])[1];

    {mix it once...}
    AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 7);
    BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 13);
    CC := CC + BB; BB := BB + CC; CC := CC xor (CC shr 17);
    DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 9);
    AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 3);
    BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 7);
    CC := CC + BB; BB := BB + CC; CC := CC xor (DD shr 15);
    DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 11);

    {swap sets...}
    T := AA; AA := CC; CC := T;
    T := BB; BB := DD; DD := T;

    {mix it twice}
    AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 7);
    BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 13);
    CC := CC + BB; BB := BB + CC; CC := CC xor (CC shr 17);
    DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 9);
    AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 3);
    BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 7);
    CC := CC + BB; BB := BB + CC; CC := CC xor (DD shr 15);
    DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 11);

    Work[0] := Left[0] xor AA xor BB;
    Work[1] := Left[1] xor CC xor DD;

    Left := Right;
    Right := Work;
  end;

  Blocks[0] := Left;
  Blocks[1] := Right;
  Move(Blocks, Block, SizeOf(Block));                                {!!.01}
end;
{ -------------------------------------------------------------------------- }
procedure InitEncryptLBC(const Key : TKey128; var Context : TLBCContext; Rounds: LongInt; Encrypt : Boolean);
type
  TSubKeys = packed record
    case Byte of
      0: (SubKeys64 : array [0..15] of TKey64);
      1: (SubKeysInts : array [0..3, 0..7] of LongInt);
  end;
var
  KeyArray  : pLongIntArray;
  AA, BB    : LongInt;
  CC, DD    : LongInt;
  EE, FF    : LongInt;
  GG, HH    : LongInt;
  I, R      : LongInt;
  Temp      : TSubKeys;
begin
  KeyArray := @Key;
  Context.Encrypt := Encrypt;
  Context.Rounds := Max(4, Min(16, Rounds));


  {fill subkeys by propagating seed}
  for I := 0 to 3 do begin
    {interleave the key with the salt}

    AA := KeyArray^[0]; BB := BCSalts[I];
    CC := KeyArray^[1]; DD := BCSalts[I];
    EE := KeyArray^[2]; FF := BCSalts[I];
    GG := KeyArray^[3]; HH := BCSalts[I];

    {mix all the bits around for 8 rounds}
    {achieves avalanche and eliminates funnels}
    for R := 0 to 7 do begin
      AA := AA xor (BB shl 11); DD := DD + AA; BB := BB + CC;
      BB := BB xor (CC shr 2);  EE := EE + BB; CC := CC + DD;
      CC := CC xor (DD shl 8);  FF := FF + CC; DD := DD + EE;
      DD := DD xor (EE shr 16); GG := GG + DD; EE := EE + FF;
      EE := EE xor (FF shl 10); HH := HH + EE; FF := FF + GG;
      FF := FF xor (GG shr 4);  AA := AA + FF; GG := GG + HH;
      GG := GG xor (HH shl 8);  BB := BB + GG; HH := HH + AA;
      HH := HH xor (AA shr 9);  CC := CC + HH; AA := AA + BB;
    end;

    {assign value to subkey}
    Context.SubKeysInts[I, 0] := AA;
    Context.SubKeysInts[I, 1] := BB;
    Context.SubKeysInts[I, 2] := CC;
    Context.SubKeysInts[I, 3] := DD;
    Context.SubKeysInts[I, 4] := EE;
    Context.SubKeysInts[I, 5] := FF;
    Context.SubKeysInts[I, 6] := GG;
    Context.SubKeysInts[I, 7] := HH;
  end;

  {reverse subkeys if decrypting - easier for EncryptLBC routine}
  if not Encrypt then begin
    for I := 0 to Context.Rounds - 1 do
        Temp.SubKeys64[(Context.Rounds - 1) - I] := Context.SubKeys64[I];
    for I := 0 to Context.Rounds - 1 do
        Context.SubKeys64[I] := Temp.SubKeys64[I];
  end;
end;
{ -------------------------------------------------------------------------- }
procedure InitEncryptBF(Key : TKey128; var Context : TBFContext);
var
  I     : Integer;
  J     : Integer;
  K     : Integer;
  Data  : LongInt;
  Block : TBFBlock;
begin
  {initialize PArray}
  Move(bf_P, Context.PBox, SizeOf(Context.PBox));
  {initialize SBox}
  Move(bf_S, Context.SBox, SizeOf(Context.SBox));

  {update PArray with the key bits}
  J := 0;
  for I := 0 to (BFRounds+1) do begin
    Data := 0;
    for K := 0 to 3 do begin
      Data := (Data shl 8) or Key[J];
      Inc(J);
      if J >= SizeOf(Key) then
        J := 0;
    end;
    Context.PBox[I] := Context.PBox[I] xor Data;
  end;

  {encrypt an all-zero string using the Blowfish algorithm and}
  {replace the elements of the P-array with the output of this process}

  Block[0] := 0;
  Block[1] := 0;
  I := 0;
  repeat
    EncryptBF(Context, Block, True);
    Context.PBox[I] := Block[0];
    Context.PBox[I+1] := Block[1];
    Inc(I, 2);
  until I > BFRounds+1;

  {continue the process, replacing the elements of the four S-boxes in}
  {order, with the output of the continuously changing Blowfish algorithm}

  for J := 0 to 3 do begin
    I := 0;
    repeat
      EncryptBF(Context, Block, True);
      Context.SBox[J, I] := Block[0];
      Context.SBox[J, I+1] := Block[1];
      Inc(I, 2);
    until I > 255;
  end;

  {in total, 521 iterations are required to generate all required subkeys. }
end;
{ -------------------------------------------------------------------------- }
procedure EncryptBF(const Context : TBFContext;
  var Block : TBFBlock; Encrypt : Boolean);
var
  I : Integer;
  TmpBlock : TBFBlockEx;                                             {!!.01}
begin
  Move(Block, TmpBlock, SizeOf(TmpBlock));                           {!!.01}
  if Encrypt then begin
    Block[0] := Block[0] xor Context.PBox[0];

    {16 Rounds to go (8 double rounds to avoid swaps)}
    I := 1;
    repeat
      {first half round }
      Block[1] := Block[1] xor Context.PBox[I] xor (((
                  Context.SBox[0, TmpBlock.Xl[3]] + Context.SBox[1, TmpBlock.Xl[2]])
                  xor Context.SBox[2, TmpBlock.Xl[1]]) + Context.SBox[3, TmpBlock.Xl[0]]);
      {second half round }
      Block[0] := Block[0] xor Context.PBox[I+1] xor (((
                  Context.SBox[0, TmpBlock.Xr[3]] + Context.SBox[1, TmpBlock.Xr[2]])
                  xor Context.SBox[2, TmpBlock.Xr[1]]) + Context.SBox[3, TmpBlock.Xr[0]]);
      Inc(I, 2);
    until I > BFRounds;
    Block[1] := Block[1] xor Context.PBox[(BFRounds+1)];
  end else begin
    Block[1] := Block[1] xor Context.PBox[(BFRounds+1)];

    {16 Rounds to go (8 double rounds to avoid swaps)}
    I := BFRounds;
    repeat
      {first half round }
      Block[0] := Block[0] xor Context.PBox[I] xor (((
                  Context.SBox[0, TmpBlock.Xr[3]] + Context.SBox[1, TmpBlock.Xr[2]])
                  xor Context.SBox[2, TmpBlock.Xr[1]]) + Context.SBox[3, TmpBlock.Xr[0]]);
      {second half round }
      Block[1] := Block[1] xor Context.PBox[i-1] xor (((
                  Context.SBox[0, TmpBlock.Xl[3]] + Context.SBox[1, TmpBlock.Xl[2]])
                  xor Context.SBox[2, TmpBlock.Xl[1]]) + Context.SBox[3, TmpBlock.Xl[0]]);
       Dec (I, 2);
     until I < 1;
     Block[0] := Block[0] xor Context.PBox[0];
  end;
end;
{ -------------------------------------------------------------------------- }
procedure EncryptBFCBC(const Context : TBFContext; const Prev : TBFBlock;
  var Block : TBFBlock; Encrypt : Boolean);
begin
  if Encrypt then begin
    XorMem(Block, Prev, SizeOf(Block));
    EncryptBF(Context, Block, Encrypt);
  end else begin
    EncryptBF(Context, Block, Encrypt);
    XorMem(Block, Prev, SizeOf(Block));
  end;
end;
{ -------------------------------------------------------------------------- }
procedure GenerateRandomKey(var Key; KeySize : Integer);
var
  I     : Integer;
begin
  Randomize;
  for I := 0 to KeySize - 1 do
    TByteArray(Key)[I] := System.Random(256);                        {!!.01}
end;

procedure GenerateLMDKey(var Key; KeySize : Integer; const ABytes: TBytes);
begin
  HashLMD(Key, KeySize, ABytes[0], Length(ABytes));
end;

{ -------------------------------------------------------------------------- }
procedure Mix128(var X : T128Bit);
var
  AA, BB, CC, DD : LongInt;
begin
  AA := X[0];  BB := X[1];  CC := X[2];  DD := X[3];

  AA := AA + DD;  DD := DD + AA;  AA := AA xor (AA shr 7);
  BB := BB + AA;  AA := AA + BB;  BB := BB xor (BB shl 13);
  CC := CC + BB;  BB := BB + CC;  CC := CC xor (CC shr 17);
  DD := DD + CC;  CC := CC + DD;  DD := DD xor (DD shl 9);
  AA := AA + DD;  DD := DD + AA;  AA := AA xor (AA shr 3);
  BB := BB + AA;  AA := AA + BB;  BB := BB xor (BB shl 7);
  CC := CC + BB;  BB := BB + CC;  CC := CC xor (DD shr 15);
  DD := DD + CC;  CC := CC + DD;  DD := DD xor (DD shl 11);

  X[0] := AA;  X[1] := BB;  X[2] := CC;  X[3] := DD;
end;
{ -------------------------------------------------------------------------- }
procedure InitLMD(var Context : TLMDContext);
var
  ContextEx : TLMDContextEx;
begin
  Move(Context, ContextEx, SizeOf(ContextEx));                       {!!.01}
  ContextEx.DigestIndex := 0;
  TBlock2048(ContextEx.Digest) := TBlock2048(Pi2048);

  ContextEx.KeyIndex := 0;
  ContextEx.KeyInts[0] := $55555555;
  ContextEx.KeyInts[1] := $55555555;
  ContextEx.KeyInts[2] := $55555555;
  ContextEx.KeyInts[3] := $55555555;
  Move(ContextEx, Context, SizeOf(Context));                         {!!.01}
end;

{ -------------------------------------------------------------------------- }
procedure UpdateLMD(var Context : TLMDContext; const Buf; BufSize : LongInt);
var
  ContextEx : TLMDContextEx;                                         {!!.01}
  AA, BB    : LongInt;
  CC, DD    : LongInt;
  I, R      : LongInt;
begin
  Move(Context, ContextEx, SizeOf(ContextEx));                       {!!.01}
  for I := 0 to BufSize - 1 do
    with ContextEx do begin
      {update Digest}
      Digest[DigestIndex] := Digest[DigestIndex] xor
                               TByteArray(Buf)[I];                   {!!.01}
      DigestIndex := DigestIndex + 1;
      if (DigestIndex = SizeOf(Digest)) then
        DigestIndex := 0;

      {update BlockKey}
      Key[KeyIndex] := Key[KeyIndex] xor TByteArray(Buf)[I];         {!!.01}
      KeyIndex := KeyIndex + 1;
      if (KeyIndex = SizeOf(Key) div 2) then begin
        AA := KeyInts[3];
        BB := KeyInts[2];
        CC := KeyInts[1];
        DD := KeyInts[0];

        {mix all the bits around for 4 rounds}
        {achieves avalanche and eliminates funnels}
        for R := 0 to 3 do begin
          AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 7);
          BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 13);
          CC := CC + BB; BB := BB + CC; CC := CC xor (CC shr 17);
          DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 9);
          AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 3);
          BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 7);
          CC := CC + BB; BB := BB + CC; CC := CC xor (DD shr 15);
          DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 11);
        end;

        KeyInts[0] := AA;
        KeyInts[1] := BB;
        KeyInts[2] := CC;
        KeyInts[3] := DD;

        KeyIndex := 0;
      end;
    end;
  Move(ContextEx, Context, SizeOf(Context));                         {!!.01}
end;
{ -------------------------------------------------------------------------- }
procedure FinalizeLMD(var Context : TLMDContext; var Digest; DigestSize : LongInt);
const
  Padding : array [0..7] of Byte = (1, 0, 0, 0, 0, 0, 0, 0);
var
  ContextEx : TLMDContextEx;                                         {!!.01}
  BCContext : TLBCContext;
  I         : Integer;
begin
 {pad with "1", followed by as many "0"s as needed to fill the block}
  Move(Context, ContextEx, SizeOf(ContextEx));                       {!!.01}
  UpdateLMD(Context, Padding, SizeOf(Padding) - ContextEx.KeyIndex);
  Move(Context, ContextEx, SizeOf(ContextEx));                       {!!.01}

  {mix context using block cipher}
  InitEncryptLBC(ContextEx.Key, BCContext, 8, True);
  for I := 0 to (SizeOf(ContextEx.Digest) div SizeOf(TLBCBlock)) - 1 do
    EncryptLBC(BCContext, PLBCBlock(@ContextEx.Digest[I * SizeOf(TLBCBlock)])^);

  {return Digest of requested DigestSize}
  {max digest is 2048-bit, although it could be greater if Pi2048 was larger}
  Move(ContextEx.Digest, Digest, Min(SizeOf(ContextEx.Digest), DigestSize));
end;
{ -------------------------------------------------------------------------- }
procedure HashLMD(var Digest; DigestSize : LongInt; const Buf; BufSize : LongInt);
var
  Context : TLMDContext;
begin
  InitLMD(Context);
  UpdateLMD(Context, Buf, BufSize);
  FinalizeLMD(Context, Digest, DigestSize);
end;
{ -------------------------------------------------------------------------- }
procedure HashMix128(var Digest : LongInt; const Buf;  BufSize : LongInt);
type
  T128BitArray = array[0..0] of T128Bit;
var
  Temp      : T128Bit;
  PTemp     : PByteArray;
  I, L   : LongInt;
begin
  Temp[0] := $243F6A88;  {first 16 bytes of Pi in binary}
  Temp[1] := $93F40317;
  Temp[2] := $0C110496;
  Temp[3] := $C709C289;

  L := BufSize div SizeOf(T128Bit);
  for I := 0 to L - 1 do begin
    Temp[0] := Temp[0] + T128BitArray(Buf)[I][0];                    {!!.01}
    Temp[1] := Temp[1] + T128BitArray(Buf)[I][1];                    {!!.01}
    Temp[2] := Temp[2] + T128BitArray(Buf)[I][2];                    {!!.01}
    Temp[3] := Temp[3] + T128BitArray(Buf)[I][3];                    {!!.01}
    Mix128(Temp);
  end;

  PTemp := @Temp;
  if (BufSize > L * SizeOf(T128Bit)) then begin
    for I := 0 to (BufSize - L * SizeOf(T128Bit)) - 1 do
      PTemp^[I] := PTemp^[I] + TByteArray(Buf)[(L * SizeOf(T128Bit)) + I]; {!!.01}
    Mix128(Temp);
  end;

  Digest := Temp[3];
end;
{ -------------------------------------------------------------------------- }
procedure XorMem(var Mem1;  const Mem2;  Count : Cardinal);
var
  i: integer;
  M1: TByteArray absolute Mem1;
  M2: TByteArray absolute Mem2;
begin
  for i := 0 to Count-1 do
     M1[i] := M1[i] xor M2[i]
end;

{ -------------------------------------------------------------------------- }
procedure HashELF(var Digest : LongInt; const Buf;  BufSize : LongInt);
var
  I, X  : LongInt;
begin
  Digest := 0;
  for I := 0 to BufSize - 1 do begin
    Digest := (Digest shl 4) + TByteArray(Buf)[I];                   {!!.01}
    X := Digest and $F0000000;
    if (X <> 0) then
      Digest := Digest xor (X shr 24);
    Digest := Digest and (not X);
  end;
end;

procedure StringHashELF(var Digest: LongInt; const Str: string; AEncoding: TEncoding);
var
  pBytes: TBytes;
begin
  pBytes := AEncoding.GetBytes(Str);
  HashELF(Digest, pBytes[0], Length(pBytes));
end;

end.
