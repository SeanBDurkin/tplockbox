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
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{*                   LBPROC.PAS 2.08                     *}
{*     Copyright (c) 2002 TurboPower Software Co         *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I LockBox.inc}

unit LbProc;
  {-stream and file routines for block and stream ciphers}

interface

uses
  Classes,
  SysUtils,
  LbCipher;

type
  ECipherException = class(Exception);

type                                                                            {!!.06a}
  TProgressProc = procedure(CurPostion, TotalSize: longint);                    {!!.06a}

var                                                                             {!!.06a}
  LbOnProgress : TProgressProc;                                                 {!!.06a}
  LbProgressSize: Longint;                                                      {!!.06a}

{ high level encryption routines }
procedure BFEncryptFile(const InFile, OutFile : string;
            const Key : TKey128; Encrypt : Boolean); 
procedure BFEncryptFileCBC(const InFile, OutFile : string;
            const Key : TKey128; Encrypt : Boolean); 
procedure BFEncryptStream(InStream, OutStream : TStream;
            const Key : TKey128; Encrypt : Boolean); 
procedure BFEncryptStreamCBC(InStream, OutStream : TStream;
            const Key : TKey128; Encrypt : Boolean);

implementation

const
  SInvalidFileFormat = 'Invalid file format';

{ == Blowfish ============================================================== }
procedure BFEncryptFile(const InFile, OutFile : string;
            const Key : TKey128; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      BFEncryptStream(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure BFEncryptFileCBC(const InFile, OutFile : string;
            const Key : TKey128; Encrypt : Boolean);
var
  InStream, OutStream : TStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      BFEncryptStreamCBC(InStream, OutStream, Key, Encrypt);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure BFEncryptStream(InStream, OutStream : TStream;
            const Key : TKey128; Encrypt : Boolean);
var
  I          : LongInt;
  Block      : TBFBlock;
  Context    : TBFContext;
  BlockCount : LongInt;
begin
  InitEncryptBF(Key, Context);

  {get the number of blocks in the file}
  BlockCount := (InStream.Size div SizeOf(Block));

  {when encrypting, make sure we have a block with at least one free}
  {byte at the end. used to store the odd byte count value}
  if Encrypt then
    Inc(BlockCount);

  {process all except the last block}
  for I := 1 to BlockCount - 1 do begin
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise ECipherException.Create(SInvalidFileFormat);
    EncryptBF(Context, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));

    if Assigned(LbOnProgress) then                                              {!!.06a}
      if InStream.Position mod LbProgressSize = 0 then                          {!!.06a}
        LbOnProgress (InStream.Position, InStream.Size);                        {!!.06a}
  end;

  if Encrypt then begin
    FillChar(Block, SizeOf(Block), #0);

    {set actual number of bytes to read}
    I := (InStream.Size mod SizeOf(Block));
    if InStream.Read(Block, I) <> I then
      raise ECipherException.Create(SInvalidFileFormat);

    {store number of bytes as last byte in last block}
    PByteArray(@Block)^[SizeOf(Block)-1] := I;

    {encrypt and save full block}
    EncryptBF(Context, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise ECipherException.Create(SInvalidFileFormat);
    EncryptBF(Context, Block, Encrypt);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
  if Assigned(LbOnProgress) then                                                {!!.06a}
    LbOnProgress (InStream.Position, InStream.Size);                            {!!.06a}
end;
{ -------------------------------------------------------------------------- }
procedure BFEncryptStreamCBC(InStream, OutStream : TStream;
            const Key : TKey128; Encrypt : Boolean);
var
  I : LongInt;
  Block : TBFBlock;
  IV : TBFBlock;
  Work : TBFBlock;
  Context : TBFContext;
  BlockCount : LongInt;
begin
  InitEncryptBF(Key, Context);

  {get the number of blocks in the file}
  BlockCount := (InStream.Size div SizeOf(Block));

  if Encrypt then
  begin
    {set up an initialization vector (IV)}
    Randomize;
    Block[0] := Random(High(Block[0]));
    Block[0] := Random(High(Block[1]));
    EncryptBF(Context, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));
    IV := Block;
  end else begin
    {read the frist block to prime the IV}
    InStream.Read(Block, SizeOf(Block));
    Dec(BlockCount);
    IV := Block;
  end;

  {when encrypting, make sure we have a block with at least one free}
  {byte at the end. used to store the odd byte count value}
  if Encrypt then
    Inc(BlockCount);

  {process all except the last block}
  for I := 1 to BlockCount - 1 do begin
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise ECipherException.Create(SInvalidFileFormat);

    if Encrypt then begin
      EncryptBFCBC(Context, IV, Block, Encrypt);
      IV := Block;
    end else begin
      Work := Block;
      EncryptBFCBC(Context, IV, Block, Encrypt);
      IV := Work;
    end;

    OutStream.Write(Block, SizeOf(Block));

    if Assigned(LbOnProgress) then                                              {!!.06a}
      if InStream.Position mod LbProgressSize = 0 then                          {!!.06a}
        LbOnProgress (InStream.Position, InStream.Size);                        {!!.06a}
  end;

  if Encrypt then begin
    FillChar(Block, SizeOf(Block), #0);

    {set actual number of bytes to read}
    I := (InStream.Size mod SizeOf(Block));
    if InStream.Read(Block, I) <> I then
      raise ECipherException.Create(SInvalidFileFormat);

    {store number of bytes as last byte in last block}
    PByteArray(@Block)^[SizeOf(Block)-1] := I;

    {encrypt and save full block}
    EncryptBFCBC(Context, IV, Block, Encrypt);
    OutStream.Write(Block, SizeOf(Block));
  end else begin
    {encrypted file is always a multiple of the block size}
    if InStream.Read(Block, SizeOf(Block)) <> SizeOf(Block) then
      raise ECipherException.Create(SInvalidFileFormat);
    EncryptBFCBC(Context, IV, Block, Encrypt);

    {get actual number of bytes encoded}
    I := PByteArray(@Block)^[SizeOf(Block)-1];

    {save valid portion of block}
    OutStream.Write(Block, I);
  end;
  if Assigned(LbOnProgress) then                                                {!!.06a}
    LbOnProgress (InStream.Position, InStream.Size);                            {!!.06a}
end;

begin                                                                           {!!.06a}
  LbOnProgress := nil;                                                          {!!.06a}
  LbProgressSize := 64;                                                         {!!.06a}
end.