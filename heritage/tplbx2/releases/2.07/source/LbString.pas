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
{*                   LBSTRING.PAS 2.07                   *}
{*     Copyright (c) 2002 TurboPower Software Co         *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I LockBox.inc}

{$H+}  {turn on huge strings}


unit LbString;
  {-string encryption routines}

interface

uses
  Classes, SysUtils, LbCipher;

procedure BFEncryptString(const InString : string; var OutString : string;
            const Key : TKey128; Encrypt : Boolean);
procedure BFEncryptStringCBC(const InString : string; var OutString : string;
            const Key : TKey128; Encrypt : Boolean);
procedure DESEncryptString(const InString : string; var OutString : string;
            const Key : TKey64; Encrypt : Boolean);
procedure DESEncryptStringCBC(const InString : string; var OutString : string;
            const Key : TKey64; Encrypt : Boolean);
procedure TripleDESEncryptString(const InString : string; var OutString : string;
            const Key : TKey128; Encrypt : Boolean);
procedure TripleDESEncryptStringCBC(const InString : string; var OutString : string;
            const Key : TKey128; Encrypt : Boolean);
procedure RDLEncryptString(const InString : string; var OutString : string;
            const Key; KeySize : Longint; Encrypt : Boolean);
procedure RDLEncryptStringCBC(const InString : string; var OutString : string;
            const Key; KeySize : Longint; Encrypt : Boolean);

function BFEncryptStringEx(const InString : string;
            const Key : TKey128; Encrypt : Boolean) : string;
function BFEncryptStringCBCEx(const InString : string;
            const Key : TKey128; Encrypt : Boolean) : string;
function DESEncryptStringEx(const InString : string;
            const Key : TKey64; Encrypt : Boolean) : string;
function DESEncryptStringCBCEx(const InString : string;
            const Key : TKey64; Encrypt : Boolean) : string;
function TripleDESEncryptStringEx(const InString : string;
            const Key : TKey128; Encrypt : Boolean) : string;
function TripleDESEncryptStringCBCEx(const InString : string;
            const Key : TKey128; Encrypt : Boolean) : string;
function RDLEncryptStringEx(const InString : string;
            const Key; KeySize : Longint; Encrypt : Boolean) : string;
function RDLEncryptStringCBCEx(const InString : string;
            const Key; KeySize : Longint; Encrypt : Boolean) : string;

procedure LbDecodeBase64(InStream, OutStream : TStream);
procedure LbEncodeBase64(InStream, OutStream : TStream);


implementation

uses
  LbProc;

const
  Lb64Table : array[0..63] of Char = ( #65,  #66,  #67,  #68,  #69,
         #70,  #71,  #72,  #73,  #74,  #75,  #76,  #77,  #78,  #79,
         #80,  #81,  #82,  #83,  #84,  #85,  #86,  #87,  #88,  #89,
         #90,  #97,  #98,  #99, #100, #101, #102, #103, #104, #105,
        #106, #107, #108, #109, #110, #111, #112, #113, #114, #115,
        #116, #117, #118, #119, #120, #121, #122,  #48,  #49,  #50,
         #51,  #52,  #53,  #54,  #55,  #56,  #57,  #43,  #47);

const
  LbD64Table : array[43..122] of Byte = ($3E, $7F, $7F, $7F, $3F, $34,
      $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $7F, $7F, $7F, $7F,
      $7F, $7F, $7F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09,
      $0A, $0B, $0C, $0D, $0E, $0F, $10, $11, $12, $13, $14, $15, $16,
      $17, $18, $19, $7F, $7F, $7F, $7F, $7F, $7F, $1A, $1B, $1C, $1D,
      $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2A,
      $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33);


{ == Base64 encoding/decoding routines ===================================== }
procedure LbDecodeBase64(InStream, OutStream : TStream);
var
  I, O, Count, c1, c2, c3 : Byte;
  InBuf  : array[0..87] of Byte;
  OutBuf : array[0..65] of Byte;
begin
  repeat
    O := 0;
    I := 0;

    Count := InStream.Read(InBuf, SizeOf(InBuf));
    if (Count = 0) then
      Break;

    { Decode data to output stream }
    while I < Count do begin
      if (InBuf[I] < 43) or (InBuf[I] > 122) or
         (InBuf[I+1] < 43) or (InBuf[I+1] > 122) or
         (InBuf[I+2] < 43) or (InBuf[I+2] > 122) or
         (InBuf[I+3] < 43) or (InBuf[I+3] > 122) then
        raise Exception.Create('Invalid Base64 Character');

      c1 := LbD64Table[InBuf[I]];
      c2 := LbD64Table[InBuf[I+1]];
      c3 := LbD64Table[InBuf[I+2]];
      OutBuf[O] := ((c1 shl 2) or (c2 shr 4));
      Inc(O);
      if Char(InBuf[I+2]) <> '=' then begin
        OutBuf[O] := ((c2 shl 4) or (c3 shr 2));
        Inc(O);
        if Char(InBuf[I+3]) <> '=' then begin
          OutBuf[O] := ((c3 shl 6) or LbD64Table[InBuf[I+3]]);
          Inc(O);
        end;
      end;
      Inc(I, 4);
    end;
    OutStream.Write(OutBuf, O);
  until Count < SizeOf(InBuf);
end;
{ -------------------------------------------------------------------------- }
procedure LbEncodeBase64(InStream, OutStream : TStream);
var
  I, O, Count : Integer;
  InBuf  : array[1..45] of Byte;
  OutBuf : array[0..62] of Char;
  Temp : Byte;
begin
  FillChar(OutBuf, Sizeof(OutBuf), #0);

  repeat
    Count := InStream.Read(InBuf, SizeOf(InBuf));
    if Count = 0 then Break;
    I := 1;
    O := 0;
    while I <= (Count-2) do begin
      { Encode 1st byte }
      Temp := (InBuf[I] shr 2);
      OutBuf[O] := Char(Lb64Table[Temp and $3F]);

      { Encode 1st/2nd byte }
      Temp := (InBuf[I] shl 4) or (InBuf[I+1] shr 4);
      OutBuf[O+1] := Char(Lb64Table[Temp and $3F]);

      { Encode 2nd/3rd byte }
      Temp := (InBuf[I+1] shl 2) or (InBuf[I+2] shr 6);
      OutBuf[O+2] := Char(Lb64Table[Temp and $3F]);

      { Encode 3rd byte }
      Temp := (InBuf[I+2] and $3F);
      OutBuf[O+3] := Char(Lb64Table[Temp]);

      Inc(I, 3);
      Inc(O, 4);
    end;

    { Are there odd bytes to add? }
    if (I <= Count) then begin
      Temp := (InBuf[I] shr 2);
      OutBuf[O] := Char(Lb64Table[Temp and $3F]);

      { One odd byte }
      if I = Count then begin
        Temp := (InBuf[I] shl 4) and $30;
        OutBuf[O+1] := Char(Lb64Table[Temp and $3F]);
        OutBuf[O+2] := '=';
      { Two odd bytes }
      end else begin
        Temp := ((InBuf[I] shl 4) and $30) or ((InBuf[I+1] shr 4) and $0F);
        OutBuf[O+1] := Char(Lb64Table[Temp and $3F]);
        Temp := (InBuf[I+1] shl 2) and $3C;
        OutBuf[O+2] := Char(Lb64Table[Temp and $3F]);
      end;
      { Add padding }
      OutBuf[O+3] := '=';
      Inc(O, 4);
    end;

    { Write encoded block to stream }
    OutStream.Write(OutBuf, O);
  until Count < SizeOf(InBuf);
end;


{ == Blowfish string encryption/decryption ================================= }
function BFEncryptStringEx(const InString : string;
            const Key : TKey128; Encrypt : Boolean) : string;  
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
  WorkStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  WorkStream := TMemoryStream.Create;
  InStream.Write(InString[1], Length(InString));
  InStream.Position := 0;

  if Encrypt then begin
    BFEncryptStream(InStream, WorkStream, Key, True);
    WorkStream.Position := 0;
    LbEncodeBase64(WorkStream, OutStream);
  end else begin
    LbDecodeBase64(InStream, WorkStream);
    WorkStream.Position := 0;
    BFEncryptStream(WorkStream, OutStream, Key, False);
  end;
  OutStream.Position := 0;
  SetLength(Result, OutStream.Size);
  OutStream.Read(Result[1], OutStream.Size);

  InStream.Free;
  OutStream.Free;
  WorkStream.Free;
end;
{ -------------------------------------------------------------------------- }
function BFEncryptStringCBCEx(const InString : string;
            const Key : TKey128; Encrypt : Boolean) : string;  
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
  WorkStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  WorkStream := TMemoryStream.Create;
  InStream.Write(InString[1], Length(InString));
  InStream.Position := 0;

  if Encrypt then begin
    BFEncryptStreamCBC(InStream, WorkStream, Key, True);
    WorkStream.Position := 0;
    LbEncodeBase64(WorkStream, OutStream);
  end else begin
    LbDecodeBase64(InStream, WorkStream);
    WorkStream.Position := 0;
    BFEncryptStreamCBC(WorkStream, OutStream, Key, False);
  end;
  OutStream.Position := 0;
  SetLength(Result, OutStream.Size);
  OutStream.Read(Result[1], OutStream.Size);

  InStream.Free;
  OutStream.Free;
  WorkStream.Free;
end;
{ -------------------------------------------------------------------------- }
procedure BFEncryptString(const InString : string; var OutString : string;
            const Key : TKey128; Encrypt : Boolean);
begin
  OutString := BFEncryptStringEx(InString, Key, Encrypt);
end;
{ -------------------------------------------------------------------------- }
procedure BFEncryptStringCBC(const InString : string; var OutString : string;
            const Key : TKey128; Encrypt : Boolean);
begin
  OutString := BFEncryptStringCBCEx(InString, Key, Encrypt);
end;


{ == DES string encryption/decryption ====================================== }
function DESEncryptStringEx(const InString : string;
            const Key : TKey64; Encrypt : Boolean) : string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
  WorkStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  WorkStream := TMemoryStream.Create;
  InStream.Write(InString[1], Length(InString));
  InStream.Position := 0;

  if Encrypt then begin
    DESEncryptStream(InStream, WorkStream, Key, True);
    WorkStream.Position := 0;
    LbEncodeBase64(WorkStream, OutStream);
  end else begin
    LbDecodeBase64(InStream, WorkStream);
    WorkStream.Position := 0;
    DESEncryptStream(WorkStream, OutStream, Key, False);
  end;
  OutStream.Position := 0;
  SetLength(Result, OutStream.Size);
  OutStream.Read(Result[1], OutStream.Size);

  InStream.Free;
  OutStream.Free;
  WorkStream.Free;
end;
{ -------------------------------------------------------------------------- }
function DESEncryptStringCBCEx(const InString : string;
            const Key : TKey64; Encrypt : Boolean) : string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
  WorkStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  WorkStream := TMemoryStream.Create;
  InStream.Write(InString[1], Length(InString));
  InStream.Position := 0;

  if Encrypt then begin
    DESEncryptStreamCBC(InStream, WorkStream, Key, True);
    WorkStream.Position := 0;
    LbEncodeBase64(WorkStream, OutStream);
  end else begin
    LbDecodeBase64(InStream, WorkStream);
    WorkStream.Position := 0;
    DESEncryptStreamCBC(WorkStream, OutStream, Key, False);
  end;
  OutStream.Position := 0;
  SetLength(Result, OutStream.Size);
  OutStream.Read(Result[1], OutStream.Size);

  InStream.Free;
  OutStream.Free;
  WorkStream.Free;
end;
{ -------------------------------------------------------------------------- }
procedure DESEncryptString(const InString : string; var OutString : string;
            const Key : TKey64; Encrypt : Boolean);
begin
  OutString := DESEncryptStringEx(InString, Key, Encrypt);
end;
{ -------------------------------------------------------------------------- }
procedure DESEncryptStringCBC(const InString : string; var OutString : string;
            const Key : TKey64; Encrypt : Boolean);
begin
  OutString := DESEncryptStringCBCEx(InString, Key, Encrypt);
end;


{ == TripleDES string encryption/decryption ================================ }
function TripleDESEncryptStringEx(const InString : string;
            const Key : TKey128; Encrypt : Boolean) : string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
  WorkStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  WorkStream := TMemoryStream.Create;
  InStream.Write(InString[1], Length(InString));
  InStream.Position := 0;

  if Encrypt then begin
    TripleDESEncryptStream(InStream, WorkStream, Key, True);
    WorkStream.Position := 0;
    LbEncodeBase64(WorkStream, OutStream);
  end else begin
    LbDecodeBase64(InStream, WorkStream);
    WorkStream.Position := 0;
    TripleDESEncryptStream(WorkStream, OutStream, Key, False);
  end;
  OutStream.Position := 0;
  SetLength(Result, OutStream.Size);
  OutStream.Read(Result[1], OutStream.Size);

  InStream.Free;
  OutStream.Free;
  WorkStream.Free;
end;
{ -------------------------------------------------------------------------- }
function TripleDESEncryptStringCBCEx(const InString : string;
            const Key : TKey128; Encrypt : Boolean) : string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
  WorkStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  WorkStream := TMemoryStream.Create;
  InStream.Write(InString[1], Length(InString));
  InStream.Position := 0;

  if Encrypt then begin
    TripleDESEncryptStreamCBC(InStream, WorkStream, Key, True);
    WorkStream.Position := 0;
    LbEncodeBase64(WorkStream, OutStream);
  end else begin
    LbDecodeBase64(InStream, WorkStream);
    WorkStream.Position := 0;
    TripleDESEncryptStreamCBC(WorkStream, OutStream, Key, False);
  end;
  OutStream.Position := 0;
  SetLength(Result, OutStream.Size);
  OutStream.Read(Result[1], OutStream.Size);

  InStream.Free;
  OutStream.Free;
  WorkStream.Free;
end;
{ -------------------------------------------------------------------------- }
procedure TripleDESEncryptString(const InString : string; var OutString : string;
            const Key : TKey128; Encrypt : Boolean);
begin
  OutString := TripleDESEncryptStringEx(InString, Key, Encrypt);
end;
{ -------------------------------------------------------------------------- }
procedure TripleDESEncryptStringCBC(const InString : string; var OutString : string;
            const Key : TKey128; Encrypt : Boolean);
begin
  OutString := TripleDESEncryptStringCBCEx(InString, Key, Encrypt);
end;


{ == Rijndael string encryption/decryption ================================== }
function RDLEncryptStringEx(const InString : string;
            const Key; KeySize : Longint; Encrypt : Boolean) : string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
  WorkStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  WorkStream := TMemoryStream.Create;
  InStream.Write(InString[1], Length(InString));
  InStream.Position := 0;

  if Encrypt then begin
    RDLEncryptStream(InStream, WorkStream, Key, KeySize, True);
    WorkStream.Position := 0;
    LbEncodeBase64(WorkStream, OutStream);
  end else begin
    LbDecodeBase64(InStream, WorkStream);
    WorkStream.Position := 0;
    RDLEncryptStream(WorkStream, OutStream, Key, KeySize, False);
  end;
  OutStream.Position := 0;
  SetLength(Result, OutStream.Size);
  OutStream.Read(Result[1], OutStream.Size);

  InStream.Free;
  OutStream.Free;
  WorkStream.Free;
end;
{ -------------------------------------------------------------------------- }
function RDLEncryptStringCBCEx(const InString : string;
            const Key; KeySize : Longint; Encrypt : Boolean) : string;
var
  InStream  : TMemoryStream;
  OutStream : TMemoryStream;
  WorkStream : TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  WorkStream := TMemoryStream.Create;
  InStream.Write(InString[1], Length(InString));
  InStream.Position := 0;

  if Encrypt then begin
    RDLEncryptStreamCBC(InStream, WorkStream, Key, KeySize, True);
    WorkStream.Position := 0;
    LbEncodeBase64(WorkStream, OutStream);
  end else begin
    LbDecodeBase64(InStream, WorkStream);
    WorkStream.Position := 0;
    RDLEncryptStreamCBC(WorkStream, OutStream, Key, KeySize, False);
  end;
  OutStream.Position := 0;
  SetLength(Result, OutStream.Size);
  OutStream.Read(Result[1], OutStream.Size);

  InStream.Free;
  OutStream.Free;
  WorkStream.Free;
end;
{ -------------------------------------------------------------------------- }
procedure RDLEncryptString(const InString : string; var OutString : string;
            const Key; KeySize : Longint; Encrypt : Boolean);
begin
  OutString := RDLEncryptStringEx(InString, Key, KeySize, Encrypt);
end;
{ -------------------------------------------------------------------------- }
procedure RDLEncryptStringCBC(const InString : string; var OutString : string;
            const Key; KeySize : Longint; Encrypt : Boolean);
begin
  OutString := RDLEncryptStringCBCEx(InString, Key, KeySize, Encrypt);
end;


end.
