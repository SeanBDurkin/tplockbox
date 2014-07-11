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
{*                   LBCLASS.PAS 2.07                    *}
{*     Copyright (c) 2002 TurboPower Software Co         *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I LockBox.inc}

{$H+}  {turn on huge strings}

unit LbClass;
  {-LockBox components and classes }

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
  LbCipher;



{ TLbBaseComponent }
type
  TLBBaseComponent = class(TLBBase)
  protected {private}
    function GetVersion : string;
    procedure SetVersion(const Value : string);
  published {properties}
    property Version : string
      read GetVersion write SetVersion stored False;
  end;


{ TLbCipher }
type
  TLbCipherMode = (cmECB, cmCBC);

  TLbCipher = class(TLbBaseComponent)
  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function DecryptBuffer(const InBuf; InBufSize : Cardinal; var OutBuf) : Cardinal;
    function EncryptBuffer(const InBuf; InBufSize : Cardinal; var OutBuf) : Cardinal;

    procedure DecryptFile(const InFile, OutFile : string); virtual; abstract;
    procedure DecryptStream(InStream , OutStream : TStream); virtual; abstract;
    function  DecryptString(const InString : string) : string; virtual; abstract;

    procedure EncryptFile(const InFile, OutFile : string); virtual; abstract;
    procedure EncryptStream(InStream, OutStream : TStream); virtual; abstract;
    function  EncryptString(const InString : string) : string; virtual; abstract;

    function OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal; virtual; abstract;
  end;


{ TLbSymmetricCipher }
type
  TLbSymmetricCipher = class(TLbCipher)
  protected {private}
    FCipherMode : TLbCipherMode;
  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure GenerateKey(const Passphrase : string); virtual; abstract;
    procedure GenerateRandomKey; virtual; abstract;
  public {properties}
    property CipherMode : TLbCipherMode
               read FCipherMode write FCipherMode;
  end;


{ TLbBlowfish }
type
  TLbBlowfish = class(TLbSymmetricCipher)
  protected {private}
    FKey : TKey128;
  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DecryptFile(const InFile, OutFile : string); override;
    procedure DecryptStream(InStream , OutStream : TStream); override;
    function  DecryptString(const InString : string) : string; override;

    procedure EncryptFile(const InFile, OutFile : string); override;
    procedure EncryptStream(InStream, OutStream : TStream); override;
    function  EncryptString(const InString : string) : string; override;

    procedure GenerateKey(const Passphrase : string); override;
    procedure GenerateRandomKey; override;

    procedure GetKey(var Key : TKey128);
    procedure SetKey(const Key : TKey128);

    function OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal; override;

  published {properties}
    property CipherMode;
  end;


{ TLbDES }
type
  TLbDES = class(TLbSymmetricCipher)
  protected {private}
    FKey : TKey64;
  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DecryptFile(const InFile, OutFile : string); override;
    procedure DecryptStream(InStream , OutStream : TStream); override;
    function  DecryptString(const InString : string) : string; override;

    procedure EncryptFile(const InFile, OutFile : string); override;
    procedure EncryptStream(InStream, OutStream : TStream); override;
    function  EncryptString(const InString : string) : string; override;

    procedure GenerateKey(const Passphrase : string); override;
    procedure GenerateRandomKey; override;

    procedure GetKey(var Key : TKey64);
    procedure SetKey(const Key : TKey64);

    function OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal; override;

  published {properties}
    property CipherMode;
  end;


{ TLb3DES }
type
  TLb3DES = class(TLbSymmetricCipher)
  protected {private}
    FKey : TKey128;
  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DecryptFile(const InFile, OutFile : string); override;
    procedure DecryptStream(InStream , OutStream : TStream); override;
    function  DecryptString(const InString : string) : string; override;

    procedure EncryptFile(const InFile, OutFile : string); override;
    procedure EncryptStream(InStream, OutStream : TStream); override;
    function  EncryptString(const InString : string) : string; override;

    procedure GenerateKey(const Passphrase : string); override;
    procedure GenerateRandomKey; override;

    procedure GetKey(var Key : TKey128);
    procedure SetKey(const Key : TKey128);

    function OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal; override;

  published {properties}
    property CipherMode;
  end;


{ TLbRijndael }
type
  TLbKeySizeRDL = (ks128, ks192, ks256);

type
  TLbRijndael = class(TLbSymmetricCipher)
  protected {private}
    FKey     : TKey256;
    FKeySize : TLbKeySizeRDL;
    FKeySizeBytes : Integer;
    procedure SetKeySize(Value : TLbKeySizeRDL);
  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DecryptFile(const InFile, OutFile : string); override;
    procedure DecryptStream(InStream , OutStream : TStream); override;
    function  DecryptString(const InString : string) : string; override;

    procedure EncryptFile(const InFile, OutFile : string); override;
    procedure EncryptStream(InStream, OutStream : TStream); override;
    function  EncryptString(const InString : string) : string; override;

    procedure GenerateKey(const Passphrase : string); override;
    procedure GenerateRandomKey; override;

    procedure GetKey(var Key);
    procedure SetKey(const Key);

    function OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal; override;

  published {properties}
    property CipherMode;
    property KeySize : TLbKeySizeRDL
      read FKeySize write SetKeySize;
  end;


{ TLbHash }
type
  TLbHash = class(TLbBaseComponent)
    protected {private}
      FBuf : array[0..1023] of Byte;
    public {methods}
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure HashBuffer(const Buf; BufSize : Cardinal); virtual; abstract;
      procedure HashFile(const AFileName : string); virtual; abstract;
      procedure HashStream(AStream: TStream); virtual; abstract;
      procedure HashString(const AStr : string); virtual; abstract;
    end;


{ TLbMD5 }
type
  TLbMD5 = class(TLbHash)
    protected {private}
      FDigest : TMD5Digest;
    public {methods}
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

      procedure GetDigest(var Digest : TMD5Digest);

      procedure HashBuffer(const Buf; BufSize : Cardinal); override;
      procedure HashFile(const AFileName : string); override;
      procedure HashStream(AStream: TStream); override;
      procedure HashString(const AStr : string); override;
    end;


{ TLbSHA1 }
type
  TLbSHA1 = class(TLbHash)
    protected {private}
      FDigest : TSHA1Digest;
    public {methods}
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

      procedure GetDigest(var Digest : TSHA1Digest);

      procedure HashBuffer(const Buf; BufSize : Cardinal); override;
      procedure HashFile(const AFileName : string); override;
      procedure HashStream(AStream: TStream); override;
      procedure HashString(const AStr : string); override;
    end;


{ TLbSCStream }
type
  TLbSCStream = class(TMemoryStream)
  protected {private}
    FContext : TLSCContext;
  public {methods}
    constructor Create(const Key; KeySize : Integer);
    procedure Reinitialize(const Key; KeySize : Integer); dynamic;
    procedure ChangeKey(const Key; KeySize : Integer); dynamic;
    function Read(var Buffer; Count : Longint) : Longint; override;
    function Write(const Buffer; Count : Longint) : Longint; override;
  end;


{ TLbSCFileStream }
type
  TLbSCFileStream = class(TFileStream)
  protected {private}
    FContext : TLSCContext;
  public {methods}
    constructor Create(const FileName : string; Mode : Word; const Key; KeySize : Integer);
    procedure Reinitialize(const Key; KeySize : Integer); dynamic;
    procedure ChangeKey(const Key; KeySize : Integer); dynamic;
    function Read(var Buffer; Count : Longint) : Longint; override;
    function Write(const Buffer; Count : Longint) : Longint; override;
  end;


{ TLbRNG32Stream }
type
  TLbRNG32Stream = class(TMemoryStream)
  protected {private}
    FContext : TRNG32Context;
  public {methods}
    constructor Create(const Key : LongInt);
    procedure Reinitialize(const Key : LongInt); dynamic;
    procedure ChangeKey(const Key : LongInt); dynamic;
    function Read(var Buffer; Count : LongInt) : LongInt; override;
    function Write(const Buffer; Count : LongInt) : LongInt; override;
  end;


{ TLbRNG32FileStream }
type
  TLbRNG32FileStream = class(TFileStream)
  protected {private}
    FContext : TRNG32Context;
  public {methods}
    constructor Create(const FileName : string; Mode : Word; const Key : LongInt);
    procedure Reinitialize(const Key : LongInt); dynamic;
    procedure ChangeKey(const Key : LongInt); dynamic;
    function Read(var Buffer; Count : LongInt) : LongInt; override;
    function Write(const Buffer; Count : LongInt) : LongInt; override;
  end;


{ TLbRNG64Stream }
  TLbRNG64Stream = class(TMemoryStream)
  protected {private}
    FContext : TRNG64Context;
  public {methods}
    constructor Create(const KeyHi, KeyLo : LongInt);
    procedure Reinitialize(const KeyHi, KeyLo : LongInt); dynamic;
    procedure ChangeKey(const KeyHi, KeyLo : LongInt); dynamic;
    function Read(var Buffer; Count : LongInt) : LongInt; override;
    function Write(const Buffer; Count : LongInt) : LongInt; override;
  end;


{ TLbRNG64FileStream }
type
  TLbRNG64FileStream = class(TFileStream)
  protected {private}
    FContext : TRNG64Context;
  public {methods}
    constructor Create(const FileName : string; Mode : Word; const KeyHi, KeyLo : LongInt);
    procedure Reinitialize(const KeyHi, KeyLo : LongInt); dynamic;
    procedure ChangeKey(const KeyHi, KeyLo : LongInt); dynamic;
    function Read(var Buffer; Count : LongInt) : LongInt; override;
    function Write(const Buffer; Count : LongInt) : LongInt; override;
  end;

implementation

uses
  LbProc, LbString, LbConst;


const
  RDLKeySizeMap : array[TLbKeySizeRDL] of Integer = (16, 24, 32);



{ == TLbBaseComponent ====================================================== }
function TLBBaseComponent.GetVersion : string;
begin
  Result := sLbVersion;
end;
{ -------------------------------------------------------------------------- }
procedure TLBBaseComponent.SetVersion(const Value : string);
begin
  { nop }
end;


{ == TLbCipher ============================================================= }
constructor TLbCipher.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLbCipher.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TLbCipher.DecryptBuffer(const InBuf; InBufSize : Cardinal; var OutBuf) : Cardinal;
var
  InS, OutS : TMemoryStream;
begin
  InS := TMemoryStream.Create;
  OutS := TMemoryStream.Create;
  try
    InS.SetSize(InBufSize);
    InS.Write(InBuf, InBufSize);
    InS.Position := 0;
    DecryptStream(InS, OutS);
    OutS.Position := 0;
    OutS.Read(OutBuf, OutS.Size);
    Result := OutS.Size;
  finally
    InS.Free;
    OutS.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
function TLbCipher.EncryptBuffer(const InBuf; InBufSize : Cardinal; var OutBuf) : Cardinal;
var
  InS, OutS : TMemoryStream;
begin
  InS := TMemoryStream.Create;
  OutS := TMemoryStream.Create;
  try
    InS.SetSize(InBufSize);
    InS.Write(InBuf, InBufSize);
    InS.Position := 0;
    EncryptStream(InS, OutS);
    OutS.Position := 0;
    OutS.Read(OutBuf, OutS.Size);
    Result := OutS.Size;
  finally
    InS.Free;
    OutS.Free;
  end;
end;


{ == TLbSymmetricCipher ==================================================== }
constructor TLbSymmetricCipher.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLbSymmetricCipher.Destroy;
begin
  inherited Destroy;
end;


{ == TLbBlowfish =========================================================== }
constructor TLbBlowfish.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLbBlowfish.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.DecryptFile(const InFile, OutFile : string);
begin
  case CipherMode of
    cmECB : BFEncryptFile(InFile, OutFile, FKey, False);
    cmCBC : BFEncryptFileCBC(InFile, OutFile, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.DecryptStream(InStream , OutStream : TStream);
begin
  case CipherMode of
    cmECB : BFEncryptStream(InStream, OutStream, FKey, False);
    cmCBC : BFEncryptStreamCBC(InStream, OutStream, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
function TLbBlowfish.DecryptString(const InString : string) : string;
begin
  case CipherMode of
    cmECB : Result := BFEncryptStringEx(InString, FKey, False);
    cmCBC : Result := BFEncryptStringCBCEx(InString, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.EncryptFile(const InFile, OutFile : string);
begin
  case CipherMode of
    cmECB : BFEncryptFile(InFile, OutFile, FKey, True);
    cmCBC : BFEncryptFileCBC(InFile, OutFile, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.EncryptStream(InStream, OutStream : TStream);
begin
  case CipherMode of
    cmECB : BFEncryptStream(InStream, OutStream, FKey, True);
    cmCBC : BFEncryptStreamCBC(InStream, OutStream, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
function TLbBlowfish.EncryptString(const InString : string) : string;
begin
  case CipherMode of
    cmECB : Result := BFEncryptStringEx(InString, FKey, True);
    cmCBC : Result := BFEncryptStringCBCEx(InString, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.GenerateKey(const Passphrase : string);
begin
  GenerateLMDKey(FKey, SizeOf(FKey), Passphrase);
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.GenerateRandomKey;
begin
  LbCipher.GenerateRandomKey(FKey, SizeOf(FKey));
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.GetKey(var Key : TKey128);
begin
  Key := FKey;
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.SetKey(const Key : TKey128);
begin
  FKey := Key;
end;
{ -------------------------------------------------------------------------- }
function TLbBlowfish.OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal;
var
  BlkCount, BlkSize : Cardinal;
begin
  BlkSize := SizeOf(TBFBlock);
  BlkCount := (InBufSize div BlkSize) + 1;                           {!!.05}
  Result := BlkCount * BlkSize;
end;


{ == TLbDES ================================================================ }
constructor TLbDES.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLbDES.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TLbDES.DecryptFile(const InFile, OutFile : string);
begin
  case CipherMode of
    cmECB : DESEncryptFile(InFile, OutFile, FKey, False);
    cmCBC : DESEncryptFileCBC(InFile, OutFile, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbDES.DecryptStream(InStream , OutStream : TStream);
begin
  case CipherMode of
    cmECB : DESEncryptStream(InStream, OutStream, FKey, False);
    cmCBC : DESEncryptStreamCBC(InStream, OutStream, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
function TLbDES.DecryptString(const InString : string) : string;
begin
  case CipherMode of
    cmECB : Result := DESEncryptStringEx(InString, FKey, False);
    cmCBC : Result := DESEncryptStringCBCEx(InString, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbDES.EncryptFile(const InFile, OutFile : string);
begin
  case CipherMode of
    cmECB : DESEncryptFile(InFile, OutFile, FKey, True);
    cmCBC : DESEncryptFileCBC(InFile, OutFile, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbDES.EncryptStream(InStream, OutStream : TStream);
begin
  case CipherMode of
    cmECB : DESEncryptStream(InStream, OutStream, FKey, True);
    cmCBC : DESEncryptStreamCBC(InStream, OutStream, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
function TLbDES.EncryptString(const InString : string) : string;
begin
  case CipherMode of
    cmECB : Result := DESEncryptStringEx(InString, FKey, True);
    cmCBC : Result := DESEncryptStringCBCEx(InString, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbDES.GenerateKey(const Passphrase : string);
begin
  GenerateLMDKey(FKey, SizeOf(FKey), Passphrase);
end;
{ -------------------------------------------------------------------------- }
procedure TLbDES.GenerateRandomKey;
begin
  LbCipher.GenerateRandomKey(FKey, SizeOf(FKey));
end;
{ -------------------------------------------------------------------------- }
procedure TLbDES.SetKey(const Key : TKey64);
begin
  FKey := Key;
end;
{ -------------------------------------------------------------------------- }
procedure TLbDES.GetKey(var Key : TKey64);
begin
  Key := FKey;
end;
{ -------------------------------------------------------------------------- }
function TLbDES.OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal;
var
  BlkCount, BlkSize : Cardinal;
begin
  BlkSize := SizeOf(TDESBlock);
  BlkCount := (InBufSize div BlkSize) + 1;                           {!!.05}
  Result := BlkCount * BlkSize;
end;


{ == TLb3DES ================================================================ }
constructor TLb3DES.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLb3DES.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TLb3DES.DecryptFile(const InFile, OutFile : string);
begin
  case CipherMode of
    cmECB : TripleDESEncryptFile(InFile, OutFile, FKey, False);
    cmCBC : TripleDESEncryptFileCBC(InFile, OutFile, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLb3DES.DecryptStream(InStream , OutStream : TStream);
begin
  case CipherMode of
    cmECB : TripleDESEncryptStream(InStream, OutStream, FKey, False);
    cmCBC : TripleDESEncryptStreamCBC(InStream, OutStream, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
function TLb3DES.DecryptString(const InString : string) : string;
begin
  case CipherMode of
    cmECB : Result := TripleDESEncryptStringEx(InString, FKey, False);
    cmCBC : Result := TripleDESEncryptStringCBCEx(InString, FKey, False);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLb3DES.EncryptFile(const InFile, OutFile : string);
begin
  case CipherMode of
    cmECB : TripleDESEncryptFile(InFile, OutFile, FKey, True);
    cmCBC : TripleDESEncryptFileCBC(InFile, OutFile, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLb3DES.EncryptStream(InStream, OutStream : TStream);
begin
  case CipherMode of
    cmECB : TripleDESEncryptStream(InStream, OutStream, FKey, True);
    cmCBC : TripleDESEncryptStreamCBC(InStream, OutStream, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
function TLb3DES.EncryptString(const InString : string) : string;
begin
  case CipherMode of
    cmECB : Result := TripleDESEncryptStringEx(InString, FKey, True);
    cmCBC : Result := TripleDESEncryptStringCBCEx(InString, FKey, True);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLb3DES.GenerateKey(const Passphrase : string);
begin
  GenerateLMDKey(FKey, SizeOf(FKey), Passphrase);
end;
{ -------------------------------------------------------------------------- }
procedure TLb3DES.GenerateRandomKey;
begin
  LbCipher.GenerateRandomKey(FKey, SizeOf(FKey));
end;
{ -------------------------------------------------------------------------- }
procedure TLb3DES.SetKey(const Key : TKey128);
begin
  FKey := Key;
end;
{ -------------------------------------------------------------------------- }
procedure TLb3DES.GetKey(var Key : TKey128);
begin
  Key := FKey;
end;
{ -------------------------------------------------------------------------- }
function TLb3DES.OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal;
var
  BlkCount, BlkSize : Cardinal;
begin
  BlkSize := SizeOf(TDESBlock);
  BlkCount := (InBufSize div BlkSize) + 1;                           {!!.05}
  Result := BlkCount * BlkSize;
end;


{ == TLbRijndael =========================================================== }
constructor TLbRijndael.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLbRijndael.Destroy;
begin
  inherited Destroy;
  KeySize := ks128;                                                    {!!.04}
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.DecryptFile(const InFile, OutFile : string);
begin
  case CipherMode of
    cmECB : RDLEncryptFile(InFile, OutFile, FKey, FKeySizeBytes, False);
    cmCBC : RDLEncryptFileCBC(InFile, OutFile, FKey, FKeySizeBytes, False);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.DecryptStream(InStream , OutStream : TStream);
begin
  case CipherMode of
    cmECB : RDLEncryptStream(InStream, OutStream, FKey, FKeySizeBytes, False);
    cmCBC : RDLEncryptStreamCBC(InStream, OutStream, FKey, FKeySizeBytes, False);
  end;
end;
{ -------------------------------------------------------------------------- }
function TLbRijndael.DecryptString(const InString : string) : string;
begin
  case CipherMode of
    cmECB : Result := RDLEncryptStringEx(InString, FKey, FKeySizeBytes, False);
    cmCBC : Result := RDLEncryptStringCBCEx(InString, FKey, FKeySizeBytes, False);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.EncryptFile(const InFile, OutFile : string);
begin
  case CipherMode of
    cmECB : RDLEncryptFile(InFile, OutFile, FKey, FKeySizeBytes, True);
    cmCBC : RDLEncryptFileCBC(InFile, OutFile, FKey, FKeySizeBytes, True);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.EncryptStream(InStream, OutStream : TStream);
begin
  case CipherMode of
    cmECB : RDLEncryptStream(InStream, OutStream, FKey, FKeySizeBytes, True);
    cmCBC : RDLEncryptStreamCBC(InStream, OutStream, FKey, FKeySizeBytes, True);
  end;
end;
{ -------------------------------------------------------------------------- }
function TLbRijndael.EncryptString(const InString : string) : string;
begin
  case CipherMode of
    cmECB : Result := RDLEncryptStringEx(InString, FKey, FKeySizeBytes, True);
    cmCBC : Result := RDLEncryptStringCBCEx(InString, FKey, FKeySizeBytes, True);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.GenerateKey(const Passphrase : string);
begin
  GenerateLMDKey(FKey, FKeySizeBytes, Passphrase);
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.GenerateRandomKey;
begin
  LbCipher.GenerateRandomKey(FKey, FKeySizeBytes);
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.GetKey(var Key);
begin
  Move(FKey, Key, FKeySizeBytes);
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.SetKey(const Key);
begin
  Move(Key, FKey, FKeySizeBytes);
end;
{ -------------------------------------------------------------------------- }
procedure TLbRijndael.SetKeySize(Value : TLbKeySizeRDL);
begin
  FKeySize := Value;
  FKeySizeBytes := RDLKeySizeMap[Value];
end;
{ -------------------------------------------------------------------------- }
function TLbRijndael.OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal;
var
  BlkCount, BlkSize : Cardinal;
begin
  BlkSize := SizeOf(TRDLBlock);
  BlkCount := (InBufSize div BlkSize) + 1;
  Result := BlkCount * BlkSize;
end;


{ == TLbHash =============================================================== }
constructor TLbHash.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLbHash.Destroy;
begin
  inherited Destroy;
end;


{ == TLbMD5 ================================================================ }
constructor TLbMD5.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLbMD5.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TLbMD5.GetDigest(var Digest : TMD5Digest);
begin
  Move(FDigest, Digest, SizeOf(Digest));
end;
{ -------------------------------------------------------------------------- }
procedure TLbMD5.HashBuffer(const Buf; BufSize : Cardinal);
begin
  HashMD5(FDigest, Buf, BufSize);
end;
{ -------------------------------------------------------------------------- }
procedure TLbMD5.HashFile(const AFileName : string);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    HashStream(FS);
  finally
    FS.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbMD5.HashStream(AStream: TStream);
var
  Context : TMD5Context;
  BufSize : Integer;
begin
  InitMD5(Context);
  BufSize := AStream.Read(FBuf, SizeOf(FBuf));
  while (BufSize > 0) do begin
    UpdateMD5(Context, FBuf, BufSize);
    BufSize := AStream.Read(FBuf, SizeOf(FBuf));
  end;
  FinalizeMD5(Context, FDigest);
end;
{ -------------------------------------------------------------------------- }
procedure TLbMD5.HashString(const AStr : string);
begin
  StringHashMD5(FDigest, AStr);
end;


{ == TLbSHA1 =============================================================== }
constructor TLbSHA1.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;
{ -------------------------------------------------------------------------- }
destructor TLbSHA1.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TLbSHA1.GetDigest(var Digest : TSHA1Digest);
begin
  Move(FDigest, Digest, SizeOf(Digest));
end;
{ -------------------------------------------------------------------------- }
procedure TLbSHA1.HashBuffer(const Buf; BufSize : Cardinal);
begin
  HashSHA1(FDigest, Buf, BufSize);
end;
{ -------------------------------------------------------------------------- }
procedure TLbSHA1.HashFile(const AFileName : string);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    HashStream(FS);
  finally
    FS.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbSHA1.HashStream(AStream: TStream);
var
  Context : TSHA1Context;
  BufSize : Integer;
begin
  InitSHA1(Context);
  BufSize := AStream.Read(FBuf, SizeOf(FBuf));
  while (BufSize > 0) do begin
    UpdateSHA1(Context, FBuf, BufSize);
    BufSize := AStream.Read(FBuf, SizeOf(FBuf));
  end;
  FinalizeSHA1(Context, FDigest);
end;
{ -------------------------------------------------------------------------- }
procedure TLbSHA1.HashString(const AStr : string);
begin
  StringHashSHA1(FDigest, AStr);
end;


{ == TLbSCStream =========================================================== }
constructor TLbSCStream.Create(const Key; KeySize : Integer);
  {-create the stream and initialize context}
begin
  inherited Create;

  Reinitialize(Key, KeySize);
end;
{ -------------------------------------------------------------------------- }
procedure TLbSCStream.Reinitialize(const Key; KeySize : Integer);
  {-reinitialize context and reposition to beginning of stream}
begin
  ChangeKey(Key, KeySize);
  Position := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TLbSCStream.ChangeKey(const Key; KeySize : Integer);
  {-reinitialize using a new key}
begin
  InitEncryptLSC(Key, KeySize, FContext);
end;
{ -------------------------------------------------------------------------- }
function TLbSCStream.Read(var Buffer; Count : LongInt) : LongInt;
  {-read Count bytes into Buffer, return bytes read}
begin
  Result := inherited Read(Buffer, Count);
  EncryptLSC(FContext, Buffer, Count);
end;
{ -------------------------------------------------------------------------- }
function TLbSCStream.Write(const Buffer; Count : LongInt) : LongInt;
  {-write Count bytes to Buffer, return bytes written}
var
  Buf : Pointer;
begin
  GetMem(Buf, Count);
  try
    Move(Buffer, Buf^, Count);
    EncryptLSC(FContext, Buf^, Count);
    Result := inherited Write(Buf^, Count);
  finally
    FreeMem(Buf, Count);
  end;
end;


{ == TLbSCFileStream ======================================================= }
constructor TLbSCFileStream.Create(const FileName : string; Mode : Word;
                                 const Key; KeySize : Integer);
  {-create the stream and initialize context}
begin
  inherited Create(FileName, Mode);

  Reinitialize(Key, KeySize);
end;
{ -------------------------------------------------------------------------- }
procedure TLbSCFileStream.Reinitialize(const Key; KeySize : Integer);
  {-reinitialize context and reposition to beginning of stream}
begin
  ChangeKey(Key, KeySize);
  Position := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TLbSCFileStream.ChangeKey(const Key; KeySize : Integer);
  {-reinitialize using a new key}
begin
  InitEncryptLSC(Key, KeySize, FContext);
end;
{ -------------------------------------------------------------------------- }
function TLbSCFileStream.Read(var Buffer; Count : LongInt) : LongInt;
  {-read Count bytes into Buffer, return bytes read}
begin
  Result := inherited Read(Buffer, Count);
  EncryptLSC(FContext, Buffer, Count);
end;
{ -------------------------------------------------------------------------- }
function TLbSCFileStream.Write(const Buffer; Count : LongInt) : LongInt;
  {-write Count bytes to Buffer, return bytes written}
var
  Buf : Pointer;
begin
  GetMem(Buf, Count);
  try
    Move(Buffer, Buf^, Count);
    EncryptLSC(FContext, Buf^, Count);
    Result := inherited Write(Buf^, Count);
  finally
    FreeMem(Buf, Count);
  end;
end;


{ == TLbRNG32Stream ======================================================== }
constructor TLbRNG32Stream.Create(const Key : LongInt);
  {-create the stream and initialize context}
begin
  inherited Create;

  Reinitialize(Key);
end;
{ -------------------------------------------------------------------------- }
procedure TLbRNG32Stream.Reinitialize(const Key : LongInt);
  {-reinitialize context and reposition to beginning of stream}
begin
  ChangeKey(Key);
  Position := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TLbRNG32Stream.ChangeKey(const Key : LongInt);
  {-reinitialize using a new key}
begin
  InitEncryptRNG32(Key, FContext);
end;
{ -------------------------------------------------------------------------- }
function TLbRNG32Stream.Read(var Buffer; Count : LongInt) : LongInt;
  {-read Count bytes into Buffer, return bytes read}
begin
  Result := inherited Read(Buffer, Count);
  EncryptRNG32(FContext, Buffer, Count);
end;
{ -------------------------------------------------------------------------- }
function TLbRNG32Stream.Write(const Buffer; Count : LongInt) : LongInt;
  {-write Count bytes to Buffer, return bytes written}
var
  Buf : Pointer;
begin
  GetMem(Buf, Count);
  try
    Move(Buffer, Buf^, Count);
    EncryptRNG32(FContext, Buf^, Count);
    Result := inherited Write(Buf^, Count);
  finally
    FreeMem(Buf, Count);
  end;
end;


{ == TLbRNG32FileStream ==================================================== }
constructor TLbRNG32FileStream.Create(const FileName : string; Mode : Word;
                                       const Key : LongInt);
  {-create the stream and initialize context}
begin
  inherited Create(FileName, Mode);

  Reinitialize(Key);
end;
{ -------------------------------------------------------------------------- }
procedure TLbRNG32FileStream.Reinitialize(const Key : LongInt);
  {-reinitialize context and reposition to beginning of stream}
begin
  ChangeKey(Key);
  Position := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TLbRNG32FileStream.ChangeKey(const Key : LongInt);
  {-reinitialize using a new key}
begin
  InitEncryptRNG32(Key, FContext);
end;
{ -------------------------------------------------------------------------- }
function TLbRNG32FileStream.Read(var Buffer; Count : LongInt) : LongInt;
  {-read Count bytes into Buffer, return bytes read}
begin
  Result := inherited Read(Buffer, Count);
  EncryptRNG32(FContext, Buffer, Count);
end;
{ -------------------------------------------------------------------------- }
function TLbRNG32FileStream.Write(const Buffer; Count : LongInt) : LongInt;
  {-write Count bytes to Buffer, return bytes written}
var
  Buf : Pointer;
begin
  GetMem(Buf, Count);
  try
    Move(Buffer, Buf^, Count);
    EncryptRNG32(FContext, Buf^, Count);
    Result := inherited Write(Buf^, Count);
  finally
    FreeMem(Buf, Count);
  end;
end;


{ == TLbRNG64Stream ======================================================== }
constructor TLbRNG64Stream.Create(const KeyHi, KeyLo : LongInt);
  {-create the stream and initialize context}
begin
  inherited Create;

  Reinitialize(KeyHi, KeyLo);
end;
{ -------------------------------------------------------------------------- }
procedure TLbRNG64Stream.Reinitialize(const KeyHi, KeyLo : LongInt);
  {-reinitialize context and reposition to beginning of stream}
begin
  ChangeKey(KeyHi, KeyLo);
  Position := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TLbRNG64Stream.ChangeKey(const KeyHi, KeyLo : LongInt);
  {-reinitialize using a new key}
begin
  InitEncryptRNG64(KeyHi, KeyLo, FContext);
end;
{ -------------------------------------------------------------------------- }
function TLbRNG64Stream.Read(var Buffer; Count : LongInt) : LongInt;
  {-read Count bytes into Buffer, return bytes read}
begin
  Result := inherited Read(Buffer, Count);
  EncryptRNG64(FContext, Buffer, Count);
end;
{ -------------------------------------------------------------------------- }
function TLbRNG64Stream.Write(const Buffer; Count : LongInt) : LongInt;
  {-write Count bytes to Buffer, return bytes written}
var
  Buf : Pointer;
begin
  GetMem(Buf, Count);
  try
    Move(Buffer, Buf^, Count);
    EncryptRNG64(FContext, Buf^, Count);
    Result := inherited Write(Buf^, Count);
  finally
    FreeMem(Buf, Count);
  end;
end;


{ == TLbRNG64FileStream ==================================================== }
constructor TLbRNG64FileStream.Create(const FileName : string; Mode : Word;
                                       const KeyHi, KeyLo : LongInt);
  {-create the stream and initialize context}
begin
  inherited Create(FileName, Mode);

  Reinitialize(KeyHi, KeyLo);
end;
{ -------------------------------------------------------------------------- }
procedure TLbRNG64FileStream.Reinitialize(const KeyHi, KeyLo : LongInt);
  {-reinitialize context and reposition to beginning of stream}
begin
  ChangeKey(KeyHi, KeyLo);
  Position := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TLbRNG64FileStream.ChangeKey(const KeyHi, KeyLo : LongInt);
  {-reinitialize using a new key}
begin
  InitEncryptRNG64(KeyHi, KeyLo, FContext);
end;
{ -------------------------------------------------------------------------- }
function TLbRNG64FileStream.Read(var Buffer; Count : LongInt) : LongInt;
  {-read Count bytes into Buffer, return bytes read}
begin
  Result := inherited Read(Buffer, Count);
  EncryptRNG64(FContext, Buffer, Count);
end;
{ -------------------------------------------------------------------------- }
function TLbRNG64FileStream.Write(const Buffer; Count : LongInt) : LongInt;
  {-write Count bytes to Buffer, return bytes written}
var
  Buf : Pointer;
begin
  GetMem(Buf, Count);
  try
    Move(Buffer, Buf^, Count);
    EncryptRNG64(FContext, Buf^, Count);
    Result := inherited Write(Buf^, Count);
  finally
    FreeMem(Buf, Count);
  end;
end;

end.
