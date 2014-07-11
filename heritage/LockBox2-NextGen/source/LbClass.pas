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
{*                   LBCLASS.PAS 2.08                    *}
{*     Copyright (c) 2002 TurboPower Software Co         *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I LockBox.inc}

{$H+}  {turn on huge strings}

unit LbClass;
  {-LockBox components and classes }

interface

uses
  Classes,
  SysUtils,
  LbCipher;

{ TLbBaseComponent }
type
  TLBBaseComponent = class(TLBBase)
  protected
    function GetVersion : string;
    procedure SetVersion(const Value : string);
  published
    property Version : string
      read GetVersion write SetVersion stored False;
  end;


{ TLbCipher }
type
  TLbCipherMode = (cmECB, cmCBC);

  TLbCipher = class(TLbBaseComponent)
  private
    FEncoding: TEncoding;
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
    function  EncryptString(const InString: string): string; virtual; abstract;

    function OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal; virtual; abstract;
    property Encoding: TEncoding read FEncoding write FEncoding;
  end;


{ TLbSymmetricCipher }
type
  TLbSymmetricCipher = class(TLbCipher)
  protected {private}
    FCipherMode : TLbCipherMode;
  public {methods}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure GenerateKey(const Passphrase: string); virtual; abstract;
    procedure GenerateRandomKey; virtual; abstract;
  public {properties}
    property CipherMode : TLbCipherMode read FCipherMode write FCipherMode;
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
    function DecryptString(const InString : string) : string; override;

    procedure EncryptFile(const InFile, OutFile : string); override;
    procedure EncryptStream(InStream, OutStream : TStream); override;
    function  EncryptString(const InString : string) : string; override;

    procedure GenerateKey(const Passphrase: string); override;
    procedure GenerateRandomKey; override;

    procedure GetKey(var Key : TKey128);
    procedure SetKey(const Key : TKey128);

    function OutBufSizeNeeded(InBufSize : Cardinal) : Cardinal; override;

  published {properties}
    property CipherMode;
  end;


implementation

uses
  LbProc, LbString, LbConst;

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
  FEncoding := TEncoding.ANSI;
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
function TLbBlowfish.DecryptString(const InString : string): string;
begin
  case CipherMode of
    cmECB : Result := BFEncryptStringEx(FEncoding.GetBytes(InString), FKey, False, FEncoding);
    cmCBC : Result := BFEncryptStringCBCEx(FEncoding.GetBytes(InString), FKey, False, FEncoding);
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
    cmECB : Result := BFEncryptStringEx(FEncoding.GetBytes(InString), FKey, True, FEncoding);
    cmCBC : Result := BFEncryptStringCBCEx(FEncoding.GetBytes(InString), FKey, True, FEncoding);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TLbBlowfish.GenerateKey(const Passphrase: string);
begin
  GenerateLMDKey(FKey, SizeOf(FKey), FEncoding.GetBytes(Passphrase));
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






end.
