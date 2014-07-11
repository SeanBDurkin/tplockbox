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

unit uTPLb_3DES;
interface
uses Classes, uTPLb_BlockCipher, uTPLb_StreamCipher;

type

T3DES = class( TInterfacedObject,
    IBlockCipher, ICryptoGraphicAlgorithm)
  private
    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;
    function  BlockSize: integer;  // in units of bits. Must be a multiple of 8.
    function  KeySize: integer;
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.
    function  MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
    function  SelfTest_Key: ansistring;
    function  SelfTest_Plaintext: ansistring;
    function  SelfTest_Ciphertext: ansistring;

  public
    constructor Create;
  end;




implementation




uses uTPLb_Constants, uTPLb_DES, SysUtils, uTPLb_I18n;
{ TDES }


type
T3DESKey = class( TSymetricKey)
  private
    FNativeKey1, FNativeKey2: uint64;
    FExpandedKey1, FExpandedKey2: TExpandedKey;

  public
    constructor Create( NativeKey1, NativeKey2: uint64);
    constructor CreateFromStream( Store: TStream);

    procedure   SaveToStream( Stream: TStream);  override;
    procedure   Burn;                            override;
  end;



T3DESCodec = class( TInterfacedObject, IBlockCodec)
  private
    FLockBoxKey: T3DESKey;

    constructor Create( LockBoxKey1: T3DESKey);
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Reset;
    procedure Burn;
  end;


function T3DES.BlockSize: integer;
begin
result := 64
end;

constructor T3DES.Create;
begin
end;

function T3DES.DefinitionURL: string;
begin
result := ''
end;

function T3DES.DisplayName: string;
begin
result := '3DES (Keying option 2)'
end;

function T3DES.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware]
end;


function T3DES.GenerateKey( Seed: TStream): TSymetricKey;
var
  SeedKey1, SeedKey2: uint64;
begin
Seed.ReadBuffer( SeedKey1, 8);
Seed.ReadBuffer( SeedKey2, 8);
result := T3DESKey.Create( SeedKey1, SeedKey2)
end;

function T3DES.KeySize: integer;
begin
result := 80
// Quote from wikipedia:
// Keying option 2 reduces the key size to 112 bits. However, this option
//  is susceptible to certain chosen-plaintext or known-plaintext attacks
//  and thus it is designated by NIST to have only 80 bits of security.
end;

function T3DES.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := T3DESKey.CreateFromStream( Store)
end;


function T3DES.MakeBlockCodec( Key: TSymetricKey): IBlockCodec;
begin
result := T3DESCodec.Create( Key as T3DESKey)
end;

function T3DES.ProgId: string;
begin
result := TripleDES_ProgId
end;

function T3DES.SeedByteSize: integer;
begin
result := 16
end;

function T3DES.SelfTest_Ciphertext: ansistring;
begin
result := ''
end;

function T3DES.SelfTest_Key: ansistring;
begin
result := ''
end;

function T3DES.SelfTest_Plaintext: ansistring;
begin
result := ''
end;

function T3DES.WikipediaReference: string;
begin
result := 'Triple_DES'
end;

{ T3DESKey }

constructor T3DESKey.Create( NativeKey1, NativeKey2: uint64);
begin
FNativeKey1 := NativeKey1;
FNativeKey2 := NativeKey2;
SetParityBitsOnKey( FNativeKey1);
SetParityBitsOnKey( FNativeKey2);
ExpandKey( FNativeKey1, FExpandedKey1);
ExpandKey( FNativeKey2, FExpandedKey2)
end;


constructor T3DESKey.CreateFromStream( Store: TStream);
begin
Store.ReadBuffer( FNativeKey1, 8);
Store.ReadBuffer( FNativeKey2, 8);
if (not hasCorrectParity( FNativeKey1)) or
   (not hasCorrectParity( FNativeKey2)) then
  raise Exception.Create( ES_InvalidKey);
ExpandKey( FNativeKey1, FExpandedKey1);
ExpandKey( FNativeKey2, FExpandedKey2)
end;



procedure T3DESKey.SaveToStream( Stream: TStream);
begin
Stream.Write( FNativeKey1, 8);
Stream.Write( FNativeKey2, 8)
end;

procedure T3DESKey.Burn;
begin
FNativeKey1 := 0;
FillChar( FExpandedKey1, SizeOf( FExpandedKey1), 0);
FNativeKey2 := 0;
FillChar( FExpandedKey2, SizeOf( FExpandedKey2), 0)
end;


{ T3DESCodec }

constructor T3DESCodec.Create( LockBoxKey1: T3DESKey);
begin
FLockBoxKey := LockBoxKey1
end;



procedure T3DESCodec.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
var
  PlaintextBlock, CiphertextBlock: uint64;
begin
Move( Plaintext.Memory^, PlaintextBlock, 8);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey.FExpandedKey1);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey.FExpandedKey2);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey.FExpandedKey1);
Move( CiphertextBlock, Ciphertext.Memory^, 8)
end;



procedure T3DESCodec.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
var
  PlaintextBlock, CiphertextBlock: uint64;
begin
Move( Ciphertext.Memory^, CiphertextBlock, 8);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey.FExpandedKey1);
DES_EncryptBlock( PlaintextBlock, CiphertextBlock, FLockBoxKey.FExpandedKey2);
DES_DecryptBlock( CiphertextBlock, PlaintextBlock, FLockBoxKey.FExpandedKey1);
Move( PlaintextBlock, Plaintext.Memory^, 8)
end;


procedure T3DESCodec.Reset;
begin
end;


procedure T3DESCodec.Burn;
begin
end;


end.
