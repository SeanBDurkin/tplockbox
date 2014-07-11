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
uses uTPLb_StreamCipher, uTPLb_BlockCipher, Classes;

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


TXXTEA_LargeBlock_LE_Encryptor = class( TInterfacedObject, IStreamEncryptor)
  public
    constructor Create( Owner1: TXXTEA_LargeBlock; Key: TXXTEA_LE_Key; CipherText: TStream);
    destructor  Destroy; override;

  private
    procedure Encrypt( const Plaintext: TStream);
    procedure End_Encrypt;
    procedure Reset;
  end;


TXXTEA_LargeBlock_LE_Decryptor = class( TInterfacedObject, IStreamDecryptor)
  public
    constructor Create( Owner1: TXXTEA_LargeBlock; Key: TXXTEA_LE_Key; CipherText: TStream);
    destructor  Destroy; override;

  private
    procedure Decrypt( const PlainTex: TStream);
    procedure End_Decrypt;
    procedure Reset;
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

procedure TXXTEA_LargeBlock_LE_Decryptor.Reset;
begin
// tbd
end;

end.
