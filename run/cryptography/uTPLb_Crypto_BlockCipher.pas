{* ***** BEGIN LICENSE BLOCK *****
Copyright 2009 Sean B. Durkin

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
// Copyright 2009 Sean B. Durkin

unit uTPLb_Crypto_BlockCipher;
interface
uses Classes, uSBD_Crypto_StreamCipher;

type

IBlockCodec = interface
  ['{7E783A4E-EF17-4820-AB33-EF8EF9DA6F22}']
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);
    procedure Burn;
  end;

IBlockCipher = interface( ICryptoGraphicAlgorithm)
  ['{CB927B43-8A02-4332-B844-A174D1D6B705}']
    function  GenerateKey( Seed: TStream): TSymetricKey;
    function  BlockSize: integer;  // in units of bits. Must be a multiple of 8.
    function  SeedByteSize: integer; // Size that the input of the GenerateKey must be.

    function  MakeBlockCodec( Key: TSymetricKey; const Params: IInterface): IBlockCodec;
    end;

TBlockChainLink = class
  protected
    FKey: TSymetricKey;
    FCV: TMemoryStream;
    FCipher: IBlockCipher;

    constructor BaseCreate(
      Key1: TSymetricKey;   // Will be referenced, not cloned.
      IV1: TMemoryStream;   // Will be cloned.
      Cipher1: IBlockCipher);    // Will be referenced, not cloned.

  protected
    procedure Burn; virtual;

  public
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  virtual; abstract;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  virtual; abstract;

    destructor Destroy; override;
  end;


IBlockChainingModel = interface( ICryptoGraphicAlgorithm)
  ['{7ED854DF-5270-41F7-820A-65BF9B5E1D35}']

  function Chain_EncryptBlock(
    Key: TSymetricKey; InitializationVector: TMemoryStream;
    const Cipher: IBlockCipher; const Param: IInterface): TBlockChainLink;

  function Chain_DecryptBlock(
    Key: TSymetricKey; InitializationVector: TMemoryStream;
    const Cipher: IBlockCipher; const Param: IInterface): TBlockChainLink;
  end;



IBlockCipherSelector = interface
  ['{B08F766E-1EB0-4BA0-9C84-8AF02E13B24C}']
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
  end;

  

implementation






uses uTPLb_StreamUtils;

{ TBlockChainLink }

constructor TBlockChainLink.BaseCreate(
  Key1: TSymetricKey; IV1: TMemoryStream; Cipher1: IBlockCipher);
begin
FKey    := Key1; // Not owned
FCV     := CloneMemoryStream( IV1); // Owned.
FCipher := Cipher1    // Not owned
end;

procedure TBlockChainLink.Burn;
begin
BurnMemoryStream( FCV);
FKey := nil // key is not owned by the chain link.
end;


destructor TBlockChainLink.Destroy;
begin
FCV.Free;
inherited;
end;

end.
