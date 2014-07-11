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
unit uTPLb_ECB;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher;

type

TECB = class( TInterfacedObject, IBlockChainingModel)
  protected
    function Chain_EncryptBlock(
      Key: TSymetricKey; InitializationVector: TMemoryStream;
      const Cipher: IBlockCodec): TBlockChainLink;

    function Chain_DecryptBlock(
      Key: TSymetricKey; InitializationVector: TMemoryStream;
      const Cipher: IBlockCodec): TBlockChainLink;

    function  DisplayName: string;
    function  ProgId: string;                              virtual;
    function  ChainingFeatures: TChainingFeatureSet;       virtual;
    function  Features: TAlgorithmicFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
  end;

TECBLink = class( TBlockChainLink)
  public
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  override;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  override;
  end;


implementation




{ TECB }

function TECB.ChainingFeatures: TChainingFeatureSet;
begin
result := [cfNoNounce]
end;

function TECB.Chain_DecryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TECBLink.BaseCreate( Key, InitializationVector, Cipher)
end;



function TECB.Chain_EncryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TECBLink.BaseCreate( Key, InitializationVector, Cipher)
end;



function TECB.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf' +
 '|section 6.1'
end;



function TECB.DisplayName: string;
begin
result := 'ECB (with block padding)'
end;



function TECB.Features: TAlgorithmicFeatureSet;
begin
result := [afCryptographicallyWeak, afOpenSourceSoftware]
end;



function TECB.ProgId: string;
begin
result := 'native.ECB'
end;





function TECB.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +}
 'Block_cipher_modes_of_operation#Electronic_codebook_.28ECB.29'
end;



{ TECBLink }

procedure TECBLink.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
FCipher.Decrypt_Block( Plaintext, Ciphertext)
end;



procedure TECBLink.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
FCipher.Encrypt_Block( Plaintext, Ciphertext)
end;

end.
