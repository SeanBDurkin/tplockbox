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
unit uTPLb_OFB;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher;

type

TOFB = class( TInterfacedObject, IBlockChainingModel)
  protected
    function Chain_EncryptBlock(
      Key: TSymetricKey; InitializationVector: TMemoryStream;
      const Cipher: IBlockCodec): TBlockChainLink;

    function Chain_DecryptBlock(
      Key: TSymetricKey; InitializationVector: TMemoryStream;
      const Cipher: IBlockCodec): TBlockChainLink;

    function  DisplayName: string;
    function  ProgId: string;
    function  Features: TAlgorithmicFeatureSet;
    function  ChainingFeatures: TChainingFeatureSet;
    function  DefinitionURL: string;
    function  WikipediaReference: string;
  end;

TOFBLink = class( TBlockChainLink)
  public
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  override;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  override;
  end;


implementation




uses uTPLb_StreamUtils;

{ TOFB }

function TOFB.ChainingFeatures: TChainingFeatureSet;
begin
result := [cfKeyStream, cfAutoXOR]
end;


function TOFB.Chain_DecryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TOFBLink.BaseCreate( Key, InitializationVector, Cipher)
end;



function TOFB.Chain_EncryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TOFBLink.BaseCreate( Key, InitializationVector, Cipher)
end;



function TOFB.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf' +
 '|section 6.4'
end;

function TOFB.DisplayName: string;
begin
result := 'OFB'
end;



function TOFB.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware, afCryptographicallyWeak]
end;


function TOFB.ProgId: string;
begin
result := 'native.OFB'
end;





function TOFB.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +}
 'Block_cipher_modes_of_operation#Output_feedback_.28OFB.29'
end;

{ TOFBLink }

procedure TOFBLink.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
FCipher.Encrypt_Block( FCV, Ciphertext);
// The plaintext block will automatically be xored with the product
//  of the above by virture of the cfAutoXOR feature.
CopyMemoryStream( Ciphertext, FCV)
end;


procedure TOFBLink.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
FCipher.Encrypt_Block( FCV, Plaintext);
// The ciphertext block will automatically be xored with the product
//  of the above by virture of the cfAutoXOR feature.
CopyMemoryStream( Plaintext, FCV)
end;




end.

