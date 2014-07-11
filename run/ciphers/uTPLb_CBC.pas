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
unit uTPLb_CBC;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher;

type

TCBC = class( TInterfacedObject, IBlockChainingModel)
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

TCBCLink = class( TBlockChainLink)
  private
    FNextCV: TMemoryStream;

  public
    procedure Burn; override;
    function  Clone: TBlockChainLink; override;
    destructor Destroy; override;
    procedure Encrypt_Block( Plaintext{in}, Ciphertext{out}: TMemoryStream);  override;
    procedure Decrypt_Block( Plaintext{out}, Ciphertext{in}: TMemoryStream);  override;
  end;


implementation








uses uTPLb_StreamUtils, uTPLb_Constants, uTPLb_I18n;
{ TCBC }

function TCBC.ChainingFeatures: TChainingFeatureSet;
begin
result := []
end;

function TCBC.Chain_DecryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
  ): TBlockChainLink;
begin
result := TCBCLink.BaseCreate( Key, InitializationVector, Cipher);
TCBCLink( result).FNextCV := TMemoryStream.Create
end;



function TCBC.Chain_EncryptBlock( Key: TSymetricKey;
  InitializationVector: TMemoryStream; const Cipher: IBlockCodec
 ): TBlockChainLink;
begin
result := TCBCLink.BaseCreate( Key, InitializationVector, Cipher)
end;



function TCBC.DefinitionURL: string;
begin
result := 'http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf' +
 '|section 6.2'
end;



function TCBC.DisplayName: string;
begin
result := 'CBC'
end;



function TCBC.Features: TAlgorithmicFeatureSet;
begin
result := [afOpenSourceSoftware, afStar]
end;




function TCBC.ProgId: string;
begin
result := CBC_ProgId
end;





function TCBC.WikipediaReference: string;
begin
result := {'http://en.wikipedia.org/wiki/' +}
 'Cipher_block_chaining#Cipher-block_chaining_.28CBC.29'
end;



{ TCBCLink }

procedure TCBCLink.Burn;
begin
inherited;
if assigned( FNextCV) then
  BurnMemoryStream( FNextCV)
end;


destructor TCBCLink.Destroy;
begin
inherited;
FNextCV.Free
end;


function TCBCLink.Clone: TBlockChainLink;
begin
result := inherited Clone;
if assigned( FNextCV) then
  TCBCLink( result).FNextCV := CloneMemoryStream( FNextCV)
end;


procedure TCBCLink.Decrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
CopyMemoryStream( Ciphertext, FNextCV);
FCipher.Decrypt_Block( Plaintext, Ciphertext);
XOR_Streams2( Plaintext, FCV);  // plaintext := plaintext xor FCV
CopyMemoryStream( FNextCV, FCV)
end;





procedure TCBCLink.Encrypt_Block( Plaintext, Ciphertext: TMemoryStream);
begin
XOR_Streams2( FCV, Plaintext);  // FCV := FCV xor plaintext
FCipher.Encrypt_Block( FCV, Ciphertext);
CopyMemoryStream( Ciphertext, FCV)
end;

end.

