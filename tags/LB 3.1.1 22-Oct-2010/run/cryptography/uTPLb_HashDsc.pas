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
unit uTPLb_HashDsc;
interface
uses Classes, uTPLb_StreamCipher;

type

IHasher = interface
  ['{982870E4-EC9B-48CD-B882-17F58F0A7D1A}']
  procedure  Update( Source{in}: TMemoryStream);
    // Source length in bytes must be EXACTLY the UpdateSize/8 .
  procedure  End_Hash( PartBlock{in}: TMemoryStream; Digest: TStream);
    // PartBlock is the final source content. The length in bytes of the
    //  payload is indicated by the stream position (not stream size). And this
    //  must be less than or equal to UpdateSize/8. It may be zero.
    // It is the responsibility of the client to set initializer the
    //  Digest position and size prior to invocation of End_Hash.
    // End_Hash simply writes DigestSize/8 bytes to the Digest stream
    //  from its current position.
  procedure  Burn;
  function   SelfTest_Source: ansistring; // Bigendien hex string, oriented into u32 groups.;
  function   SelfTest_ReferenceHashValue: ansistring; // as above
  end;

IHashDsc = interface( ICryptoGraphicAlgorithm)
  // Hash descriptor. Describes a hashing algorithm.
  ['{A3922AFC-C917-4364-9FD1-FD84A3E37558}']
    function  DigestSize: integer;  // in units of bits. Must be a multiple of 8.
    function  UpdateSize: integer; // Size that the input to the Update must be.

    function  MakeHasher( const Params: IInterface): IHasher;
    end;


implementation

end.
