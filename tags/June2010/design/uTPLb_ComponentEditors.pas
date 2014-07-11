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
unit uTPLb_ComponentEditors;
interface


procedure Register;

implementation











uses Classes, TypInfo, DesignIntf, DesignEditors, VCLEditors, SysUtils,
     uTPLb_Hash, uTPLb_CryptographicLibrary, uTPLb_ComponentAbout,
     uTPLb_BaseNonVisualComponent, uTPLb_HashDsc, uTPLb_Codec,
     uTPLb_BlockCipher;

type
TLockBoxEditor = class( TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


THashEditor2 = class( TStringProperty, IProperty)
  protected
    function GetEditValue2( out Value: string): Boolean;  virtual;
    function IProperty.GetEditValue = GetEditValue2;
    function SharedNonEmptyLibrary: Boolean;

  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue( const Value: string); override;
  end;

TCipherEditor = class( TStringProperty, IProperty)
  protected
    function GetEditValue2( out Value: string): Boolean;  virtual;
    function IProperty.GetEditValue = GetEditValue2;
    function SharedNonEmptyLibrary: Boolean;

  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue( const Value: string); override;
  end;

TChainEditor = class( TCipherEditor)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue( const Value: string); override;
  end;


procedure Register;
begin
RegisterPropertyEditor( TypeInfo( string), TTPLb_BaseNonVisualComponent, 'About', TLockBoxEditor);
RegisterPropertyEditor( TypeInfo( string), THash, 'Hash', THashEditor2);
RegisterPropertyEditor( TypeInfo( string), TCodec, 'Cipher', TCipherEditor);
RegisterPropertyEditor( TypeInfo( string), TCodec, 'ChainMode', TChainEditor)
end;




{ THashEditor2 }

function THashEditor2.GetAttributes: TPropertyAttributes;
begin
result := [ paValueList, paSortList, paMultiSelect,
            paReadOnly, paRevertable, paValueEditable]
end;


function THashEditor2.SharedNonEmptyLibrary: Boolean;
var
  HashComp: THash;
  Lib, Lib2: TCryptographicLibrary;
  j: integer;
begin
result := False;
Lib2 := nil;
if PropCount >= 1 then
  begin
  if GetComponent( 0) is THash then
      HashComp := THash( GetComponent( 0))
    else
      HashComp := nil;
  if assigned( HashComp) then
      Lib := HashComp.CryptoLibrary
    else
      Lib := nil;
  result := assigned( Lib);
  if not result then exit;
  for j := 1 to PropCount - 1 do
    begin
    if GetComponent( j) is THash then
        HashComp := THash( GetComponent( 0))
      else
        HashComp := nil;
    if assigned( HashComp) then
        Lib2 := HashComp.CryptoLibrary
      else
        Lib := nil;
    result := Lib = Lib2;
    if not result then break
    end
  end
end;




function THashEditor2.GetEditValue2( out Value: string): Boolean;
begin
result := GetEditValue( Value);
if not result then exit;
result := SharedNonEmptyLibrary;
if not result then
  Value := '(Libraries mixed or not set)'
end;



procedure THashEditor2.GetValues( Proc: TGetStrProc);
var
  HashComp: THash;
  Lib: TCryptographicLibrary;
  j: integer;
  HashChoices: IInterfaceList;
  Hash: IHashDsc;
begin
if (PropCount >= 1) and (GetComponent( 0) is THash) then
      HashComp := THash( GetComponent( 0))
    else
      HashComp := nil;
if assigned( HashComp) then
    Lib := HashComp.CryptoLibrary
  else
    Lib := nil;
if not assigned( Lib) then exit;
HashChoices := Lib.GetHashChoices;
for j := 0 to HashChoices.Count - 1 do
  if Supports( HashChoices[j], IHashDsc, Hash) then
    Proc( Lib.ComputeHashDisplayName( Hash))
end;



procedure THashEditor2.SetValue( const Value: string);
var
  HashComp: THash;
  Lib: TCryptographicLibrary;
  j: integer;
  ProgId: string;
  HashChoices: IInterfaceList;
  Hash: IHashDsc;
begin
if not SharedNonEmptyLibrary then exit;
if (PropCount >= 1) and (GetComponent( 0) is THash) then
      HashComp := THash( GetComponent( 0))
    else
      HashComp := nil;
if assigned( HashComp) then
    Lib := HashComp.CryptoLibrary
  else
    Lib := nil;
ProgId := '';
HashChoices := Lib.GetHashChoices;
if assigned( Lib) then
  for j := 0 to HashChoices.Count - 1 do
    if Supports( HashChoices[j], IHashDsc, Hash) then
      begin
      if Value <> Lib.ComputeHashDisplayName( Hash) then continue;
      ProgId := Hash.ProgId;
      break
      end;
if ProgId = '' then exit;
for j := 0 to PropCount - 1 do
  begin
  if GetComponent( j) is THash then
      HashComp := THash( GetComponent( j))
    else
      HashComp := nil;
  HashComp.HashId := ProgId
  end;
Modified
end;

{ TLockBoxEditor }

procedure TLockBoxEditor.Edit;
var
  doCreate: boolean;
  SelectedComponent: TComponent;
begin
doCreate := not assigned( TPLb_fmComponentAbout);
if doCreate then
  TPLb_fmComponentAbout := TTPLb_fmComponentAbout.Create( nil);
try
  if (PropCount >= 1) and (GetComponent( 0) is TComponent) then
      SelectedComponent := TComponent( GetComponent( 0))
    else
      SelectedComponent := nil;
  TPLb_fmComponentAbout.UpdateAbout( SelectedComponent);
  if not doCreate then
    begin
    TPLb_fmComponentAbout.Show;
    TPLb_fmComponentAbout.BringToFront
    end;
  TPLb_fmComponentAbout.ShowModal
finally
if doCreate then
  begin
  TPLb_fmComponentAbout.Release;
  TPLb_fmComponentAbout := nil
  end
end end;




function TLockBoxEditor.GetAttributes: TPropertyAttributes;
begin
result := [paDialog, paReadOnly, paMultiSelect]
end;

{ TCipherEditor }

function TCipherEditor.GetAttributes: TPropertyAttributes;
begin
result := [ paValueList, paSortList, paMultiSelect,
            paReadOnly, paRevertable, paValueEditable]
end;

function TCipherEditor.GetEditValue2( out Value: string): Boolean;
begin
result := GetEditValue( Value);
if not result then exit;
result := SharedNonEmptyLibrary;
if not result then
  Value := '(Libraries mixed or not set)'
end;


procedure TCipherEditor.GetValues( Proc: TGetStrProc);
var
  CodecComp: TCodec;
  Lib: TCryptographicLibrary;
  j: integer;
  CipherChoices: IInterfaceList;
  CipherChoice: ICipherChoice;
  CipherDisplayName: string;
  isBlockCipher: boolean;
  StreamCipherId: string;
  BlockCipherId: string;
begin
if (PropCount >= 1) and (GetComponent( 0) is TCodec) then
      CodecComp := TCodec( GetComponent( 0))
    else
      CodecComp := nil;
if assigned( CodecComp) then
    Lib := CodecComp.CryptoLibrary
  else
    Lib := nil;
if not assigned( Lib) then exit;
CipherChoices := Lib.GetCipherChoices;
for j := 0 to CipherChoices.Count - 1 do
  if Supports( CipherChoices[j], ICipherChoice, CipherChoice) then
    begin
    CipherChoice.GetChoiceParams(
      CipherDisplayName, isBlockCipher, StreamCipherId, BlockCipherId);
    Proc( CipherDisplayName)
    end
end;


procedure TCipherEditor.SetValue( const Value: string);
var
  CodecComp: TCodec;
  Lib: TCryptographicLibrary;
  j: integer;
  ProgId: string;
  CipherChoices: IInterfaceList;
  CipherChoice: ICipherChoice;
  CipherDisplayName: string;
  isBlockCipher: boolean;
  StreamCipherId: string;
  BlockCipherId: string;
  Found: boolean;
begin
if not SharedNonEmptyLibrary then exit;
if (PropCount >= 1) and (GetComponent( 0) is TCodec) then
      CodecComp := TCodec( GetComponent( 0))
    else
      CodecComp := nil;
if assigned( CodecComp) then
    Lib := CodecComp.CryptoLibrary
  else
    Lib := nil;
ProgId := '';
CipherChoices := Lib.GetCipherChoices;
Found := False;
if assigned( Lib) then
  for j := 0 to CipherChoices.Count - 1 do
    if Supports( CipherChoices[j], ICipherChoice, CipherChoice) then
      begin
      CipherChoice.GetChoiceParams(
        CipherDisplayName, isBlockCipher, StreamCipherId, BlockCipherId);
      if Value <> CipherDisplayName then continue;
      Found := True;
      break
      end;
if not Found then exit;
for j := 0 to PropCount - 1 do
  begin
  if GetComponent( j) is TCodec then
      CodecComp := TCodec( GetComponent( j))
    else
      CodecComp := nil;
  CodecComp.StreamCipherId := StreamCipherId;
  CodecComp.BlockCipherId  := BlockCipherId
  end;
Modified
end;



function TCipherEditor.SharedNonEmptyLibrary: Boolean;
var
  CodecComp: TCodec;
  Lib, Lib2: TCryptographicLibrary;
  j: integer;
begin
result := False;
Lib2 := nil;
if PropCount >= 1 then
  begin
  if GetComponent( 0) is TCodec then
      CodecComp := TCodec( GetComponent( 0))
    else
      CodecComp := nil;
  if assigned( CodecComp) then
      Lib := CodecComp.CryptoLibrary
    else
      Lib := nil;
  result := assigned( Lib);
  if not result then exit;
  for j := 1 to PropCount - 1 do
    begin
    if GetComponent( j) is TCodec then
        CodecComp := TCodec( GetComponent( 0))
      else
        CodecComp := nil;
    if assigned( CodecComp) then
        Lib2 := CodecComp.CryptoLibrary
      else
        Lib := nil;
    result := Lib = Lib2;
    if not result then break
    end
  end
end;

{ TChainEditor }

procedure TChainEditor.GetValues( Proc: TGetStrProc);
var
  CodecComp: TCodec;
  Lib: TCryptographicLibrary;
  j: integer;
  ChainChoices: IInterfaceList;
  ChainChoice: IBlockChainingModel;
begin
if (PropCount >= 1) and (GetComponent( 0) is TCodec) then
      CodecComp := TCodec( GetComponent( 0))
    else
      CodecComp := nil;
if assigned( CodecComp) then
    Lib := CodecComp.CryptoLibrary
  else
    Lib := nil;
if not assigned( Lib) then exit;
ChainChoices := Lib.GetChainChoices;
for j := 0 to ChainChoices.Count - 1 do
  if Supports( ChainChoices[j], IBlockChainingModel, ChainChoice) then
    Proc( Lib.ComputeChainDisplayName( ChainChoice))
end;




procedure TChainEditor.SetValue( const Value: string);
var
  CodecComp: TCodec;
  Lib: TCryptographicLibrary;
  j: integer;
  ChainChoices: IInterfaceList;
  ChainChoice: IBlockChainingModel;
  Found: boolean;
begin
if not SharedNonEmptyLibrary then exit;
if (PropCount >= 1) and (GetComponent( 0) is TCodec) then
      CodecComp := TCodec( GetComponent( 0))
    else
      CodecComp := nil;
if assigned( CodecComp) then
    Lib := CodecComp.CryptoLibrary
  else
    Lib := nil;
ChainChoices := Lib.GetChainChoices;
Found := False;
if assigned( Lib) then
  for j := 0 to ChainChoices.Count - 1 do
    if Supports( ChainChoices[j], IBlockChainingModel, ChainChoice) then
      begin
      if Value <> Lib.ComputeChainDisplayName( ChainChoice) then continue;
      Found := True;
      break
      end;
if not Found then exit;
for j := 0 to PropCount - 1 do
  begin
  if GetComponent( j) is TCodec then
      CodecComp := TCodec( GetComponent( j))
    else
      CodecComp := nil;
  CodecComp.ChainModeId := ChainChoice.ProgId
  end;
Modified
end;

end.