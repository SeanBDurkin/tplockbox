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
unit uTPLb_Signatory;
interface
uses Classes, uTPLb_BaseNonVisualComponent, uTPLb_Codec, uTPLb_Asymetric;

type
TSignatory = class( TTPLb_BaseNonVisualComponent)
  private
    FCodec: TCodec;
    FCryptoKeys : TAsymetricKeyPair;
    FSigningKeys: TAsymetricKeyPair;

    procedure SetCodec( Value: TCodec);

  protected
    procedure Notification(
      AComponent: TComponent; Operation: TOperation); override;

  public
    destructor Destroy; override;
    procedure Sign( Document, Signature: TStream);
    function  Verify( Document, Signature: TStream): boolean;

    procedure GenerateKeys;
    procedure LoadKeysFromStream( Store: TStream);
    procedure Burn;

    property Codec: TCodec     read FCodec write SetCodec;
  end;

implementation










{ TSignatory }

procedure TSignatory.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
inherited;
if (Operation = opRemove) and (AComponent = FCodec) then
  SetCodec( nil)
end;



procedure TSignatory.SetCodec( Value: TCodec);
begin
if FCodec = Value then exit;
if assigned( FCodec) then
  FCodec.RemoveFreeNotification( self);
FCodec := Value;
if assigned( FCodec) then
  FCodec.FreeNotification( self)
end;



destructor TSignatory.Destroy;
begin
SetCodec( nil);
FCryptoKeys.Free;
FSigningKeys.Free;
inherited
end;


procedure TSignatory.Burn;
begin

end;

procedure TSignatory.GenerateKeys;
begin

end;

procedure TSignatory.LoadKeysFromStream( Store: TStream);
begin

end;


procedure TSignatory.Sign( Document, Signature: TStream);
begin

end;

function TSignatory.Verify( Document, Signature: TStream): boolean;
begin

end;

end.
