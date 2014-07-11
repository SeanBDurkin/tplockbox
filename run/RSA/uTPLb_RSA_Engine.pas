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
unit uTPLb_RSA_Engine;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_Asymetric, uTPLb_Codec, uTPLb_CodecIntf,
     uTPLb_HugeCardinal, uTPLb_MemoryStreamPool;

type
TRSA_Engine = class( TAsymetric_Engine)
  protected
    // ICryptoGraphicAlgorithm
    function  DisplayName: string;                  override;
    function  ProgId: string;                       override;
    function  Features: TAlgorithmicFeatureSet;     override;
    function  DefinitionURL: string;                override;
    function  WikipediaReference: string;           override;

    // IStreamCipher = interface( )
    function  LoadKeyFromStream( Store: TStream): TSymetricKey;     override;

    function  AsymetricKeyPairClass: TAsymetricKeyPairClass; override;
    function  EncClass: TAsymetricEncryptorClass; override;
    function  DecClass: TAsymetricDecryptorClass; override;

  public
    procedure GenerateAsymetricKeyPair(
      KeySizeInBits: cardinal;
      ProgressSender: TObject;
      ProgressEvent: TGenerateAsymetricKeyPairProgress;
      var KeyPair: TAsymetricKeyPair; var wasAborted: boolean);
                                                        override;

    function  CreateFromStream( Store: TStream; Parts: TKeyStoragePartSet):
      TAsymetricKeyPair;                                override;
  end;


TRSAKeyPair = class;
TRSAKeyPart = class( TAsymtricKeyPart)
  protected
    F_RSA_n: THugeCardinal;  // Non-Owning.
    FOwner: TRSAKeyPair;
    function  NominalKeyBitLength: cardinal;   override;
  public
    procedure SaveToStream  ( Store: TStream); override;
    procedure LoadFromStream( Store: TStream); override;
    procedure Burn;                            override;
  end;

TRSA_PublicKeyPart = class( TRSAKeyPart)
  public
    F_RSA_e: THugeCardinal;  // Non-Owning.
    procedure SaveToStream  ( Store: TStream); override;
    procedure LoadFromStream( Store: TStream); override;
    procedure Burn;                            override;
    function  isEmpty: boolean;                override;
  end;

TRSA_PrivateKeyPart = class( TRSAKeyPart)
  public
    F_RSA_d: THugeCardinal;  // Non-Owning.
    procedure SaveToStream  ( Store: TStream); override;
    procedure LoadFromStream( Store: TStream); override;
    procedure Burn;                            override;
    function  isEmpty: boolean;                override;
  end;

TRSAKeyPair = class( TAsymetricKeyPair)
  private
    procedure LinkParts;

  protected
    FPool: TMemoryStreamPool;
    function StoreHugeCardinal(
      Number: THugeCardinal; StoreStream: TStream): boolean; virtual;
    function LoadHugeCardinal_IfNotAlready(
      StoreStream: TStream; var Number: THugeCardinal): boolean; virtual;

  public
    F_RSA_n, F_RSA_d, F_RSA_e: THugeCardinal; // Owning.

    constructor CreateEmpty;                   override;
    destructor  Destroy;                       override;
    procedure   LoadFromStream( Store: TStream; Parts: TKeyStoragePartSet); override;
    procedure   StoreToStream( Store: TStream; Parts: TKeyStoragePartSet); override;
    procedure   Burn;                          override;
  end;

TRSA_Encryptor = class( TAsymetricEncryptor)
  public
    function  GenerateSymetricKey: TSymetricKey;     override;
    function  VerifySignature(
      Document: TStream;       // FCipherText is the signature to be verified.
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean): boolean;             override;
  end;

TRSA_Decryptor = class( TAsymetricDecryptor)
  public
    function  LoadSymetricKey( Ciphertext: TStream): TSymetricKey;     override;
    procedure Sign(           // FPlaintext is the document to be signed.
      Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean);                      override;
  end;

implementation


















uses uTPLb_RSA_Primitives, SysUtils, Math, uTPLb_HugeCardinalUtils,
     uTPLB_Constants, uTPLb_I18n;

{ TRSA_Engine }

function TRSA_Engine.AsymetricKeyPairClass: TAsymetricKeyPairClass;
begin
result := TRSAKeyPair
end;

function TRSA_Engine.DecClass: TAsymetricDecryptorClass;
begin
result := TRSA_Decryptor
end;

function TRSA_Engine.DefinitionURL: string;
begin
result := 'http://www.ietf.org/rfc/rfc3447.txt'
end;


function TRSA_Engine.DisplayName: string;
begin
result := RSA_DisplayName
end;






function TRSA_Engine.EncClass: TAsymetricEncryptorClass;
begin
result := TRSA_Encryptor
end;



function TRSA_Engine.Features: TAlgorithmicFeatureSet;
begin
result := (inherited Features);
Include( result, afStar)
end;



type TRSA_Gen_Key_Helper = class
  private
    FPool: TMemoryStreamPool;
    FdoOwn: boolean;
    FNumbersTested: integer;
    FClient_ProgressEvent: TGenerateAsymetricKeyPairProgress;

    procedure Progress_Event( Sender: TObject; BitsProcessed,
         TotalBits: int64; var doAbort: boolean);
    procedure PrimalityTest_Event( CountPrimalityTests: integer);

  public
    constructor Create(
      Client_ProgressEvent1: TGenerateAsymetricKeyPairProgress;
      Pool1: TMemoryStreamPool);
    destructor Destroy; override;
  end;

procedure TRSA_Engine.GenerateAsymetricKeyPair(
  KeySizeInBits: cardinal; ProgressSender: TObject;
  ProgressEvent: TGenerateAsymetricKeyPairProgress;
  var KeyPair: TAsymetricKeyPair; var wasAborted: boolean);
var
  RSAKeyPair: TRSAKeyPair;
  N, e, d, Totient: THugeCardinal;
  GeneratePrimePassCount: integer; // 1 .. 20;
  Helper: TRSA_Gen_Key_Helper;
begin
RSAKeyPair := AsymetricKeyPairClass.CreateEmpty as TRSAKeyPair;

GeneratePrimePassCount := 5;
Helper := TRSA_Gen_Key_Helper.Create( ProgressEvent, RSAKeyPair.FPool);

try
uTPLb_HugeCardinalUtils.Compute_RSA_Fundamentals_2Factors(
  KeySizeInBits, StandardExponent, N, e, d, Totient,
  Helper.Progress_Event, Helper.PrimalityTest_Event,
  GeneratePrimePassCount, RSAKeyPair.FPool, Helper.FNumbersTested, wasAborted);

if not wasAborted then
    begin
    RSAKeyPair.F_RSA_n := N;
    RSAKeyPair.F_RSA_d := d;
    RSAKeyPair.F_RSA_e := e;
    RSAKeyPair.LinkParts
    end
  else
    begin
    N.Free; d.Free; e.Free
    end
finally
  Helper.Free
  end;
KeyPair := RSAKeyPair
end;



function TRSA_Engine.CreateFromStream(
  Store: TStream; Parts: TKeyStoragePartSet): TAsymetricKeyPair;
begin
result := TRSAKeyPair.CreateEmpty;
result.LoadFromStream( Store, Parts)
end;


function TRSA_Engine.LoadKeyFromStream( Store: TStream): TSymetricKey;
begin
result := CreateFromStream( Store, [partPublic, partPrivate])
end;


function TRSA_Engine.ProgId: string;
begin
result := RSA_ProgId
end;


function TRSA_Engine.WikipediaReference: string;
begin
result := 'RSA'
end;



{ TRSAKeyPart }

procedure TRSAKeyPart.Burn;
begin
F_RSA_n.Burn
end;


procedure TRSAKeyPart.LoadFromStream( Store: TStream);
begin
FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_n)
end;



function TRSAKeyPart.NominalKeyBitLength: cardinal;
var
  Xtra: cardinal;
begin
result := F_RSA_n.BitLength;
Xtra   := result mod 8;
if Xtra > 0 then
  Inc( result, 8 - Xtra)
end;



procedure TRSAKeyPart.SaveToStream( Store: TStream);
begin
FOwner.StoreHugeCardinal( F_RSA_n, Store)
end;



{ TRSA_Encryptor }

function TRSA_Encryptor.GenerateSymetricKey: TSymetricKey;
var
  Key: TRSA_PublicKeyPart;
begin
Key := FPublicKey as TRSA_PublicKeyPart;
result := Generate_RSA_SymetricKey(
  Key.F_RSA_n, Key.F_RSA_e, FCipherText, FSymetricCodec.BlockCipher)
end;


type
TMonitor = class
  private
    FSender: TObject;
    FEvent: TOnEncDecProgress;
    function OnProgress( Sender: TObject; CountBytesProcessed: int64): boolean;
  public
    constructor Create( Event: TOnEncDecProgress; Sender: TObject);
  end;


function TRSA_Encryptor.VerifySignature(
  Document: TStream; ProgressSender: TObject;
  ProgressEvent: TOnEncDecProgress; var wasAborted: boolean): boolean;
var
  Key: TRSA_PublicKeyPart;
  Monitor: TMonitor;
  OpRes: TLongOpResult;
begin
Key := FPublicKey as TRSA_PublicKeyPart;
wasAborted := False;
result := assigned( Key) and
          assigned( Key.F_RSA_n) and (not Key.F_RSA_n.isZero) and
          assigned( Key.F_RSA_e) and (not Key.F_RSA_e.isZero);
if result then
  begin
  Monitor := TMonitor.Create( ProgressEvent, ProgressSender);
  try
  OpRes := RSASSA_PSS_VERIFY( Key.F_RSA_n, Key.F_RSA_e, Document, FCipherText,
    Monitor.OnProgress)
  finally
  Monitor.Free;
  end;
  result     := OpRes = opPass;
  wasAborted := OpRes = opAbort
  end
end;



{ TRSA_Decryptor }

function TRSA_Decryptor.LoadSymetricKey( Ciphertext: TStream): TSymetricKey;
var
  Key: TRSA_PrivateKeyPart;
begin
Key := FPrivateKey as TRSA_PrivateKeyPart;
result := Extract_RSA_SymetricKey(
  Key.F_RSA_d, Key.F_RSA_n, Ciphertext, FSymetricCodec.BlockCipher)
end;




procedure TRSA_Decryptor.Sign(
  Signature: TStream; ProgressSender: TObject;
  ProgressEvent: TOnEncDecProgress; var wasAborted: boolean);
var
  Succeeded: boolean;
  Key: TRSA_PrivateKeyPart;
  OpRes: TLongOpResult;
  Monitor: TMonitor;

begin
wasAborted := False;
Key := FPrivateKey as TRSA_PrivateKeyPart;
Succeeded := assigned( Key) and
             assigned( Key.F_RSA_d) and (not Key.F_RSA_d.isZero) and
             assigned( Key.F_RSA_n) and (not Key.F_RSA_n.isZero);
if Succeeded then
  begin
  Monitor := TMonitor.Create( ProgressEvent, ProgressSender);
  try
  OpRes := uTPLb_RSA_Primitives.RSASSA_PSS_SIGN(
     Key.F_RSA_d, Key.F_RSA_n, FPlainText, Signature, Monitor.OnProgress)
  finally
    Monitor.Free
  end;
//  Succeeded  := OpRes = opPass;
  wasAborted := opRes = opAbort
  // Discard Succeeded. It should be impossible to fail.
  end
end;




function StoreHugeCardinal_Primitive(
  Number: THugeCardinal; StoreStream: TStream): boolean;
// Stores the number in the stream using the cannonical format.
// Returns True if the Number was assigned.
var
  L: cardinal;
begin
result := assigned( Number);
if result then
    L := (Number.BitLength + 7) div 8
  else
    L := 0;
if not assigned( StoreStream) then exit;
StoreStream.WriteBuffer( L, SizeOf( L));
if L > 0 then
  Number.StreamOut( LittleEndien, StoreStream, L)
end;


function LoadHugeCardinal_Primitive(
  StoreStream: TStream; Pool1: TMemoryStreamPool): THugeCardinal;
// Loads the number from the stream using the cannonical format.
var
  L: cardinal;
  ValueStream: TMemoryStream;
begin
StoreStream.Read( L, SizeOf( L));
ValueStream := TMemoryStream.Create;
try
StoreStream.ReadBuffer( ValueStream.Memory^, L);
ValueStream.Position := 0;
result := THugeCardinal.CreateFromStreamIn(
  L*8, LittleEndien, ValueStream, Pool1)
finally
ValueStream.Free
end end;



function TRSAKeyPair.StoreHugeCardinal(
  Number: THugeCardinal; StoreStream: TStream): boolean;
  // virtual method.
begin
result := StoreHugeCardinal_Primitive( Number, StoreStream)
end;


procedure TRSAKeyPair.StoreToStream( Store: TStream; Parts: TKeyStoragePartSet);
begin
if Parts <> [] then
  StoreHugeCardinal( F_RSA_n, Store);
if partPublic in Parts then
  StoreHugeCardinal( F_RSA_e, Store);
if partPrivate in Parts then
  StoreHugeCardinal( F_RSA_d, Store)
end;


function TRSAKeyPair.LoadHugeCardinal_IfNotAlready(
  StoreStream: TStream; var Number: THugeCardinal): boolean;
  // virtual method.
var
  L: cardinal;
  ValueStream: TMemoryStream;
begin
result := not assigned( Number);
if not result then exit; // Only load if we are not already loaded.
StoreStream.Read( L, SizeOf( L));
ValueStream := TMemoryStream.Create;
try
ValueStream.Size := L;
if L > 0 then
  StoreStream.ReadBuffer( ValueStream.Memory^, L);
ValueStream.Position := 0;
Number := THugeCardinal.CreateFromStreamIn(
  L*8, LittleEndien, ValueStream, FPool)
finally
ValueStream.Free
end;
if Number.isZero then
  FreeAndNil( Number)
end;


{ TRSA_PublicKeyPart }

procedure TRSA_PublicKeyPart.Burn;
begin
inherited;
F_RSA_e.Burn
end;

function TRSA_PublicKeyPart.isEmpty: boolean;
begin
result := (not assigned( F_RSA_e)) or F_RSA_e.isZero
end;


procedure TRSA_PublicKeyPart.LoadFromStream( Store: TStream);
begin
inherited;
FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_e)
end;


procedure TRSA_PublicKeyPart.SaveToStream( Store: TStream);
begin
inherited;
FOwner.StoreHugeCardinal( F_RSA_e, Store)
end;


{ TRSA_PrivateKeyPart }

procedure TRSA_PrivateKeyPart.Burn;
begin
inherited;
F_RSA_d.Burn
end;


function TRSA_PrivateKeyPart.isEmpty: boolean;
begin
result := (not assigned( F_RSA_d)) or F_RSA_d.isZero
end;

procedure TRSA_PrivateKeyPart.LoadFromStream( Store: TStream);
begin
inherited;
FOwner.LoadHugeCardinal_IfNotAlready( Store, F_RSA_d)
end;


procedure TRSA_PrivateKeyPart.SaveToStream( Store: TStream);
begin
inherited;
FOwner.StoreHugeCardinal( F_RSA_d, Store)
end;


{ TRSAKeyPair }

procedure TRSAKeyPair.Burn;
begin
F_RSA_n.Burn;
F_RSA_e.Burn;
F_RSA_d.Burn;
FreeAndNil( F_RSA_n);
FreeAndNil( F_RSA_d);
FreeAndNil( F_RSA_e);
LinkParts
end;



constructor TRSAKeyPair.CreateEmpty;
begin
FPool := TMemoryStreamPool.Create;
F_RSA_n := nil;
F_RSA_d := nil;
F_RSA_e := nil;
FPublicPart := TRSA_PublicKeyPart.Create;
FPrivatePart := TRSA_PrivateKeyPart.Create;
LinkParts;
end;



procedure TRSAKeyPair.LinkParts;
begin
(FPublicPart as TRSA_PublicKeyPart).FOwner    := self;
(FPublicPart  as TRSA_PublicKeyPart).F_RSA_n  := F_RSA_n;
(FPublicPart  as TRSA_PublicKeyPart).F_RSA_e  := F_RSA_e;

(FPrivatePart as TRSA_PrivateKeyPart).FOwner  := self;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_n := F_RSA_n;
(FPrivatePart as TRSA_PrivateKeyPart).F_RSA_d := F_RSA_d;
end;




destructor TRSAKeyPair.Destroy;
begin
F_RSA_n.Free;
F_RSA_d.Free;
F_RSA_e.Free;
FPool.Free;
inherited
end;



procedure TRSAKeyPair.LoadFromStream( Store: TStream; Parts: TKeyStoragePartSet);
begin
if Parts <> [] then
  FreeAndNil( F_RSA_n);
if partPublic in Parts then
  FreeAndNil( F_RSA_e);
if partPrivate in Parts then
  FreeAndNil( F_RSA_d);

if Parts <> [] then
  LoadHugeCardinal_IfNotAlready( Store, F_RSA_n);
if partPublic in Parts then
  LoadHugeCardinal_IfNotAlready( Store, F_RSA_e);
if partPrivate in Parts then
  LoadHugeCardinal_IfNotAlready( Store, F_RSA_d);

LinkParts
end;





{ TRSA_Gen_Key_Helper }

constructor TRSA_Gen_Key_Helper.Create(
  Client_ProgressEvent1: TGenerateAsymetricKeyPairProgress;
  Pool1: TMemoryStreamPool);
begin
FPool := Pool1;
FdoOwn := not assigned( FPool);
if FdoOwn then
  FPool := TMemoryStreamPool.Create;
FNumbersTested := 0;
FClient_ProgressEvent := Client_ProgressEvent1
end;


destructor TRSA_Gen_Key_Helper.Destroy;
begin
if FdoOwn then
  FPool.Free;
inherited
end;


procedure TRSA_Gen_Key_Helper.PrimalityTest_Event( CountPrimalityTests: integer);
begin
end;


procedure TRSA_Gen_Key_Helper.Progress_Event(
  Sender: TObject; BitsProcessed, TotalBits: int64; var doAbort: boolean);
begin
if assigned( FClient_ProgressEvent) then
  FClient_ProgressEvent( Sender, FNumbersTested, doAbort)
end;

{ TMonitor }

constructor TMonitor.Create(Event: TOnEncDecProgress; Sender: TObject);
begin
FEvent := Event;
FSender := Sender
end;

function TMonitor.OnProgress(
  Sender: TObject; CountBytesProcessed: int64): boolean;
begin
// Discard the low-level 'Sender'
if assigned( FEvent) then
    result := FEvent( FSender, CountBytesProcessed)
  else
    result := True
end;

end.
