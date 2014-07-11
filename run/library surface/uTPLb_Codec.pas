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
unit uTPLb_Codec;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher,
     uTPLb_BaseNonVisualComponent, uTPLb_CryptographicLibrary,
     uTPLb_HashDsc, uTPLb_Hash, uTPLb_StreamUtils;


type
TCodecMode = (cmUnitialized, cmIdle, cmEncrypting, cmDecrypting);

TOnEncDecProgress = function ( Sender: TObject; CountBytesProcessed: int64): boolean of object;

TGenerateAsymetricKeyPairProgress = procedure (
  Sender: TObject; CountPrimalityTests: integer;
  var doAbort: boolean) of object;


ICodec = interface
  ['{721D22EB-66C7-45B7-B926-D7E5C964AED8}']
    procedure SetStreamCipher( const Value: IStreamCipher);
    procedure SetBlockCipher ( const Value: IBlockCipher);
    procedure SetChainMode   ( const Value: IBlockChainingModel);
    function  GetMode: TCodecMode;
    function  GetStreamCipher: IStreamCipher;
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    function  GetOnProgress  : TOnEncDecProgress;
    procedure SetOnProgress( Value: TOnEncDecProgress);
    function  GetAsymetricKeySizeInBits: cardinal;
    procedure SetAsymetricKeySizeInBits( value: cardinal);
    function  GetAsymGenProgressEvent: TGenerateAsymetricKeyPairProgress;
    procedure SetAsymGenProgressEvent( Value: TGenerateAsymetricKeyPairProgress);

    function  GetCipherDisplayName( Lib: TCryptographicLibrary): string;

    procedure Init( const Key: string);
    procedure SaveKeyToStream( Store: TStream);
    procedure InitFromStream( Store: TStream);
    procedure InitFromKey( Key: TSymetricKey);   // Transfers ownership.
    procedure Reset;
    procedure Burn( doIncludeBurnKey: boolean);

    // Asymetric support
    function  isAsymetric: boolean;
    procedure InitFromGeneratedAsymetricKeyPair;

    procedure Sign(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean);

    function VerifySignature(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean): boolean;


    procedure Begin_EncryptMemory( CipherText{out}: TStream);
    procedure EncryptMemory( const Plaintext{in}; PlaintextLen: integer);
    procedure End_EncryptMemory;

    procedure Begin_DecryptMemory( PlainText{out}: TStream);
    procedure DecryptMemory( const CipherText{in}; CiphertextLen: integer);
    procedure End_DecryptMemory;

    procedure EncryptStream( Plaintext, CipherText: TStream);
    procedure DecryptStream( Plaintext, CipherText: TStream);

    procedure EncryptFile( const Plaintext_FileName, CipherText_FileName: string);
    procedure DecryptFile( const Plaintext_FileName, CipherText_FileName: string);

    procedure EncryptString( const Plaintext: string; var CipherText_Base64: ansistring);
    procedure DecryptString( var Plaintext: string; const CipherText_Base64: ansistring);

    procedure EncryptAnsiString( const Plaintext: ansistring; var CipherText_Base64: ansistring);
    procedure DecryptAnsiString( var Plaintext: ansistring; const CipherText_Base64: ansistring);

    function  isUserAborted: boolean;

    property  Mode: TCodecMode                   read GetMode;
    property  StreamCipher: IStreamCipher        read GetStreamCipher write SetStreamCipher;
    property  BlockCipher : IBlockCipher         read GetBlockCipher  write SetBlockCipher;
    property  ChainMode   : IBlockChainingModel  read GetChainMode    write SetChainMode;
    property  OnProgress  : TOnEncDecProgress    read GetonProgress   write SetOnProgress;
    property  AsymetricKeySizeInBits: cardinal   read GetAsymetricKeySizeInBits
                                                 write SetAsymetricKeySizeInBits;
    property  OnAsymGenProgress: TGenerateAsymetricKeyPairProgress
                                           read GetAsymGenProgressEvent write SetAsymGenProgressEvent;
  end;




TSimpleCodec = class( TInterfacedPersistent, ICodec, IBlockCipherSelector, IEventOrigin)
  private
    FMode: TCodecMode;
    FStreamCipher: IStreamCipher;
    FParameterizedStreamCipher: IStreamCipher;
    FBlockCipher : IBlockCipher;
    FChainMode   : IBlockChainingModel;
    FOnProgress  : TOnEncDecProgress;
    FSender: TObject;

    FKey: TSymetricKey;
    FEnc: IStreamEncryptor;
    FDec: IStreamDecryptor;
    FPasswordHasher: IHash;
    FPasswordHasherObject: TObject;  // TSimpleHash
    FXtextCount: int64;
    FisUserAborted: boolean;
    FOutput: TStream;
    FBuffer: TMemoryStream;
    FDesalination: TDesalinationWriteStream;
    FisSalting: boolean;

    FAsymetricKeySizeInBits: cardinal;
    FAsymGenProgressEvent  : TGenerateAsymetricKeyPairProgress;

    procedure SetStreamCipher( const Value: IStreamCipher);
    procedure SetBlockCipher ( const Value: IBlockCipher);
    procedure SetChainMode   ( const Value: IBlockChainingModel);
    function  GetMode: TCodecMode;
    function  GetStreamCipher: IStreamCipher;
    function  GetBlockCipher : IBlockCipher;
    function  GetChainMode   : IBlockChainingModel;
    function  GetOnProgress  : TOnEncDecProgress;
    procedure SetOnProgress( Value: TOnEncDecProgress);
    procedure SetEventSender( Sender: TObject);
    function  isNotBase64Converter: boolean;
    function  GetAsymetricKeySizeInBits: cardinal;
    procedure SetAsymetricKeySizeInBits( value: cardinal);
    function  GetAsymGenProgressEvent: TGenerateAsymetricKeyPairProgress;
    procedure SetAsymGenProgressEvent( Value: TGenerateAsymetricKeyPairProgress);

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Init( const Key: string);
    procedure SaveKeyToStream( Store: TStream);
    procedure InitFromStream( Store: TStream);
    procedure InitFromKey( Key: TSymetricKey);   // Transfers ownership.
    procedure Reset;
    procedure Burn( doIncludeBurnKey: boolean);

    function  isAsymetric: boolean;
    procedure InitFromGeneratedAsymetricKeyPair;

    procedure Sign(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean);

    function VerifySignature(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      var wasAborted: boolean): boolean;

    procedure Begin_EncryptMemory( CipherText{out}: TStream);
    procedure EncryptMemory( const Plaintext; PlaintextLen: integer);
    procedure End_EncryptMemory;

    procedure Begin_DecryptMemory( Plaintext{out}: TStream);
    procedure DecryptMemory( const CipherText{in}; CiphertextLen: integer);
    procedure End_DecryptMemory;

    procedure EncryptStream( Plaintext, CipherText: TStream);
    procedure DecryptStream( Plaintext, CipherText: TStream);

    procedure EncryptFile( const Plaintext_FileName, CipherText_FileName: string);
    procedure DecryptFile( const Plaintext_FileName, CipherText_FileName: string);

    procedure EncryptString( const Plaintext: string; var CipherText_Base64: ansistring);
    procedure DecryptString( var Plaintext: string; const CipherText_Base64: ansistring);

    procedure EncryptAnsiString( const Plaintext: ansistring; var CipherText_Base64: ansistring);
    procedure DecryptAnsiString( var Plaintext: ansistring; const CipherText_Base64: ansistring);

    function  isUserAborted: boolean;
    function  GetCipherDisplayName( Lib: TCryptographicLibrary): string;

    property  Mode: TCodecMode                   read GetMode;
    property  StreamCipher: IStreamCipher        read GetStreamCipher write SetStreamCipher;
    property  BlockCipher : IBlockCipher         read GetBlockCipher  write SetBlockCipher;
    property  ChainMode   : IBlockChainingModel  read GetChainMode    write SetChainMode;
    property  OnProgress  : TOnEncDecProgress    read GetonProgress   write SetOnProgress;
  end;






ICodec_TestAccess = interface
  // This method is ONLY to be used by unit test programs.
  ['{1DCED340-E6C0-4B97-BBAA-98305B5D4F5E}']
    function GetCodecIntf: ICodec;
  end;



TCodec = class( TTPLb_BaseNonVisualComponent, ICryptographicLibraryWatcher,
                 ICodec_TestAccess)
  strict private
    FPassword: string;

  private
    FCodecObj: TSimpleCodec;
    FCodec   : ICodec;
    FLib: TCryptographicLibrary;
    FStreamCipherId: string;
    FBlockCipherId: string;
    FChainId: string;
    FIntfCached: boolean;

    procedure SetLib( Value: TCryptographicLibrary);
    procedure Dummy( const Value: string);
    procedure SetStreamCipherId( const Value: string);
    procedure SetBlockCipherId( const Value: string);
    procedure SetChainId( const Value: string);
    procedure SetIntfCached( Value: boolean);
    procedure ReadData_Stream( Reader: TReader);
    procedure WriteData_Stream( Writer: TWriter);
    procedure ReadData_Block( Reader: TReader);
    procedure WriteData_Block( Writer: TWriter);
    procedure ReadData_Chain( Reader: TReader);
    procedure WriteData_Chain( Writer: TWriter);
    function  GetMode: TCodecMode;
    function  GetOnProgress: TOnHashProgress;
    procedure SetOnProgress(const Value: TOnHashProgress);
    procedure ProgIdsChanged;
    function  GetCodecIntf: ICodec;
    procedure SetPassword( const Password: string);
    procedure GenerateAsymetricKeyPairProgress_Event(
       Sender: TObject; CountPrimalityTests: integer;
       var doAbort: boolean);
    function  GetAsymetricKeySizeInBits: cardinal;
    procedure SetAsymetricKeySizeInBits( value: cardinal);

  protected
    procedure Notification(
      AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties( Filer: TFiler); override;
    function  GetCipherDisplayName: string; virtual;
    function  GetChainDisplayName: string; virtual;
    procedure Loaded; override;

    property  InterfacesAreCached: boolean     read FIntfCached write SetIntfCached;

  public

    FGenerateAsymetricKeyPairProgress_CountPrimalityTests: integer;

    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Burn;
    procedure Reset;
    procedure SaveKeyToStream( Store: TStream);
    procedure InitFromStream( Store: TStream);

    function  isUserAborted: boolean;

    function  isAsymetric: boolean;
    procedure InitFromKey( Key: TSymetricKey);   // Transfers ownership.
    procedure InitFromGeneratedAsymetricKeyPair;

    procedure Begin_EncryptMemory( CipherText{out}: TStream);
    procedure EncryptMemory( const Plaintext; PlaintextLen: integer);
    procedure End_EncryptMemory;

    procedure Begin_DecryptMemory( Plaintext{out}: TStream);
    procedure DecryptMemory( const CipherText{in}; CiphertextLen: integer);
    procedure End_DecryptMemory;

    procedure EncryptStream( Plaintext, CipherText: TStream);
    procedure DecryptStream( Plaintext, CipherText: TStream);

    procedure EncryptFile( const Plaintext_FileName, CipherText_FileName: string);
    procedure DecryptFile( const Plaintext_FileName, CipherText_FileName: string);

    procedure EncryptString( const Plaintext: string; var CipherText_Base64: ansistring);
    procedure DecryptString( var Plaintext: string; const CipherText_Base64: ansistring);

    procedure EncryptAnsiString( const Plaintext: ansistring; var CipherText_Base64: ansistring);
    procedure DecryptAnsiString( var Plaintext: ansistring; const CipherText_Base64: ansistring);

    property  StreamCipherId: string            read FStreamCipherId  write SetStreamCipherId;
    property  BlockCipherId: string             read FBlockCipherId   write SetBlockCipherId;
    property  ChainModeId: string               read FChainId         write SetChainId;
    property  Password: string                  read FPassword        write SetPassword;
    property  Mode: TCodecMode                  read GetMode;

  published
    property  Cipher: string                      read GetCipherDisplayName write Dummy stored False;
    property  ChainMode: string                   read GetChainDisplayName write Dummy stored False;
    property  AsymetricKeySizeInBits: cardinal    read GetAsymetricKeySizeInBits
                                                  write SetAsymetricKeySizeInBits;

    property  CryptoLibrary: TCryptographicLibrary    read FLib write SetLib;
    property  OnProgress  : TOnHashProgress    read GetOnProgress   write SetOnProgress;
  end;







implementation






uses SysUtils, Math, uTPLB_SHA1, uTPLb_ECB, uTPLb_Random,
     uTPLb_BinaryUtils, uTPLb_Asymetric;



const
  SaltSize = 8; // 8 bytes or 64 bits of entropy to be injected.
  Default_AsymetricKeySizeInBits = 1024;
{ TSimpleCodec }

constructor TSimpleCodec.Create;
begin
FMode := cmUnitialized;
FStreamCipher := nil;
FParameterizedStreamCipher := nil;
FBlockCipher  := nil;
FChainMode    := nil;
FOnProgress   := nil;
FKey := nil;
FEnc := nil;
FDec := nil;
FPasswordHasherObject := TSimpleHash.Create;
Supports( FPasswordHasherObject, IHash, FPasswordHasher);
FPasswordHasher.Hash := TSHA1.Create;
FisUserAborted := False;
FXtextCount := 0;
FOutput := nil;
FBuffer := TMemoryStream.Create;
FSender := self;
FDesalination := TDesalinationWriteStream.Create;
FisSalting := False;
FAsymetricKeySizeInBits := Default_AsymetricKeySizeInBits;
FAsymGenProgressEvent := nil
end;


destructor TSimpleCodec.Destroy;
begin
Burn( True);
FPasswordHasher := nil;
FPasswordHasherObject.Free;
FKey.Free;
FBuffer.Free;
FDesalination.Free;
inherited;
end;



function  TSimpleCodec.isAsymetric: boolean;
begin
if assigned( FStreamCipher) and (not assigned( FParameterizedStreamCipher)) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher
  end;
result := assigned( FParameterizedStreamCipher) and
         (afAsymetric in FParameterizedStreamCipher.Features) and
         Supports( FParameterizedStreamCipher, IAsymetric_Engine)
end;


procedure TSimpleCodec.Init( const Key: string);
var
  L, Diff, Amt: integer;
  Asymetric_Engine: IAsymetric_Engine;
  KeyPair: TAsymetricKeyPair;
begin
if Key = '' then
  raise Exception.Create( 'TSimpleCodec.Init - No password.');
if FMode in [cmEncrypting, cmDecrypting] then
  Reset;
FMode := cmUnitialized;
FXtextCount := 0;
FreeAndNil( FKey);
FisUserAborted := False;
if isAsymetric then
  raise Exception.Create( 'TSimpleCodec.Init - Asymetric codecs are not initialized by string.');
FEnc := nil; FDec := nil;
FisUserAborted := False;
    // Symetric Cipher
    begin
    // The key is based on a hash of the Key string for symetric ciphers.
    try
      if FParameterizedStreamCipher.SeedByteSize <> -1 then
        begin
        FPasswordHasher.HashString( Key);
        FBuffer.Size := 0;
        FBuffer.CopyFrom( FPasswordHasher.HashOutputValue, 0);
        FPasswordHasher.Burn;
        L := FBuffer.Size;
        if L = 0 then
          raise Exception.Create( 'TSimpleCodec.Init - Hash failed.');
        Diff := L - FParameterizedStreamCipher.SeedByteSize;
        if Diff > 0 then
            // Truncate
            FBuffer.Size := FParameterizedStreamCipher.SeedByteSize

          else if Diff < 0 then
            begin // Extend by repitition.
            FBuffer.Seek( 0, soEnd);
            Diff := - Diff;
            repeat
              Amt := Min( Diff, L);
              FBuffer.Write( FBuffer.Memory^, Amt);
             Dec( Diff, Amt)
            until Diff <= 0
            end
        end;
      // A SeedByteSize of -1 means that the seed (FBuffer) is irrelevant.
      // This might be so in non-encryption type transformations.
      FBuffer.Position := 0;
      FKey := FParameterizedStreamCipher.GenerateKey( FBuffer)
    finally
      FPasswordHasher.Burn;
      BurnMemoryStream( FBuffer)
      end  // try
    end; // else
if FisUserAborted then
    FMode := cmUnitialized
  else
    FMode := cmIdle;
FisSalting := False
end;


procedure TSimpleCodec.InitFromKey( Key: TSymetricKey);
begin
if FMode in [cmEncrypting, cmDecrypting] then
  Reset;
FMode := cmUnitialized;
FXtextCount := 0;
FreeAndNil( FKey);
FisUserAborted := False;
if not assigned( FParameterizedStreamCipher) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher
  end;
FEnc := nil; FDec := nil;
FKey := Key; // Ownership xferred.
FMode := cmIdle;
FisSalting := False
end;



procedure TSimpleCodec.InitFromStream( Store: TStream);
begin
if FMode in [cmEncrypting, cmDecrypting] then
  Reset;
FMode := cmUnitialized;
FXtextCount := 0;
FreeAndNil( FKey);
FisUserAborted := False;
if not assigned( FParameterizedStreamCipher) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher
  end;
FEnc := nil; FDec := nil;
FKey := FParameterizedStreamCipher.LoadKeyFromStream( Store);
FMode := cmIdle;
FisSalting := False
end;


function TSimpleCodec.isNotBase64Converter: boolean;
begin
result := not Supports( FStreamCipher, IisBase64Converter)
end;

function TSimpleCodec.isUserAborted: boolean;
begin
result := FisUserAborted
end;


procedure TSimpleCodec.Reset;
begin
if FMode = cmUnitialized then
  raise Exception.Create( 'TSimpleCodec.Init - Reset when not intialized.');
FXtextCount := 0;
if assigned( FEnc) then
  FEnc.Reset;  // Alternative would be to release FEnc.
if assigned( FDec) then
  FDec.Reset;
FMode := cmIdle;
FBuffer.Size := 0;
FisUserAborted := False;
FisSalting := False;
FParameterizedStreamCipher := nil
end;


procedure TSimpleCodec.Begin_EncryptMemory( CipherText{out}: TStream);
begin
if FMode <> cmIdle then
  raise Exception.Create( 'TSimpleCodec.Begin_EncryptMemory - Wrong mode.');

// Require:
//  1. assigned( FStreamCipher)
//  2. NOT afNotImplementedYet in FStreamCipher.Features;
//  3. if afBlockAdapter in FStreamCipher.Features then
//    3.1 assigned( FBlockCipher)
//    3.2 NOT afNotImplementedYet in FBlockCipher.Features;
//    3.3 assigned( FChainMode)
//    3.4 NOT afNotImplementedYet in FChainMode.Features;

if (not assigned( FStreamCipher)) or
   (afNotImplementedYet in FStreamCipher.Features) or

   ((afBlockAdapter in FStreamCipher.Features) and (
     (not assigned( FBlockCipher)) or
     (afNotImplementedYet in FBlockCipher.Features) or
     (not assigned( FChainMode)) or
     (afNotImplementedYet in FChainMode.Features)
     )) then
  raise Exception.Create( 'TSimpleCodec.Begin_EncryptMemory - Algorithms not set.');

if not assigned( FParameterizedStreamCipher) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher;
  FEnc := nil
  end;

FDec := nil;
if (not assigned( FEnc)) or (FOutput <> Ciphertext) then
    begin
    FEnc := nil;
    FOutput := Ciphertext;
    FEnc := FParameterizedStreamCipher.Start_Encrypt( FKey, FOutput)
    end
  else
    FEnc.Reset;

FMode := cmEncrypting;
FXtextCount := 0;
FisSalting := False;
FisUserAborted := False;
end;


procedure TSimpleCodec.Begin_DecryptMemory( Plaintext{out}: TStream);
var
  useDesalination: boolean;
  FinalOutput: TStream;
begin
if FMode <> cmIdle then
  raise Exception.Create( 'TSimpleCodec.Begin_DecryptMemory - Wrong mode.');
if (not assigned( FStreamCipher)) or
   (afNotImplementedYet in FStreamCipher.Features) or

   ((afBlockAdapter in FStreamCipher.Features) and (
     (not assigned( FBlockCipher)) or
     (afNotImplementedYet in FBlockCipher.Features) or
     (not assigned( FChainMode)) or
     (afNotImplementedYet in FChainMode.Features)
     )) then
  raise Exception.Create( 'TSimpleCodec.Begin_DecryptMemory - Algorithms not set.');

if not assigned( FParameterizedStreamCipher) then
  begin
  FParameterizedStreamCipher := FStreamCipher.Parameterize( self);
  if not assigned( FParameterizedStreamCipher) then
    FParameterizedStreamCipher := FStreamCipher;
  FDec := nil
  end;

useDesalination := ([afCompressor, afConverter, afDoesNotNeedSalt] *
                    FParameterizedStreamCipher.Features) = [];

if useDesalination then
    FinalOutput := FDesalination.FreshwaterStream
  else
    FinalOutput := FOutput;

FEnc := nil;
if (not assigned( FDec)) or (FinalOutput <> Plaintext) then
    begin
    FDec := nil;
    FOutput := Plaintext;
    if useDesalination then
        begin
        FDesalination.FreshwaterStream := FOutput;
        FOutput := FDesalination;
        FDesalination.SaltVolume := SaltSize
        end;
    FDec := FParameterizedStreamCipher.Start_Decrypt( FKey, FOutput)
    end
  else
    FDec.Reset;

FMode := cmDecrypting;
FXtextCount := 0;
FisUserAborted := False;
end;


procedure TSimpleCodec.Burn( doIncludeBurnKey: boolean);
begin
if assigned( FKey) and (not doIncludeBurnKey) and (FMode <> cmUnitialized) then
    FMode := cmIdle
  else
    begin
    FMode := cmUnitialized;
    if assigned( FKey) then
      begin
      FKey.Burn;
      FreeAndNil( FKey)
      end
    end;
FParameterizedStreamCipher := nil;
FEnc := nil;
FDec := nil;
FPasswordHasher.Burn;
FXtextCount := 0;
FisUserAborted := False;
BurnMemoryStream( FBuffer);
if FOutput = FDesalination then
  begin
  FDesalination.FreshwaterStream := nil;
  FDesalination.SaltVolume       := SaltSize
  end;
FOutput := nil;
FisSalting := False
end;


procedure TSimpleCodec.DecryptAnsiString(
  var Plaintext: ansistring;
  const CipherText_Base64: ansistring);
var
  Temp, Ciphertext: TMemoryStream;
  L: integer;
begin
Temp := TMemoryStream.Create;
Ciphertext := TMemoryStream.Create;
try
  if isNotBase64Converter then
      Base64_to_stream( CipherText_Base64, Ciphertext)
    else
      // If its already a Base64 encoder, no point in double-encoding it as base64.
      AnsiString_to_stream( CipherText_Base64, Ciphertext);
  Begin_DecryptMemory( Temp);
  L := Ciphertext.Size;
  if L > 0 then
    DecryptMemory( Ciphertext.Memory^, L);
  End_DecryptMemory;
  if FisUserAborted then
      Plaintext := ''
    else
      begin
      Temp.Position := 0;
      L := Temp.Size;
      SetLength( Plaintext, L div SizeOf( AnsiChar));
      if L > 0 then
        Temp.Read( Plaintext[1], L)
      end
finally
  BurnMemoryStream( Temp);
  BurnMemoryStream( Ciphertext);
  Temp.Free;
  Ciphertext.Free
end end;



procedure TSimpleCodec.DecryptFile(
  const Plaintext_FileName,
  CipherText_FileName: string);
var
  plaintext, ciphertext: TStream;
begin
plaintext  := TFileStream.Create( Plaintext_FileName, fmCreate);
Ciphertext := TFileStream.Create( Ciphertext_FileName, fmOpenRead);
try
  DecryptStream( plaintext, ciphertext)
finally
  PlainText.Free;
  CipherText.Free
end end;




procedure TSimpleCodec.DecryptStream( Plaintext, CipherText: TStream);
var
  Temp: TMemoryStream;
  BytesRead, Amnt: integer;
begin
Temp := TMemoryStream.Create;
try
  Ciphertext.Position := 0;
  Amnt := Max( Min( CipherText.Size, 1024), 1);
  Temp.Size := Amnt;
  Begin_DecryptMemory( PlainText);
  repeat
    if Temp.Size > Amnt then
      BurnMemoryStream( Temp);
    if Temp.Size <> Amnt then
      Temp.Size := Amnt;
    BytesRead := Ciphertext.Read( Temp.Memory^, Amnt);
    if BytesRead = 0 then break;
    DecryptMemory( Temp.Memory^, BytesRead)
  until (BytesRead < Amnt) or FisUserAborted;
  if not FisUserAborted then
    End_DecryptMemory
finally
  BurnMemoryStream( Temp);
  Temp.Free
end end;


procedure TSimpleCodec.DecryptString(
  var Plaintext: string;
  const CipherText_Base64: ansistring);
var
  Temp, Ciphertext: TMemoryStream;
  L: integer;
begin
Temp := TMemoryStream.Create;
Ciphertext := TMemoryStream.Create;
try
  if isNotBase64Converter then
      Base64_to_stream( CipherText_Base64, Ciphertext)
    else
      // If its already a Base64 encoder, no point in double-encoding it as base64.
      AnsiString_to_stream( CipherText_Base64, Ciphertext);
  Begin_DecryptMemory( Temp);
  L := Ciphertext.Size;
  if L > 0 then
    DecryptMemory( Ciphertext.Memory^, L);
  End_DecryptMemory;
  if FisUserAborted then
      Plaintext := ''
    else
      begin
      Temp.Position := 0;
      L := Temp.Size;
      SetLength( Plaintext, L div SizeOf( Char));
      if (L mod SizeOf( Char)) <> 0 then
        Dec( L, L mod SizeOf( Char));
      if L > 0 then
        Temp.Read( Plaintext[1], L)
      end
finally
  BurnMemoryStream( Temp);
  BurnMemoryStream( Ciphertext);
  Temp.Free;
  Ciphertext.Free
end end;


procedure TSimpleCodec.EncryptAnsiString(
  const Plaintext: ansistring;
  var CipherText_Base64: ansistring);
var
  Temp: TMemoryStream;
  L: integer;
begin
Temp := TMemoryStream.Create;
try
  Begin_EncryptMemory( Temp);
  L := Length( Plaintext) * SizeOf( AnsiChar);
  if L > 0 then
    EncryptMemory( Plaintext[1], L);
  End_EncryptMemory;
  if FisUserAborted then
      CipherText_Base64 := ''
    else
      begin
      Temp.Position := 0;
      if isNotBase64Converter then
          CipherText_Base64 := Stream_to_Base64( Temp)
        else
          // If its already a Base64 encoder, no point in double-encoding it as base64.
          CipherText_Base64 := Stream_to_AnsiString( Temp)
      end
finally
  BurnMemoryStream( Temp);
  Temp.Free
end end;




procedure TSimpleCodec.EncryptFile(
  const Plaintext_FileName,
  CipherText_FileName: string);
var
  plaintext, ciphertext: TStream;
begin
plaintext  := TFileStream.Create( Plaintext_FileName, fmOpenRead);
Ciphertext := TFileStream.Create( Ciphertext_FileName, fmCreate);
try
  EncryptStream( plaintext, ciphertext)
finally
  PlainText.Free;
  CipherText.Free
end end;



procedure TSimpleCodec.EncryptMemory(
  const Plaintext; PlaintextLen: integer);
var
  P: PByte;
  L, Amt: integer;
  Salt: packed array[ 0..SaltSize-1 ] of byte;
begin
if FisUserAborted then exit;
if FMode <> cmEncrypting then
  raise Exception.Create( 'TSimpleCodec.EncryptMemory - Wrong mode.');
P := @Plaintext;
L := PlaintextLen;
if (FXtextCount = 0) and (L > 0) and (not FisSalting) and
         (([afCompressor, afConverter, afDoesNotNeedSalt] *
           FParameterizedStreamCipher.Features) = []) then
  begin
  FisSalting := True;
  TRandomStream.Instance.Read( Salt, SaltSize);
  EncryptMemory( Salt, SaltSize);
  FisSalting := False
  end;
try
// Break up the plaintext memory into at most 1 KiB chunks.
while (L > 0) and (not FisUserAborted) do
  begin
  Amt := Min( L, 1024);
  if FBuffer.Size > Amt then
    BurnMemoryStream( FBuffer);
  if FBuffer.Size <> Amt then
    FBuffer.Size := Amt;
  Move( P^, FBuffer.Memory^, Amt);
  FBuffer.Position := 0;
  // Encrypt the chunk.
  FEnc.Encrypt( FBuffer);
  Dec( L, Amt);
  Inc( P, Amt);
  Inc( FXtextCount, Amt);
  // Check for user abort.
  if assigned( FOnProgress) and (L > 0) and (not FisSalting) then
    FisUserAborted := not FOnProgress( FSender, FXtextCount)
  end
finally
BurnMemoryStream( FBuffer)
end end;


procedure TSimpleCodec.DecryptMemory(
  const CipherText{in}; CiphertextLen: integer);
var
  P: PByte;
  L, Amt: integer;
begin
if FisUserAborted then exit;
if FMode <> cmDecrypting then
  raise Exception.Create( 'TSimpleCodec.DecryptMemory - Wrong mode.');
P := @CipherText;
L := CiphertextLen;
try
while (L > 0) and (not FisUserAborted) do
  begin
  Amt := Min( L, 1024);
  if FBuffer.Size > Amt then
    BurnMemoryStream( FBuffer);
  if FBuffer.Size <> Amt then
    FBuffer.Size := Amt;
  Move( P^, FBuffer.Memory^, Amt);
  FBuffer.Position := 0;
  FDec.Decrypt( FBuffer);
  Dec( L, Amt);
  Inc( P, Amt);
  Inc( FXtextCount, Amt);
  if assigned( FOnProgress) and (L > 0) then
    FisUserAborted := not FOnProgress( FSender, FXtextCount)
  end
finally
BurnMemoryStream( FBuffer)
end end;



procedure TSimpleCodec.EncryptStream( Plaintext, CipherText: TStream);
var
  Temp: TMemoryStream;
  Amnt, BytesRead: integer;
begin
Temp := TMemoryStream.Create;
try
  // Break up stream into at most 1 KiB blocks, but don't needlessly
  //  oversize the buffer.
  Plaintext.Position := 0;
  Amnt := Max( Min( PlainText.Size, 1024), 1);
  Temp.Size := Amnt;
  Begin_EncryptMemory( CipherText);
  repeat
    if Temp.Size > Amnt then
      BurnMemoryStream( Temp);
    if Temp.Size <> Amnt then
      Temp.Size := Amnt;
    BytesRead := PlainText.Read( Temp.Memory^, Amnt);
    if BytesRead = 0 then break;
    EncryptMemory( Temp.Memory^, BytesRead)
  until (BytesRead < Amnt) or FisUserAborted;
  if not FisUserAborted then
    End_EncryptMemory
finally
  BurnMemoryStream( Temp);
  Temp.Free
end end;


procedure TSimpleCodec.EncryptString(
  const Plaintext: string;
  var CipherText_Base64: ansistring);
var
  Temp: TMemoryStream;
  L: integer;
begin
Temp := TMemoryStream.Create;
try
  Begin_EncryptMemory( Temp);
  L := Length( Plaintext) * SizeOf( Char);
  if L > 0 then
    EncryptMemory( Plaintext[1], L);
  End_EncryptMemory;
  if FisUserAborted then
      CipherText_Base64 := ''
    else
      begin
      Temp.Position := 0;
      if isNotBase64Converter then
          CipherText_Base64 := Stream_to_Base64( Temp)
        else
          // If its already a Base64 encoder, no point in double-encoding it as base64.
          CipherText_Base64 := Stream_to_AnsiString( Temp)
      end
finally
  Temp.Free
end end;



procedure TSimpleCodec.End_DecryptMemory;
begin
case FMode of
  cmIdle:       begin end;

  cmDecrypting: begin
                FDec.End_Decrypt;
                Reset
                end;

  cmUnitialized,
  cmEncrypting:
  raise Exception.Create( 'TSimpleCodec.End_DecryptMemory - Wrong mode.');
  end
end;


procedure TSimpleCodec.End_EncryptMemory;
begin
case FMode of
  cmIdle:       begin end;

  cmEncrypting: begin
                FEnc.End_Encrypt;
                FMode := cmIdle;
                FisUserAborted := False
                end;

  cmUnitialized,
  cmDecrypting:
  raise Exception.Create( 'TSimpleCodec.End_EncryptMemory - Wrong mode.');
  end
end;



function TSimpleCodec.GetAsymetricKeySizeInBits: cardinal;
begin
result := FAsymetricKeySizeInBits
end;

function TSimpleCodec.GetAsymGenProgressEvent: TGenerateAsymetricKeyPairProgress;
begin
result := FAsymGenProgressEvent
end;

function TSimpleCodec.GetBlockCipher: IBlockCipher;
begin
result := FBlockCipher
end;

function TSimpleCodec.GetChainMode: IBlockChainingModel;
begin
result := FChainMode
end;

function TSimpleCodec.GetCipherDisplayName( Lib: TCryptographicLibrary): string;
var
  TempCipher: IStreamCipher;
begin
TempCipher := FParameterizedStreamCipher;
if not assigned( TempCipher) and assigned( FStreamCipher) then
  TempCipher := FStreamCipher.Parameterize( self);
if not assigned( TempCipher) then
  TempCipher := FStreamCipher;
if assigned( TempCipher) then
    begin
    if assigned( Lib) then
        result := Lib.ComputeCipherDisplayName( TempCipher, FBlockCipher)
      else
        result := TCryptographicLibrary.ComputeCipherDisplayName( TempCipher, FBlockCipher)
    end
  else
    result := ''
end;


function TSimpleCodec.GetMode: TCodecMode;
begin
result := FMode
end;

function TSimpleCodec.GetOnProgress: TOnEncDecProgress;
begin
result := FOnProgress
end;


function TSimpleCodec.GetStreamCipher: IStreamCipher;
begin
result := FStreamCipher
end;


procedure TSimpleCodec.SaveKeyToStream( Store: TStream);
begin
if assigned( FKey) and assigned( Store) then
  FKey.SaveToStream( Store)
end;






procedure TSimpleCodec.SetAsymetricKeySizeInBits( value: cardinal);
begin
if FAsymetricKeySizeInBits = Value then exit;
if FMode in [cmEncrypting, cmDecrypting] then
  raise Exception.Create( 'TSimpleCodec.AsymetricKeySizeInBits - Cannot set parameter whilst enc/decrypting.');
FAsymetricKeySizeInBits := Value;
if FMode = cmIdle then
  Burn( True);
end;




procedure TSimpleCodec.SetAsymGenProgressEvent(
  Value: TGenerateAsymetricKeyPairProgress);
begin
FAsymGenProgressEvent := value
end;



procedure TSimpleCodec.SetBlockCipher( const Value: IBlockCipher);
begin
if FBlockCipher = Value then exit;
if FMode in [cmEncrypting, cmDecrypting] then
  raise Exception.Create( 'TSimpleCodec.Init - Cannot set Cipher whilst enc/decrypting.');
if FMode = cmIdle then
  Burn( True);
FBlockCipher := Value
end;


procedure TSimpleCodec.SetChainMode( const Value: IBlockChainingModel);
begin
if FChainMode = Value then exit;
if FMode in [cmEncrypting, cmDecrypting] then
  raise Exception.Create( 'TSimpleCodec.Init - Cannot set Cipher whilst enc/decrypting.');
if FMode = cmIdle then
  Burn( True);
FChainMode := Value;
end;


procedure TSimpleCodec.SetEventSender( Sender: TObject);
begin
FSender := Sender;
if not assigned( FSender) then
  FSender := self
end;


procedure TSimpleCodec.SetOnProgress( Value: TOnEncDecProgress);
begin
FOnProgress := Value
end;



procedure TSimpleCodec.SetStreamCipher( const Value: IStreamCipher);
begin
if FStreamCipher = Value then exit;
if FMode in [cmEncrypting, cmDecrypting] then
  raise Exception.Create( 'TSimpleCodec.Init - Cannot set Cipher whilst enc/decrypting.');
if FMode = cmIdle then
  Burn( True);
FStreamCipher := Value
end;




procedure TSimpleCodec.InitFromGeneratedAsymetricKeyPair;
var
  Asymetric_Engine: IAsymetric_Engine;
  KeyPair: TAsymetricKeyPair;
begin
if FMode in [cmEncrypting, cmDecrypting] then
  Reset;
FMode := cmUnitialized;
FXtextCount := 0;
FreeAndNil( FKey);
FisUserAborted := False;
FEnc := nil; FDec := nil;
FisUserAborted := False;
FisSalting := False;
if (not isAsymetric) or (not (Supports( FParameterizedStreamCipher,
    IAsymetric_Engine, Asymetric_Engine))) then
  raise Exception.Create( 'TSimpleCodec.Init - Asymetric codecs are not initialized by string.');
FreeAndNil( FKey);
Asymetric_Engine.GenerateAsymetricKeyPair(
  FAsymetricKeySizeInBits, FSender, FAsymGenProgressEvent, KeyPair,
  FisUserAborted);
if FisUserAborted then
    FMode := cmUnitialized
  else
    begin
    FMode := cmIdle;
    FKey := KeyPair
    end
end;



procedure TSimpleCodec.Sign(Document, Signature: TStream;
  ProgressSender: TObject; ProgressEvent: TOnEncDecProgress;
  var wasAborted: boolean);
begin

end;

function TSimpleCodec.VerifySignature(Document, Signature: TStream;
  ProgressSender: TObject; ProgressEvent: TOnEncDecProgress;
  var wasAborted: boolean): boolean;
begin

end;

{
To Do for TP LockBox 3
======================

Codec CascadeNext/Prev
Codec Cascade Encrypt
Codec CascadeOutputStream
Codec CascadeInputStream
Patent numbers:
CFB-8bit
AES
RSA
MS CryptAPI wrap
OpenSSL wrap
n/i the other hashes
Blowfish
Identify and n/i the other ciphers
design package
Study RSA theory
Study LB 2's implementation of big integers
Study LB 2's implementation of RSA
}


{ TCodec }

constructor TCodec.Create( AOwner: TComponent);
var
  Origin: IEventOrigin;
begin
inherited Create( AOwner);
FCodecObj := TSimpleCodec.Create;
FCodec    := FCodecObj as ICodec;
if Supports( FCodecObj, IEventOrigin, Origin) then
  Origin.SetEventSender( self);
FLib     := nil;
FStreamCipherId  := '';
FBlockCipherId  := '';
FChainId  := '';
FIntfCached := False;
FCodec.OnAsymGenProgress := GenerateAsymetricKeyPairProgress_Event
end;


destructor TCodec.Destroy;
begin
FCodec.Burn( True);
SetLib( nil);
FCodec := nil;
FCodecObj.Free;
if FPassword <> '' then
  BurnMemory( FPassword[1], Length( FPassword) * SizeOf( Char));
FPassword := '';
inherited
end;


procedure TCodec.DefineProperties(Filer: TFiler);
begin
inherited;
Filer.DefineProperty('StreamCipherId', ReadData_Stream, WriteData_Stream, True);
Filer.DefineProperty('BlockCipherId' , ReadData_Block , WriteData_Block , True);
Filer.DefineProperty('ChainId'       , ReadData_Chain , WriteData_Chain , True)
end;



procedure TCodec.Begin_EncryptMemory( CipherText: TStream);
begin
InterfacesAreCached := True;
FCodec.Begin_EncryptMemory( Ciphertext)
end;


procedure TCodec.Begin_DecryptMemory( Plaintext: TStream);
begin
InterfacesAreCached := True;
FCodec.Begin_DecryptMemory( Plaintext)
end;



procedure TCodec.Burn;
begin
FCodec.Burn( False)
end;


procedure TCodec.DecryptAnsiString(
  var Plaintext: ansistring; const CipherText_Base64: ansistring);
begin
InterfacesAreCached := True;
FCodec.DecryptAnsiString( Plaintext, CipherText_Base64)
end;



procedure TCodec.DecryptFile(
  const Plaintext_FileName, CipherText_FileName: string);
begin
InterfacesAreCached := True;
FCodec.DecryptFile( Plaintext_FileName, CipherText_FileName)
end;


procedure TCodec.DecryptMemory( const CipherText; CiphertextLen: integer);
begin
InterfacesAreCached := True;
FCodec.DecryptMemory( CipherText, CiphertextLen)
end;



procedure TCodec.DecryptStream( Plaintext, CipherText: TStream);
begin
InterfacesAreCached := True;
FCodec.DecryptStream( Plaintext, CipherText)
end;



procedure TCodec.DecryptString(
  var Plaintext: string; const CipherText_Base64: ansistring);
begin
InterfacesAreCached := True;
FCodec.DecryptString( Plaintext, CipherText_Base64)
end;


procedure TCodec.Dummy( const Value: string);
begin
end;



procedure TCodec.EncryptAnsiString(
  const Plaintext: ansistring; var CipherText_Base64: ansistring);
begin
InterfacesAreCached := True;
FCodec.EncryptAnsiString( Plaintext, CipherText_Base64)
end;



procedure TCodec.EncryptFile(
  const Plaintext_FileName, CipherText_FileName: string);
begin
InterfacesAreCached := True;
FCodec.EncryptFile( Plaintext_FileName, CipherText_FileName)
end;



procedure TCodec.EncryptMemory( const Plaintext; PlaintextLen: integer);
begin
InterfacesAreCached := True;
FCodec.EncryptMemory( Plaintext, PlaintextLen)
end;



procedure TCodec.EncryptStream( Plaintext, CipherText: TStream);
begin
InterfacesAreCached := True;
FCodec.EncryptStream( Plaintext, CipherText)
end;



procedure TCodec.EncryptString(
  const Plaintext: string; var CipherText_Base64: ansistring);
begin
InterfacesAreCached := True;
FCodec.EncryptString( Plaintext, CipherText_Base64)
end;



procedure TCodec.End_DecryptMemory;
begin
InterfacesAreCached := True;
FCodec.End_DecryptMemory
end;



procedure TCodec.End_EncryptMemory;
begin
InterfacesAreCached := True;
FCodec.End_EncryptMemory
end;



procedure TCodec.GenerateAsymetricKeyPairProgress_Event(
  Sender: TObject; CountPrimalityTests: integer; var doAbort: boolean);
var
  CountBytesProcessed: int64;
begin
if assigned( FCodec.OnProgress) then
  begin
  CountBytesProcessed := -1;
  FGenerateAsymetricKeyPairProgress_CountPrimalityTests  := CountPrimalityTests;
  doAbort := not FCodec.OnProgress( self, CountBytesProcessed)
  end
end;


function TCodec.GetAsymetricKeySizeInBits: cardinal;
begin
result := FCodec.AsymetricKeySizeInBits
end;


function TCodec.GetChainDisplayName: string;
begin
InterfacesAreCached := True;
if FCodec.ChainMode <> nil then
    result := TCryptographicLibrary.ComputeChainDisplayName( FCodec.ChainMode)
  else
    result := ''
end;


function TCodec.GetCipherDisplayName: string;
begin
InterfacesAreCached := True;
result := FCodec.GetCipherDisplayName( FLib)
end;



function TCodec.GetCodecIntf: ICodec;
begin
InterfacesAreCached := True;
result := FCodec
end;



function TCodec.GetMode: TCodecMode;
begin
result := FCodec.Mode
end;


function TCodec.GetOnProgress: TOnHashProgress;
begin
result := FCodec.OnProgress
end;


function TCodec.isAsymetric: boolean;
begin
InterfacesAreCached := True;
result := FCodec.isAsymetric
end;


function TCodec.isUserAborted: boolean;
begin
result := FCodec.isUserAborted
end;


procedure TCodec.Loaded;
begin
inherited;
InterfacesAreCached := True
end;



procedure TCodec.Notification( AComponent: TComponent; Operation: TOperation);
begin
inherited;
if (Operation = opRemove) and (AComponent = FLib) then
  SetLib( nil)
end;



procedure TCodec.ProgIdsChanged;
begin
InterfacesAreCached := False
end;



procedure TCodec.ReadData_Stream( Reader: TReader);
begin
StreamCipherId := Reader.ReadString
end;

procedure TCodec.ReadData_Block( Reader: TReader);
begin
BlockCipherId := Reader.ReadString
end;

procedure TCodec.ReadData_Chain( Reader: TReader);
begin
ChainModeId := Reader.ReadString
end;



procedure TCodec.Reset;
begin
FGenerateAsymetricKeyPairProgress_CountPrimalityTests := 0;
FCodec.Reset
end;


procedure TCodec.InitFromGeneratedAsymetricKeyPair;
begin
InterfacesAreCached := True;
FCodec.InitFromGeneratedAsymetricKeyPair
end;


procedure TCodec.InitFromKey( Key: TSymetricKey);
begin
InterfacesAreCached := True;
FCodec.InitFromKey( Key)
end;


procedure TCodec.InitFromStream( Store: TStream);
begin
InterfacesAreCached := True;
FCodec.InitFromStream( Store);
FPassword := '' // Because not using a password.
end;


procedure TCodec.SaveKeyToStream( Store: TStream);
begin
InterfacesAreCached := True;
FCodec.SaveKeyToStream( Store)
end;


procedure TCodec.SetAsymetricKeySizeInBits( value: cardinal);
begin
FCodec.AsymetricKeySizeInBits := Value
end;



procedure TCodec.SetBlockCipherId( const Value: string);
begin
if FBlockCipherId = Value then exit;
InterfacesAreCached := False;
FBlockCipherId := Value
end;



procedure TCodec.SetChainId( const Value: string);
begin
if FChainId = Value then exit;
InterfacesAreCached := False;
FChainId := Value
end;



procedure TCodec.SetIntfCached( Value: boolean);
begin
if FIntfCached = Value then exit;
FIntfCached := Value;
FCodec.StreamCipher  := nil;
FCodec.BlockCipher   := nil;
FCodec.ChainMode     := nil;
if FIntfCached and assigned( FLib) then
  begin
  FCodec.StreamCipher := FLib.StreamCipherIntfc( FStreamCipherId);
  FCodec.BlockCipher  := FLib.BlockCipherIntfc( FBlockCipherId);
  FCodec.ChainMode    := FLib.BlockChainingModelIntfc( FChainId);
  if (FPassword <> '') and assigned( FCodec.StreamCipher) and (not FCodec.isAsymetric) then
    FCodec.Init( FPassword)
  end
end;



procedure TCodec.SetLib( Value: TCryptographicLibrary);
var
  OldLib: TCryptographicLibrary;
begin
if FLib = Value then exit;
if assigned( FLib) then
  begin
  OldLib := FLib;
  FLib := nil;
  OldLib.DegisterWatcher( self);
  OldLib.RemoveFreeNotification( self)
  end;
InterfacesAreCached := False;
FLib := Value;
if assigned( FLib) then
  begin
  FLib.FreeNotification( self);
  FLib.RegisterWatcher ( self)
  end
end;




procedure TCodec.SetOnProgress(const Value: TOnHashProgress);
begin
FCodec.OnProgress := Value
end;



procedure TCodec.SetPassword( const Password: string);
begin
FPassword := Password;
if InterfacesAreCached then
    begin
    if (FPassword <> '') and
       assigned( FCodec.StreamCipher) then
      FCodec.Init( FPassword)
    end
  else
    InterfacesAreCached := True
end;



procedure TCodec.SetStreamCipherId( const Value: string);
begin
if FStreamCipherId = Value then exit;
InterfacesAreCached := False;
FStreamCipherId := Value
end;




procedure TCodec.WriteData_Block( Writer: TWriter);
begin
Writer.WriteString( FBlockCipherId)
end;



procedure TCodec.WriteData_Stream( Writer: TWriter);
begin
Writer.WriteString( FStreamCipherId)
end;



procedure TCodec.WriteData_Chain( Writer: TWriter);
begin
Writer.WriteString( FChainId)
end;

end.
