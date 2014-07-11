unit uTPLb_CodecIntf;
interface
uses Classes, uTPLb_StreamCipher, uTPLb_BlockCipher,
     uTPLb_CryptographicLibrary;

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
    function  GetKey: TSymetricKey;

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
      SigningKeys_PrivatePart: TObject;  // Must be uTPLb_Asymetric.TAsymtricKeyPart
      var wasAborted: boolean);

    function VerifySignature(
      Document, Signature: TStream;
      ProgressSender: TObject;
      ProgressEvent: TOnEncDecProgress;
      SigningKeys_PublicPart: TObject; // Must be uTPLb_Asymetric.TAsymtricKeyPart
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

    function  GetAborted: boolean;
    procedure SetAborted( Value: boolean);

    property  Mode: TCodecMode                   read GetMode;
    property  Key: TSymetricKey                  read GetKey;
    property  StreamCipher: IStreamCipher        read GetStreamCipher write SetStreamCipher;
    property  BlockCipher : IBlockCipher         read GetBlockCipher  write SetBlockCipher;
    property  ChainMode   : IBlockChainingModel  read GetChainMode    write SetChainMode;
    property  OnProgress  : TOnEncDecProgress    read GetonProgress   write SetOnProgress;
    property  AsymetricKeySizeInBits: cardinal   read GetAsymetricKeySizeInBits
                                                 write SetAsymetricKeySizeInBits;
    property  OnAsymGenProgress: TGenerateAsymetricKeyPairProgress
                                           read GetAsymGenProgressEvent write SetAsymGenProgressEvent;
    property isUserAborted: boolean              read GetAborted write SetAborted;
  end;


implementation

end.
