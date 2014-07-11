unit umfmMakeSampleKey;

interface

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uTPLb_MemoryStreamPool, uTPLb_Signatory, uTPLb_Codec,
  uTPLb_BaseNonVisualComponent, uTPLb_CryptographicLibrary, uTPLb_Hash;

type
  TmfmMakeSampleKey = class(TForm)
    btnGenRSA: TButton;
    memoOutput: TMemo;
    btnGenAES256: TButton;
    btnAbort: TButton;
    lblCountPrimalityTests: TLabel;
    lblCountPrimalityTestsValue: TLabel;
    lblKeySize: TLabel;
    edtKeySize: TEdit;
    CryptographicLibrary1: TCryptographicLibrary;
    Codec1: TCodec;
    btnGenCompRSAKeys: TButton;
    Hash1: THash;
    Signatory1: TSignatory;
    procedure btnGenRSAClick(Sender: TObject);
    procedure btnGenAES256Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnGenCompRSAKeysClick(Sender: TObject);
    function Codec1Progress(Sender: TObject;
      CountBytesProcessed: Int64): Boolean;

  private
    FPool: TMemoryStreamPool;
    FNumbersTested: integer;
    FwasAborted: boolean;
    FisGeneratingKeys: boolean;

    procedure GenRSA_Progress(
      Sender: TObject; BitsProcessed, TotalBits: int64;
      var doAbort: boolean);

    procedure GenRSA_TPrimalityTestNotice( CountPrimalityTests: integer);

    procedure Put( const Line: string);
    procedure PutFmt( const Line: string; const Args: array of const);
    procedure SetNumbersTested( Value: integer);

  public
    property PrimalityTestCount: integer  read FNumbersTested
                                          write SetNumbersTested;
  end;

var
  mfmMakeSampleKey: TmfmMakeSampleKey;

implementation















uses SysUtils, uTPLb_RSA_Primitives, uTPLb_HugeCardinalUtils,
     uTPLb_HugeCardinal, uTPLb_StreamUtils, uTPLb_AES, uTPLb_HashDsc,
     uTPLb_BlockCipher, uTPLb_StreamCipher, uTPLb_Random,
     uTPLb_Asymetric;
{$R *.dfm}

const
  MinBits = 256;
  MaxBits = 4096;


procedure TmfmMakeSampleKey.FormCreate( Sender: TObject);
begin
FPool := TMemoryStreamPool.Create;
FwasAborted := False;
FNumbersTested := 0;
memoOutput.Clear;
FisGeneratingKeys := False
end;

procedure TmfmMakeSampleKey.FormDestroy( Sender: TObject);
begin
FPool.Free
end;


procedure TmfmMakeSampleKey.btnGenRSAClick( Sender: TObject);
var
  RequiredBitLengthOfN: integer;
  N, e, d, Totient: THugeCardinal;
  S: TDateTime;
  LocalNumbersTested: integer;
  LocalWasAborted: boolean;
  Secs: integer;
  Tmp: TStream;
  Code: integer;
  procedure ReportHugeCardinal( const Label1: string; Value: THugeCardinal);
  var
    B64: ansistring;
  begin
  Tmp.Size := 0;
  Value.StreamOut( LittleEndien, Tmp, (Value.BitLength + 7) div 8);
  B64 := Stream_to_Base64( Tmp);
  PutFmt( '%s (little-endien; base64) = "%s";', [Label1, B64]);
  Put( '')
  end;

begin
FwasAborted := False;
LocalWasAborted := False;
FNumbersTested := 0;
LocalNumbersTested := 0;
btnAbort.Enabled := True;
Val( edtKeySize.Text, RequiredBitLengthOfN, Code);
if (edtKeySize.Text='') or (Code <> 0) or
   (RequiredBitLengthOfN < MinBits) or
   (RequiredBitLengthOfN > MaxBits) then
  begin
  PutFmt( 'Key size ("%s") invalid or out of range.',
    [edtKeySize.Text]);
  PutFmt( 'Correct range is %d bits to %d bits.',
    [MinBits, MaxBits]);
  exit
  end;
Tmp := FPool.NewMemoryStream( 0);
try
PutFmt( 'Now generating an RSA-%d key pair', [RequiredBitLengthOfN]);
S := Now;
Compute_RSA_Fundamentals_2Factors(
  RequiredBitLengthOfN, StandardExponent,
  N, e, d, Totient,
  GenRSA_Progress, GenRSA_TPrimalityTestNotice,
  5, FPool,
  LocalNumbersTested, LocalWasAborted);
if LocalWasAborted then
    Put( 'Generation aborted by user.')
  else
    begin
    Put( 'Generation completed.');
    Secs := Round( (Now - S) * SecsPerDay);
    PutFmt( 'Generation took %ds.', [Secs]);
    PutFmt( '%d primality tests were conducted to reach this goal,', [LocalNumbersTested]);
    PutFmt( 'at a rate of %.1f tests per second.', [LocalNumbersTested / Secs]);
    Put( '');
    PutFmt( 'n has %d bits.', [N.BitLength]);
    PutFmt( 'e has %d bits.', [e.BitLength]);
    PutFmt( 'd has %d bits.', [d.BitLength]);
    ReportHugeCardinal( 'n', n);
    ReportHugeCardinal( 'e', e);
    PutFmt( 'e (decimal) = %d', [StandardExponent]);
    Put( '');
    ReportHugeCardinal( 'd', d);
    Put( '')
    end;
finally
Tmp.Free;
btnAbort.Enabled := False;
N.Free;
e.Free;
d.Free;
Totient.Free;
end end;


procedure TmfmMakeSampleKey.btnGenCompRSAKeysClick( Sender: TObject);
var
  Mem: TStream;
  RequiredBitLengthOfN, Code: integer;
begin
FisGeneratingKeys := True;
Mem := TMemoryStream.Create;
try
  Val( edtKeySize.Text, RequiredBitLengthOfN, Code);
  if (edtKeySize.Text='') or (Code <> 0) then
    begin
    Put( '');
    exit;
    end;
  if RequiredBitLengthOfN < MinBits then
    RequiredBitLengthOfN := MinBits;
  if RequiredBitLengthOfN > MaxBits then
    RequiredBitLengthOfN := MaxBits;
  PutFmt( 'Generating two key pairs of size %d.', [RequiredBitLengthOfN]);
  Signatory1.Codec.AsymetricKeySizeInBits := RequiredBitLengthOfN;
  Signatory1.GenerateKeys;
  Signatory1.StoreKeysToStream( Mem, [partPublic]);
  PutFmt( 'Public key = "%s"', [Stream_to_Base64( Mem)])
finally
  FisGeneratingKeys := False;
  Mem.Free
end end;


function TmfmMakeSampleKey.Codec1Progress(
  Sender: TObject; CountBytesProcessed: Int64): Boolean;
begin
result := True;
if FisGeneratingKeys then
  GenRSA_TPrimalityTestNotice(
    Codec1.FGenerateAsymetricKeyPairProgress_CountPrimalityTests)
end;


procedure TmfmMakeSampleKey.GenRSA_Progress(
  Sender: TObject; BitsProcessed, TotalBits: int64; var doAbort: boolean);
begin
Application.ProcessMessages;
doAbort := FwasAborted
end;

procedure TmfmMakeSampleKey.GenRSA_TPrimalityTestNotice(
  CountPrimalityTests: integer);
begin
Application.ProcessMessages;
PrimalityTestCount := CountPrimalityTests
end;

procedure TmfmMakeSampleKey.Put(const Line: string);
begin
memoOutput.Lines.Add( Line)
end;



procedure TmfmMakeSampleKey.PutFmt(
  const Line: string; const Args: array of const);
begin
Put( Format( Line, Args))
end;



procedure TmfmMakeSampleKey.SetNumbersTested( Value: integer);
begin
FNumbersTested := Value;
lblCountPrimalityTestsValue.Caption := Format( '%d', [FNumbersTested])
end;


procedure TmfmMakeSampleKey.btnAbortClick( Sender: TObject);
begin
FwasAborted := True;
(Sender as TButton).Enabled := False
end;



procedure TmfmMakeSampleKey.btnGenAES256Click( Sender: TObject);
var
  AES: IBlockCipher;
  KeyStream: TMemoryStream;
  Tmp: TStream;
  Key: TSymetricKey;
  KeySize: integer;

begin
KeyStream := nil;
Tmp := nil;
Key := nil;
KeySize := 256;
AES := TAES.Create( KeySize) as IBlockCipher;
try
KeyStream := FPool.NewMemoryStream( AES.KeySize div 8);
RandomFillStream( KeyStream);
KeyStream.Position := 0;
Key := AES.GenerateKey( KeyStream);
Tmp := FPool.NewMemoryStream( 0);
Key.SaveToStream( Tmp);
PutFmt( 'AES-%d key (base64) = "%s";', [KeySize, Stream_to_Base64( Tmp)]);
finally
Tmp.Free;
Key.Free;
KeyStream.Free
end end;


end.
