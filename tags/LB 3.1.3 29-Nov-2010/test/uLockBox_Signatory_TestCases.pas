unit uLockBox_Signatory_TestCases;
interface
uses TestFramework, uTPLb_MemoryStreamPool, Classes, uTPLb_Signatory,
     uTPLb_Codec, uTPLb_CryptographicLibrary, uTPLb_Random;

const

Key1: ansistring =
'gAAAABVmVoAtVC4GOZi1ue+wlopkpnPkpp00bgzW5fphnNmG8ulfRoPPe9l1Asp+kfZEikoh' +
'SRmcPNW3yOKwNMyqZCFY0CWOjnChXgytNPaMYyYbWZZ/v9LNv6zZ0ucpThUuw4Prz5JJP7a7' +
'o5NDlIWD73gYCixJqJD5a0aXkWYqUG+EAwAAAAEAAYAAAADnwM7wBHZVUpEi9H+O5urr7+1q' +
'YuJtH57gYHDtw/N6I+FC2BxQhYIGydxvKOx87+z6aPYUVaLVc4NmSCL02DeO1vNlF5lRYhA0' +
'/nMChO9nCtdvF39OdD7qbSVWlnyl/tEUXRAFsxC7DzgKyrFck0rJAaEvRspL+eOZXcNCJeUU' +
'FYAAAADl+xGZxpkIb/VWE2943aqWQRpTET/3sjHXG+/ckZ1heGoXwtrUeQ5iztVT7oIg1mbE' +
'cl9b2Y6q2jjGynOMT6VNYDpQUqneDQ7e6LQXtukkJTOrT0SAF/2QYLxJ8fs5BE5ImeDRwb90' +
'WlkHWYGXQklRF71ONBXyvMTzwVYxKjuXiwMAAAABAAF/AAAAxWlNOs6snpUASKl0Q+iGbTF9' +
'sxEkQx/+d4Vb9kJ4GuyfRGC74yd7zgz9r5P/cwezX6jOjJqws8ZV9IeNb+UhepJ0rrAeFB1p' +
'0IINXXS6u8cw4PDZ7bwJUt/nYLH+tt14ZhYN9Lf1LXq6trqnXbA5CBwvseS7zkcDwvGMefWR' +
'wQ==';

type
TRSASignatory_TestCase = class( TTestCase)
  private
    FSeed_StandardTest: integer;
    FDocumentSize_StandardTest: integer;

  protected
    FKey: TStream;
    FSig: TSignatory;
    FcodecRSA: TCodec;
    FLib: TCryptographicLibrary;
    FDocument: TMemoryStream;
    FSignature: TStream;
    FRand: TRandomStream;

    procedure SetUp; override;
    procedure TearDown; override;
    function  OnProgress( Sender: TObject; CountBytesProcessed: Int64): Boolean;
    procedure Generic_Signatory_InversionTests( Seed: uint64{=-1 for random}; DocumentSize: integer);

  published
    procedure Signatory_InversionTests;
  end;

implementation




















uses SysUtils, uTPLb_StreamUtils, uTPLb_Constants, uTPLb_Asymetric;


procedure InitUnit_Signatory_TestCases;
begin
TestFramework.RegisterTest( TRSASignatory_TestCase.Suite)
end;



procedure DoneUnit_Signatory_TestCases;
begin
end;



{ TRSASignatory_TestCase }

procedure TRSASignatory_TestCase.SetUp;
begin
FKey := TMemoryStream.Create;
Base64_to_stream( Key1, FKey);
FKey.Position := 0;
FSig := TSignatory.Create( nil);
FcodecRSA := TCodec.Create( nil);
FLib := TCryptographicLibrary.Create( nil);
FcodecRSA.CryptoLibrary  := FLib;
FcodecRSA.StreamCipherId := RSA_ProgId;
FcodecRSA.OnProgress := OnProgress;
FSig.Codec := FcodecRSA;
FSig.LoadKeysFromStream( FKey, [partPublic, partPrivate]);
FKey.Position := 0;
FDocument  := TMemoryStream.Create;
FSignature := TMemoryStream.Create;
FSeed_StandardTest := 1;
FDocumentSize_StandardTest := 4000;
FRand := TRandomStream.Instance
end;




procedure TRSASignatory_TestCase.Signatory_InversionTests;
begin
Generic_Signatory_InversionTests( FSeed_StandardTest, FDocumentSize_StandardTest)
end;



procedure TRSASignatory_TestCase.Generic_Signatory_InversionTests(
  Seed: uint64; DocumentSize: integer);
begin
if Seed = -1 then
    FRand.Randomize
  else
    FRand.Seed := Seed;
FDocument.Size := DocumentSize;
RandomFillStream( FDocument);
FDocument.Position := 0;
FSignature.Size := 0;
Check( FSig.Sign( FDocument, FSignature), 'RSA signature generation failed.');
DisplayStream( FSignature);
Check( FSig.Verify( FDocument, FSignature)<>vFail, 'RSA signature verification failed.')
end;





function TRSASignatory_TestCase.OnProgress( Sender: TObject; CountBytesProcessed: Int64): Boolean;
begin
result := True
end;





procedure TRSASignatory_TestCase.TearDown;
begin
FreeAndNil( FSig);
FreeAndNil( FcodecRSA);
FreeAndNil( FLib);
FreeAndNil( FKey);
FreeAndNil( FDocument);
FreeAndNil( FSignature)
end;




initialization
InitUnit_Signatory_TestCases;


finalization
DoneUnit_Signatory_TestCases;

end.
