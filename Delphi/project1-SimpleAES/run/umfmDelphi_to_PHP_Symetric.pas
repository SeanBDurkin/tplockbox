unit umfmDelphi_to_PHP_Symetric;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, uTPLb_Codec, uTPLb_BaseNonVisualComponent,
  uTPLb_CryptographicLibrary, ExtCtrls;

type
  TmfmDelphi_to_PHP_Symetric = class(TForm)
    rgTestVectors: TRadioGroup;
    rgChainMode: TRadioGroup;
    edtPassword: TEdit;
    memoPlaintext: TMemo;
    lblPassword: TLabel;
    lblPlaintext: TLabel;
    cryptoMain: TCryptographicLibrary;
    codecAES: TCodec;
    memoOutput: TMemo;
    btnEncrypt: TButton;
    actlstMain: TActionList;
    actEncrypt: TAction;
    edtSeed: TEdit;
    lblSeed: TLabel;
    btnRandomize: TButton;
    actRandomize: TAction;
    rgCipher: TRadioGroup;
    procedure actEncryptUpdate(Sender: TObject);
    procedure actEncryptExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgTestVectorsClick(Sender: TObject);
    procedure rgChainModeClick(Sender: TObject);
    procedure actRandomizeUpdate(Sender: TObject);
    procedure actRandomizeExecute(Sender: TObject);

  private
    procedure LogFmt( const sLine: string; const Args: array of const);
    function  SpaceOut( const sCompacted: string): string;

  public
    { Public declarations }
  end;

var
  mfmDelphi_to_PHP_Symetric: TmfmDelphi_to_PHP_Symetric;

implementation



uses uTPLb_Random, uTPLb_StreamUtils, uTPLb_Constants;

{$R *.dfm}


function StreamToHex( Data: TStream): string;
var
  b: byte;
  sByte: string;
begin
Data.Position := 0;
result := '';
while Data.Read( b, 1) = 1 do
  begin
  sByte := Format( '%x', [b]);
  if Odd( Length( sByte)) then
    sByte := '0' + sByte;
  result := result + sByte
  end
end;


procedure TmfmDelphi_to_PHP_Symetric.actEncryptExecute( Sender: TObject);
const
  TestCaseNames: array[0..2] of string = ('Test Vector 1', 'Test Vector 2', 'Custom');
var
  usPlaintext: UTF8String;
  aCiphertext: ansistring;
  OriginalSeed: int64;
  stmCipher: TStream;
  sHex: string;
begin
memoOutput.Clear;
case rgCipher.ItemIndex of
  0:   codecAES.BlockCipherId := Format( AES_ProgId, [128]);
end;
case rgChainMode.ItemIndex of
  0:   codecAES.ChainModeId := CFB_ProgId;
  1:   codecAES.ChainModeId := CBC_ProgId;
  2:   codecAES.ChainModeId := ECB_ProgId;
  end;
codecAES.UTF8Password := edtPassword.Text;
usPlaintext := memoPlaintext.Lines.Text;
OriginalSeed := StrToInt64( edtSeed.Text);
TRandomStream.Instance.Seed := OriginalSeed;
codecAES.EncryptAnsiString( usPlaintext, aCiphertext);
// NextSeed := TRandomStream.Instance.Seed;
LogFmt( 'Test case = %s', [TestCaseNames[rgTestVectors.ItemIndex]]);
LogFmt( 'Cipher = %s', [codecAES.Cipher]);
LogFmt( 'Chain mode = %s', [codecAES.ChainMode]);
LogFmt( 'PRNG seed = %d', [OriginalSeed]);
LogFmt( 'Passord (UTF-8) = ''%s''', [codecAES.UTF8Password]);

LogFmt( '------------', []);
stmCipher := TMemoryStream.Create;
codecAES.Key.SaveToStream( stmCipher);
sHex := StreamToHex( stmCipher);
stmCipher.Free;
LogFmt( 'key as hex = %s', [sHex]);
LogFmt( 'Plaintext (UTF-8)', []);
LogFmt( '''%s''', [usPlaintext]);
LogFmt( '------------', []);
LogFmt( 'ciphertext (base64) [Includes prepended IV and block quantisation] =', []);
LogFmt( ' ''%s''', [ SpaceOut( aCiphertext)]);
LogFmt( '------------', []);
stmCipher := TMemoryStream.Create;
Base64_to_stream( aCiphertext, stmCipher);
sHex := StreamToHex( stmCipher);
stmCipher.Free;
LogFmt( 'ciphertext (hex) [Includes prepended IV and block quantisation] =', []);
LogFmt( ' ''%s''', [ SpaceOut( sHex)]);
LogFmt( '------------', []);
end;

procedure TmfmDelphi_to_PHP_Symetric.actEncryptUpdate( Sender: TObject);
begin
//
end;

procedure TmfmDelphi_to_PHP_Symetric.actRandomizeExecute(Sender: TObject);
begin
TRandomStream.Instance.Randomize;
edtSeed.Text := IntToStr( TRandomStream.Instance.Seed)
end;

procedure TmfmDelphi_to_PHP_Symetric.actRandomizeUpdate(Sender: TObject);
begin
(Sender as TAction).Enabled := rgTestVectors.ItemIndex = 2
end;

procedure TmfmDelphi_to_PHP_Symetric.FormCreate( Sender: TObject);
begin
memoOutput.Clear;
LogFmt( 'Select test case and chain mode.', []);
LogFmt( 'Enter password and plaintext message and then press the ''Encrypt'' button.', []);
end;

procedure TmfmDelphi_to_PHP_Symetric.LogFmt(
  const sLine: string; const Args: array of const);
begin
memoOutput.Lines.Add( Format( sLine, Args))
end;

procedure TmfmDelphi_to_PHP_Symetric.rgChainModeClick( Sender: TObject);
begin
//
end;

procedure TmfmDelphi_to_PHP_Symetric.rgTestVectorsClick( Sender: TObject);
var
  isCustom: boolean;
begin
case rgTestVectors.ItemIndex of
  0:   begin
       edtPassword.Text := 'Your lips are smoother than vasoline.';
       memoPlaintext.Lines.Text := 'Leeeeeeeeeroy Jenkins!';
         // Above is constrained to:
         //  More than 16 and not a whole multiple of 16 bytes as UTF-8.
       edtSeed.Text := '1';
       rgChainMode.ItemIndex := 0;
       rgCipher.ItemIndex := 0;
       end;
  1:   begin
       edtPassword.Text := 'ORATIO IN L. CATILINAM PRIMA';
       memoPlaintext.Lines.Text := 'Quo usque tandem abutere, Catili';
         // Above is constrained to:
         //  A whole multiple of 16 bytes as UTF-8, excluding the empty case.
       edtSeed.Text := '333';
       rgChainMode.ItemIndex := 0;
       rgCipher.ItemIndex := 0
       end;
  2:   ;
  end;
isCustom := rgTestVectors.ItemIndex = 2;
edtPassword.ReadOnly := not isCustom;
memoPlaintext.ReadOnly := not isCustom;
edtSeed.ReadOnly := not isCustom;
rgChainMode.Enabled := isCustom;
rgCipher.Enabled := isCustom
end;

function TmfmDelphi_to_PHP_Symetric.SpaceOut( const sCompacted: string): string;
const
  NewLineSpacing = 70;
  BunchSpacing = 6;
var
  i, j: integer;
begin
SetLength( result, 2 * Length( sCompacted));
i := 1;
for j := 1 to Length( sCompacted) do
  begin
  if ((j mod NewLineSpacing) = 1) and (j <> 1) then
      begin
      result[i] := #13;
      Inc( i);
      result[i] := #10;
      Inc( i)
      end
    else if ((j mod BunchSpacing) = 1) and (j <> 1) then
      begin
      result[i] := ' ';
      Inc( i)
      end;
  result[i] := sCompacted[j];
  Inc( i)
  end;
SetLength( result, i - 1)
end;

end.
