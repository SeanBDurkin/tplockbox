unit uCSharpDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uTPLb_Codec, uTPLb_BaseNonVisualComponent, uTPLb_CryptographicLibrary,
  ImgList, PlatformDefaultStyleActnCtrls, ActnList, ActnMan, StdCtrls, ToolWin,
  ActnCtrls, ExtCtrls, StdActns;

type
  TmfmCSharpDemo = class(TForm)
    CryptLib: TCryptographicLibrary;
    Codec: TCodec;
    pnlControls: TFlowPanel;
    lblReferencePlaintext: TLabel;
    edtReferencePlaintext: TButtonedEdit;
    lblCiphertext: TLabel;
    edtCiphertext: TButtonedEdit;
    lblReconstructedPlaintext: TLabel;
    edtReconstructedPlaintext: TButtonedEdit;
    tlbrMain: TActionToolBar;
    memoLog: TMemo;
    actmngrMainActions: TActionManager;
    imglstGlyphs16x16: TImageList;
    dlgOpenReadFile: TOpenDialog;
    dlgSelectReconFile: TOpenDialog;
    actTest: TAction;
    FileExit1: TFileExit;
    procedure edtReferencePlaintextRightButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actTestExecute(Sender: TObject);

  private
    procedure DecryptFile( const Ciphertext, ReconPlaintext: string);
    procedure Put( const Line: string);
    procedure PutFmt( const Line: string; const Args: array of const);
    function  PlaintextFN: string;
    function  CiphertextFN: string;
    function  ReconFN: string;
    function  GetEditFileName( Control: TButtonedEdit; DlgWithIni: TOpenDialog): string;

  public
    { Public declarations }
  end;

var
  mfmCSharpDemo: TmfmCSharpDemo;

implementation

{$R *.dfm}


// CSharp Code (Scott Sedgwick)
//============================================================================
//using System;
//using System.IO;
//using System.Security.Cryptography;
//
//namespace Cochlear.CustomSound.DataAccessLayer.DataExchange
//{
//    public class RijndaelDataExchangeFileEncryption : IDataFileEncryption
//    {
//        public static readonly byte[] CryptographicKey = new byte[16];
//
//        private delegate ICryptoTransform CreateCryptTransform(RijndaelManaged rijndael);
//
//        public void Encrypt(Stream source, Stream destination)
//        {
//            ApplyCryptoTransformToStream(source, destination, rijndael => rijndael.CreateEncryptor());
//        }
//
//        public void Decrypt(Stream source, Stream destination)
//        {
//            ApplyCryptoTransformToStream(source, destination, rijndael => rijndael.CreateDecryptor());
//        }
//
//        private static void ApplyCryptoTransformToStream(Stream source, Stream destination, CreateCryptTransform createCryptTransform)
//        {
//            if (source == null)
//            {
//                throw new ArgumentNullException("source");
//            }
//            if (destination == null)
//            {
//                throw new ArgumentNullException("destination");
//            }
//
//            using (var rijndael = new RijndaelManaged())
//            {
//                rijndael.Key = CryptographicKey;
//                rijndael.Mode = CipherMode.ECB;
//                rijndael.Padding = PaddingMode.Zeros;
//
//                var decryptorStream = new CryptoStream(destination, createCryptTransform(rijndael), CryptoStreamMode.Write);
//                StreamUtil.WriteTo(decryptorStream, source);
//                decryptorStream.FlushFinalBlock();
//            }
//            destination.Seek(0, SeekOrigin.Begin);
//        }
//    }
//}
//============================================================================



uses ShLwApi, StrUtils, uCSharpZeroPadding;


function CompareFiles( const f1, f2: string): boolean;
var
  s1, s2: TStream;
  buff1, buff2: TBytes;
  L: integer;
  s1L: integer;
begin
L := 1024;
SetLength( buff1, L);
SetLength( buff2, L);
s1 := TFileStream.Create( f1, fmOpenRead);
try
s2 := TFileStream.Create( f2, fmOpenRead);
try
 result := s1.Size = s2.Size;
 if result then
   repeat
     s1L := s1.Read( buff1[0], L);
     result  := s1L = s2.Read( buff2[0], L);
     if result and (s1L > 0) then
       result := CompareMem( @buff1[0], @buff2[0], s1L)
   until (not result) or (s1L < L)
finally
s2.Free
end
finally
s1.free
end;
end;

procedure TmfmCSharpDemo.actTestExecute(Sender: TObject);
var
  s1: TStream;
  Ok: boolean;
  Fill, j: integer;
  zero: byte;
begin
Put( 'Decryption using AES-128, ECB, ISO 9797-1 method 2 padding, no IV, no nonce, no salt.');
DecryptFile( CiphertextFN, ReconFN);
PutFmt( 'Decrypted "%s" and put the reconstructed plaintext into file "%s".', [CiphertextFN, ReconFN]);
PutFmt( 'Now comparing recon file with reference plaintext "%s"',[PlaintextFN]);
Ok := CompareFiles( ReconFN, PlaintextFN);
if not Ok then
  begin
  // Adjust for incorrect decryption on the C# side which fails to strip the zeros.
  // Emulate C# buggy behaviour for compatibility.
  s1 := TFileStream.Create( ReconFN, fmOpenReadWrite);
  Fill := 16 - (s1.Size mod 16);
  if Fill <> 16 then
    begin
    s1.Seek( 0, soEnd);
    zero := 0;
    for j := 1 to Fill do
      s1.Write( Zero, 1)
    end;
  s1.Free
  end;
Ok := CompareFiles( ReconFN, PlaintextFN);
PutFmt( 'Result of test = %s', [IfThen( Ok, 'PASS', 'FAIL')])
end;


function RelToAbs( const Base, Rel: string): string;
begin
SetLength( result, MAX_PATH);
PathCanonicalize( @result[1], PChar( IncludeTrailingBackslash( Base) + Rel));
result := StrPas( PChar( result))
end;

function TmfmCSharpDemo.GetEditFileName( Control: TButtonedEdit; DlgWithIni: TOpenDialog): string;
var
  Base: string;
begin
result := Trim( Control.Text);
if (result <> '') and (ExtractFilePath( result) = '') then
  begin
  Base := DlgWithIni.InitialDir;
  if Pos('..',Base)=1 then
    Base := RelToAbs( ExtractFilePath(Application.ExeName), Base);
  result := RelToAbs( Base, result)
  end;
end;

function TmfmCSharpDemo.CiphertextFN: string;
begin
result := GetEditFileName( edtCiphertext, dlgOpenReadFile)
end;

function TmfmCSharpDemo.PlaintextFN: string;
begin
result := GetEditFileName( edtReferencePlaintext, dlgOpenReadFile)
end;

function TmfmCSharpDemo.ReconFN: string;
begin
result := GetEditFileName( edtReconstructedPlaintext, dlgSelectReconFile)
end;


procedure TmfmCSharpDemo.DecryptFile( const Ciphertext, ReconPlaintext: string);
const
  KEY  : array[0..15] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);
var
  InStream, OutStream: TStream;
  ms: TMemoryStream;
begin
Codec.Reset;
ms := TMemoryStream.Create;
try
  ms.Write(KEY, 16);
  ms.Position := 0;
  Codec.InitFromStream(ms);
finally
  ms.Free;
end;
Codec.DecryptFile( ReconPlaintext, Ciphertext)
end;


procedure TmfmCSharpDemo.edtReferencePlaintextRightButtonClick( Sender: TObject);
var
  SenderEdit: TButtonedEdit;
  Dialog: TOpenDialog;
  Title: string;
begin
SenderEdit := Sender as TButtonedEdit;
if SenderEdit = edtReferencePlaintext then
    begin
    Dialog := dlgOpenReadFile;
    Title := 'Select reference plaintext'
    end
  else if Sender = edtCiphertext then
    begin
    Dialog := dlgOpenReadFile;
    Title := 'Select ciphertext (input)'
    end
  else if Sender = edtReconstructedPlaintext then
    Dialog := dlgSelectReconFile;
if Title <> '' then
  Dialog.Title := Title;
Dialog.FileName := SenderEdit.Text;
if Dialog.Execute then
  SenderEdit.Text := Dialog.FileName
end;


procedure TmfmCSharpDemo.FormCreate( Sender: TObject);
begin
memoLog.Clear;
PutFmt( 'Welcome to %s .', [ExtractFileName( Application.ExeName)]);
CryptLib.RegisterStreamCipher( TCSharpZeroPadding.Create);
Codec.StreamCipherId := 'Sedgwick.StreamToBlock'
end;

procedure TmfmCSharpDemo.Put( const Line: string);
begin
memoLog.Lines.Add( Line)
end;

procedure TmfmCSharpDemo.PutFmt( const Line: string; const Args: array of const);
begin
Put( Format( Line, Args))
end;


end.
