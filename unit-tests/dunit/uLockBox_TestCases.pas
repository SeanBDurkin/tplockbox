unit uLockBox_TestCases;
interface
uses TestFramework, TPLB3.Hash, TPLB3.CryptographicLibrary, Classes,
     TPLB3.Codec, TPLB3.StreamCipher, TPLB3.HugeCardinal,
     TPLB3.MemoryStreamPool;

type

// This test suite is down for maintenance.

TEnvironment_TestCase = class( TTestCase)
  published
    procedure Test_Environment;
  end;






implementation










uses SysUtils, TPLB3.HashDsc, TPLB3.BinaryUtils, TPLB3.StreamUtils,
     TPLB3.ECB, TPLB3.BlockCipher, TPLB3.Random, TPLB3.HugeCardinalUtils,
     TPLB3.IntegerUtils, TPLB3.SVN_Keywords, Dialogs, TPLB3.PointerArithmetic;


{ TTestCaseFirst }


procedure InitUnit_TestCases;
begin
 // TestFramework.RegisterTest( TEnvironment_TestCase.Suite);
 // Uncomment the above when the new version is ready.
end;

procedure DoneUnit_TestCases;
begin
end;




function rcs_version_AsInteger: integer;
var
  s, Pattern: string;
  P, Code: integer;
begin
s := TestFramework.rcs_version;
// s like '$Revision: 27 $' or '$Revision: 41 $'
Pattern := '$Revision: ';
P := Pos( Pattern, s);
if P > 0 then
  Delete( s, P, Length( Pattern));
Pattern := ' ';
P := Pos( Pattern, s);
if P > 0 then
  SetLength( s, P-1);
Val( s, result, Code);
if (s = '') or (Code <> 0) then
  result := -1 // Signifying unknown version
end;


function DelphiVersion_DisplayName: string;
begin
result := '';
{$ifdef VER210}
result := 'Delphi 2010';
{$endif}

{$ifdef VER200}
result := 'Delphi 2009';
{$endif}

{$ifdef VER190}
result := 'Delphi 2007 for .NET';
{$endif}

{$ifdef VER180}
  {$ifdef VER185}
   result := 'Delphi 2007';
  {$else}
   result := 'Delphi 2006';
  {$endif}
{$endif}

{$ifdef VER170}
result := 'Delphi 2005';
{$endif}

{$ifdef VER160}
result := 'Delphi 8 for .NET';
{$endif}

{$ifdef VER150}
result := 'Delphi 7';
{$endif}

{$IF compilerversion < 15}
 // Delphi 6 or earlier
result := 'Archaic version of Delphi (not supported).';
{$IFEND}

{$IF (compilerversion >= 22.0) and (compilerversion < 23.0)}
result := 'Delphi XE';
{$IFEND}

{$IF (compilerversion >= 23.0) and (compilerversion < 24.0)}
  result := 'Delphi XE2';
{$IFEND}

{$IF (compilerversion >= 24.0) and (compilerversion < 25.0)}
  result := 'Delphi XE3';
{$IFEND}

{$IF (compilerversion >= 25.0) and (compilerversion < 26.0)}
  result := 'Delphi XE4';
{$IFEND}

{$IF (compilerversion >= 26.0) and (compilerversion < 27.0)}
  result := 'Delphi XE5';
{$IFEND}

{$IF (compilerversion >= 27.0) and (compilerversion < 28.0)}
  result := 'Delphi XE6';
{$IFEND}

{$IF (compilerversion >= 23.0) and (compilerversion < 28.0)}
  {$IFDEF WIN32}
  result := result + ' Win 32-bit';
  {$ENDIF}

  {$IFDEF WIN64}
  result := result + ' Win 64-bit';;
  {$ENDIF}

  {$IFDEF IOS}
  result := result + ' iOX';
  {$ENDIF}

  {$IFDEF MACOS}
  result := result + ' OS X';
  {$ENDIF}

  {$IFDEF ANDROID}
  result := result + ' Android';
  {$ENDIF}
{$IFEND}


{$IF compilerversion >= 28.0}
result := 'Unrecognised version of Delphi later than Delphi XE6';
{$IFEND}

if result = '' then
  result := 'Unrecognised version of Delphi';
end;


{ TEnvironment_TestCase }

procedure TEnvironment_TestCase.Test_Environment;
var
  Ver: integer;
  Announcement: string;

begin
Ver := rcs_version_AsInteger;
if System.RTLVersion >= 19.00 then
    Check( (Ver >= 27) and (Ver <= 41),
      'These unit tests were ONLY made for revisions 27 through to 41 of D-Unit.')
  else
    Check( Ver = 36,
      'For D7 and D2007 these unit tests were ONLY made for revision 36 of D-Unit.');
      // Nota bene: revision 41 is not compatible with Delphi 7.

Check( (System.RTLVersion = 15.00) or
       (System.RTLVersion = 17.00) or
       (System.RTLVersion = 18.00) or
       (System.RTLVersion = 21.00) or
       (System.RTLVersion = 23.00),
  'These unit tests were ONLY made for Delphi 7, Delphi 2005, Delphi 2007 for win32 ' +
  '(Enterprise edition), Delphi 2010, Delphi XE2 win 32-bit and Delphi win XE 64-bit.');

if SizeOf( TrueNativeInt ) <> SizeOf( pointer) then
  ShowMessageFmt( 'Integer   = %d bytes'#13#10 +
                  'NativeInt = %d bytes'#13#10 +
                  'TrueNativeInt = %d bytes'#13#10 +
                  'Pointer = %d bytes'#13#10 +
                  'Compiler = %.2f bytes',
  [SizeOf(Integer), SizeOf( NativeInt), SizeOf( TrueNativeInt), SizeOf( pointer), CompilerVersion]);
Check( (SizeOf( TrueNativeInt ) = SizeOf( pointer)) and
       (SizeOf( TrueNativeUInt) = SizeOf( pointer)),
       'uTPLb_PointerArithmetic Native Integer definitions wrong.');

Announcement := Format( 'You are running DUnit at SVN revision %d.', [Ver]);
Announcement := Announcement + #13#10;
Announcement := Announcement + Format( 'The SVN revision of the ' +
 'uTPLb_SVN_Keywords unit of the TPLB3 run-time package is %d.'#13#10 +
 'Check the TPLB3 project options for the run-time package library version.',
 [TPLB3Runtime_SVN_Revision]);


Announcement := Announcement + #13#10;
Announcement := Announcement + Format( 'You are testing with compiler %s',
 [DelphiVersion_DisplayName]);

ShowMessage( Announcement);
end;






initialization
InitUnit_TestCases;


finalization
DoneUnit_TestCases;



end.

