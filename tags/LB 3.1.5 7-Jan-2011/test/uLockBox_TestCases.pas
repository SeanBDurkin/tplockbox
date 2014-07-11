unit uLockBox_TestCases;
interface
uses TestFramework, uTPLb_Hash, uTPLb_CryptographicLibrary, Classes,
     uTPLb_Codec, uTPLb_StreamCipher, uTPLb_HugeCardinal,
     uTPLb_MemoryStreamPool;

type


TEnvironment_TestCase = class( TTestCase)
  published
    procedure Test_Environment;
  end;






implementation










uses SysUtils, uTPLb_HashDsc, uTPLb_BinaryUtils, uTPLb_StreamUtils,
     uTPLb_ECB, uTPLb_BlockCipher, uTPLb_Random, uTPLb_HugeCardinalUtils,
     uTPLb_IntegerUtils;


{ TTestCaseFirst }


procedure InitUnit_TestCases;
begin
TestFramework.RegisterTest( TEnvironment_TestCase.Suite);
end;

procedure DoneUnit_TestCases;
begin
end;




{ TEnvironment_TestCase }

procedure TEnvironment_TestCase.Test_Environment;
begin
Check( TestFramework.rcs_version = '$Revision: 27 $',
  'These unit tests were ONLY made for revision 27 of D-Unit.');

Check( (System.RTLVersion = 18.00) or (System.RTLVersion = 21.00),
  'These unit tests were ONLY made for Delphi 2007 for win32 (Enterprise edition), and Delphi 2010.')
end;






initialization
InitUnit_TestCases;


finalization
DoneUnit_TestCases;

// TO DO before first public release.
// ==================================
// Implement the SHA-2 family  [It may be feasible to defer this one until after first public release.]
// Develop Introductory Video
// Polish About box
// Develop Demonstration program.
// Make project heads for Delphi versions:
//  - D7          [It may be feasible to defer this one until after first public release.]
//  - D2005       [It may be feasible to defer this one until after first public release.]
//  - D2007

// TO DO after first public release.
// =================================
// Promotion
// Develop wiki documenation for Developers.
// Implement Twofish
// Develop Inno installer script.
// Design and implement Codec Cascadance


end.

