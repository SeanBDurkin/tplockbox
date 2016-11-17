unit uLockBox_TestCases;
interface
uses
  DUnitM.UnitTestFramework, DUnitM.StringUtils, Xml.XMLIntf,
  DUnitM.DDExtensions;

type

TTestCase = class( TObject)
  public
    Assert: IAssert;
  end;

[TestFixture('General')]
[Description('Tests about identity and environment')]
TEnvironment_TestCase = class( TTestCase)
  published
    [Test]
    [Description('Compiler version')]
    procedure Test_Environment;
  end;






implementation










uses SysUtils, TPLB3.HashDsc, TPLB3.BinaryUtils, TPLB3.StreamUtils,
     TPLB3.ECB, TPLB3.BlockCipher, TPLB3.Random, TPLB3.HugeCardinalUtils,
     TPLB3.IntegerUtils, TPLB3.SVN_Keywords, Dialogs, TPLB3.PointerArithmetic;






function DelphiVersion_DisplayName: string;
begin
if CompilerVersion > 31.0 then
  result := 'post Delphi 10.1'
else if CompilerVersion = 31.0 then
  result := 'Delphi 10.1 Berlin'
else if CompilerVersion = 30.0 then
  result := 'Delphi 10 Seattle'
else if CompilerVersion = 29.0 then
  result := 'Delphi XE8'
else if CompilerVersion = 28.0 then
  result := 'Delphi XE7'
else if CompilerVersion = 27.0 then
  result := 'Delphi XE6'
else if CompilerVersion = 26.5 then
  result := 'AppMethod 1'
else if CompilerVersion = 26.0 then
  result := 'Delphi XE5'
else if CompilerVersion = 25.0 then
  result := 'Delphi XE4'
else if CompilerVersion = 24.0 then
  result := 'Delphi XE3'
else if CompilerVersion = 23.0 then
  result := 'Delphi XE2'
else if CompilerVersion = 22.0 then
  result := 'Delphi XE'
else if CompilerVersion = 21.0 then
  result := 'Delphi 2010'
else if CompilerVersion = 20.0 then
  result := 'Delphi 2009'
else if CompilerVersion = 19.0 then
  result := 'Delphi 2007 .NET'
else if CompilerVersion = 18.5 then
  result := 'Delphi 2007'
else if CompilerVersion = 18.0 then
  result := 'Delphi 2006'
else if CompilerVersion = 17.0 then
  result := 'Delphi 2005'
else if CompilerVersion = 16.0 then
  result := 'Delphi 8 .NET'
else if CompilerVersion = 15.0 then
  result := 'Delphi 7'
else if CompilerVersion = 14.0 then
  result := 'Delphi 6'
else
  result := 'Unrecognised version of Delphi'
end;



procedure TEnvironment_TestCase.Test_Environment;
begin
Assert.AreEqual( SizeOf( TrueNativeInt ), SizeOf( pointer), 'uTPLb_PointerArithmetic Native Integer definitions wrong.');
Assert.AreEqual( SizeOf( TrueNativeUInt), SizeOf( pointer), 'uTPLb_PointerArithmetic Native Integer definitions wrong.');
if CompilerVersion <> 31.0 then
  Assert.Warn( Format( 'This test suite was built for Delphi 10.1 Berlin. You are using %s', [DelphiVersion_DisplayName]));
end;







end.

