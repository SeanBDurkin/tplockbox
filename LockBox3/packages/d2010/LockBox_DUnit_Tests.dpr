program LockBox_DUnit_Tests;

uses
  Forms,
  GUITestRunner,
  TestFramework,
  uLockBox_TestCases in '..\..\unit-tests\dunit\uLockBox_TestCases.pas',
  uLockBox_Signatory_TestCases in '..\..\unit-tests\dunit\uLockBox_Signatory_TestCases.pas',
  uLockBox_RSA_TestCases in '..\..\unit-tests\dunit\uLockBox_RSA_TestCases.pas',
  uLockBox_OpenSSLTestCases in '..\..\unit-tests\dunit\uLockBox_OpenSSLTestCases.pas',
  uLockBox_HugeCardinalTestCases in '..\..\unit-tests\dunit\uLockBox_HugeCardinalTestCases.pas',
  uLockBox_Hashes in '..\..\unit-tests\dunit\uLockBox_Hashes.pas',
  uLockBox_CipherTestCases in '..\..\unit-tests\dunit\uLockBox_CipherTestCases.pas';

{$R *.res}

begin
Application.Initialize;
GUITestRunner.RunRegisteredTests
end.
