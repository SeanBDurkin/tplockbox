program LockBox_DUnit_Tests;

uses
  Forms,
  TestFramework,
  GUITestRunner,
  uLockBox_CipherTestCases in '..\..\unit-tests\dunit\uLockBox_CipherTestCases.pas',
  uLockBox_HugeCardinalTestCases in '..\..\unit-tests\dunit\uLockBox_HugeCardinalTestCases.pas',
  uLockBox_RSA_TestCases in '..\..\unit-tests\dunit\uLockBox_RSA_TestCases.pas',
  uLockBox_Signatory_TestCases in '..\..\unit-tests\dunit\uLockBox_Signatory_TestCases.pas',
  uLockBox_Hashes in '..\..\unit-tests\dunit\uLockBox_Hashes.pas',
  TPLB3.SVN_Keywords in '..\..\run\TPLB3.SVN_Keywords.pas',
  uLockBox_OpenSSLTestCases in '..\..\unit-tests\dunit\uLockBox_OpenSSLTestCases.pas',
  TPLB3.OpenSSL in '..\..\run\TPLB3.OpenSSL.pas',
  uLockBox_TestCases in '..\..\unit-tests\dunit\uLockBox_TestCases.pas';

{$R *.res}

begin
Application.Initialize;
GUITestRunner.RunRegisteredTests
end.
