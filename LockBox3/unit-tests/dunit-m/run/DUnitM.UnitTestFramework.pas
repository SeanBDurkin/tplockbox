{***************************************************************************}
{                                                                           }
{        DUnit-M                                                            }
{                                                                           }
{        Copyright (C) 2015 Sean B. Durkin                                  }
{                                                                           }
{        Author: Sean B. Durkin                                             }
{        sean@seanbdurkin.id.au                                             }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{***************************************************************************}
{                                                                           }
{               NOTICE of DERIVATION and CHANGE                             }
{                                                                           }
{  This project is a derived work. It is dervied from the DUnitX library    }
{  created By Vincent Parrett                                               }
{  (hosted at https://github.com/VSoftTechnologies/DUnitX).                 }
{  The copyright holder for the original code is as per the following       }
{  comment block. The copyright holder for all changes in this file from    }
{  that base is as denoted following.                                       }
{                                                                           }
{        Copyright (C) 2015 Sean B. Durkin                                  }
{                                                                           }
{        Author: Sean B. Durkin                                             }
{        sean@seanbdurkin.id.au                                             }
{***************************************************************************}

// The Original DUnitX comment header was ....

{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2012 Vincent Parrett                              }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{ Contributors : Vincent Parrett                                            }
{                Jason Smith                                                }
{                Nick Hodges                                                }
{                Nicholas Ring                                              }
{                                                                           }
{***************************************************************************}

// DUnit-M is a unit testing framework for software written in and for Delphi XE7
// or later. Some of the files from this project are either copied or derived
// from the DUnitX project.

unit DUnitM.UnitTestFramework;

interface

uses SBD.Messages, SBD.Generics, Classes, SysUtils, RTTI;

{$I ../includes/DUnitM.inc}

const
  // Message URL for framework messages:
  DUnixtM_Framework_URL = 'http://www.seanbdurkin.id.au/pascaliburnus2';
  DUnixtM_Framework_Prefix = 'dm';

  // Message codes for framework messages:
  UserIssuedWarning = 1;
  TestFailure = 2;
  TestError = 3;
  IgnoredTestCase = 4;
  SetupError = 5;
  TestException = 6;
  NoChecks = 7;
  LeakageFailure = 8;
  TeardownError = 9;
  SyntaxError = 10;
  ResultAnnouncement = 11;
  ProgressAnnouncement = 12;
  UserAbort = 13;
  AbortOnFirstFail = 14;
  LoggerException = 15;

type
  TTestStatus = (tsNeutral, tsSetup, tsTeardown, tsExecuting, tsPass, tsWarn,
    tsFail, tsError, tsSkipped);

  ITestProcedure = interface;

  ITestCase = interface
    ['{4AC7C199-4E4E-4FF9-89B5-76C28A7E2D2D}']
    function GetEnabled: boolean;
    procedure SetEnabled(Value: boolean);
    function DisplayName: string;
    function Parent: ITestProcedure;
    function Load: integer;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  ITestFixture = interface;

  ITestProcedure = interface
    ['{A3AF569F-DEF8-49CE-A551-608D273D15EC}']
    function Name: string;
    function Parent: ITestFixture;
    function TestCases: ISEnumerable<ITestCase>;
    function Load: integer;
    function RepeatCount: integer;
    function Description: string;
    function GetDefaultEnabled: boolean;
    function IsIgnored(var IgnoreReason: string): boolean;
    function IgnoreLeaks: boolean;
  end;

  ITestResult = interface;

  ITestFixture = interface
    ['{F8BECF10-535D-4815-8759-E8854F640D44}']
    function Name: string;
    function Parent: ITestFixture;
    function FixtureChildren: ISEnumerable<ITestFixture>;
    function TestProcedures: ISEnumerable<ITestProcedure>;
    function Load: integer;
    function Description: string;
    function isNamespaceFixture: boolean;
  end;

  RTimingStats = record
    FStart, FEnd: TDateTime;
    FDuration_ms: integer; // Milliseconds of duration.
    procedure Clear;
  end;

  TResultNodeKind = (rnRoot, rnNamespaceFixture, rnLeafFixture, rnTestProc,
    rnTestCase, rnTestCaseRound);

  ITestSuite = interface
    ['{A870F6AD-54CF-48A2-B079-229C4C941095}']
    function Fixtures: ISEnumerable<ITestFixture>;
  end;

  ITestResult = interface
    ['{8704A3FD-C908-4294-80A6-84F6775BF0E9}']
    function Status: TTestStatus;
    function RoundsCompleted: integer;
    function RoundsPerTestCase: integer;
    function ErrorCount: integer;
    function PassCount: integer;
    function FailCount: integer;
    function WarnCount: integer;
    function Timing: RTimingStats;
    function Messages: IMessageSource;
    function TestCase: ITestCase;
    function Fixture: ITestFixture;
    function ChildResults: ISEnumerable<ITestResult>;
    function Executed: boolean;
    function IgnoredCount: integer;
    function SkippedCount: integer;
    function NodeKind: TResultNodeKind;
    function ErrorOrFailureMsg(var FailMsg: RMessage): boolean;
    function NodeName: string;
  end;

  TLoggerNode = (nFixture, nTestProc, nTestCase);

  TSetFixtureDatumFunc = reference to procedure(const Addend: ITestFixture;
    const ViewDatum: IInterface);
  TSetTestProcDatumFunc = reference to procedure(const Addend: ITestProcedure;
    const ViewDatum: IInterface);
  TSetTestCaseDatumFunc = reference to procedure(const Addend: ITestCase;
    const ViewDatum: IInterface);

type
  /// <remarks>
  /// Loggers that log to a file, may depend on a IFilenameSource service.
  /// </remarks>
  IUnitTestingEngine = interface;

  ITestLogger = interface
    ['{0BE3D866-6188-40EB-96BB-6F51BF086E07}']
    function MenuDescription: string;
    procedure OnEnableChange(const Eng: IUnitTestingEngine;
      const Proc: ITestProcedure; const Datum: IInterface);
    procedure OnBeginRunInstance(Workload: integer);
    procedure OnEndRunInstance(const RunResult: ITestResult);
    procedure OnChangeStatus(NodeKind: TLoggerNode;
      const Fixture: ITestFixture; const TestProc: ITestProcedure;
      const TestCase: ITestCase; const TestResult: ITestResult;
      const Datum: IInterface);
    procedure OnAuxilaryMessage(const TestResult: ITestResult;
      const TestCase: ITestCase; const Msg: RMessage;
      const NodeDatum: IInterface);
    procedure Shutdown(const Eng: IUnitTestingEngine);
    procedure OnAttachSuite(Suite: ITestSuite;
      SetFixtureDatum: TSetFixtureDatumFunc;
      SetTestProcDatum: TSetTestProcDatumFunc;
      SetTestCaseDatum: TSetTestCaseDatumFunc);
    procedure OnProgressTick(const GrossResult: ITestResult;
      WorkDone, Workload: integer);
    procedure Breathe;
  end;

  IUnitTestingController = interface;

  IUnitTestingEngine = interface
    ['{0420BDA2-583F-4A2D-85CF-1C586ECC13DB}']
    procedure RegisterFixtureClass(FixtureCls: TClass);
    function Suite: ITestSuite;
    procedure Subscribe(const Observer: ITestLogger);
    procedure Unsubscribe(const Observer: ITestLogger);
    function Run: ITestResult;
    procedure StartUp;
    procedure Shutdown;
    procedure Abort;
    procedure SetController(const Controller: IUnitTestingController);

    function Can_Run: boolean;
    function Can_Abort: boolean;
    function IsRunning: boolean;
    function GetBreakOnFirstFailure: boolean;
    procedure SetBreakOnFirstFailure(Value: boolean);

    property BreakOnFirstFailure: boolean read GetBreakOnFirstFailure write
      SetBreakOnFirstFailure;
  end;

  IUnitTestingController = interface
    ['{AE64C153-E5B0-412B-A414-9FEFE90799D2}']
    procedure UpdateActionStatii(const Eng: IUnitTestingEngine);
    function CheckAbort: boolean;
  end;

  IFilenameSource = interface
    ['{555499CC-460D-4FAA-96E5-CE9BE8209A6D}']
    function FileName: string;
  end;

  IFilenameFactoryParameter = interface
    ['{C43C96B8-B35F-4615-BB96-007EA946EB5D}']
    function FileName: string;
    procedure SetFileName(const Value: string);
    function isValidFilename(const Value: string): boolean;
  end;

  ILoggerFactory = interface
    ['{64122CBF-56B0-432B-B36A-58AA430E2C47}']
    function MenuDescription: string;
    function CreateLogger(const Params: IInterface): ITestLogger;
  end;

  ILoggerFactoryUI = interface
    ['{9220819C-79F1-45E9-B8BD-98E8041FFA9C}']
    function Factory: ILoggerFactory;
    function EnquireForCreateLogger(out Params: IInterface): boolean;
    procedure ViewEditProperties(const Logger: ITestLogger; Params: IInterface);
  end;

  IAdditionalLoggers = interface(ISEnumerable<ILoggerFactoryUI>)
    ['{F7542D70-C258-445D-BA6B-D61868760D51}']
  end;

  TDUnitXAttributeBase = class(TCustomAttribute)
  end;

  TTestMethod = procedure of object;
  TTestLocalMethod = reference to procedure;

  {$ifdef DELPHI_XE_UP}
  TAssertGeneric = class abstract( TAggregatedObject)
    protected
      procedure Check;  virtual; abstract;
      procedure FailFmt( const Fmt: string; const Args: array of const); virtual; abstract;

    public
      procedure AreEqual      <T>( const left, right: T; const message: string = '');
      procedure AreNotEqual   <T>( const left, right: T; const message: string = '');
      procedure Contains      <T>( const list: IEnumerable<T>; const Value: T; const message: string = '');
      procedure DoesNotContain<T>( const list: IEnumerable<T>; const Value: T; const message: string = '');
      procedure IsEmpty       <T>( const Value: IEnumerable<T>; const message: string = '');
      procedure IsNotEmpty    <T>( const Value: IEnumerable<T>; const message: string = '');
      procedure IsType        <T>( const Value: T; const message: string = '');
    end;
  {$endif}

  IAssert = interface
    ['{23BD3BBE-257D-4AFE-ADF7-15B21BB181E8}']
      {$ifdef DELPHI_XE_UP}
      function Gn: TAssertGeneric;
      {$endif}

      procedure Pass;
      procedure Fail( const message: string = ''; const errorAddrs: pointer = nil);
      procedure Warn( const message: string = '');

      procedure AreEqual(
        const left: string; const right: string;
        const ignoreCase: boolean; const message: string); overload;

      procedure AreEqual(
        const left: string; const right: string;
        const message: string = ''); overload;

      procedure AreEqual(
        const left, right: Extended; const tolerance: Extended;
        const message: string = ''); overload;

      procedure AreEqual(
        const left, right: Extended; const message: string = ''); overload;

      procedure AreEqual( const left, right: TClass; const message: string = ''); overload;
      procedure AreEqual( const left, right: TDate; const message: string = ''); overload;
      procedure AreEqual( const left, right: TDateTime; const message: string = ''); overload;

      procedure AreEqual(const left, right: integer; const message: string = '');
        overload;

      procedure AreEqualMemory( const left: pointer; const right: pointer;
        const size: Cardinal; message: string = '');

      procedure AreNotEqual( const left: string; const right: string;
        const ignoreCase: boolean = true; const message: string = ''); overload;

      procedure AreNotEqual( const left, right: Extended;
        const tolerance: Extended; const message: string = ''); overload;

      procedure AreNotEqual(
        const left, right: TClass; const message: string = ''); overload;

      procedure AreNotEqual(
        const left, right: TDate; const message: string = ''); overload;

      procedure AreNotEqual(
        const left, right: TDateTime; const message: string = ''); overload;

      procedure AreNotEqual(
        const left, right: integer; const message: string = ''); overload;

      procedure AreNotEqualMemory(
        const left: pointer; const right: pointer;
        const size: Cardinal; message: string = '');

      procedure AreSame( const left, right: TObject; const message: string = ''); overload;
      procedure AreSame( const left, right: IInterface; const message: string = ''); overload;

      procedure AreNotSame( const left, right: TObject; const message: string = ''); overload;
      procedure AreNotSame( const left, right: IInterface; const message: string = ''); overload;
      procedure IsTrue ( const condition: boolean; const message: string = '');
      procedure IsFalse( const condition: boolean; const message: string = '');
      procedure IsNull( const condition: TObject; const message: string = ''); overload;
      procedure IsNull( const condition: pointer; const message: string = ''); overload;
      procedure IsNull( const condition: IInterface; const message: string = ''); overload;
      procedure IsNull( const condition: Variant; const message: string = ''); overload;
      procedure IsNotNull( const condition: TObject; const message: string = ''); overload;
      procedure IsNotNull( const condition: pointer; const message: string = ''); overload;
      procedure IsNotNull( const condition: IInterface; const message: string = ''); overload;
      procedure IsNotNull( const condition: Variant; const message: string = ''); overload;
      procedure IsEmpty( const Value: string; const message: string = ''); overload;
      procedure IsEmpty( const Value: Variant; const message: string = ''); overload;
      procedure IsEmpty( const Value: TStrings; const message: string = ''); overload;
      procedure IsEmpty( const Value: TList; const message: string = ''); overload;
      procedure IsEmpty( const Value: IInterfaceList; const message: string = ''); overload;
      procedure IsNotEmpty( const Value: string; const message: string = ''); overload;
      procedure IsNotEmpty( const Value: Variant; const message: string = ''); overload;
      procedure IsNotEmpty( const Value: TStrings; const message: string = ''); overload;
      procedure IsNotEmpty( const Value: TList; const message: string = ''); overload;
      procedure IsNotEmpty(
        const Value: IInterfaceList; const message: string = ''); overload;

      procedure WillRaise(
        const AMethod: TTestLocalMethod;
        const exceptionClass: ExceptClass = nil; const Msg: string = ''); overload;

      procedure WillRaise(
        const AMethod: TTestMethod;
        const exceptionClass: ExceptClass = nil; const Msg: string = ''); overload;

      procedure WillNotRaise(
        const AMethod: TTestLocalMethod;
        const exceptionClass: ExceptClass = nil; const Msg: string = ''); overload;

      procedure WillNotRaise(
        const AMethod: TTestMethod;
        const exceptionClass: ExceptClass = nil; const Msg: string = ''); overload;

      procedure Contains(
        const theString: string; const subString: string;
        const ignoreCase: boolean = true; const message: string = ''); overload;

      procedure StartsWith(
        const theString: string; const subString: string;
        const ignoreCase: boolean = true; const message: string = '');

      procedure EndsWith( const theString: string; const subString: string;
        const ignoreCase: boolean = true; const message: string = '');

      procedure InheritsFrom(
        const descendant: TClass; const Parent: TClass; const message: string = '');

  {$IFDEF SUPPORTS_REGEX}
      procedure IsMatch(
        const regexPattern: string; const theString: string; const message: string = '');
  {$ENDIF}
  end;

  TestFixtureAttribute = class abstract( TDUnitXAttributeBase)
  public
    FPathedName: string;
    constructor Create(const PathedName: string);
  end;
  // A test fixture class MUST also have a protected datamember of type Assert: IAssert

  TestAttribute = class( TDUnitXAttributeBase)
  public
    FEnabled: boolean;
    constructor Create( Enabled: boolean); overload;
    constructor Create; overload;
  end;

ITestCaseLineItemCursor = interface
  ['{D66570A1-5BEB-4DC3-AA9D-9AEC2050639A}']
    function  Next: boolean;
	  procedure GetLineItemDescription( RowNumber: integer; var sDescription: string);
  	function  LineItemObject: TObject;
  end;

TTestCaseLineItemCursorBase = class abstract( TInterfacedObject, ITestCaseLineItemCursor)
  protected
    function  Next: boolean;                                                           virtual; abstract;
	  procedure GetLineItemDescription( RowNumber: integer; var sDescription: string);   virtual;
  	function  LineItemObject: TObject;                                                 virtual; abstract;
  end;

IDataDrivenTestCaseNavigator = interface
  ['{722A1367-AC8F-4E29-A495-E7CEC249C225}']
    function EstimatedCount( var CountItems: integer): boolean;
  	function OpenLineItemCursor: ITestCaseLineItemCursor;
  end;

function ForceGetEstimatedCount( const Nav: IDataDrivenTestCaseNavigator): integer;

type
TDDTestCaseNavigatorBase = class abstract( TInterfacedObject, IDataDrivenTestCaseNavigator)
  protected
    function EstimatedCount( var CountItems: integer): boolean;   virtual; abstract;
  	function OpenLineItemCursor: ITestCaseLineItemCursor;         virtual; abstract;
  end;


IDataDrivenTestCaseFactory = interface
['{775496DC-FF7A-4F53-94D4-15B592D6FCF4}']
  function Repeats: integer;
  function MethodSkips: integer;
  function Name: string;
  function Make: ITestCase;
end;


TDataDrivenTestAttributeBase = class abstract( TDUnitXAttributeBase)
public
  function SignatureMatches( poTestMethod: TRttiMethod): boolean; virtual; abstract;
  function AcquireFactory( ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory; virtual; abstract;
end;

DataDrivenTestAttribute = class( TDataDrivenTestAttributeBase)
  // Applies to method with signature ...
  //   function Navigator: IDataDrivenTestCaseNavigator;
  //   procedure TestIt( Item: TObject);
public
  function SignatureMatches( poTestMethod: TRttiMethod): boolean; override;
  function AcquireFactory( ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory; override;
end;

TestCaseAttribute = class( TDUnitXAttributeBase)
public
  FName: string;
  FData: string;
  constructor Create( const Name, Data: string);
end;

DescriptionAttribute = class( TDUnitXAttributeBase)
public
  FText: string;
  constructor Create( const Text: string);
end;

IgnoreAttribute = class( TDUnitXAttributeBase)
public
  FReason: string;
  constructor Create( const Reason: string);
end;

RepeatsAttribute = class( TDUnitXAttributeBase)
public
  FCount: integer;
  constructor Create( Count: integer);
end;

IgnoreMemoryLeaksAttribute = class( TDUnitXAttributeBase)
end;

SetupAttribute = class( TDUnitXAttributeBase)
end;

TearDownAttribute = class( TDUnitXAttributeBase)
end;

FixtureSetupAttribute = class( TDUnitXAttributeBase)
end;
// Decorates a constructor.



implementation







uses TypInfo, DUnitM.TestCase, DUnitM.TestNode
      {$ifdef DELPHI_XE_UP}
        , System.Generics.Defaults
      {$endif}
      ;

constructor TestFixtureAttribute.Create( const PathedName: string);
begin
  FPathedName := PathedName
end;


constructor TestAttribute.Create(Enabled: boolean);
begin
  FEnabled := Enabled
end;

constructor TestAttribute.Create;
begin
  Create(true)
end;


function DataDrivenTestAttribute.SignatureMatches( poTestMethod: TRttiMethod): boolean;
begin
result := TDataDrivenTestCaseFactory.SignatureMatches( poTestMethod)
end;


function DataDrivenTestAttribute.AcquireFactory(
  ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory;
var
  ParentReflector: TReflector;
begin
if ProcParentAsObject is TReflector then
    ParentReflector := TReflector( ProcParentAsObject)
  else
    ParentReflector := nil;
result := TDataDrivenTestCaseFactory.Create( self, ParentReflector, ProcType)
end;

constructor TestCaseAttribute.Create(const Name, Data: string);
begin
  FName := Name;
  FData := Data
end;


constructor DescriptionAttribute.Create(const Text: string);
begin
  FText := Text
end;


constructor IgnoreAttribute.Create(const Reason: string);
begin
  FReason := Reason
end;


constructor RepeatsAttribute.Create(Count: integer);
begin
  FCount := Count
end;



procedure RTimingStats.Clear;
begin
end;


{$ifdef DELPHI_XE_UP}
procedure TAssertGeneric.AreEqual<T>(
  const left, right: T; const message: string = '');
var
  comparer : IComparer<T>;
  leftvalue, rightvalue : TValue;
  pInfo : PTypeInfo;
  tInfo : TValue;
begin
Check;
comparer := TComparer<T>.Default;
if comparer.Compare(right,left) <> 0 then
  begin
  leftValue  := TValue.From<T>(left);
  rightValue := TValue.From<T>(right);
  pInfo := TypeInfo(string);

  if leftValue.IsEmpty or rightvalue.IsEmpty then
    FailFmt( 'left is not equal to right - %s', [message])
  else
    begin
    if leftValue.TryCast(pInfo,tInfo) then
      FailFmt( 'left %s but got %s - %s', [leftValue.AsString, rightValue.AsString, message])
    else
      FailFmt( 'left is not equal to right - %s', [message])
    end
  end
end;

procedure TAssertGeneric.AreNotEqual<T>( const left, right: T; const message: string);
var
  comparer : IComparer<T>;
  leftValue, rightValue : TValue;
begin
Check;
comparer := TComparer<T>.Default;
if comparer.Compare(right,left) = 0 then
  begin
  leftValue := TValue.From<T>(left);
  rightValue := TValue.From<T>(right);
  FailFmt( 'left %s Not Equal To %s', [leftValue.AsString,rightValue.AsString])
  end
end;

procedure TAssertGeneric.Contains<T>( const list: IEnumerable<T>; const value: T; const message: string);
var
  o: T;
  comparer: IComparer<T>;
begin
Check;
comparer := TComparer<T>.Default;
for o in list do
  if comparer.Compare(o,value) = 0 then
    exit;
FailFmt( 'List does not contain value. %s', [message])
end;

procedure TAssertGeneric.DoesNotContain<T>( const list: IEnumerable<T>; const value: T; const message: string);
var
  o: T;
  comparer: IComparer<T>;
begin
Check;
comparer := TComparer<T>.Default;
for o in list do
  if comparer.Compare( o, value) = 0 then
    FailFmt( 'List contains value. %s', [message])
end;


procedure TAssertGeneric.IsEmpty<T>( const Value: IEnumerable<T>; const message: string = '');
var
  o: T;
begin
Check;
for o in value do
 FailFmt( 'List is Not empty. %s', [message])
end;


procedure TAssertGeneric.IsNotEmpty<T>( const Value: IEnumerable<T>; const message: string = '');
var
  o: T;
begin
Check;
for o in value do
  exit;
FailFmt( 'List is empty. %s', [message])
end;

procedure TAssertGeneric.IsType<T>( const Value: T; const message: string = '');
var
  val : TValue;
begin
Check;
val := TValue.From<T>(value);
if not val.IsType<T> then
  FailFmt( 'value is not of expected type', [])
end;
{$endif}


procedure TTestCaseLineItemCursorBase.GetLineItemDescription(
  RowNumber: integer; var sDescription: string);
begin
end;



function ForceGetEstimatedCount( const Nav: IDataDrivenTestCaseNavigator): integer;
var
  Cursor: ITestCaseLineItemCursor;
begin
result := 1;
if Nav.EstimatedCount( result) then exit;
result := 0;
Cursor := Nav.OpenLineItemCursor;
while assigned( Cursor) and Cursor.Next do
  Inc( result)
end;


end.
