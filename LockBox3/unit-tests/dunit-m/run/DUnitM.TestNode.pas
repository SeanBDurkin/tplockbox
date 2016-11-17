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

unit DUnitM.TestNode;
interface
uses SBD.Messages, DUnitM.UnitTestFramework, SysUtils, DUnitM.MemoryMonitor,
     SBD.Generics, Generics.Collections, Rtti, SBD.ServiceProvider;
type

ITestNode = interface;
IRunInstance = interface;
IRunInstanceServices = interface
  ['{E234CF36-9AD7-46A2-A542-688BC0C939D1}']
    procedure StatusChange( const Source: IRunInstance; const Changee: ITestNode; const Round: integer);
      // Round=-1 for testcase results and above.
    procedure Put( const Source: IRunInstance; const Node: ITestNode; const Msg: RMessage; const Round: integer);
    function  IsAborted: boolean;
  end;
TStatusInformerProc = reference to procedure( Value: TTestStatus);
IRunInstance = interface( ITestResult)
  ['{69870E86-AE6A-42D0-8992-052674A45E4A}']
    procedure Run(
      const RunInstanceServices: IRunInstanceServices;
      OnStatusChangeProc: TStatusInformerProc);
    procedure Clear;
    function  Parent: IRunInstance;
    function  WorkLoad: integer;
    function  TestNode: ITestNode;
    function  ChildInstances: IList2<IRunInstance>;
    function  Description: string;
  end;

IFixtureRunInstance = interface
  ['{35624EEC-A6ED-4287-9B16-16FD4D921394}']
    function  MakeProcedureRunInstance( const TestProc: ITestProcedure): IRunInstance;
    function  MakeFixtureRunInstance( const Fixture: ITestFixture): IRunInstance;
  end;

ICheckGateway = interface
  ['{8CF6F0D4-3768-4E69-97F0-E86AC79EC99D}']
    procedure Check;
    procedure Pass;
    procedure Fail( const sErrMsg: string; Address: pointer);
    procedure Warn( const sWarning: string);
  end;

ETestFrameworkException = class( Exception);
EAbort = class( ETestFrameworkException);
ETestFailure = class( EAbort);
ETestPass = class( EAbort);

TGetObjectFunc = reference to function: TObject;

PReflector = ^TReflector;
TReflector = class
   public
     FController: TObject;
     FWeakReferences: TList<PReflector>;
     constructor Create( Controller: TObject);
     destructor Destroy; override;
     procedure  SetWeakReference( var Ref: TReflector);
     procedure  ClearWeakReference( var Ref: TReflector);
   end;

ITestCaseInternal = interface( ITestCase)
  ['{4F6B72EA-F054-473E-9DD0-5CB8AC295DB5}']
    procedure ShutDown;
    function  RequiresPreliminaryLoadCount: boolean;
    procedure SetDeferredError( const ErMsg: string);
    function  GetDeferredError( var ErMsg: string): boolean;
    procedure Execute( Platter: TObject);
    function  EstimateRoundCount: integer;
    function  MakeRunInstance( ParentAsObject: TReflector; TestBed: TGetObjectFunc; const Mem1: IMemoryMonitor): IRunInstance;
  end;

IRunnerExecutiveServices = interface
  ['{77943668-0977-4B68-92B0-732709CCEEB3}']
    procedure EnterLeaveRunners( Delta: integer);
    function  Suite: ITestSuite;
    procedure OnBeginRunInstance( const Instance: IRunInstance; Workload: integer);
    procedure OnEndRunInstance( const Instance: IRunInstance; const RunResult: ITestResult);
    function  IsAborted: boolean;
    procedure OnChangeStatus(
                        NodeKind: TLoggerNode;
                  const Fixture: ITestFixture;
                  const TestProc: ITestProcedure;
                  const TestCase: ITestCase;
                  const TestResult: ITestResult);
    procedure OnAuxilaryMessage( const Node: ITestNode; const TestResult: ITestResult; const TestCase: ITestCase; const Msg: RMessage; Round: integer);
  end;

ITestNode = interface
  ['{D92D4093-EF1D-4FBE-A54B-473FD2747006}']
    procedure AttachViewDatum( const Logger: ITestLogger; const ViewDatum: IInterface);
    function  ViewDatumOfLogger( const Logger: ITestLogger): IInterface;
    function  NodeKind: TResultNodeKind;
    procedure ShutDown;
    function  Name: string;
    function  Description: string;
    function  ParentNode: ITestNode;
    function  NodeChildren: ISEnumerable<ITestNode>;
    function  Load: integer;
    function  AsObject: TReflector;
    function  HasEnabledTestCases: boolean;
  end;

IFixtureChild = interface
  ['{69387C5A-FE24-4548-B071-45307DF69E91}']
    function  MakeRunInstanceOfFixtureChild( const ParentFixture: IRunInstance; var Addend: IRunInstance): boolean;
  end;

IShutDown = interface
  ['{1A2A2FB1-C87B-4B6F-8708-4BB4F6FE690D}']
    procedure ShutDown;
  end;

IRunInstances = interface( IList2<IRunInstance>)
  ['{837F57D3-E844-4653-91EF-BF99A31600C3}']
    procedure Reset;
  end;


ITestCaseLineCursoredItem = interface
  ['{47DA010A-1BD4-49AC-989E-0CC842D3E639}']
    function  Next: boolean;
	  function  LineItemDescription: string;
    procedure ConductTest;
  end;



IDataDrivenTestCase = interface( ITestCaseInternal)
  ['{7E491901-1F50-4FA9-9E56-51927772D738}']
    function AcquireDataDriver     ( Platter: TObject): IDataDrivenTestCaseNavigator;
  	function OpenLineItemTestCursor( Platter: TObject): ITestCaseLineCursoredItem;
  end;

ITestCasesInternal = interface( IList2<ITestCase>)
  ['{9041FD2C-5248-4DDD-93D5-14BC7B05CEC1}']
    function  AddCase( ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TestCaseAttribute): ITestCaseInternal;
    function  AddDataDrivenCase( ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TDataDrivenTestAttributeBase): ITestCaseInternal;
    function  AddSimpleCase( ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TestAttribute): ITestCaseInternal;
    function  isDataDriven( ProcParentAsObject: TReflector; ProcType: TRttiMethod; var DDTestCaseFactory: IDataDrivenTestCaseFactory): boolean;
    function  AddByFactory( const Fact: IDataDrivenTestCaseFactory): ITestCaseInternal;
    procedure ShutDown;
  end;

TTestNode = class abstract( TInterfacedObject, ITestNode, IShutDown)
  protected
    FParent: TReflector; // weak reference to parent, with ARC.
    FWeakClients: TReflector;
    FViewTokens: TDictionary<ITestLogger,IInterface>;

    function  ViewDatumOfLogger( const Logger: ITestLogger): IInterface;
    procedure AttachViewDatum( const Logger: ITestLogger; const ViewDatum: IInterface);    virtual;
    function  NodeKind: TResultNodeKind;                                                   virtual; abstract;
    procedure ShutDown;                                                                    virtual;
    function  Name: string;                                                                virtual; abstract;
    function  ParentNode: ITestNode;                                                       virtual;
    function  NodeChildren: ISEnumerable<ITestNode>;                                       virtual; abstract;
    function  Load: integer;                                                               virtual; abstract;
    function  AsObject: TReflector;                                                        virtual;
    function  HasEnabledTestCases: boolean;                                                virtual;
    function  Description: string;                                                         virtual;

  public
    constructor CreateTestNode( ParentAsObject: TReflector);
    destructor Destroy; override;
  end;

TRunInstances = class( TList2<IRunInstance>, IRunInstances)
  private
    procedure Reset;
  end;

TTestResults = class( TList2<ITestResult>)
  public
    constructor CreateCastFrom( Source: IRunInstances);
  end;

ITestNodes = interface( IList2<ITestNode>)
  ['{0C592110-ACF3-425C-BF6E-C09B5C2C166D}']
  end;

TTestNodes = class( TList2<ITestNode>, ITestNodes)
  end;

ITestFixtureInternal = interface( ITestFixture)
  ['{6D6DD812-1CE5-41CC-A0CA-AE5BB525D321}']
    function  AsObject: TReflector;
    function  PreliminaryPlatterObject: TObject;
    procedure Setup( Platter: TObject);
    procedure Teardown( Platter: TObject);
    function  MakeRunInstance( ParentRunInstance: TReflector; const Mem1: IMemoryMonitor): IRunInstance;
  end;

IPlatterFactory = interface
  ['{1B77577F-0CC9-43AE-A5AF-37D46C541FC8}']
    function  CreatePlatter( var Hold: IInterface): TObject;
    procedure ReleasePlatter( var Hold: IInterface; var Obj: TObject);
    procedure SetGateway( TestBed1: TObject; const Gateway: ICheckGateway);
  end;

ITestProcedureInternal = interface( ITestProcedure)
  ['{6D6DD812-1CE5-41CC-A0CA-AE5BB525D321}']
    function  AsObject: TReflector;
    procedure ShutDown;
    function  MakeRunInstance( ParentAsObject: TReflector; TestBed: TGetObjectFunc; const Mem1: IMemoryMonitor): IRunInstance;
    function  DeclaredMethodSkips: integer;
  end;

ITestNodeFactory = interface
  ['{6FC8CB4D-652E-4196-8C8D-81230CDC9CA7}']
    function  GetParentAsObject: TReflector;
    procedure SetParentAsObject( Value: TReflector);

    function  MakeNode(  const NodeName: string;
                         Method: TRttiMethod;
                         FixtureType: TRttiType;
                         TestCaseAttri: TDUnitXAttributeBase
                         ): ITestNode;
    property ParentAsObject: TReflector  read GetParentAsObject write SetParentAsObject;
  end;

IProceduresInternal = interface( IList2<ITestProcedure>)
  ['{EAB481FC-5C86-4F93-9304-FC5B6509A113}']
    function  AddProcedure( const Factory: ITestNodeFactory; Method: TRttiMethod; FixtureType: TRttiType): ITestProcedureInternal;
    procedure ShutDown;
  end;



TBaseNodeFactory = class abstract( TInterfacedObject, ITestNodeFactory)
  protected
    FParentAsObject: TReflector;
    [Injection] FServices: IServiceProvider;

    function  GetParentAsObject: TReflector;              virtual;
    procedure SetParentAsObject( Value: TReflector);      virtual;

    function  MakeNode(  const NodeName: string;
                         Method: TRttiMethod;
                         FixtureType: TRttiType;
                         TestCaseAttri: TDUnitXAttributeBase
                         ): ITestNode;                    virtual; abstract;
  end;

procedure SetFrameworkMessage(
  var Msg: RMessage; MessageCode: integer; const Text: string; Lv: TMessageLevel);

implementation















constructor TReflector.Create( Controller: TObject);
begin
FController     := Controller;
FWeakReferences := TList<PReflector>.Create
end;

destructor TReflector.Destroy;
var
  Client: PReflector;
begin
for Client in FWeakReferences do
  Client^ := nil;
FWeakReferences.Free;
inherited
end;

procedure TReflector.ClearWeakReference( var Ref: TReflector);
begin
FWeakReferences.Remove( @Ref);
Ref := nil
end;

procedure TReflector.SetWeakReference( var Ref: TReflector);
begin
if Ref = self then exit;
ClearWeakReference( Ref);
FWeakReferences.Add( @Ref);
Ref := self
end;




constructor TTestNode.CreateTestNode( ParentAsObject: TReflector);
begin
if assigned( ParentAsObject) then
  ParentAsObject.SetWeakReference( FParent);
FWeakClients := TReflector.Create( self);
FViewTokens  := TDictionary<ITestLogger,IInterface>.Create
end;

function TTestNode.Description: string;
begin
result := Name
end;

destructor TTestNode.Destroy;
begin
ShutDown;
if assigned( FParent) then
  FParent.ClearWeakReference( FParent);
FWeakClients.Free;
FViewTokens.Free;
inherited;
end;

function TTestNode.HasEnabledTestCases: boolean;
var
  Child: ITestNode;
begin
result := False;
for Child in NodeChildren do
  result := result or Child.HasEnabledTestCases
end;

function TTestNode.AsObject: TReflector;
begin
result := FWeakClients
end;

procedure TTestNode.AttachViewDatum(
  const Logger: ITestLogger; const ViewDatum: IInterface);
var
  Current: IInterface;
begin
if FViewTokens.ContainsKey( Logger) then
    Current := FViewTokens[ Logger]
  else
    Current := nil;
if ViewDatum = Current then exit;
if assigned( ViewDatum) and assigned( Current) then
    FViewTokens[ Logger] := ViewDatum
  else if assigned( Current) then
    FViewTokens.Remove( Logger)
  else
    FViewTokens.Add( Logger, ViewDatum)
end;




function TTestNode.ParentNode: ITestNode;
begin
if (not assigned( FParent)) or
   (not Supports( FParent.FController, ITestNode, result)) then
  result := nil
end;

procedure TTestNode.ShutDown;
var
  Child: ITestNode;
begin
FViewTokens.Clear;
for Child in NodeChildren do
  Child.ShutDown
end;


function TTestNode.ViewDatumOfLogger( const Logger: ITestLogger): IInterface;
begin
if FViewTokens.ContainsKey( Logger) then
    result := FViewTokens[ Logger]
  else
    result := nil
end;



constructor TTestResults.CreateCastFrom( Source: IRunInstances);
var
  Run: IRunInstance;
begin
inherited Create;
for Run in Source do
  Add( Run as ITestResult)
end;






procedure TRunInstances.Reset;
var
  Child: IRunInstance;
begin
for Child in (Self as IList2<IRunInstance>) do
  Child.Clear
end;

procedure SetFrameworkMessage(
  var Msg: RMessage; MessageCode: integer; const Text: string; Lv: TMessageLevel);
begin
Msg.FMessageURL.FURL := DUnixtM_Framework_URL;
Msg.FMessageURL.FPrefix :=  DUnixtM_Framework_Prefix;
Msg.FMessageCode := MessageCode;
Msg.FDisplayText := Text;
Msg.FLevel := Lv;
Msg.FStamp := Now;
SetLength( Msg.FData, 0)
end;


{ TBaseNodeFactory }

function TBaseNodeFactory.GetParentAsObject: TReflector;
begin
result := FParentAsObject
end;

procedure TBaseNodeFactory.SetParentAsObject( Value: TReflector);
begin
FParentAsObject := Value
end;

end.
