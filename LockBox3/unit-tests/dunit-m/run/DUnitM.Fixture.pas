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

unit DUnitM.Fixture;
interface
uses DUnitM.TestNode, Rtti, SBD.Generics, DUnitM.UnitTestFramework, DUnitM.MemoryMonitor,
     SBD.ServiceProvider;

type
IFixturesInternal = interface( IList2<ITestFixture>)
  ['{BCEC2CA8-A15E-440E-BF32-A7C02029C092}']
    function  FindOrAddNamespaceFixture( const NodeName: string; const NSFactory: ITestNodeFactory): ITestFixtureInternal;
    function  AddLeafFixture( const Factory: ITestNodeFactory; const FixtureName: string; FixtureType: TRttiType): ITestFixtureInternal;
    procedure ShutDown;
  end;

ITestSuiteInternal = interface( ITestSuite)
  ['{F1AA35C1-1904-4BF5-84AD-4D86C61B9FC0}']
    procedure RegisterFixture( FixtureType: TRttiType);
    function  Run( const ExecServices: IRunnerExecutiveServices): ITestResult;
  end;


procedure RegisterServices( const Provider: IServiceProvider);


implementation






















uses SysUtils, DUnitM.RttiUtils, TypInfo, DUnitM.TestProcedure,
     SBD.Messages,SBD.Messages.Solution, DUnitM.StringUtils,
     DUnitM.MemoryMonitor.Solution, DUnitM.Assertion,
     DUnitM.RunInstance;


type
TNamespaceFixtureOrSuite = class abstract( TTestNode, ITestFixture, ITestFixtureInternal)
  protected
    FChildren: IFixturesInternal; // TFixtureList
    function  Parent: ITestFixture;                           virtual;
    function  FixtureChildren: ISEnumerable<ITestFixture>;    virtual;
    function  TestProcedures: ISEnumerable<ITestProcedure>;   virtual;
    function  isNamespaceFixture: boolean;                    virtual;
    function  PreliminaryPlatterObject: TObject;              virtual;
    procedure Setup( Platter: TObject);                       virtual;
    procedure Teardown( Platter: TObject);                    virtual;
    function  NodeChildren: ISEnumerable<ITestNode>;          override;
    function  Load: integer;                                  override;
    function  MakeRunInstance( ParentRunInstance: TReflector; const Mem1: IMemoryMonitor): IRunInstance;  virtual;

  public
    constructor CreateNamespaceFixture( ParentAsObject: TReflector);
  end;

TNamespaceFactory = class sealed( TBaseNodeFactory)
  protected
    function  MakeNode(  const NodeName: string;
                         Method: TRttiMethod;
                         FixtureType: TRttiType;
                         TestCaseAttri: TDUnitXAttributeBase
                         ): ITestNode;                    override;
  public
    [Configuration('rnNamespaceFixture')] constructor ServiceModeCreate;

  private type
    TNamespaceFixture = class( TNamespaceFixtureOrSuite, IFixtureChild)
      private
        FName: string;
        FDescription: string;

      protected
        function  Description: string;                            override;
        function  Name: string;                                   override;
        function  NodeKind: TResultNodeKind;                      override;
        // IFixtureChild
        function  MakeRunInstanceOfFixtureChild( const ParentFixture: IRunInstance; var Addend: IRunInstance): boolean; virtual;
    end;
  end;


TFixtureList = class( TList2<ITestFixture>, IFixturesInternal, IShutDown)
  public
    function  FindOrAddNamespaceFixture( const NodeName: string; const NSFactory: ITestNodeFactory): ITestFixtureInternal;
    function  AddLeafFixture( const Factory: ITestNodeFactory; const FixtureName: string; FixtureType: TRttiType): ITestFixtureInternal;
    procedure ShutDown;
  end;


TSuiteFactory = class sealed( TBaseNodeFactory)
  protected
    function  MakeNode(  const NodeName: string;
                         Method: TRttiMethod;
                         FixtureType: TRttiType;
                         TestCaseAttri: TDUnitXAttributeBase
                         ): ITestNode;                    override;
  public
    [Configuration('rnRoot')] constructor ServiceModeCreate;

  private
    [Injection( 'rnLeafFixture')] FLeafFactory: ITestNodeFactory;
    [Injection( 'rnNamespaceFixture')] FNSFactory: ITestNodeFactory;

  private type
    TTestSuite = class sealed( TNamespaceFixtureOrSuite, ITestSuite, ITestSuiteInternal)
      private
        FLeafFactory: ITestNodeFactory;
        FNSFactory: ITestNodeFactory;

        function  Fixtures: ISEnumerable<ITestFixture>;
        procedure RegisterFixture( FixtureType: TRttiType);
        function  Run( const ExecServices: IRunnerExecutiveServices): ITestResult;
        function  CreateRunInstance( const ExecServices: IRunnerExecutiveServices): IRunInstance;
        function  ProvideRunInstanceServices( const ExecServices: IRunnerExecutiveServices): IRunInstanceServices;

      protected
        function  NodeKind: TResultNodeKind;                      override;
        function  Name: string;                                   override;
        function  Parent: ITestFixture;                           override;
        function  Description: string;                            override;
      end;
  end;


TCaseBearingFixtureFactory = class sealed( TBaseNodeFactory)
  protected
    function  MakeNode(  const NodeName: string;
                         Method: TRttiMethod;
                         FixtureType: TRttiType;
                         TestCaseAttri: TDUnitXAttributeBase
                         ): ITestNode;                    override;
  public
    [Configuration('rnLeafFixture')] constructor ServiceModeCreate;

  private
    [Injection( 'rnTestProc')] FProcFactory: ITestNodeFactory;

  private type
    TCaseBearingFixture = class sealed( TNamespaceFactory.TNamespaceFixture, IPlatterFactory)
      private
        FFixtureClass: TRttiType;
        FProcedures: IProceduresInternal;
        FSetupMeth: TRttiMethod;
        FTearDownMeth: TRttiMethod;
        FFixtureSetupMeth: TRttiMethod;
        FAssertDataMembers: IList2<TRttiField>;
        FPreliminaryPlatterObject: TObject;

      protected
        procedure ShutDown;                                      override;
        function  TestProcedures: ISEnumerable<ITestProcedure>;  override;
        function  Load: integer;                                 override;
        function  isNamespaceFixture: boolean;                   override;
        function  PreliminaryPlatterObject: TObject;             override;
        procedure Setup( Platter: TObject);                      override;
        procedure Teardown( Platter: TObject);                   override;
        function  NodeKind: TResultNodeKind;                     override;
        function  MakeRunInstance( ParentRunInstance: TReflector; const Mem1: IMemoryMonitor): IRunInstance;  override;

      private
        function  CreatePlatter( var Hold: IInterface): TObject;
        procedure ReleasePlatter( var Hold: IInterface; var Obj: TObject);
        procedure SetGateway( TestBed1: TObject; const Gateway: ICheckGateway);
        procedure ComputeAndCacheLoads;
      end;
  end;



TFixture_RunInstance2 = class( TTestNode_RunInstanceWithChildren, IFixtureRunInstance)
  protected
    FFixture: ITestFixtureInternal;

    // IFixtureRunInstance
    function  MakeProcedureRunInstance( const TestProc: ITestProcedure): IRunInstance;  virtual;
    function  MakeFixtureRunInstance( const Fixture: ITestFixture): IRunInstance;
    function  TestNode: ITestNode;                  override;
    function  RoundsPerTestCase: integer;           override;
    function  Fixture: ITestFixture;                override;
    function  NodeKind: TResultNodeKind;            override;
    procedure DoInnerRun;                           override;

  public
    constructor Create( ParentAsObject: TReflector; const Fixture1: ITestFixtureInternal; const Mem1: IMemoryMonitor);
  end;

TCaseBearingFixture_RunInstance2 = class( TFixture_RunInstance2)
  protected
    FTestBed: TObject;
    function  MakeProcedureRunInstance( const TestProc: ITestProcedure): IRunInstance;  override;
    procedure DoInnerRun;                                                               override;
    function  NodeKind: TResultNodeKind;                                                override;
  end;

TSuite_RunInstance2 = class( TFixture_RunInstance2)
  protected
    function NodeKind: TResultNodeKind;                                               override;
  public
    constructor Create( const Suite1: ITestSuiteInternal; const Mem1: IMemoryMonitor);
  end;


procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterServiceClass( ITestNodeFactory, TSuiteFactory);
Provider.RegisterServiceClass( ITestNodeFactory, TCaseBearingFixtureFactory);
Provider.RegisterServiceClass( ITestNodeFactory, TNamespaceFactory);

end;


constructor TNamespaceFixtureOrSuite.CreateNamespaceFixture( ParentAsObject: TReflector);
begin
inherited CreateTestNode( ParentAsObject);
FChildren := TFixtureList.Create
end;

function TNamespaceFactory.TNamespaceFixture.Description: string;
begin
result := FDescription
end;

function TNamespaceFixtureOrSuite.FixtureChildren: ISEnumerable<ITestFixture>;
begin
result := FChildren
end;

function TNamespaceFixtureOrSuite.isNamespaceFixture: boolean;
begin
result := True
end;

function TNamespaceFixtureOrSuite.Load: integer;
var
  Child: ITestFixture;
begin
result := 0;
for Child in FixtureChildren do
  Inc( Result, Child.Load)
end;

function TNamespaceFixtureOrSuite.MakeRunInstance( ParentRunInstance: TReflector; const Mem1: IMemoryMonitor): IRunInstance;
begin
result := TFixture_RunInstance2.Create( ParentRunInstance, Self, Mem1)
end;

function TNamespaceFactory.TNamespaceFixture.MakeRunInstanceOfFixtureChild(
  const ParentFixture: IRunInstance; var Addend: IRunInstance): boolean;
var
  FixRun: IFixtureRunInstance;
begin
result := Supports( ParentFixture, IFixtureRunInstance, FixRun);
if result then
  Addend := FixRun.MakeFixtureRunInstance( self)
end;

function TNamespaceFactory.TNamespaceFixture.Name: string;
begin
result := FName
end;

function TNamespaceFixtureOrSuite.NodeChildren: ISEnumerable<ITestNode>;
var
  Accumulator: ITestNodes;
  TestFixture: ITestFixture;
  TestProc: ITestProcedure;
begin
Accumulator := TTestNodes.Create;
result := Accumulator;
for TestFixture in FixtureChildren do
  Accumulator.Add( TestFixture as ITestNode);
for TestProc in TestProcedures do
  Accumulator.Add( TestProc as ITestNode)
end;

function TNamespaceFactory.TNamespaceFixture.NodeKind: TResultNodeKind;
begin
result := rnNamespaceFixture
end;

function TNamespaceFixtureOrSuite.Parent: ITestFixture;
begin
if (not assigned( FParent)) or
   (not Supports( FParent.FController, ItestFixture, result)) then
  result := nil
end;

function TNamespaceFixtureOrSuite.PreliminaryPlatterObject: TObject;
begin
result := nil
end;

procedure TNamespaceFixtureOrSuite.Setup( Platter: TObject);
begin
end;

type
TEmptyProcedures = class( TSEnumerable<ITestProcedure>)
  protected
    function GetEnumerator: ISEnumerator<ITestProcedure>; override;
  end;

function TEmptyProcedures.GetEnumerator: ISEnumerator<ITestProcedure>;
begin
result := TEmptyCursor<ITestProcedure>.Create( nil)
end;


procedure TNamespaceFixtureOrSuite.Teardown( Platter: TObject);
begin
end;

function TNamespaceFixtureOrSuite.TestProcedures: ISEnumerable<ITestProcedure>;
begin
result := TEmptyProcedures.Create
end;


function TCaseBearingFixtureFactory.TCaseBearingFixture.CreatePlatter( var Hold: IInterface): TObject;
var
  Cls: TClass;
  InstanceObj: TObject;
begin
Cls := TypInfo.GetTypeData( FFixtureClass.Handle)^.ClassType;
if assigned( FFixtureSetupMeth) then
    begin
    InstanceObj := Cls.NewInstance;
    result := InstanceObj;
    FFixtureSetupMeth.Invoke( result, []);
    result.AfterConstruction
    end
  else
    result := Cls.Create;
if (not Supports( result, IInterface, Hold)) or
   ((Hold._AddRef + Hold._Release) = -2) then
    Hold := nil
end;


procedure TCaseBearingFixtureFactory.TCaseBearingFixture.SetGateway(
  TestBed1: TObject; const Gateway: ICheckGateway);
var
  AssertDataMember: TRttiField;
  AssertChecker: IAssert;
  Injection: TValue;
begin
if not assigned( TestBed1) then exit;
AssertChecker := nil;
if assigned( Gateway) and (FAssertDataMembers.Count > 0) then
    AssertChecker := TAssertChecker.Create( Gateway)
  else
    AssertChecker := nil;
Injection := TValue.From<IAssert>( AssertChecker);
for AssertDataMember in FAssertDataMembers do
  AssertDataMember.SetValue( TestBed1, Injection)
end;



procedure TCaseBearingFixtureFactory.TCaseBearingFixture.ReleasePlatter( var Hold: IInterface; var Obj: TObject);
begin
if assigned( Hold) then
    Hold := nil
  else
    begin
    Hold := nil;
    Obj.Free
    end;
Obj := nil
end;

procedure TCaseBearingFixtureFactory.TCaseBearingFixture.ComputeAndCacheLoads;
var
  Proc: ITestProcedure;
  TCase: ITestCase;
  CaseIntnl: ITestCaseInternal;
  RequiresPreliminaryLoadCount: boolean;
  Obj: TObject;
  Hold: IInterface;
begin
RequiresPreliminaryLoadCount := False;
for Proc in FProcedures do
  for TCase in Proc.TestCases do
    if Supports( TCase, ITestCaseInternal, CaseIntnl) and
       CaseIntnl.RequiresPreliminaryLoadCount then
         begin
         RequiresPreliminaryLoadCount := True;
         break
         end;
if RequiresPreliminaryLoadCount then
  try
    Obj := CreatePlatter( Hold);
    try
      FPreliminaryPlatterObject := Obj;
      Load
    finally
      FPreliminaryPlatterObject := nil;
      ReleasePlatter( Hold, Obj)
      end
    except on E: Exception do
      for Proc in FProcedures do
        for TCase in Proc.TestCases do
          if Supports( TCase, ITestCaseInternal, CaseIntnl) then
             CaseIntnl.SetDeferredError( E.Message)
    end
end;

function TCaseBearingFixtureFactory.TCaseBearingFixture.isNamespaceFixture: boolean;
begin
result := False
end;

function TCaseBearingFixtureFactory.TCaseBearingFixture.Load: integer;
var
  Proc: ITestProcedure;
begin
result := inherited Load;
for Proc in TestProcedures do
  Inc( result, Proc.Load)
end;

function TCaseBearingFixtureFactory.TCaseBearingFixture.MakeRunInstance(
  ParentRunInstance: TReflector; const Mem1: IMemoryMonitor): IRunInstance;
begin
result := TCaseBearingFixture_RunInstance2.Create( ParentRunInstance, self, Mem1)
end;

function TCaseBearingFixtureFactory.TCaseBearingFixture.NodeKind: TResultNodeKind;
begin
result := rnLeafFixture
end;

function TCaseBearingFixtureFactory.TCaseBearingFixture.PreliminaryPlatterObject: TObject;
begin
result := FPreliminaryPlatterObject
end;

procedure TCaseBearingFixtureFactory.TCaseBearingFixture.Setup( Platter: TObject);
begin
if assigned( FSetupMeth) then
  FSetupMeth.Invoke( Platter, [])
end;

procedure TCaseBearingFixtureFactory.TCaseBearingFixture.ShutDown;
begin
inherited;
FProcedures.ShutDown
end;

procedure TCaseBearingFixtureFactory.TCaseBearingFixture.Teardown( Platter: TObject);
begin
if assigned( FTeardownMeth) then
  FTeardownMeth.Invoke( Platter, [])
end;

function TCaseBearingFixtureFactory.TCaseBearingFixture.TestProcedures: ISEnumerable<ITestProcedure>;
begin
result := FProcedures
end;

function TFixtureList.AddLeafFixture(
  const Factory: ITestNodeFactory; const FixtureName: string; FixtureType: TRttiType): ITestFixtureInternal;
begin
result := Factory.MakeNode( FixtureName, nil, FixtureType, nil) as ITestFixtureInternal;
Add( result as ITestFixture)
end;

function TFixtureList.FindOrAddNamespaceFixture(
  const NodeName: string; const NSFactory: ITestNodeFactory): ITestFixtureInternal;
var
  Run: ITestFixture;
begin
result := nil;
for Run in (self as IFixturesInternal) do
  if (Run.Name = NodeName) and Run.isNamespaceFixture then
    begin
    result := Run as ITestFixtureInternal;
    break
    end;
if assigned( result) then exit;
result := NSFactory.MakeNode( NodeName, nil, nil, nil) as ITestFixtureInternal;
// result := TNamespaceFactory.TNamespaceFixture.CreateNamespaceFixture( ParentName, FixtureParentAsObject);
Add( result as ITestFixture)
end;


procedure TFixtureList.ShutDown;
var
  Child: ITestFixture;
  Shut: IShutDown;
begin
for Child in (self as IFixturesInternal) do
  if Supports( Child, IShutDown, Shut) then
    Shut.ShutDown
end;











function TSuiteFactory.TTestSuite.Description: string;
begin
result := ''
end;

function TSuiteFactory.TTestSuite.Fixtures: ISEnumerable<ITestFixture>;
begin
result := FixtureChildren
end;

function TSuiteFactory.TTestSuite.Name: string;
begin
result := ''
end;

function TSuiteFactory.TTestSuite.NodeKind: TResultNodeKind;
begin
result := rnRoot
end;

function TSuiteFactory.TTestSuite.Parent: ITestFixture;
begin
result := nil
end;


type
TRunInstanceServices = class( TInterfacedObject, IRunInstanceServices)
  private
    FExecServices: IRunnerExecutiveServices;
    procedure StatusChange( const Source: IRunInstance; const Changee: ITestNode; const Round: integer);
    procedure Put( const Source: IRunInstance; const Node: ITestNode; const Msg: RMessage; const Round: integer);
    function  IsAborted: boolean;
  public
    constructor Create( const ExecServices1: IRunnerExecutiveServices);
  end;

function TSuiteFactory.TTestSuite.ProvideRunInstanceServices(
  const ExecServices: IRunnerExecutiveServices): IRunInstanceServices;
begin
result := TRunInstanceServices.Create( ExecServices)
end;


procedure TSuiteFactory.TTestSuite.RegisterFixture( FixtureType: TRttiType);
var
  FA: TestFixtureAttribute;
  Path, sParticle, sParent: string;
  FixtureList: IFixturesInternal;
  Fixture: ITestFixtureInternal;
  FixtureParentAsObject: TReflector;
begin
if TAttributes.GetFirst<TestFixtureAttribute>( FixtureType, FA) then
    Path := FA.FPathedName
  else
    Path := FixtureType.UnitName + '.' + FixtureType.Name;
sParent := '';
FixtureParentAsObject := nil;
FixtureList := FChildren;
for sParticle in Split( Path, '.') do
  begin
  if sParent <> '' then
    begin
    FNSFactory.ParentAsObject := FixtureParentAsObject;
    Fixture := FixtureList.FindOrAddNamespaceFixture( sParent, FNSFactory);
    FixtureList := Fixture.FixtureChildren as IFixturesInternal;
    FixtureParentAsObject := Fixture.AsObject
    end;
  sParent := sParticle
  end;
if sParticle <> '' then
  begin
  FLeafFactory.ParentAsObject := FixtureParentAsObject;
  FixtureList.AddLeafFixture( FLeafFactory, sParticle, FixtureType)
  end
end;


function TSuiteFactory.TTestSuite.Run(
  const ExecServices: IRunnerExecutiveServices): ITestResult;
var
  Instance: IRunInstance;
begin
ExecServices.EnterLeaveRunners( +1);
Instance := CreateRunInstance( ExecServices);
ExecServices.OnBeginRunInstance( Instance, Load);
try
  Instance.Run( ProvideRunInstanceServices( ExecServices), nil)
finally
  result := Instance as ITestResult;
  ExecServices.OnEndRunInstance( Instance, result);
  ExecServices.EnterLeaveRunners( -1)
  end
end;


function TSuiteFactory.TTestSuite.CreateRunInstance(
  const ExecServices: IRunnerExecutiveServices): IRunInstance;
var
  Provider: IServiceProvider;
  Mem: IMemoryMonitor;
begin
Provider := StandardServiceProvider;
DUnitM.MemoryMonitor.Solution.RegisterServices( Provider);
Provider.Gn.Acquire<IMemoryMonitor>( self, Mem);
result := TSuite_RunInstance2.Create( self, Mem)
end;



constructor TRunInstanceServices.Create(
  const ExecServices1: IRunnerExecutiveServices);
begin
FExecServices := ExecServices1
end;

function TRunInstanceServices.IsAborted: boolean;
begin
result := assigned( FExecServices) and FExecServices.IsAborted
end;

procedure TRunInstanceServices.Put(
  const Source: IRunInstance; const Node: ITestNode;
  const Msg: RMessage; const Round: integer);
begin
if assigned( FExecServices) then
  FExecServices.OnAuxilaryMessage( Node, Source, Source.TestCase, Msg, Round)
end;


procedure TRunInstanceServices.StatusChange(
  const Source: IRunInstance; const Changee: ITestNode; const Round: integer);
var
  Fixture : ITestFixture;
  TestProc: ITestProcedure;
  TestCase: ITestCase;
  LoggerNodeKind: TLoggerNode;
  EngineNodeKind: TResultNodeKind;
  s: string;
begin
try
s := '1';
EngineNodeKind := Changee.NodeKind;
s := '2';
if not (EngineNodeKind in [rnNamespaceFixture .. rnTestCaseRound]) then exit;
s := '3';
case EngineNodeKind of
  rnNamespaceFixture,
  rnLeafFixture     : begin
s := '4';
                      LoggerNodeKind := nFixture;
                      Fixture  := Changee as ITestFixture;
                      TestProc := nil;
                      TestCase := nil
;s := '5';
                      end;

  rnTestProc        : begin
;s := '6';
                      LoggerNodeKind := nTestProc;
                      TestProc := Changee as ITestProcedure;
                      Fixture  := TestProc.Parent;
                      TestCase := nil
;s := '7';
                      end;

  rnTestCase,
  rnTestCaseRound   : begin
;s := '8';
                      LoggerNodeKind := nTestCase;
                      TestCase := Changee as ITestCase;
                      TestProc := TestCase.Parent;
                      Fixture  := TestProc.Parent
;s := '9';
                      end
  else                LoggerNodeKind := nFixture
  end;
s := '10';
FExecServices.OnChangeStatus( LoggerNodeKind, Fixture, TestProc, TestCase, Source)
;s := '111';
except
  s := s + 'F'
end;
end;


constructor TFixture_RunInstance2.Create(
  ParentAsObject: TReflector;
  const Fixture1: ITestFixtureInternal; const Mem1: IMemoryMonitor);
var
  Child: ITestNode;
  Node: ITestNode;
  Addend: IRunInstance;
  FixChild: IFixtureChild;
begin
FFixture := Fixture1;
inherited Create( ParentAsObject, Mem1);
if Supports( FFixture, ITestNode, Node) then
  for Child in Node.NodeChildren do
    if Supports( Child, IFixtureChild, FixChild) and
       FixChild.MakeRunInstanceOfFixtureChild( self, Addend) and
       assigned( Addend) then
         FChildren.Add( Addend)
end;

procedure TFixture_RunInstance2.DoInnerRun;
var
  Child: IRunInstance;
begin
for Child in FChildren do
  begin
  Child.Run( FRunInstanceServices,
             procedure( Value: TTestStatus)
             begin
             if Value in [tsSetup, tsTeardown, tsExecuting] then
               Status := Value
             end);
  if FStatus = tsError then break
  end;
if ErrorCount > 0 then
  Status := tsError
else if FailCount > 0 then
  Status := tsFail
else if WarnCount > 0 then
  Status := tsWarn
else if PassCount > 0 then
  Status := tsPass
else
  Status := tsSkipped
end;

function TFixture_RunInstance2.Fixture: ITestFixture;
begin
result := FFixture as ITestFixture
end;

function TFixture_RunInstance2.MakeFixtureRunInstance(
  const Fixture: ITestFixture): IRunInstance;
begin
result := (Fixture as ITestFixtureInternal).MakeRunInstance( FWeakClients, FMem)
end;

function TFixture_RunInstance2.MakeProcedureRunInstance(
  const TestProc: ITestProcedure): IRunInstance;
begin
result := nil
end;

function TFixture_RunInstance2.NodeKind: TResultNodeKind;
begin
result := rnNamespaceFixture
end;

function TFixture_RunInstance2.RoundsPerTestCase: integer;
begin
result := 1
end;

function TFixture_RunInstance2.TestNode: ITestNode;
begin
result := FFixture as ITestNode
end;


procedure TCaseBearingFixture_RunInstance2.DoInnerRun;
var
  PlatterFactory: IPlatterFactory;
  Hold: IInterface;
  FixNode: ITestNode;
begin
if  Supports( FFixture, ITestNode, FixNode) and
    FixNode.HasEnabledTestCases and
    Supports( FFixture, IPlatterFactory, PlatterFactory) and
    assigned( PlatterFactory) then
      FTestBed := PlatterFactory.CreatePlatter( Hold)
    else
      FTestBed := nil;
try
  inherited DoInnerRun
finally
  if assigned( PlatterFactory) then
    PlatterFactory.ReleasePlatter( Hold, FTestBed)
  end;
end;

function TCaseBearingFixture_RunInstance2.MakeProcedureRunInstance(
  const TestProc: ITestProcedure): IRunInstance;
begin
result := (TestProc as ITestProcedureInternal).MakeRunInstance(
  FWeakClients,
  function: TObject
    begin
    result := FTestBed
    end,
    FMem)
end;

function TCaseBearingFixture_RunInstance2.NodeKind: TResultNodeKind;
begin
result := rnLeafFixture
end;


constructor TSuite_RunInstance2.Create(
  const Suite1: ITestSuiteInternal; const Mem1: IMemoryMonitor);
begin
inherited Create( nil, Suite1 as ITestFixtureInternal, Mem1)
end;

function TSuite_RunInstance2.NodeKind: TResultNodeKind;
begin
result := rnRoot
end;


function TSuiteFactory.MakeNode(
  const NodeName: string; Method: TRttiMethod;
  FixtureType: TRttiType; TestCaseAttri: TDUnitXAttributeBase): ITestNode;
var
  AddendObj: TTestSuite;
begin
AddendObj := TTestSuite.CreateNamespaceFixture( nil);
AddendObj.FLeafFactory := FLeafFactory;
AddendObj.FNSFactory   := FNSFactory;
result := AddendObj
end;

constructor TSuiteFactory.ServiceModeCreate;
begin
end;

{ TCaseBearingFixtureFactory }

function TCaseBearingFixtureFactory.MakeNode(
  const NodeName: string;
  Method: TRttiMethod; FixtureType: TRttiType;
  TestCaseAttri: TDUnitXAttributeBase): ITestNode;
var
  Addend: TCaseBearingFixture;
  Desc: DescriptionAttribute;
  Meth: TRttiMethod;
  DataMember: TRttiField;
  Skips: integer;
begin
Addend := TCaseBearingFixture.CreateNamespaceFixture( FParentAsObject);
result := Addend as ITestNode;
// result := TCaseBearingFixture.CreateFixture( FixtureName=NodeName, Parent1=FParentAsObject, FixtureType)
Addend.FFixtureClass := FixtureType;
Addend.FName := NodeName;
Addend.FDescription := '';
Addend.FProcedures := TProcedureList.Create;
FProcFactory.ParentAsObject := Addend.FWeakClients;

// Fixtures can have [Description]s. If more than one, they are concatinated.
for Desc in TAttributes.Get<DescriptionAttribute>( FixtureType) do
  begin
  if Addend.FDescription <> '' then
    Addend.FDescription := Addend.FDescription + #13#10;
  Addend.FDescription := Addend.FDescription + Desc.FText
  end;
Addend.FSetupMeth := nil;
Addend.FTeardownMeth := nil;
Addend.FFixtureSetupMeth := nil;
for Meth in FixtureType.GetDeclaredMethods do
  begin
  // [Setup] procedure MySetup;
  if (not assigned( Addend.FSetupMeth)) and TAttributes.Has<SetupAttribute>( Meth) then
    Addend.FSetupMeth := Meth;

  // [Teardown] procedure MyTearDown;
  if (not assigned( Addend.FTeardownMeth)) and TAttributes.Has<TeardownAttribute>( Meth) then
    Addend.FTeardownMeth := Meth;

  // [FixtureSetup] constructor MyCreate;
  if (not assigned( Addend.FFixtureSetupMeth)) and TAttributes.Has<FixtureSetupAttribute>( Meth) then
    Addend.FFixtureSetupMeth := Meth
  end;

// Pick defaults, when we couldn't find an attribute.
for Meth in FixtureType.GetDeclaredMethods do
  begin
  // published procedure Setup;
  if (not assigned( Addend.FSetupMeth)) and (Meth.Visibility = TypInfo.mvPublished) and
     SameText( Meth.Name, 'Setup') and (Meth.MethodKind = mkProcedure) and
     (Length( Meth.GetParameters) = 0) then
     Addend.FSetupMeth := Meth;

  // published procedure Teardown;
  if (not assigned( Addend.FTeardownMeth)) and (Meth.Visibility = TypInfo.mvPublished) and
     SameText( Meth.Name, 'Teardown') and (Meth.MethodKind = mkProcedure) and
     (Length( Meth.GetParameters) = 0) then
     Addend.FTeardownMeth := Meth;

  // published constructor AnyName;
  if (not assigned( Addend.FFixtureSetupMeth)) and (Meth.Visibility = TypInfo.mvPublished) and
     (Meth.MethodKind = mkConstructor) and
     (Length( Meth.GetParameters) = 0) then
     Addend.FFixtureSetupMeth := Meth
  end;

Addend.FAssertDataMembers := TList2<TRttiField>.Create;
for DataMember in FixtureType.GetFields do
  begin
  // FMyAssert: IAssert;
  if (DataMember.FieldType.TypeKind = tkInterface) and
     (DataMember.FieldType is TRttiInterfaceType) and
     IsEqualGUID( TRttiInterfaceType( DataMember.FieldType).GUID, IAssert) then
    Addend.FAssertDataMembers.Add( DataMember)
  end;
Skips := 0;
for Meth in FixtureType.GetDeclaredMethods do
  begin
  if Skips > 0 then
      Dec( Skips)
  else if TAttributes.Has<TestAttribute>( Meth) or
          TAttributes.Has<TestCaseAttribute>( Meth) or
          TAttributes.Has<DataDrivenTestAttribute>( Meth) then
      Skips := (Addend.FProcedures.AddProcedure( FProcFactory, Meth, FixtureType)
        as ITestProcedureInternal).DeclaredMethodSkips
  end;
if Addend.FProcedures.GetCount = 0 then
  raise Exception.Create( 'Test fixture has no valid test procedures.');
Addend.ComputeAndCacheLoads
end;

constructor TCaseBearingFixtureFactory.ServiceModeCreate;
begin
end;

{ TNamespaceFactory }

function TNamespaceFactory.MakeNode( const NodeName: string; Method: TRttiMethod;
  FixtureType: TRttiType; TestCaseAttri: TDUnitXAttributeBase): ITestNode;
var
  AddendObj: TNamespaceFixture;
begin
AddendObj := TNamespaceFixture.CreateNamespaceFixture( FParentAsObject);
AddendObj.FName := NodeName;
AddendObj.FDescription := '';
result := AddendObj
end;


constructor TNamespaceFactory.ServiceModeCreate;
begin
end;

end.
