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

unit DUnitM.ViewModel_VCLForms;
interface
uses DUnitM.UnitTestFramework, Forms, DUnitM.BaseExecutive, DUnitM.ViewModel_Tree,
     Classes, Controls, Generics.Collections, SBD.Messages;

//    TODO:
//     2.  Add menu items for secondary loggers
//     3.  Windows Console solution
//     4.  OS X FM Solution
//     5.  OS X Console Solution
//     6.  iOS FM Solution
//     7.  Android FM Solution
//     8.  Refactor to injection
//     9.  Attribute extensibility

type
TPutLevel = (lvDebug, lvNormal, lvHighLight);

IViewModel_VCLForms = interface( IUnitTestingController)
  ['{B9BF01AA-1877-4073-9E43-C4A72F8FA8FC}']
    procedure FormLoaded( Form: TCustomForm);
    procedure FirstIdleEvent;
    procedure FormDestroyed;
    function  Executive: IExecutive;
    function  Model: IUnitTestingEngine;
    procedure Run;
    procedure AttachSecondaryLogger( const Addend: ITestLogger);
    procedure DetachSecondaryLogger( const Observer: ITestLogger);
    procedure ClearLog;
    procedure Put( Level: TPutLevel; const Line: string; const Args: array of const);
    procedure ToggleSelections;
    procedure ClearSelections;
    function  CanClearSelections: boolean;
    procedure SelectAll;
    procedure SelectFailed;
    procedure SelectDefault;
    function  CanSelectFailed: boolean;
    function  GetShowProgressBar: boolean;
    procedure SetShowProgressBar( Value: boolean);
    function  GetProgressPosition: integer;
    procedure SetProgressPosition( Value: integer);
    function  GetProgressMax: integer;
    procedure SetProgressMax( Value: integer);
    procedure RequestAbort;
    function  IsAbortRequested: boolean;
    function  CanToggleHaltOnFirstFailure: boolean;
    function  GetDoHaltOnFirstFailure: boolean;
    procedure SetDoHaltOnFirstFailure( Value: boolean);
    function  CanDialog: boolean;

    procedure ClearAll;

    property ShowProgressBar: boolean     read GetShowProgressBar  write SetShowProgressBar;
    property ProgressPosition: integer    read GetProgressPosition write SetProgressPosition;
    property ProgressMax: integer         read GetProgressMax      write SetProgressMax;
    property  DoHaltOnFirstFailure: boolean read GetDoHaltOnFirstFailure write SetDoHaltOnFirstFailure;
  end;

TSuiteRunnerState = (rsIdle, rsSettingUp, rsExecuting, rsTearingDown, rsBetweenTests);

TViewModel_VCLForms = class( TInterfacedObject, ITestLogger, IUnitTestingController, IViewModel_VCLForms)
  private
    FTree: IVisualTestSuiteTree;
    FTestCaseCount: integer;

    function  Executive: IExecutive;
    function  Model: IUnitTestingEngine;
    procedure ClearTree;

  protected type
    TNode = class abstract( TInterfacedObject, INodeRenderer)
      protected
        FToken: IVisualTestSuiteNode;
        FModel: TViewModel_VCLForms;
        FState: TTestStatus;
        FTestResult: ITestResult;

        procedure Attached( const Node: IVisualTestSuiteNode);
        procedure Detach;                                        virtual;
        function  GetState: TTestStatus;                         virtual;
        function  GetKind: TVisualTestSuiteNodeKind;             virtual; abstract;
        function  GetDisplayName: string;                        virtual; abstract;
        function  GetFullCycleCount: integer;                    virtual; abstract;
        function  GetDoneCycleCount: integer;                    virtual;
        procedure SetChecked( Value: boolean; Source: TSetCheckedSource);  virtual; abstract;
        function  IsChecked: boolean;                            virtual;
        function  Hint: string;                                  virtual;  abstract;
      public
        constructor Create( Model1: TViewModel_VCLForms);
      end;


  protected
    FForm: TCustomForm;
    FModel: IUnitTestingEngine;
    FState: TSuiteRunnerState;
    FisInOperation: boolean;
    FAbortRequested: boolean;
    FExecutive: IExecutive;

    function  MenuDescription: string;
    procedure OnEnableChange( const Eng: IUnitTestingEngine; const Proc: ITestProcedure; const Datum: IInterface);
    procedure OnBeginRunInstance( Workload: integer);
    procedure OnEndRunInstance( const RunResult: ITestResult);
    procedure OnChangeStatus(
                        NodeKind: TLoggerNode;
                  const Fixture: ITestFixture;
                  const TestProc: ITestProcedure;
                  const TestCase: ITestCase;
                  const TestResult: ITestResult;
                  const NodeDatum: IInterface);
    procedure OnAuxilaryMessage( const TestResult: ITestResult; const TestCase: ITestCase; const Msg: RMessage; const NodeDatum: IInterface);
    procedure Shutdown( const Eng: IUnitTestingEngine);
    procedure OnAttachSuite( Suite: ITestSuite;
             SetFixtureDatum: TSetFixtureDatumFunc;
             SetTestProcDatum: TSetTestProcDatumFunc;
             SetTestCaseDatum: TSetTestCaseDatumFunc);
    procedure OnProgressTick( const GrossResult: ITestResult; WorkDone, WorkLoad: integer);           virtual;

    procedure EnterOperation( isEntering: boolean);                                                   virtual;
    procedure FormLoaded( Form: TCustomForm);                                                         virtual;
    procedure FirstIdleEvent;                                                                         virtual;
    procedure FormDestroyed;                                                                          virtual;
    function  AcquireExecutive: IExecutive;                                                           virtual;
    procedure UpdateActionStatii( const Eng: IUnitTestingEngine);
    function  GetDoHaltOnFirstFailure: boolean;
    procedure SetDoHaltOnFirstFailure( Value: boolean);
    function  CanToggleHaltOnFirstFailure: boolean;      virtual;
    function  CanDialog: boolean;                        virtual;

    function TreeOwner: TComponent;         virtual; abstract;
    function TreeParent: TWinControl;       virtual; abstract;
    function TreeName: string;              virtual; abstract;

    procedure IntegrateSecondariesIntoMenus;              virtual; abstract;
    procedure AttachVisualTree;                           virtual;
    procedure AttachSecondaryLogger( const Addend: ITestLogger);
    procedure DetachSecondaryLogger( const Observer: ITestLogger);

    procedure ClearLog;                                           virtual; abstract;
    procedure Put( Level: TPutLevel; const Line: string; const Args: array of const); virtual; abstract;
    procedure SetDisplayState( Value: TSuiteRunnerState);         virtual;
    procedure Breathe;                                            virtual; abstract;
    procedure InitiateView( TestCaseCount: integer);              virtual;
    procedure SetApplicationTitle( const Title: string);          virtual;
    function  GetShowProgressBar: boolean;                        virtual;
    procedure SetShowProgressBar( Value: boolean);                virtual;
    function  GetProgressPosition: integer;                       virtual;
    procedure SetProgressPosition( Value: integer);               virtual;
    function  GetProgressMax: integer;                            virtual;
    procedure SetProgressMax( Value: integer);                    virtual;
    function  CheckAbort: boolean;                                virtual;
    procedure ClearAll;

    function  FindVisualNode(
                const NodeDatum: IInterface;
                var VisNode: IVisualTestSuiteNode; var Node: TNode): boolean;

  public
    constructor Create;                                          virtual;
    property ShowProgressBar: boolean     read GetShowProgressBar  write SetShowProgressBar;
    property ProgressPosition: integer    read GetProgressPosition write SetProgressPosition;
    property ProgressMax: integer         read GetProgressMax      write SetProgressMax;

  private
    procedure AttachVCLForm( Form: TCustomForm);
    procedure AttachExecutive;
    procedure AttachModel;
    procedure RegisterTestFixtures;
    procedure AcquireVisualTree;
    procedure PopulateTree;
    procedure Run;
    procedure ToggleSelections;
    procedure ClearSelections;
    function  CanClearSelections: boolean;
    procedure SelectAll;
    procedure SelectFailed;
    procedure SelectDefault;
    function  CanSelectFailed: boolean;
    procedure SetClearAll( Value: boolean);
    procedure RequestAbort;
    function  IsAbortRequested: boolean;

  private type
    TPerNode = reference to procedure( Leaf: TNode; var doBreak: boolean);
  private
    function  PerLeafNode( Proc: TPerNode): boolean; // Returns True if exit via PerNode break.
  end;


implementation













uses DUnitM.Executive, SysUtils, Math;

type
TFixtureNode = class( TViewModel_VCLForms.TNode)
  protected
    FFixture: ITestFixture;

    procedure Detach;                                        override;
    function  GetKind: TVisualTestSuiteNodeKind;             override;
    function  GetDisplayName: string;                        override;
    function  GetFullCycleCount: integer;                    override;
    procedure SetChecked( Value: boolean; Source: TSetCheckedSource);  override;
    function  Hint: string; override;
  public
    constructor Create( Model1: TViewModel_VCLForms; const Fixture1: ITestFixture);
  end;

TTestCaseNode = class( TViewModel_VCLForms.TNode)
  protected
    FTestCase: ITestCase;

    procedure Detach;                                        override;
    function  GetKind: TVisualTestSuiteNodeKind;             override;
    function  GetDisplayName: string;                        override;
    function  GetFullCycleCount: integer;                    override;
    procedure SetChecked( Value: boolean; Source: TSetCheckedSource);  override;
    function  IsChecked: boolean;                            override;
    function  Hint: string; override;
  public
    constructor Create( Model1: TViewModel_VCLForms; const TestCase1: ITestCase);
  end;

constructor TViewModel_VCLForms.Create;
begin
FAbortRequested := False
end;

procedure TViewModel_VCLForms.DetachSecondaryLogger( const Observer: ITestLogger);
begin
Model.Unsubscribe( Observer)
end;

procedure TViewModel_VCLForms.FormLoaded( Form: TCustomForm);
begin
AttachVCLForm( Form)
end;


function TViewModel_VCLForms.GetProgressMax: integer;
begin
result := 100
end;

function TViewModel_VCLForms.GetProgressPosition: integer;
begin
result := 0
end;

function TViewModel_VCLForms.GetShowProgressBar: boolean;
begin
result := False
end;

procedure TViewModel_VCLForms.InitiateView( TestCaseCount: integer);
begin
SetDisplayState( rsIdle);
SetProgressMax( 100);
SetShowProgressBar( False)
end;


function TViewModel_VCLForms.IsAbortRequested: boolean;
begin
result := FAbortRequested
end;

procedure TViewModel_VCLForms.AttachVCLForm( Form: TCustomForm);
begin
FForm := Form;
AttachExecutive;
SetApplicationTitle( FExecutive.ApplicationTitle)
end;


procedure TViewModel_VCLForms.AttachVisualTree;
begin
FModel.Subscribe( self)
end;

procedure TViewModel_VCLForms.SetApplicationTitle( const Title: string);
begin
FForm.Caption := Title
end;

procedure TViewModel_VCLForms.AttachExecutive;
begin
FExecutive := AcquireExecutive;
if not assigned( FExecutive) then exit;
if Application.MainForm = FForm then
  FExecutive.DeclareMainForm( FForm)
end;


procedure TViewModel_VCLForms.FirstIdleEvent;
begin
AttachModel;
RegisterTestFixtures;
IntegrateSecondariesIntoMenus;              // Overriden in view
AcquireVisualTree;
AttachVisualTree;                           // Overriden in view
PopulateTree;
InitiateView( FTestCaseCount);
FExecutive.StartUp
end;


function TViewModel_VCLForms.AcquireExecutive: IExecutive;
begin
result := DUnitM.Executive.AcquireExecutive
end;

procedure TViewModel_VCLForms.AttachModel;
begin
FModel := FExecutive.Model;
FModel.SetController( self)
end;


procedure TViewModel_VCLForms.AttachSecondaryLogger( const Addend: ITestLogger);
begin
FModel.Subscribe( Addend)
end;

procedure TViewModel_VCLForms.AcquireVisualTree;
var
  Factory: IVisualTestSuiteTreeFactory;
begin
if FExecutive.Services.Gn.Acquire<IVisualTestSuiteTreeFactory>( nil, Factory) then
  FTree := Factory.MakeVisualTestSuiteTree( TreeOwner, TreeParent, TreeName)
end;

procedure TViewModel_VCLForms.PopulateTree;
begin
Model.StartUp
end;

procedure TViewModel_VCLForms.OnAttachSuite(
  Suite: ITestSuite;
  SetFixtureDatum: TSetFixtureDatumFunc;
  SetTestProcDatum: TSetTestProcDatumFunc;
  SetTestCaseDatum: TSetTestCaseDatumFunc);
var
  Fixture: ITestFixture;

  procedure PopulateSubTree( const Fixture1: ITestFixture; const ParentVisualNode: IVisualTestSuiteNode);
  var
    FixtureContext : IVisualTestSuiteTreeChangeContext;
    TestCaseContext: IVisualTestSuiteTreeChangeContext;
    Newbie, TestCaseNewbs: IVisualTestSuiteNode;
    Addend: TNode;
    TestCase: ITestCase;
    Child: ITestFixture;
    Proc: ITestProcedure;
  begin
  FixtureContext := FTree.Change( ParentVisualNode);
  for Newbie in FixtureContext.Append( 1) do
    begin
    Addend := TFixtureNode.Create( self, Fixture1);
    Newbie.Datum := Pointer( Addend);
    FixtureContext.AttachRenderer( Newbie, Addend);
    if assigned( SetFixtureDatum) then
      SetFixtureDatum( Fixture1, Newbie);
    FTree.SetChecked( Newbie, False, csPopulation);
    for Child in Fixture1.FixtureChildren do
      PopulateSubTree( Child, Newbie);
    for Proc in Fixture1.TestProcedures do
      for TestCase in Proc.TestCases do
        begin
        TestCaseContext := FTree.Change( Newbie);
        for TestCaseNewbs in TestCaseContext.Append(1) do
          begin
          Addend := TTestCaseNode.Create( self, TestCase);
          if TestCase.Enabled then
            Inc( FTestCaseCount);
          TestCaseNewbs.Datum := Pointer( Addend);
          TestCaseContext.AttachRenderer( TestCaseNewbs, Addend);
          SetTestCaseDatum( TestCase, TestCaseNewbs);
          // At start-up the enabled state of each test case will the the DefaultEnabled.
          // Business rule:
          //  DefaultEnabled is true iff:
          //    (1) The test procedure is NOT marked as [Test(False)];  and
          //    (2) The test procedure is NOT set to ignored [Ignore('For some reason.')].
          FTree.SetChecked( TestCaseNewbs, TestCase.Enabled, csPopulation);
          end
        end
    end
  end;

begin
FTestCaseCount := 0;
FTree.BeforePopulate;
for Fixture in Suite.Fixtures do
  PopulateSubTree( Fixture, nil);
FTree.AfterPopulate
end;


procedure TViewModel_VCLForms.RegisterTestFixtures;
begin
FExecutive.RegisterTestFixtures
end;

procedure TViewModel_VCLForms.RequestAbort;
begin
FAbortRequested := True
end;

procedure TViewModel_VCLForms.ClearTree;
var
  GenericNode: IVisualTestSuiteNode;
begin
for GenericNode in FTree.AllNodes do
  begin
  TNode( GenericNode.Datum).FState := tsNeutral;
  TNode( GenericNode.Datum).FTestResult := nil;
  FTree.InvalidateRendering( GenericNode)
  end
end;

procedure TViewModel_VCLForms.Run;
begin
EnterOperation( True);
try
  ClearTree;
  Breathe;
  FModel.Run
finally
  EnterOperation( False)
  end
end;

procedure TViewModel_VCLForms.SetDisplayState( Value: TSuiteRunnerState);
begin
FState := Value;
if FState = rsIdle then
  FAbortRequested := False
end;

function TViewModel_VCLForms.GetDoHaltOnFirstFailure: boolean;
begin
result := FModel.BreakOnFirstFailure
end;

procedure TViewModel_VCLForms.SetDoHaltOnFirstFailure( Value: boolean);
begin
FModel.BreakOnFirstFailure := Value
end;

const
  LevelStrings: array[ SBD.Messages.TMessageLevel] of string = (
    { lvDebug      ==> }  '',
    { lvStatistic  ==> }  '',
    { lvHint       ==> }  'Hint (%d)',
    { lvRegular    ==> }  '',
    { lvWarning    ==> }  'Warning (%d)',
    { lvError      ==> }  'Error (%d)',
    { lvFatalError ==> }  'Fatal error (%d)'
    );


function MessageToString( const Msg: RMessage): string;
begin
result := LevelStrings[ Msg.FLevel];
if Pos( '%d', result) > 0 then
  result := Format( result, [Msg.FMessageCode]);
if result <> '' then
  result := result + ': ';
Result := result + Msg.FDisplayText
end;


procedure TViewModel_VCLForms.SetProgressMax( Value: integer);
begin
end;

procedure TViewModel_VCLForms.SetProgressPosition( Value: integer);
begin
end;

procedure TViewModel_VCLForms.SetShowProgressBar( Value: boolean);
begin
end;

procedure TViewModel_VCLForms.Shutdown( const Eng: IUnitTestingEngine);
begin
FModel.SetController( nil)
end;

procedure TViewModel_VCLForms.EnterOperation( isEntering: boolean);
begin
FisInOperation := isEntering
end;

function TViewModel_VCLForms.Executive: IExecutive;
begin
result := FExecutive
end;

procedure TViewModel_VCLForms.FormDestroyed;
var
  Exec: IExecutive;
begin
FTree := nil;
if (Application.MainForm = FForm) and assigned( FExecutive) then
  begin
  Exec := FExecutive;
  FExecutive := nil;
  Exec.ShutDown
  end
end;

function TViewModel_VCLForms.MenuDescription: string;
begin
result := 'Main form'
end;

function TViewModel_VCLForms.Model: IUnitTestingEngine;
begin
result := FModel
end;


procedure TViewModel_VCLForms.OnAuxilaryMessage(
  const TestResult: ITestResult; const TestCase: ITestCase;
  const Msg: RMessage; const NodeDatum: IInterface);
var
  Lvl: TPutLevel;
  VisNode: IVisualTestSuiteNode;
  Node: TNode;
  s: string;
begin
Lvl := lvDebug;
if FindVisualNode( NodeDatum, VisNode, Node) then
  begin
  // TODO: Attach message to the VisNode
  end;
case Msg.FLevel of
  SBD.Messages.lvDebug : Lvl := DUnitM.ViewModel_VCLForms.lvDebug;

  lvStatistic,
  lvHint,
  lvRegular,
  lvWarning   : Lvl := lvNormal;

  lvError,
  lvFatalError: Lvl := lvHighLight;
  end;
s := LevelStrings[ Msg.FLevel];
if Pos( '%d', s) > 0 then
  s := Format( s, [Msg.FMessageCode]);
if (s <> '') and assigned( TestResult) and (TestCase.DisplayName <> '') then
  s := s + ' - ';
if assigned( TestCase) then
    begin
    if TestCase.DisplayName <> '' then
      s := s + '"' + TestCase.DisplayName + '"'
    end
  else if assigned( TestResult) then
    begin
    if TestResult.NodeName <> '' then
      s := s + '"' + TestResult.NodeName + '"'
    end;
if s <> '' then
  s := s + ': ';
Put( Lvl, s + Msg.FDisplayText, [])
end;

procedure TViewModel_VCLForms.OnBeginRunInstance( Workload: integer);
begin
SetDisplayState( rsBetweenTests);
Put( lvNormal, 'Begin Run. Workload = %d', [Workload]);
SetShowProgressBar( True)
end;


function TViewModel_VCLForms.FindVisualNode(
  const NodeDatum: IInterface;
  var VisNode: IVisualTestSuiteNode; var Node: TNode): boolean;
begin
result := Supports( NodeDatum, IVisualTestSuiteNode, VisNode) and
          assigned( VisNode.Datum) and
          (TObject( VisNode.Datum) is TNode);
if result then
  Node := TNode( VisNode.Datum)
end;

procedure TViewModel_VCLForms.OnChangeStatus( NodeKind: TLoggerNode;
  const Fixture: ITestFixture; const TestProc: ITestProcedure;
  const TestCase: ITestCase; const TestResult: ITestResult;
  const NodeDatum: IInterface);
var
  VisNode: IVisualTestSuiteNode;
  Node: TNode;
  s: string;
  State: TSuiteRunnerState;
begin
State := rsSettingUp;
if FindVisualNode( NodeDatum, VisNode, Node) then
   begin
   Node.FState := TestResult.Status;
   Node.FTestResult := TestResult;
   FTree.InvalidateRendering( VisNode);
   Breathe;
   end;
if assigned( TestResult) and (Node.FState in [tsPass, tsWarn, tsFail, tsError]) then
  begin
  case TestResult.NodeKind of
    rnRoot: s := 'suite';
    rnNamespaceFixture, rnLeafFixture: s := 'Fixture ' + Fixture.Name;
    rnTestProc: s := 'procedure ' + TestProc.Name;
    rnTestCase: s := 'test case ' + TestCase.DisplayName;
    rnTestCaseRound: s := 'round'; // ? Round?
    end;
  Put( lvNormal, '%s completed in %d ms.', [s ,TestResult.Timing.FDuration_ms])
  end;
case TestResult.Status of
  tsNeutral  : State := rsBetweenTests;
  tsSetup    : State := rsSettingUp;
  tsTeardown : State := rsTearingDown;
  tsExecuting: State := rsExecuting;
  tsPass     : State := rsBetweenTests;
  tsWarn     : State := rsBetweenTests;
  tsFail     : State := rsBetweenTests;
  tsError    : State := rsBetweenTests;
  tsSkipped  : State := rsBetweenTests;
  end;
SetDisplayState( State)
end;


procedure TViewModel_VCLForms.OnEnableChange(
  const Eng: IUnitTestingEngine; const Proc: ITestProcedure; const Datum: IInterface);
begin
end;

procedure TViewModel_VCLForms.OnEndRunInstance( const RunResult: ITestResult);
begin
Put( lvNormal, 'End Run. RoundsCompleted = %d', [RunResult.RoundsCompleted]);
SetDisplayState( rsIdle)
end;




procedure TViewModel_VCLForms.OnProgressTick(
  const GrossResult: ITestResult; WorkDone, WorkLoad: integer);
begin
ProgressMax      := WorkLoad;
ProgressPosition := WorkDone
end;

function TViewModel_VCLForms.PerLeafNode( Proc: TPerNode): boolean;
var
  GenericNode: IVisualTestSuiteNode;
  Child: IVisualTestSuiteNode;
  Node: TNode;
  isLeaf: boolean;
begin
result := False;
for GenericNode in FTree.AllNodes do
  begin
  isLeaf := True;
  for Child in FTree.Nodes( GenericNode) do
    begin
    isLeaf := False;
    break
    end;
  if not isLeaf then continue;
  Node := TNode( GenericNode.Datum);
  Proc( Node, result);
  if result then break
  end
end;


function TViewModel_VCLForms.CanClearSelections: boolean;
begin
result := PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := Leaf.IsChecked
  end)
end;

function TViewModel_VCLForms.CanDialog: boolean;
begin
result := not FModel.IsRunning
end;

function TViewModel_VCLForms.CanSelectFailed: boolean;
begin
result := PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := Leaf.FState in [tsError, tsFail]
  end)
end;

function TViewModel_VCLForms.CanToggleHaltOnFirstFailure: boolean;
begin
result := not FModel.IsRunning
end;

function TViewModel_VCLForms.CheckAbort: boolean;
begin
result := FAbortRequested
end;

procedure TViewModel_VCLForms.SetClearAll( Value: boolean);
begin
PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := False;
  Leaf.SetChecked( Value, csUser);
  FTree.SetChecked( Leaf.FToken, Leaf.IsChecked, csUser)
  end)
end;

procedure TViewModel_VCLForms.ClearAll;
begin
ClearSelections;
ClearTree;
ClearLog;
ShowProgressBar := False
end;

procedure TViewModel_VCLForms.ClearSelections;
begin
SetClearAll( False)
end;

procedure TViewModel_VCLForms.SelectAll;
begin
SetClearAll( True)
end;

procedure TViewModel_VCLForms.SelectDefault;
begin
PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := False;
  Leaf.SetChecked( (Leaf as TTestCaseNode).FTestCase.Parent.GetDefaultEnabled, csUser);
  FTree.SetChecked( Leaf.FToken, Leaf.IsChecked, csUser)
  end)
end;

procedure TViewModel_VCLForms.SelectFailed;
begin
PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := False;
  Leaf.SetChecked( Leaf.FState in [tsError, tsFail], csUser);
  FTree.SetChecked( Leaf.FToken, Leaf.IsChecked, csUser)
  end)
end;

procedure TViewModel_VCLForms.ToggleSelections;
begin
PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := False;
  Leaf.SetChecked( not Leaf.IsChecked, csUser);
  FTree.SetChecked( Leaf.FToken, Leaf.IsChecked, csUser)
  end)
end;



procedure TViewModel_VCLForms.UpdateActionStatii( const Eng: IUnitTestingEngine);
begin

end;

procedure TViewModel_VCLForms.TNode.Attached( const Node: IVisualTestSuiteNode);
begin
FToken := Node
end;

constructor TViewModel_VCLForms.TNode.Create( Model1: TViewModel_VCLForms);
begin
FModel := Model1
end;

procedure TViewModel_VCLForms.TNode.Detach;
begin
FModel := nil;
FToken := nil
end;

function TViewModel_VCLForms.TNode.GetDoneCycleCount: integer;
begin
if assigned( FTestResult) then
    result := FTestResult.RoundsCompleted
  else
    result := 0
end;

function TViewModel_VCLForms.TNode.GetState: TTestStatus;
begin
result := FState
end;

function TViewModel_VCLForms.TNode.IsChecked: boolean;
begin
result := False
end;

{ TFixtureNode }

constructor TFixtureNode.Create(
  Model1: TViewModel_VCLForms; const Fixture1: ITestFixture);
begin
inherited Create( Model1);
FFixture := Fixture1;
FState   := tsNeutral
end;

procedure TFixtureNode.Detach;
begin
inherited Detach;
FFixture := nil
end;

function TFixtureNode.GetDisplayName: string;
begin
result := FFixture.Name
end;


function TFixtureNode.GetFullCycleCount: integer;
begin
result := FFixture.Load
end;

function TFixtureNode.GetKind: TVisualTestSuiteNodeKind;
begin
result := nkTestFixture
end;


function TFixtureNode.Hint: string;
begin
result := ''
end;

procedure TFixtureNode.SetChecked( Value: boolean; Source: TSetCheckedSource);
begin
end;

{ TTestCaseNode }

constructor TTestCaseNode.Create(
  Model1: TViewModel_VCLForms; const TestCase1: ITestCase);
begin
inherited Create( Model1);
FTestCase := TestCase1
end;

procedure TTestCaseNode.Detach;
begin
inherited Detach;
FTestCase := nil
end;

function TTestCaseNode.GetDisplayName: string;
begin
result := FTestCase.DisplayName
end;


function TTestCaseNode.GetFullCycleCount: integer;
begin
result := FTestCase.Load
end;

function TTestCaseNode.GetKind: TVisualTestSuiteNodeKind;
begin
result := nkTestCase
end;


function TTestCaseNode.Hint: string;
var
  Msg: RMessage;
  Child: ITestResult;

  procedure AccumulateMessage( var s: string);
  begin
  if Msg.FDisplayText = '' then exit;
  if s <> '' then
    s := s + #13#10;
  s := MessageToString( Msg)
  end;

begin
if assigned( FTestResult) then
  begin
  for Msg in FTestResult.Messages do
    AccumulateMessage( result);
  for Child in FTestResult.ChildResults do
    for Msg in Child.Messages do
      AccumulateMessage( result)
  end
end;


function TTestCaseNode.IsChecked: boolean;
begin
result := FTestCase.Enabled
end;


procedure TTestCaseNode.SetChecked( Value: boolean; Source: TSetCheckedSource);
begin
if (Source = csUser) and (FTestCase.Enabled <> Value) then
  FTestCase.Enabled := Value
  // Assume it is successful.
  // If not, we must test for failure and propagate back to the view
end;

end.
