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

unit DUnitM.TestCase;
interface
uses DUnitM.UnitTestFramework, DUnitM.TestNode, Rtti, SBD.Generics, SBD.Messages,
     DUnitM.MemoryMonitor, DUnitM.RunInstance, DUnitM.TestCaseParameters;

type

TTestCase = class( TTestNode, ITestCase, ITestCaseInternal)
  private
    function  GetEnabled: boolean;
    procedure SetEnabled( Value: boolean);
    function  DisplayName: string;
    function  Parent: ITestProcedure;
    procedure SetDeferredError( const ErMsg: string);
    function  GetDeferredError( var ErMsg: string): boolean;

  protected
    FName: string;
    FData: string;
    FEnabled: boolean;
    FParams: TTestCaseParamProcessor;
    FDeferredError: string;

    function  RequiresPreliminaryLoadCount: boolean;      virtual;
    function  Load: integer;                              override;
    procedure ShutDown;                                   override;
    function  NodeKind: TResultNodeKind;                  override;
    function  Name: string;                               override;
    function  NodeChildren: ISEnumerable<ITestNode>;      override;
    procedure Execute( Platter: TObject);
    function  EstimateRoundCount: integer;                virtual;
    function  MakeRunInstance( ParentAsObject: TReflector; TestBed: TGetObjectFunc; const Mem1: IMemoryMonitor): IRunInstance; virtual;
    function  HasEnabledTestCases: boolean;                                                override;

  public
    FProcType: TRttiMethod;

    constructor CreateCase( ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TestCaseAttribute);
    destructor  Destroy; override;
  end;

type
/// <remarks>Controller is TDataDrivenTestCase below</remarks>
TDataDrivenTestCaseNavManager = class( TAggregatedObject)
  public
    /// <remarks>
    /// Has signature like ...
    ///  function Navigator: IDataDrivenTestCaseNavigator;
    /// </remarks>
    FNavigatorMeth: TRttiMethod;
    /// <remarks>
    /// Actually of type TDataDrivenTestCase. This is the parent object and controller.
    /// </remarks>
    FParentTestCase: TTestCase;

    /// <remarks>
    /// Generally, this just points back to TDataDrivenTestCase.FProcType
    /// Has signature like ...
    ///  procedure TestLineItem( Item: TObject);
    /// </remarks>
    function  TestMeth: TRttiMethod; virtual;

    constructor Create( Parent: TTestCase; Method: TRttiMethod; Attribute: TDataDrivenTestAttributeBase); virtual;
    function  EstimatedCount( TestBed: TObject; var CountItems: integer): boolean;   virtual;
    function  OpenLineItemCursor( TestBed: TObject): ITestCaseLineItemCursor;        virtual;
    procedure TestLineItem( TestBed: TObject; Cursor: ITestCaseLineItemCursor);      virtual;
    function  Name: string;                                                          virtual;

  protected
    constructor PrimitiveCreate( Parent: TTestCase; Method: TRttiMethod);
  end;
TDataDrivenTestCaseNavManagerClass = class of TDataDrivenTestCaseNavManager;


TDataDrivenTestCase = class( TTestCase, IDataDrivenTestCase)
  private
    FisLoadCached: boolean;
    FLoad: integer;
    FNavManager: TDataDrivenTestCaseNavManager; // Can be a descendant type.

    function  AcquireDataDriver( Platter: TObject): IDataDrivenTestCaseNavigator;
  	function  OpenLineItemTestCursor( Platter: TObject): ITestCaseLineCursoredItem;

  protected
    function  Load: integer;                              override;
    function  RequiresPreliminaryLoadCount: boolean;      override;
    function  EstimateRoundCount: integer;                override;

  public
    constructor CreateCase(
      MgrCls1: TDataDrivenTestCaseNavManagerClass;
      ProcParentAsObject: TReflector; ProcType: TRttiMethod;
      Attri: TDataDrivenTestAttributeBase);

    destructor Destroy; override;
  end;

TSimpleTestCase = class( TTestCase)
  private
    constructor CreateCase( ProcParentAsObject: TReflector; ProcType: TRttiMethod);
  end;

TTestCaseList = class ( TList2<ITestCase>, ITestCasesInternal, IShutDown)
  public
    function  AddCase( ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TestCaseAttribute): ITestCaseInternal;
    function  AddDataDrivenCase(ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TDataDrivenTestAttributeBase): ITestCaseInternal;
    function  AddSimpleCase( ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TestAttribute): ITestCaseInternal;
    function  isDataDriven( ProcParentAsObject: TReflector; ProcType: TRttiMethod; var DDTestCaseFactory: IDataDrivenTestCaseFactory): boolean;
    function  AddByFactory( const Fact: IDataDrivenTestCaseFactory): ITestCaseInternal;
    procedure ShutDown;
  end;




TTestCase_RunInstance = class( TTestNode_RunInstanceWithChildren)
  protected
    FDeferredErrorCount: integer;
    FTestCase: ITestCaseInternal;
    FTestBed: TGetObjectFunc;
    FRoundsCompleted: integer;
    FMessages: IMessageList;

    procedure Clear;                        override;
    function  HasMessageList( var Lst: IMessageList): boolean; override;
    function  WorkLoad: integer;            override;
    function  TestNode: ITestNode;          override;
    function  RoundsCompleted: integer;     override;
    function  RoundsPerTestCase: integer;   override;
    function  TestCase: ITestCase;          override;
    function  NodeKind: TResultNodeKind;    override;
    procedure DoInnerRun;                   override;
    function  IgnoredCount: integer;        override;
    function  SkippedCount: integer;        override;
    function  ErrorCount: integer;          override;

  public
    constructor Create( ParentAsObject: TReflector; TestBed: TGetObjectFunc;
                        const Case1: ITestCaseInternal; const Mem1: IMemoryMonitor);
  end;


type
TDataDrivenTestCaseFactory = class( TInterfacedObject, IDataDrivenTestCaseFactory)
  private
    ProcAsObject: TReflector;
    ProcMethod: TRttiMethod;
    DrivingAttribute: DataDrivenTestAttribute;
    FRepeats, FSkips: integer;
    FName: string;
    FMgrCls: TDataDrivenTestCaseNavManagerClass;
    function Repeats: integer;
    function MethodSkips: integer;
    function Name: string;
    function Make: ITestCase;
  public
    constructor Create( Attri: DataDrivenTestAttribute; ProcParentAsObject: TReflector; ProcType: TRttiMethod);
    class function SignatureMatches( poTestMethod: TRttiMethod): boolean;
    class function MethodMatches_Procedure_Param1_Object( Method: TRttiMethod): boolean;
    class function MethodMatches_Function_Returns_IDataDrivenTestCaseNavigator( Method: TRttiMethod): boolean;
  end;




implementation






uses Generics.Collections, SysUtils, TypInfo, SBD.Utils.XML2, Math,
     SBD.Messages.Solution, DUnitM.TestCaseRound, DUnitM.RttiUtils;



constructor TTestCase.CreateCase(
  ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TestCaseAttribute);
begin
inherited CreateTestNode( ProcParentAsObject);
FName := Attri.FName;
FProcType := ProcType;
FEnabled := Parent.GetDefaultEnabled;
FViewTokens := TDictionary<ITestLogger,IInterface>.Create;
FData   := Attri.FData;
FParams := TTestCaseParamProcessor.Create( FProcType, FData);
FParams.Parse;
if FParams.FParseResult = psSyntaxError then
  raise Exception.Create( '[TestCase] syntax error')
end;

destructor TTestCase.Destroy;
begin
inherited;
FParams.Free
end;

function TTestCase.DisplayName: string;
begin
result := Name
end;

function TTestCase.EstimateRoundCount: integer;
begin
result := Parent.RepeatCount
end;

procedure TTestCase.Execute( Platter: TObject);
begin
FParams.Invoke( Platter)
end;

function TTestCase.GetDeferredError( var ErMsg: string): boolean;
begin
ErMsg := FDeferredError;
result := ErMsg <> ''
end;

function TTestCase.GetEnabled: boolean;
begin
result := FEnabled
end;

function TTestCase.Load: integer;
begin
result := Parent.RepeatCount
end;

function TTestCase.MakeRunInstance(
  ParentAsObject: TReflector; TestBed: TGetObjectFunc; const Mem1: IMemoryMonitor): IRunInstance;
begin
result := TTestCase_RunInstance.Create( ParentAsObject, TestBed, self, Mem1)
end;

function TTestCase.Name: string;
begin
result := FName
end;

function TTestCase.NodeChildren: ISEnumerable<ITestNode>;
var
  Accumulator: ITestNodes;
begin
Accumulator := TTestNodes.Create;
result := Accumulator
end;

function TTestCase.NodeKind: TResultNodeKind;
begin
result := rnTestCase
end;

function TTestCase.Parent: ITestProcedure;
begin
if (not assigned( FParent)) or
   (not Supports( FParent.FController, ITestProcedure, result)) then
  result := nil
end;


function TTestCase.RequiresPreliminaryLoadCount: boolean;
begin
result := False
end;

//    The NUnit format
//    ===================
//    Examples:
//       NUnit 2.5: http://nunit.org/files/testresult_25.txt
//       NUnit 3.0: http://nunit.org/files/testresult_30.txt
//       Will probably go iwth 2.5
//    Schema:
//       NUnit 2.5: http://nunit.org/files/testresult_schema_25.txt
//       ? unknown version at http://www.nunit.org/docs/2.2/files/Results.xsd

procedure TTestCase.SetDeferredError( const ErMsg: string);
begin
FDeferredError := ErMsg
end;

procedure TTestCase.SetEnabled( Value: boolean);
begin
FEnabled := Value
end;

procedure TTestCase.ShutDown;
begin
inherited;
FViewTokens.Clear
end;



constructor TDataDrivenTestCase.CreateCase(
  MgrCls1: TDataDrivenTestCaseNavManagerClass; ProcParentAsObject: TReflector; ProcType: TRttiMethod; Attri: TDataDrivenTestAttributeBase);
begin
inherited CreateTestNode( ProcParentAsObject);
if not assigned( MgrCls1) then
  MgrCls1 := TDataDrivenTestCaseNavManager;
FNavManager := MgrCls1.Create( self, ProcType, Attri);
FName       := FNavManager.Name;
FEnabled    := Parent.GetDefaultEnabled;
FData       := '';
FParams     := nil;
FisLoadCached := False;
FLoad         := 1
end;

destructor TDataDrivenTestCase.Destroy;
begin
FNavManager.Free;
inherited
end;

function TDataDrivenTestCase.EstimateRoundCount: integer;
begin
result := Load
end;


type
TDriver = class( TInterfacedObject, IDataDrivenTestCaseNavigator)
  private
    FPlatter: TObject;
    FMngr: TDataDrivenTestCaseNavManager;
    function EstimatedCount( var CountItems: integer): boolean;
  	function OpenLineItemCursor: ITestCaseLineItemCursor;
  public
    constructor Create( Platter: TObject; Mngr: TDataDrivenTestCaseNavManager);
  end;

constructor TDriver.Create( Platter: TObject; Mngr: TDataDrivenTestCaseNavManager);
begin
FPlatter := Platter;
FMngr    := Mngr
end;

function TDriver.EstimatedCount( var CountItems: integer): boolean;
begin
result := FMngr.EstimatedCount( FPlatter, CountItems)
end;

function TDriver.OpenLineItemCursor: ITestCaseLineItemCursor;
begin
result := FMngr.OpenLineItemCursor( FPlatter)
end;

type TTestCaseLineCursoredItem = class( TInterfacedObject, ITestCaseLineCursoredItem)
  private
    FPlatter: TObject;
    FMngr: TDataDrivenTestCaseNavManager;
    FPureCursor: ITestCaseLineItemCursor;
    FRowNumber: integer;
    function  Next: boolean;
	  function  LineItemDescription: string;
    procedure ConductTest;
  public
    constructor Create( Platter: TObject; Mngr: TDataDrivenTestCaseNavManager);
  end;

constructor TTestCaseLineCursoredItem.Create( Platter: TObject; Mngr: TDataDrivenTestCaseNavManager);
begin
FPlatter    := Platter;
FMngr       := Mngr;
FRowNumber  := 0;
FPureCursor := Mngr.OpenLineItemCursor( FPlatter)
end;

function TTestCaseLineCursoredItem.Next: boolean;
begin
result := FPureCursor.Next;
if result then
  Inc( FRowNumber)
end;

function TTestCaseLineCursoredItem.LineItemDescription: string;
begin
result := Format( 'Line item %d', [FRowNumber]);
FPureCursor.GetLineItemDescription( FRowNumber, result)
end;

procedure TTestCaseLineCursoredItem.ConductTest;
begin
FMngr.TestLineItem( FPlatter, FPureCursor)
end;


function TDataDrivenTestCase.AcquireDataDriver( Platter: TObject): IDataDrivenTestCaseNavigator;
begin
result := TDriver.Create( Platter, FNavManager)
end;

function TDataDrivenTestCase.Load: integer;
var
  Obj: TObject;
  Cursor: IDataDrivenTestCaseNavigator;
  Fixture: ITestFixtureInternal;
begin
if not FisLoadCached then
  begin
  result := 1;
  try
    FisLoadCached := True;
    FLoad := 1;
    if FDeferredError <> '' then exit;
    if Supports( Parent.Parent, ITestFixtureInternal, Fixture) then
        Obj := Fixture.PreliminaryPlatterObject
      else
        Obj := nil;
    if assigned( Obj) then
        begin
        Fixture.Setup( Obj);
        try
          Cursor := AcquireDataDriver( Obj);
          try
            FLoad  := ForceGetEstimatedCount( Cursor)
          finally
            Cursor := nil
            end
        finally
          Fixture.Teardown( Obj)
          end;
        if FLoad <= 0 then
          begin
          FLoad := 1;
          SetDeferredError( 'Data driven test procedure returned an invalid preliminary load estimate.')
          end
        end
      else
        SetDeferredError( 'Preliminatry load estimate could not be computed due to unavailable test fixture.')
  except on E: Exception do
      begin
      FLoad := 1;
      SetDeferredError( E.Message)
      end
    end
  end;
result := FLoad
end;


function TDataDrivenTestCase.OpenLineItemTestCursor( Platter: TObject): ITestCaseLineCursoredItem;
begin
result := TTestCaseLineCursoredItem.Create( Platter, FNavManager)
end;

function TDataDrivenTestCase.RequiresPreliminaryLoadCount: boolean;
begin
result := not FisLoadCached
end;


{ TSimpleTestCase }

constructor TSimpleTestCase.CreateCase(
  ProcParentAsObject: TReflector; ProcType: TRttiMethod);
begin
inherited CreateTestNode( ProcParentAsObject);
FProcType := ProcType;
FName := FProcType.Name;
FEnabled := Parent.GetDefaultEnabled;
FData   := '';
FParams := TTestCaseParamProcessor.CreateSimple( FProcType)
end;





function TTestCaseList.AddCase( ProcParentAsObject: TReflector;
  ProcType: TRttiMethod; Attri: TestCaseAttribute): ITestCaseInternal;
begin
result := TTestCase.CreateCase( ProcParentAsObject, ProcType, Attri);
Add( result)
end;

function TTestCaseList.AddDataDrivenCase(ProcParentAsObject: TReflector;
  ProcType: TRttiMethod; Attri: TDataDrivenTestAttributeBase): ITestCaseInternal;
begin
result := TDataDrivenTestCase.CreateCase( nil, ProcParentAsObject, ProcType, Attri);
Add( result)
end;

function TTestCaseList.AddSimpleCase(ProcParentAsObject: TReflector;
  ProcType: TRttiMethod; Attri: TestAttribute): ITestCaseInternal;
begin
result := TSimpleTestCase.CreateCase( ProcParentAsObject, ProcType);
Add( result)
end;



function TTestCaseList.isDataDriven(
  ProcParentAsObject: TReflector; ProcType: TRttiMethod;
  var DDTestCaseFactory: IDataDrivenTestCaseFactory): boolean;
var
  oAttrib: TCustomAttribute;
  Attri, BestMatch: TDataDrivenTestAttributeBase;
begin
result    := False;
BestMatch := nil;
for Attri in TAttributes.Get<TDataDrivenTestAttributeBase>( ProcType) do
  begin
  if not Attri.SignatureMatches( ProcType) then continue;
  result    := True;
  BestMatch := Attri
  // If there are multiple matches, we take the last.
  end;
if result then
  DDTestCaseFactory := Attri.AcquireFactory( ProcParentAsObject, ProcType)
end;


function TTestCaseList.AddByFactory(
  const Fact: IDataDrivenTestCaseFactory): ITestCaseInternal;
begin
result := Fact.Make as ITestCaseInternal;
Add( result)
end;

procedure TTestCaseList.ShutDown;
var
  Child: ITestCase;
  Shut: IShutDown;
begin
for Child in (self as ITestCasesInternal) do
  if Supports( Child, IShutDown, Shut) then
    Shut.ShutDown
end;






type
TRepeatRowsCursor = class( TInterfacedObject, ITestCaseLineCursoredItem)
  private
    FCase: ITestCaseInternal;
    FTestBed: TObject;
    FRounds: integer;
    FRound: integer;
    function  Next: boolean;
	  function  LineItemDescription: string;
    procedure ConductTest;
  public
    constructor Create( Case1: ITestCaseInternal; TestBed1: TObject);
  end;

function TTestCase.HasEnabledTestCases: boolean;
var
  sErr: string;
begin
result := GetEnabled and (not GetDeferredError( sErr))
end;




constructor TTestCase_RunInstance.Create(
  ParentAsObject: TReflector;
  TestBed: TGetObjectFunc; const Case1: ITestCaseInternal;
  const Mem1: IMemoryMonitor);
begin
FMessages := TMessageList.Create;
inherited Create( ParentAsObject, Mem1);
FTestCase := Case1;
FTestBed  := TestBed
end;

procedure TTestCase_RunInstance.Clear;
begin
inherited;
FMessages.Clear;
FDeferredErrorCount := 0;
FRoundsCompleted    := 0;
if assigned( FMem) then
  FMem.HeapMonitoring_OFF
end;


procedure TTestCase_RunInstance.DoInnerRun;
var
  TestBedObj: TObject;
  sErr: string;
  Addend: IRunInstance;
  sIgnoreReason: string;
  DD: IDataDrivenTestCase;
  Cursor: ITestCaseLineCursoredItem;
begin
TestBedObj := FTestBed();
if FTestCase.Enabled and FTestCase.GetDeferredError( sErr) then
    begin
    FDeferredErrorCount := 1;
    Status := tsError;
    PutSimpleMessage( SyntaxError, sErr, lvError)
    end
  else if FTestCase.Enabled and (not FRunInstanceServices.IsAborted) then
    begin
    if Supports( FTestCase, IDataDrivenTestCase, DD) then
        Cursor := DD.OpenLineItemTestCursor( TestBedObj)
      else
        Cursor := TRepeatRowsCursor.Create( FTestCase, TestBedObj);
	  Status := tsExecuting;
    while Cursor.Next and (not FRunInstanceServices.IsAborted) do
      begin
      Addend := TTestCaseRound_RunInstance.Create( FWeakClients, TestBedObj, FTestCase, FRoundsCompleted, FMem, Cursor);
      FChildren.Add( Addend);
      Addend.Run( FRunInstanceServices,
        procedure ( ChildStatus: TTestStatus)
        begin
        if ChildStatus in [tsPass..tsSkipped] then
          Inc( FRoundsCompleted);
        if ChildStatus in [tsSetup, tsExecuting, tsTeardown] then
          Status := ChildStatus
        end);
      FExecuted := True;
      if ErrorCount > 0 then break
      end;
	  if ErrorCount > 0 then
	    Status := tsError
    else if FailCount > 0 then
	    Status := tsFail
    else if WarnCount > 0 then
	    Status := tsWarn
     else if FRoundsCompleted > 0 then
	     Status := tsPass
     else
       // Skipped because there were no rounds to execute.
       Status := tsSkipped
	  end
  else if FTestCase.Parent.IsIgnored( sIgnoreReason) and (not FRunInstanceServices.IsAborted) then
    begin
    // Skipped because the test case is marked as ignore
    Status := tsSkipped;
    Self.PutSimpleMessage( IgnoredTestCase, sIgnoreReason, lvHint)
    end
  else
    // Skipped because teh test case is disabled (but not ignored) or user aborted
    Status := tsSkipped
end;

function TTestCase_RunInstance.ErrorCount: integer;
begin
result := FDeferredErrorCount + inherited ErrorCount
end;

function TTestCase_RunInstance.HasMessageList(var Lst: IMessageList): boolean;
begin
result := True;
Lst    := FMessages
end;

function TTestCase_RunInstance.IgnoredCount: integer;
// The ignored count of a test case is ...
//  If the status is skipped and the test proc is marked as Ignore, then it is RoundsPerTestCase.
//  Otherwise it is zero.
var
  sIgnoreReason: string;
begin
if (FStatus = tsSkipped) and FTestCase.Parent.IsIgnored( sIgnoreReason) then
    result := RoundsPerTestCase
  else
    result := 0
end;

function TTestCase_RunInstance.NodeKind: TResultNodeKind;
begin
result := rnTestCase
end;

function TTestCase_RunInstance.RoundsCompleted: integer;
begin
result := FRoundsCompleted
end;

function TTestCase_RunInstance.RoundsPerTestCase: integer;
begin
result := Max( FTestCase.EstimateRoundCount, FChildren.Count)
end;

function TTestCase_RunInstance.SkippedCount: integer;
// The skip count of a test case is ...
//  If the status is some kind of done (pass, fail, error, skip) then
//   it is the remainder (RoundsPerTestCase - passes - fails - errors - ignores)
//  Otherwise it is zero
begin
if FStatus in [tsPass..tsSkipped] then
    result := RoundsPerTestCase - PassCount - FailCount - ErrorCount - IgnoredCount
  else
    result := 0
end;

function TTestCase_RunInstance.TestCase: ITestCase;
begin
result := FTestCase
end;

function TTestCase_RunInstance.TestNode: ITestNode;
begin
result := FTestCase as ITestNode
end;

function TTestCase_RunInstance.WorkLoad: integer;
begin
if FTestCase.Enabled then
    result := RoundsPerTestCase
  else
    result := 0
end;

constructor TDataDrivenTestCaseNavManager.PrimitiveCreate(
  Parent: TTestCase; Method: TRttiMethod);
begin
inherited Create( Parent as IInterface);
FParentTestCase           := Parent;
FParentTestCase.FProcType := Method;
FNavigatorMeth            := nil
end;


//    [DataDrivenTest]
//    [Description('Tests the StringToDate() function')]
//      function  Navigator: IDataDrivenTestCaseNavigator;
//      procedure TestLineItem( Item: TObject);

constructor TDataDrivenTestCaseNavManager.Create(
  Parent: TTestCase; Method: TRttiMethod; Attribute: TDataDrivenTestAttributeBase);
begin
PrimitiveCreate( Parent, Method);
NextDeclaredMethod( Method, FNavigatorMeth)
end;


function TDataDrivenTestCaseNavManager.EstimatedCount(
  TestBed: TObject; var CountItems: integer): boolean;
var
  Args: array of TValue;
  Nav: IDataDrivenTestCaseNavigator;
  Crs: ITestCaseLineItemCursor;
begin
// FNavigatorMeth has signature function Navigator: IDataDrivenTestCaseNavigator();
Nav := FNavigatorMeth.Invoke( TestBed, []).AsType<IDataDrivenTestCaseNavigator>;
Nav._AddRef;
CountItems := 0;
result := assigned( Nav) and Nav.EstimatedCount( CountItems);
if result or (not assigned( Nav)) then exit;
Crs := Nav.OpenLineItemCursor;
result := assigned( Crs);
if result then
  while Crs.Next do
    Inc( CountItems)
end;


function TDataDrivenTestCaseNavManager.Name: string;
begin
result := TestMeth.Name
end;

function TDataDrivenTestCaseNavManager.OpenLineItemCursor(
  TestBed: TObject): ITestCaseLineItemCursor;
var
  Args: array of TValue;
  Nav: IDataDrivenTestCaseNavigator;
begin
// FNavigatorMeth has signature function Navigator: IDataDrivenTestCaseNavigator();
Nav := FNavigatorMeth.Invoke( TestBed, []).AsType<IDataDrivenTestCaseNavigator>;
Nav._AddRef;
result := Nav.OpenLineItemCursor
end;

procedure TDataDrivenTestCaseNavManager.TestLineItem(
  TestBed: TObject; Cursor: ITestCaseLineItemCursor);
// TestMeth() has signature like ...
//  procedure TestLineItem( Item: TObject);
begin
TestMeth.Invoke( TestBed, [Cursor.LineItemObject])
end;

function TDataDrivenTestCaseNavManager.TestMeth: TRttiMethod;
begin
result := FParentTestCase.FProcType
end;


class function TDataDrivenTestCaseFactory.MethodMatches_Procedure_Param1_Object( Method: TRttiMethod): boolean;
// Method is like ... procedure TestIt( Item: TObject);
begin
result := (Method.MethodKind = mkProcedure) and
          (Length( Method.GetParameters) = 1) and
          (Method.GetParameters[0].ParamType.TypeKind = tkClass) and
          ((Method.GetParameters[0].Flags * [pfArray, pfOut]) = [])
end;

class function TDataDrivenTestCaseFactory.MethodMatches_Function_Returns_IDataDrivenTestCaseNavigator( Method: TRttiMethod): boolean;
// Method is like ... function Navigator: IDataDrivenTestCaseNavigator;
begin
result := (Method.MethodKind = mkFunction) and
          (Length( Method.GetParameters) = 0) and
          (Method.ReturnType.TypeKind = tkInterface) and
          (Method.ReturnType is TRttiInterfaceType) and
          IsEqualGUID(
              TRttiInterfaceType( Method.ReturnType).GUID,
              IDataDrivenTestCaseNavigator)
end;


class function TDataDrivenTestCaseFactory.SignatureMatches(
  poTestMethod: TRttiMethod): boolean;
var
  Next: TRttiMethod;
begin
  // like ... procedure TestIt( Item: TObject);
  result := MethodMatches_Procedure_Param1_Object( poTestMethod) and
            NextDeclaredMethod( poTestMethod, Next) and
            // like ... function Navigator: IDataDrivenTestCaseNavigator;
            MethodMatches_Function_Returns_IDataDrivenTestCaseNavigator( Next)
end;

constructor TDataDrivenTestCaseFactory.Create( Attri: DataDrivenTestAttribute; ProcParentAsObject: TReflector; ProcType: TRttiMethod);
begin
ProcAsObject := ProcParentAsObject;
ProcMethod   := ProcType;
DrivingAttribute := Attri;
FRepeats     := 1;
FSkips       := 1;
FMgrCls      := TDataDrivenTestCaseNavManager;
FName        := ProcMethod.Name
end;

function TDataDrivenTestCaseFactory.Repeats: integer;
begin
result := FRepeats
end;

function TDataDrivenTestCaseFactory.MethodSkips: integer;
begin
result := FSkips
end;

function TDataDrivenTestCaseFactory.Name: string;
begin
result := FName
end;

function TDataDrivenTestCaseFactory.Make: ITestCase;
begin
result := TDataDrivenTestCase.CreateCase( FMgrCls, ProcAsObject, ProcMethod, DrivingAttribute)
end;


constructor TRepeatRowsCursor.Create( Case1: ITestCaseInternal; TestBed1: TObject);
begin
FCase    := Case1;
FTestBed := TestBed1;
FRounds  := FCase.Parent.RepeatCount;
FRound   := -1
end;



function TRepeatRowsCursor.LineItemDescription: string;
begin
if FRounds = 1 then
    result := 'single round'
  else
    result := Format( 'Round %d', [FRound + 1])
end;

function TRepeatRowsCursor.Next: boolean;
begin
result := FRound <= (FRounds - 2);
if result then
  Inc( FRound)
end;


procedure TRepeatRowsCursor.ConductTest;
begin
FCase.Execute( FTestBed)
end;


end.
