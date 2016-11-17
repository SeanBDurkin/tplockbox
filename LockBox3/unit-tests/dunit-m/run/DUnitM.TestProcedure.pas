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

unit DUnitM.TestProcedure;
interface
uses DUnitM.TestNode, Rtti, SBD.Generics, DUnitM.MemoryMonitor, DUnitM.UnitTestFramework,
     SBD.ServiceProvider;

type
TProcedureList = class ( TList2<ITestProcedure>, IProceduresInternal, IShutDown)
  public
    function  AddProcedure( const Factory: ITestNodeFactory; Method: TRttiMethod; FixtureType: TRttiType): ITestProcedureInternal;
    procedure ShutDown;
  end;


procedure RegisterServices( const Provider: IServiceProvider);

implementation


uses DUnitM.RttiUtils, SysUtils, TypInfo, SBD.Messages, DUnitM.TestCase,
     SBD.Messages.Solution, DUnitM.RunInstance;

type
TProcedureFactory = class sealed( TBaseNodeFactory)
  protected
    function  MakeNode(  const NodeName: string;
                         Method: TRttiMethod;
                         FixtureType: TRttiType;
                         TestCaseAttri: TDUnitXAttributeBase
                         ): ITestNode;                    override;
  public
    [Configuration('rnTestProc')] constructor ServiceModeCreate;

  private type
    TTestProcedure = class( TTestNode, ITestProcedure, ITestProcedureInternal, IFixtureChild)
      private
        FTestMeth: TRttiMethod;
        FCases: ITestCasesInternal;
        FIgnoreMemoryLeaks: boolean;
        FIgnore: boolean;
        FIgnoreReason: string;
        FDefaultEnabled: boolean;
        FDescription: string;
        FName: string;
        FRepeats: integer;
        FSkips: integer;

        function Parent: ITestFixture;
        function TestCases: ISEnumerable<ITestCase>;
        function RepeatCount: integer;
        function Description: string;
        function GetDefaultEnabled: boolean;
        function IsIgnored( var IgnoreReason: string): boolean;
        function MakeRunInstance( ParentAsObject: TReflector; TestBed: TGetObjectFunc; const Mem1: IMemoryMonitor): IRunInstance;
        function  IgnoreLeaks: boolean;
       function  DeclaredMethodSkips: integer;

        // IFixtureChild
        function  MakeRunInstanceOfFixtureChild( const ParentFixture: IRunInstance; var Addend: IRunInstance): boolean;

      protected
        function  NodeKind: TResultNodeKind;                                                   override;
        procedure ShutDown;                                                                    override;
        function  Name: string;                                                                override;
        function  NodeChildren: ISEnumerable<ITestNode>;                                       override;
        function  Load: integer;                                                               override;
      end;
  end;


procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterServiceClass( ITestNodeFactory, TProcedureFactory);

end;


function TProcedureFactory.TTestProcedure.DeclaredMethodSkips: integer;
begin
result := FSkips
end;

function TProcedureFactory.TTestProcedure.Description: string;
begin
result := FDescription
end;

function TProcedureFactory.TTestProcedure.GetDefaultEnabled: boolean;
begin
result := FDefaultEnabled
end;

function TProcedureFactory.TTestProcedure.IgnoreLeaks: boolean;
begin
result := FIgnoreMemoryLeaks
end;

function TProcedureFactory.TTestProcedure.IsIgnored( var IgnoreReason: string): boolean;
begin
result := FIgnore;
IgnoreReason := FIgnoreReason
end;

function TProcedureFactory.TTestProcedure.Load: integer;
var
  TestCase: ITestCase;
begin
result := 0;
for TestCase in TestCases do
  if TestCase.Enabled then
    inc( result, TestCase.Load)
end;

function TProcedureFactory.TTestProcedure.Name: string;
begin
  result := FName
end;

function TProcedureFactory.TTestProcedure.NodeChildren: ISEnumerable<ITestNode>;
var
  Accumulator: ITestNodes;
  TestCase: ITestCase;
begin
  Accumulator := TTestNodes.Create;
  result := Accumulator;
  for TestCase in TestCases do
    Accumulator.Add( TestCase as ITestNode)
end;

function TProcedureFactory.TTestProcedure.NodeKind: TResultNodeKind;
begin
  result := rnTestProc
end;

function TProcedureFactory.TTestProcedure.Parent: ITestFixture;
begin
  result := ParentNode as ITestFixture
end;

function TProcedureFactory.TTestProcedure.RepeatCount: integer;
begin
  result := FRepeats
end;

procedure TProcedureFactory.TTestProcedure.ShutDown;
begin
  inherited;
  FCases.ShutDown
end;

function TProcedureFactory.TTestProcedure.TestCases: ISEnumerable<ITestCase>;
begin
  result := FCases
end;

function TProcedureList.AddProcedure(
  const Factory: ITestNodeFactory; Method: TRttiMethod; FixtureType: TRttiType): ITestProcedureInternal;
begin
  result := Factory.MakeNode( '', Method, FixtureType, nil) as ITestProcedureInternal;
  Add( result as ITestProcedure)
end;


procedure TProcedureList.ShutDown;
var
  Proc: ITestProcedure;
  Shut: IShutdown;
begin
  for Proc in (self as IProceduresInternal) do
    if Supports( Proc, IShutdown, Shut) then
      Shut.ShutDown
end;

type
TProcedure_RunInstance = class( TTestNode_RunInstanceWithChildren)
  private
    FTestBed: TGetObjectFunc;
    FTestProc: ITestProcedureInternal;

  protected
    function  TestNode: ITestNode;                 override;
    function  RoundsPerTestCase: integer;          override;
    function  Fixture: ITestFixture;               override;
    function  NodeKind: TResultNodeKind;           override;
    procedure DoInnerRun;                          override;

  public
    constructor Create( ParentAsObject: TReflector; TestBed: TGetObjectFunc; const TestProc1: ITestProcedureInternal; const Mem1: IMemoryMonitor);
  end;

function TProcedureFactory.TTestProcedure.MakeRunInstance(
  ParentAsObject: TReflector; TestBed: TGetObjectFunc; const Mem1: IMemoryMonitor): IRunInstance;
begin
  result := TProcedure_RunInstance.Create( ParentAsObject, TestBed, self, Mem1)
end;

function TProcedureFactory.TTestProcedure.MakeRunInstanceOfFixtureChild(
  const ParentFixture: IRunInstance; var Addend: IRunInstance): boolean;
var
  FixRun: IFixtureRunInstance;
begin
  result := Supports( ParentFixture, IFixtureRunInstance, FixRun);
  if result then
    Addend := FixRun.MakeProcedureRunInstance( self)
end;

constructor TProcedure_RunInstance.Create(
  ParentAsObject: TReflector;
  TestBed: TGetObjectFunc; const TestProc1: ITestProcedureInternal;
  const Mem1: IMemoryMonitor);
var
  TestCase: ITestCase;
begin
inherited Create( ParentAsObject, Mem1);
  FTestBed  := TestBed;
  FTestProc := TestProc1;
  for TestCase in FTestProc.TestCases do
    FChildren.Add( (TestCase as ITestCaseInternal)
      .MakeRunInstance( FWeakClients, FTestBed, FMem))
end;

procedure TProcedure_RunInstance.DoInnerRun;
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
      FExecuted := True;
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

function TProcedure_RunInstance.Fixture: ITestFixture;
begin
  result := FTestProc.Parent
end;

function TProcedure_RunInstance.NodeKind: TResultNodeKind;
begin
  result := rnTestProc
end;

function TProcedure_RunInstance.RoundsPerTestCase: integer;
begin
  result := FTestProc.Load
end;

function TProcedure_RunInstance.TestNode: ITestNode;
begin
  result := FTestProc as ITestNode
end;


function TProcedureFactory.MakeNode( const NodeName: string; Method: TRttiMethod;
  FixtureType: TRttiType; TestCaseAttri: TDUnitXAttributeBase): ITestNode;
var
  Desc: DescriptionAttribute;
  TestAttri: TestAttribute;
  IgnoreAttri: IgnoreAttribute;
  RepeatsAttri: RepeatsAttribute;
  IgnoreMemoryLeaksAttri: IgnoreMemoryLeaksAttribute;
  DataDrivenTestAttri: TDataDrivenTestAttributeBase;
  Addend: TTestProcedure;
  TestCaseAttri2: TestCaseAttribute;
  Skips: integer;
  DDFactory: IDataDrivenTestCaseFactory;
begin
  Addend := TTestProcedure.CreateTestNode( FParentAsObject);
  result := Addend as ITestNode;
  Addend.FTestMeth := Method;
  Addend.FCases := TTestCaseList.Create;
  Addend.FIgnoreMemoryLeaks := False;
  Addend.FIgnore := False;
  Addend.FIgnoreReason := '';
  Addend.FDefaultEnabled := True;
  Addend.FRepeats := 1;
  Addend.FName := Addend.FTestMeth.Name;
  for Desc in TAttributes.Get<DescriptionAttribute>( Method) do
    begin
      if Addend.FDescription <> '' then
        Addend.FDescription := Addend.FDescription + #13#10;
      Addend.FDescription := Addend.FDescription + Desc.FText
    end;
  if TAttributes.GetFirst<IgnoreAttribute>( Addend.FTestMeth, IgnoreAttri) then
  begin
    Addend.FIgnore := True;
    Addend.FDefaultEnabled := False;
    Addend.FIgnoreReason := IgnoreAttri.FReason
  end;
  if TAttributes.GetFirst<RepeatsAttribute>( Addend.FTestMeth, RepeatsAttri) then
    Addend.FRepeats := RepeatsAttri.FCount;
  if TAttributes.GetFirst<TestAttribute>( Addend.FTestMeth, TestAttri) then
    Addend.FDefaultEnabled := TestAttri.FEnabled and (not Addend.FIgnore);
  if TAttributes.GetFirst<IgnoreMemoryLeaksAttribute>( Addend.FTestMeth, IgnoreMemoryLeaksAttri) then
    Addend.FIgnoreMemoryLeaks := True;

  {$if CompilerVersion = 21.0}
  try
  {$ifend}

    if TAttributes.GetFirst<TestCaseAttribute>( Addend.FTestMeth, TestCaseAttri2) then
      begin
      for TestCaseAttri2 in TAttributes.Get<TestCaseAttribute>( Addend.FTestMeth) do
        Addend.FCases.AddCase( Addend.FWeakClients, Addend.FTestMeth, TestCaseAttri2)
      end

    else if Addend.FCases.isDataDriven( Addend.FWeakClients, Addend.FTestMeth, DDFactory) then
        begin
        Addend.FRepeats := DDFactory.Repeats;
        Addend.FSkips   := DDFactory.MethodSkips;
        Addend.FName    := DDFactory.Name;
        Addend.FCases.AddByFactory( DDFactory)
        end

    else if TAttributes.GetFirst<TestAttribute>( Addend.FTestMeth, TestAttri) then
      Addend.FCases.AddSimpleCase( Addend.FWeakClients, Addend.FTestMeth, TestAttri);

  {$if CompilerVersion = 21.0}
  except
  begin
    if TCompileInformation.BuiltWithRuntimePackages then
      raise Exception.Create(
        'Due to a compiler bug in Delphi 2010, when your application is "Build with packages"=CHECKED,'#13#10 +
        'you must compile after build. To just build and then run, will result in a defective executable.');
      // NOTE: The compiler bug may also prevent this exception message being propagated, as it will
      //  be maksed by AV's.
  end
end;
  {$ifend}

  if Addend.FCases.Count = 0 then
    raise Exception.Create( 'Test procedure has no valid test cases.')
end;

constructor TProcedureFactory.ServiceModeCreate;
begin
end;

end.
