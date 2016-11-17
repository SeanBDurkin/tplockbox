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

unit DUnitM.UnitTestFramework.Solution;
interface
uses DUnitM.UnitTestFramework, SBD.ServiceProvider, Rtti, SBD.Generics,
     DUnitM.MemoryMonitor;

procedure RegisterServices( const Provider: IServiceProvider);
// Provides:
//   GUID                  config        ArrayId                          Mode
//   ----------------------------------------------------------------------------------
//   IUnitTestingEngine        ''        ''                              Competitive
//
// Requires:
//   GUID                  config        Mode
//   -------------------------------------------------
//   IMemoryMonitor            ''        Competitive

implementation



// ****************************************************************
// ********** Common definitions **********************************
// ****************************************************************
uses SBD.Messages, Classes, DUnitM.StringUtils, DUnitM.RttiUtils,
     Generics.Collections, SysUtils, TypInfo, SBD.Utils.XML2,
     Math, Variants, StrUtils, SBD.Messages.Solution, DUnitM.TestNode,
     DUnitM.Fixture;

type





























// ****************************************************************
// ********** Unit Testing Engine *********************************
// ****************************************************************

TUnitTestingEngine = class( TInterfacedObject, IUnitTestingEngine, IRunnerExecutiveServices)
  private
    [Injection('rnRoot')] FSuiteFactory: ITestNodeFactory;
    [Injection] FMemMonitor: IMemoryMonitor;

  private
    FStarted: boolean;
    FCtx: TRttiContext;
    FSuite: ITestSuite;
    FLoggers: IList2<ITestLogger>;
    FController: IUnitTestingController;
    FRunInstanceCount: integer;
    FAbortPending, FAborted: boolean;
    FBreakOnFirstFail: boolean;
    FDeferredLoggerException: Exception;
    FTroubleMaker: ITestLogger;

    procedure RegisterFixtureClass( FixtureCls: TClass);
    function  Suite: ITestSuite;
    procedure EnterLeaveRunners( Delta: integer);
    procedure Subscribe( const Observer: ITestLogger);
    procedure Unsubscribe( const Observer: ITestLogger);
    function  Run: ITestResult;
    procedure StartUp;
    procedure Shutdown;
    procedure Abort;
    function  IsAborted: boolean;
    procedure SetController( const Controller: IUnitTestingController);
    function  Can_Run: boolean;
    function  Can_Abort: boolean;
    function  IsRunning: boolean;
    procedure OnBeginRunInstance( const Instance: IRunInstance; Workload: integer);
    procedure OnEndRunInstance( const Instance: IRunInstance; const RunResult: ITestResult);
    procedure OnChangeStatus(
                        NodeKind: TLoggerNode;
                  const Fixture: ITestFixture;
                  const TestProc: ITestProcedure;
                  const TestCase: ITestCase;
                  const TestResult: ITestResult);
    procedure OnAuxilaryMessage( const Node: ITestNode; const TestResult: ITestResult; const TestCase: ITestCase; const Msg: RMessage; Round: integer);
    function  GetBreakOnFirstFailure: boolean;
    procedure SetBreakOnFirstFailure( Value: boolean);
    procedure MarkLoggerException( const TroubleMaker: ITestLogger; const LoggerException: Exception);
    procedure FlushLoggerExceptions;

  strict private
    procedure StartUpLogger( const Logger: ITestLogger);

  strict private type
    TLoggerProc = reference to procedure( const Logger: ITestLogger; var doBreak: boolean);

  private
    procedure ForEachLogger( Proc: TLoggerProc; doTrapExceptions: boolean);

  public
    [Configuration] constructor ServiceModeCreate;
    destructor Destroy; override;
  end;















// ****************************************************************
// ********** Unit Testing Engine - Solution **********************
// ****************************************************************


procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterServiceClass( IUnitTestingEngine, TUnitTestingEngine)
end;

constructor TUnitTestingEngine.ServiceModeCreate;
begin
FCtx.Create;
FDeferredLoggerException := nil;
FSuite   := FSuiteFactory.MakeNode( '', nil, nil, nil) as ITestSuite;
FLoggers := TList2<ITestLogger>.Create;
FBreakOnFirstFail := False
end;


procedure TUnitTestingEngine.Abort;
begin
FAbortPending := True
end;

function TUnitTestingEngine.Can_Abort: boolean;
begin
result := (FRunInstanceCount > 0) and (not FAbortPending)
end;

function TUnitTestingEngine.Can_Run: boolean;
begin
result := FRunInstanceCount = 0
end;

destructor TUnitTestingEngine.Destroy;
begin
Shutdown;
FSuite   := nil;
FLoggers := nil;
FCtx.Free;
inherited
end;

procedure TUnitTestingEngine.EnterLeaveRunners( Delta: integer);
begin
if (FRunInstanceCount = 0) and (Delta > 0) and FAbortPending then
  FAbortPending := False;
Inc( FRunInstanceCount, Delta);
if (FRunInstanceCount <= 0) and FAbortPending then
  FAbortPending := False;
end;

procedure TUnitTestingEngine.ForEachLogger(
  Proc: TLoggerProc; doTrapExceptions: boolean);
var
  Logger: ITestLogger;
  doBreak: boolean;
begin
doBreak := False;
if assigned( Proc) then
  begin
  if doTrapExceptions then
      for Logger in FLoggers do
        try
          Proc( Logger, doBreak);
          if doBreak then break
        except on E: Exception do
          MarkLoggerException( Logger, E)
          end
    else
      for Logger in FLoggers do
        begin
        Proc( Logger, doBreak);
        if doBreak then break
        end
  end;
FlushLoggerExceptions
end;

function TUnitTestingEngine.GetBreakOnFirstFailure: boolean;
begin
result := FBreakOnFirstFail
end;

function TUnitTestingEngine.IsAborted: boolean;
var
  Msg: RMessage;
begin
if (not FAbortPending) and assigned( FController) and FController.CheckAbort then
  FAbortPending := True;
if FAbortPending and (not FAborted) then
  begin
  FAborted := True;
  SetFrameworkMessage( Msg, UserAbort, 'Abortion of run, at user request, is now pending.', lvRegular);
  ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
                 begin
                 Logger.OnAuxilaryMessage( nil, nil, Msg, nil)
                 end
                 , True)
  end;
result := FAborted
end;

function TUnitTestingEngine.IsRunning: boolean;
begin
result := FRunInstanceCount > 0
end;


procedure TUnitTestingEngine.OnAuxilaryMessage(
  const Node: ITestNode; const TestResult: ITestResult;
  const TestCase: ITestCase; const Msg: RMessage; Round: integer);
var
  Msg2: RMessage;
begin
Msg2 := Msg;
ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
               var Datum: IInterface;
               begin
               Datum := Node.ViewDatumOfLogger( Logger);
               Logger.OnAuxilaryMessage( TestResult, TestCase, Msg2, Datum)
               end
               , True)
end;


procedure TUnitTestingEngine.OnBeginRunInstance(
  const Instance: IRunInstance; Workload: integer);
begin
ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
               begin
               Logger.OnBeginRunInstance( Workload);
               Logger.OnProgressTick( Instance, 0, WorkLoad)
               end
               , True)
end;

procedure TUnitTestingEngine.OnEndRunInstance(
  const Instance: IRunInstance; const RunResult: ITestResult);
begin
ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
               begin
               Logger.OnEndRunInstance( RunResult)
               end
               , True)
end;

procedure TUnitTestingEngine.OnChangeStatus(
  NodeKind: TLoggerNode; const Fixture: ITestFixture;
  const TestProc: ITestProcedure; const TestCase: ITestCase;
  const TestResult: ITestResult);
var
  Node: ITestNode;
  Ok: boolean;
  Run: IRunInstance;
  RoundsCompleted, Workload: integer;
  GrossResult: ITestResult;
  sCompletedMsg, sPrefix: string;
  Msg: RMessage;
  Round: integer;

begin
case NodeKind of
  nFixture : Ok := Supports( Fixture , ITestNode, Node);
  nTestProc: Ok := Supports( TestProc, ITestNode, Node);
  nTestCase: Ok := Supports( TestCase, ITestNode, Node);
  else       Ok := False
  end;
case TestResult.Status of
  tsNeutral,
  tsSetup,
  tsTeardown,
  tsExecuting:  sCompletedMsg := '';
  tsPass     :  sCompletedMsg := 'completed with result PASS.';
  tsWarn     :  sCompletedMsg := 'completed with result WARN.';
  tsFail     :  sCompletedMsg := 'completed with result FAIL.';
  tsError    :  sCompletedMsg := 'completed with result ERROR.';
  tsSkipped  :  sCompletedMsg := 'completed with result SKIP.';
  end;
Run   := TestResult as IRunInstance;
Round := 1;
if sCompletedMsg <> '' then
  case TestResult.NodeKind of
    rnRoot         : sPrefix := '';
    rnNamespaceFixture, rnLeafFixture: sPrefix := 'Fixture';
    rnTestProc     : sPrefix := 'Test Procedure';
    rnTestCase     : sPrefix := 'Test Case';
    rnTestCaseRound: begin
                     Round     := Run.Parent.RoundsCompleted;
                     sPrefix   := Format( 'Round %d', [Round])
                     end;
    end;
if sPrefix <> '' then
  begin
  SetFrameworkMessage( Msg, ResultAnnouncement, sPrefix + ' [' + Run.Description + '] ' + sCompletedMsg, lvStatistic);
  OnAuxilaryMessage( Run.TestNode, TestResult, Run.TestCase, Msg, Round);
  end;
if (TestResult.NodeKind = rnTestCaseRound) and
   (TestResult.Status in [tsPass, tsWarn, tsFail, tsError]) then
  begin
  while assigned( Run) and assigned( Run.Parent) and (Run.NodeKind <> rnRoot) do
    Run := Run.Parent;
  if assigned( Run) then
    begin
    RoundsCompleted := Run.RoundsCompleted;
    Workload        := Run.WorkLoad;
    GrossResult     := Run as ITestResult;
    SetFrameworkMessage( Msg, ProgressAnnouncement, Format( 'Progress is %d of %d', [RoundsCompleted,Workload]), lvDebug);
    OnAuxilaryMessage( (TestResult as IRunInstance).TestNode, TestResult, TestResult.TestCase, Msg, Round);
    ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
                   begin
                   Logger.OnProgressTick( GrossResult, RoundsCompleted, WorkLoad)
                   end
                   , True)
        end
  end;
if Ok then
  ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
                 var Datum: IInterface;
                 begin
                 Datum := Node.ViewDatumOfLogger( Logger);
                 if assigned( Datum) then
                   Logger.OnChangeStatus( NodeKind, Fixture, TestProc, TestCase, TestResult, Datum)
                 end
                 , True);

if (TestResult.NodeKind = rnTestCaseRound) and (not FAbortPending) and
   (  ((TestResult.Status = tsFail) and FBreakOnFirstFail)  or
       (TestResult.Status = tsError))
     then
      begin
      FAbortPending := True;
      if TestResult.Status = tsFail then
        begin
        SetFrameworkMessage( Msg, AbortOnFirstFail, 'Aborting on first failure.', lvRegular);
        ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
                       begin
                       Logger.OnAuxilaryMessage( nil, nil, Msg, nil)
                       end
                       , True);
        end
      end;
ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
               begin
               Logger.Breathe
               end
               , True)
end;


procedure TUnitTestingEngine.RegisterFixtureClass( FixtureCls: TClass);
begin
(FSuite as ITestSuiteInternal).RegisterFixture( FCtx.GetType( FixtureCls));
end;

function TUnitTestingEngine.Run: ITestResult;
begin
FAbortPending := False;
FAborted      := False;
result := (FSuite as ITestSuiteInternal).Run( self);
FAbortPending := False
end;

procedure TUnitTestingEngine.SetBreakOnFirstFailure( Value: boolean);
begin
FBreakOnFirstFail := Value
end;

procedure TUnitTestingEngine.SetController(
  const Controller: IUnitTestingController);
begin
FController := Controller
end;

procedure TUnitTestingEngine.Shutdown;
var
  Shut: IShutDown;
begin
if FLoggers.Count > 0 then
  begin
  FlushLoggerExceptions;
  ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
                 var SelfAsEngine: IUnitTestingEngine;
                 begin
                 if RefCount <= 0 then
                     // If some how, RefCount is zero, it is no longer safe to
                     //  cast to an interface, as this would cause a double
                     //  destruction of the TUnitTestingEngine instance.
                     // I don't think it is possible to reach this point,
                     //  but just in case ...
                     SelfAsEngine := nil
                   else
                     SelfAsEngine := self as IUnitTestingEngine;
                 Logger.Shutdown( SelfAsEngine)
                 end
                 , False);
  FLoggers.Clear
  end;
FTroubleMaker := nil;
if Supports( FSuite, IShutDown, Shut) then
  Shut.Shutdown;
FSuite      := nil;
FController := nil;
FMemMonitor := nil
end;

procedure TUnitTestingEngine.StartUpLogger( const Logger: ITestLogger);
begin
Logger.OnAttachSuite( FSuite,
  procedure (const Addend: ITestFixture; const ViewDatum: IInterface)
    begin
    (Addend as ITestNode).AttachViewDatum( Logger, ViewDatum)
    end,
  procedure (const Addend: ITestProcedure; const ViewDatum: IInterface)
    begin
    (Addend as ITestNode).AttachViewDatum( Logger, ViewDatum)
    end,
  procedure (const Addend: ITestCase; const ViewDatum: IInterface)
    begin
    (Addend as ITestNode).AttachViewDatum( Logger, ViewDatum)
    end
  )
end;

procedure TUnitTestingEngine.StartUp;
begin
if FStarted then Exit;
FStarted := True;
ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
               begin
               StartUpLogger( Logger)
               end
               , False)
end;

procedure TUnitTestingEngine.Subscribe( const Observer: ITestLogger);
begin
if not assigned( Observer) then exit;
FLoggers.Add( Observer);
if FStarted then
  StartUpLogger( Observer)
end;

function TUnitTestingEngine.Suite: ITestSuite;
begin
result := FSuite
end;

procedure TUnitTestingEngine.Unsubscribe( const Observer: ITestLogger);

  procedure DetachFrom( Node: ITestNode);
  var
    Child: ITestNode;
  begin
  Node.AttachViewDatum( Observer, nil);
  for Child in Node.NodeChildren do
    DetachFrom( Child)
  end;

begin
FLoggers.Remove( Observer);
if FTroubleMaker = Observer then
  FTroubleMaker := nil;
DetachFrom( FSuite as ITestNode)
end;


procedure TUnitTestingEngine.MarkLoggerException( const TroubleMaker: ITestLogger; const LoggerException: Exception);
begin
if not assigned( FDeferredLoggerException) then
  // Some logger exceptions must be lost. If we don't draw the line some where,
  //  we could get a stack overflow of exceptions raised by faulty loggers
  //  reporting exceptions by other (or the same) faulty loggers, and so on.

  // The policy is that we just handle one level of exception and swallow the rest.
  // An exception raised by a logger method, will be broadcast as an error message
  //  of all other loggers.
  begin
  FDeferredLoggerException := LoggerException;
  FTroubleMaker            := TroubleMaker
  end
end;


procedure TUnitTestingEngine.FlushLoggerExceptions;
var
  Msg: RMessage;
  TroubleName: string;
  MinCount: integer;
begin
if not assigned( FDeferredLoggerException) then exit;
if assigned( FTroubleMaker) then
    MinCount := 2
  else
    MinCount := 1;
if FLoggers.Count >= MinCount then
  begin
  if assigned( FTroubleMaker) then
      try
        TroubleName := FTroubleMaker.MenuDescription;
        if TroubleName = '' then
          TroubleName := '(unnamed)'
      except on E: Exception do
        TroubleName := E.Message
      end
    else
      TroubleName := '(unsubscribed logger)';
  SetFrameworkMessage( Msg, LoggerException,
    Format( 'Logger "%s" reported exception %s: %s',
            [TroubleName, FDeferredLoggerException.ClassName, FDeferredLoggerException.Message]),
            lvError);
  ForEachLogger( procedure( const Logger: ITestLogger; var doBreak: boolean)
                 begin
                 if Logger <> FTroubleMaker then
                   Logger.OnAuxilaryMessage( nil, nil, Msg, nil)
                 end
                 , True)
  end;
FDeferredLoggerException := nil;
FTroubleMaker            := nil
end;











end.
