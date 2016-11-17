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

unit DUnitM.NUnit25.ResultsLog.Solution;
interface
uses Classes, DUnitM.NUnit, DUnitM.XMLWriter, SBD.ServiceProvider;
type

TNUnit25_ResultsLog = class( TInterfacedObject, INUnit25_ResultsLog)
  private
    [Injection('UTF-8')] FXMLWriter: IXMLWriter;
{$REGION 'HelpInsight'}
/// <remarks>
///  OpenLog() clears the stream and writes like ...
///  <code>
///     &lt;test-results name="etc" &gt;&#x0A;
///       &lt;environment nunit-version="2.5.8.0" /&gt;&#x0A;
///       &lt;culture-info current-culture="en" current-uiculture="en" /&gt;&#x0A;
///        &lt;test-suite name="etc.exe" executed="true" result="Success" success="true" time="0.000" asserts="0" type="Assembly" &gt;&#x0A;
///          &lt;results
///  </code>
/// </remarks>
{$ENDREGION}
    function OpenLog( OutStream1: TStream;
      const ProgName: string;
      RunStamp: TDateTime;
      Total, Failed, Errored, NotRun, Ignored, Skipped: integer;
      RunResult: TNUnitResult;
      GrossDuration: double)
      : INUnit25_TestSuite; // units of days.
{$REGION 'HelpInsight'}
/// <remarks>
///  CloseLog() closes the stream and assumes that all open elements have been closed.
/// </remarks>
{$ENDREGION}
    procedure CloseLog;

  private
    FDoc: IXMLWriter_Document;

  public
    [Configuration] constructor ServiceModeCreate;
  end;


procedure RegisterServices( const Provider: IServiceProvider);

implementation













uses SysUtils, SBD.Utils.XML2;

procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterServiceClass( INUnit25_ResultsLog, TNUnit25_ResultsLog)
end;


type
TTestSuite = class abstract( TInterfacedObject, INUnit25_TestSuite)
  protected
    FParent: INUnit25_TestSuite;
    FElement: IXMLWriter_Element;
    FRoot:    IXMLWriter_Element;

    procedure AddAttribute( const Name, Value: string);            overload;
    procedure AddAttribute( const Name: string; Value: integer);   overload;
    procedure AddAttribute( const Name: string; Value: boolean);   overload;
    procedure AddAttribute( const Name: string; DurationValue: double);   overload;
    procedure AddAttribute( const Name: string; Value: TNUnitResult);   overload;
    procedure AddSuccessAttribute( Value: TNUnitResult);
    procedure AddOptionalAttribute( const Name, Value: string);
    procedure PushElement( const ElementName: string);
    procedure PopElement;
    procedure AddTextedElement( const ElementName, Content: string);
    procedure AddMessage( const Content: string);
    procedure CloseTestResults;
    function  ElementStack: string;

{$REGION 'HelpInsight'}
/// <remarks>
///  See the TGroup override. Not valid for TestCase
/// </remarks>
{$ENDREGION}
    function AddNamespaceSuite(
      const Name: string; Executed: boolean; NetResult: TNUnitResult;
      Time: double; Asserts: integer): INUnit25_TestSuite;          virtual; abstract;

{$REGION 'HelpInsight'}
/// <remarks>
///  See the TGroup override. Not valid for TestCase
/// </remarks>
{$ENDREGION}
    function AddFixtureSuite(
      const Name, Description: string; Executed: boolean; NetResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string): INUnit25_TestSuite;                    virtual; abstract;

    procedure AddTestCase(
      const Name, Description: string; Executed: boolean; CaseResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string);                                        virtual; abstract;

    function Close: INUnit25_TestSuite;                             virtual; abstract;

    procedure PushTestElement(
      const ElementName, Name, Description: string; Executed: boolean;
      RunResult: TNUnitResult; Duration: double; Asserts: integer);

  public
    constructor Create( const Parent1: INUnit25_TestSuite; const ParentElement1: IXMLWriter_Element);
  end;

type
TGroup = class ( TTestSuite)
  protected
{$REGION 'HelpInsight'}
/// <remarks>
///  AddNamespaceSuite() at the TGroup/TFixture level writes like ...
///  <code>
///    &lt;test-suite name="NewTests" executed="false" result="Success" success="true" time="0.000" asserts="0" type="Namespace" &gt;&#x0A;
///     &lt;results
///  </code>
///  where "NewTests" comes from the 'Name' parameter value. Notice that the 'type' attribute value is 'Namespace'.
/// </remarks>
{$ENDREGION}
    function AddNamespaceSuite(
      const Name: string; Executed: boolean; NetResult: TNUnitResult;
      Time: double; Asserts: integer): INUnit25_TestSuite;  override;

{$REGION 'HelpInsight'}
/// <remarks>
///  AddFixtureSuite() at the TGroup/TFixture level writes like ...
///  <code>
///    &lt;test-suite name="Sprint5" description="Banana banana banana" executed="false" result="Success" success="true" time="0.000" asserts="0" type="TestFixture" &gt;&#x0A;
///     &lt;results
///  </code>
///  where "NewTests" comes from the 'Name' parameter value. Notice that the 'type' attribute value is 'TestFixture'.
///  Also optionally, a &lt;reason/&gt; element may be inserted beneath &lt;test-suite/&gt;, at the same level as &lt;results/&gt;.
/// </remarks>
{$ENDREGION}
    function AddFixtureSuite(
      const Name, Description: string; Executed: boolean; NetResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string): INUnit25_TestSuite;                             override;

    procedure AddTestCase(
      const Name, Description: string; Executed: boolean; CaseResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string);                                                 override;

{$REGION 'HelpInsight'}
/// <remarks>
///  Close() closes the TestSuite by writing ...
///  <code>
///     &lt;/results&gt;&#x0A;
///    &lt;/test-suite&gt;
///  </code>
///  It is assumed that the &lt;/test-suite&gt; and &lt;/results&gt; are open upon function entry.
/// </remarks>
{$ENDREGION}
    function Close: INUnit25_TestSuite;                                      override;

  public
    constructor CreateGeneric( const Parent1: INUnit25_TestSuite;
     const ParentElement: IXMLWriter_Element;
     const TestSuiteType, Name, Description: string; Executed: boolean;
     RunResult: TNUnitResult; GrossDuration: Double; Asserts: integer;
     const Reason: string);

{$REGION 'HelpInsight'}
/// <remarks>
///  CreateAssembly() writes like ...
///  <code>
///    &lt;test-suite name="etc.exe" executed="true" result="Success" success="true" time="0.000" asserts="0" type="Assembly" &gt;&#x0A;
///      &lt;results
///  </code>
/// </remarks>
{$ENDREGION}
    constructor CreateAssembly(
     const ParentElement: IXMLWriter_Element;
     const ProgName: string; RunResult: TNUnitResult; GrossDuration: Double;
     const Root1: IXMLWriter_Element);

    constructor CreateNamespace( const Parent1: INUnit25_TestSuite;
     const ParentElement: IXMLWriter_Element;
     const Name: string; Executed: boolean;
     RunResult: TNUnitResult; GrossDuration: Double; Asserts: integer);
  end;

TFixture = class ( TGroup)
  protected
{$REGION 'HelpInsight'}
/// <remarks>
///  AddTestCase() writes like a &lt;test-case/&gt; element...
///  <code>
///    &lt;test-case name="TestOne" description="Banana banana" executed="true" result="Success" success="true" time="0.000" asserts="0" /&gt;&#x0A;
///  </code>
///  Optional sub-elements may include either &lt;failure/&gt;/&lt;stack-trace/&gt; or reason &lt;elements/&gt;.
/// </remarks>
{$ENDREGION}
    procedure AddTestCase(
      const Name, Description: string; Executed: boolean; CaseResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string);                                                 override;

  public
    constructor CreateFixture( const Parent1: INUnit25_TestSuite;
     const ParentElement: IXMLWriter_Element;
     const Name, Description: string; Executed: boolean;
     RunResult: TNUnitResult; GrossDuration: Double; Asserts: integer;
     const Reason: string);
  end;

TTestCase = class ( TTestSuite)
  protected
    function AddNamespaceSuite(
      const Name: string; Executed: boolean; NetResult: TNUnitResult;
      Time: double; Asserts: integer): INUnit25_TestSuite;  override;

    function AddFixtureSuite(
      const Name, Description: string; Executed: boolean; NetResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string): INUnit25_TestSuite;                             override;

    procedure AddTestCase(
      const Name, Description: string; Executed: boolean; CaseResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string);                                                 override;

    function Close: INUnit25_TestSuite;                                      override;

  public
    constructor Create( const Parent1: INUnit25_TestSuite;
     const ParentElement: IXMLWriter_Element;
     const Name, Description: string; Executed: boolean;
     RunResult: TNUnitResult; GrossDuration: Double; Asserts: integer);
  end;


function EncodeResult( NUnitResult: TNUnitResult): string;
const
  ResultStrings: array[ TNUnitResult] of string = (
    'Failure', 'Success', 'Ignored', 'Error', 'Inconclusive', 'NotRunnable');
begin
result := ResultStrings[ NUnitResult]
end;

function EncodeDuration( Duration: double): string;
begin
result := Format( '%.3f', [Duration])
end;


constructor TNUnit25_ResultsLog.ServiceModeCreate;
begin
end;

procedure TNUnit25_ResultsLog.CloseLog;
begin
FDoc.Close // Closes the stream
end;

function TNUnit25_ResultsLog.OpenLog(
      OutStream1: TStream;
      const ProgName: string;
      RunStamp: TDateTime;
      Total, Failed, Errored, NotRun, Ignored, Skipped: integer;
      RunResult: TNUnitResult;
      GrossDuration: double): INUnit25_TestSuite;
var
  El: IXMLWriter_Element;
  ErrorCount: integer;
  FailureCount: integer;
  Root: IXMLWriter_Element;

  procedure AddAttribute( const Name, Value: string); overload;
  begin
  El.AddAttribute( '', Name, Value)
  end;

  procedure AddAttribute( const Name: string; Value: integer); overload;
  begin
  AddAttribute( Name, Txs_integer.Encode( Value))
  end;

begin
FDoc := FXMLWriter.OpenDocument( OutStream1);
El := FDoc.AddElement( '', 'test-results');
Root := El;
AddAttribute( 'name', ProgName);
AddAttribute( 'total', Total);
ErrorCount   := 0;
FailureCount := 0;
case RunResult of
  rFailure: FailureCount := 1;
  rError  : ErrorCount   := 1;
  end;
AddAttribute( 'errors', ErrorCount);
AddAttribute( 'failures', FailureCount);
AddAttribute( 'not-run', NotRun);
AddAttribute( 'inconclusive', 0);
AddAttribute( 'ignored', Ignored);
AddAttribute( 'skipped', Skipped);
AddAttribute( 'invalid', 0);
AddAttribute( 'date', Txs_dateTime.EncodeDate( Trunc( RunStamp)));
AddAttribute( 'time', Txs_dateTime.EncodeTime( Frac( RunStamp), False, 0));
El := El.AddElement( '', 'environment');
AddAttribute( 'nunit-version','2.5.8.0');
AddAttribute( 'clr-version','2.0.50727.1433');
AddAttribute( 'os-version','(to be developed)');
AddAttribute( 'platform','(to be developed)');
AddAttribute( 'cwd','(to be developed)');
AddAttribute( 'machine-name','(to be developed)');
AddAttribute( 'user','(to be developed)');
AddAttribute( 'user-domain','(to be developed)');
El := El.Close as IXMLWriter_Element;
El := El.AddElement( '', 'culture-info');
AddAttribute( 'current-culture','en');
AddAttribute( 'current-uiculture','en');
El := El.Close as IXMLWriter_Element;
result := TGroup.CreateAssembly( El, ProgName, RunResult, GrossDuration, Root)
end;


procedure TTestSuite.AddAttribute( const Name, Value: string);
begin
FElement.AddAttribute( '', Name, Value)
end;

procedure TTestSuite.AddAttribute( const Name: string; Value: boolean);
begin
AddAttribute( Name, Txs_boolean.Encode( Value))
end;

procedure TTestSuite.AddAttribute( const Name: string; Value: integer);
begin
AddAttribute( Name, Txs_integer.Encode( Value))
end;

procedure TTestSuite.AddAttribute( const Name: string; Value: TNUnitResult);
begin
AddAttribute( Name, EncodeResult( Value))
end;

procedure TTestSuite.AddTextedElement( const ElementName, Content: string);
begin
PushElement( ElementName);
FElement.AddTextNode( Content);
PopElement
end;


procedure TTestSuite.AddMessage( const Content: string);
begin
AddTextedElement( 'message', Content)
end;

procedure TTestSuite.AddAttribute( const Name: string; DurationValue: double);
begin
AddAttribute( Name, EncodeDuration( DurationValue))
end;

procedure TTestSuite.AddOptionalAttribute( const Name, Value: string);
begin
if Value <> '' then
  AddAttribute( Name, Value)
end;

procedure TTestSuite.AddSuccessAttribute( Value: TNUnitResult);
begin
AddAttribute( 'success', Value = rSuccess)
end;

procedure TTestSuite.CloseTestResults;
begin
if assigned( FRoot) then
  FRoot.Close     // closes the test-results element
end;

constructor TTestSuite.Create(
  const Parent1: INUnit25_TestSuite; const ParentElement1: IXMLWriter_Element);
begin
FParent  := Parent1;
FElement := ParentElement1
end;

function TTestSuite.ElementStack: string;
begin
if assigned( FElement) then
    result := FElement.ElementStack
  else
    result := ''
end;

procedure TTestSuite.PushElement( const ElementName: string);
begin
FElement := FElement.AddElement( '', ElementName)
end;

procedure TTestSuite.PushTestElement(
  const ElementName, Name, Description: string;
  Executed: boolean; RunResult: TNUnitResult; Duration: double; Asserts: integer);
begin
PushElement( ElementName);
AddAttribute( 'name', Name);
AddOptionalAttribute( 'description', Description); // Only valid for 'TestFixture'
AddAttribute( 'executed', Executed);
AddAttribute( 'result', RunResult);
AddSuccessAttribute( RunResult);
AddAttribute( 'time', Duration);
AddAttribute( 'asserts', Asserts)
end;



procedure TTestSuite.PopElement;
begin
if not Supports( FElement.Close, IXMLWriter_Element, FElement) then
  FElement := nil
end;

{ TGroup }

constructor TGroup.CreateAssembly(
  const ParentElement: IXMLWriter_Element;
  const ProgName: string; RunResult: TNUnitResult;
  GrossDuration: Double; const Root1: IXMLWriter_Element);
begin
CreateGeneric( nil, ParentElement, 'Assembly', ProgName, '', True, RunResult, GrossDuration, 0, '');
FRoot := Root1
end;

constructor TGroup.CreateGeneric(
  const Parent1: INUnit25_TestSuite;
  const ParentElement: IXMLWriter_Element;
  const TestSuiteType, Name, Description: string; Executed: boolean;
  RunResult: TNUnitResult; GrossDuration: Double; Asserts: integer;
  const Reason: string);
begin
inherited Create( Parent1, ParentElement);
PushTestElement( 'test-suite', Name, Description, Executed, RunResult, GrossDuration, Asserts);
AddAttribute( 'type', TestSuiteType); // Either 'Assembly', 'Namespace' or 'TestFixture'
if Reason <> '' then  // Only valid for 'TestFixture'
  begin
  PushElement( 'reason');
  AddMessage( Reason);
  PopElement
  end;
PushElement( 'results')
end;


constructor TGroup.CreateNamespace(
  const Parent1: INUnit25_TestSuite;
  const ParentElement: IXMLWriter_Element; const Name: string;
  Executed: boolean; RunResult: TNUnitResult; GrossDuration: Double;
  Asserts: integer);
begin
CreateGeneric( Parent1, ParentElement, 'Namespace', Name, '', Executed,
               RunResult, GrossDuration, Asserts, '')
end;

function TGroup.AddNamespaceSuite( const Name: string; Executed: boolean;
  NetResult: TNUnitResult; Time: double;
  Asserts: integer): INUnit25_TestSuite;
begin
result := TGroup.CreateNamespace( self, FElement, Name, Executed, NetResult, Time, Asserts)
end;

function TGroup.AddFixtureSuite( const Name, Description: string; Executed: boolean;
  NetResult: TNUnitResult; Time: double; Asserts: integer;
  const Reason: string): INUnit25_TestSuite;
begin
result := TFixture.CreateFixture( self, FElement, Name, Description, Executed, NetResult, Time, Asserts, Reason)
end;

procedure TGroup.AddTestCase( const Name, Description: string; Executed: boolean;
  CaseResult: TNUnitResult; Time: double; Asserts: integer;
  const Reason: string);
begin
raise Exception.Create( 'Test cases cannot be direct children of Namespace groups.')
end;

function TGroup.Close: INUnit25_TestSuite;
begin
(FElement.Close as IXMLWriter_Element) // closes <results>
// Now we are at <test-suite>
.Close; // closes <test-suite>
// Now we are at original parent
result := FParent
end;



{ TFixture }

constructor TFixture.CreateFixture( const Parent1: INUnit25_TestSuite;
  const ParentElement: IXMLWriter_Element;
  const Name, Description: string; Executed: boolean; RunResult: TNUnitResult;
  GrossDuration: Double; Asserts: integer; const Reason: string);
begin
inherited CreateGeneric(
  Parent1, ParentElement, 'TestFixture', Name, Description, Executed,
  RunResult, GrossDuration, Asserts, Reason)
end;




procedure TFixture.AddTestCase(
  const Name, Description: string; Executed: boolean; CaseResult: TNUnitResult;
  Time: double; Asserts: integer; const Reason: string);
var
  TestCase: INUnit25_TestSuite;
begin
TestCase := TTestCase.Create( self, FElement, Name, Description, Executed, CaseResult, Time, Asserts);
if Reason <> '' then
  begin
  if CaseResult = rFailure then
      begin
      PushElement( 'failure');
      AddMessage( Reason);
      AddTextedElement( 'stack-trace', 'Not implemented')
      end
    else
      begin
      PushElement( 'reason');
      AddMessage( Reason)
      end;
  PopElement // reason/failure
  end;
TestCase.Close
end;




{ TTestCase }

constructor TTestCase.Create( const Parent1: INUnit25_TestSuite;
  const ParentElement: IXMLWriter_Element;
  const Name, Description: string; Executed: boolean; RunResult: TNUnitResult;
  GrossDuration: double; Asserts: integer);
begin
// <test-case name="NUnit.Tests.Assemblies.MockTestFixture.FailingTest"
//  executed="True" result="Failure" success="False" time="0.013" asserts="0">
inherited Create( Parent1, ParentElement);
PushTestElement( 'test-case', Name, Description, Executed, RunResult, GrossDuration, Asserts)
end;

function TTestCase.Close: INUnit25_TestSuite;
begin
PopElement; // closes <test-case>
// Now we are at original parent
result := FParent
end;

function TTestCase.AddFixtureSuite(const Name, Description: string; Executed: boolean;
  NetResult: TNUnitResult; Time: double; Asserts: integer;
  const Reason: string): INUnit25_TestSuite;
begin
raise Exception.Create( 'Test cases cannot have children.')
end;

function TTestCase.AddNamespaceSuite(const Name: string; Executed: boolean;
  NetResult: TNUnitResult; Time: double;
  Asserts: integer): INUnit25_TestSuite;
begin
raise Exception.Create( 'Test cases cannot have children.')
end;

procedure TTestCase.AddTestCase(const Name, Description: string; Executed: boolean;
  CaseResult: TNUnitResult; Time: double; Asserts: integer;
  const Reason: string);
begin
raise Exception.Create( 'Test cases cannot have children.')
end;



end.
