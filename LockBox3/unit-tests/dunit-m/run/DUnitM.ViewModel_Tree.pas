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

unit DUnitM.ViewModel_Tree;
interface
uses Classes, Controls, DUnitM.UnitTestFramework;

type

TVisualTestSuiteNodeKind = (nkNeutral, nkTestCase, nkTestProc, nkTestFixture, nkGroup);

IVisualTestSuiteNode = interface
  ['{660A0B5D-7DCC-4C62-B3A2-54CD873DCDC5}']
    {$REGION 'property accesssors'}
    function  GetDatum: pointer;
    procedure SetDatum( Value: pointer);
    {$ENDREGION}
    property Datum: pointer    read GetDatum write SetDatum;
  end;

TInserPosition = (iBefore, iAfter);

TSetCheckedSource = (csPopulation, csPostPopulate, csUser);
INodeRenderer = interface
  ['{1C46A49A-44C3-4505-B7A7-D46D7B1AFC51}']
    procedure Attached( const Node: IVisualTestSuiteNode);
    procedure Detach;
    function  GetState: TTestStatus;
    function  GetKind: TVisualTestSuiteNodeKind;
    function  GetDisplayName: string;
    function  Hint: string;
    function  GetFullCycleCount: integer;
    function  GetDoneCycleCount: integer;
    procedure SetChecked( Value: boolean; Source: TSetCheckedSource);
  end;

IVisualTestSuiteTreeChangeContext = interface
  ['{B81C4E46-E097-4FB3-9D7F-5DCD526AE8D3}']
    function  ChildNodes: IEnumerable<IVisualTestSuiteNode>;
    function  Insert( const Sibling: IVisualTestSuiteNode; Position: TInserPosition; AddCount: integer): IVisualTestSuiteNode;
    function  Append( AddCount: integer): IEnumerable<IVisualTestSuiteNode>;
    procedure AttachRenderer( const Newbie: IVisualTestSuiteNode; const Renderer: INodeRenderer);
    procedure Delete( const Victim: IVisualTestSuiteNode);
  end;

IVisualTestSuiteTree = interface
  ['{92018040-E850-4275-8357-0E200E223A2E}']
  // When constructed by a ServiceProvider, IComponentContext may be injected as a data member.
    function  FactoryDisplayName: string;
    function  Nodes( const Parent: IVisualTestSuiteNode): IEnumerable<IVisualTestSuiteNode>;
    function  AllNodes: IEnumerable<IVisualTestSuiteNode>;
    function  Change( const Parent: IVisualTestSuiteNode): IVisualTestSuiteTreeChangeContext;
    procedure InvalidateRendering( const DeltaNode: IVisualTestSuiteNode);
    procedure BeforePopulate;
    procedure AfterPopulate;
    procedure SetChecked( const Node: IVisualTestSuiteNode; Value: boolean; Source: TSetCheckedSource);
  end;

IVisualTestSuiteTreeFactory = interface
  ['{991B77D7-E057-47FB-8B9B-0A2253AB1168}']
    function MakeVisualTestSuiteTree( AOwner: TComponent; AParent: TWinControl; const AName: string): IVisualTestSuiteTree;
  end;

implementation

end.
