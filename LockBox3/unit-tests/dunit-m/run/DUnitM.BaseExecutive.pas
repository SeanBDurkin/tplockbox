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

unit DUnitM.BaseExecutive;
interface
uses DUnitM.UnitTestFramework, Classes, SBD.ServiceProvider;

type

IExecutive = interface
  ['{45B82A12-5F95-4515-BFE6-CE54799E4E2F}']
    procedure StartUp;
    procedure ShutDown;
    function  Model: IUnitTestingEngine;
    procedure DeclareMainForm( MainForm: TComponent);
    function  Services: IServiceProvider;
    procedure RegisterTestFixtures;
    function  ApplicationTitle: string;
  end;

TBaseExecutive = class( TInterfacedObject, IExecutive)
  private
    FMainForm: TComponent;

    procedure StartUp;
    procedure ShutDown;
    function  Model: IUnitTestingEngine;
    procedure DeclareMainForm( MainForm: TComponent);
    function  Services: IServiceProvider;

  protected
    FServices: IServiceProvider;
    FModel: IUnitTestingEngine;

    function  ApplicationTitle: string;                   virtual; abstract;
    procedure RegisterTestFixtures;                       virtual; abstract;
    procedure RegisterServices;                           virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation







uses DUnitM.UnitTestFramework.Solution, DUnitM.MemoryMonitor.Solution,
     DUnitM.Fixture, DUnitM.TestProcedure, DUnitM.NUnit.LoggerUI.Solution,
     SBD.Generics, DUnitM.NUnit.Logger.Solution,
     DUnitM.NUnit25.ResultsLog.Solution,
     DUnitM.XMLWriter.Solution;


constructor TBaseExecutive.Create;
begin
FServices := StandardServiceProvider;
RegisterServices;
FServices.Gn.Acquire<IUnitTestingEngine>( nil, FModel)
end;

procedure TBaseExecutive.DeclareMainForm( MainForm: TComponent);
begin
FMainForm := MainForm
end;

destructor TBaseExecutive.Destroy;
begin
ShutDown;
inherited
end;

function TBaseExecutive.Model: IUnitTestingEngine;
begin
result := FModel
end;


type
TAdditionalLoggers = class( TList2<ILoggerFactoryUI>, IAdditionalLoggers)
  end;

procedure TBaseExecutive.RegisterServices;
begin
DUnitM.UnitTestFramework.Solution.RegisterServices( FServices);
DUnitM.MemoryMonitor.Solution    .RegisterServices( FServices);
DUnitM.Fixture                   .RegisterServices( FServices);
DUnitM.TestProcedure             .RegisterServices( FServices);
DUnitM.NUnit25.ResultsLog.Solution .RegisterServices( FServices);
DUnitM. XMLWriter.Solution       .RegisterServices( FServices);

DUnitM.NUnit.LoggerUI.Solution.RegisterServices( FServices);
FServices.SetCooperativeAffinity( ILoggerFactoryUI, '', '',
  function( const Config: string): IInterface
  begin
  result := TAdditionalLoggers.Create as IAdditionalLoggers
  end,

  procedure( const Collection, Addend: IInterface)
  begin
  (Collection as IList2<ILoggerFactoryUI>).Add( Addend as ILoggerFactoryUI)
  end);

DUnitM.NUnit.Logger.Solution.RegisterServices( FServices);
// Register more loggers and thier factories here ...

// As required, acquire the collection of factories like so ...
//  if FServices.Acquire( ILoggerFactoryUI, nil, Gang, '', nil) then
//    for Member in Gang do
//      DoSomethingWith( Member)
end;

function TBaseExecutive.Services: IServiceProvider;
begin
result := FServices
end;

procedure TBaseExecutive.ShutDown;
begin
if assigned( FServices) then
  begin
  FServices.ShutDown;
  FServices := nil
  end;
if assigned( FModel) then
  begin
  FModel.Shutdown;
  FModel    := nil
  end
end;

procedure TBaseExecutive.StartUp;
begin
end;



end.
