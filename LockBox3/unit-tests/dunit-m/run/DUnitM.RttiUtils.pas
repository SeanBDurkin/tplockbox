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

unit DUnitM.RttiUtils;
interface
uses Rtti, SBD.Generics, Sysutils;

type

TAttributes = class
  public
    class function Get<T: TCustomAttribute>( Subject: TRttiObject): ISEnumerable<T>;
    class function GetFirst<T: TCustomAttribute>( Subject: TRttiObject; out Attri: T): boolean;
    class function Has<T: TCustomAttribute>( Subject: TRttiObject): boolean;
  end;

{$REGION 'Internal only'}
type
TAttributesEnumerable<T: TCustomAttribute> = class ( TSEnumerable<T>)
  private
    FSubject: TRttiObject;
  protected
    function GetEnumerator: ISEnumerator<T>;         override;
  public
    constructor Create( Subject: TRttiObject);
  end;

type
TAttributesCursor<T: TCustomAttribute> = class( TSEnumerator<T>)
  private
    FAll: TArray<TCustomAttribute>;
    FIdx: integer;
  protected
    function MoveNext: boolean;                        override;
    function GetCurrent: T;                            override;
  public
    constructor Create( Enumerable1: TSEnumerable<T>);  override;
  end;
{$ENDREGION}


TCompileInformation = class
  public
    class function BuiltWithRuntimePackages: boolean;
    class function CompilerName: string;
  end;

function NextDeclaredMethod( BaseMethod: TRttiMethod; var Next: TRttiMethod): boolean;

implementation





class function TAttributes.Get<T>( Subject: TRttiObject): ISEnumerable<T>;
begin
result := TAttributesEnumerable<T>.Create( Subject)
end;

class function TAttributes.GetFirst<T>( Subject: TRttiObject; out Attri: T): boolean;
var
  Run: T;
begin
Attri := Default( T);
for Run in Get<T>( Subject) do
  begin
  Attri := Run;
  break
  end;
result := assigned( Attri)
end;


class function TAttributes.Has<T>( Subject: TRttiObject): boolean;
var
  Dummy: T;
begin
result := TAttributes.GetFirst<T>( Subject, Dummy)
end;

constructor TAttributesEnumerable<T>.Create( Subject: TRttiObject);
begin
FSubject := Subject
end;



function TAttributesEnumerable<T>.GetEnumerator: ISEnumerator<T>;
begin
result := TAttributesCursor<T>.Create( self);
end;

constructor TAttributesCursor<T>.Create( Enumerable1: TSEnumerable<T>);
begin
inherited Create( Enumerable1);
FAll := (Enumerable1 as TAttributesEnumerable<T>).FSubject.GetAttributes;
FIdx := -1
end;

function TAttributesCursor<T>.GetCurrent: T;
begin
result := T( FAll[ FIdx])
end;

function TAttributesCursor<T>.MoveNext: boolean;
begin
result := False;
while (not result) and (FIdx < Length( FAll)) do
  begin
  Inc( FIdx);
  result := (FIdx < Length( FAll)) and FAll[ FIdx].InheritsFrom( T)
  end
end;


class function TCompileInformation.BuiltWithRuntimePackages: boolean;
begin
result := FindClassHInstance( TObject) <> HInstance
end;

class function TCompileInformation.CompilerName: string;
begin
if CompilerVersion > 28 then
    result := 'post XE7'
  else if CompilerVersion = 28 then
    result := 'XE7'
  else if CompilerVersion = 27 then
    result := 'XE6'
  else if CompilerVersion = 26.5 then
    result := 'Appmethod 1.0'
  else if CompilerVersion = 26 then
    result := 'XE5'
  else if CompilerVersion = 25 then
    result := 'XE4'
  else if CompilerVersion = 24 then
    result := 'XE3'
  else if CompilerVersion = 23 then
    result := 'XE2'
  else if CompilerVersion = 22 then
    result := 'XE'
  else if CompilerVersion = 21 then
    result := 'D2010'
  else if CompilerVersion = 20 then
    result := 'D2009'
  else if CompilerVersion = 18.5 then
    result := 'D2007'
  else if CompilerVersion = 18 then
    result := 'D2006'
  else if CompilerVersion = 17 then
    result := 'D2005'
  else if CompilerVersion = 16 then
    result := 'D8'
  else if CompilerVersion = 15 then
    result := 'D7'
  else if CompilerVersion = 14 then
    result := 'D6'
end;


function NextDeclaredMethod( BaseMethod: TRttiMethod; var Next: TRttiMethod): boolean;
var
  Meth: TRttiMethod;
  Found: boolean;
begin
result := False;
Found  := False;
Next   := nil;
if assigned( BaseMethod) then
  for Meth in BaseMethod.Parent.GetDeclaredMethods do
    begin
    if (not Found) and (Meth <> BaseMethod) then continue;
    if Found then
        begin
        result := True;
        Next   := Meth;
        break
        end
      else
        Found := True
    end
end;


end.
