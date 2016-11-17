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

unit DUnitM.StringUtils;
interface
uses SBD.Generics;

type
IStringEnumerable = interface( ISEnumerable<string>)
  ['{12EB292B-9B33-4088-B523-826BF289E44A}']
  end;

IStringCursor = interface( ISEnumerator<string>)
  ['{0C0D1CFD-FBC6-4376-A09C-D4083C2CDBBA}']
  end;

function Split( const Joined, Delimiter : string): IStringEnumerable;

{$ifdef MSWINDOWS}
function AbsPathToRelPath( const AbsPath, BasePath: string): string;
function PathCanonise( const Uncanonical: string): string;
{$endif}

implementation


{$ifdef MSWINDOWS}
uses Windows, ShLwApi, SysUtils;
{$endif}

type
TSplitter = class( TSEnumerable<string>, IStringEnumerable)
  private
    FJoined: string;
    FDelimiter: string;

  private type
    TParticleCursor = class ( TSEnumerator<string>)
      private
        FBuffer: string;
        FCrsrDelim: string;
        FDelimLen: integer;
        function DelimPos: integer;
      protected
        function MoveNext: boolean;                              override;
        function GetCurrent: string;                             override;
      public
        constructor Create( Enumerable1: TSEnumerable<string>);  override;
      end;

  protected
    function GetEnumerator: ISEnumerator<string>;                override;
  public
    constructor Create( const Joined, Delimiter : string);
  end;


function Split( const Joined, Delimiter : string): IStringEnumerable;
begin
result := TSplitter.Create( Joined, Delimiter)
end;


constructor TSplitter.Create( const Joined, Delimiter: string);
begin
FJoined    := Joined;
FDelimiter := Delimiter
end;

constructor TSplitter.TParticleCursor.Create( Enumerable1: TSEnumerable<string>);
begin
inherited Create( Enumerable1);
with Enumerable1 as TSplitter do
  begin
  FCrsrDelim := FDelimiter;
  FBuffer    := FCrsrDelim + FJoined
  end;
FDelimLen  := Length( FCrsrDelim)
end;

function TSplitter.TParticleCursor.DelimPos: integer;
begin
result := Pos( FCrsrDelim, FBuffer)
end;

function TSplitter.TParticleCursor.GetCurrent: string;
var
  P: integer;
begin
result := FBuffer;
P := DelimPos;
if P > 0 then
  SetLength( result, P - 1)
end;

function TSplitter.TParticleCursor.MoveNext: boolean;
var
  P: integer;
begin
P := DelimPos;
result := P > 0;
if not result then exit;
Delete( FBuffer, 1, P + FDelimLen - 1)
end;

function TSplitter.GetEnumerator: ISEnumerator<string>;
begin
result := TSplitter.TParticleCursor.Create( self)
end;


{$ifdef MSWINDOWS}
function AbsPathToRelPath( const AbsPath, BasePath: string): string;
// Copied from Andreas Rejbrand (http://stackoverflow.com/users/282848/andreas-rejbrand)
// http://stackoverflow.com/questions/5329472
// TODO: Find solution for non-Windows platforms.
var
  Path: array[0..MAX_PATH-1] of char;
begin
if AbsPath = BasePath then
    result := ''  // Same place. Use empty rather than '.' .
  else if AbsPath = '' then
    result := ''  // Path is unknown or not yet intialized.
  else if BasePath = '' then
    result := AbsPath // No base. So Absolute is the best we can get
  else if PathRelativePathTo( @Path[0], PChar( BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar( AbsPath), 0) then
    result := ExcludeTrailingPathDelimiter( Path)
  else
    result := AbsPath; // The computation bombed for some reason. So Absolute is the best we can get.
if Copy( result, 1, 2) = '.\' then
  System.Delete( result, 1, 2)
end;


function PathCanonicalize( lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathCanonicalizeW';

function PathCanonise( const Uncanonical: string): string;
var
  Path: array[0..MAX_PATH-1] of char;
begin
if PathCanonicalize( @Path[0], PChar( Uncanonical)) then
    begin
    result := Path;
    SetLength( result, StrLen( PChar( @Path[0])))
    end
  else
    result := Uncanonical
end;
{$endif}

end.
