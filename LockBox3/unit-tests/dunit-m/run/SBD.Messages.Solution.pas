{***************************************************************************}
{                                                                           }
{           Copyright (C) 2014 Sean B. Durkin                               }
{                                                                           }
{           sean@seanbdurkin.id.au                                          }
{           http://seanbdurkin.id.au/sean                                   }
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

unit SBD.Messages.Solution;
interface
uses SBD.Generics, SBD.Messages, Generics.Collections, SBD.ServiceProvider;

type
TMessageList = class;
TMessageSource = class( TSEnumerable<RMessage>, IMessageSource)
  private
    FFilter: TMessageLevelSet;
    FList: TMessageList;
    FHold: IInterface;
  protected
    function GetEnumerator: ISEnumerator<RMessage>;   override;
    function EnumeratorClass: TClass;                 virtual;
  public
    constructor CreateAsSource( Filter: TMessageLevelSet; List1: TMessageList);
  end;

TMessageEnumerator = class( TSEnumerator<RMessage>)
  private
    FBaseCursor: ISEnumerator<RMessage>;
  protected
    function MoveNext: boolean;                        override;
    function GetCurrent: RMessage;                     override;
  public
    constructor CreateMesgEnumrtr( Source: TMessageSource);
  end;

TMessageList = class( TList2<RMessage>, IMessageList, IMessageSink)
  private
    function  AsSink: IMessageSink;
    function  AsSource( Filter: TMessageLevelSet): IMessageSource;
    function  Count( Filter: TMessageLevelSet): integer;
    function  Has( Filter: TMessageLevelSet): boolean;
    function  Clone: IMessageList;
    procedure Post( const Addend: RMessage);
    function  ListeningLevel: TMessageLevel;
  end;





procedure RegisterServices( const Provider: IServiceProvider);

implementation




uses SysUtils;

procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterFlyweightService( IMessageList,
  function( const Config: string; const ServiceProvider: IServiceProvider): IInterface
    begin
    Supports( TMessageList.Create, IMessageList, result)
    end)
end;


function TMessageList.AsSink: IMessageSink;
begin
result := self
end;

function TMessageList.AsSource( Filter: TMessageLevelSet): IMessageSource;
begin
result := TMessageSource.CreateAsSource( Filter, self)
end;

function TMessageList.Clone: IMessageList;
type TMessageListClass = class of TMessageList;
var
  Newbie: TMessageList;
  Msg: RMessage;
  Sink: IMessageSink;
begin
Newbie := TMessageListClass( ClassType).Create;
result := Newbie;
Sink   := result.AsSink;
for Msg in AsSource( AllMessages) do
  Sink.Post( Msg)
end;

function TMessageList.Count( Filter: TMessageLevelSet): integer;
var
  Msg: RMessage;
begin
if Filter = AllMessages then
    result := GetCount
  else
    begin
    result := 0;
    for Msg in AsSource( Filter) do
      Inc( result)
    end
end;


function TMessageList.Has( Filter: TMessageLevelSet): boolean;
var
  Msg: RMessage;
begin
if Filter = AllMessages then
    result := GetCount > 0
  else
    begin
    result := False;
    for Msg in AsSource( Filter) do
      begin
      result := True;
      break
      end
    end
end;

function TMessageList.ListeningLevel: TMessageLevel;
begin
if Has( [lvFatalError]) then
    result := lvFatalError
  else
    result := Low( TMessageLevel)
end;

procedure TMessageList.Post( const Addend: RMessage);
begin
Add( Addend)
end;


constructor TMessageSource.CreateAsSource(
  Filter: TMessageLevelSet; List1: TMessageList);
begin
FFilter := Filter;
FList   := List1;
FHold   := List1 as IInterface
end;

function TMessageSource.EnumeratorClass: TClass;
begin
result := TMessageEnumerator
end;

function TMessageSource.GetEnumerator: ISEnumerator<RMessage>;
type TMessageEnumeratorClass = class of TMessageEnumerator;
begin
result := TMessageEnumeratorClass( EnumeratorClass).CreateMesgEnumrtr( self)
end;



constructor TMessageEnumerator.CreateMesgEnumrtr( Source: TMessageSource);
begin
inherited Create( Source);
FBaseCursor := Source.FList.GetEnumerator
end;

function TMessageEnumerator.GetCurrent: RMessage;
begin
result := FBaseCursor.Current
end;

function TMessageEnumerator.MoveNext: boolean;
begin
repeat
  result := FBaseCursor.MoveNext
until (not result) or (FBaseCursor.Current.FLevel in TMessageSource( FEnumerable).FFilter)
end;

end.
