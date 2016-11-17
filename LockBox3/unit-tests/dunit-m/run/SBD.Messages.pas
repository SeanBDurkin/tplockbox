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

unit SBD.Messages;
interface
uses SBD.Generics;

type

RMessageURL = record
    /// <remarks>
    ///  A URL together with an error code represents a family of messages with
    ///  the same structure. The URL is restricted in the following ways:
    ///   (1) It must be a valid URL.
    ///   (2) It must be in canonical form
    ///   (3) Only http: or https: protocol is permitted.
    ///   (4) Absolute only. No relative URL permitted.
    ///   (5) No fragments are permitted.
    ///   (6) Non-empty. Nulls and empty strings are not permitted.
    /// </remarks>
    FURL: string;

    ///<remarks>
    ///  Prefix is a unique abbrieviation for the URL.
    ///  It may be either the preferred prefix or the actual prefix, depending
    ///   on context.
    ///</remarks>
    FPrefix: string;
  end;

type
TMessageLevel = (
    /// <remarks>Message for debugging purposes only.</remarks>
  lvDebug,
    /// <remarks>Low importance, verbose message or general information.</remarks>
  lvStatistic,
    /// <remarks>In use for compilers, parsers and interpreters.
    //   Suggests an improvement in efficiency,</remarks>
  lvHint,
    /// <remarks>Regular message, not fitting into any other category.</remarks>
  lvRegular,
    /// <remarks>A potential defect.</remarks>
  lvWarning,
    /// <remarks>A recoverable error.</remarks>
  lvError,
    /// <remarks>A non-recoverable error. All further processing will cease.</remarks>
  lvFatalError);
TMessageLevelSet = set of TMessageLevel;

const
  AllMessages: TMessageLevelSet = [Low( TMessageLevel) .. High( TMessageLevel)];
  Errors     : TMessageLevelSet = [lvError, lvFatalError];

type
RMessage = record
    FMessageURL: RMessageURL; // The prefix part of this is the preferred prefix.
    FMessageCode: integer;    // Restricted to be non-zero.
    FDisplayText: string;     // Trim and non-empty.
    FLevel: TMessageLevel;
    FStamp: TDateTime;
    /// <remarks>
    ///  Restricted to the following types:
    ///            vtInteger
    ///            vtBoolean
    ///            vtExtended
    ///            vtClass
    ///            vtCurrency
    ///            vtInt64
    ///            vtUnicodeString
    ///  Length( FData) restricted to a maximum of 9.
    /// </remarks>
    FData: array of TVarRec;
    procedure Clear;
  end;

IMessageSink = interface
  ['{B5B7C8EB-3E76-44BD-A192-370385F860DB}']
    procedure Post( const Addend: RMessage);
    function  ListeningLevel: TMessageLevel;
  end;

IMessageSource = interface( ISEnumerable<RMessage>)
  ['{5BE35232-0DEA-4F31-86B1-AD58F4AC1EED}']
    function GetEnumerator: ISEnumerator<RMessage>;
  end;

IMessageList = interface( IList2<RMessage>)
  ['{0868AF4B-A4A3-42A3-85F1-9877762718A4}']
    function  AsSink: IMessageSink;
    function  AsSource( Filter: TMessageLevelSet): IMessageSource;
    procedure Clear;
    function  Count( Filter: TMessageLevelSet): integer;
    function  Has( Filter: TMessageLevelSet): boolean;
    function  Clone: IMessageList;
  end;


function UniqueCloneMessage( const Source: RMessage): RMessage;
procedure UniqueCloneList( const Source: IMessageList; var Destination: IMessageList);
procedure CopyMessages( const Source: IMessageSource; const Destination: IMessageSink);

implementation









procedure RMessage.Clear;
begin
FMessageURL.FURL := '';
FMessageURL.FPrefix := '';
FMessageCode := 0;
FDisplayText := '';
FLevel := lvDebug;
FStamp := 0.0;
SetLength( FData, 0)
end;


function UniqueCloneMessage( const Source: RMessage): RMessage;
begin
result := Source;
UniqueString( result.FDisplayText);
UniqueString( result.FMessageURL.FURL);
UniqueString( result.FMessageURL.FPrefix)
end;

procedure UniqueCloneList( const Source: IMessageList; var Destination: IMessageList);
var
  Msg: RMessage;
begin
for Msg in Source.AsSource( AllMessages) do
  Destination.Add( UniqueCloneMessage( Msg))
end;

procedure CopyMessages( const Source: IMessageSource; const Destination: IMessageSink);
var
  Msg: RMessage;
begin
for Msg in Source do
  Destination.Post( Msg);
end;

end.
