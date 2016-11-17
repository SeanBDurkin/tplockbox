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

unit DUnitM.XMLWriter.Solution;
interface

// Service Files
// interface       [Configuration]  Affinity         Contract
// =============================================================================
// IXMLWriter      'UTF-8'          Competitive      Writes lexical XML to file.



uses Classes, DUnitM.XMLWriter, SBD.ServiceProvider;

type

TXMLWriter = class( TInterfacedObject, IXMLWriter)
  private
    function OpenDocument( OutStream1: TStream): IXMLWriter_Document;
  public
    [Configuration('UTF-8')] constructor ServiceModeCreate;
  end;


procedure RegisterServices( const Provider: IServiceProvider);


var
  sXLog: string;

implementation





uses Generics.Collections, SysUtils, SBD.Utils.XML2;

type
TNamespaceAxis = class
  private
    FInheritedDeclarations: TNamespaceAxis;
    FDefaultNamespace: string;
    FDeclaredNamespaces  : TDictionary<string,string>; // URL --> prefix
    FUndeclaredNamespaces: TDictionary<string,string>; // URL --> prefix
  public
    constructor Create( InheriteFrom: TNamespaceAxis);
    destructor Destroy; override;
    function  HasPrefix( const Prefix: string; var URI: string): boolean;
    function  HasNamespace( const URI: string; var Prefix: string): boolean;
    procedure DeclareNamespace( const PreferredPrefix, URI: string);
    procedure UnDeclareNamespace( const URI: string);
    procedure DeclareDefaultNamespace( const URI: string);
    procedure UnDeclareDefaultNamespace;
    function  Serialise: string;
  end;

IStringStream = interface
  ['{F13608A9-4846-44BB-88F3-C32A7B227CAC}']
    procedure Put( const Datum: string);
  end;

TNodeParent = class( TInterfacedObject, IXMLWriter_NodeParent)
  protected
    FCanvas: IStringStream;
    FParent: IXMLWriter_NodeParent;
    function  Close: IXMLWriter_NodeParent;                                   virtual; abstract;
    function  AddElement( const URI, LocalName: string): IXMLWriter_Element;  virtual; abstract;
    procedure AddProcessingInstruction( const Target, Content: string);       virtual;
    procedure AddComment( const Content: string);                             virtual;
    procedure AddTextNode( const Value: string);                              virtual;
    procedure Put( const Datum: string);                                      virtual;
    procedure PutPrefix( const Prefix: string);
    function  Canvas: IStringStream;
  public
    constructor Create( const Canvas1: IStringStream; Parent1: IXMLWriter_NodeParent);
  end;

TElement = class( TNodeParent, IXMLWriter_Element)
  private
    FNamespaces: TNamespaceAxis;
    FhasChildren: boolean;
    FPrefix: string;
    FURI: string;
    FLocalName: string;
    FLevel: integer;

  protected
    function  Close: IXMLWriter_NodeParent;                                   override;
    function  AddElement( const URI, LocalName: string): IXMLWriter_Element;  override;
    procedure AddProcessingInstruction( const Target, Content: string);       override;
    procedure AddComment( const Content: string);                             override;
    procedure AddTextNode( const Value: string);                              override;

  private
    procedure DeclareNamespace( const PreferredPrefix, URI: string);
    procedure UnDeclareNamespace( const URI: string);
    procedure DeclareDefaultNamespace( const URI: string);
    procedure UnDeclareDefaultNamespace;
    procedure AddAttribute( const Namespace, LocalName, Value: string);
    procedure SetChildNodes;
    procedure PutIndent;
    function  ElementStack: string;

  public
    constructor Create(
      const URI, LocalName: string;
      const Canvas1: IStringStream; const Parent1: IXMLWriter_NodeParent;
      Namespaces: TNamespaceAxis; Level1: integer);
    destructor Destroy; override;
  end;

TDocument = class( TNodeParent, IXMLWriter_Document, IStringStream)
  private
    FOutStream: TStream;
  protected
    procedure Put( const Datum: string);                      override;
    function  Close: IXMLWriter_NodeParent;                   override;
    function  AddElement( const URI, LocalName: string): IXMLWriter_Element;  override;
    procedure AddTextNode( const Value: string);              override;
  public
    constructor Create( OutStream1: TStream);
    destructor Destroy; override;
  end;


procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterServiceClass( IXMLWriter, TXMLWriter)
end;


function AttributeOutputEscape( const Plaintext: string): string;
begin
result := StringReplace( Plaintext, '&'   , '&Amp;' , [rfReplaceAll]);
result := StringReplace( result   , '<'   , '&lt;'  , [rfReplaceAll]);
result := StringReplace( result   , '>'   , '&gt;'  , [rfReplaceAll]);
result := StringReplace( result   , ''''  , '&apos;', [rfReplaceAll]);
result := StringReplace( result   , '"'   , '&quot;', [rfReplaceAll])
end;

function TextNodeOutputEscape( const Plaintext: string): string;
begin
result := StringReplace( Plaintext, '&'   , '&Amp;' , [rfReplaceAll]);
result := StringReplace( result   , '<'   , '&lt;'  , [rfReplaceAll]);
result := StringReplace( result   , ']]>' , ']]&gt;', [rfReplaceAll])
end;


function TXMLWriter.OpenDocument( OutStream1: TStream): IXMLWriter_Document;
begin
result := TDocument.Create( OutStream1)
end;

constructor TXMLWriter.ServiceModeCreate;
begin
end;

{ TNodeParent }

procedure TNodeParent.AddComment(
  const Content: string);
begin
Assert( not Contains( '-' + Content + '-', '--'), '');
Put( '<!--' + Content + '-->')
end;

procedure TNodeParent.AddProcessingInstruction( const Target, Content: string);
begin
Put( '<?' + Target + ' ' + Content + '?>')
end;

procedure TNodeParent.AddTextNode( const Value: string);
begin
Put( TextNodeOutputEscape( Value))
end;

function TNodeParent.Canvas: IStringStream;
begin
result := FCanvas;
if not assigned( Result) then
  Supports( Self, IStringStream, result)
end;

constructor TNodeParent.Create(
  const Canvas1: IStringStream; Parent1: IXMLWriter_NodeParent);
begin
FCanvas := Canvas1;
FParent := Parent1
end;

procedure TNodeParent.Put( const Datum: string);
var
  Canvas1: IStringStream;
begin
Canvas1 := Canvas;
if assigned( Canvas1) then
  Canvas1.Put( Datum)
end;

procedure TNodeParent.PutPrefix( const Prefix: string);
begin
if Prefix <> '' then
  Put( Prefix + ':')
end;

{ TElement }

procedure TElement.AddAttribute( const Namespace, LocalName, Value: string);
var
  Prefix: string;
begin
Assert( not FhasChildren, 'Cannot add attributes after child nodes.');
if not FNamespaces.HasNamespace( Namespace, Prefix) then
  begin
  FNamespaces.DeclareNamespace( 'ns0', Namespace);
  FNamespaces.HasNamespace( Namespace, Prefix)
  end;
PutPrefix( Prefix);
Put( LocalName + '="' + AttributeOutputEscape( Value) + '" ')
end;

procedure TElement.AddComment( const Content: string);
begin
SetChildNodes;
inherited AddComment( Content)
end;

function TElement.AddElement( const URI, LocalName: string): IXMLWriter_Element;
begin
SetChildNodes;
result := TElement.Create( URI, LocalName, Canvas, self, FNamespaces, FLevel)
end;

procedure TElement.AddProcessingInstruction( const Target, Content: string);
begin
SetChildNodes;
inherited AddProcessingInstruction( Target, Content)
end;

procedure TElement.SetChildNodes;
begin
if FhasChildren then exit;
FhasChildren := True;
Put( '>'#$0A)
end;

procedure TElement.AddTextNode( const Value: string);
begin
SetChildNodes;
inherited AddTextNode( Value)
end;

function TElement.Close: IXMLWriter_NodeParent;
begin
if FhasChildren then
    begin
    PutIndent;
    Put( '</');
    PutPrefix( FPrefix);
    Put( FLocalName)
    end
  else
    Put( '/');
 Put( '>'#$0A);
 result := FParent
end;

constructor TElement.Create(
  const URI, LocalName: string; const Canvas1: IStringStream;
  const Parent1: IXMLWriter_NodeParent; Namespaces: TNamespaceAxis;
  Level1: integer);
begin
FLevel := Level1 + 1;
inherited Create( Canvas1, Parent1);
FURI := URI;
FLocalName := LocalName;
FhasChildren := False;
FNamespaces := TNamespaceAxis.Create( Namespaces);
if FURI <> '' then
  FNamespaces.DeclareNamespace( 'ns0', FURI);
FNamespaces.HasNamespace( FURI, FPrefix);
PutIndent;
Canvas.Put( '<');
PutPrefix( FPrefix);
Put( FLocalName + ' ')
end;

procedure TElement.DeclareDefaultNamespace( const URI: string);
begin
FNamespaces.DeclareDefaultNamespace( URI)
end;

procedure TElement.DeclareNamespace( const PreferredPrefix, URI: string);
begin
FNamespaces.DeclareNamespace( PreferredPrefix, URI)
end;

destructor TElement.Destroy;
begin
FNamespaces.Free;
inherited
end;

function TElement.ElementStack: string;
var
  ParentElement: IXMLWriter_Element;
begin
if Supports( FParent, IXMLWriter_Element, ParentElement) then
    result := ParentElement.ElementStack
  else
    result := '';
result := result + '/' + FLocalName
end;

const IndentFactor = 2; // 2 spaces of indentation per level.

procedure TElement.PutIndent;
var
  sWhite: string;
  j: integer;
  Spaces: integer;
begin
Spaces := FLevel * IndentFactor;
if Spaces <= 0 then exit;
SetLength( sWhite, Spaces);
for j := 1 to Spaces do
  sWhite[j] := ' ';
Put( sWhite)
end;

procedure TElement.UnDeclareDefaultNamespace;
begin
FNamespaces.UnDeclareDefaultNamespace
end;

procedure TElement.UnDeclareNamespace( const URI: string);
begin
FNamespaces.UnDeclareNamespace( URI)
end;

{ TDocument }

function TDocument.AddElement( const URI, LocalName: string): IXMLWriter_Element;
begin
result := TElement.Create( URI, LocalName, Canvas, self, nil, -1)
end;

procedure TDocument.AddTextNode( const Value: string);
begin
if XMLTrim( Value) = '' then
    inherited AddTextNode( Value)
  else
    raise Exception.Create( 'Cannot add text child to a document, except white space.')
end;


function TDocument.Close: IXMLWriter_NodeParent;
begin
FreeAndNil( FOutStream);
result := nil
end;

constructor TDocument.Create( OutStream1: TStream);
begin
sXLog := '';
FOutStream := OutStream1;
inherited Create( nil, nil)
end;

destructor TDocument.Destroy;
begin
Close;
inherited
end;


procedure TDocument.Put( const Datum: string);
var
  AsUTF8: UTF8String;
  Len: integer;
begin
sXLog := sXLog + Datum;
AsUTF8 := UTF8Encode( StringReplace( Datum, #$0D#$0A, #$0A, [rfReplaceAll]));
Len    := Length( AsUTF8);
if Len > 0 then
  FOutStream.WriteBuffer( AsUTF8[1], Len)
end;

{ TNamespaceAxis }

constructor TNamespaceAxis.Create( InheriteFrom: TNamespaceAxis);
begin
FInheritedDeclarations := InheriteFrom;
FDeclaredNamespaces    := TDictionary<string,string>.Create;
if assigned( FInheritedDeclarations) then
    FDefaultNamespace  := FInheritedDeclarations.FDefaultNamespace
  else
    begin
    FDefaultNamespace  := '';
    FDeclaredNamespaces.Add( 'http://www.w3.org/XML/1998/namespace', 'xml');
    FDeclaredNamespaces.Add( 'http://www.w3.org/2000/xmlns/', 'xmlns')
    end;
FUndeclaredNamespaces := TDictionary<string,string>.Create
end;

destructor TNamespaceAxis.Destroy;
begin
FDeclaredNamespaces.Free;
FUndeclaredNamespaces.Free;
inherited
end;


procedure TNamespaceAxis.DeclareDefaultNamespace( const URI: string);
begin
FDefaultNamespace := URI
end;

procedure TNamespaceAxis.DeclareNamespace( const PreferredPrefix, URI: string);
var
  Prefix: string;
  Dummy: string;
  n: integer;
begin
if  HasNamespace( URI, Prefix) then exit;
if FUndeclaredNamespaces.ContainsKey( URI) then
    FUndeclaredNamespaces.Remove( URI)
  else
    begin
    Prefix := PreferredPrefix;
    n := -1;
    while HasPrefix( Prefix, Dummy) do
      begin
      Inc( n);
      Prefix := Format( 'ns%d', [n])
      end;
    FDeclaredNamespaces.Add( URI, Prefix)
    end
end;

function TNamespaceAxis.HasNamespace(
  const URI: string; var Prefix: string): boolean;
begin
result := URI = FDefaultNamespace;
if result then
  begin
  Prefix := '';
  exit;
  end;
result := FDeclaredNamespaces.ContainsKey( URI);
if result then
    Prefix := FDeclaredNamespaces[ URI]
  else
    result := assigned( FInheritedDeclarations) and
              FInheritedDeclarations.HasNamespace( URI, Prefix) and
              (not FUndeclaredNamespaces.ContainsKey( URI))
end;

function TNamespaceAxis.HasPrefix(
  const Prefix: string; var URI: string): boolean;
var
  Pair: TPair<string,string>;
begin
result := FDeclaredNamespaces.ContainsValue( Prefix);
if result then
    for Pair in FDeclaredNamespaces do
      begin
      if Pair.Value = Prefix then
        URI := Pair.Key
      end
  else
    result := assigned( FInheritedDeclarations) and
              FInheritedDeclarations.HasPrefix( Prefix, URI) and
              (not FUndeclaredNamespaces.ContainsValue( Prefix))
end;

function TNamespaceAxis.Serialise: string;
var
  InheritedDefault: string;
  Pair: TPair<string,string>;
begin
if assigned( FInheritedDeclarations) then
    InheritedDefault := FInheritedDeclarations.FDefaultNamespace
  else
    InheritedDefault := '';
if FDefaultNamespace <> InheritedDefault then
  result := 'xmlns="' + FDefaultNamespace + '" ';
for Pair in FDeclaredNamespaces do
  result := result + 'xmlns:' + Pair.Value + '="' + Pair.Key + '" ';
for Pair in FUndeclaredNamespaces do
  result := result + 'xmlns:' + Pair.Value + '="" ';
end;

procedure TNamespaceAxis.UnDeclareDefaultNamespace;
begin
FDefaultNamespace := ''
end;

procedure TNamespaceAxis.UnDeclareNamespace( const URI: string);
var
  Prefix: string;
begin
if URI = FDefaultNamespace then
  FDefaultNamespace := '';
if FDeclaredNamespaces.ContainsKey( URI) then
  FDeclaredNamespaces.Remove( URI);
if HasNamespace( URI, Prefix) then
  FUndeclaredNamespaces.Add( URI, Prefix)
end;

end.
