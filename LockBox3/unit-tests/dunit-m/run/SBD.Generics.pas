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

unit SBD.Generics;
interface

uses Generics.Collections, Generics.Defaults;
type

ISEnumerator<T> = interface
  ['{7DF98A2B-53EF-4D8D-AC14-ED9AC7A56C1B}']
    function MoveNext: boolean;
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

ISEnumerable<T> = interface
  ['{42397EFC-5A3B-4873-8F09-7F2DD9AF2095}']
    function GetEnumerator: ISEnumerator<T>;
  end;

type
TSEnumerable<T> = class abstract( TInterfacedObject, ISEnumerable<T>)
  protected
    function GetEnumerator: ISEnumerator<T>;  virtual; abstract;
  end;

TSEnumerator<T> = class abstract( TInterfacedObject, ISEnumerator<T>)
  protected
    FEnumerable: TSEnumerable<T>;
    FEnumHold  : IInterface;
    function MoveNext: boolean;                        virtual; abstract;
    function GetCurrent: T;                            virtual; abstract;
  public
    constructor Create( Enumerable1: TSEnumerable<T>);  virtual;
  end;

IListWrap<T> = interface
  ['{A3BE9895-B1B5-4F16-8C61-FD1DF85D8B7A}']
    function GetList: TList<T>;
    property List: TList<T>  read GetList;
  end;

TListWrap<T> = class( TInterfacedObject, IListWrap<T>)
  protected
    FList: TList<T>;
    function GetList: TList<T>;
  public
    constructor Create;                                      overload;
    constructor Create( const AComparer: IComparer<T>);      overload;
    constructor Create( Collection: TEnumerable<T>);         overload;
    constructor Create( const Collection: ISEnumerable<T>);  overload;
    destructor  Destroy; override;
    property List: TList<T> read GetList;
  end;

IList2<T> = interface( ISEnumerable<T>)
  ['{E2FD2C1F-E9B1-4D21-AD21-9C8CA9F2B977}']
    {$REGION 'property accessors'}
    function  GetCapacity: integer;
    procedure SetCapacity( Value: integer);
    function  GetCount: integer;
    procedure SetCount( Value: integer);
    function  GetItem( Index: integer): T;
    procedure SetItem( Index: integer; const Value: T);
    function  GetOnNotify: TCollectionNotifyEvent<T>;
    procedure SetOnNotify( Value: TCollectionNotifyEvent<T>);
    {$ENDREGION}

    function  Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(Collection: TEnumerable<T>); overload;
    procedure AddRange(const Collection: ISEnumerable<T>); overload;
    procedure Insert(Index: Integer; const Value: T);
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: ISEnumerable<T>); overload;
    function  Remove(const Value: T): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function  Extract(const Value: T): T;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function  First: T;
    function  Last: T;
    procedure Clear;
    function  Contains(const Value: T): Boolean;
    function  IndexOf(const Value: T): Integer;
    function  LastIndexOf(const Value: T): Integer;
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function  BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function  BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;
    procedure TrimExcess;
    property  Capacity: Integer read GetCapacity write SetCapacity;
    property  Count: Integer read GetCount write SetCount;
    property  Items[Index: Integer]: T read GetItem write SetItem; default;
    property  OnNotify: TCollectionNotifyEvent<T> read GetOnNotify write SetOnNotify;
  end;

TListCursor<T> = class( TSEnumerator<T>)
  protected
    FIdx: integer;
    function MoveNext: boolean;                        override;
    function GetCurrent: T;                            override;
    function ListObj: TList<T>;
  public
    constructor Create( Enumerable1: TSEnumerable<T>); override;
  end;


TList2<T> = class( TSEnumerable<T>, IList2<T>)
  protected
    FListObj: TList<T>;

    function  GetEnumerator: ISEnumerator<T>;  override;
    function  GetCapacity: integer;
    procedure SetCapacity( Value: integer);
    function  GetCount: integer;
    procedure SetCount( Value: integer);
    function  GetItem( Index: integer): T;
    procedure SetItem( Index: integer; const Value: T);
    function  GetOnNotify: TCollectionNotifyEvent<T>;
    procedure SetOnNotify( Value: TCollectionNotifyEvent<T>);
    function  Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(const Collection: ISEnumerable<T>); overload;
    procedure AddRange(Collection: TEnumerable<T>); overload;
    procedure Insert(Index: Integer; const Value: T);
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: ISEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;
    function  Remove(const Value: T): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function  Extract(const Value: T): T;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function  First: T;
    function  Last: T;
    procedure Clear;
    function  Contains(const Value: T): Boolean;
    function  IndexOf(const Value: T): Integer;
    function  LastIndexOf(const Value: T): Integer;
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function  BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function  BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;
    procedure TrimExcess;

  public
    constructor Create; overload;
    constructor Create( const AComparer: IComparer<T>); overload;
    constructor Create( Collection: TEnumerable<T>); overload;
    constructor Create( const Collection: ISEnumerable<T>); overload;
    destructor  Destroy; override;

    property  Capacity: Integer read GetCapacity write SetCapacity;
    property  Count: Integer read GetCount write SetCount;
    property  Items[Index: Integer]: T read GetItem write SetItem; default;
    property  OnNotify: TCollectionNotifyEvent<T> read GetOnNotify write SetOnNotify;
  end;

TEmptyCursor<T> = class( TSEnumerator<T>)
  protected
    function MoveNext: boolean;                        override;
    function GetCurrent: T;                            override;
  end;

implementation



//function TSEnumerable<T>.GetEnumerator: IEnumerator<T>;
////  This does not work for D2010 due to a compiler bug. So must override for D2010.
//var
//  Cls: TClass;
//begin
//Cls := EnumeratorClass;
//if Cls.InheritsFrom( TSEnumerator<T>) then
//    result := TSEnumerator<T>( Cls).Create( self)
//    // ^ This is the part that breaks in D2010. It seems the compiler
//    //    can't find the correct location for the class VMT.
//  else
//    result := nil
//end;


constructor TSEnumerator<T>.Create( Enumerable1: TSEnumerable<T>);
begin
FEnumerable := Enumerable1;
FEnumHold   := FEnumerable as IInterface
end;


constructor TListWrap<T>.Create;
begin
FList := TList<T>.Create
end;

constructor TListWrap<T>.Create( const AComparer: IComparer<T>);
begin
FList := TList<T>.Create( AComparer)
end;

constructor TListWrap<T>.Create( Collection: TEnumerable<T>);
begin
FList := TList<T>.Create( Collection)
end;

constructor TListWrap<T>.Create( const Collection: ISEnumerable<T>);
var
  Addend: T;
begin
Create;
for Addend in Collection do
  FList.Add( Addend)
end;


destructor TListWrap<T>.Destroy;
begin
FList.Free;
inherited
end;

function TListWrap<T>.GetList: TList<T>;
begin
result := FList
end;



function TEmptyCursor<T>.GetCurrent: T;
begin
result := default( T)
end;

function TEmptyCursor<T>.MoveNext: boolean;
begin
result := False
end;


function TList2<T>.Add( const Value: T): Integer;
begin
result := FListObj.Add( Value)
end;

procedure TList2<T>.AddRange( const Collection: IEnumerable<T>);
begin
FListObj.AddRange( Collection)
end;

procedure TList2<T>.AddRange( Collection: TEnumerable<T>);
begin
FListObj.AddRange( Collection)
end;

procedure TList2<T>.AddRange(const Values: array of T);
begin
FListObj.AddRange( Values)
end;

procedure TList2<T>.AddRange( const Collection: ISEnumerable<T>);
begin
InsertRange( FListObj.Count, Collection)
end;

function TList2<T>.BinarySearch(const Item: T; out Index: Integer;
  const AComparer: IComparer<T>): Boolean;
begin
result := FListObj.BinarySearch( Item, Index, AComparer)
end;

function TList2<T>.BinarySearch(const Item: T; out Index: Integer): Boolean;
begin
result := FListObj.BinarySearch( Item, Index)
end;

procedure TList2<T>.Clear;
begin
FListObj.Clear
end;

function TList2<T>.Contains(const Value: T): Boolean;
begin
result := FListObj.Contains( Value)
end;

constructor TList2<T>.Create;
begin
FListObj := TList<T>.Create
end;

constructor TList2<T>.Create( const AComparer: IComparer<T>);
begin
FListObj := TList<T>.Create( AComparer)
end;

constructor TList2<T>.Create( Collection: TEnumerable<T>);
begin
FListObj := TList<T>.Create( Collection)
end;

constructor TList2<T>.Create( const Collection: ISEnumerable<T>);
begin
Create;
InsertRange( 0, Collection)
end;

procedure TList2<T>.Delete( Index: Integer);
begin
FListObj.Delete( Index)
end;

procedure TList2<T>.DeleteRange( AIndex, ACount: Integer);
begin
FListObj.DeleteRange( AIndex, ACount)
end;

destructor TList2<T>.Destroy;
begin
FListObj.Free;
inherited
end;

procedure TList2<T>.Exchange(Index1, Index2: Integer);
begin
FListObj.Exchange( Index1, Index2)
end;

function TList2<T>.Extract(const Value: T): T;
begin
result := FListObj.Extract( Value)
end;

function TList2<T>.First: T;
begin
result := FListObj.First
end;

function TList2<T>.GetCapacity: integer;
begin
result := FListObj.Capacity
end;

function TList2<T>.GetCount: integer;
begin
result := FListObj.Count
end;

function TList2<T>.GetEnumerator: ISEnumerator<T>;
begin
result := TListCursor<T>.Create( self)
end;

function TList2<T>.GetItem(Index: integer): T;
begin
result := FListObj[ Index]
end;

function TList2<T>.GetOnNotify: TCollectionNotifyEvent<T>;
begin
result := FListObj.OnNotify
end;

function TList2<T>.IndexOf( const Value: T): Integer;
begin
result := FListObj.IndexOf( Value)
end;

procedure TList2<T>.Insert(Index: Integer; const Value: T);
begin
FListObj.Insert( Index, Value)
end;

procedure TList2<T>.InsertRange(Index: Integer;
  const Collection: ISEnumerable<T>);
var
  Addend: T;
begin
for Addend in Collection do
  begin
  Insert( Index, Addend);
  Inc( Index)
  end
end;

procedure TList2<T>.InsertRange(Index: Integer;
  const Collection: IEnumerable<T>);
begin
FListObj.InsertRange( Index, Collection)
end;

procedure TList2<T>.InsertRange(Index: Integer;
  const Collection: TEnumerable<T>);
begin
FListObj.InsertRange( Index, Collection)
end;

procedure TList2<T>.InsertRange(Index: Integer; const Values: array of T);
begin
FListObj.InsertRange( Index, Values)
end;

function TList2<T>.Last: T;
begin
result := FListObj.Last
end;

function TList2<T>.LastIndexOf(const Value: T): Integer;
begin
result := FListObj.LastIndexOf( Value)
end;

procedure TList2<T>.Move(CurIndex, NewIndex: Integer);
begin
FListObj.Move( CurIndex, NewIndex)
end;

function TList2<T>.Remove(const Value: T): Integer;
begin
result := FListObj.Remove( Value)
end;

procedure TList2<T>.Reverse;
begin
FListObj.Reverse
end;

procedure TList2<T>.SetCapacity(Value: integer);
begin
FListObj.Capacity := Value
end;

procedure TList2<T>.SetCount(Value: integer);
begin
FListObj.Count := Value
end;

procedure TList2<T>.SetItem(Index: integer; const Value: T);
begin
FListObj[ Index] := Value
end;

procedure TList2<T>.SetOnNotify(Value: TCollectionNotifyEvent<T>);
begin
FListObj.OnNotify := Value
end;

procedure TList2<T>.Sort(const AComparer: IComparer<T>);
begin
FListObj.Sort( AComparer)
end;

procedure TList2<T>.Sort;
begin
FListObj.Sort
end;

procedure TList2<T>.TrimExcess;
begin
FListObj.TrimExcess
end;


constructor TListCursor<T>.Create( Enumerable1: TSEnumerable<T>);
begin
inherited Create( Enumerable1);
FIdx := -1
end;

function TListCursor<T>.ListObj: TList<T>;
begin
result := TList2<T>( FEnumerable).FListObj
end;

function TListCursor<T>.GetCurrent: T;
begin
result := ListObj[ FIdx]
end;

function TListCursor<T>.MoveNext: boolean;
begin
result := FIdx <= (ListObj.Count - 2);
if result then
  Inc( FIdx)
end;



end.
