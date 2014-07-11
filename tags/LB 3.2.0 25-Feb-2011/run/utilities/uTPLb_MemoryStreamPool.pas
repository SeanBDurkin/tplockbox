{* ***** BEGIN LICENSE BLOCK *****
Copyright 2010 Sean B. Durkin
This file is part of TurboPower LockBox 3. TurboPower LockBox 3 is free
software being offered under a dual licensing scheme: LGPL3 or MPL1.1.

The contents of this file are subject to the Mozilla Public License (MPL)
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Alternatively, you may redistribute it and/or modify it under the terms of
the GNU Lesser General Public License (LGPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

You should have received a copy of the Lesser GNU General Public License
along with TurboPower LockBox 3.  If not, see <http://www.gnu.org/licenses/>.

TurboPower LockBox is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. In relation to LGPL,
see the GNU Lesser General Public License for more details. In relation to MPL,
see the MPL License for the specific language governing rights and limitations
under the License.

The Initial Developer of the Original Code for TurboPower LockBox version 2
and earlier was TurboPower Software.

 * ***** END LICENSE BLOCK ***** *}

unit uTPLb_MemoryStreamPool;
interface
uses Classes;

type

TMemoryStreamPool = class
  private
    FAllocRecords: TList; // of TAllocationRecord
    FCache: pointer;
    FCacheSegmentInUse: TBits;
    FCachedCount: integer;

  protected
    procedure GetMem1   ( var p: pointer; var CoVector: integer; Size: Integer);   virtual;
    procedure FreeMem1  ( P: Pointer; CoVector: integer; Size: Integer);       virtual;
    procedure ReallocMem1( var p: Pointer; var CoVector: integer; OldSize, Size: Integer);  virtual;
    function  NullCoVector: integer; virtual;
    function  CanManageSize( Size: integer): boolean; virtual;

  public
    function  BayCount: integer;
    procedure GetUsage( Size: integer; var Current, Peak: integer);
    function  GetSize( Idx: Integer): integer;

    function NewMemoryStream( InitSize: integer): TMemoryStream; virtual;
    constructor Create;
    destructor  Destroy; override;
  end;


TPooledMemoryStream = class( TMemoryStream)
  protected
    FPool: TMemoryStreamPool;
    FCoVector: integer;

    function Realloc( var NewCapacity: Longint): Pointer; override;

  public
    constructor Create( Pool1: TMemoryStreamPool);
  end;


implementation






uses RTLConsts, SysUtils, uTPLb_PointerArithmetic;


const
  iNullCoVector = -1;
  MemoryDelta = $200; // Copy from implementation section of Classes.
  CacheSegmentSize = MemoryDelta * 2; // Should be the same or a multiple of MemoryDelta.
  AnticipatedPeakUsage = 30;

type

PAllocationRecord = ^TAllocationRecord;
TAllocationRecord = record
  FSize: integer;
  FCurrentUsage: integer;
  FPeakUsage: integer;
  end;

TAllocationRecordList = class( TList)
  private
    function GetSize( Idx: integer): integer;

  protected
    procedure Notify( Ptr: Pointer; Action: TListNotification); override;

  public
    procedure RecordUsage( Size: integer);
    procedure RecordReturn( Size: integer);
    procedure GetUsage( Size: integer; var Current, Peak: integer);
    property  Sizes[ Idx: Integer]: integer read GetSize; default;
  end;

{ TMemoryStreamPool }

function TMemoryStreamPool.NewMemoryStream( InitSize: integer): TMemoryStream;
begin
result := TPooledMemoryStream.Create( self);
if InitSize > 0 then
  result.Size := InitSize
end;

function TMemoryStreamPool.NullCoVector: integer;
begin
result := iNullCoVector
end;


{ TPooledMemoryStream }

constructor TPooledMemoryStream.Create( Pool1: TMemoryStreamPool);
begin
FPool := Pool1;
if assigned( FPool) then
  FCoVector := FPool.NullCoVector;
inherited Create
end;



function TPooledMemoryStream.Realloc( var NewCapacity: Integer): Pointer;
// Fragments of this method were copied from the Classes unit and modified.
begin
if (NewCapacity > 0) and (NewCapacity <> Size) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
result := Memory;
if NewCapacity <> Capacity then
  begin
  if NewCapacity = 0 then
      begin
      if assigned( FPool) then
          begin
          FPool.FreeMem1( Memory, FCoVector, Capacity);
          FCoVector := FPool.NullCoVector
          end
        else
          FreeMem( Memory);
      result := nil
      end
    else
      begin
      if Capacity = 0 then
          begin
          if assigned( FPool) then
              FPool.GetMem1( result, FCoVector, NewCapacity)
            else
              GetMem( result, NewCapacity)
          end
        else
          begin
          if assigned( FPool) then
              FPool.ReallocMem1( result, FCoVector, Capacity, NewCapacity)
            else
              ReallocMem( result, NewCapacity)
          end;
      if Result = nil then
        raise EStreamError.CreateRes( @SMemoryStreamError)
    end
  end
end;










function TMemoryStreamPool.BayCount: integer;
begin
result := FAllocRecords.Count
end;


function TMemoryStreamPool.CanManageSize( Size: integer): boolean;
begin
result := (Size <= CacheSegmentSize) and (FCachedCount < AnticipatedPeakUsage)
end;




constructor TMemoryStreamPool.Create;
begin
FAllocRecords := TAllocationRecordList.Create;
GetMem( FCache, CacheSegmentSize * AnticipatedPeakUsage);
FCacheSegmentInUse := TBits.Create;
FCacheSegmentInUse.Size := AnticipatedPeakUsage;
FCachedCount := 0
end;


destructor TMemoryStreamPool.Destroy;
begin
FAllocRecords.Free;
FCacheSegmentInUse.Free;
FreeMem( FCache);
inherited
end;


procedure TMemoryStreamPool.FreeMem1( P: Pointer; CoVector: integer; Size: integer);
// Note: Size = -1 means size unknown.
begin
if CoVector = iNullCoVector then
    FreeMem( P)
  else
    begin
    FCacheSegmentInUse[ CoVector] := False;
    if FCachedCount > 0 then
      Dec( FCachedCount)
    end;
TAllocationRecordList( FAllocRecords).RecordReturn( Size)
end;



procedure TMemoryStreamPool.GetMem1( var p: pointer; var CoVector: integer; Size: Integer);
var
  j, Idx: integer;
begin
Idx := -1;
if CanManageSize( Size) then
  for j := 0 to AnticipatedPeakUsage - 1 do
    begin
    if FCacheSegmentInUse[j] then continue;
    Idx := j;
    break
    end;
if Idx <> -1 then
    begin
    p := Offset( FCache, Idx * CacheSegmentSize);
    CoVector := Idx;
    FCacheSegmentInUse[ Idx] := True;
    Inc( FCachedCount)
    end
  else
    begin
    GetMem( p, Size);
    CoVector := iNullCoVector
    end;
TAllocationRecordList( FAllocRecords).RecordUsage( Size)
end;



function TMemoryStreamPool.GetSize( Idx: Integer): integer;
begin
result := TAllocationRecordList( FAllocRecords).Sizes[ Idx]
end;



procedure TMemoryStreamPool.GetUsage( Size: integer; var Current, Peak: integer);
begin
TAllocationRecordList( FAllocRecords).GetUsage( Size, Current, Peak)
end;



procedure TMemoryStreamPool.ReallocMem1( var p: Pointer; var CoVector: integer; OldSize, Size: Integer);
var
  isNewManaged: boolean;
begin
isNewManaged := CanManageSize( Size);
// TO DO: Override and implement efficient caching.
if (CoVector <> iNullCoVector) and isNewManaged then
    begin
    // Old AND new managed
    // Do nothing. Old and New both fit in the same physical space.
    end
  else if (CoVector = iNullCoVector) and (not isNewManaged) then
    begin // neither Old NOR New new managed
    ReallocMem( P, Size);
    // CoVector = iNullCoVector
    end
  else
    begin // Old, new mixed managed
    FreeMem1( P, CoVector, OldSize); // -1 means old size not given.
    GetMem1( p, CoVector, Size)
    end
end;


{ TAllocationRecordList }

function TAllocationRecordList.GetSize( Idx: integer): integer;
begin
result := PAllocationRecord( Items[ Idx])^.FSize
end;


procedure TAllocationRecordList.GetUsage(
  Size: integer; var Current, Peak: integer);
var
  j: integer;
  P: PAllocationRecord;
begin
Current := 0;
Peak    := 0;
for j := 0 to Count - 1 do
  begin
  P := PAllocationRecord( Items[j]);
  if P^.FSize <> Size then continue;
  Current := P^.FCurrentUsage;
  Peak    := P^.FPeakUsage;
  break
  end
end;

procedure TAllocationRecordList.Notify( Ptr: Pointer; Action: TListNotification);
var
  P: PAllocationRecord;
begin
if Action = lnDeleted then
  begin
  P := PAllocationRecord( Ptr);
  Dispose( P)
  end;
inherited
end;



procedure TAllocationRecordList.RecordReturn( Size: integer);
var
  j: integer;
  P: PAllocationRecord;
  Current: integer;
begin
P := nil;
Current := 0;
for j := 0 to Count - 1 do
  begin
  P := PAllocationRecord( Items[j]);
  if P^.FSize <> Size then continue;
  Current := P^.FCurrentUsage;
  break
  end;
if Current > 0 then
  P^.FCurrentUsage := Current - 1
end;



procedure TAllocationRecordList.RecordUsage( Size: integer);
var
  j: integer;
  P, Q: PAllocationRecord;
  Current, Peak: integer;
begin
Current := 0;
Peak    := 0;
P := nil;
for j := 0 to Count - 1 do
  begin
  Q := PAllocationRecord( Items[j]);
  if Q^.FSize <> Size then continue;
  Current := Q^.FCurrentUsage;
  Peak    := Q^.FPeakUsage;
  P       := Q;
  break
  end;
if not assigned( P) then
  begin
  New( P);
  P^.FSize := Size;
  Add( P)
  end;
Inc( Current);
if Current > Peak then
  Peak := Current;
P^.FCurrentUsage := Current;
P^.FPeakUsage    := Peak
end;

end.
