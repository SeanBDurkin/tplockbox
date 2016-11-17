unit SBD.ServiceProvider.Internal;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
//  COPYRIGHT NOTICE
//  ================
//  copyright (c) Sean B. Durkin, 2014
//  The copyright holder and author, is and was Sean B. Durkin of Sydney,
//   Australia.
//
//
//  STATEMENT OF COPYING PERMISSION
//  ===============================
//  You are hereby granted permission to copy this unit under the conditions of
//  the Mozilla Public License, v. 2.0 (MPL 2.0). The MPL 2.0 license can be
//  found in the file named "MPL2.0_LicenseTerms.txt" distributed along with
//  this library.
//
//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

interface
uses SBD.ServiceProvider, Classes, Generics.Collections, Rtti;

type

RServiceKey = record
    FServiceIID: TGUID;
    FConfig: string;
    end;

RService = record
    FLiveService: IInterface;
    FFlyweightCreateBase: TClass;
    FFlyweightFactory: TServiceFactory;
    FConstructor: TRttiMethod;
    FCookie: Integer;
    FIdentifier: string;
    procedure Create;
  end;

RGroupOfServices = record
  private type
    TServiceList = class( TList<RService>)
    public
      procedure SetLiveService( Idx: integer; const LiveService: IInterface);
      function  Clone: TServiceList;
    end;
  public
    FAffinity: TServiceAffinity;
    FMembers: TServiceList;
    FActiveIndex: integer;
    FisPooled: boolean;
    FMakeCollection: TCooperativeCollectionFactory;
    FAddToCollection: TAddToCollectionProc;
    FCollectionName: string;
    procedure Create;
    procedure Free;
    function  isEmpty: boolean;
  end;

IServiceProviderInternal = interface
  ['{FD2CF9F3-477D-4505-8E74-3C866B1611F9}']
    function Internal_Acquire( const Svc: TGUID; const Client: IInterface;
      out Intf; const Config: string;
      const SecondaryProvider: IServiceProvider;
      SecondaryIsOverride: boolean): boolean;

    function GnAcquire( const ServiceIID: TGUID; const Client: IInterface;
      out Intf; const Config: string;
      const InjectionOverride: IServiceProvider): boolean;

    function GetServices: TDictionary<RServiceKey,RGroupOfServices>;
  end;

TServiceProvider = class( TInterfacedObject, IServiceProvider, IServiceProviderInternal)
  private // IServiceProvider
    function  GetServiceAffinity( const ServiceIID: TGUID; const Config: string): TServiceAffinity;
    function  GetActiveArrayMemberCookie( const ServiceIID: TGUID; const Config: string): integer;
    procedure SetActiveArrayMemberByCookie( const ServiceIID: TGUID; const Config: string; Cookie: integer);
    function  GetActiveArrayMemberName( const ServiceIID: TGUID; const Config: string): string;
    procedure SetActiveArrayMemberByName( const ServiceIID: TGUID; const Config, Name: string);
    function  GetNameByCookie( Cookie: integer): string;
    function  GetIsPooled( const ServiceIID: TGUID; const Config: string): boolean;
    procedure SetIsPooled( const ServiceIID: TGUID; const Config: string; Value: boolean);
    function  Gn: TGenericServiceProvider;
    function  GetGroupCount( const ServiceIID: TGUID; const Config: string): integer;
    function  GetCookies( const ServiceIID: TGUID; const Config: string): TIntegers;
    function  GetCookieByName( const ServiceIID: TGUID; const Config, Name: string): integer;
    function  GetCookieOfLiveService( const ServiceIID: TGUID; const Service: IInterface; const Config: string): integer;
    function  GetCookiesOfServiceClass( Registrant: TClass): TIntegers;
    procedure SetCompetitiveAffinity( const ServiceIID: TGUID; const Config: string);
    procedure SetCooperativeAffinity( const ServiceIID: TGUID;
                  const CollectionName, Config: string;
                  MakeCollection: TCooperativeCollectionFactory;
                  AddToCollection: TAddToCollectionProc);
    function  RegisterLiveService( const ServiceIID: TGUID; const Service: IInterface;
                                   const Config: string = ''; const SelectionArrayMemberName: string = ''): integer;
    function  RegisterFlyweightService( const ServiceIID: TGUID; Factory: TServiceFactory;
                                        const Config: string = ''; const SelectionArrayMemberName: string = ''): integer;
    {$if CompilerVersion < 22}
    function  RegisterServiceClass( const ServiceIID: TGUID; Registrant: TClass): integer;
    {$else}
    function  RegisterServiceClass( const ServiceIID: TGUID; Registrant: TClass): TIntegers;
    {$ifend}
    procedure DeregisterServiceByCookie( Cookie: integer);
    procedure DeregisterServicesByCookie( const Cookies: TIntegers);
    procedure DeregisterLiveServiceByName( const ServiceIID: TGUID; const Config: string = ''; const SelectionArrayMemberName: string = '');
    procedure DeregisterServicesOfClass( Registrant: TClass);
    function  Acquire( const ServiceIID: TGUID; const Client: IInterface; out Intf; const Config: string = ''; const InjectionOverride: IServiceProvider = nil): boolean;
    function  Clone: IServiceProvider;
    procedure Merge( const Source: IServiceProvider);
    procedure ShutDown;

  strict private
    function  AcquireOneService(  const Client: IInterface; const SecondaryProvider: IServiceProvider;
                                  const Config: string; const Svc: TGUID;
                                  out Intf1; const Value1: RService; var isInception: boolean): boolean;

  private // IServiceProviderInternal
    function Internal_Acquire( const Svc: TGUID; const Client: IInterface;
      out Intf; const Config: string;
      const SecondaryProvider: IServiceProvider;
      SecondaryIsOverride: boolean): boolean;
    function IServiceProviderInternal.GnAcquire = Acquire;

    function  Internal_GetServices: TDictionary<RServiceKey,RGroupOfServices>;
    function  IServiceProviderInternal.GetServices = Internal_GetServices;

  private
    procedure OnServiceNotify( Sender: TObject;
      const Item: RGroupOfServices; Action: TCollectionNotification);
    constructor CreateAsClone( Source: TServiceProvider);
    constructor BaseCreate;
    function  FindKey( Cookie: Integer; out Key: RServiceKey; out Idx: integer): boolean;
    procedure RemoveGroup( const Key: RServiceKey);
    function  NextCookie: integer;

  private type TServiceProc = reference to function (
                const ServiceIID: TGUID; const Config, Id: string;
                RegCls: TClass; ConstructorMethod: TRttiMethod): integer;

    function ParseServiceClass(
      Registrant: TClass; const ServiceIID: TGUID;
      ParseAction: TServiceProc;
      var Cookies: TIntegers): integer;

  private
    FGn: TGenericServiceProvider;
    FServices: TDictionary<RServiceKey,RGroupOfServices>;
    FdoFreeGroupOnRemoval: boolean;
    FNextCookie: integer;
    FRtti: TRttiContext;

  public
    constructor Create;
    destructor  Destroy; override;
  end;

implementation


















uses SysUtils, TypInfo, Generics.Defaults;

type
TSBDUnit_BaseAttributeClass = class of TSBDUnit_BaseAttribute;
TSuiteInjector = class;
IInjectableMember = interface
  ['{EB594A82-EAAE-4555-BAA0-33F86E6B5D8D}']
    function  IsInjectable: boolean;
    function  AsRttiMember: TRttiMember;
    function  MemberType: TRttiType;
    procedure SetValue( const Inject: TValue);
  end;

TInjectableMember = class( TInterfacedObject, IInjectableMember)
  protected
    FOwner: TSuiteInjector;

    function  IsInjectable: boolean;   virtual; abstract;
    function  AsRttiMember: TRttiMember;  virtual; abstract;
    function  MemberType: TRttiType;   virtual; abstract;
    procedure SetValue( const Inject: TValue);    virtual; abstract;
  public
    constructor Create( Owner1: TSuiteInjector);
  end;

ISuiteInjector = interface
  ['{72088119-43E6-4273-928B-0C84E0F84791}']
    procedure   Inject( const Provider: IServiceProvider);
  end;

TSuiteInjector = class( TInterfacedObject, ISuiteInjector)
  public
    FInjectionSubject: TObject;
    FServiceType : TRttiType;
    FClient: IInterface;
    FInjectionsClient: IInterface;
    FInjectionOverride: IServiceProvider;

    constructor Create( InjectionSubject1: TObject;
                        ServiceType1 : TRttiType;
                        const Client1: IInterface;
                        const InjectionOverride: IServiceProvider);
    destructor  Destroy; override;
    procedure   Inject( const Provider: IServiceProvider);

  protected
    FMembers: TEnumerable<IInjectableMember>;
    function  GetMembers: TEnumerable<IInjectableMember>;   virtual; abstract;
  end;
TSuiteInjectorClass = class of TSuiteInjector;
TSuitesArray = array[0..1] of TSuiteInjectorClass;


TDataMemberInjector = class( TSuiteInjector)
  protected
    function  GetMembers: TEnumerable<IInjectableMember>;          override;
  end;

TWritablePropertyInjector = class( TSuiteInjector)
  protected
    function  GetMembers: TEnumerable<IInjectableMember>;          override;
  end;

TInjectableDataMember = class( TInjectableMember)
  private
    FMember: TRttiField;
  protected
    function  IsInjectable: boolean;              override;
    function  AsRttiMember: TRttiMember;          override;
    function  MemberType: TRttiType;              override;
    procedure SetValue( const Inject: TValue);    override;
  public
    constructor Create( Owner1: TSuiteInjector; Member1: TRttiField);
  end;

TInjectablePropMember = class( TInjectableMember)
  private
    FMember: TRttiProperty;
  protected
    function  IsInjectable: boolean;              override;
    function  AsRttiMember: TRttiMember;          override;
    function  MemberType: TRttiType;              override;
    procedure SetValue( const Inject: TValue);    override;
  public
    constructor Create( Owner1: TSuiteInjector; Member1: TRttiProperty);
  end;


TMemberEnumable<MemberT> = class( TEnumerable<IInjectableMember>)
  private
    FMembers: TArray<MemberT>;
    FSuite: TSuiteInjector;
  public
    constructor Create( Suite1: TSuiteInjector);     virtual;
  end;

TPropEnumerable = class( TMemberEnumable<TRttiProperty>)
  protected
    function DoGetEnumerator: TEnumerator<IInjectableMember>; override;
  private
    constructor Create( Suite1: TSuiteInjector);     override;
  end;

TFieldEnumerable = class( TMemberEnumable<TRttiField>)
  protected
    function DoGetEnumerator: TEnumerator<IInjectableMember>; override;
  private
    constructor Create( Suite1: TSuiteInjector);     override;
  end;

TMemberEnumator<MemberT> = class( TEnumerator<IInjectableMember>)
  protected
    FOwner: TMemberEnumable<MemberT>;
    FIndex: integer;
    function DoMoveNext: Boolean; override;
  public
    constructor Create( Owner1: TMemberEnumable<MemberT>);
  end;

TPropEnumator = class( TMemberEnumator<TRttiProperty>)
  protected
    function DoGetCurrent: IInjectableMember; override;
  public
    constructor Create( Owner1: TMemberEnumable<TRttiProperty>);
  end;

TFieldEnumator = class( TMemberEnumator<TRttiField>)
  protected
    function DoGetCurrent: IInjectableMember; override;
  public
    constructor Create( Owner1: TMemberEnumable<TRttiField>);
  end;

TServiceEqualityComparer = class( TInterfacedObject, IEqualityComparer<RServiceKey>)
  private
    function Key_Equals( const Left, Right: RServiceKey): boolean;
    function IEqualityComparer<RServiceKey>.Equals = Key_Equals;
    function Key_GetHashCode( const Value: RServiceKey): integer;
    function IEqualityComparer<RServiceKey>.GetHashCode = Key_GetHashCode;
  end;


constructor TServiceProvider.BaseCreate;
begin
FGn := TGenericServiceProvider.Create( self);
FServices := TDictionary<RServiceKey,RGroupOfServices>.Create( TServiceEqualityComparer.Create);
FServices.OnValueNotify := OnServiceNotify;
FdoFreeGroupOnRemoval := False;
FRtti.Create
end;


constructor TServiceProvider.Create;
begin
BaseCreate;
FNextCookie := 1;

RegisterFlyweightService( IServiceProvider,
  function( const Config: string;
  const ServiceProvider: IServiceProvider): IInterface
    begin
    result := self as IServiceProvider
    end, '', '');

RegisterFlyweightService( IServiceProvider,
  function( const Config: string;
  const ServiceProvider: IServiceProvider): IInterface
    begin
    result := StandardServiceProvider
    end, 'New', '')
end;

constructor TServiceProvider.CreateAsClone( Source: TServiceProvider);
var
  Pair: TPair<RServiceKey,RGroupOfServices>;
  ClonedGroup: RGroupOfServices;
begin
BaseCreate;
FNextCookie := Source.FNextCookie;
for Pair in Source.FServices do
  begin
  ClonedGroup := Pair.Value;
  ClonedGroup.FMembers := ClonedGroup.FMembers.Clone;
  FServices.Add( Pair.Key, ClonedGroup)
  end
end;


procedure TServiceProvider.Merge( const Source: IServiceProvider);
var
  IntrnlIntf: IServiceProviderInternal;
  Pair: TPair<RServiceKey,RGroupOfServices>;
  Group: RGroupOfServices;
  Service, Addend: RService;
  isNewGroup: boolean;
begin
if Supports( Source, IServiceProviderInternal, IntrnlIntf) then
  for Pair in IntrnlIntf.GetServices do
    begin
    isNewGroup := not FServices.ContainsKey( Pair.Key);
    if isNewGroup then
        Group.Create
      else
        Group := FServices[ Pair.Key];
    for Service in Pair.Value.FMembers do
      begin
      Addend := Service;
      Addend.FCookie := NextCookie;
      Group.FMembers.Add( Addend)
      end;
    if isNewGroup then
      Group.FAffinity := Pair.Value.FAffinity;
    if (not assigned( Group.FMakeCollection))  and
       (not assigned( Group.FAddToCollection))  then
       begin
       Group.FMakeCollection  := Pair.Value.FMakeCollection;
       Group.FAddToCollection := Pair.Value.FAddToCollection
       end;
    Group.FisPooled := Group.FisPooled or Pair.Value.FisPooled;
    FServices.AddOrSetValue( Pair.Key, Group)
    end
end;


procedure TServiceProvider.OnServiceNotify( Sender: TObject;
  const Item: RGroupOfServices; Action: TCollectionNotification);
begin
if FdoFreeGroupOnRemoval and (Action = cnRemoved) then
  Item.Free
end;



function TServiceProvider.ParseServiceClass(
  Registrant: TClass; const ServiceIID: TGUID; ParseAction: TServiceProc;
  var Cookies: TIntegers): integer;
var
  ServiceType: TRttiType;
  A: TCustomAttribute;
  M: TRttiMethod;
  ConfigAttrib: Configuration;
  j: integer;
  ConstructorParamCount: integer;
  Params: TArray<TRttiParameter>;
  Configs, Blacklist: TStrings;
  sConfig: string;
  Cookie: integer;
  L: integer;
begin
SetLength( Cookies, 0);
result := 0;
L := 1;
if assigned( Registrant) and Supports( Registrant, ServiceIID) then
  begin
  Configs := TStringList.Create;
  Blacklist := TStringList.Create;
  try
    ServiceType := FRtti.GetType( Registrant);
    for A in ServiceType.GetAttributes do
      if A is BlacklistConfigs then
        Blacklist.AddStrings( BlacklistConfigs(A).FConfigs);
    for M in ServiceType.GetMethods do
      begin
      if not M.IsConstructor then continue;
      ConstructorParamCount := -1;
      Params := M.GetParameters;
      if Length( Params) = 0 then
          ConstructorParamCount := 0
        else if (Length( Params) = 1) and
                (Params[0].ParamType.TypeKind = tkUString) and
                (Params[0].Flags = [pfConst]) then
          ConstructorParamCount := 1;
      if ConstructorParamCount = -1 then continue;
      for A in M.GetAttributes do
        begin
        if not (A is Configuration) then continue;
        ConfigAttrib := Configuration(A);
        for j := 0 to ConfigAttrib.FConfigs.Count - 1 do
          begin
          sConfig := ConfigAttrib.FConfigs[j];
          if (Configs  .IndexOf( sConfig) <> -1) or
             (Blacklist.IndexOf( sConfig) <> -1) then
          continue;
          Configs.Add( sConfig);
          if assigned( ParseAction) then
              Cookie := ParseAction( ServiceIID, sConfig, ConfigAttrib.FId, Registrant, M)
            else
              Cookie := 0;
          if Cookie >= 1 then
            begin
            if result = 0 then
              result := Cookie;
            SetLength( Cookies, L);
            Cookies[L-1] := Cookie;
            Inc( L)
            end;
          end
        end
      end
  finally
    Configs.Free;
    Blacklist.Free
    end
  end
end;

procedure RService.Create;
begin
FLiveService := nil;
FFlyweightCreateBase := nil;
FFlyweightFactory := nil;
FConstructor := nil;
FCookie := -1;
FIdentifier := ''
end;



procedure RGroupOfServices.Create;
begin
FAffinity    := afCompetitive;
FCollectionName := '';
FMembers     := TServiceList.Create;
FMakeCollection  := nil;
FAddToCollection := nil;
FIsPooled    := False;
FActiveIndex := -1
end;

procedure RGroupOfServices.Free;
begin
FMembers.Free
end;


function RGroupOfServices.isEmpty: boolean;
begin
result := FMembers.Count = 0;
if result and (FAffinity = afCooperative) then
  result := (not assigned( FMakeCollection)) and (not assigned( FAddToCollection))
end;

function RGroupOfServices.TServiceList.Clone: TServiceList;
begin
result := TServiceList.Create( self)
end;

procedure RGroupOfServices.TServiceList.SetLiveService(
  Idx: integer; const LiveService: IInterface);
var
  Member: RService;
begin
if (Idx < 0) or (Idx >= Count) then exit;
Member := Items[Idx];
if assigned( Member.FLiveService) then exit;
Member.FLiveService := LiveService;
Items[ Idx] := Member
end;


function TServiceProvider.Internal_GetServices: TDictionary<RServiceKey, RGroupOfServices>;
begin
result := FServices
end;


function TServiceProvider.Acquire( const ServiceIID: TGUID;
  const Client: IInterface; out Intf; const Config: string;
  const InjectionOverride: IServiceProvider): boolean;
begin
result := Internal_Acquire( ServiceIID, Client, Intf, Config, InjectionOverride, True)
end;


function TServiceProvider.AcquireOneService(
    const Client: IInterface; const SecondaryProvider: IServiceProvider;
    const Config: string; const Svc: TGUID;
    out Intf1; const Value1: RService; var isInception: boolean): boolean;
var
  Obj: TObject;
  IntfResult: IInterface;

  procedure CreateFromFlyweight;
  const
    Suites: TSuitesArray = (TDataMemberInjector, TWritablePropertyInjector);
  var
    Cls: TClass;
    ServiceType: TRttiType;
    SuiteCls: TSuiteInjectorClass;
    Hold: IInterface;
    Injector: ISuiteInjector;
    A: TArray<TRttiParameter>;  // Work-around bug in D2010-with-packages
  begin
  Cls := Value1.FFlyweightCreateBase;
  if assigned( Cls) then
      begin
      Obj  := Cls.NewInstance;
      Supports( Obj, IInterface, Hold);
      // ^-- Needed for reference counted objects which do not use
      //  the BeforeConstruction property to set the reference count
      //  to one.
      ServiceType := FRtti.GetType( Cls);
      for SuiteCls in Suites do
        begin
        Injector := nil;
        Injector := SuiteCls.Create( Obj, ServiceType, Client, SecondaryProvider) as ISuiteInjector;
        Injector.Inject( self)
        end;
      end
    else
      Obj := nil;
  if not assigned( Value1.FConstructor) then
    raise Exception.Create( 'Internal error - AcquireOneService');
  A := Value1.FConstructor.GetParameters;
  if Length( A) = 0 then
      Value1.FConstructor.Invoke( Obj, [])
    else
      Value1.FConstructor.Invoke( Obj, [Config]);
  Obj.AfterConstruction;
  result := Supports( Obj, Svc, IntfResult);
  if result then
      IInterface( Intf1) := IntfResult
    else
      FreeAndNil( Obj);
  isInception := result
  end;

begin
isInception := False;
IInterface( Intf1) := Value1.FLiveService;
result := assigned( IInterface( Intf1));
if result then exit;
Obj := nil;
try
  if not assigned( Value1.FFlyweightFactory) then
     CreateFromFlyweight
  else
      begin
      IntfResult := Value1.FFlyweightFactory( Config, self);
      result := Supports( IntfResult, Svc, Intf1);
      isInception := result
      end
  except
    begin
    Obj.Free;
    raise
    end
  end
end;


function TServiceProvider.Internal_Acquire( const Svc: TGUID;
  const Client: IInterface; out Intf; const Config: string;
  const SecondaryProvider: IServiceProvider;
  SecondaryIsOverride: boolean): boolean;
const
  Suites: array[0..1] of TSuiteInjectorClass = (TDataMemberInjector, TWritablePropertyInjector);
var
  Key: RServiceKey;
  Value: RService;
  Group: RGroupOfServices;
  Gang: IInterface;
  Gang_AsInterfaceList: IInterfaceList;
  GangMember: IInterface;
  isNewbie: boolean;
  Idx: integer;

begin
Key.FServiceIID := Svc;
Key.FConfig     := Config;
result := FServices.TryGetValue( Key, Group);
if not result then exit;
case Group.FAffinity of
  afCooperative:
    begin
    Gang_AsInterfaceList := nil;
    if assigned( Group.FMakeCollection) then
        Gang := Group.FMakeCollection( Config)
      else
        Gang := IInterface( (TInterfaceList.Create as IInterfaceList));
    IInterface( Intf) := Gang;
    for Idx := 0 to Group.FMembers.Count - 1 do
      begin
      Value := Group.FMembers[ Idx];
      if AcquireOneService( Client, SecondaryProvider, Config, Svc, GangMember, Value, isNewbie) then
        begin
        if assigned( Group.FAddToCollection) then
            Group.FAddToCollection( Gang, GangMember)
          else if Supports( Gang, IInterfaceList, Gang_AsInterfaceList) then
            Gang_AsInterfaceList.Add( GangMember)
          else
            isNewbie := False;
        if isNewbie and Group.FisPooled then
          Group.FMembers.SetLiveService( Idx, GangMember)
        end
      end
    end;
  afCompetitive:
    begin
    Idx := Group.FActiveIndex;
    result := (Idx >= 0) and (Idx < Group.FMembers.Count);
    if not result then exit;
    result := AcquireOneService( Client, SecondaryProvider, Config, Svc, Intf, Group.FMembers[Idx], isNewbie);
    if isNewbie and Group.FisPooled then
      Group.FMembers.SetLiveService( Idx, IInterface( Intf))
    end
  end
end;

function TServiceProvider.Clone: IServiceProvider;
begin
result := TServiceProvider.CreateAsClone( self)
end;

procedure TServiceProvider.DeregisterLiveServiceByName(
  const ServiceIID: TGUID; const Config, SelectionArrayMemberName: string);
begin
DeregisterServiceByCookie( GetCookieByName( ServiceIID, Config, SelectionArrayMemberName))
end;

procedure TServiceProvider.DeregisterServiceByCookie( Cookie: integer);
var
  Key: RServiceKey;
  Idx: Integer;
  Group: RGroupOfServices;
begin
if (Cookie >= 1) and FindKey( Cookie, Key, Idx) then
  begin
  Group := FServices.Items[ Key];
  Group.FMembers.Delete( Idx);
  if Group.FActiveIndex >= Group.FMembers.Count then
     Group.FActiveIndex := Group.FMembers.Count - 1;
  if Group.IsEmpty then
      RemoveGroup( Key)
    else
      FServices.Items[Key] := Group
  end;
end;

procedure TServiceProvider.DeregisterServicesByCookie( const Cookies: TIntegers);
var
  Cookie: integer;
begin
for Cookie in Cookies do
  DeregisterServiceByCookie( Cookie)
end;

procedure TServiceProvider.DeregisterServicesOfClass( Registrant: TClass);
begin
DeregisterServicesByCookie( GetCookiesOfServiceClass( Registrant))
end;

destructor TServiceProvider.Destroy;
begin
ShutDown;
FServices.Free;
FRtti.Free;
FGn.Free;
inherited
end;

function TServiceProvider.FindKey(
  Cookie: Integer; out Key: RServiceKey; out Idx: integer): boolean;
var
  Pair: TPair<RServiceKey,RGroupOfServices>;
  Service: RService;
begin
result := False;
for Pair in FServices do
  begin
  Idx := 0;
  for Service in Pair.Value.FMembers do
    begin
    result := Service.FCookie = Cookie;
    if result then Break;
    Inc( Idx)
    end;
  if not result then continue;
  Key := Pair.Key;
  break
  end
end;

function TServiceProvider.GetActiveArrayMemberCookie(
  const ServiceIID: TGUID; const Config: string): integer;
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Members: RGroupOfServices.TServiceList;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
Members := nil;
if FServices.ContainsKey( Key) then
    begin
    Group   := FServices.Items[Key];
    Members := Group.FMembers;
    result  := Group.FActiveIndex
    end
  else
    result := -1;
if (result >= 0) and (result < Members.Count) then
    result := Members[result].FCookie
  else
    result := 0
end;

function TServiceProvider.GetActiveArrayMemberName(
  const ServiceIID: TGUID; const Config: string): string;
var
  Key: RServiceKey;
  Idx: integer;
  Group: RGroupOfServices;
  Members: RGroupOfServices.TServiceList;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
Members := nil;
if FServices.ContainsKey( Key) then
    begin
    Group   := FServices.Items[Key];
    Members := Group.FMembers;
    Idx     := Group.FActiveIndex
    end
  else
    Idx := -1;
if (Idx >= 0) and (Idx < Members.Count) then
    result := Members[Idx].FIdentifier
  else
    result := ''
end;

function TServiceProvider.GetCookieByName(
  const ServiceIID: TGUID; const Config, Name: string): integer;
var
  Key: RServiceKey;
  Service: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
result := 0;
if FServices.ContainsKey( Key) then
  for Service in FServices.Items[Key].FMembers do
    begin
    if Service.FIdentifier <> Name then continue;
    result := Service.FCookie;
    break
    end
end;

function TServiceProvider.GetCookieOfLiveService(
  const ServiceIID: TGUID; const Service: IInterface; const Config: string): integer;
var
  Key: RServiceKey;
  ServiceRec: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
result := 0;
if FServices.ContainsKey( Key) then
  for ServiceRec in FServices.Items[Key].FMembers do
    begin
    if ServiceRec.FLiveService <> Service then continue;
    result := ServiceRec.FCookie;
    break
    end
end;

function TServiceProvider.GetCookies(
  const ServiceIID: TGUID; const Config: string): TIntegers;
var
  Key: RServiceKey;
  Service: RService;
  L: integer;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
L := 1;
if FServices.ContainsKey( Key) then
  for Service in FServices.Items[Key].FMembers do
    if Service.FCookie >= 1 then
      begin
      SetLength( result, L);
      result[L-1] := Service.FCookie;
      Inc( L)
      end;
end;

function TServiceProvider.GetCookiesOfServiceClass(
  Registrant: TClass): TIntegers;
var
  Group: RGroupOfServices;
  Service: RService;
  L: integer;
begin
L := 1;
for Group in FServices.Values do
  for Service in Group.FMembers do
    begin
    if Service.FFlyweightCreateBase <> Registrant then continue;
    SetLength( result, L);
    result[L-1] := Service.FCookie;
    Inc( L)
    end
end;

function TServiceProvider.GetGroupCount(
  const ServiceIID: TGUID; const Config: string): integer;
var
  Key: RServiceKey;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    result := FServices.Items[Key].FMembers.Count
  else
    result := 0
end;

function TServiceProvider.GetIsPooled(
  const ServiceIID: TGUID; const Config: string): boolean;
var
  Key: RServiceKey;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
result := FServices.ContainsKey( Key) and FServices.Items[Key].FisPooled
end;

function TServiceProvider.GetNameByCookie( Cookie: integer): string;
var
  Key: RServiceKey;
  Idx: integer;
begin
if FindKey( Cookie, Key, Idx) then
    result := FServices.Items[Key].FMembers[Idx].FIdentifier
  else
    result := ''
end;

function TServiceProvider.GetServiceAffinity(
  const ServiceIID: TGUID; const Config: string): TServiceAffinity;
var
  Key: RServiceKey;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    result := FServices.Items[Key].FAffinity
  else
    result := afCompetitive
end;

function TServiceProvider.Gn: TGenericServiceProvider;
// Design notes
// 1. TServiceProvider owns and weakly points to an instance of
//      TGenericServiceProvider (via FGn).
// 2. TGenericServiceProvider weakly points (data member = FController) back to
//      the parent TServiceProvider through the IServiceProviderInternal
//      interface.
begin
result := FGn
end;


function TServiceProvider.NextCookie: integer;
begin
result := FNextCookie;
Inc( FNextCookie)
end;

function TServiceProvider.RegisterFlyweightService(
  const ServiceIID: TGUID; Factory: TServiceFactory; const Config,
  SelectionArrayMemberName: string): integer;
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Value: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    Group := FServices.Items[ Key]
  else
    Group.Create;
Value.FLiveService := nil;
Value.FFlyweightFactory := Factory;
Value.FIdentifier := SelectionArrayMemberName;
result := NextCookie;
Value.FCookie := result;
Group.FMembers.Add( Value);
if Group.FActiveIndex = -1 then
   Group.FActiveIndex := 0;
FServices.AddOrSetValue( Key, Group)
end;

function TServiceProvider.RegisterLiveService(
  const ServiceIID: TGUID; const Service: IInterface; const Config,
  SelectionArrayMemberName: string): integer;
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Value: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    Group := FServices.Items[ Key]
  else
    Group.Create;
Value.FFlyweightCreateBase := nil;
Value.FFlyweightFactory := nil;
Value.FLiveService := Service;
Value.FIdentifier := SelectionArrayMemberName;
result := NextCookie;
Value.FCookie := result;
Group.FMembers.Add( Value);
if Group.FActiveIndex = -1 then
   Group.FActiveIndex := 0;
FServices.AddOrSetValue( Key, Group)
end;

{$if CompilerVersion < 22}
function TServiceProvider.RegisterServiceClass( const ServiceIID: TGUID; Registrant: TClass): integer;
{$else}
function  TServiceProvider.RegisterServiceClass( const ServiceIID: TGUID; Registrant: TClass): TIntegers;
{$ifend}
var
  AllCookies: TIntegers;
  FirstCookie: integer;
begin
FirstCookie :=
  ParseServiceClass( Registrant, ServiceIID,
  function (
    const ServiceIID: TGUID; const Config, Id: string;
    RegCls: TClass; ConstructorMethod: TRttiMethod): integer
  var
    Key: RServiceKey;
    Service: RService;
    Group: RGroupOfServices;
  begin
    result := NextCookie;
    Key.FServiceIID := ServiceIID;
    Key.FConfig     := Config;
    if FServices.ContainsKey( Key) then
        Group := FServices[ Key]
      else
        Group.Create;
    Service.Create;
    Service.FFlyweightCreateBase := RegCls;
    Service.FConstructor := ConstructorMethod;
    Service.FCookie := result;
    Service.FIdentifier := Id;
    Group.FMembers.Add( Service);
    if Group.FActiveIndex < 0 then
      Group.FActiveIndex := 0;
    FServices.AddOrSetValue( Key, Group)
  end,
  AllCookies);
{$if CompilerVersion < 22}
result := FirstCookie;
{$else}
result := AllCookies;
{$ifend}
if FirstCookie = 0 then
  raise Exception.CreateFmt( 'TServiceProvider.RegisterServiceClass - ' +
    '%s not validly decorated for registration.<br/>', [Registrant.ClassName])
end;

procedure TServiceProvider.RemoveGroup( const Key: RServiceKey);
begin
FdoFreeGroupOnRemoval := True;
try
  FServices.Remove( Key)
finally
  FdoFreeGroupOnRemoval := False
  end
end;

procedure TServiceProvider.SetActiveArrayMemberByCookie(
  const ServiceIID: TGUID; const Config: string; Cookie: integer);
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Idx: Integer;
begin
if Cookie <= 0 then exit;
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if not FServices.ContainsKey( Key) then exit;
Group := FServices.Items[ Key];
for Idx := 0 to Group.FMembers.Count - 1 do
  begin
  if Group.FMembers[Idx].FCookie <> Cookie then continue;
  if Group.FActiveIndex <> Idx then
    begin
    Group.FActiveIndex := Idx;
    FServices.Items[ Key] := Group
    end;
  break
  end
end;

procedure TServiceProvider.SetActiveArrayMemberByName(
  const ServiceIID: TGUID; const Config, Name: string);
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Value: RService;
  Cookie: integer;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if not FServices.ContainsKey( Key) then exit;
Group := FServices.Items[ Key];
Cookie := 0;
for Value in Group.FMembers do
  begin
  if Value.FIdentifier <> Name then continue;
  Cookie := Value.FCookie;
  break
  end;
if Cookie >= 1 then
  SetActiveArrayMemberByCookie( ServiceIID, Config, Cookie)
end;

procedure TServiceProvider.SetCompetitiveAffinity(
  const ServiceIID: TGUID; const Config: string);
var
  Key: RServiceKey;
  Group: RGroupOfServices;
begin
if GetServiceAffinity( ServiceIID, Config) = afcompetitive then exit;
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    Group := FServices.Items[ Key]
  else
    Group.Create;
Group.FAffinity := afcompetitive;
if (Group.FActiveIndex < 0) or
   (Group.FActiveIndex >= Group.FMembers.Count) then
  Group.FActiveIndex := 0;
Group.FMakeCollection  := nil;
Group.FAddToCollection := nil;
if Group.isEmpty then
    begin
    if FServices.ContainsKey( Key) then
        RemoveGroup( Key)
      else
        Group.Free
    end
  else
    FServices.AddOrSetValue( Key, Group)
end;

procedure TServiceProvider.SetCooperativeAffinity(
  const ServiceIID: TGUID;
  const CollectionName, Config: string;
  MakeCollection: TCooperativeCollectionFactory;
  AddToCollection: TAddToCollectionProc);
var
  Key: RServiceKey;
  Group: RGroupOfServices;
begin
if GetServiceAffinity( ServiceIID, Config) = afCooperative then exit;
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    Group := FServices.Items[ Key]
  else
    Group.Create;
if CollectionName <> '' then
  Group.FCollectionName := CollectionName;
Group.FAffinity := afCooperative;
Group.FMakeCollection  := MakeCollection;
Group.FAddToCollection := AddToCollection;
FServices.AddOrSetValue( Key, Group)
end;

procedure TServiceProvider.SetIsPooled(
  const ServiceIID: TGUID; const Config: string; Value: boolean);
var
  Key: RServiceKey;
  Group: RGroupOfServices;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    begin
    Group := FServices.Items[Key];
    if Group.FisPooled <> Value then
      begin
      Group.FisPooled := Value;
      FServices.Items[Key] := Group
      end
    end
  else if Value then
    raise Exception.Create('Cannot set isPooled on unregistered service');
end;

procedure TServiceProvider.ShutDown;
begin
FdoFreeGroupOnRemoval := True;
try
  FServices.Clear
finally
  FdoFreeGroupOnRemoval := False
  end;
if assigned( FGn) then
  FGn.ShutDown
end;


constructor TSuiteInjector.Create(
  InjectionSubject1: TObject; ServiceType1: TRttiType;
  const Client1: IInterface; const InjectionOverride: IServiceProvider);
begin
FInjectionSubject  := InjectionSubject1;
FServiceType       := ServiceType1;
FClient            := Client1;
FInjectionOverride := InjectionOverride;
Supports( FInjectionSubject, IInterface, FInjectionsClient)
end;


destructor TSuiteInjector.Destroy;
begin
FMembers.Free;
inherited
end;

procedure TSuiteInjector.Inject( const Provider: IServiceProvider);
var
  Member: IInjectableMember;
  isInjection: boolean;
  sInjectionConfig: string;
  Inject: IInterface;
  InjectAsRttiValue: TValue;
  MemberType1: TRttiType;
  doInject: boolean;
  InjectableGUID: TGUID;
  AvailableCount: integer;
  InjectionAttri: TSBDUnit_BaseAttribute;
  Providers: TList<IServiceProvider>;
  SelectedProvider, Provider1: IServiceProvider;
  isMarkedCollection: boolean;
  CollectionAttri: TSBDUnit_BaseAttribute;
  sColName: string;
  Pair: TPair<RServiceKey,RGroupOfServices>;
  isIInterfaceList: boolean;

  function IsMemberDecorated( const Memb: IInjectableMember; AttriCls: TSBDUnit_BaseAttributeClass;
                              var Attri: TSBDUnit_BaseAttribute): boolean;
  var
    Run: TCustomAttribute;
  begin
  result := False;
  for Run in Memb.AsRttiMember.GetAttributes do
    begin
    result := Run.InheritsFrom( AttriCls);
    if not result then continue;
    Attri := Run as TSBDUnit_BaseAttribute;
    break
    end;
  end;

begin
for Member in GetMembers do
  begin
  MemberType1 := Member.MemberType;
  sInjectionConfig := '';
  Inject := nil;
  InjectionAttri := nil;
  isInjection := IsMemberDecorated( Member, Injection, InjectionAttri);
  if isInjection then
    sInjectionConfig := (InjectionAttri as Injection).FConfig;
  doInject := False;
  if isInjection and
     (MemberType1.TypeKind = tkInterface) and
     (MemberType1 is TRttiInterfaceType) and
     Member.IsInjectable then
    begin
    InjectableGUID := TRttiInterfaceType( MemberType1).GUID;
    Providers := TList<IServiceProvider>.Create;
    try
      if assigned( FInjectionOverride) then
        Providers.Add( FInjectionOverride);
      if assigned( Provider) then
        Providers.Add( Provider);
      SelectedProvider := Provider;
      isIInterfaceList := IsEqualGUID( TRttiInterfaceType( MemberType1).GUID, IInterfaceList);
      isMarkedCollection := IsMemberDecorated( Member, Collection, CollectionAttri);
      if isMarkedCollection then
          sColName := (CollectionAttri as Collection).FName
        else
          sColName := '';
      AvailableCount := 0;
      for Provider1 in Providers do
        begin
        if not isMarkedCollection then
            AvailableCount := Provider1.GetGroupCount( InjectableGUID, sInjectionConfig)
          else
            begin
            for Pair in (Provider1 as IServiceProviderInternal).GetServices do
              begin
              if (Pair.Key.FConfig = sInjectionConfig) and
                 (Pair.Value.FAffinity = afCooperative) and
                 (Pair.Value.FCollectionName = sColName) and
                 (Pair.Value.FMembers.Count > 0) and
                 (assigned( Pair.Value.FMakeCollection)  or isIInterfaceList) then
                   begin
                   InjectableGUID := Pair.Key.FServiceIID;
                   Inc( AvailableCount, Pair.Value.FMembers.Count);
                   break
                   end
              end
            end;
        if AvailableCount = 0 then continue;
        SelectedProvider := Provider1;
        break
        end;
      if AvailableCount = 0 then
          begin
          doInject := IsEqualGUID( InjectableGUID, IInterface) and
                      (sInjectionConfig = sClientRef) and
                      assigned( FClient);
          if doInject then
            Inject := FClient
          end
        else
          doInject := (SelectedProvider as IServiceProviderInternal).Internal_Acquire(
            InjectableGUID, FInjectionsClient, Inject, sInjectionConfig, nil, False)
    finally
      Providers.Free
      end;
    if doInject then
      begin
      TValue.Make( @Inject, Member.MemberType.Handle, InjectAsRttiValue);
      Member.SetValue( InjectAsRttiValue)
      end
    end;
  end
end;


function TDataMemberInjector.GetMembers: TEnumerable<IInjectableMember>;
begin
if not Assigned( FMembers) then
  FMembers := TFieldEnumerable.Create( self);
result := FMembers
end;



function TWritablePropertyInjector.GetMembers: TEnumerable<IInjectableMember>;
begin
if not Assigned( FMembers) then
  FMembers := TPropEnumerable.Create( self);
result := FMembers
end;



constructor TInjectableMember.Create( Owner1: TSuiteInjector);
begin
FOwner := Owner1
end;


function TInjectableDataMember.AsRttiMember: TRttiMember;
begin
result := FMember
end;

constructor TInjectableDataMember.Create(
  Owner1: TSuiteInjector; Member1: TRttiField);
begin
inherited Create( Owner1);
FMember := Member1
end;


function TInjectableDataMember.IsInjectable: boolean;
begin
result := True
end;


function TInjectableDataMember.MemberType: TRttiType;
begin
result := FMember.FieldType
end;


procedure TInjectableDataMember.SetValue( const Inject: TValue);
begin
FMember.SetValue( FOwner.FInjectionSubject, Inject)
end;


function TInjectablePropMember.AsRttiMember: TRttiMember;
begin
result := FMember
end;

constructor TInjectablePropMember.Create(
  Owner1: TSuiteInjector; Member1: TRttiProperty);
begin
inherited Create( Owner1);
FMember := Member1
end;

function TInjectablePropMember.IsInjectable: boolean;
begin
result := FMember.IsWritable and (FMember is TRttiInstanceProperty)
end;

function TInjectablePropMember.MemberType: TRttiType;
begin
result := FMember.PropertyType
end;

procedure TInjectablePropMember.SetValue( const Inject: TValue);
begin
FMember.SetValue( FOwner.FInjectionSubject, Inject)
end;

{ TMemberEnumable<MemberT> }

constructor TMemberEnumable<MemberT>.Create( Suite1: TSuiteInjector);
begin
FSuite := Suite1
end;


constructor TPropEnumerable.Create( Suite1: TSuiteInjector);
var
  A: TArray<TRttiProperty>;  // Work-around D1020-with-delphi bug
begin
inherited Create( Suite1);
A        := FSuite.FServiceType.GetProperties;
FMembers := A
end;

function TPropEnumerable.DoGetEnumerator: TEnumerator<IInjectableMember>;
begin
result := TPropEnumator.Create( self)
end;


constructor TFieldEnumerable.Create( Suite1: TSuiteInjector);
var
  A: TArray<TRttiField>; // Needed to work-around D2010-with-packages bug.
begin
inherited Create( Suite1);
A        := FSuite.FServiceType.GetFields;
FMembers := A
end;

function TFieldEnumerable.DoGetEnumerator: TEnumerator<IInjectableMember>;
begin
result := TFieldEnumator.Create( self)
end;


constructor TMemberEnumator<MemberT>.Create( Owner1: TMemberEnumable<MemberT>);
begin
FOwner := Owner1;
FIndex := -1
end;

function TMemberEnumator<MemberT>.DoMoveNext: Boolean;
begin
result := FIndex < Length( FOwner.FMembers);
if not result then exit;
inc( FIndex);
result := FIndex < Length( FOwner.FMembers)
end;


constructor TPropEnumator.Create( Owner1: TMemberEnumable<TRttiProperty>);
begin
inherited Create( Owner1)
end;

function TPropEnumator.DoGetCurrent: IInjectableMember;
begin
result := TInjectablePropMember.Create( FOwner.FSuite, FOwner.FMembers[FIndex])
end;


constructor TFieldEnumator.Create(Owner1: TMemberEnumable<TRttiField>);
begin
inherited Create( Owner1)
end;

function TFieldEnumator.DoGetCurrent: IInjectableMember;
begin
result := TInjectableDataMember.Create( FOwner.FSuite, FOwner.FMembers[FIndex])
end;


function TServiceEqualityComparer.
  Key_Equals( const Left, Right: RServiceKey): boolean;
begin
result := IsEqualGUID( Left.FServiceIID, Right.FServiceIID) and
          (Left.FConfig = Right.FConfig)
end;


{$IF CompilerVersion < 24}
function Low( const s: string): integer; inline;
begin
result := 1;
end;
{$IFEND}

function TServiceEqualityComparer.Key_GetHashCode(
  const Value: RServiceKey): integer;
var
  L: integer;
begin
result := BobJenkinsHash( Value.FServiceIID, SizeOf( Value.FServiceIID), 0);
L := Length( Value.FConfig);
if L > 0 then
  result := BobJenkinsHash( Value.FConfig[ Low( Value.FConfig)], L * SizeOf( Char), result)
end;

end.
