unit SBD.ServiceProvider;
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
uses Classes, Generics.Collections;

const
  sClientRef = 'client'; // Use: decorate an IInterface data member/property with [Injection('client')]
  SBD_DependencyInjectionFramework_Version = 4;

type
IServiceProvider = interface;
TServiceFactory = reference to function(
  const Config: string;
  const ServiceProvider: IServiceProvider): IInterface;

TServiceAffinity = (
  afcompetitive,     // A single service or an array of competing services with
                     //  the same interface and config. Select one, the 'active'
                     //  service, like radio buttons to be the one acquired.
  afCooperative);    // A gang of services with the same interface and config,
                     //  working co-operatively together to solve a shared problem.

TIntegers = array of integer;
// Warning: Support for Generics in D2010 is very flaky,
//  so use this instead of TArray<Integer>

TGenericServiceProvider = class
  strict private
    FController: TObject;
  public
    function Acquire<Svc:IInterface>( const Client: IInterface; out Intf: Svc; const Config: string = ''; const InjectionOverride: IServiceProvider = nil): boolean;

  public // Syntactically public but you should read as strict private
    constructor Create( Controller1: TObject);
    procedure   ShutDown;
  end;

TCooperativeCollectionFactory = reference to function( const Config: string): IInterface;
TAddToCollectionProc = reference to procedure( const Collection, Addend: IInterface);

IServiceProvider = interface
  ['{4E38BEF6-2768-46E3-8740-25B8C526C912}']
    {$REGION 'property accessors'}
    function  GetServiceAffinity( const ServiceIID: TGUID; const Config: string): TServiceAffinity;
    function  GetActiveArrayMemberCookie( const ServiceIID: TGUID; const Config: string): integer;
    procedure SetActiveArrayMemberByCookie( const ServiceIID: TGUID; const Config: string; Cookie: integer);
    function  GetActiveArrayMemberName( const ServiceIID: TGUID; const Config: string): string;
    procedure SetActiveArrayMemberByName( const ServiceIID: TGUID; const Config, Name: string);
    function  GetNameByCookie( Cookie: integer): string;
    function  GetIsPooled( const ServiceIID: TGUID; const Config: string): boolean;
    procedure SetIsPooled( const ServiceIID: TGUID; const Config: string; Value: boolean);
    function  Gn: TGenericServiceProvider;
    {$ENDREGION}

    // Lookup methods
    function GetGroupCount( const ServiceIID: TGUID; const Config: string): integer;
    function GetCookies( const ServiceIID: TGUID; const Config: string): TIntegers;
    function GetCookieByName( const ServiceIID: TGUID; const Config, Name: string): integer;
    function GetCookieOfLiveService( const ServiceIID: TGUID; const Service: IInterface; const Config: string): integer;
    function GetCookiesOfServiceClass( Registrant: TClass): TIntegers;
    property  ActiveArrayMemberCookie[ const ServiceIID: TGUID; const Config: string]: integer
                    read GetActiveArrayMemberCookie write SetActiveArrayMemberByCookie;
    property  ActiveArrayMemberName[ const ServiceIID: TGUID; const Config: string]: string
                    read GetActiveArrayMemberName write SetActiveArrayMemberByName;
    property  NameByCookie[ Cookie: integer]: string   read GetNameByCookie;

    // Registration methods
    procedure SetCompetitiveAffinity( const ServiceIID: TGUID; const Config: string);
    procedure SetCooperativeAffinity(
      const ServiceIID: TGUID;
      const CollectionName: string;
      const Config: string;
      MakeCollection: TCooperativeCollectionFactory;
      AddToCollection: TAddToCollectionProc);

    function  RegisterLiveService( const ServiceIID: TGUID; const Service: IInterface;
                                   const Config: string = ''; const SelectionArrayMemberName: string = ''): integer;
    function  RegisterFlyweightService( const ServiceIID: TGUID; Factory: TServiceFactory;
                                        const Config: string = ''; const SelectionArrayMemberName: string = ''): integer;
    {$if CompilerVersion < 22}
    function RegisterServiceClass( const ServiceIID: TGUID; Registrant: TClass): integer;
    {$else}
    function RegisterServiceClass( const ServiceIID: TGUID; Registrant: TClass): TIntegers;
    // Due to a compiler bug we cannot use this ^ form in D2010, unless the client code
    //  declares a variable of type TIntegers. Scince this rule would be a bit too arbitary for
    //  users to consume, we resolve the issue by not using TIntegers in D2010.
    {$ifend}

    // Deregistration methods
    procedure DeregisterServiceByCookie( Cookie: integer);
    procedure DeregisterServicesByCookie( const Cookies: TIntegers);
    procedure DeregisterLiveServiceByName( const ServiceIID: TGUID; const Config: string = ''; const SelectionArrayMemberName: string = '');
    procedure DeregisterServicesOfClass( Registrant: TClass);

    // Acquisition methods
    function  Acquire( const ServiceIID: TGUID; const Client: IInterface; out Intf; const Config: string = ''; const InjectionOverride: IServiceProvider = nil): boolean;

    // Life-cycle methods
    function  Clone: IServiceProvider;
    procedure Merge( const Source: IServiceProvider); // Add Source's services to this.
    procedure ShutDown;

    // Main properties of service acquisition
    property  ServiceAffinity[ const ServiceIID: TGUID; const Config: string]: TServiceAffinity   read GetServiceAffinity;
    property  IsPooled[ const ServiceIID: TGUID; const Config: string]: boolean
                    read GetIsPooled write SetIsPooled;
  end;


TSBDUnit_BaseAttribute = class( TCustomAttribute) end;

Injection = class( TSBDUnit_BaseAttribute)
  public
    FConfig: string;

    constructor Create;                           overload;
    constructor Create( const Config1: string);   overload;
  end;

Configuration = class( TSBDUnit_BaseAttribute)
   public
     FConfigs: TStrings;
     FId: string;

     constructor Create;                             overload;
     constructor Create( const Config1: string);     overload;
     destructor Destroy; override;
   end;

BlacklistConfigs = class( TSBDUnit_BaseAttribute)
   public
     FConfigs: TStrings;

     constructor Create( const Config1: string);     overload;
     destructor Destroy; override;
   end;

Collection = class( TSBDUnit_BaseAttribute)
  public
    FName: string;

    constructor Create( const Name1: string);
  end;


function StandardServiceProvider: IServiceProvider;

implementation




uses TypInfo, SysUtils, SBD.ServiceProvider.Internal;
{ TGenericServiceProvider }




function TGenericServiceProvider.Acquire<Svc>(
  const Client: IInterface; out Intf: Svc; const Config: string;
  const InjectionOverride: IServiceProvider): boolean;
var
  Intnl: IServiceProviderInternal;
begin
result := Supports( FController, IServiceProviderInternal, Intnl) and
          Intnl.GnAcquire( GetTypeData(TypeInfo( Svc)).Guid,
            Client, Intf, Config, InjectionOverride)
end;


function CommaSplit( const CommaSeparedList: string): TStrings;
begin
result := TStringList.Create;
result.StrictDelimiter := True;
result.CommaText := Trim( CommaSeparedList)
end;


constructor Configuration.Create( const Config1: string);
const
  IdIntro = 'Id=';
var
  j: integer;
begin
FConfigs := CommaSplit( Config1);
for j := FConfigs.Count - 1 downto 0 do
  if SameText( Copy( Trim( FConfigs[j]), 1, Length( IdIntro)), IdIntro) then
    begin
    FId := Trim( FConfigs[j]);
    Delete( FId, 1, Length( IdIntro));
    FId := Trim( FId);
    FConfigs.Delete( j)
    end;
if FConfigs.Count = 0 then
  FConfigs.Add( '')
end;

constructor Configuration.Create;
begin
Create('')
end;


destructor Configuration.Destroy;
begin
FConfigs.Free;
inherited
end;


constructor TGenericServiceProvider.Create( Controller1: TObject);
begin
FController := Controller1
end;

procedure TGenericServiceProvider.ShutDown;
begin
FController := nil
end;

{ Injection }

constructor Injection.Create;
begin
FConfig := ''
end;

constructor Injection.Create( const Config1: string);
begin
FConfig := Config1
end;

{ BlacklistConfigs }

constructor BlacklistConfigs.Create( const Config1: string);
begin
FConfigs := CommaSplit( Config1)
end;

destructor BlacklistConfigs.Destroy;
begin
FConfigs.Free;
inherited
end;

function StandardServiceProvider: IServiceProvider;
begin
result := TServiceProvider.Create as IServiceProvider
end;



constructor Collection.Create( const Name1: string);
begin
FName := Name1
end;

end.
