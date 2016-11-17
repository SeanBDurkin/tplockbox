unit DunitM.CommandLine.Console.Solution;
interface
uses DUnitM.CommandLine.General, Types, XMLIntf, Generics.Collections, Classes;


type
TConsoleCommandLineProtocol = class( TInterfacedObject, IApplicationSpecificCommandLineFactory)
  private
    function Make(
      const RawForm: IPrimitiveCommandLine;
      const ProtocolReader: ICommandLineFactory;
      var   CommandLine: ICommandLine;
      var   Errors: TCommandLineReadErrorSet)
          : boolean;
  end;


implementation






uses SBD.Utils.XML2;

function TConsoleCommandLineProtocol.Make(
  const RawForm: IPrimitiveCommandLine;
  const ProtocolReader: ICommandLineFactory; var CommandLine: ICommandLine;
  var Errors: TCommandLineReadErrorSet): boolean;
begin
result := ProtocolReader.Make(
  RawForm,
  'DocParamName',     // TODO
  'NamespaceDecls',   // TODO
  CommandLine, Errors,
  procedure( const Registrar: IParameterRegistrar)
    begin
    // TODO

    end,

  function( const Doc: IXMLDocument; NameValues: TStrings): boolean
    begin
    // TODO
    end,
  nil)
end;

end.
