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

unit DUnitM.XMLWriter;
interface


uses Classes;

type
IXMLWriter_Element = interface;
IXMLWriter_NodeParent = interface
  ['{14A30196-F7B1-4D37-9C7A-BDA9B520CB96}']
    function  Close: IXMLWriter_NodeParent;
    function  AddElement( const URI, LocalName: string): IXMLWriter_Element;
    procedure AddProcessingInstruction( const Target, Content: string);
    procedure AddComment( const Content: string);
    procedure AddTextNode( const Value: string);
  end;

IXMLWriter_Element = interface( IXMLWriter_NodeParent)
  ['{5F7435DA-A642-4B3D-AD6A-4ED8799DDA2D}']
    procedure DeclareNamespace( const PreferredPrefix, URI: string);
    procedure UnDeclareNamespace( const URI: string);
    procedure DeclareDefaultNamespace( const URI: string);
    procedure UnDeclareDefaultNamespace;
    procedure AddAttribute( const Namespace, LocalName, Value: string);
    function  ElementStack: string;
  end;

IXMLWriter_Document = interface( IXMLWriter_NodeParent)
  ['{EF843DAD-8609-46A8-A77D-B78D3D0F4BAE}']
  end;

IXMLWriter = interface
  ['{D51A54E2-30C5-417A-B896-A91BFD267DAE}']
    function OpenDocument( OutStream1: TStream): IXMLWriter_Document;
  end;



implementation

end.
