unit SampleTests;
interface
uses DUnitM.UnitTestFramework, DUnitM.StringUtils, XMLIntf,
     DUnitM.DDExtensions
    {$ifdef MSWINDOWS}
     , ADODB
    {$endif}
    ;

type

// [TestFixture] identifies and names the test fixture class.
//  Use the '.' character to separate name path particles, and thus group your test fixtures.
[TestFixture('NewTests.Sprint5')]
[Description('Banana banana banana')]
TSampleThreeFixture = class
  protected
    Assert: IAssert;    // Every test fixture class must have this data member.

  public
    // The [Test] annotation identifies a test procedure. Without a [TestCase], one [TestCase] is implied.
    // The boolean parameter to [Test] is the default value of its selection status in the suite of test cases.
    [Test(False)]
    [Description('Banana banana')]
     procedure TestOne;

    // Use [TestCase] to identify one or more test cases within a test procedure.
    // The first parameter to [TestCase] is the name of the test case.
    // The second parameter is a space-separated list of actual parameter values to apply to the test procedure.
    //  Only the following paramter types are supported:
    //     integer
    //     string
    //     float
    //     boolean
    //     any enumerated type
    //     TDateTime
    //  Formal parameters must be pass-by-value, and string parameters must be declared with const.
    //  Actual DateTime literals must be presented in [TestCase] in standard xml format, as shown below.
    [Test]
    [TestCase('Test 2a','IntegerParam="1"')]
    [TestCase('Test 2b','IntegerParam="77"')]
     procedure TestTwo( IntegerParam: integer);

    [Test]
    [Ignore('For some reason.')]
     procedure IgnoreMe;

    [Test]
    [TestCase('TripleTest - pie case','A="Blueberry pie" B="2014-01-21T14:00:00"')]
    [TestCase('TripleTest - 1<2 case','A="1 is < 2"   B="2014-01-21T14:00:00"')]
    [Repeats(3)]
     procedure DoThreeTimes( const A: string; B: TDateTime);

    [Test]
    [IgnoreMemoryLeaks]
     procedure Leaky;

//    // 1. Data-driven testing - general case
//    [DataDrivenTest]
//    [Description('Tests the StringToDate() function')]
//      procedure TestLineItem( LineItemObject: TObject);
//      function  Navigator: IDataDrivenTestCaseNavigator;
//
//    // 2. Data-driven testing using an ADO table
//    {$ifdef MSWINDOWS}
//    [DataDrivenTest]
//      [DUnitM.DDExtensions.ADO('ADO-connection-string','TabName')]
//        procedure TestIt2( DataSource: TADOTable);
//        function  TestIt2FileName: string;
//    {$endif}
//
//    // 3. Data-driven testing using a MS Excel file via ADO
//    {$ifdef MSWINDOWS}
//    [DataDrivenTest]
//      [DUnitM.DDExtensions.Excel( 'StringToDate$' {$ifdef SUPPORTS_REGEX}, 'ConnectionStringOverrides' {$endif})]
//        procedure TestIt3( DataSource: TADOTable);
//        function  TestIt3FileName: string;
//    {$endif}
//
//    // 4. Data-driven testing using a Text file
//    [DataDrivenTest]
//      [DUnitM.DDExtensions.TextFile]
//        procedure TestIt4( const Line: string);
//        function  TestIt4FileName: string;
//
//    // 5. Data-driven testing using an XML file
//    {$ifdef MSWINDOWS}
//    [DataDrivenTest]
//      [DUnitM.DDExtensions.XML('m:test-data/m:test-case','xmlns:m="http://www.mlc.com.au/dunit-m/examples"')]
//        procedure TestIt5( const Node: IXMLNode);
//        function  TestIt5FileName: string;
//    {$endif}

    [Setup]    procedure DenotedSetup;
    [TearDown] procedure DenotedTearDown;
    [FixtureSetup] constructor Create;

    destructor Destroy; override;

  published
    procedure Setup;
    procedure TearDown;
  end;

implementation
















constructor TSampleThreeFixture.Create;
begin
end;

procedure TSampleThreeFixture.DenotedSetup;
begin
end;

procedure TSampleThreeFixture.DenotedTearDown;
begin
end;

destructor TSampleThreeFixture.Destroy;
begin
inherited;
end;

procedure TSampleThreeFixture.DoThreeTimes( const A: string; B: TDateTime);
begin                                   // A="Blueberry pie" B="2014-01-21T14:00:00"
Assert.AreEqual( A, 'Blueberry pie')
end;


procedure TSampleThreeFixture.IgnoreMe;
begin
Assert.Fail('I should be ignored')
end;

procedure TSampleThreeFixture.Leaky;
var
  P: ^integer;
begin
New( P);  // Deliberate leak
Assert.Pass
end;

//function TSampleThreeFixture.Navigator: IDataDrivenTestCaseNavigator;
//begin
//  // Create and return an IDataDrivenTestCaseNavigator into a cursor of an object collection.
//  // These object (representing line items to test) will be passed into the previous method
//  //  (which in this case is procedure TestLineItem()) for testing.
//end;
//
procedure TSampleThreeFixture.Setup;
begin
end;

procedure TSampleThreeFixture.TearDown;
begin
end;

//procedure TSampleThreeFixture.TestIt2( DataSource: TADOTable);
//begin
//
//end;
//
//function TSampleThreeFixture.TestIt2FileName: string;
//begin
//  // Return a file-name to be used in conjustion with the connection string parameter
//  //  of the [DUnitM.DDExtensions.ADO] attribute to specify a file-based ADO table.
//  // Each row in the table is a test case, for with the previous method (TestIt2) is called.
//end;

{$ifdef MSWINDOWS}
//procedure TSampleThreeFixture.TestIt3(DataSource: TADOTable);
//begin
//
//end;
//
//function TSampleThreeFixture.TestIt3FileName: string;
//begin
//  // Return a file-name of an xlsx file. The connection string can
//  // be overriden by the parameters of the [DUnitM.DDExtensions.Excel] attribute.
//  // Each row in the table is a test case, for with the previous method (TestIt3) is called.
//end;
{$endif}

//procedure TSampleThreeFixture.TestIt4( const Line: string);
//begin
//
//end;
//
//function TSampleThreeFixture.TestIt4FileName: string;
//begin
//  // Return a file-name of an text file (utf-8 encoding).
//  // Each line in the file is a test case, for with the previous method (TestIt4) is called.
//end;

{$ifdef MSWINDOWS}
//procedure TSampleThreeFixture.TestIt5( const Node: IXMLNode);
//begin
//
//end;
//
//function TSampleThreeFixture.TestIt5FileName: string;
//begin
//
//end;
{$endif}

//procedure TSampleThreeFixture.TestLineItem( LineItemObject: TObject);
//begin
//  // Return a file-name of an xml file.
//  // Each node in the given XPath 1.0 expression of the [DUnitM.DDExtensions.XML] attribute,
//  //  in the document is a test case, for with the previous method (TestIt5) is called.
//end;

procedure TSampleThreeFixture.TestOne;
begin
Assert.IsTrue( True, 'TestOne works')
end;

procedure TSampleThreeFixture.TestTwo( IntegerParam: integer);
begin
Assert.AreEqual( IntegerParam, 77)
end;

end.
