unit uLockBox_HugeCardinalTestCases;
interface
uses TestFramework, uTPLb_Hash, uTPLb_CryptographicLibrary, Classes,
     uTPLb_Codec, uTPLb_StreamCipher, uTPLb_HugeCardinal,
     uTPLb_MemoryStreamPool;


type
THugeCardinal_TestCase = class( TTestCase)
  protected
    Fbig1234: THugeCardinal;
    Fbig2313: THugeCardinal;
    Fbig3547: THugeCardinal;
    Temp1, Temp2, Temp3, Temp4: THugeCardinal;
    Temp2000_1: THugeCardinal;
    Temp2000_2: THugeCardinal;
    T3, F100: THugeCardinal;
    TmpStream: TMemoryStream;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_CreateZero;
    procedure CreateRandom;
    procedure CreateSmall;
    procedure Test_Clone;
    procedure Test_Assign;
    procedure Test_Zeroise;
    procedure Test_CompareSmall;
    procedure Test_Compare;
    procedure Test_AssignSmall;
    procedure Test_BitLength;
    procedure Test_MaxBits;
    procedure Test_Add;
    procedure Test_Increment;
    procedure Test_Subtract;
    procedure Test_MulPower2;
    procedure Test_MulSmall;
    procedure Test_Multiply;
    procedure Test_Modulo;
    procedure Test_AddMod;
    procedure Test_MultiplyMod;
    procedure Test_isOdd;
    procedure Test_CreateFromStreamIn;
    procedure Test_CloneSized;
    procedure Test_Resize;
    procedure Test_AssignFromStreamIn;
    procedure Test_Swap;
    procedure Test_ExtactSmall;
    procedure Test_StreamOut;
    procedure Test_PowerMod;
    procedure Test_SmallExponent_PowerMod;
  end;

implementation







uses SysUtils;




procedure InitUnit_HugeCardinalTestCases;
begin
TestFramework.RegisterTest( THugeCardinal_TestCase.Suite)
end;

procedure DoneUnit_HugeCardinalTestCases;
begin
end;


{ THugeCardinal_TestCase }

procedure THugeCardinal_TestCase.SetUp;
var
  j: integer;
begin
Temp1 := THugeCardinal.CreateSmall( 1234, 100, nil);
Temp2 := THugeCardinal.CreateSmall( 10000, 100, nil);
Temp3 := THugeCardinal.CreateSmall( 2313, 100, nil);
Temp4 := THugeCardinal.CreateSmall( 3547, 100, nil);
Fbig1234 := THugeCardinal.CreateZero( 2000, nil);
Fbig2313 := THugeCardinal.CreateZero( 2000, nil);
Fbig3547 := THugeCardinal.CreateZero( 2000, nil);
Temp2000_1 := THugeCardinal.CreateZero( 2000, nil);
Temp2000_2 := THugeCardinal.CreateZero( 2000, nil);
for j := 1 to 16 do
  begin
  T3 := Fbig1234.Multiply( Temp2);
  FBig1234.Assign( T3);
  FreeAndNil( T3);
  Fbig1234.Add     ( Temp1);

  T3 := Fbig2313.Multiply( Temp2);
  Fbig2313.Assign( T3);
  FreeAndNil( T3);
  Fbig2313.Add     ( Temp3);

  T3 := Fbig3547.Multiply( Temp2);
  Fbig3547.Assign( T3);
  FreeAndNil( T3);
  Fbig3547.Add     ( Temp4);
  end;
F100 := THugeCardinal.CreateSmall( 100, 100, nil);
TmpStream := TMemoryStream.Create;
end;



procedure THugeCardinal_TestCase.TearDown;
begin
FreeAndNil( Temp1);
FreeAndNil( Temp2);
FreeAndNil( Temp2000_1);
FreeAndNil( Temp2000_2);
FreeAndNil( FBig1234);
FreeAndNil( Fbig2313);
FreeAndNil( Fbig3547);
FreeAndNil( Temp3);
FreeAndNil( Temp4);
FreeAndNil( T3);
FreeAndNil( F100);
FreeAndNil( TmpStream)
end;




procedure THugeCardinal_TestCase.CreateRandom;
var
  Rand: THugeCardinal;
begin
// Create a big random and make sure bits is not more than the specified length.
Rand := THugeCardinal.CreateRandom( 201, 300, False, nil);
try
  Check( (Rand.BitLength <= 201) and
         (Rand.BitLength >= 100), 'HugeCardinal Create Random failed.')
finally
Rand.Free
end end;



procedure THugeCardinal_TestCase.CreateSmall;
begin
// Create 100 and check it is 100
Check( F100.isSmall and (F100.ExtractSmall = 100),
  'HugeCardinal CreateSmall failed.')
end;



procedure THugeCardinal_TestCase.Test_Add;
begin
//Check that ...
//1234123412341234123412341234123412341234123412341234123412341234 +
//2313231323132313231323132313231323132313231323132313231323132313 =
//----------------------------------------------------------------
//3547354735473547354735473547354735473547354735473547354735473547
//Construct these numbers by lots of small adds and multiplies (by 10).
Temp2000_1.Assign( Fbig1234);
Temp2000_1.Add( Fbig2313);
Check( Temp2000_1.Compare( Fbig3547) = rEqualTo, 'HugeCardinal addition failed.')
end;


procedure THugeCardinal_TestCase.Test_AddMod;
begin
//Check that ...
//1234123412341234123412341234123412341234123412341234123412341234 +
//2313231323132313231323132313231323132313231323132313231323132313 mod
//                                                             100 =
//----------------------------------------------------------------
//                                                              47
Temp2000_1.Assign( Fbig1234);
Temp2000_1.AddMod( Fbig2313, F100);
Check( Temp2000_1.isSmall and (Temp2000_1.ExtractSmall = 47),
  'HugeCardinal AddMod failed.')
end;


procedure THugeCardinal_TestCase.Test_Assign;
begin
// Start with a big random y.
// x := y
// check x = y
Temp2000_1.Assign( FBig1234);
Check( Temp2000_1.Compare( FBig1234) = rEqualTo,
  'Huge Cardinal Assign failed.')
end;



procedure THugeCardinal_TestCase.Test_AssignFromStreamIn;
begin
// Manually stream out x.
// Stream it back in.
// Check it is the same a sthe original
TmpStream.Clear;
FBig1234.StreamOut( LittleEndien, TmpStream);
FreeAndNil( Temp2000_1);
TmpStream.Position := 0;
Temp2000_1 := THugeCardinal.CreateFromStreamIn( FBig1234.MaxBits, LittleEndien, TmpStream, nil);
FreeAndNil( TmpStream);
Check( Temp2000_1.Compare( Fbig1234) = rEqualTo, 'HugeCardinal AssignFromStreamIn failed.')
end;


procedure THugeCardinal_TestCase.Test_AssignSmall;
begin
// x := 10000;
// check x = 10000
Temp2000_1.AssignSmall( 10000);
Check( Temp2000_1.isSmall and (Temp2000_1.ExtractSmall = 10000),
  'HugeCardinal AssignSmall failed.')
end;


procedure THugeCardinal_TestCase.Test_BitLength;
var
  bl: integer;
begin
// check zero. bitlength = 0
// x := 1234123412341234123412341234123412341234123412341234123412341234;
// check (x shl 1).BL = x.BL + 1
// manually count bits in x by shl -1 for maxbits.
// check manual count = original BitLength property.
Temp2000_1.Zeroise;
Check( Temp2000_1.BitLength = 0, 'BitLength 0 failed.');
bl := FBig1234.BitLength;
FBig1234.MulPower2( 1);
Check( (FBig1234.BitLength) - bl = 1, 'BitLength failed.')
end;



procedure THugeCardinal_TestCase.Test_Clone;
begin
// x := 1234123412341234123412341234123412341234123412341234123412341234;
// y := clone x;
// check x = y
// check x.maxbits = y.maxbits
FreeAndNil( Temp2000_1);
Temp2000_1 := FBig1234.Clone;
Check( Temp2000_1.Compare( FBig1234) = rEqualTo, 'THugeCardinal.Clone failed.');
Check( FBig1234.Compare( Temp2000_1) = rEqualTo, 'THugeCardinal.Clone failed.');
Check( Temp2000_1.MaxBits = Temp2000_1.MaxBits,  'THugeCardinal.Clone failed.')
end;



procedure THugeCardinal_TestCase.Test_CloneSized;
begin
// x := clone y, but with a different size.
// check x = y
// check size x is corrrect.
FreeAndNil( Temp2000_1);
Temp2000_1 := FBig1234.CloneSized( 3999);
Check( Temp2000_1.Compare( FBig1234) = rEqualTo, 'THugeCardinal.CloneSized failed.');
Check( FBig1234.Compare( Temp2000_1) = rEqualTo, 'THugeCardinal.CloneSized failed.');
Check( Temp2000_1.MaxBits = 4000,  'THugeCardinal.CloneSized failed.')
end;



procedure THugeCardinal_TestCase.Test_Compare;
begin
//Check that ...
//1234123412341234123412341234123412341234123412341234123412341234 compare to
//2313231323132313231323132313231323132313231323132313231323132313 =
//----------------------------------------------------------------
// less than.

//Check that ...
//2313231323132313231323132313231323132313231323132313231323132313 compare to
//1234123412341234123412341234123412341234123412341234123412341234 =
//----------------------------------------------------------------
// greater than.

//Check that ...
//2313231323132313231323132313231323132313231323132313231323132313 compare to
//2313231323132313231323132313231323132313231323132313231323132313 =
//----------------------------------------------------------------
// equals.
Check( FBig2313.Compare( FBig1234) = rGreaterThan, 'THugeCardinal.Compare failed.');
Check( FBig1234.Compare( FBig2313) = rLessThan   , 'THugeCardinal.Compare failed.');
Check( FBig2313.Compare( FBig2313) = rEqualTo    , 'THugeCardinal.Compare failed.')
end;



procedure THugeCardinal_TestCase.Test_CompareSmall;
begin
// x := 999
// compare x = 999
// compare x = 665
Temp2000_1.AssignSmall( 999);
Check( Temp2000_1.isSmall and (Temp2000_1.ExtractSmall =  999), 'THugeCardinal.CompareSmall failed.');
Check( Temp2000_1.isSmall and (Temp2000_1.ExtractSmall <> 665), 'THugeCardinal.CompareSmall failed.')
end;



procedure THugeCardinal_TestCase.Test_CreateZero;
begin
// x := 0
// compare x = 0
FreeAndNil( Temp2000_1);
Temp2000_1 := THugeCardinal.CreateZero( 10, nil);
Check( Temp2000_1.isZero, 'THugeCardinal.CreateZero failed.');
end;

procedure THugeCardinal_TestCase.Test_ExtactSmall;
begin
// x is big.
// y := x + 44 - x
// check y = 44
Temp2000_1.Assign( FBig1234);
Temp2000_1.Increment( 44);
Temp2000_1.Subtract( FBig1234);
Check( Temp2000_1.isSmall and (Temp2000_1.ExtractSmall = 44),
  'THugeCardinal.ExtactSmall/isSmall failed.')
end;

procedure THugeCardinal_TestCase.Test_Increment;
var
  v: uint64;
begin
//Check that ...
// $FFFFFFFFFFFFFFFFFF +  // 8 bytes of $FF
//                  1 =
//---------------------
//$1000000000000000000  // 9 bytes
int64rec( v).Lo := $FFFFFFFF;
int64rec( v).Hi := $FFFFFFFF;
Temp2000_1.AssignSmall( v);
Temp2000_1.Increment( 1);
Temp2000_2.AssignSmall( 1);
Temp2000_2.MulPower2( 64);
Check( Temp2000_1.Compare( Temp2000_2) = rEqualTo,
  'THugeCardinal Increment failed.')
end;


procedure THugeCardinal_TestCase.Test_isOdd;
begin
//Check that ...
//1234123412341234123412341234123412341234123412341234123412341234 is even.

// Check that ...
//2313231323132313231323132313231323132313231323132313231323132313 is odd.

// Check that 0 is even.
Check( not FBig1234.isOdd, 'THugeCardinal IsOdd failed.');
FBig1234.Increment( -1);
Check( FBig1234.isOdd, 'THugeCardinal IsOdd failed.');
FBig1234.Zeroise;
Check( not FBig1234.isOdd, 'THugeCardinal IsOdd failed.');
end;


procedure THugeCardinal_TestCase.Test_MaxBits;
begin
// create x with 5 bits.
// check x.maxbits = 64
// create x with 100 bits.
// check x.maxbits = 104
FreeAndNil( Temp2000_1);
Temp2000_1 := THugeCardinal.CreateZero( 5, nil);
Check( Temp2000_1.MaxBits = 64, 'MaxBits failed.');
FreeAndNil( Temp2000_1);
Temp2000_1 := THugeCardinal.CreateZero( 100, nil);
Check( Temp2000_1.MaxBits = 104, 'MaxBits failed.')
end;



procedure THugeCardinal_TestCase.Test_Modulo;
begin
//Check that ...
//1234123412341234123412341234123412341234123412341234123412341234 mod
//                                                            1000 =
//----------------------------------------------------------------
//                                                             234
Temp2000_1.Assign( FBig1234);
Temp2000_2.AssignSmall( 1000);
FreeAndNil( T3);
T3 := Temp2000_1.Modulo( Temp2000_2);
Check( T3.isSmall and (T3.ExtractSmall = 234),
  'THugeCardinal Modulo failed.')
end;



procedure THugeCardinal_TestCase.Test_MulPower2;
begin
// Check that x + x = x shl 1
// Check that x shl 88 shl -88 = x
Temp2000_1.Assign( FBig1234);
Temp2000_1.Add   ( FBig1234);
Temp2000_2.Assign( FBig1234);
Temp2000_2.MulPower2( 1);
Check( Temp2000_1.Compare( Temp2000_2) = rEqualTo,
  'THugeCardinal MulPower2 failed.');

Temp2000_1.Assign( FBig1234);
Temp2000_1.MulPower2(  88);
Temp2000_1.MulPower2( -88);
Check( Temp2000_1.Compare( FBig1234) = rEqualTo,
  'THugeCardinal MulPower2 failed.');
end;



procedure THugeCardinal_TestCase.Test_MulSmall;
begin
// check that x + x + x = 3 * x
Temp2000_1.Assign( FBig1234);
Temp2000_1.Add   ( FBig1234);
Temp2000_1.Add   ( FBig1234);
Temp2000_2.Assign( FBig1234);
Temp2000_2.MulSmall( 3);
Check( Temp2000_1.Compare( Temp2000_2) = rEqualTo,
  'THugeCardinal MulSmall failed.');
end;

procedure THugeCardinal_TestCase.Test_Subtract;
begin
// check x + y - y = x
Temp2000_1.Assign( FBig1234);
Temp2000_1.Add     ( FBig2313);
Temp2000_1.Subtract( FBig2313);
Check( Temp2000_1.Compare( FBig1234) = rEqualTo,
  'THugeCardinal Subtract failed.');
end;


procedure THugeCardinal_TestCase.Test_Zeroise;
begin
// x := 1234123412341234123412341234123412341234123412341234123412341234;
// zeroise x
// check that x = 0
Temp2000_1.Assign( FBig1234);
Temp2000_1.Zeroise;
Check( Temp2000_1.IsZero,'THugeCardinal Zeroise failed.');
end;








procedure THugeCardinal_TestCase.Test_Swap;
begin
// x and y big values.
// swap x & y
// check x = original y
// check x.maxbits = original y.maxbits
// check y = original x
// check y.maxbits = original x.maxbits

end;

procedure THugeCardinal_TestCase.Test_CreateFromStreamIn;
begin
// Manually stream out x.
// Stream it back into constructor.
// Check it is the same a sthe original

end;

procedure THugeCardinal_TestCase.Test_Multiply;
begin
// Check that 10^33 * 10^44 = 10^77

end;

procedure THugeCardinal_TestCase.Test_MultiplyMod;
begin
//Check that ...
//1234123412341234123412341234123412341234123412341234123412341234 *
//2313231323132313231323132313231323132313231323132313231323132313 mod
//                                                             100 =
//----------------------------------------------------------------
//                                                              36

end;

procedure THugeCardinal_TestCase.Test_PowerMod;
begin
// TBD

end;

procedure THugeCardinal_TestCase.Test_Resize;
begin
// x create as 100 bits. random.
// resize to 200 bits.
// check x.size = 200
// check x = original value.

end;


procedure THugeCardinal_TestCase.Test_SmallExponent_PowerMod;
begin
// TBD

end;


procedure THugeCardinal_TestCase.Test_StreamOut;
begin
// x big.
// stream out.
// stream in.
// check x = original value

end;


initialization
InitUnit_HugeCardinalTestCases;


finalization
DoneUnit_HugeCardinalTestCases;

end.
