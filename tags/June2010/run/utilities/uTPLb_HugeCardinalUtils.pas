{* ***** BEGIN LICENSE BLOCK *****
Copyright 2010 Sean B. Durkin

This file is part of TurboPower LockBox.
TurboPower LockBox is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TurboPower LockBox is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the Lesser GNU General Public License
along with TurboPower LockBox.  If not, see <http://www.gnu.org/licenses/>.

The Initial Developer of the Original Code for TurboPower LockBox version 2
and earlier was TurboPower Software.

 * ***** END LICENSE BLOCK ***** *}
// Copyright 2010 Sean B. Durkin
unit uTPLb_HugeCardinalUtils;
interface
uses uTPLb_HugeCardinal, uTPLb_MemoryStreamPool;


type
TPrimalityTestNoticeProc = procedure( CountPrimalityTests: integer) of object;

function gcd( a, b: THugeCardinal): THugeCardinal;
// Computes the Greatest Common Divisor of a and b.
// A and b may be trashed as a result.
// The result is a new object.


function lcm( a, b: THugeCardinal): THugeCardinal;
// Computes the Least Common Multiple of a and b.
// A and b may be trashed as a result.
// The result is a new object.


function isCoPrime( a, b: THugeCardinal): boolean;
// Returns True if and only if a and b are co-prime.
// A and b may be trashed as a result.


function isProbablyPrime( p: THugeCardinal; OnProgress: TProgress; var wasAborted: boolean): boolean;
// True  means that p is almost certainly prime.
// False means that p is composite.
// The Fermat Primality test is used with only 1 pass.
// p is preserved.


function hasSmallFactor( p: THugeCardinal): boolean;
// True  means that p does not have a small factor.
//  It is a good candidate for testing for primality.
// False means that p is composite, and indeed, at least one of its
//  factors is a small number.


function GeneratePrime( NumBits: integer;
                        OnProgress: TProgress;
                        OnPrimalityTest: TPrimalityTestNoticeProc;
                        PassCount: integer;
                        Pool1: TMemoryStreamPool;
                        var Prime: THugeCardinal;
                        var NumbersTested: integer): boolean;
// Returns True if Prime was succesfully generated.
// Returns False if user aborted.
// On input, Prime is nil.
// Upon output, if user did not abort, Prime is a prime number
//  of NumBits bits. PassCount passes of the Fermat Primality test
//  are used to test primality of random candidates.


function Inverse( Prime, Modulus: THugeCardinal): THugeCardinal;
// Computes multiplicative inverse of Prime over modulus.
// Assumes:
//   Modulus >= 3
//   2 <= Prime < Modulus
//   'Prime' is a prime number.


const
  StandardExponent = 65537;

procedure Compute_RSA_Fundamentals_2Factors(
  RequiredBitLengthOfN: integer;
  Fixed_e: uint64; // Put as -1 to generate a random number.
                   //  A value of StandardExponent is recommended.
  var N, e, d, Totient: THugeCardinal;
  OnProgress: TProgress;
  OnPrimalityTest: TPrimalityTestNoticeProc;
  GeneratePrimePassCount: integer; // 1 .. 20;
  Pool1: TMemoryStreamPool;
  var NumbersTested: integer;
  var wasAborted: boolean);


function Validate_RSA_Fundamentals(
  var N, e, d, Totient: THugeCardinal): boolean;


var
  GreatestPassCount: integer = 0;  // Investigative tool used for tweeking the algorithm.
  RSA_FailCount: integer = 0;

implementation












uses SysUtils, Classes, uTPLb_PointerArithmetic, uTPLb_IntegerUtils;

const
  SmallPrimeSetCardinality = 200;
  EratosthenesSieveSize = 2000; // Must choose value such that:
    // SmallPrimeSetCardinality'th prime number < EratosthenesSieveSize

var
  SmallPrimes: array[ 0.. SmallPrimeSetCardinality-1] of integer;


procedure PreComputeSmallPrimes; forward;
// The above computes the set of small prime numbers.

procedure InitUnit_HugeCardinalUtils;
begin
FillChar( SmallPrimes, SizeOf( SmallPrimes), 0);
PreComputeSmallPrimes
end;


procedure DoneUnit_HugeCardinalUtils;
begin
end;


procedure PreComputeSmallPrimes;
//  Uses the sieve of Eratosthenes algorithm.
var
  p, j, k: integer;
  Numbers: TBits;
begin
p := 2;
Numbers := TBits.Create;
try
Numbers.Size := EratosthenesSieveSize;
// Numbers[j] = False means that j is possibly prime.
// Numbers[j] = True  means that j is composite.
for j := 0 to SmallPrimeSetCardinality-1 do
  begin
  // p is a prime.
  k := 2 * p;
  while k < EratosthenesSieveSize do
    begin
    Numbers[k] := True;
    Inc( k, p)
    end;
  SmallPrimes[j] := p;
  repeat
    Inc( p)
  until (p >= EratosthenesSieveSize) or (not Numbers[p]);
  if p >= EratosthenesSieveSize then
    raise Exception.CreateFmt(
      'EratosthenesSieveSize value (%d) too small for required number of ' +
      'pre-computed small primes (%d primes)',
      [EratosthenesSieveSize, SmallPrimeSetCardinality])
  end;
finally
Numbers.Free
end end;



function gcd( a, b: THugeCardinal): THugeCardinal;
// Uses Stein's algorithm.
// TO DO: Implement an alternate implementation using Euclid and
//  measure the performance difference.
var
  ResultBitShifts: integer;
  isA_Odd, isB_Odd: boolean;
begin
ResultBitShifts := 0;
result := nil;
repeat
  if a.isZero then
      result := b
    else if b.isZero then
      result := a
    else
      begin
      isA_Odd := a.isOdd;
      isB_Odd := b.isOdd;
      if (not isA_Odd) and (not isB_Odd) then
          begin
          Inc( ResultBitShifts);
          a.MulPower2( -1);  // a := a / 2;
          b.MulPower2( -1);  // b := b / 2;
          end

        else if isA_Odd and (not isB_odd) then
          b.MulPower2( -1)

        else if (not isA_Odd) and isB_odd then
          a.MulPower2( -1)

        else
          case a.Compare( b) of
            rGreaterThan:
              begin
              a.Subtract( b);
              a.MulPower2( -1)
              end;

            rEqualTo:
              result := a;

            rLessThan:
              begin
              b.Subtract( a);
              b.MulPower2( -1)
              end;
            end;
      end;
until Assigned( result);
result := result.CloneSized( result.BitLength + ResultBitShifts);
result.MulPower2( ResultBitShifts)
end;


function lcm( a, b: THugeCardinal): THugeCardinal;
var
  gcd1, Temp, Quotient, Remainder: THugeCardinal;
begin
Quotient  := nil;
Remainder := nil;
result := a.Multiply( b);
gcd1   := gcd( a, b);
try
if (not gcd1.isSmall) or (gcd1.ExtractSmall <> 1) then
  begin
  result.Divide( gcd1, Quotient, Remainder);
  Assert( Remainder.isZero, 'function lcm failed.');
  Temp     := result;
  result   := Quotient;
  Quotient := Temp
  end
finally
  gcd1.Free;
  Quotient.Free;
  Remainder.Free
end end;



function isCoPrime( a, b: THugeCardinal): boolean;
var
  gcd1: THugeCardinal;
begin
gcd1 := gcd( a, b);
result := gcd1.isSmall and (gcd1.ExtractSmall = 1);
gcd1.Free
end;


function isProbablyPrime( p: THugeCardinal; OnProgress: TProgress; var wasAborted: boolean): boolean;
var
  Witness: THugeCardinal;
  pMinusOne: THugeCardinal;
begin
pMinusOne  := nil;
wasAborted := False;
if p.isSmall and (p.ExtractSmall <= 3) then
  begin
  result := True;
  exit
  end;
Witness := nil;
try
repeat
  FreeAndNil( Witness);
  Witness := THugeCardinal.CreateZero( p.BitLength, p.FPool);
  Witness.Random( p);
until (not Witness.isSmall) or (Witness.ExtractSmall > 1);
// 2 <= Witness < p
pMinusOne := p.Clone;
try
pMinusOne.Increment( -1);
wasAborted := not Witness.PowerMod( pMinusOne, p, OnProgress);
// isPrime := (Witness ** (p-1) mod p) = 1
result := Witness.isSmall and (Witness.ExtractSmall = 1)
finally
Witness.Free
end
finally
pMinusOne.Free
end
end;




function hasSmallFactor( p: THugeCardinal): boolean;
var
  j, q: integer;
  Modulus, TestValue, TestValue2: THugeCardinal;
  doSmallTest: boolean;
  pValue: uint64;
begin
result := not p.isOdd;
if result then exit;
doSmallTest := p.isSmall;
if doSmallTest then
    pValue := p.ExtractSmall
  else
    pValue := 0;
Modulus   := THugeCardinal.CreateZero( 0, p.FPool);
TestValue := THugeCardinal.CreateZero( p.MaxBits, p.FPool);
try
  for j := 1 to SmallPrimeSetCardinality-1 do
    begin
    q := SmallPrimes[j];  // q is a small prime number.
    if doSmallTest and (q >= pValue) then break;
    Modulus.AssignSmall( q);
    TestValue.Assign( p);
    TestValue2 := TestValue.Modulo( Modulus);
    result := TestValue2.isZero;
    TestValue2.Free;
    if result then break
    end
finally
  Modulus.Free;
  TestValue.Free
  end
end;


function GeneratePrime( NumBits: integer;
                        OnProgress: TProgress;
                        OnPrimalityTest: TPrimalityTestNoticeProc;
                        PassCount: integer;
                        Pool1: TMemoryStreamPool;
                        var Prime: THugeCardinal;
                        var NumbersTested: integer): boolean;
// Returns True if Prime was succesfully generated.
// Returns False if user aborted.
const
  PrimeDeltas: array[ 0..7 ] of integer = (6, 4, 2, 4, 2, 2, 6, 2);
var
  Thirty, Temp, Temp2: THugeCardinal;
  Delta: int64;
  Idx, j: integer;
  FoundPrime: boolean;
  wasAborted: boolean;
begin
if PassCount <= 0 then
   PassCount := 1;
if PassCount > 20 then
   PassCount := 20;
// generate p1
Prime := THugeCardinal.CreateRandom( NumBits, NumBits + 2, True, Pool1);
Thirty := THugeCardinal.CreateSmall( 30, 0, Pool1);
Temp   := nil; Temp2 := nil;
try
  Temp  := Prime.Clone;
  Temp2 := Temp.Modulo( Thirty);
  Delta := 31 - Temp2.ExtractSmall;
finally
  Thirty.Free;
  Temp.Free; Temp2.Free
  end;
// p := p1 - (p1 mod 30) + 1;
Prime.Increment( Delta);
// So now, p mod 30 = 1

// i := 0;
Idx := 0;
result := True;
wasAborted := False;
repeat
  FoundPrime := not hasSmallFactor( Prime);
  if FoundPrime then
    for j := 1 to PassCount do
      begin
      FoundPrime := isProbablyPrime( Prime, OnProgress, wasAborted);
      result := not wasAborted;
      if result and (not FoundPrime) and (j > GreatestPassCount) then
        GreatestPassCount := j;
      if (not FoundPrime) or (not result) then break;
      end;
  Inc( NumbersTested);
  if (not wasAborted) and assigned( OnPrimalityTest) then
    OnPrimalityTest( NumbersTested);
  if FoundPrime or (not result) then break;
  // This candidate failed, so try the next likely one.
  //  p += (6,4,2,4,2,2,6,2)[i];
  Delta := PrimeDeltas[ Idx];
  Prime.Increment( Delta);
  if Idx >= 7 then
      Idx := 0
    else
      Inc( Idx)
until FoundPrime or (not result);
if not result then
  FreeAndNil( Prime)
end;


function Inverse( Prime, Modulus: THugeCardinal): THugeCardinal;
// Computes multiplicative inverse of Prime over modulus.
// Assumes:
//   Modulus >= 3
//   2 <= Prime < Modulus
//   'Prime' is a prime number.
var
  xPrev: THugeCardinal;
  xPrevPve: boolean;
  x: THugeCardinal;
  xPve: boolean;
  r: THugeCardinal;
  rPrev: THugeCardinal;
  div1: THugeCardinal;
  rem1: THugeCardinal;
  Delta: THugeCardinal;
  xNext: THugeCardinal;
  xNextPve: boolean;
  Bits: integer;
  Pool: TMemoryStreamPool;
  Cmp: TCompareResult;
begin
Bits := Modulus.BitLength;
Pool := Modulus.FPool;
xPrev := THugeCardinal.CreateZero( Bits, Pool);
xPrevPve := True;
x := THugeCardinal.CreateSmall( 1, Bits, Pool);
xPve := True;
r := Prime.CloneSized( Bits);
rPrev := Modulus.Clone;
div1  := nil;
rem1  := nil;
Delta := nil;
xNext := nil;
try
while (not r.isSmall) or (r.ExtractSmall > 1) do
  begin
  FreeAndNil( div1); FreeAndNil( rem1);
  rPrev.Divide( r, div1, rem1);
  rPrev.Assign( r);
  r.Assign( rem1);
  FreeAndNil( Delta);
  Delta := x.Multiply( div1);
  FreeAndNil( xNext);
  xNext := xPrev.Clone;
  if xPve = xPrevPve then
      begin
      Cmp := xPrev.Compare( Delta);
      if Cmp in [rGreaterThan, rEqualTo] then
          begin
          xNext.Subtract( Delta);
          xNextPve := xPve
          end
        else
          begin
          xNext.Assign( Delta);
          xNext.Subtract( xPrev);
          xNextPve := not xPve
          end;
      end
    else
      begin
      xNext.Add( Delta);
      xNextPve := xPrevPve
      end;
  xPrev.Assign( x);
  xPrevPve := xPve;
  x.Assign( xNext);
  xPve := xNextPve
  end
finally
result := x;
xPrev.Free;
r.Free;
rPrev.Free;
div1.Free;
rem1.Free;
Delta.Free;
xNext.Free;
end end;



function isAllSmallFactors( Number: THugeCardinal): boolean;
var
  Rem: THugeCardinal;
  j, SmallPrime: integer;
  Divisor, Quotient, Remainder: THugeCardinal;
begin
Rem := Number.Clone;
while (not Rem.isZero) and (not Rem.isOdd) do
  Rem.MulPower2( -1);
Divisor := THugeCardinal.CreateZero( 0, Number.FPool);
Quotient := nil;
Remainder := nil;
try
for j := Low( SmallPrimes) + 1 to High( SmallPrimes) do
  begin
  SmallPrime := SmallPrimes[j];
  if (SmallPrime = 0) or
     (Rem.CompareSmall( SmallPrime) in [rEqualTo, rLessThan]) then break;
  Divisor.AssignSmall( SmallPrime);
  repeat
    FreeAndNil( Quotient);
    FreeAndNil( Remainder);
    Rem.Divide( Divisor, Quotient, Remainder);
    if Remainder.isZero then
      Rem.Assign( Quotient);
  until (not Remainder.isZero) or (Rem.Compare( Divisor) in [rEqualTo, rLessThan])
  end;
result := Rem.BitLength <= 16;
finally
Rem.Free;
Divisor.Free;
Quotient.Free;
Remainder.Free
end end;



procedure Compute_RSA_Fundamentals_2Factors(
  RequiredBitLengthOfN: integer;
  Fixed_e: uint64; // Put as -1 to generate a random number.
                   //  A value of StandardExponent is recommended.
  var N, e, d, Totient: THugeCardinal;
  OnProgress: TProgress;
  OnPrimalityTest: TPrimalityTestNoticeProc;
  GeneratePrimePassCount: integer; // 1 .. 20; 20 is good.
  Pool1: TMemoryStreamPool;
  var NumbersTested: integer;
  var wasAborted: boolean);
// Refer: http://en.wikipedia.org/wiki/Rsa#Key_generation_2
// This could easily be adapted to multiple factors, but for the
//  time being, two will do.
// At the moment CRT exponents and co-efficients are not returned.
// In future, speed improvements could be made to Signature and Descryption
//  operations by leveraging the Chinese Remainder Theroum (CRT).

  procedure CopyHugeCard( var Dest: THugeCardinal; Source: THugeCardinal);
  begin
  if assigned( Dest) and (Dest.MaxBits >= Source.BitLength) then
      Dest.Assign( Source)
    else
      begin
      FreeAndNil( Dest);
      Dest := Source.Clone
      end
  end;

var
  p_bits, q_bits: integer;
  p, q, pMinus1, qMinus1: THugeCardinal;
  Dummy: integer;
  e_TrashCopy, Totient_TrashCopy: THugeCardinal;
  Ok: boolean;

begin
// Algorithm to generate RSA fundamental numbers:
// 1.  Compute p bits        p.bits := n.bits div 2 + 2;
// 2.  Compute q bits        q.bits := n.bits div 2 - 2;
// 3.  Compute p & p-1       p := Random Prime with exactly p.bits bits.
// 4.  Test for Pollard attack on p   Check (p-1) is not ONLY small factors.
// 5.  Compute q & q-1       q := Random Prime with exactly q.bits bits.
// 6.  Test for Pollard attack on q   Check (q-1) is not ONLY small factors.
// 7.  Compute Totient (Carmichael function) := lcm( p-1, q-1)
// 8.  Compute n := p * q;
// 9.  Test N.Bits not too small    n.bits should not be 16 or more, smaller than the target.
// 10. Compute e                    e :=  65537
// 10.5 Test that e and Totient are co-prime.
// 11. Compute d                    d := Inverse( e, Totient);
// 12. Test Wiener attack on d      Checks d.bits >= ((n.bits div 4) + 4)
// If any test fails, loop back to step 3 and retry.
// If user abort at any point, then break loop.


Assert( RequiredBitLengthOfN >= 32, 'RequiredBitLengthOfN too small.');
Assert( RequiredBitLengthOfN <= 32768, 'RequiredBitLengthOfN absurdly large.');
if Fixed_e <> -1 then
  Assert( (RequiredBitLengthOfN - BitCount_64( Fixed_e)) >= 16,
          'RequiredBitLengthOfN too small for n.');
p := nil;
q := nil;
pMinus1 := nil;
qMinus1 := nil;
wasAborted := False;
N := nil;
e := nil;
d := nil;
Totient := nil;
Dummy := 0;

// Steps 1 & 2;
q_bits := (RequiredBitLengthOfN div 2) - 2;
p_bits := RequiredBitLengthOfN - q_bits;

try try
repeat
  // Step 3
  FreeAndNil( p);
  wasAborted := not GeneratePrime(
    p_bits, OnProgress, OnPrimalityTest, GeneratePrimePassCount, Pool1,
    p, NumbersTested);
  if wasAborted then break;
  CopyHugeCard( pMinus1, p);
  pMinus1.Increment( -1);

  // Step 4 test: Test for Pollard attack on p
  if isAllSmallFactors( pMinus1) then
    continue;

  // Step 5
  FreeAndNil( q);
  wasAborted := not GeneratePrime(
    q_bits, OnProgress, OnPrimalityTest, GeneratePrimePassCount, Pool1,
    q, NumbersTested);
  if wasAborted then break;
  CopyHugeCard( qMinus1, q);
  qMinus1.Increment( -1);

  // Step 6 test: Test for Pollard attack on q
  if isAllSmallFactors( qMinus1) then
    continue;

  // Step 7.
  FreeAndNil( Totient);
  Totient := lcm( pMinus1, qMinus1);

  // Step 8.
  FreeAndNil( n);
  n := p.Multiply( q);

  // Step 9
  if (RequiredBitLengthOfN - n.BitLength) >= 16 then
    continue;

  // Step 10
  if not assigned( e) then
    begin
    if Fixed_e = -1 then
        // We require e and Totient to be co-prime.
        // e does not have to be prime, however, its just easier
        // to generate a small prime number.
        GeneratePrime( 18, nil, nil, 10, Pool1, e, Dummy)
      else
        e := THugeCardinal.CreateSmall( Fixed_e, 0, Pool1);
        // The popular choice for Fixed_e is 2**16+1
    end;

  // Step 10.5
  // For a specified choice of e, check that e is co-prime to the Totient.
  if Fixed_e <> -1 then
    begin
    e_TrashCopy       := e.Clone;
    Totient_TrashCopy := Totient.Clone;
    try
      Ok := isCoPrime( e_TrashCopy, Totient_TrashCopy);
      finally
      e_TrashCopy.Free;
      Totient_TrashCopy.Free;
      end;
    if not Ok then
      continue // Totient not compatible with e. Try another totient!
    end;

  // Step 11.
  FreeAndNil( d);
  d := Inverse( e, Totient);

  // 12. Test Wiener attack on d      Checks d.bits >= ((n.bits div 4) + 4)
  if d.BitLength < ((n.BitLength div 4) + 4) then
    continue;

  // The Sanity Clause.
  // Two more tests...
  //   Test:  e * d (mod Totient) == 1
  //   Test:  m**e**d = m    where m is a random number. 1 trial.
  // TO DO: Boost the number of inversion trials (2nd test).
  if not Validate_RSA_Fundamentals( n, e, d, Totient) then
    begin
    // This can happen if we get bad luck with False Primality Witnesses.
    // Some-what counter-intuitively, this event seems to be more frequent
    //  with lower key sizes.
    Inc( RSA_FailCount);
    continue
    end;

  break
until wasAborted
finally
  p.Free; q.Free; pMinus1.Free; qMinus1.Free
  end;
except
    begin
    FreeAndNil( N);
    FreeAndNil( e);
    FreeAndNil( d);
    FreeAndNil( Totient);
    raise
    end
  end
end;


function Validate_RSA_Fundamentals(
  var N, e, d, Totient: THugeCardinal): boolean;
var
  TestVector, Reconstruction: THugeCardinal;
  eCopy: THugeCardinal;
begin
if assigned( Totient) then
    begin
    // Test:  e * d (mod Totient) == 1
    ecopy := e.CloneSized( N.BitLength);
    ecopy.MultiplyMod( d, Totient);
    result := ecopy.isSmall and (ecopy.ExtractSmall = 1);
    FreeAndNil( ecopy)
    end
  else
    result := True;
if not result then exit;

// Test for: m**e**d = m
try
TestVector := THugeCardinal.CreateZero( N.BitLength, N.FPool);
try
TestVector.Random( N);
Reconstruction := TestVector.Clone;
try
Reconstruction.PowerMod( e, N, nil);
Reconstruction.PowerMod( d, N, nil);
result := TestVector.Compare( Reconstruction) = rEqualTo
finally
Reconstruction.Free
end
finally
TestVector.Free
end;
except
  result := False
end;
end;

initialization
InitUnit_HugeCardinalUtils;


finalization
DoneUnit_HugeCardinalUtils;

end.
