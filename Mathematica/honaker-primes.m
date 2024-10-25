(* rosettacode-url: https://rosettacode.org/wiki/Honaker_primes#Mathematica_/Wolfram_Language *)

ClearAll[HonakerPrimeQ, DigitSum, NextHonakerPrime, HonakerPrime];

DigitSum[n_Integer] := Total@IntegerDigits[n]; /; $Version < 14;

HonakerPrimeQ[p_Integer] := PrimeQ[p] && (DigitSum[p] == DigitSum@PrimePi[p]);

NextHonakerPrime[n_Integer?Positive] := NestWhile[NextPrime, n + 1, ! HonakerPrimeQ[#] &]

HonakerPrime[n_Integer?Positive] := Nest[NextHonakerPrime, 1, n];

SetAttributes[{HonakerPrimeQ, NextHonakerPrime, HonakerPrime}, Listable];

(* Output *)

first50HonakerPrimes = HonakerPrime[Range[50]];
honaker10k = HonakerPrime[10^4];

Print["The first 50 Honaker primes are:"];
Grid[
    Partition[
        Transpose[{Range[50], PrimePi[first50HonakerPrimes],first50HonakerPrimes}],
        UpTo[5]],
    Alignment -> Right]
Print["The 10,000 Honaker prime is ", {PrimePi[honaker10k], honaker10k}]
