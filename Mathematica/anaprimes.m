(* Rosettacode-urls: https://rosettacode.org/wiki/Anaprimes#Mathematica_/_Wolfram_Language *)

ClearAll[nDigitPrimes, anaprimes, output, maximalEquivalenceClasses];

nDigitPrimes[n_Integer?Positive] := Prime@Range[PrimePi[10^(n - 1) + 1], PrimePi[10^n]];

anaprimes[n_Integer?Positive] := GatherBy[nDigitPrimes[n], Sort[IntegerDigits[#]] &];
SetAttributes[anaprimes, Listable];

(*Produce output*)

maximalEquivalenceClasses = MaximalBy[Length] /@ anaprimes[Range[9]];

output[n_Integer?Positive] :=
    Module[{class = maximalEquivalenceClasses[[n]]},
           Print["Largest group(s) of ", n, "-digit anaprimes (", Length[First@class], " members):"];
           Print["First: ", First[class], " Last: ", Last[class]];
           Print[];
    ];

Map[output, Range[3, 9]];
