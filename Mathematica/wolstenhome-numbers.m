(* rosettacode URL: https://rosettacode.org/wiki/Wolstenholme_numbers#Mathematica_/_Wolfram_Language *)

ClearAll[wolstenhomeNumber, primeWolstenhomeNumber, indexOfNextPrimeWolstenhomeNumber, ns, first15PrimeWolstenhomeNumbers, digitCount];

wolstenhomeNumber[n_Integer?Positive] := Numerator@HarmonicNumber[n, 2];
primeWolstenhomeNumber[n_Integer?Positive] := wolstenhomeNumber@Nest[indexOfNextPrimeWolstenhomeNumber, 1, n];

indexOfNextPrimeWolstenhomeNumber[n_Integer?Positive] := NestWhile[# + 1 &, n + 1, ! (PrimeQ@wolstenhomeNumber@#) &];

digitCount[n_Integer] := Length@IntegerDigits@n;

SetAttributes[{wolstenhomeNumber, indexOfNextPrimeWolstenhomeNumber, digitCount}, Listable];

ns = {500, 1000, 2500, 5000, 10000};
first15PrimeWolstenhomeNumbers = wolstenhomeNumber@NestList[indexOfNextPrimeWolstenhomeNumber, 2, 14];

Print["The first 20 Wolstenhome numbers are: ", wolstenhomeNumber[Range[20]], "."];
Print["The first four prime Wolstenhome numbers are: ", first15PrimeWolstenhomeNumbers[[1 ;; 4]], "."];
Print["The ", First@#, "th Wolstenhome number has ", Last@#, " digits."] & /@ Transpose[{ns, digitCount@wolstenhomeNumber@ns}];
Print["Digit counts of the first n prime Wolstenhome numbers:"];
Print[
    TableForm[Transpose[{Range@Length@first15PrimeWolstenhomeNumbers, digitCount@first15PrimeWolstenhomeNumbers}],
    TableHeadings -> {None, {"n", "# of digits"}}]
];
