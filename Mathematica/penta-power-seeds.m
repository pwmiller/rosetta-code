(* rosettacode-url: https://rosettacode.org/wiki/Penta-power_prime_seeds#Mathematica_/_Wolfram_Language *)

ClearAll[maybePentaPowerSeedQ, pentaPowerSeedQ, seeds, pentaPowerSeed, stats, commafied, printOutput];

maybePentaPowerSeedQ[n_Integer] :=  And[
                    PrimeQ[2 n + 1],
                    PrimeQ[n^2 + n + 1],
                    PrimeQ[n^3 + n + 1],
                    PrimeQ[n^4 + n + 1]
                  ];

pentaPowerSeedQ[n_Integer] := PrimeQ[n + 2] && maybePentaPowerSeedQ[n];

firstLargerThan[k_Integer, ns_List : seeds] :=
  Min@Select[ns, GreaterThan[k]];

seeds = {1};

pentaPowerSeed[n_Integer /; 1 <= n <= Length@seeds] := seeds[[n]];

pentaPowerSeed[n_Integer /; n > Length@seeds] := Module[{next},
   next = NextPrime[Last@seeds + 2] - 2;
   While[ Length@seeds < n,
    If[maybePentaPowerSeedQ[next],
     AppendTo[seeds, next];
     ];
    next = NextPrime[next + 2] - 2;
    ];
   Return[Last@seeds];
   ];

pentaPowerSeed[start_Integer, end_Integer] := Block[{},
   pentaPowerSeed[end];
   Return[seeds[[start ;; end]]];
   ];

SetAttributes[{pentaPowerSeed, pentaPowerSeedQ, firstLargerThan}, Listable];

stats[seeds_List] := Module[{upperLimit, bounds, values, indices},
   upperLimit = Floor[Max[seeds]/10^6];
   bounds = Range[upperLimit];
   values = firstLargerThan /@ (10^6 * bounds);
   indices = Position[seeds, #] & /@ values // Flatten;
   Return[Transpose@{bounds, values, indices}];
   ];

commafied[n_Integer] := ToString@NumberForm[n, DigitBlock -> 3];

SetAttributes[{commafied}, Listable];

printOutput[] := Module[{first30, k},
   first30 = commafied[pentaPowerSeed[1, 30]];
   first30 =
    Partition[StringPadLeft[first30, StringLength@Last@first30 ], 6] //
     TableForm;
   Print["The first 30 penta-power seeds are:"];
   Print[first30];
   Print[];
   k = Length@seeds;
   While[pentaPowerSeed[k] < 10*10^6,
    k++];
   Print[
    Join[{"First penta-power prime seed greater than:"},
      StringJoin[
         StringPadLeft[commafied[#[[1]] * 10^6], 10],
         " is " , StringPadLeft[commafied[#[[2]]], 10], " at index ",
         StringPadLeft[commafied[#[[3]]], 3], "."] & /@
       stats[seeds]] // TableForm];
   ];

printOutput[];
