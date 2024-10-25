(* rosettacode URL: https://rosettacode.org/wiki/Binomial_transform#Mathematica_/_Wolfram_Language *)

ClearAll[forwardBinomialTransform, inverseBinomialTransform, primeFlipFlop, P];

forwardBinomialTransform[a_] := Sum[Binomial[#, k] a[k], {k, 0, #}] &;
inverseBinomialTransform[b_] := Sum[(-1)^(# - k) Binomial[#, k] b[k], {k, 0, #}] &;
selfInvertingBinaryTransform[a_] := Sum[(-1)^k Binomial[#, k] a[k], {k, 0, #}] &;

(* Prime flip flop sequence *) 

primeFlipFlop[n_] := If[PrimeQ[n], 1, 0];

(* Padovan sequence *) 

P[0] = 1;
P[1] = P[2] = 0;
P[n_Integer?Positive] = P[n - 2] + P[n - 3];

transforms[seq_, domain_] := {
   seq /@ domain,
   forwardBinomialTransform@seq /@ domain,
   inverseBinomialTransform@seq /@ domain,
   inverseBinomialTransform@forwardBinomialTransform@seq /@ domain,
   selfInvertingBinaryTransform@seq /@ domain,
   selfInvertingBinaryTransform@selfInvertingBinaryTransform@seq /@ 
    domain};
labels[seqName_String] := {
   seqName <> ":",
   "Forward binomial transform:",
   "Inverse binomial transform:",
   "Round trip:",
   "Self-inverting binary transform:",
   "Re-inverted:"
   };

(* Output *)

Print[
    Riffle[labels["Catalan number sequence"], ToString /@ transforms[CatalanNumber, Range[0, 14]]] // TableForm, "\n",
    Riffle[labels["Prime flip-flop sequence"],ToString /@ transforms[primeFlipFlop, Range[0, 14]]] // TableForm, "\n",
    Riffle[labels["Fibonacci sequence"],      ToString /@ transforms[Fibonacci,     Range[0, 14]]] // TableForm, "\n",
    Riffle[labels["Padovan sequence"],        ToString /@ transforms[P,             Range[0, 14]]] // TableForm
];
