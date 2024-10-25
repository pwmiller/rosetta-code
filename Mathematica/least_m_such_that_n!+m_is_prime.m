(* rosettacode-url: https://rosettacode.org/wiki/Least_m_such_that_n!_%2B_m_is_prime *)

ClearAll[m, n];

(* Least positive m such that n! + m is prime *)

m[n_Integer] := Module[{nfact = n!},
                       Return[NextPrime[nfact] - nfact];
                ];
SetAttributes[m, Listable];

Partition[m[Range[0, 49]], UpTo[10]] // TableForm

                                        n = 50;
While[m[n] < 1000,
      n++;];
Print["First m > 1000: ", m[n], " for ", n, "!"]; 

