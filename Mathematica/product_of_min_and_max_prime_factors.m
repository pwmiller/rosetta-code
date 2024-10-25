(* rosettacode-url: https://rosettacode.org/wiki/Product_of_min_and_max_prime_factors#Mathematica_/_Wolfram_Language *)

ClearAll[primeFactors, productOfMinAndMaxPrimeFactor];

primeFactors[n_Integer] := First@Transpose@FactorInteger[n];

productOfMinAndMaxPrimeFactor[n_Integer] :=
    Times @@ MinMax @ primeFactors[n];

TableForm@Partition[
    productOfMinAndMaxPrimeFactor /@ Range[100],
    UpTo[10]]
