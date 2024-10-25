(* rosettacode-url: https://rosettacode.org/wiki/Check_if_two_polygons_overlap#Mathematica_/_Wolfram_Language *)

polygons = Polygon /@ {
      {{0, 0}, {0, 2}, {1, 4}, {2, 2}, {2, 0}},
                  {{4, 0}, {4, 2}, {5, 4}, {6, 2}, {6, 0}},
                  {{1, 0}, {1, 2}, {5, 4}, {9, 2}, {9, 0}}
    };

overlappingPolygonsQ[polys___] := ! RegionDisjoint[polys];

imageOptions = {ImageSize -> Tiny, AlignmentPoint -> Center,
   AspectRatio -> 3/4};

Grid[
 {
  Style[#, Large] & /@ {"Plot", "Description", "Overlapping?"},
  {Graphics[{{Red,
        First@#}, {Blue,
        Last@#}} &  [{polygons[[1]], polygons[[2]]}], imageOptions],
   Style["Polygon 1 (red) and 2 (blue)"],
   overlappingPolygonsQ[polygons[[1]], polygons[[2]]]},

  {Graphics[{{Darker[Magenta],           First@#}, {Darker@Green,
        Last@#}} &  [{polygons[[3]], polygons[[1]]}], imageOptions],
   Style["Polygon 1 (dark magenta) and 3 (green)"],
   overlappingPolygonsQ[polygons[[1]], polygons[[3]]]},

  {Graphics[{{Darker@Red, First@#}, {Lighter@Blue,
        Last@#}} &  [{polygons[[3]], polygons[[2]]}], imageOptions],
   Style["Polygon 2 (dark red) and 3 (lighter blue)"],
   overlappingPolygonsQ[polygons[[3]], polygons[[2]]]}
  },
 Frame -> All, Spacings -> {2, 1.5, 2},
 Alignment -> {Center, Center},
 ItemStyle -> {FontFamily -> "CMU Concrete", FontSize -> Medium},
 Background -> {{}, {None, {LightGray, LightBlue}}}]
