(* rosettacode-url: https://rosettacode.org/wiki/Doomsday_rule#Mathematica_/_Wolfram_Language *)

doomsday[year_Integer?Positive] := Mod[2 + 5 Mod[year, 4] + 4 Mod[year, 100] + 6 Mod[year, 400], 7];

firstDoomsday[month_Integer /; 1 <= month <= 2, year_Integer?Positive /; LeapYearQ[{year}]] := {4, 1}[[month]];
firstDoomsday[month_Integer /; 1 <= month <= 2, year_Integer?Positive] := {3, 7}[[month]];
firstDoomsday[month_Integer /; 3 <= month <= 12, year_Integer?Positive] := {7, 4, 2, 6, 4, 1, 5, 3, 7, 5}[[month - 2]];

weekday[{year_Integer?Positive, month_Integer?Positive /; 1 <= month <= 12, day_Integer?Positive /; 1 <= day <= 31}] :=
    Mod[doomsday[year] + 7 + day - firstDoomsday[month, year], 7];

weekdayName[weekday_Integer /; 0 <= weekday <= 6] :=
    {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"}[[weekday + 1]];

dates = {{1800, 1, 6}, {1875, 3, 29}, {1915, 12, 7}, {1970, 12, 23}, {2043, 5, 14}, {2077, 2, 12}, {2101, 4, 2}};

weekdayName /@ weekday /@ dates
