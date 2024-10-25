(* rosettacode-url: https://rosettacode.org/wiki/Rosetta_Code/Find_unimplemented_tasks#Mathematica_/_Wolfram_Language *)

ClearAll[ImportAll, unimplementedTasks];

ImportAll[lang_String] :=
    Module[
        {data = {}, continue = True, cmcontinue = "", xml},
        While[continue,
              xml =Import[
                  "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:" <>
                  lang <>
                  "&cmlimit=500&format=xml&cmcontinue=" <>
                  cmcontinue,
                  "XML"];
              AppendTo[data, Cases[xml, XMLElement["cm", _, {}], Infinity]];
              cmcontinue = Cases[xml, XMLElement["continue", cmcontinue_, {}] :> cmcontinue, Infinity];
              If[Length[cmcontinue] > 0,
                 cmcontinue = Last@First@First@cmcontinue;,
                 (*else*) cmcontinue = "";];
              continue = (cmcontinue != "");
        ];
        Return[Cases[data, HoldPattern["title" -> x_] :> x, Infinity]];
    ];

unimplementedTasks[lang_String] := Complement[ImportAll["Programming_Tasks"], ImportAll[lang]];
