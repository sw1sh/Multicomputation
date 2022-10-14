Package["Wolfram`Multicomputation`"]

PackageExport["MultiStringReplace"]
PackageExport["ApplyStringRules"]



MultiStringReplace[s_String, rule_Rule, OptionsPattern[]] := Block[{
    lhs = wrap[First[rule]],
    rhs = wrap[Last[rule]],
    pos
},
    pos = StringPosition[s, #] & /@ lhs;
    AssociationMap[
        StringReplacePart[s, Take[rhs, UpTo[Length[#]]], Take[#, UpTo[Length[rhs]]]] &,
        ResourceFunction["SelectTuples"][pos, Length[#] === 1 || IntervalIntersection @@ (Interval /@ #) === Interval[] &]
    ]
]

MultiStringReplace[s_String, rules : {_Rule...}, opts : OptionsPattern[]] :=
    Association @ MapIndexed[KeyMap[List /* Prepend[First[#2]]] @ MultiStringReplace[s, #1, opts] &, rules]

StringReplaceKeys[s_String, rule_Rule, OptionsPattern[]] :=
     ResourceFunction["SelectTuples"][StringPosition[s, #] & /@ wrap[First[rule]], Length[#] === 1 || IntervalIntersection @@ (Interval /@ #) === Interval[] &]

StringReplaceKeys[s_String, rules : {_Rule...}, opts : OptionsPattern[]] :=
    Catenate @ MapIndexed[Map[List /* Prepend[First[#2]]] @ StringReplaceKeys[s, #1, opts] &, rules]

ApplyStringRules[hg_ ? LinkedHypergraphQ, rule_] := With[{rules = wrap[rule]},
    Association @ Map[
        Block[{
            ruleId = #1[[1]], pos = Catenate[Range @@@ #1[[2]]],
            replaceRule, lhs, rhs,
            created, start, end
        },
            replaceRule = rules[[ruleId]];
            lhs = replaceRule[[1]];
            rhs = replaceRule[[2]];
            start = pos[[1]];
            end = pos[[-1]];
            created = With[{inputLen = StringLength[lhs], outputLen = StringLength[rhs]},
                If[ outputLen > inputLen,
                    Join[hg[[Range[start, start + inputLen - 1], 1]], Table[Unique[\[FormalC]], outputLen - inputLen]],
                    hg[[Range[start, start + outputLen - 1], 1]]
                ]
            ];
            <|
                "Destroyed" -> hg[[pos, 1]],
                "Created" -> created,
                "Rule" -> ruleId,
                "Position" -> List /@ pos
            |> -> Insert[Delete[hg, List /@ pos], Splice @ Thread[{created, Characters[rhs], Append[Rest[created], If[Length[hg] > end, hg[[end + 1, 1]], 0]]}], start]
        ] &,
        StringReplaceKeys[StringJoin[hg[[All, 2]]], rules]
    ]
]