Package["Wolfram`Multicomputation`"]

PackageImport["Wolfram`MulticomputationInit`"]

PackageExport["Multi"]

PackageScope["$MultiKeys"]
PackageScope["$MultiOptions"]
PackageScope["$MultiOptionsPattern"]
PackageScope["MultiDataQ"]



$MultiKeys := {"Expression", "Values", "Matches", "Rules", "EvaluateOptions", "ReplaceArguments", "ReplaceOptions", "ExtraOptions"}

Multi[data_Association] /; ContainsOnly[Keys[data], $MultiKeys] && !ContainsExactly[Keys[data], $MultiKeys] :=
    Multi[<|
        "Expression" -> Sequence[],
        "Values" -> <||>,
        "Matches" -> <||>,
        "Rules" -> {},
        "EvaluateOptions" -> {},
        "ReplaceArguments" -> Sequence[{{}}],
        "ReplaceOptions" -> {},
        "ExtraOptions" -> {},
        KeyDrop[data, Complement[Keys[data, $MultiKeys]]]
    |>]


MultiDataQ[data_Association] := MatchQ[data, KeyValuePattern[{
    "Expression" :> _,
    "Values" -> <||> | KeyValuePattern[_[{_Integer ...}, _]],
    "Matches" -> <||> |
        KeyValuePattern[_[
            {{{_Integer ...} ...}, _String, Repeated[_Rule | RuleDelayed, {0, 1}]},
            {(_Rule | _RuleDelayed | None) ..}
        ]] |
        KeyValuePattern[
            _[{{{_Integer ...} ...}, "Rule", {_Integer, {<||> | KeyValuePattern[_Pattern :> _] ..}}}, {__}]
        ],
    "Rules" -> {(_Rule | _RuleDelayed) ...},
    "EvaluateOptions" -> OptionsPattern[],
    "ReplaceOptions" -> OptionsPattern[],
    "ExtraOptions" -> OptionsPattern[]
    }]
]

MultiDataQ[___] := False

Multi[data_]["Data"] := data
Multi[data_]["ValidQ"] := MultiDataQ[data]


mergeData[data__] := Append[Last[{data}][[{"Expression"}]]] @
    MapAt[First, "ReplaceArguments"] @ MapAt[Flatten /* DeleteDuplicates, {{"EvaluateOptions"}, {"ReplaceOptions"}}] @
    MapAt[Apply@Union, "Rules"] @ MapAt[Association/*KeySort, {{"Values"}, {"Matches"}}] @ Merge[KeyDrop["Expression"] /@ {data}, Identity]



$MultiOptions := $MultiOptions = Join[{"DeepMultiEvaluate" -> True}, Options[MultiEvaluate], Options[MultiReplace]]

$MultiOptionsPattern := $MultiOptionsPattern = Alternatives @@ ReplacePart[$MultiOptions, {_, 2} -> _]

Options[Multi] = $MultiOptions

Multi[multi_Multi] := multi

Multi[
    expr_,
    rule : Except[$MultiOptionsPattern, _Rule | _RuleDelayed],
    arg : {} | {{}} | Except[OptionsPattern[$MultiOptions]] : Automatic,
    head_Symbol : List,
    opts : OptionsPattern[$MultiOptions]
] := Multi[Unevaluated @ expr, Unevaluated @ {rule}, arg, head, opts]

Multi[
    expr_,
    rules : {Except[$MultiOptionsPattern, _Rule | _RuleDelayed] ...},
    arg : {} | {{}} | Except[OptionsPattern[$MultiOptions]] : Automatic,
    head_Symbol : List,
    opts : OptionsPattern[$MultiOptions]] :=
With[{
    multi = Which[
        MatchQ[Hold[expr], Hold[_Multi]],
        expr,

        MatchQ[Unevaluated @ expr, head[head[]]],
        Multi[<|"Expression" :> head[]|>],

        MatchQ[Unevaluated @ expr, _head],
        If[
            TrueQ[OptionValue[Multi, {opts}, "DeepMultiEvaluate"]],
            MapAt[
                HoldApply[Multi, Sequence @@ FilterRules[{opts}, Options[MultiEvaluate]]],
                Unevaluated @ expr,
                    ElementPositions[Unevaluated @ expr, head]
            ],
            Multi[<|"Expression" :> expr|>]
        ],

        True,
        Multi[Unevaluated @ expr, Sequence @@ FilterRules[{opts}, Options[MultiEvaluate]]]
    ]
},
    With[{
        multiExpr = Unevaluated @@ multi["HoldExpression"]
    },
    With[{
        matches = MultiReplace[
            multiExpr,
            Unevaluated @ rules,
            Replace[arg, Automatic -> Join[If[ListQ[multiExpr], {{All}}, {{}}], Append[All] /@ Position[multiExpr, _Not | _And | _Or, All, Heads -> False]]],
            head,
            FilterRules[{opts}, Options[MultiReplace]],
            Heads -> False,
            Method -> "Substitution",
            "ReturnMatches" -> True,
            "PatternSubstitutions" -> True
        ]
    },
    Multi[<|
        multi["Data"],
        "Rules" -> rules,
        "Matches" -> Join[multi["Matches"], Association @ KeyValueMap[{#1[[2]], "Rule", Drop[#1, {2}]} -> #2 &, matches]],
        "EvaluateOptions" -> FilterRules[{opts}, Options[MultiEvaluate]],
        "ReplaceArguments" -> Sequence[arg, head],
        "ReplaceOptions" -> FilterRules[{opts}, Options[MultiReplace]],
        "ExtraOptions" -> FilterRules[{opts}, Except[Join[Options[MultiEvaluate], Options[MultiReplace]]]]
    |>]
    ]]
]


(* Multi[expr_, rules : {},
    arg : {} | {{}} | Except[OptionsPattern[]] : Automatic,
    head_Symbol : List,
    opts : OptionsPattern[Join[Options[MultiEvaluate], Options[MultiReplace]]]] :=
With[{
    multi = If[
        TrueQ[OptionValue[{opts}, "DeepMultiEvaluate"]] && ListQ[Unevaluated @ expr],
        MapAt[HoldApply[Multi, Sequence @@ FilterRules[{opts}, Options[MultiEvaluate]]], Unevaluated @ expr, ElementPositions[Unevaluated @ expr, List]],
        Multi[Unevaluated @ expr, Sequence @@ FilterRules[{opts}, Options[MultiEvaluate]]]
    ]
},
    Multi[<|
        multi["Data"],
        "Rules" -> rules,
        "EvaluateOptions" -> FilterRules[{opts}, Options[MultiEvaluate]],
        "ReplaceArguments" -> Sequence[arg, head],
        "ReplaceOptions" -> FilterRules[{opts}, Options[MultiReplace]]|>
    ]
] *)

Multi /: (f : Except[Multi])[left___, HoldPattern[Multi[alts_List]], right___] := With[{
    i = Length @ Unevaluated @ {left} + 1
},
    If[
        ! FreeQ[Unevaluated @ f[left, MultiPlaceholder, right], _Multi, {1}] || MatchQ[Unevaluated@f, _Multi],
        Multi[<|
            "Expression" :> Evaluate @ (f @@ (Unevaluated /@ Hold[left, MultiPlaceholder, right])),
            "Values" -> <|{i} :> alts|>
        |>],
        Multi[<|
            "Expression" :> f[left, MultiPlaceholder, right], "Values" -> <|{i} :> alts|>|>]
    ] /; !SequenceHoldQ[Unevaluated[f[left, MultiPlaceholder, right]]] &&
        !AtomQ[Unevaluated @ f[left, MultiPlaceholder, right]] &&
        !HoldPositionQ[Unevaluated[f[left, MultiPlaceholder, right]], i]
]


multi_Multi[args___] /; multi["ValidQ"] := Function[Null,
    With[{expr := #, data = multi["Data"]},
    With[{
        newData = <|data,
        "Values" -> KeyMap[Prepend[0], data["Values"]],
        "Matches" -> KeyMap[MapAt[Prepend[0], {1, All}], data["Matches"]]
    |>},
        If[ !FreeQ[Unevaluated @ expr[args], _Multi, 1, Heads -> False],
            With[{newMulti = MultiPlaceholder[Unique[]][args]},
                Multi[
                    Function[Null, <|mergeData[newMulti["Data"], newData], "Expression" :> #|>, HoldFirst] @@
                        ReplacePart[newMulti["HoldExpression"], {1, 0} :> expr]
                ]
            ],
            Multi[<|newData, "Expression" :> expr[args]|>]
        ]
    ]],
   HoldFirst
] @@ multi["HoldExpression"]


Multi /: (f : Except[Multi])[left___, multi : HoldPattern @ Multi[KeyValuePattern["Expression" :> subExpr_]], right___] /;
    multi["ValidQ"] && FreeQ[Unevaluated @ f, _Multi] :=
With[{
    i = Length @ Unevaluated @ {left} + 1
},
    With[{data = multi["Data"]},
    With[{newData = <|data,
         "Values" -> KeyMap[Prepend[i], data["Values"]],
         "Matches" -> KeyMap[MapAt[Prepend[i], {1, All}], data["Matches"]]
    |>},
        If[ !FreeQ[Unevaluated @ f[left, subExpr, right], _Multi, 1, Heads -> False],
            With[{newMulti = (f @@ (Unevaluated /@ Hold[left, subExpr, right]))},
                Multi[mergeData[newData, newMulti["Data"]]]
            ],
            Multi[<|newData, "Expression" :> f[left, subExpr, right]|>]
        ]
    ]] /; !SequenceHoldQ[Unevaluated[f[left, subExpr, right]]] &&
        !AtomQ[Unevaluated @ f[left, subExpr, right]] &&
        !HoldPositionQ[Unevaluated[f[left, subExpr, right]], i]
]

Multi[alts_List][args___] := With[{ph = MultiPlaceholder[Unique[]]},
    If[
        !FreeQ[Unevaluated @ ph[args], _Multi, {1}],
        Multi[<|
            "Expression" :> Evaluate @ ph[args],
            "Values" -> <|{0} :> alts|>
        |>],
        Multi[<|"Expression" :> ph[args], "Values" -> <|{0} :> alts|>|>]
    ]
]

Multi[alts_List, opts : OptionsPattern[$MultiOptions]] := With[{ph = MultiPlaceholder[Unique[]]},
    Multi[<|
        "Expression" :> ph,
        "Values" -> <|{} :> alts|>,
        "EvaluateOptions" -> FilterRules[{opts}, Options[MultiEvaluate]],
        "ReplaceOptions" -> FilterRules[{opts}, Options[MultiReplace]],
        "ExtraOptions" -> FilterRules[{opts}, Except[Join[Options[MultiEvaluate], Options[MultiReplace]]]]
    |>]
]

Multi[assoc_Association, args__] := Multi[Values[assoc], args]


Multi[subExpr : Except[_List | _Association], opts : OptionsPattern[$MultiOptions]] := Multi[<|
    "Expression" :> subExpr,
    "Matches" -> GroupBy[
        With[{filterOpts = FilterRules[{opts}, Options[MultiEvaluate]]},
            MultiEvaluate[subExpr, "Keys", filterOpts]
        ],
        Take[#, 2] &,
        Map[If[Length @ # > 2, Last @ #, None] &]
    ],
    "EvaluateOptions" -> FilterRules[{opts}, Options[MultiEvaluate]],
    "ReplaceOptions" -> FilterRules[{opts}, Options[MultiReplace]],
    "ExtraOptions" -> FilterRules[{opts}, Except[Join[Options[MultiEvaluate], Options[MultiReplace]]]]
|>
]


Multi[Except[data_ /; AssociationQ[Unevaluated @ data]]] := $Failed

