Package["Wolfram`Multicomputation`"]

PackageImport["Wolfram`MulticomputationInit`"]
PackageImport["WolframInstitute`Hypergraph`"]

PackageExport["MultiwaySystemQ"]
PackageExport["MultiwaySystem"]
PackageExport["$StateVertexShapeFunction"]
PackageExport["$EventVertexShapeFunction"]



MultiwaySystemQ[MultiwaySystem[_Multi, _String]] := True
MultiwaySystemQ[___] := False


Options[MultiwaySystem] = {Method -> Automatic}

MultiwaySystem[rules_, opts : OptionsPattern[]] := MultiwaySystem[rules, FirstCase[Unevaluated[rules], (Rule | RuleDelayed)[lhs_, _] :> {lhs}, All], opts]

m : MultiwaySystem[rules_, init_, OptionsPattern[]] /; ! MultiwaySystemQ[Unevaluated[m]] := Enclose @ Block[{
    type = First[ConfirmBy[MultiwayType /@ wrap[rules], Apply[Equal]]],
    method, methodOpts
},
    {method, methodOpts} = Replace[OptionValue[Method], {
        Automatic :> {Switch[type, "Hypergraph", WolframModelMulti, "String", StringMulti, "CA", CAMulti, "WIHypergraph", WIHypergraphMulti, _, HypergraphMulti], {}},
        "Hypergraph" | {"Hypergraph", opts : OptionsPattern[]} :> {WolframModelMulti, {opts}},
        "String" | {"String", opts : OptionsPattern[]} :> {StringMulti, {opts}},
        {type_, opts : OptionsPattern[]} :> {Switch[type, "Hypergraph", HypergraphMulti, "WolframModel", WolframModelMulti, "String", StringMulti, "CA", CAMulti, "WIHypergraph", WIHypergraphMulti, _, HypergraphMulti], {opts}}
    }];
    MultiwaySystem[
        Evaluate[method[Unevaluated[init], rules, methodOpts]],
        type
    ]
]

SetAttributes[MultiwaySystem, HoldFirst]


MultiwaySystemProp[_, "Properties"] := Sort @ {
    "Properties", "Multi", "Type", "Graph", "EvolutionGraph", "StatesGraph", "CausalGraph", "BranchialGraph", "AllStatesBranchialGraph",
    "EvolutionCausalGraph", "EvolutionEventsGraph", "CausalBranchialGraph", "TokenEventGraph", "CausalEvolutionGraph", "CausalStatesGraph"
}

MultiwaySystemProp[HoldPattern[MultiwaySystem[multi_, _]], "Multi"] := multi

MultiwaySystemProp[HoldPattern[MultiwaySystem[_, type_]], "Type"] := type


StateShape[hg : {{_Integer | _Symbol, __}...}, size_ : Automatic, opts___] := ResourceFunction["WolframModelPlot"][hg, FilterRules[{opts}, Options[ResourceFunction["WolframModelPlot"]]], ImageSize -> size, PlotRangePadding -> 0]
StateShape[hg_ ? HypergraphQ, size_, opts___] := SimpleHypergraphPlot[hg, FilterRules[{opts}, Options[SimpleHypergraphPlot]], ImageSize -> size]
StateShape[_Missing, ___] := ""
StateShape[expr_, ___] := expr

$StateVertexShapeFunction[type_, opts___] := Function[Inset[
    Framed[
        Style[
            Switch[type, "CA" | ("CA" -> "Tokens"), ArrayPlot[If[MatchQ[type, _Rule], FromLinkedHypergraph[{#2}, "CA"], #2], ImageSize -> 64 #3], "Tokens", StateShape[MapAt[Style[#, Bold] &, #2, 2], #3, opts], _, StateShape[#2, #3, opts]],
            If[MatchQ[type, "String" | (_ -> "Tokens")], {FontFamily -> "Arial",  FontWeight -> Plain}, {}],
            OptionValue[MultiEvaluate, "StateStyleOptions"]
        ],
        OptionValue[MultiEvaluate, "StateFrameOptions"]
    ],
    #1,
    {0, 0}
    ]
]


MultiwaySystemProp[m_, "Graph" | "EvolutionGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
Block[{g},
    g = m["Multi"]["Graph", n,
        FilterRules[{opts}, Options[Graph]],
        VertexLabels -> Placed[Automatic, Tooltip],
        VertexShapeFunction -> With[{extra = m["ExtraOptions"]}, Tooltip[$StateVertexShapeFunction[type, extra][#1, FromLinkedHypergraph[#2, Replace[type, "Expression" -> "HoldExpression"]], #3], #2] &],
        VertexSize -> If[StringEndsQ[type, "Hypergraph"], 64, Automatic],
        EdgeStyle -> ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph", "EdgeStyle"],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]]];
    g
]]

MultiwaySystemProp[m_, "StatesGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[Join[{"IncludeInitialState" -> False}, Options[canonicalizeStates], Options[Graph]]]] := With[{type = m["Type"]},
Block[{g},
    g = Graph[
        m["Expression"],
        Flatten[
            If[ #2[[2]] === {},
                {},
                MapThread[{from, to} |-> (DirectedEdge[from, #] & /@ to), {ReleaseHold[#1[[1]]]["Expression"], #2[[2]]}, 1]
             ] & @@@
                Partition[m["Multi"]["Foliations", n][[1]], 2, 1],
            2
        ],
        FilterRules[{opts}, Options[Graph]],
        VertexLabels -> Placed[Automatic, Tooltip],
        VertexShapeFunction -> With[{extra = m["ExtraOptions"]}, Tooltip[$StateVertexShapeFunction[type, extra][#1, FromLinkedHypergraph[#2, Replace[type, "Expression" -> "HoldExpression"]], #3], #2] &],
        VertexSize -> If[StringEndsQ[type, "Hypergraph"], 64, Automatic],
        EdgeStyle -> ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph", "EdgeStyle"],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    If[ TrueQ[Lookup[{opts}, "IncludeInitialState"]],
        g = EdgeAdd[g, DirectedEdge[{{1, Missing[]}}, #] & /@ m["Expression"]]
    ];
    g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]], "CanonicalStateFunction" -> Automatic];
    g
]]

StringSubstitutionEventShape[DirectedEdge[from_, to_, tag_], size_, ___] := Framed[
    Style[
        Row @ SequenceReplace[
            SequenceReplace[from,
                vs : {{Alternatives @@ tag["Input"], ___}...} :>
                    Column[{
                        FromLinkedHypergraph[vs, "String"],
                        FromLinkedHypergraph[Cases[to, {Alternatives @@ tag["Output"], ___}], "String"]
                    }, BaseStyle -> {FontFamily -> "Arial", FontWeight -> Plain}, OptionValue[MultiEvaluate, "EventColumnOptions"]]
            ],
            vs : {{__}...} :> FromLinkedHypergraph[vs, "String"]
        ],
        FontFamily -> "Arial", FontWeight -> Plain,
        OptionValue[MultiEvaluate, "EventStyleOptions"]
    ],
    OptionValue[MultiEvaluate, "EventFrameOptions"]
]

ListSubstitutionEventShape[DirectedEdge[from_, to_, tag_], size_, ___] := Framed[
    Style[
        SequenceReplace[
            SequenceReplace[from,
                vs : {{Alternatives @@ tag["Input"], ___}...} :>
                    Column[{
                        Row[FromLinkedHypergraph[vs, "List"], ","],
                        Row[FromLinkedHypergraph[Cases[to, {Alternatives @@ tag["Output"], ___}], "List"], ","]
                    }, OptionValue[MultiEvaluate, "EventColumnOptions"]]
            ],
            vs : {{__}...} :> Row[FromLinkedHypergraph[vs, "List"], ","]
        ],
        OptionValue[MultiEvaluate, "EventStyleOptions"]
    ],
    OptionValue[MultiEvaluate, "EventFrameOptions"]
]

WolframModelEventShape[DirectedEdge[from_, to_, tag_], size_, ___] := With[{
    lhs = Replace[from, {l_, p_, rest___} :> If[p === None, {rest}, {l, HoldForm[p], rest}], {1}],
    rhs = Replace[to, {l_, p_, rest___} :> If[p === None, {rest}, {l, HoldForm[p], rest}], {1}]
},
Framed[
    Row[{
        If[ DeleteCases[{}] @ lhs === {},
            Graphics[{}, ImageSize -> size / 2],
            ResourceFunction["WolframModelPlot"][DeleteCases[{}] @ lhs,
                GraphHighlight -> DeleteCases[{}] @ Extract[lhs, Position[from, {Alternatives @@ tag["Input"], ___}]],
                GraphHighlightStyle -> Dashed,
                ImageSize -> size / 2
            ]
        ],
        Graphics[{LightGray, FilledCurve[
            {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
            {{{-1., 0.1848}, {0.2991, 0.1848}, {-0.1531, 0.6363}, {0.109, 0.8982}, {1., 0.0034},
            {0.109, -0.8982}, {-0.1531, -0.6363}, {0.2991, -0.1848}, {-1., -0.1848}, {-1., 0.1848}}}
        ]}, ImageSize -> size / 5],
        ResourceFunction["WolframModelPlot"][DeleteCases[{}] @ rhs, GraphHighlight -> DeleteCases[{}] @ Extract[rhs, Position[to, {Alternatives @@ tag["Output"], ___}]], ImageSize -> size / 2]
    }],
    OptionValue[MultiEvaluate, "EventFrameOptions"]
]
]

Options[WIHypergraphEventShape] = Options[Hypergraph]

WIHypergraphEventShape[DirectedEdge[from_, to_, tag_], size_, opts : OptionsPattern[]] := Framed[
    If[ KeyExistsQ[tag, "Match"],
        First @ HighlightRule[
            {Replace[tag["Match"], IconizedObject[e_, ___] :> e]},
            Hypergraph[FromLinkedHypergraph[from, "WIHypergraph"], FilterRules[{opts}, Options[Hypergraph]]],
            ImageSize -> size / 2
        ],
        SimpleHypergraphPlot[FromLinkedHypergraph[to, "WIHypergraph"], FilterRules[{opts}, Options[SimpleHypergraphPlot]], ImageSize -> size]
    ],
    OptionValue[MultiEvaluate, "EventFrameOptions"]
]

DefaultEventShape[DirectedEdge[from_, to_, tag_], size_, ___] := Framed[
    Style[
        Block[{
            lhsPos = tag["Position"],
            rhsPos = FirstPosition[to, {#, ___}, {1}, Heads -> False] & /@ tag["Output"],
            lhs, rhs, lhsRootPos, rhsRootPos
        },
            If[MissingQ[lhsPos], Return["", Block]];
            lhs = Extract[from, lhsPos];
            rhs = Extract[to, rhsPos];
            lhsRootPos = FirstPosition[from, LinkedHypergraphRoot[lhs], {1}, Heads -> False];
            rhsRootPos = FirstPosition[to, LinkedHypergraphRoot[rhs], {1}, Heads -> False];
            FromLinkedHypergraph[
                Delete[
                    ReplacePart[from, lhsRootPos -> {
                        Extract[from, lhsRootPos, First],
                        Column[{
                            HoldForm @@ TreeExpression[LinkedHypergraphToRootTree[from, Extract[from, lhsRootPos, First]], "HeldHeadTrees"],
                            HoldForm @@ TreeExpression[LinkedHypergraphToRootTree[to, Extract[to, rhsRootPos, First]], "HeldHeadTrees"]
                        }, OptionValue[MultiEvaluate, "EventColumnOptions"]] //. HoldPattern[Construct[f_, x__]] :> f[x]
                    }],
                    DeleteCases[lhsPos, lhsRootPos]
                ],
                "HoldExpression"
            ]
        ],
        OptionValue[MultiEvaluate, "EventStyleOptions"]
    ],
    OptionValue[MultiEvaluate, "EventFrameOptions"]
]


TokenEventShape[DirectedEdge[from_, to_, tag_], size_, ___] := Framed[
    StandardForm @ Style[
        Block[{
            lhsPos = tag["Position"],
            rhsPos = FirstPosition[to, {#, ___}, {1}, Heads -> False] & /@ tag["Output"],
            lhs, rhs
        },
            If[MissingQ[lhsPos], Return["", Block]];
            lhs = Extract[from, lhsPos];
            rhs = Extract[to, rhsPos];
            Column[{
                If[MatchQ[lhs, {{_, _Missing}}], {}, MapAt[Style[#, Bold] &, {All, 2}] @ lhs],
                MapAt[Style[#, Bold] &, {All, 2}] @ rhs
            }, BaseStyle -> {FontFamily -> "Arial", FontWeight -> Plain}, OptionValue[MultiEvaluate, "EventColumnOptions"]]
        ],
        OptionValue[MultiEvaluate, "EventStyleOptions"]
    ],
    OptionValue[MultiEvaluate, "EventFrameOptions"]
]

$EventVertexShapeFunction[type_, opts : OptionsPattern[]] := With[{f = Switch[
    type,
    "String",
    StringSubstitutionEventShape,
    "List",
    ListSubstitutionEventShape,
    "Hypergraph",
    WolframModelEventShape,
    "WIHypergraph",
    WIHypergraphEventShape,
    "Tokens",
    TokenEventShape,
    _,
    DefaultEventShape
]}, {
    fopts = FilterRules[{opts}, Options[f]]
},
    Function[Replace[#2, DirectedEdge[Interpretation[_, from_] | from_, Interpretation[_, to_] | to_, tag_] :> Inset[StatusArea[f[DirectedEdge[from, to, tag], #3, fopts], tag], #1, {0, 0}]]]
]


MultiwaySystemProp[m_, "CausalGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
Block[{g},
    g = m["Multi"]["CausalGraph", n,
        FilterRules[{opts}, Options[CausalGraph]],
        "IncludeInitialEvent" -> False,
        VertexLabels -> DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip],
        VertexShapeFunction -> Function[$EventVertexShapeFunction[type, m["ExtraOptions"]][#1, #2, #3]],
        VertexSize -> If[StringEndsQ[type, "Hypergraph"], 64, Automatic],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    If[VertexCount[g] == 0, Return[g]];
    With[{
        stateCanonicalFunction = Replace[OptionValue[canonicalizeStates, FilterRules[{opts}, Options[canonicalizeStates]], "CanonicalStateFunction"], {
            Automatic -> Function[FromLinkedHypergraph[#, type]],
            "Canonical" -> CanonicalLinkedHypergraph,
            Full -> Function[FromLinkedHypergraph[CanonicalLinkedHypergraph[#], type]],
            _ -> Identity
        }]
    },
        g = VertexReplace[g, DirectedEdge[from_, to_, tag___] :> DirectedEdge[Interpretation[stateCanonicalFunction[from], from], Interpretation[stateCanonicalFunction[to], to], tag]]
    ];
    g = canonicalizeEvents[g, type, FilterRules[{opts}, Options[canonicalizeEvents]]];
    g
]]

MultiwaySystemProp[m_, "TokenEventGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
Block[{g},
    g = m["Multi"]["TokenEventGraph", n,
        FilterRules[{opts}, Options[MultiTokenEventGraph]],
        "IncludeInitialEvent" -> False,
        "IncludeInitialState" -> False,
         VertexLabels -> {
            DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip]
        },
        VertexShapeFunction -> {
            _DirectedEdge -> $EventVertexShapeFunction[type, m["ExtraOptions"]],
            Except[_DirectedEdge] -> Function[Tooltip[$StateVertexShapeFunction[type -> "Tokens", m["ExtraOptions"]][#1, #2, #3], #2]]
        },
        VertexSize -> If[StringEndsQ[type, "Hypergraph"], 64, Automatic],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeTokens[g, type, FilterRules[{opts}, Options[canonicalizeTokens]]];
    g = canonicalizeEvents[g, type, FilterRules[{opts}, Options[canonicalizeEvents]], VertexShapeFunction -> _DirectedEdge -> $EventVertexShapeFunction["Tokens"]];
    g
]]

CanonicalEventFunction[event : DirectedEdge[Interpretation[ifrom_ : None, from_] | from_, Interpretation[ito_ : None, to_] | to_, tag_], type_, keys_List] := Block[{fromIso, canonicalFrom, toIso, canonicalTo},
    {fromIso, canonicalFrom} = CanonicalLinkedHypergraph[from, "IncludeIsomorphism" -> True];
    {toIso, canonicalTo} = CanonicalLinkedHypergraph[to, "IncludeIsomorphism" -> True];
    DirectedEdge[
        If[ifrom === None, #, Interpretation[ifrom, #]] & @ FromLinkedHypergraph[canonicalFrom, type],
        If[ito === None, #, Interpretation[ito, #]] & @ FromLinkedHypergraph[canonicalTo, type],
        Part[MapAt[ReplaceAll[toIso], "Output"] @ MapAt[ReplaceAll[fromIso], "Input"] @ tag, Key /@ keys]
    ]
]

Options[canonicalizeStates] = Join[{"CanonicalStateFunction" -> None}, Options[Graph]];
canonicalizeStates[g_, type_, opts : OptionsPattern[]] := With[{
    stateCanonicalFunction = Replace[OptionValue["CanonicalStateFunction"], {
        Automatic -> Function[FromLinkedHypergraph[#, type]],
        "Canonical" | "CanonicalHypergraph" /; type === "WIHypergraph" :> ToLinkedHypergraph @* CanonicalHypergraph @* (FromLinkedHypergraph[ReplacePart[#, {_, 2} -> Automatic], type] &),
        "Canonical" | "CanonicalHypergraph" -> CanonicalLinkedHypergraph,
        Full -> Function[FromLinkedHypergraph[CanonicalLinkedHypergraph[#], type]]
    }]
},
    If[ stateCanonicalFunction === None,
        g,
        Graph[
            VertexReplace[g, state : Except[_DirectedEdge] :> stateCanonicalFunction[state]],
            FilterRules[{opts}, Options[Graph]],
            VertexShapeFunction -> Except[_DirectedEdge] -> Switch[
                OptionValue["CanonicalStateFunction"],
                Automatic | Full, (Tooltip[$StateVertexShapeFunction[type][##], #2] &),
                _, (Tooltip[$StateVertexShapeFunction[type][#1, FromLinkedHypergraph[#2, type], #3], #2] &)
            ]
        ]
    ]
]

Options[canonicalizeTokens] = Join[{"CanonicalTokenFunction" -> None}, Options[Graph]];
canonicalizeTokens[g_, type_, opts : OptionsPattern[]] := With[{
    tokenCanonicalFunction = Replace[OptionValue["CanonicalTokenFunction"], {
        Automatic -> Function[FromLinkedHypergraph[{#}, type]],
        "Canonical" | "CanonicalHypergraph" -> Function[CanonicalLinkedHypergraph[{#}]],
        Full -> Function[FromLinkedHypergraph[CanonicalLinkedHypergraph[{#}], type]]
    }]
},
    If[ tokenCanonicalFunction === None,
        g,
        Graph[
            VertexReplace[g, token : Except[_DirectedEdge] :> tokenCanonicalFunction[token]],
            FilterRules[{opts}, Options[Graph]],
            VertexShapeFunction -> Except[_DirectedEdge] -> $StateVertexShapeFunction[type]
        ]
    ]
]

Options[canonicalizeEvents] = Join[{"CanonicalEventFunction" -> None}, Options[Graph]]
canonicalizeEvents[g_, type_, opts : OptionsPattern[]] := With[{
    eventCanonicalFunction = Replace[OptionValue["CanonicalEventFunction"], {
        Full -> Function[CanonicalEventFunction[#, type, {}]],
        Automatic -> Function[CanonicalEventFunction[#, type, {"Input", "Output", "Step"}]],
        "Tokens" -> Function[event, With[{lhs = Replace[event[[1]], Interpretation[_, e_] :> e], rhs = Replace[event[[2]], Interpretation[_, e_] :> e]}, DirectedEdge[
            Extract[lhs, event[[3]]["Position"]],
            Extract[rhs, FirstPosition[rhs, {#, ___}, {1}, Heads -> False] & /@ event[[3]]["Output"]],
            Append[KeyTake[event[[3]], {"Input", "Output"}], "Position" -> List /@ Range[Length[event[[3]]["Input"]]]]
        ]]],
        keys : {___String} :> Function[CanonicalEventFunction[#, None, keys]]
    }]
},
    If[ eventCanonicalFunction === None,
        Graph[g, FilterRules[{opts}, Options[Graph]]],
        Block[{oldEvents = VertexList[g, _DirectedEdge], newEvents},
            newEvents = Replace[oldEvents, event : DirectedEdge[from_, to_, ___] :>
                Replace[eventCanonicalFunction[event], newTag : Except[_DirectedEdge] :> DirectedEdge[from , to, newTag]],
                {1}
            ];
            Graph[
                VertexReplace[g, Thread[oldEvents -> newEvents, List, 2]],
                FilterRules[{opts}, Options[Graph]],
                VertexLabels -> DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip],
                VertexShapeFunction -> Normal @ GroupBy[
                    MapThread[
                        {oldEvent, newEvent} |-> newEvent -> Function[$EventVertexShapeFunction[type][#1, oldEvent, #3]],
                        {oldEvents, newEvents}
                    ],
                    First,
                    #[[1, 2]] &
                ]
            ]
        ]
    ]
]

MultiwaySystemProp[
    m_, "EvolutionCausalGraph", n : _Integer ? Positive : 1,
    opts : OptionsPattern[Join[{"CanonicalStateFunction" -> None, "CanonicalEventFunction" -> None}, Options[EvolutionCausalGraph]]]
] :=
Block[{g},
With[{type = m["Type"]},
    g = m["Multi"]["EvolutionCausalGraph", n,
        FilterRules[{opts}, {Options[CausalGraph], Options[EvolutionCausalGraph]}],
        "IncludeInitialEvent" -> False,
        "IncludeInitialState" -> False,
        VertexLabels -> {
            v_ :> Placed[HoldForm[v], Tooltip],
            DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip]
        },
        VertexShapeFunction -> {
            _DirectedEdge -> $EventVertexShapeFunction[type, m["ExtraOptions"]],
            Except[_DirectedEdge] -> Function[$StateVertexShapeFunction[type, m["ExtraOptions"]][#1, FromLinkedHypergraph[#2, Replace[type, "Expression" -> "HoldExpression"]], #3]]
        },
        EdgeShapeFunction -> DirectedEdge[Except[_DirectedEdge], _DirectedEdge] -> "Line",
        VertexSize -> If[StringEndsQ[type, "Hypergraph"], 64, Automatic],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]]];
    g = canonicalizeEvents[g, type, FilterRules[{opts}, Options[canonicalizeEvents]]];
    g
]
]

MultiwaySystemProp[m_, "EvolutionEventsGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
    EdgeDelete[m["EvolutionCausalGraph", n, opts], DirectedEdge[_DirectedEdge, _DirectedEdge, ___]]

MultiwaySystemProp[m_, prop : "EvolutionBranchialGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := Block[{g},
    With[{type = m["Type"]},
        g = EvolutionBranchialGraph[Graph[VertexList[m["Multi"]["CausalGraph", n,
            FilterRules[{opts}, Options[CausalGraph]],
            "IncludeInitialEvent" -> False
        ]]]];
        g = Graph[g,
            VertexLabels -> v_ :> Placed[HoldForm[v], Tooltip],
            VertexShapeFunction -> Function[$StateVertexShapeFunction[type, m["ExtraOptions"]][#1, FromLinkedHypergraph[#2, Replace[type, "Expression" -> "HoldExpression"]], #3]],
            VertexSize -> If[StringEndsQ[type, "Hypergraph"], 64, Automatic],
            GraphLayout -> "LayeredDigraphEmbedding",
            PerformanceGoal -> "Quality"
        ];
        g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]]];
        g
    ]
]

MultiwaySystemProp[m_, prop : "CausalBranchialGraph" | "EvolutionCausalBranchialGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
    canonicalizeEvents[
        CausalBranchialGraph[m[StringDelete[prop, "Branchial"], n, FilterRules[{opts}, {Options[CausalGraph], Options[EvolutionCausalGraph]}], "CanonicalEventFunction" -> None], FilterRules[{opts}, Options[CausalBranchialGraph]]],
        m["Type"],
        FilterRules[{opts}, Options[canonicalizeEvents]]
    ]

MultiwaySystemProp[m_, prop : "BranchialGraph" | "AllStatesBranchialGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
Block[{g},
    g = Graph[
        If[ prop === "BranchialGraph",
            UndirectedEdge @@@ ReleaseHold[m["Multi"]["Foliations", n - 1][[1, -1, 1]]]["BranchPairs"],
            Block[{foliations = m["Multi"]["Foliations", n - 1][[1, All, 1]]},
                Sequence @@ {
                    Catenate[ReleaseHold[#]["Expression"] & /@ foliations],
                    Catenate[UndirectedEdge @@@ ReleaseHold[#]["BranchPairs"] & /@ foliations]
                }
            ]
        ],
        FilterRules[{opts}, Options[Graph]],
        EdgeStyle -> ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph"]["EdgeStyle"],
        VertexShapeFunction -> Function[Tooltip[$StateVertexShapeFunction[type, m["ExtraOptions"]][#1, FromLinkedHypergraph[#2, Replace[type, "Expression" -> "HoldExpression"]], #3], #2]],
        VertexSize -> If[StringEndsQ[type, "Hypergraph"], 64, Automatic],
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]], "CanonicalStateFunction" -> Automatic];
    g
]]

MultiwaySystemProp[m_, "CausalEvolutionGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
    CausalStatesGraph[
        m["CausalGraphStructure", n,
            "CanonicalizeStates" -> False,
            "IncludeInitialEvent" -> True, "CanonicalEventFunction" -> None,
            "EdgeDeduplication" -> True, "TransitiveReduction" -> False
        ], FilterRules[{opts}, Options[CausalStatesGraph]]
    ]

MultiwaySystemProp[m_, "CausalStatesGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
    CausalStatesGraph[
        m["CausalGraphStructure", n,
            "IncludeInitialEvent" -> True, "CanonicalStateFunction" -> Automatic, "CanonicalEventFunction" -> None,
            "EdgeDeduplication" -> True, "TransitiveReduction" -> False
        ], FilterRules[{opts}, Options[CausalStatesGraph]]
    ]

MultiwaySystemProp[m_, prop_String, opts___] /; StringEndsQ[prop, "GraphStructure"] :=
    m[StringDelete[prop, "Structure"], opts, VertexShapeFunction -> Automatic, VertexSize -> Automatic]

MultiwaySystemProp[m_, args___] := m["Multi"][args]

m_MultiwaySystem[args___] /; MultiwaySystemQ[m] := MultiwaySystemProp[m, args]


MakeBoxes[mobj_MultiwaySystem, form_] := 
    BoxForm`ArrangeSummaryBox[
        "MultiwaySystem",
        mobj,
        None,
        {BoxForm`SummaryItem[{"Type: ", mobj["Type"]}]},
        {BoxForm`SummaryItem[{mobj["HoldExpression"]}]},
        form
    ]

