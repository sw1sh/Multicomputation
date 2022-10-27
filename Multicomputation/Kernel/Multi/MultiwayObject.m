Package["Wolfram`Multicomputation`"]

PackageExport["MultiwayObjectQ"]
PackageExport["MultiwayObject"]



MultiwayObjectQ[MultiwayObject[_Multi, _String]] := True
MultiwayObjectQ[___] := False


Options[MultiwayObject] = {Method -> Automatic}

MultiwayObject[rules_, opts : OptionsPattern[]] := MultiwayObject[rules, FirstCase[Unevaluated[rules], (Rule | RuleDelayed)[lhs_, _] :> {lhs}, All], opts]

m : MultiwayObject[rules_, init_, OptionsPattern[]] /; ! MultiwayObjectQ[Unevaluated[m]] := Enclose @ Block[{
    type = First[ConfirmBy[MultiwayType /@ wrap[rules], Apply[Equal]]],
    method
},
    method = Replace[OptionValue[Method], {
        Automatic -> Switch[type, "Hypergraph", WolframModelMulti, "String", StringMulti, _, HypergraphMulti],
        "Hypergraph" -> WolframModelMulti,
        "String" -> StringMulti,
        _ -> HypergraphMulti
    }];
    MultiwayObject[
        Evaluate[method[init, rules]],
        type
    ]
]

SetAttributes[MultiwayObject, HoldFirst]


MultiwayObjectProp[_, "Properties"] := {
    "Properties", "Multi", "Type", "StatesGraph", "CausalGraph", "BranchialGraph",
    "EvolutionCausalGraph", "EvolutionEventsGraph", "CausalBranchialGraph", "TokenEventGraph"
}

MultiwayObjectProp[HoldPattern[MultiwayObject[multi_, _]], "Multi"] := multi

MultiwayObjectProp[HoldPattern[MultiwayObject[_, type_]], "Type"] := type


StateShape[hg : {{__}...}, size_ : Automatic] := ResourceFunction["WolframModelPlot"][hg, ImageSize -> size, PlotRangePadding -> 0]
StateShape[_Missing, ___] := ""
StateShape[expr_, ___] := expr

$StateVertexShapeFunction = Function[Inset[
    Framed[
        Style[StateShape[#2, #3], Hue[0.62, 1, 0.48]],
        Background -> Directive[Opacity[0.2], Hue[0.62, 0.45, 0.87]],
        FrameMargins -> {{2, 2}, {0, 0}},
        FrameStyle -> Directive[Opacity[0.5], Hue[0.62, 0.52, 0.82]],
        RoundingRadius -> 0
    ],
    #1,
    {0, 0}
    ]
]


MultiwayObjectProp[m_, "Graph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
Block[{g},
    g = m["Multi"]["Graph", n,
        opts,
        VertexLabels -> Placed[Automatic, Tooltip],
        VertexShapeFunction -> (Tooltip[$StateVertexShapeFunction[#1, FromLinkedHypergraph[#2, type], #3], #2] &),
        VertexSize -> If[type === "Hypergraph", 64, Automatic],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]], "StateCanonicalFunction" -> Automatic];
    g
]]

MultiwayObjectProp[m_, "StatesGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
With[{type = m["Type"]},
Block[{g},
    g = Graph[
        Graph @ Flatten[
            MapThread[{from, to} |-> (DirectedEdge[from, #] & /@ to), {ReleaseHold[#1[[1]]]["Expression"], #2[[2]]}, 1] & @@@
                Partition[m["Multi"]["Foliations", n][[1]], 2, 1],
            2
        ],
        FilterRules[{opts}, Options[Graph]],
        VertexLabels -> v_ :> Placed[HoldForm[v], Tooltip],
        VertexShapeFunction -> (Tooltip[$StateVertexShapeFunction[#1, FromLinkedHypergraph[#2, type], #3], #2] &),
        VertexSize -> If[type === "Hypergraph", 64, Automatic],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]], "StateCanonicalFunction" -> Automatic];
    g
]]

StringSubstitutionEventShape[DirectedEdge[from_, to_, tag_]] := Framed[
    Style[
        Row @ SequenceReplace[
            SequenceReplace[from,
                vs : {{Alternatives @@ tag["Destroyed"], ___}...} :>
                    Column[{
                        FromLinkedHypergraph[vs, "String"],
                        FromLinkedHypergraph[Cases[to, {Alternatives @@ tag["Created"], ___}], "String"]
                    }, Alignment -> Center, Spacings -> 0]
            ],
            vs : {{__}...} :> FromLinkedHypergraph[vs, "String"]
        ],
        Black
    ],
    Background -> Directive[Opacity[0.7], Hue[0.14, 0.34, 1]],
    FrameMargins -> {{2, 2}, {0, 0}},
    FrameStyle -> Directive[Opacity[0.4], Hue[0.09, 1, 0.91]]
]

WolframModelEventShape[DirectedEdge[from_, to_, tag_], size_] := With[{
    lhs = from[[All, 3 ;;]],
    rhs = to[[All, 3 ;;]]
},
Framed[
    Style[
        If[ lhs === {{}},
            Graphics[{}, ImageSize -> size / 2],
            ResourceFunction["WolframModelPlot"][lhs,
                GraphHighlight -> Extract[lhs, Position[from, {Alternatives @@ tag["Destroyed"], ___}]],
                GraphHighlightStyle -> Dashed,
                ImageSize -> size / 2
            ]
        ] ->
            ResourceFunction["WolframModelPlot"][rhs, GraphHighlight -> Extract[rhs, Position[to, {Alternatives @@ tag["Created"], ___}]], ImageSize -> size / 2]
    ],
    Background -> Directive[Opacity[0.7], Hue[0.14, 0.34, 1]],
    FrameMargins -> {{2, 2}, {0, 0}},
    FrameStyle -> Directive[Opacity[0.4], Hue[0.09, 1, 0.91]]
]
]

DefaultEventShape[DirectedEdge[from_, to_, tag_], type_] := Style[
    Column[{
            FromLinkedHypergraph[from, type],
            FromLinkedHypergraph[to, type]
        },
        Alignment -> Center, Spacings -> 0
    ],
    Background -> Directive[Opacity[0.7], Hue[0.14, 0.34, 1]],
    FrameMargins -> {{2, 2}, {0, 0}},
    FrameStyle -> Directive[Opacity[0.4], Hue[0.09, 1, 0.91]]
]

DefaultEventShape[DirectedEdge[from_, to_, tag_], type_] := Framed[
    Style[
        Block[{
            lhsPos = Position[from, {Alternatives @@ tag["Destroyed"], ___}, {1}, Heads -> False],
            rhsPos = Position[to, {Alternatives @@ tag["Created"], ___}, {1}, Heads -> False],
            lhs, rhs, lhsRootPos, rhsRootPos
        },
            lhs = Extract[from, lhsPos];
            rhs = Extract[to, rhsPos];
            lhsRootPos = FirstPosition[from, LinkedHypergraphRoot[lhs], {1}, Heads -> False];
            rhsRootPos = FirstPosition[to, LinkedHypergraphRoot[rhs], {1}, Heads -> False];
            FromLinkedHypergraph[
                Delete[
                    ReplacePart[from, lhsRootPos -> {
                        Extract[from, lhsRootPos, First],
                        Column[{
                            TreeExpression @ LinkedHypergraphToRootTree[from, Extract[from, lhsRootPos, First]],
                            TreeExpression @ LinkedHypergraphToRootTree[to, Extract[to, rhsRootPos, First]]
                        }, Alignment -> Center, Spacings -> 0]
                    }],
                    DeleteCases[lhsPos, lhsRootPos]
                ],
                "Expression"
            ]
        ],
        Black
    ],
    Background -> Directive[Opacity[0.7], Hue[0.14, 0.34, 1]],
    FrameMargins -> {{2, 2}, {0, 0}},
    FrameStyle -> Directive[Opacity[0.4], Hue[0.09, 1, 0.91]]
]

EventShapeFunction[type_] := Switch[
    type,
    "String",
    Function[Replace[#2, event : DirectedEdge[from_, to_, tag_] :> Inset[Tooltip[StringSubstitutionEventShape[event], tag], #1, {0, 0}]]],
    "Hypergraph",
    Function[Replace[#2, event : DirectedEdge[from_, to_, tag_] :> Inset[Tooltip[WolframModelEventShape[event, #3], tag], #1, {0, 0}]]],
    _,
    Function[Replace[#2, event : DirectedEdge[from_, to_, tag_] :> Inset[Tooltip[DefaultEventShape[event, type], tag], #1, {0, 0}]]]
]

MultiwayObjectProp[m_, "CausalGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
Block[{g},
    g = m["Multi"]["CausalGraph", n,
        FilterRules[{opts}, Options[CausalGraph]],
        "IncludeInitialEvent" -> False,
        VertexLabels -> DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip],
        VertexShapeFunction -> EventShapeFunction[type],
        VertexSize -> If[type === "Hypergraph", 64, Automatic],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeEvents[g, type, FilterRules[{opts}, Options[canonicalizeEvents]]];
    g
]]

MultiwayObjectProp[m_, "TokenEventGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
Block[{g},
    g = m["Multi"]["TokenEventGraph", n,
        FilterRules[{opts}, Options[MultiTokenEventGraph]],
        "IncludeInitialEvent" -> False,
        "IncludeInitialState" -> False,
         VertexLabels -> {
            token_ :> Placed[FromLinkedHypergraph[{token}, type], Tooltip],
            DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip]
        },
        VertexShapeFunction -> {
            _DirectedEdge -> EventShapeFunction[type],
            Except[_DirectedEdge] -> Function[Tooltip[$StateVertexShapeFunction[#1, FromLinkedHypergraph[{#2}, type], #3], #2]]
        },
        VertexSize -> If[type === "Hypergraph", 64, Automatic],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeTokens[g, type, FilterRules[{opts}, Options[canonicalizeTokens]]];
    g = canonicalizeEvents[g, type, FilterRules[{opts}, Options[canonicalizeEvents]]];
    g
]]

CanonicalEventFunction[DirectedEdge[from_, to_, tag_], type_, keys_List] := Block[{fromIso, canonicalFrom, toIso, canonicalTo},
    {fromIso, canonicalFrom} = CanonicalLinkedHypergraph[from, "IncludeIsomorphism" -> True];
    {toIso, canonicalTo} = CanonicalLinkedHypergraph[to, "IncludeIsomorphism" -> True];
    DirectedEdge[
        FromLinkedHypergraph[canonicalFrom, type], FromLinkedHypergraph[canonicalTo, type],
        Part[MapAt[ReplaceAll[toIso], "Created"] @ MapAt[ReplaceAll[fromIso], "Destroyed"] @ tag, Key /@ keys]
    ]
]

Options[canonicalizeStates] = {"StateCanonicalFunction" -> None}
canonicalizeStates[g_, type_, OptionsPattern[]] := With[{
    stateCanonicalFunction = Replace[OptionValue["StateCanonicalFunction"], {
        Automatic -> Function[FromLinkedHypergraph[#, type]],
        "Canonical" -> CanonicalLinkedHypergraph
    }]
},
    If[ stateCanonicalFunction === None,
        g,
        Graph[
            VertexReplace[g, state : Except[_DirectedEdge] :> stateCanonicalFunction[state]],
            VertexShapeFunction -> Except[_DirectedEdge] -> If[
                OptionValue["StateCanonicalFunction"] === Automatic,
                $StateVertexShapeFunction,
                (Tooltip[$StateVertexShapeFunction[#1, FromLinkedHypergraph[#2, type], #3], #2] &)
            ]
        ]
    ]
]

Options[canonicalizeTokens] = {"TokenCanonicalFunction" -> None}
canonicalizeTokens[g_, type_, OptionsPattern[]] := With[{
    tokenCanonicalFunction = Replace[OptionValue["TokenCanonicalFunction"], Automatic -> Function[FromLinkedHypergraph[{#}, type]]]
},
    If[ tokenCanonicalFunction === None,
        g,
        Graph[
            VertexReplace[g, token : Except[_DirectedEdge] :> tokenCanonicalFunction[token]],
            VertexShapeFunction -> Except[_DirectedEdge] -> $StateVertexShapeFunction
        ]
    ]
]

Options[canonicalizeEvents] = {"EventCanonicalFunction" -> None}
canonicalizeEvents[g_, type_, OptionsPattern[]] := With[{
    eventCanonicalFunction = Replace[OptionValue["EventCanonicalFunction"], {
        Full -> Function[CanonicalEventFunction[#, type, {"Destroyed", "Created", "Step", "TreePosition"}]],
        Automatic -> Function[CanonicalEventFunction[#, type, {"Destroyed", "Created"}]]
    }]
},
    If[ eventCanonicalFunction === None,
        g,
        Block[{oldEvents = VertexList[g, _DirectedEdge], newEvents},
            newEvents = Replace[oldEvents, event : DirectedEdge[from_, to_, ___] :>
                Replace[eventCanonicalFunction[event], newTag : Except[_DirectedEdge] :> DirectedEdge[from , to, newTag]],
                {1}
            ];
            Graph[
                VertexReplace[g, Thread[oldEvents -> newEvents, List, 2]],
                VertexShapeFunction -> Normal @ GroupBy[
                    MapThread[
                        {oldEvent, newEvent} |-> newEvent -> Function[EventShapeFunction[type][#1, oldEvent, #3]],
                        {oldEvents, newEvents}
                    ],
                    First,
                    #[[1, 2]] &
                ]
            ]
        ]
    ]
]

MultiwayObjectProp[
    m_, "EvolutionCausalGraph", n : _Integer ? Positive : 1,
    opts : OptionsPattern[Join[{"StateCanonicalFunction" -> None, "EventCanonicalFunction" -> None}, Options[EvolutionCausalGraph]]]
] :=
Block[{g},
With[{type = m["Type"]},
    g = m["Multi"]["EvolutionCausalGraph", n,
        FilterRules[{opts}, Options[EvolutionCausalGraph]],
        "IncludeInitialEvent" -> False,
        "IncludeInitialState" -> False,
        VertexLabels -> {
            v_ :> Placed[HoldForm[v], Tooltip],
            DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip]
        },
        VertexShapeFunction -> {
            _DirectedEdge -> EventShapeFunction[type],
            Except[_DirectedEdge] -> Function[$StateVertexShapeFunction[#1, FromLinkedHypergraph[#2, type], #3]]
        },
        VertexSize -> If[type === "Hypergraph", 64, Automatic],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]]];
    g = canonicalizeEvents[g, type, FilterRules[{opts}, Options[canonicalizeEvents]]];
    g
]
]

MultiwayObjectProp[m_, "EvolutionEventsGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
    EdgeDelete[m["EvolutionCausalGraph", n, opts], DirectedEdge[_DirectedEdge, _DirectedEdge, ___]]

MultiwayObjectProp[m_, "CausalBranchialGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
    CausalBranchialGraph[m["CausalGraph", n, opts], n, FilterRules[{opts}, Options[CausalBranchialGraph]]]

MultiwayObjectProp[m_, "BranchialGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
Block[{g},
    g = Graph[
        UndirectedEdge @@@ ReleaseHold[m["Multi"]["Foliations", n - 1][[1, -1, 1]]]["BranchPairs"],
        EdgeStyle -> ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph"]["EdgeStyle"],
        VertexShapeFunction -> Function[Tooltip[$StateVertexShapeFunction[#1, FromLinkedHypergraph[#2, type], #3], #2]],
        VertexSize -> If[type === "Hypergraph", 64, Automatic],
        PerformanceGoal -> "Quality"
    ];
    g = canonicalizeStates[g, type, FilterRules[{opts}, Options[canonicalizeStates]], "StateCanonicalFunction" -> Automatic];
    g
]]

MultiwayObjectProp[m_, prop_String, opts___] /; StringEndsQ[prop, "GraphStructure"] :=
    m[StringDelete[prop, "Structure"], opts, VertexShapeFunction -> Automatic]

MultiwayObjectProp[m_, args___] := m["Multi"][args]

m_MultiwayObject[args___] /; MultiwayObjectQ[m] := MultiwayObjectProp[m, args]


MakeBoxes[mobj_MultiwayObject, form_] := With[{
    icon = ResourceFunction["WolframModelPlot"][{{9, 5}, {6, 5}, {8, 5}, {1, 2}, {1, 5}, {4, 8, 4}, {1, 3, 8}, {9, 8, 10}, {4, 7, 3}, {4, 9, 10}}]
},
    BoxForm`ArrangeSummaryBox[
        "MultiwayObject",
        mobj,
        icon,
        {{}},
        {{}},
        form
    ]
]

