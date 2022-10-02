Package["Wolfram`Multicomputation`"]

PackageExport["MultiwayObjectQ"]
PackageExport["MultiwayObject"]



MultiwayObjectQ[MultiwayObject[_Multi, _String]] := True
MultiwayObjectQ[___] := False


m : MultiwayObject[rules_, init_] /; ! MultiwayObjectQ[Unevaluated[m]] := Enclose @ With[{
    type = First[ConfirmBy[MultiwayType /@ wrap[rules], Apply[Equal]]]
},
    MultiwayObject[Evaluate[HypergraphMulti[init, rules]], type]
]

SetAttributes[MultiwayObject, HoldFirst]



MultiwayObjectProp[HoldPattern[MultiwayObject[multi_, _]], "Multi"] := multi

MultiwayObjectProp[HoldPattern[MultiwayObject[_, type_]], "Type"] := type

MultiwayObjectProp[m_, "Graph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
    VertexReplace[
        m["Multi"]["Graph", n,
            opts,
            VertexShapeFunction -> (ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["VertexShapeFunction"][#1, FromLinkedHypergraph[#2, type], #3] &),
            GraphLayout -> "LayeredDigraphEmbedding",
            PerformanceGoal -> "Quality"
        ],
        v_ :> CanonicalLinkedHypergraph[v]
    ]
]

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

MultiwayObjectProp[m_, "CausalGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
    m["Multi"]["CausalGraph", n,
        opts,
        VertexLabels -> None,
        VertexShapeFunction ->
            Switch[
                type,
                "String",
                Function[Replace[#2, event : DirectedEdge[from_, to_, tag_] :> Inset[Tooltip[StringSubstitutionEventShape[event], tag], #1, #3]]],
                _,
                Function[Replace[#2, event : DirectedEdge[from_, to_, tag_] :> Inset[Tooltip[{
                    FromLinkedHypergraph[from, type],
                    FromLinkedHypergraph[to, type]
                }, tag], #1, #3]]]
            ],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality"
    ]
]

MultiwayObjectProp[m_, "EvolutionCausalGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] := With[{type = m["Type"]},
    VertexReplace[
        m["Multi"]["EvolutionCausalGraph", n,
            opts,
            VertexLabels -> None,
            VertexShapeFunction -> {
                _DirectedEdge -> Switch[
                    type,
                    "String",
                    Function[Replace[#2, event : DirectedEdge[from_, to_, tag_] :> Inset[Tooltip[StringSubstitutionEventShape[event], tag], #1, #3]]],
                    _,
                    Function[Replace[#2, event : DirectedEdge[from_, to_, tag_] :> Inset[Tooltip[{
                        FromLinkedHypergraph[from, type],
                        FromLinkedHypergraph[to, type]
                    }, tag], #1, #3]]]
                ],
                Except[_DirectedEdge] -> Function[ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["VertexShapeFunction"][#1, FromLinkedHypergraph[#2, type], #3]]
            },
            GraphLayout -> "LayeredDigraphEmbedding",
            PerformanceGoal -> "Quality"
        ],
        v : Except[_DirectedEdge] :> CanonicalLinkedHypergraph[v]
    ]
]

MultiwayObjectProp[m_, "EvolutionEventsGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
    EdgeDelete[m["EvolutionCausalGraph", n, opts], DirectedEdge[_DirectedEdge, _DirectedEdge, ___]]

MultiwayObjectProp[m_, "CausalBranchialGraph", n : _Integer ? Positive : 1, opts : OptionsPattern[]] :=
    CausalBranchialGraph[m["CausalGraph", n], n, opts]

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

