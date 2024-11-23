Package["Wolfram`Multicomputation`"]

PackageExport["EvolutionGraph"]
PackageExport["BranchialGraph"]
PackageExport["CausalGraph"]
PackageExport["EvolutionCausalGraph"]
PackageExport["CausalBranchialGraph"]
PackageExport["EvolutionBranchialGraph"]
PackageExport["MultiTokenEventGraph"]
PackageExport["CausalStatesGraph"]

PackageExport["DirectedGraphTree"]
PackageExport["ToDirectedAcyclicGraph"]
PackageExport["AddInitState"]
PackageExport["VertexCompleteGraph"]
PackageExport["RemoveInitState"]
PackageExport["RemoveInitEvent"]

PackageScope["lineGraph"]



positionLargest[l_] := First @ FirstPosition[l, Max[l], {{}}, Heads -> False]

DirectedGraphTree[g_Graph, Shortest[defaultRoot_ : Automatic], opts : OptionsPattern[]] /; AcyclicGraphQ[g] && LoopFreeGraphQ[g] :=
    If[ VertexCount[g] == 0,
        Tree[{}],
        With[{
            root = Replace[defaultRoot, Automatic :> VertexList[g][[positionLargest[Length @ VertexOutComponent[g, If[FreeQ[#, _Pattern], #, Verbatim[#]]] & /@ VertexList[g]]]]]
        },
            Tree[root, Replace[DirectedGraphTree[g, #] & /@ VertexOutComponent[g, If[FreeQ[root, _Pattern], root, Verbatim[root]], {1}], {} -> None], opts]
        ]
    ]

ToDirectedAcyclicGraph[g_Graph, Shortest[defaultRoot_ : Automatic], opts : OptionsPattern[]] := With[{
    root = Replace[defaultRoot, Automatic :> VertexList[g][[positionLargest[Length @ VertexOutComponent[g,  If[FreeQ[#, _Pattern], #, Verbatim[#]]] & /@ VertexList[g]]]]]
},
	ResourceFunction["ToDirectedAcyclicGraph"][UndirectedGraph[g], root]
]

VertexCompleteGraph[vs_List] := With[{n = Length[vs]}, AdjacencyGraph[vs, SparseArray[{i_, i_} -> 0, {n, n}, 1]]]

Options[BranchialGraph] = Options[Graph]

BranchialGraph[tree_Tree, opts : OptionsPattern[]] := Graph[
    VertexAdd[GraphUnion @@ Map[VertexCompleteGraph, DeleteCases[None] @ Reap[TreeScan[Sow, tree, All -> "OriginalChildrenData"]][[2, 1]]], {TreeData[tree]}],
    opts,
    ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph"]["Options"]
]

BranchialGraph[g_Graph, type : _String | None : None, opts : OptionsPattern[]] :=
    BranchialGraph[DirectedGraphTree[g], opts, VertexLabels -> If[type === None, None, v_ :> Tooltip[FromLinkedHypergraph[v, type],  v]]]


TreeNodes[tree_] := Prepend[TreeData[tree]] @ Catenate[TreeNodes /@ Replace[TreeChildren[tree], None -> {}]]

AddInitState[g_Graph, i_Integer : 0] := EdgeAdd[
    g,
    MapIndexed[
        Which[
            MatchQ[#1, {_ ? LinkedHypergraphQ, _Integer}],
            With[{l = #1[[1, 1, 1]]},
                DirectedEdge[{{{l, Missing[i]}}, #1[[2]] - 1}, #1, <|"Input" -> {l}, "Output" -> #1[[1, All, 1]], "Rule" -> 0, "Position" -> {{1}}, "MatchPositions" -> {{}}, "MatchType" -> "Init", "Step" -> i + 1|>]
            ],
            MatchQ[#1, _ ? LinkedHypergraphQ],
            DirectedEdge[{{i, Missing[i]}}, #1, <|"Input" -> {i}, "Output" -> #1[[All, 1]], "Rule" -> 0, "Position" -> {{1}}, "MatchPositions" -> {{}}, "MatchType" -> "Init", "Step" -> i + 1|>],
            True,
            DirectedEdge[{Missing[i]}, #1, <|"Rule" -> 0, "Position" -> {{1}}, "MatchPositions" -> {{}}, "MatchType" -> "Init", "Step" -> i + 1|>]
        ] &,
        Pick[VertexList[g], Thread[VertexInDegree[g] == 0]]
    ]
]


lineGraph[g_, opts : OptionsPattern[Graph]] := With[{
    edges = DeleteDuplicates @ Catenate[(v |-> DirectedEdge @@@
        Tuples[{
            Cases[EdgeList[g], DirectedEdge[_, Verbatim[v], ___]],
            Cases[EdgeList[g], DirectedEdge[Verbatim[v], __]]}]) /@ VertexList[g]
    ]
},
    Graph[
        EdgeList[g], edges,
        (* VertexStyle -> AnnotationValue[g, EdgeStyle],
        VertexLabels -> Placed[EdgeList[g], Tooltip],
        VertexLabelStyle -> AnnotationValue[g, EdgeLabelStyle],
        EdgeLabels -> Placed["EdgeTag", Tooltip], *)
        opts
    ]
]

removeCycles[g_] := EdgeDelete[EdgeDelete[g, FindCycle[g, Infinity, All][[All, -1]]], _[x_, x_, ___]]


Options[EvolutionGraph] := Join[{"Hold" -> False, "RetraceVisited" -> True}, Options[Graph]]

SetAttributes[EvolutionGraph, HoldFirst]

EvolutionGraph[multi_Multi, steps_Integer : 1, lvl_Integer : 2, opts : OptionsPattern[]] := Block[{i = 0, holdQ = TrueQ[OptionValue["Hold"]], visited = {}},
    Graph[
        If[ holdQ,
            If[MatchQ[#, HoldForm[_List]], First @ Map[HoldForm, #, {2}], {#}] & @ multi["HoldExpression"],
            Developer`ToList[multi["Expression"]]
        ],
        DeleteCases[Except[_DirectedEdge]] @ Flatten @ Reap[
            Nest[
                Function[
                    With[{
                        edges = (
                            i++;
                            Sow[Map[
                                ReplacePart[#, {3} ->
                                    Append[
                                        If[Length[#[[3]]] > 3, #[[3, 4]], <||>],
                                        <|If[MatchQ[#[[3]], {{___Integer} ...}], {"MatchPositions" -> #[[3]], "MatchType" -> "Value"}, {"MatchPositions" -> #[[3, 1]], "MatchType" ->  #[[3, 2]]}], "Step" -> i|>
                                    ]
                                ] &] @
                                DeleteCases[DirectedEdge[_, _, _Missing]] @ ReleaseHold[#][If[holdQ, "HoldEdges", "Edges"], lvl]
                            ]
                        )
                    },
                        If[ Length @ edges > 0,
                            Block[{outputs = DeleteDuplicates @ edges[[All, 2]]},
                                If[ ! TrueQ[OptionValue["RetraceVisited"]],
                                    visited = Union[visited, outputs];
                                    outputs = Complement[outputs, visited]
                                ];
                                Hold[
                                    Evaluate @ If[
                                        holdQ,
                                        Function[
                                            Null,
                                            Multi[Unevaluated @ {##}, multi["AllReplaceArguments"]],
                                            HoldAll
                                        ] @@ Flatten[HoldForm @@ #] &,
                                        Multi[#, multi["AllReplaceArguments"]] &
                                    ] @ outputs
                                ]
                            ],
                            #
                        ]
                    ]
                ],
                Hold @ multi,
                steps
            ]
        ][[2]],
        FilterRules[{opts}, Options[Graph]],
        VertexLabels -> Placed[Automatic, Tooltip],
        EdgeStyle -> ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph", "EdgeStyle"]
    ]
]


Options[CausalGraph] = Join[{"IncludeInitialEvent" -> True, "TransitiveReduction" -> True, "EdgeDeduplication" -> True, "CanonicalizeStates" -> False}, Options[Graph]]

CausalGraph[g_, type : _String | None : None, opts : OptionsPattern[]] := Enclose @ Block[{gg = g, i = -1, lg, nodes, positions, events, links, repl, mat},
    gg = Graph[DirectedEdge[{#[[1]], #[[3, "Step"]] - 1}, {#[[2]], #[[3, "Step"]]}, #[[3]]] & /@ EdgeList[gg]];
    If[
        Count[VertexInDegree[gg], 0] > 1,
        gg = AddInitState[gg, i--]
    ];
    gg = AddInitState[gg, i];
    lg = IndexGraph @ lineGraph @ gg;
    nodes = If[
        VertexCount[lg] == 0,
        {},
	    SortBy[First] @ TreeLevel[TreeMap[List, ConfirmBy[DirectedGraphTree[lg], TreeQ], All -> {"Data", "Position"}], All -> "Data"]
    ];
	positions = Last /@ nodes;
    events = MapAt[First, EdgeList[gg], {{All, 1}, {All, 2}}];
	events = Map[MapAt[Append[{"TreePosition" -> #[[2]], "Index" -> #[[1]]}], events[[#[[1]]]], {3}] &, nodes];
    If[ AllTrue[events, KeyExistsQ[#[[3]], "Input"] && KeyExistsQ[#[[3]], "Output"] &],
        If[ TrueQ[OptionValue["CanonicalizeStates"]],
            links = Union @ Flatten @ Join[events[[All, 1, All, ;; 1]], events[[All, 1, All, 3 ;;]], events[[All, 2, All, ;; 1]], events[[All, 2, All, 3 ;;]]];
            repl = AssociationThread[links, Range[Length[links]]];
            events = MapAt[Replace[repl], events, {{All, 3, Key["Input"], All}, {All, 3, Key["Output"], All}, {All, 1, All, 1}, {All, 1, All, 3;;}, {All, 2, All, 1}, {All, 2, All, 3;;}}]
        ];
        If[!TrueQ[OptionValue["IncludeInitialEvent"]], events = DeleteCases[events, DirectedEdge[{{_, _Missing}}, __]]];
        mat = Outer[
            Boole[
                #1["Step"] < #2["Step"] &&
                MatchQ[#2["TreePosition"], Append[#1["TreePosition"], __]] &&
                IntersectingQ[#1["Output"], #2["Input"]]
            ] &,
            events[[All, 3]],
            events[[All, 3]]
        ],
        If[!TrueQ[OptionValue["IncludeInitialEvent"]], events = DeleteCases[events, DirectedEdge[{{_, _Missing}}, __]]];
        mat = Outer[
            Boole[
                #1["Step"] < #2["Step"] &&
                MatchQ[#2["TreePosition"], Append[#1["TreePosition"], __]] &&
                (AnyTrue[#2["MatchPositions"], MatchQ[Alternatives @@ Append[___] /@ #1["MatchPositions"]]] ||
                AnyTrue[#1["MatchPositions"], MatchQ[Alternatives @@ Append[___] /@ #2["MatchPositions"]]])
            ] &,
            events[[All, 3]],
            events[[All, 3]]
        ]
    ];
	gg = AdjacencyGraph[
		events,
        mat,
		FilterRules[{opts}, Options[Graph]],
		VertexLabels -> If[type === None, None, DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip]],
		GraphLayout -> "LayeredDigraphEmbedding",
		EdgeStyle -> ResourceFunction["WolframPhysicsProjectStyleData"]["CausalGraph"]["EdgeStyle"],
        VertexStyle -> ResourceFunction["WolframPhysicsProjectStyleData"]["EvolutionCausalGraph"]["EventVertexStyle"]
	];
    gg = Switch[OptionValue["TransitiveReduction"],
        True,
        TransitiveReductionGraph[gg],
        False,
        gg,
        _,
        TransitiveClosureGraph[gg]
    ];
    If[ !TrueQ[OptionValue["EdgeDeduplication"]],
        gg = EdgeAdd[gg, Catenate[Table[#, Max[0, Length[Intersection[#[[1, 3]]["Output"], #[[2, 3]]["Input"]]] - 1]] & /@ EdgeList[gg]]]
    ];
    (* gg = Graph[VertexList[gg], Insert[#, Intersection[#[[1, 3]]["Output"], #[[2, 3]]["Input"]], 3] & /@ EdgeList[gg], Options[gg]]; *)
    gg
]

Options[EvolutionCausalGraph] = Join[{"IncludeInitialState" -> True, "IncludeInitialEvent" -> True}, Options[Graph]]
EvolutionCausalGraph[cg_Graph, type : _String | None : None, opts : OptionsPattern[]] := Block[{multiwayEdges, states},
    multiwayEdges = Catenate[{DirectedEdge[#[[1]], #], DirectedEdge[#, #[[2]]]} & /@ VertexList[cg]];
    states = Union[VertexList[cg][[All, 1]], VertexList[cg][[All, 2]]];
    If[!TrueQ[OptionValue["IncludeInitialState"]],
        states = DeleteCases[states, {{_, _Missing}}];
        multiwayEdges = DeleteCases[multiwayEdges, DirectedEdge[{{_, _Missing}}, __] | DirectedEdge[_, {{_, _Missing}}, ___]]
    ];
    Graph[
        If[TrueQ[OptionValue["IncludeInitialEvent"]], Identity, VertexDelete[#, DirectedEdge[{{_, _Missing}}, __]] &] @
            EdgeAdd[cg, multiwayEdges, VertexLabels -> None],
        FilterRules[{opts}, Options[Graph]],
        EdgeStyle -> (# -> ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["EdgeStyle"] & /@ multiwayEdges),
        VertexLabels -> If[type === None, None, # -> Automatic & /@ states],
        VertexStyle -> (# -> Directive[ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["VertexStyle"], EdgeForm[]] & /@ states)
    ]
]

Options[CausalBranchialGraph] = Join[{"Slide" -> False}, Options[Graph]]
CausalBranchialGraph[cg_Graph, n_Integer : 1, opts : OptionsPattern[]] := Block[{
    branchEdges = SubsetCases[VertexList[cg], {
        a : DirectedEdge[_, _, KeyValuePattern[{"Input" -> x_, "TreePosition" -> {p___, _}}]],
        b : DirectedEdge[_, _, KeyValuePattern[{"Input" -> y_, "TreePosition" -> {p___, _}}]]
    } /; IntersectingQ[x, y] :> UndirectedEdge[a, b], Overlaps -> True],
    slideBranchEdges
},
    slideBranchEdges = If[ TrueQ[OptionValue["Slide"]],
        Join[branchEdges, Flatten[
            Table[
                Outer[
                    UndirectedEdge,
                    VertexOutComponent[cg, #[[1]], {i}],
                    VertexOutComponent[cg, #[[2]], {i}]
                ] & /@ branchEdges,
                {i, n}
            ],
            3
        ]],
        branchEdges
    ];
    Graph[
        EdgeAdd[cg, slideBranchEdges],
        FilterRules[{opts}, Options[Graph]],
        VertexCoordinates -> Thread[VertexList[cg] -> GraphEmbedding[cg]],
        EdgeStyle -> (#1 -> Directive[
            ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph"]["EdgeStyle"], Thick, Dashed
        ] &) /@ slideBranchEdges
    ]
]

Options[EvolutionBranchialGraph] = Options[Graph]
EvolutionBranchialGraph[g_Graph, opts : OptionsPattern[]] := Block[{
    branchEdges = SubsetCases[EdgeList[g], {
        DirectedEdge[_, a_, KeyValuePattern[{"Input" -> x_, "TreePosition" -> {p___, _}}]],
        DirectedEdge[_, b_, KeyValuePattern[{"Input" -> y_, "TreePosition" -> {p___, _}}]]
    } /; IntersectingQ[x, y] :> UndirectedEdge[a, b], Overlaps -> True]
},
    Graph[
        EdgeAdd[g, branchEdges],
        opts,
        VertexCoordinates -> Thread[VertexList[g] -> GraphEmbedding[g]],
        EdgeStyle -> (#1 -> Directive[
            ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph"]["EdgeStyle"], Thick, Dashed
        ] &) /@ branchEdges
    ]
]

Options[MultiTokenEventGraph] = Join[{"IncludeInitialState" -> True, "IncludeInitialEvent" -> True, "CausalEdges" -> False, "Events" -> True}, Options[Graph]]
MultiTokenEventGraph[cg_Graph, type : _String | None : None, opts : OptionsPattern[]] := Block[{
    edges = Flatten @ Replace[
        VertexList[cg],
        event : DirectedEdge[from_, to_, tag_] :> If[TrueQ[OptionValue["Events"]],
            If[ KeyExistsQ[tag, "SubEvents"],
                Catenate[
                    Join[
                        Cases[from, token : {Alternatives @@ #["Input"], ___} :> DirectedEdge[token, DirectedEdge[from, to, Join[KeyDrop[tag, "SubEvents"], #]]]],
                        Cases[to, token : {Alternatives @@ #["Output"], ___} :> DirectedEdge[DirectedEdge[from, to, Join[KeyDrop[tag, "SubEvents"], #]], token]]
                    ] & /@ tag["SubEvents"][[All, 1]]
                ],
                Join[
                    Cases[from, token : {Alternatives @@ tag["Input"], ___} :> DirectedEdge[token, event]],
                    Cases[to, token : {Alternatives @@ tag["Output"], ___} :> DirectedEdge[event, token]]
                ]
            ],
            If[ KeyExistsQ[tag, "SubEvents"],
                Catenate[
                    DirectedEdge @@@ Tuples[{Cases[from, {Alternatives @@ #["Input"], ___}], Cases[to, {Alternatives @@ #["Output"], ___}]}] & /@ tag["SubEvents"][[All, 1]]
                ],
                DirectedEdge @@@ Tuples[{Cases[from, {Alternatives @@ tag["Input"], ___}], Cases[to, {Alternatives @@ tag["Output"], ___}]}]
            ]
        ],
        {1}
    ],
    tokens
},
    tokens = DeleteDuplicates @ Cases[edges, {DirectedEdge[token_, _DirectedEdge] :> token, DirectedEdge[_DirectedEdge, token_] :> token}];
    If[ ! TrueQ[OptionValue["IncludeInitialState"]],
        tokens = DeleteCases[tokens, {_, _Missing}];
        edges = DeleteCases[edges, DirectedEdge[{_, _Missing}, __] | DirectedEdge[_, {_, _Missing}, ___]]
    ];
    Graph[
        If[ TrueQ[OptionValue["Events"]],
            EdgeAdd[
                If[TrueQ[OptionValue["CausalEdges"]], Identity, If[EdgeCount[#] > 0, EdgeDelete[#, DirectedEdge[_DirectedEdge, _DirectedEdge, ___]], #] &] @
                    If[TrueQ[OptionValue["IncludeInitialEvent"]], Identity, VertexDelete[#, DirectedEdge[{{_, _Missing}}, __]] &] @
                        VertexDelete[cg, DirectedEdge[_, _, tag_] /; KeyExistsQ[tag, "SubEvents"]],
                edges
            ],
            edges
        ],
        FilterRules[{opts}, Options[Graph]],
        EdgeStyle -> (# -> ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["EdgeStyle"] & /@ edges),
        VertexLabels -> If[type === None, None, # -> Automatic & /@ tokens],
        VertexStyle -> (# -> Directive[ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["VertexStyle"], EdgeForm[]] & /@ tokens)
    ]
]


Options[CausalStatesGraph] = Join[{"TransitiveReduction" -> False}, Options[Graph]];

CausalStatesGraph[cg_, opts : OptionsPattern[]] := Graph[
	Most[VertexList[cg]],
    FilterRules[{opts, VertexSize -> 128}, Options[Graph]],
	VertexShapeFunction -> Function[
		Inset[Tooltip[Framed[
			If[TrueQ[OptionValue["TransitiveReduction"]], TransitiveReductionGraph, Identity] @ VertexInComponentGraph[
				cg,
				VertexList[cg, DirectedEdge[_, Interpretation[#2, _] | #2,___]],
				EdgeStyle -> Directive[
					Arrowheads[0.05],
					ResourceFunction["WolframPhysicsProjectStyleData"]["CausalGraph", "EdgeStyle"]
				],
				ImageSize -> #3
			],
			Background -> LightGray], #2],
			#1
		]
	],
    GraphLayout -> "LayeredDigraphEmbedding",
    PerformanceGoal -> "Quality"
] // VertexReplace[#, Catenate @ KeyValueMap[Thread[#2 -> #1, List, 1] &] @ GroupBy[VertexList[#], Replace[Interpretation[e_, _] :> e]]] &

