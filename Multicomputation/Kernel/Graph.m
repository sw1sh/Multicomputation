Package["Wolfram`Multicomputation`"]

PackageExport["StatesGraph"]
PackageExport["BranchialGraph"]
PackageExport["CausalGraph"]
PackageExport["EvolutionCausalGraph"]
PackageExport["CausalBranchialGraph"]
PackageExport["MultiTokenEventGraph"]

PackageExport["DirectedGraphTree"]
PackageExport["ToDirectedAcyclicGraph"]
PackageExport["AddInitState"]
PackageExport["VertexCompleteGraph"]
PackageExport["lineGraph"]
PackageExport["RemoveInitState"]
PackageExport["RemoveInitEvent"]



positionLargest[l_] := First @ FirstPosition[l, Max[l], {1}, Heads -> False]

DirectedGraphTree[g_Graph, Shortest[defaultRoot_ : Automatic], opts : OptionsPattern[]] /; AcyclicGraphQ[g] && LoopFreeGraphQ[g] := With[{
    root = Replace[defaultRoot, Automatic :> VertexList[g][[positionLargest[Length @ VertexOutComponent[g,  If[FreeQ[#, _Pattern], #, Verbatim[#]]] & /@ VertexList[g]]]]]
},
	Tree[root, Replace[DirectedGraphTree[g, #] & /@ VertexOutComponent[g, If[FreeQ[root, _Pattern], root, Verbatim[root]], {1}], {} -> None], opts]
]

ToDirectedAcyclicGraph[g_Graph, Shortest[defaultRoot_ : Automatic], opts : OptionsPattern[]] := With[{
    root = Replace[defaultRoot, Automatic :> VertexList[g][[positionLargest[Length @ VertexOutComponent[g,  If[FreeQ[#, _Pattern], #, Verbatim[#]]] & /@ VertexList[g]]]]]
},
	ResourceFunction["ToDirectedAcyclicGraph"][UndirectedGraph[g], root]
]

VertexCompleteGraph[vs_List] := With[{n = Length[vs]}, AdjacencyGraph[vs, SparseArray[{i_, i_} -> 0, {n, n}, 1]]]

BranchialGraph[tree_Tree, opts___] := Graph[
    VertexAdd[GraphUnion @@ Map[VertexCompleteGraph, DeleteCases[None] @ Reap[TreeScan[Sow, tree, All -> "OriginalChildrenData"]][[2, 1]]], {TreeData[tree]}],
    opts,
    ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph"]["Options"]
]

BranchialGraph[g_Graph, type : _String | None : None, opts___] :=
    BranchialGraph[DirectedGraphTree[g], opts, VertexLabels -> If[type === None, None, v_ :> Tooltip[FromLinkedHypergraph[v, type],  v]]]


TreeNodes[tree_] := Prepend[TreeData[tree]] @ Catenate[TreeNodes /@ Replace[TreeChildren[tree], None -> {}]]

AddInitState[g_Graph, i_Integer : 0] := EdgeAdd[
    g,
    MapIndexed[
        If[ MatchQ[#1, {_, _Integer}],
            With[{l = #1[[1, 1, 1]]},
                DirectedEdge[{{{l, Missing[]}}, #1}, #1, {{}, "InitState", None, <|"Destroyed" -> {l}, "Created" -> #1[[1, All, 1]], "Rule" -> 0, "Position" -> {}, "Step" -> i + 1|>}]
            ],
            DirectedEdge[{{i, Missing[]}}, #1, {{}, "InitState", None, <|"Destroyed" -> {i}, "Created" -> #1[[All, 1]], "Rule" -> 0, "Position" -> {}, "Step" -> i + 1|>}]
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


Options[StatesGraph] := Join[{"Hold" -> False}, Options[Graph]]

SetAttributes[StatesGraph, HoldFirst]

StatesGraph[multi_Multi, steps_Integer : 1, lvl_Integer : 2, opts : OptionsPattern[]] := Block[{i = 0, holdQ = TrueQ[OptionValue["Hold"]]},
    Graph[
        DeleteCases[Except[_DirectedEdge]] @ Flatten @ Reap[
            Nest[
                Function[
                    With[{
                        edges = (
                            i++;
                            Sow[Map[MapAt[Append[<|"Step" -> i|>], #, If[Length[#[[3]]] > 3, {3, 4}, {3}]] &] @
                                DeleteCases[DirectedEdge[_, _, _Missing]] @ ReleaseHold[#][If[holdQ, "HoldEdges", "Edges"], lvl]
                            ]
                        )
                    },
                        If[ Length @ edges > 0,
                            Hold[
                                Evaluate @ If[
                                    holdQ,
                                    Function[
                                        Null,
                                        Multi[Unevaluated @ {##}, multi["AllReplaceArguments"]],
                                        HoldAll
                                    ] @@ Flatten[HoldForm @@ #] &,
                                    Multi[#, multi["AllReplaceArguments"]] &
                                ] @ DeleteDuplicates @ edges[[All, 2]]
                            ],
                            #
                        ]
                    ]
                ],
                Hold @ multi,
                steps
            ]
        ][[2]],
        opts,
        VertexLabels -> Placed[Automatic, Tooltip]
    ]
]


Options[CausalGraph] = Join[{"IncludeInitialEvent" -> True}, Options[Graph]]

CausalGraph[g_, type : _String | None : None, opts : OptionsPattern[]] := Enclose @ Block[{gg = g, lg, nodes, positions, events, links, repl, mat},
    If[
        Count[VertexInDegree[gg], 0] > 1,
        gg = AddInitState[gg]
    ];
    gg = Graph[DirectedEdge[{#[[1]], #[[3, -1, "Step"]] - 1}, {#[[2]], #[[3, -1, "Step"]]}, #[[3]]] & /@ EdgeList[gg]];
    gg = AddInitState[gg, -1];
    lg = IndexGraph @ lineGraph @ gg;
	nodes = SortBy[First] @ TreeLevel[TreeMap[List, ConfirmBy[DirectedGraphTree[lg], TreeQ], All -> {"Data", "Position"}], All -> "Data"];
	positions = Last /@ nodes;
    events = MapAt[Last, MapAt[First, EdgeList[gg], {{All, 1}, {All, 2}}], {All, 3}];
	events = Map[MapAt[Append[{"TreePosition" -> #[[2]], "Index" -> #[[1]]}], events[[#[[1]]]], {3}] &, nodes];
    If[!TrueQ[OptionValue["IncludeInitialEvent"]], events = DeleteCases[events, DirectedEdge[{{_, _Missing}}, __]]];
    If[ AllTrue[events, KeyExistsQ[#[[3]], "Destroyed"] && KeyExistsQ[#[[3]], "Created"] &],
        links = Union @@ Catenate[Values /@ events[[All, 3, {"Destroyed", "Created"}]]];
        repl = AssociationThread[links, Range[Length[links]]];
        events = MapAt[Replace[repl], events, {{All, 3, Key["Destroyed"], All}, {All, 3, Key["Created"], All}, {All, 1, All, 1}, {All, 1, All, 3;;}, {All, 2, All, 1}, {All, 2, All, 3;;}}];
        mat = Outer[
            Boole[#1["Step"] < #2["Step"] && MatchQ[#2["TreePosition"], Append[#1["TreePosition"], __]] && IntersectingQ[#1["Created"], #2["Destroyed"]]] &,
            events[[All, 3]],
            events[[All, 3]]
        ],
        mat = Outer[
            Boole[#1["Step"] < #2["Step"] && MatchQ[#2["TreePosition"], Append[#1["TreePosition"], __]]] &,
            events[[All, 3]],
            events[[All, 3]]
        ]
    ];
	AdjacencyGraph[
		events,
        mat,
		FilterRules[{opts}, Options[Graph]],
		VertexLabels -> If[type === None, None, DirectedEdge[_, _, tag_] :> Placed[tag, Tooltip]],
		GraphLayout -> "LayeredDigraphEmbedding",
		ResourceFunction["WolframPhysicsProjectStyleData"]["CausalGraph"]["Options"]
	]
]

Options[EvolutionCausalGraph] = Join[{"IncludeInitialState" -> True, "IncludeInitialEvent" -> True}, Options[Graph]]
EvolutionCausalGraph[cg_Graph, type : _String | None : None, opts : OptionsPattern[]] := Block[{multiwayEdges, states},
    multiwayEdges = Catenate[{DirectedEdge[#[[1]], #], DirectedEdge[#, #[[2]]]} & /@ VertexList[cg]];
    states = Union[VertexList[cg][[All, 1]], VertexList[cg][[All, 2]]];
    If[!TrueQ[OptionValue["IncludeInitialState"]],
        states = DeleteCases[states, {{_, _Missing}}];
        multiwayEdges = DeleteCases[multiwayEdges, DirectedEdge[{{_, _Missing}}, __]]
    ];
    Graph[
        If[TrueQ[OptionValue["IncludeInitialEvent"]], Identity, VertexDelete[#, DirectedEdge[{{_, _Missing}}, __]] &] @
            EdgeAdd[cg, multiwayEdges, VertexLabels -> None],
        opts,
        EdgeStyle -> (# -> ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["EdgeStyle"] & /@ multiwayEdges),
        VertexLabels -> If[type === None, None, # -> Automatic & /@ states],
        VertexStyle -> (# -> Directive[ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["VertexStyle"], EdgeForm[]] & /@ states)
    ]
]


CausalBranchialGraph[cg_Graph, n_Integer : 1, opts : OptionsPattern[]] := Block[{
    branchEdges = SubsetCases[VertexList[cg], {
        a : DirectedEdge[_, _, KeyValuePattern[{"Destroyed" -> x_, "TreePosition" -> {p___, _}}]],
        b : DirectedEdge[_, _, KeyValuePattern[{"Destroyed" -> y_, "TreePosition" -> {p___, _}}]]
    } /; IntersectingQ[x, y] :> UndirectedEdge[a, b], Overlaps -> True],
    slideBranchEdges
},
    slideBranchEdges = Join[branchEdges, Flatten[
        Table[
            Outer[
                UndirectedEdge,
                VertexOutComponent[cg, #[[1]], {i}],
                VertexOutComponent[cg, #[[2]], {i}]
            ] & /@ branchEdges,
            {i, n}
        ],
        3
    ]];
    Graph[
        EdgeAdd[cg, slideBranchEdges],
        opts,
        EdgeShapeFunction -> (#1 -> Function[{
            ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph"]["EdgeStyle"], StadiumShape[#1[[{1, -1}]], .05]}
        ] &) /@ branchEdges
    ]
]

Options[MultiTokenEventGraph] = Join[{"IncludeInitialState" -> True, "IncludeInitialEvent" -> True}, Options[Graph]]
MultiTokenEventGraph[cg_Graph, type : _String | None : None, opts : OptionsPattern[]] := Block[{
    edges = Flatten @ Replace[
        VertexList[cg],
		    event : DirectedEdge[from_, to_, tag_] :> Join[
			    Cases[from, token : {Alternatives @@ tag["Destroyed"], ___} :> DirectedEdge[token, event]],
			    Cases[to, token : {Alternatives @@ tag["Created"], ___} :> DirectedEdge[event, token]]
            ],
        {1}
    ],
    tokens
},
    tokens = DeleteDuplicates @ Cases[Vertex, {DirectedEdge[token_, _DirectedEdge] :> token, DirectedEdge[_DirectedEdge, token_] :> token}];
    If[!TrueQ[OptionValue["IncludeInitialState"]],
        tokens = DeleteCases[tokens, {_, _Missing}];
        edges = DeleteCases[edges, DirectedEdge[{_, _Missing}, __]]
    ];
    Graph[
        If[TrueQ[OptionValue["IncludeInitialEvent"]], Identity, VertexDelete[#, DirectedEdge[{{_, _Missing}}, __]] &] @
            EdgeAdd[cg, edges, VertexLabels -> None],
        FilterRules[{opts}, Options[Graph]],
        EdgeStyle -> (# -> ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["EdgeStyle"] & /@ edges),
        VertexLabels -> If[type === None, None, # -> Automatic & /@ tokens],
        VertexStyle -> (# -> Directive[ResourceFunction["WolframPhysicsProjectStyleData"]["StatesGraph"]["VertexStyle"], EdgeForm[]] & /@ tokens)
    ]
]

