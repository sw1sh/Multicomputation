Package["Wolfram`Multicomputation`"]

PackageExport["BranchialGraph"]
PackageExport["CausalGraph"]

PackageExport["DirectedGraphTree"]
PackageExport["AddInitState"]
PackageExport["VertexCompleteGraph"]



DirectedGraphTree[g_Graph, Shortest[defaultRoot_ : Automatic], opts : OptionsPattern[]] := With[{
    root = Replace[defaultRoot, Automatic -> VertexList[g][[First @ PositionLargest[Length @ VertexOutComponent[g,  If[FreeQ[#, _Pattern], #, Verbatim[#]]] & /@ VertexList[g]]]]]
},
	Tree[root, Replace[DirectedGraphTree[g, #] & /@ VertexOutComponent[g, If[FreeQ[root, _Pattern], root, Verbatim[root]], {1}], {} -> None], opts]
]

VertexCompleteGraph[vs_List] := With[{n = Length[vs]}, AdjacencyGraph[vs, SparseArray[{i_, i_} -> 0, {n, n}, 1]]]

BranchialGraph[tree_Tree] := Graph[
    VertexAdd[GraphUnion @@ Map[VertexCompleteGraph, DeleteCases[None] @ Reap[TreeScan[Supplied["OriginalChildrenData", Sow[#2] &], tree]][[2, 1]]], {TreeData[tree]}],
    ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph"]["Options"]
]


TreeNodes[tree_] := Prepend[TreeData[tree]] @ Catenate[TreeNodes /@ Replace[TreeChildren[tree], None -> {}]]

AddInitState[g_Graph, i_Integer : 0] := EdgeAdd[
    g,
    MapIndexed[
        DirectedEdge[{{i, Missing[]}}, #1, {{}, "InitState", None, <|"Destroyed" -> {i}, "Created" -> #1[[All, 1]], "Rule" -> 0, "Position" -> {}|>}] &,
        Pick[VertexList[g], Thread[VertexInDegree[g] == 0]]
    ]
]


lineGraph[g_, opts : OptionsPattern[Graph]] := With[{
    edges = DeleteDuplicates @ Catenate[(v |-> DirectedEdge[##, #1[[2]]] & @@@
         Tuples[{Catenate[Cases[EdgeList[g], DirectedEdge[_, v, ___]]& /@ VertexInComponent[g, v, {1}]],
         Catenate[Cases[EdgeList[g], DirectedEdge[v, __]]& /@ VertexOutComponent[g, v, {1}]]}]) /@ VertexList[g]
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

Options[CausalGraph] = Options[Graph]

CausalGraph[g_, type_String : "Graph", opts : OptionsPattern[]] := Enclose @ Module[{gg = g, lg, nodes, positions, events, root, links, repl},
    If[
        Count[VertexInDegree[gg], 0] > 1,
        gg = AddInitState[gg]
    ];
    gg = AddInitState[gg, -1];
    lg = IndexGraph @ lineGraph @ gg;
	nodes = SortBy[First] @ TreeNodes @ TreeMap[Supplied["Position", List], ConfirmBy[DirectedGraphTree[lg], TreeQ]];
	positions = Last /@ nodes;
    events = MapAt[Last, EdgeList[gg], {All, 3}];
	events = Map[MapAt[Append[{"TreePosition" -> #[[2]], "Index" -> #[[1]]}], events[[#[[1]]]], {3}] &, nodes];
    (* If[ TrueQ[OptionValue["AddInitEvent"]],
        root = events[[ FirstPosition[positions, {}, {1}, Heads -> False][[1]] ]];
        events = Prepend[MapAt[Prepend[1], events, {All, 3, Key["TreePosition"]}],
            DirectedEdge[{}, root[[1]], <|"Destroyed" -> {}, "Created" -> root[[1]][[All, 1]], "Position" -> {}, "TreePosition" -> {}, "Index" -> 0|>
        ]]
    ]; *)
    links = Union @@ Catenate[Values /@ events[[All, 3, {"Destroyed", "Created"}]]];
    repl = AssociationThread[links, Range[Length[links]]];
    events = MapAt[Map[Replace[repl]], events, {{All, 3, Key["Destroyed"]}, {All, 3, Key["Created"]}}];
	AdjacencyGraph[
		events,
		SparseArray @ Outer[Boole[MatchQ[#2["TreePosition"], Append[#1["TreePosition"], __]] && IntersectingQ[#1["Created"], #2["Destroyed"]]] &, events[[All, 3]], events[[All, 3]]],
		FilterRules[{opts}, Options[Graph]],
		VertexLabels -> v_ :> Tooltip[FromLinkedHypergraph[v[[2]], type],  v[[3]]],
		GraphLayout -> "LayeredDigraphEmbedding",
		ResourceFunction["WolframPhysicsProjectStyleData"]["CausalGraph"]["Options"]
	]
]
