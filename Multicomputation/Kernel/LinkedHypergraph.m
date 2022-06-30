Package["Wolfram`Multicomputation`"]

PackageExport["ToLinkedHypergraph"]
PackageExport["FromLinkedHypergraph"]
PackageExport["HypergraphMulti"]
PackageExport["WolframModelMulti"]
PackageExport["CanonicalLinkedHypergraph"]
PackageExport["IndexedTreeToLinkedHypergraph"]
PackageExport["UniquifyIndexedTree"]
PackageExport["LinkedHypergraphToIndexedTree"]

PackageScope["ApplyHypergraphRules"]



HypergraphToLinkedHypergraph[hg_] := Module[{perm, iso},
	{perm, iso} = ResourceFunction["FindCanonicalHypergraphIsomorphism"][hg, "IncludePermutation" -> True];
	Join[
		MapThread[Join[{#3, #2}, #1] &, {Permute[hg /. iso, perm], Permute[hg, perm], Max[iso] + Range[Length[hg]]}],
		{#, #} & /@ Range[Max[iso]]
	]
]

LinkedHypergraphToHypergraph[lhg_, OptionsPattern[]] := Select[lhg, Length[#] > 2 &][[All, 3 ;;]]

GraphToLinkedHypergraph[g_] := With[{ig = IndexGraph @ g}, MapThread[{#1, #2, Splice @ VertexOutComponent[ig, #1, {1}]} &, {VertexList[ig], VertexList[g]}]]

LinkedHypergraphToGraph[hg : {{_, _, ___} ...}, opts : OptionsPattern[]] := With[{map = Association[Rule @@@ hg[[All, ;; 2]]]},
	Graph[hg[[All, ;; 2]], Catenate @ Map[Thread[#[[;; 2]] \[DirectedEdge] With[{vs = #[[3 ;;]]}, Thread[{vs, Lookup[map, vs]}]], List, {2}] &, hg], opts]
]

treeHypergraph[t_, i_] := Module[{children = TreeChildren[t], results},
	results = FoldList[List /* Replace[{{hyperEdges_, j_}, child_} :> treeHypergraph[child, j]], {{}, i + 1}, Replace[children, None -> {}]];
	{Prepend[Join @@ results[[All, 1]], {i, TreeData[t], Splice[results[[;; - 2, 2]]]}], results[[-1, 2]]}
]

TreeToLinkedHypergraph[t_] := First @ treeHypergraph[t, 1]

IndexedTreeToLinkedHypergraph[t_] := With[{children = TreeChildren[t]},
    If[ children === None,
        Replace[{TreeData[t]}, {{_}} -> Nothing],
        Prepend[
            Catenate[IndexedTreeToLinkedHypergraph /@ children],
            Join[TreeData[t], First @* TreeData /@ children]
        ]
    ]
]


linkedHypergraphToTree[rootKey_, map_] := With[{root = map[rootKey]},
    If[ MissingQ[root],
        {rootKey},
        Tree[
            {rootKey, First @ root},
            Replace[linkedHypergraphToTree[#, map] & /@ Rest[root], {} -> None]
        ]
    ]
]


LinkedHypergraphToIndexedTree[hg : {{_, ___} ...}, opts : OptionsPattern[]] := With[{map = Select[Length[#] > 0 &] @ GroupBy[hg, First, Rest @* First]},
	Tree[linkedHypergraphToTree[First @ Complement[Keys[map], Catenate @ Values[map[[All, 2 ;;]]]], map], opts]
]

UniquifyIndexedTree[tree_Tree] := TreeMapAt[MapAt[Sow[Unique[], "UniqueIndex"] &, {1}], tree, Catenate[Rest @ TreePosition[tree, #] & /@ Keys @ Select[Counts @ TreeLeaves[tree], # > 1 &]]]

LinkedHypergraphToTree[hg : {{_, _, ___} ...}, opts : OptionsPattern[]] := TreeMap[Last] @ LinkedHypergraphToIndexedTree[hg, opts]


ListToLinkedHypergraph[l_] := With[{len = Length[l]}, Append[{len, Last[l], 0}] @ MapThread[{#1, #2, #1 + 1} &, {Range[len - 1], Most[l]}]]

LinkedHypergraphToList[hg : {{_, _, ___} ...}, OptionsPattern[]] := Enclose @ DeleteMissing @ Map[Last] @ FindHamiltonianPath @ ConfirmBy[LinkedHypergraphToGraph[hg], PathGraphQ[#] || EmptyGraphQ[#] &]

StringToLinkedHypergraph[s_] := ListToLinkedHypergraph[Characters[s]]

LinkedHypergraphToString[hg : {{_, _, ___} ...}, OptionsPattern[]] := Enclose @ StringJoin[ToString /@ ConfirmBy[LinkedHypergraphToList[hg], ListQ]]

PatternToLinkedHypergraph[expr_, patt_ : None] := If[patt === None,
	TreeToLinkedHypergraph[ExpressionTree[expr]],
	With[{map = AssociationThread[#, Table[Unique[], Length[#]]] & @ Cases[expr, patt, All]},
		ReplaceAt[Reverse /@ Normal @ map, {All, 2}] @ TreeToLinkedHypergraph[ExpressionTree[expr /. map]]
	]
]

ConstructPatternToLinkedHypergraph[expr_, patt_ : None] := If[patt === None,
	TreeToLinkedHypergraph[ExpressionTree[expr, "Atoms", Heads -> True]],
	With[{map = AssociationThread[#, Table[Unique[], Length[#]]] & @ Cases[expr, patt, All, Heads -> True]},
		ReplaceAt[Reverse /@ Normal @ map, {All, 2}] @ TreeToLinkedHypergraph[TreeMap[Replace[Null -> Construct]] @ ExpressionTree[expr /. map, "Atoms", Heads -> True]]
	]
]


ToLinkedHypergraph[expr_, type_String : Automatic] := Which[
	GraphQ[expr],
	GraphToLinkedHypergraph[expr],
	TreeQ[expr],
	TreeToLinkedHypergraph[expr],
	StringQ[expr],
	StringToLinkedHypergraph[expr],
	MatchQ[expr, _String -> _String],
	StringToLinkedHypergraph[expr[[1]]] -> StringToLinkedHypergraph[expr[[2]]],
	MatchQ[expr, {{__}...}],
	HypergraphToLinkedHypergraph[expr],
	ListQ[expr],
	ListToLinkedHypergraph[expr],
	MatchQ[expr, _Rule | _RuleDelayed],
    If[
        FreeQ[expr, _[___][___]],
	    Rule @@ {
            PatternToLinkedHypergraph[expr[[1]], _Pattern],
            PatternToLinkedHypergraph[expr[[2]], _Pattern]
        },
        Rule @@ {
            ConstructPatternToLinkedHypergraph[expr[[1]], _Pattern],
            ConstructPatternToLinkedHypergraph[expr[[2]], _Pattern]
        }
    ],
	!FreeQ[expr, _[___][___]],
    TreeToLinkedHypergraph[TreeMap[Replace[Null -> Construct]] @ ExpressionTree[expr, "Atoms", Heads -> True]],
    True,
	TreeToLinkedHypergraph[ExpressionTree[expr]]
]

ToLinkedHypergraph[expr_, patt_] := PatternToLinkedHypergraph[expr, patt]


FromLinkedHypergraph[hg : {_List...}, type_String : "Graph", opts : OptionsPattern[]] := FromLinkedHypergraph[Select[hg, Length[#] > 1 &], type, opts]

FromLinkedHypergraph[hg : {{_, _, ___} ...}, type_String : "Graph", opts : OptionsPattern[]] := Switch[
	type,
	"Tree",
	LinkedHypergraphToTree[hg, opts],
	"Expression",
	Enclose @ TreeExpression[Confirm @ LinkedHypergraphToTree[hg, opts], "Heads"],
	"HoldExpression",
	Enclose[HoldForm @@ TreeExpression[Confirm @ LinkedHypergraphToTree[hg, opts], "HeldHeads"]],
	"List",
	LinkedHypergraphToList[hg, opts],
	"String",
	LinkedHypergraphToString[hg, opts],
	"Hypergraph",
	LinkedHypergraphToHypergraph[hg, opts],
	_,
	LinkedHypergraphToGraph[hg, opts]
]


CanonicalHypergraphRule[rule : (lhs : {{__}...} -> {{__}...})] := Rule @@ TakeDrop[CanonicalHypergraph[List @@ rule, 1], Length[lhs]]

LinkedHypergraphRuleToPatternRule[rule_] := Module[{leftLinks, rightLinks, newLinks, patts, leftMap, newMap, newRule},
	{leftLinks, rightLinks} = Union @ Flatten[{#[[All, ;; 1]], #[[All, 3 ;;]]}] & /@ List @@ rule;
	newLinks = Complement[rightLinks, leftLinks];
    patts = Union @ Cases[rule[[1]], Verbatim[Pattern][s_, _] :> s, All];
	leftMap = AssociationThread[leftLinks -> (Symbol["\[FormalX]" <> ToString[#]] & /@ Range[Length[leftLinks]])];
	newMap = AssociationThread[newLinks -> (Symbol["\[FormalY]" <> ToString[#]] & /@ Range[Length[newLinks]])];
	newRule = RuleDelayed @@
				MapAt[Replace[newMap], {{2, All, 1}, {2, All, 3 ;;}}] @
				MapAt[Replace[Pattern[#, _] & /@ leftMap], {{1, All, 1}, {1, All, 3 ;;}}] @
				MapAt[Replace[leftMap], {{2, All, 1}, {2, All, 3 ;;}}] @
                MapAt[Replace[p_Pattern :> (p /. Blank -> BlankNullSequence)], {1, All, 2}] @
                (* MapAt[Replace[s : Alternatives @@ patts :> Hold[Splice @ Prepend[First[{s}]] @ Table[Unique[], Length[{s}] - 1]]], {2, All, 2}] *)
                rule;
	With[{
		newVars = Values[newMap],
		rhs = Extract[newRule, {2}, Hold]
	},
		Function[Null, ReplacePart[newRule, {2} :> Module[newVars, #]], HoldFirst] @@ rhs
	]
]

CanonicalLinkedHypergraph[lhg_] := Module[{
    perm, sorted, links, repl
},
    If[Length[lhg] == 0, Return[lhg]];
    perm = First @ ResourceFunction["FindCanonicalHypergraphIsomorphism"][lhg, "IncludePermutation" -> True];
    sorted = Permute[lhg, perm];
    links = DeleteDuplicates @ Flatten[{sorted[[All, ;; 1]], Select[sorted, Length[#] > 1 &][[All, 3 ;;]]}];
	repl = AssociationThread[links -> Range[Length[links]]];
	Join[
        MapAt[Replace[repl], #, {{1}, {3 ;;}}] & /@ Select[sorted, Length[#] > 1 &],
        MapAt[Replace[repl], #, {1}] & /@ Select[sorted, Length[#] == 1 &]
    ]
]


PatternRuleToMultiReplaceRule[rule : _[lhs_, _]] := With[{
    nothings = Unevaluated @@ Array[Nothing &, Length[lhs] - 1, Automatic, Hold @* List]
},
	ResourceFunction["SpliceAt"][
		MapAt[List, {2}] @ MapAt[Splice, {2, 2}] @ rule,
		nothings,
		{2, 2}
	]
]


ApplyHypergraphRules[x_, rules_] := With[{rhsLengths = Extract[#, {2, 1, 2, 1}, Length] & /@ rules},
    Association @ KeyValueMap[
        Module[{
            state = First[#2],
            newState, uniqueLinks, repl,
            destroyed = Union @ Extract[x, #1[[2]]][[All, 1]],
            created
        },
            created = Take[state, MapAt[UpTo, #1[[2, 1, 1]] + {0, rhsLengths[[#1[[1]]]] - 1}, {2}]][[All, 1]];
            {newState, uniqueLinks} = Reap[
                IndexedTreeToLinkedHypergraph @ UniquifyIndexedTree @ LinkedHypergraphToIndexedTree[state],
                "UniqueIndex"
            ];
            If[ Length[uniqueLinks] > 0,
                uniqueLinks = First[uniqueLinks];
                repl = AssociationThread[uniqueLinks, Max @ Select[newState[[All, 1]], IntegerQ] + Range[Length[uniqueLinks]]];
                newState = ReplaceAt[newState, repl, {{All, 1}, {All, 3 ;;}}];
                created = Join[created, Values[repl]]
            ];
            <|
                "Destroyed" -> destroyed, "Created" -> created, "Rule" -> #1[[1]], "Position" -> #1[[2]]
            |> -> newState
        ] &,
       MultiReplace[x, rules, {1}, "Mode" -> "Subsets"]
    ]
]

HypergraphMulti[init_, rules_] := With[{
	multReplaceRules = Map[ToLinkedHypergraph /* LinkedHypergraphRuleToPatternRule /* PatternRuleToMultiReplaceRule, wrap[rules]]
},
    Multi[
		ToLinkedHypergraph /@ wrap[init],
		x_ :> ApplyHypergraphRules[x, multReplaceRules],
		{1}
	]
]


ApplyWolframModelRules[x_, rules_] := Module[{
    eo = ResourceFunction["WolframModel"][<|"PatternRules" -> rules|>, x, 1],
    edges
},
    edges = eo["AllEventsEdgesList"];
    Association[
        With[{state = Join[Delete[x, List /@ #[[2, 1]]], edges[[#[[2, 2]]]]]},
            <|
                "Destroyed" -> edges[[#[[2, 1]]]][[All, 1]],
                "Created" -> edges[[#[[2, 2]]]][[All, 1]]
            |> -> state
        ] & /@ eo["AllEventsList"]
    ]
]

WolframModelMulti[init_, rules_] := With[{
	patternRules = Map[ToLinkedHypergraph /* LinkedHypergraphRuleToPatternRule, wrap[rules]]
},
    Multi[
		ToLinkedHypergraph /@ wrap[init],
		x_ :> ApplyWolframModelRules[x, patternRules],
		{1}
	]
]


