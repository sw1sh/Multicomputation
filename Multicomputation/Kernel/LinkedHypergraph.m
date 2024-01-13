Package["Wolfram`Multicomputation`"]

PackageImport["Wolfram`MulticomputationInit`"]
PackageImport["WolframInstitute`Hypergraph`"]

PackageExport["LinkedHypergraphQ"]
PackageExport["ToLinkedHypergraph"]
PackageExport["FromLinkedHypergraph"]
PackageExport["HypergraphMulti"]
PackageExport["WolframModelMulti"]
PackageExport["WIHypergraphMulti"]
PackageExport["StringMulti"]
PackageExport["CAMulti"]
PackageExport["CanonicalLinkedHypergraph"]
PackageExport["IndexedTreeToLinkedHypergraph"]
PackageExport["LinkedHypergraphRoot"]
PackageExport["UniquifyIndexedTree"]
PackageExport["LinkedHypergraphToIndexedTree"]
PackageExport["LinkedHypergraphRuleToPatternRule"]
PackageExport["PatternRuleToMultiReplaceRule"]
PackageExport["ApplyCARules"]
PackageExport["ApplyHypergraphRules"]
PackageExport["ApplyWolframModelRules"]
PackageExport["MultiwayType"]

PackageExport["LinkedHypergraphToRootTree"]


link = _Integer | _Symbol | _Symbol[_Integer]

LinkedHypergraphQ[{{link, _, link...}...}] := True
LinkedHypergraphQ[___] := False


Options[HypergraphToLinkedHypergraph] = {"Permute" -> True}
HypergraphToLinkedHypergraph[hg_, OptionsPattern[]] := Module[{perm, iso},
	{perm, iso} = ResourceFunction["FindCanonicalHypergraphIsomorphism"][hg, "IncludePermutation" -> True];
	Join[
		MapThread[Join[{#2, None}, #1] &,
			{
				If[	TrueQ[OptionValue["Permute"]],
					Permute[hg /. iso, perm],
					hg /. iso, Max[iso]
				],
				Max[iso] + Range[Length[hg]]
			}
		]
	]
]

HypergraphRuleToLinkedHypergraphRule[lhs_ -> rhs_] := Module[{perm, iso, newLhs, newRhs},
	{perm, iso} = ResourceFunction["FindCanonicalHypergraphIsomorphism"][Join[lhs, rhs], "IncludePermutation" -> True];
	{lhsPerm, rhsPerm} = FindPermutation /@ TakeDrop[PermutationList[perm, Length[lhs] + Length[rhs]], Length[lhs]];
	newLhs = Join[
		MapThread[Join[{#2, None}, #1] &, {
			Permute[lhs /. iso, lhsPerm],
			Max[iso] + Range[Length[lhs]]
		}]
	];
	newRhs = Join[
		MapThread[Join[{#2, None}, #1] &, {
			Permute[rhs /. iso, rhsPerm],
			Max[iso] + Range[Length[rhs]]
		}]
	];
	newLhs -> newRhs
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

LinkedHypergraphToRootTree[hg : {{_, ___} ...}, root_, opts : OptionsPattern[]] := With[{map = Select[Length[#] > 0 &] @ GroupBy[hg, First, Rest @* First]},
	TreeMap[Last] @ Tree[linkedHypergraphToTree[root, map], opts]
]


LinkedHypergraphToIndexedTree[hg : {{_, ___} ...}, opts : OptionsPattern[]] := With[{map = Select[Length[#] > 0 &] @ GroupBy[hg, First, Rest @* First]},
	Tree[linkedHypergraphToTree[First[Complement[Keys[map], Catenate @ Values[map[[All, 2 ;;]]]], Missing[]], map], opts]
]

LinkedHypergraphRoot[hg : {{_, ___} ...}] := With[{map = Select[Length[#] > 1 &] @ GroupBy[hg, First, First]},
	map[First[Complement[Keys[map], Catenate @ Values[map[[All, 3 ;;]]]], Missing[]]]
]

UniquifyIndexedTree[tree_Tree] :=
	TreeMapAt[
		MapAt[Sow[Unique[], "UniqueIndex"] &, {1}],
		tree,
		Catenate[Rest @ TreePosition[tree, #] & /@ Keys @ Select[Counts[TreeData /@ TreeLeaves[tree]], # > 1 &]]
	]

LinkedHypergraphToTree[hg : {{_, _, ___} ...}, opts : OptionsPattern[]] := TreeMap[Last] @ LinkedHypergraphToIndexedTree[hg, opts]


ListToLinkedHypergraph[l_] := With[{len = Length[l]}, If[len == 0, {}, Append[{len, Last[l], 0}] @ MapThread[{#1, #2, #1 + 1} &, {Range[len - 1], Most[l]}]]]

LinkedHypergraphToList[hg : {{_, _, ___} ...}, OptionsPattern[]] := Enclose @ DeleteMissing @ Map[Last] @ FindHamiltonianPath @ ConfirmBy[LinkedHypergraphToGraph[hg], PathGraphQ[#] || EmptyGraphQ[#] &]

StringToLinkedHypergraph[s_] := ListToLinkedHypergraph[Characters[s]]

LinkedHypergraphToString[hg : {{_, _, ___} ...}, OptionsPattern[]] := Enclose @ StringJoin[ToString /@ ConfirmBy[LinkedHypergraphToList[hg], ListQ]]


PatternToLinkedHypergraph[Verbatim[HoldPattern][expr_] | expr_, patt_ : None] := If[patt === None,
	TreeToLinkedHypergraph[ExpressionTree[Unevaluated[expr], "HeldHeads"]],
	With[{map = AssociationThread[#, Table[Unique[], Length[#]]] & @ Cases[Unevaluated[expr], patt, All]},
		ReplaceAt[Reverse /@ Normal @ map, {All, 2, 1}] @ TreeToLinkedHypergraph[With[{newExpr = Unevaluated @@ (Hold[expr] /. map)}, ExpressionTree[newExpr, "HeldHeads"]]]
	]
]

ConstructPatternToLinkedHypergraph[expr_, patt_ : None] := If[patt === None,
	TreeToLinkedHypergraph[ExpressionTree[Unevaluated[expr], "HeldAtoms", Heads -> True]],
	With[{map = AssociationThread[#, Table[Unique[], Length[#]]] & @ Cases[Unevaluated[expr], patt, All, Heads -> True]},
		ReplaceAt[Reverse /@ Normal @ map, {All, 2, 1}] @ TreeToLinkedHypergraph[TreeMap[Replace[Null -> HoldComplete[Construct]]] @ With[{newExpr = Unevaluated @@ (Hold[expr] /. map)}, ExpressionTree[newExpr, "HeldAtoms", Heads -> True]]]
	]
]


MultiwayType[expr_] := Which[
	HypergraphQ[expr],
	"WIHypergraph",
	GraphQ[expr],
	"Graph",
	TreeQ[expr],
	"Tree",
	StringQ[expr],
	"String",
	MatchQ[expr, {{__Integer | ___Symbol}...}],
	"Hypergraph",
	ListQ[expr],
	"List",

	HypergraphRuleQ[expr],
	"WIHypergraph",
	MatchQ[expr, _String -> _String],
	"String",
	MatchQ[expr, _CellularAutomaton],
	"CA",
	MatchQ[expr, {{__Integer | ___Symbol}...} -> {{__Integer | ___Symbol}...}],
	"Hypergraph",
	MatchQ[expr, _List -> _List],
	"List",
	MatchQ[expr, _Rule | _RuleDelayed],
    If[
        FreeQ[expr, _[___][___]],
	    "Expression",
        "ConstructExpression"
    ],
	!FreeQ[expr, _[___][___]],
	"ConstructExpression",
    True,
	"Expression"
]

ToLinkedHypergraph[expr_, autoType : _String | Automatic : Automatic] := With[{type = Replace[autoType, Automatic :> MultiwayType[expr]]},
	If[	MatchQ[Unevaluated[expr], _Rule | _RuleDelayed],
		Switch[type,
			"String",
			StringToLinkedHypergraph[expr[[1]]] -> StringToLinkedHypergraph[expr[[2]]],
			"List",
			ListToLinkedHypergraph[expr[[1]]] -> ListToLinkedHypergraph[expr[[2]]],
			"Hypergraph",
			HypergraphRuleToLinkedHypergraphRule[expr],
			"ConstructExpression",
			If[	MatchQ[Unevaluated[expr], _[_, _Condition]],
				Function[Null, Condition[ReplaceAt[Evaluate @ ConstructPatternToLinkedHypergraph[expr[[1]], PatternHead[___]], HoldComplete[e_] :> e, {All, 2}], #2] :> {{1, #1}}, HoldAll] @@ ReplacePart[expr, {2, 0} -> Hold][[2]],
				ReplaceAt[
					RuleDelayed @@ {
						ConstructPatternToLinkedHypergraph[expr[[1]], PatternHead[___]],
						Function[Null, ConstructPatternToLinkedHypergraph[Unevaluated[#], PatternHead[___]], HoldAll] @@ Extract[expr, {2}, Hold]
					},
					HoldComplete[e_] :> e,
					{All, All, 2}
				]
			],
			_,
			If[	MatchQ[Unevaluated[expr], _[_, _Condition]],
				Function[Null, Condition[ReplaceAt[Evaluate @ PatternToLinkedHypergraph[expr[[1]], PatternHead[___]], HoldComplete[e_] :> e, {All, 2}], #2] :> {{1, #1}}, HoldAll] @@ ReplacePart[expr, {2, 0} -> Hold][[2]],
				ReplaceAt[
					RuleDelayed @@ {
						PatternToLinkedHypergraph[expr[[1]], PatternHead[___]],
						Function[Null, PatternToLinkedHypergraph[Unevaluated[#], PatternHead[___]], HoldAll] @@ Extract[expr, {2}, Hold]
					},
					HoldComplete[e_] :> e,
					{All, All, 2}
				]
			]
		],
		Switch[type,
			"Graph",
			GraphToLinkedHypergraph[Unevaluated[expr]],
			"Tree",
			TreeToLinkedHypergraph[Unevaluated[expr]],
			"String",
			StringToLinkedHypergraph[Unevaluated[expr]],
			"CA",
			CAToLinkedHypergraph[Unevaluated[expr]],
			"Hypergraph",
			HypergraphToLinkedHypergraph[Unevaluated[expr]],
			"WIHypergraph",
			WIHypergraphToLinkedHypergraph[Unevaluated[expr]],
			"List",
			ListToLinkedHypergraph[Unevaluated[expr]],
			"ConstructExpression",
			ReplaceAt[HoldComplete[e_] :> e, {All, 2}] @ ConstructPatternToLinkedHypergraph[Unevaluated[expr], PatternHead[___]],
			_,
			ReplaceAt[HoldComplete[e_] :> e, {All, 2}] @ PatternToLinkedHypergraph[Unevaluated[expr], PatternHead[___]]
		]
	]
]

ToLinkedHypergraph[expr_, patt_] := PatternToLinkedHypergraph[expr, patt]


FromLinkedHypergraph[hg : {_List...}, type : _String | None : "Graph", opts : OptionsPattern[]] := FromLinkedHypergraph[Select[hg, Length[#] > 1 &], type, opts]

FromLinkedHypergraph[hg : {{_, _, ___} ...}, type : _String | None : "Graph", opts : OptionsPattern[]] := Switch[
	type,
	None,
	hg,
	"Tree",
	LinkedHypergraphToTree[hg, opts],
	"Expression" | "ConstructExpression",
	Enclose @ TreeExpression[Confirm @ LinkedHypergraphToTree[hg, opts], "Heads"],
	"HoldExpression",
	Enclose[HoldForm @@ TreeExpression[Confirm @ LinkedHypergraphToTree[hg, opts], "HeldHeads"] //. HoldPattern[Construct[f_, x_]] :> f[x]],
	"List",
	LinkedHypergraphToList[hg, opts],
	"String",
	LinkedHypergraphToString[hg, opts],
	"CA",
	LinkedHypergraphToCA[hg, opts],
	"Hypergraph",
	LinkedHypergraphToHypergraph[hg, opts],
	"WIHypergraph",
	LinkedHypergraphWIHypergraph[hg, opts],
	_,
	LinkedHypergraphToGraph[hg, opts]
]


CanonicalHypergraphRule[rule : (lhs : {{__}...} -> {{__}...})] := Rule @@ TakeDrop[CanonicalHypergraph[List @@ rule, 1], Length[lhs]]

LinkedHypergraphRuleToPatternRule[rule_] := Module[{
	leftLinks, rightLinks, leftPayloads, rightPayloads,
	newLinks, newVars, patts, leftMap, newMap, newRule
},
	newRule = ReplaceAt[rule, Verbatim[Condition][expr_, _] :> expr, {1}];
	{leftLinks, rightLinks} = Union @ Flatten[{#[[All, ;; 1]], #[[All, 3 ;;]]}] & /@ List @@ newRule;
	{leftPayloads, rightPayloads} = Union @ #[[All, 2]] & /@ List @@ newRule;
	newLinks = Complement[rightLinks, leftLinks];
    patts = Union @ Cases[newRule[[1]], Verbatim[Pattern][s_, _] :> s, All];
	leftMap = AssociationThread[leftLinks -> (Symbol["\[FormalX]" <> ToString[#]] & /@ Range[Length[leftLinks]])];
	newMap = AssociationThread[newLinks -> (Symbol["\[FormalY]" <> ToString[#]] & /@ Range[Length[newLinks]])];
	newRule = RuleDelayed @@
				ReplaceAt[newMap, {{2, All, 1}, {2, All, 3 ;;}}] @
				ReplaceAt[Pattern[#, _] & /@ leftMap, {{1, All, 1}, {1, All, 3 ;;}}] @
				ReplaceAt[leftMap, {{2, All, 1}, {2, All, 3 ;;}}] @
                ReplaceAt[p_Pattern :> (p /. Blank -> BlankNullSequence), {1, All, 2}] @
                newRule;
	Function[test, If[Unevaluated[test] =!= None, newRule = MapAt[Condition[#, test] &, newRule, {1}]], HoldAll] @@
		Replace[rule[[1]], {Verbatim[Condition][_, test_] :> Hold[test], _ -> Hold[None]}];
	newVars = Complement[Cases[rightPayloads, p_Pattern :> First[p], All], Cases[leftPayloads, p_Pattern :> First[p], All]];
	With[{
		newSymbols = Join[Values[newMap], newVars],
		rhs = Extract[newRule, {2}, Hold]
	},
		Function[Null, ReplacePart[newRule, {2} :> Module[newSymbols, #]], HoldFirst] @@ rhs
	]
]

Options[CanonicalLinkedHypergraph] = {"IncludeIsomorphism" -> False, "IncludePermutation" -> False}

CanonicalLinkedHypergraph[lhg_, OptionsPattern[]] := Block[{
    perm, sorted, links, repl, result,
	isoQ = TrueQ[OptionValue["IncludeIsomorphism"]], permQ = TrueQ[OptionValue["IncludePermutation"]]
},
    If[Length[lhg] == 0, Return[lhg]];
    perm = First @ ResourceFunction["FindCanonicalHypergraphIsomorphism"][lhg, "IncludePermutation" -> True];
    sorted = Permute[lhg, perm];
    links = DeleteDuplicates @ Flatten[{sorted[[All, ;; 1]], Select[sorted, Length[#] > 1 &][[All, 3 ;;]]}];
	repl = AssociationThread[links -> Range[Length[links]]];
	result = Join[
        MapAt[Replace[repl], #, {{1}, {3 ;;}}] & /@ Select[sorted, Length[#] > 1 &],
        MapAt[Replace[repl], #, {1}] & /@ Select[sorted, Length[#] == 1 &]
    ];
	If[	isoQ,
		If[permQ, {perm, repl, result}, {repl, result}],
		If[permQ, {perm, result}, result]
	]
]


PatternRuleToMultiReplaceRule[rule : _[Verbatim[Condition][lhs_, _] | lhs_, _Module]] := With[{
    nothings = Unevaluated @@ Array[Nothing &, Length[lhs] - 1, Automatic, Hold @* List]
},
	ResourceFunction["SpliceAt"][
		MapAt[List, {2}] @ MapAt[Splice, {2, 2}] @ rule,
		nothings,
		{2, 2}
	]
]

PatternRuleToMultiReplaceRule[rule : _[lhs_List | Verbatim[HoldPattern][lhs_List], Verbatim[Module][vars_, rhs_List]]] := Block[{
	l = Length[Unevaluated[lhs]], r = Length[Unevaluated[rhs]],
	newRule
},
	newRule = If[r > 1, MapAt[Splice, rule, {2, 2}], Delete[rule, {2, 2, 0}]];
	If[r < l, newRule = Nest[Insert[Nothing, {2, 2}], MapAt[List, newRule, {2}], l - r]];
	ReplaceAt[newRule /. Verbatim[Module][{}, body_] :> body, Splice[body_] :> body, {2}]
]

Options[ApplyHypergraphRules] = Join[{"Hypergraph" -> True, "Evaluate" -> False, "Mode" -> "OrderlessSubsets", "ProcessMatches" -> Identity, "StateVisitFunction" -> (Null &), "EventSelectionFunction" -> (#2 &)}, Options[MultiReplace]]
ApplyHypergraphRules[x_, rules_, opts : OptionsPattern[]] := With[{
	rhsLengths = Replace[
		rules,
		_[_, {rhs___} | rhs___] :> Total @ Replace[
			Unevaluated[{rhs}], {
				Verbatim[Module][_, Splice[splice_List]] :> Length[Unevaluated[splice]],
				Nothing -> 0,
				_ -> 1
			},
			{1}
		],
		{1}
	]
},
	OptionValue["StateVisitFunction"][x, rules];
    OptionValue["EventSelectionFunction"][x, #] & @ Association @ KeyValueMap[
        Block[{
            state = If[TrueQ[OptionValue["Evaluate"]], ToLinkedHypergraph @ FromLinkedHypergraph[#, "Expression"] &, Identity] @ First[#2],
            uniqueLinks, repl,
            destroyed = Union @ Extract[x, #1[[2]]][[All, 1]],
            created
        },
            created = Take[state, MapAt[UpTo, #1[[2, 1, 1]] + {0, rhsLengths[[#1[[1]]]] - 1}, {2}]][[All, 1]];
			If[	! TrueQ[OptionValue["Hypergraph"]],
				{state, uniqueLinks} = Reap[
					IndexedTreeToLinkedHypergraph @ UniquifyIndexedTree @ LinkedHypergraphToIndexedTree[state],
					"UniqueIndex"
				];
				If[ Length[uniqueLinks] > 0,
					uniqueLinks = First[uniqueLinks];
					repl = AssociationThread[uniqueLinks, Max @ Select[state[[All, 1]], IntegerQ] + Range[Length[uniqueLinks]]];
					state = ReplaceAt[state, repl, {{All, 1}, {All, 3 ;;}}];
					created = Join[created, Values[repl]]
				]
			];
            <|
                "Input" -> destroyed, "Output" -> created, "Rule" -> #1[[1]], "Position" -> #1[[2]]
            |> -> state
        ] &,
        OptionValue["ProcessMatches"] @ MultiReplace[x, rules, {1}, FilterRules[{opts}, Options[MultiReplace]], "Mode" -> "OrderlessSubsets"]
    ]
]

Options[HypergraphMulti] = Join[{"Type" -> Automatic}, Options[ApplyHypergraphRules], $MultiOptions]
HypergraphMulti[init_, rule_, opts : OptionsPattern[]] := Enclose @ Block[{
	rules = wrap[rule], type
},
	type = Replace[OptionValue["Type"], Automatic :> First[ConfirmBy[MultiwayType /@ rules, Apply[Equal], "All rules must have the same type."]]];
	With[{
		multReplaceRules = Map[ToLinkedHypergraph[#, type] & /* LinkedHypergraphRuleToPatternRule /* PatternRuleToMultiReplaceRule, rules],
		hypergraphQ = type === "Hypergraph",
		listInit = Unevaluated @@ If[MatchQ[Unevaluated[init], {{___List}}] || type === "List" && MatchQ[Unevaluated[init], {___List}], Hold[init], Hold[{init}]],
		applyOptions = FilterRules[{opts}, Options[ApplyHypergraphRules]]
	},
		Multi[
			Function[Null, ToLinkedHypergraph[Unevaluated[#], type], HoldAll] /@ listInit,
			RuleDelayed @@ Hold[\[FormalCapitalH]_, ApplyHypergraphRules[\[FormalCapitalH], multReplaceRules, applyOptions, "Hypergraph" -> hypergraphQ]],
			{1},
			FilterRules[{opts}, $MultiOptions],
			"DeepMultiEvaluate" -> False
		]
	]
]


ApplyWolframModelRules[x_, rules_] := Module[{
    eo = ResourceFunction["WolframModel"][<|"PatternRules" -> rules|>, x, 1, "EventSelectionFunction" -> None],
    edges
},
    edges = eo["AllEventsEdgesList"];
    Association[
        With[{state = Join[Delete[x, List /@ #[[2, 1]]], edges[[#[[2, 2]]]]]},
            <|
                "Input" -> edges[[#[[2, 1]]]][[All, 1]],
                "Output" -> edges[[#[[2, 2]]]][[All, 1]],
				"Position" -> List /@ #[[2, 1]]
            |> -> state
        ] & /@ eo["AllEventsList"]
    ]
]

WolframModelMulti[init_, rules_, opts : OptionsPattern[]] := With[{
	patternRules = Map[ToLinkedHypergraph /* LinkedHypergraphRuleToPatternRule, wrap[rules]]
},
    Multi[
		ToLinkedHypergraph /@ wrap[init],
		RuleDelayed @@ Hold[\[FormalCapitalH]_, ApplyWolframModelRules[\[FormalCapitalH], patternRules]],
		{1},
		FilterRules[{opts}, $MultiOptions],
		"DeepMultiEvaluate" -> False
	]
]

StringMulti[init_, rule_, opts : OptionsPattern[]] := With[{
	rules = wrap[rule]
},
	Multi[
		ToLinkedHypergraph[#, "String"] & /@ wrap[init],
		RuleDelayed @@ Hold[\[FormalCapitalH]_, ApplyStringRules[\[FormalCapitalH], rules]],
		{1},
		FilterRules[{opts}, $MultiOptions],
		"DeepMultiEvaluate" -> False
	]
]


ApplyWIHypergraphRules[lh_ ? LinkedHypergraphQ, rules_, opts : OptionsPattern[]] := Block[{
	h = Hypergraph[FromLinkedHypergraph[lh, "WIHypergraph"], opts], vertexIndex
},
	vertexIndex = PositionIndex[VertexList[h]];
	Association @ Catenate @ MapIndexed[
		{rule, i} |->
			MapIndexed[With[{input = Join[#DeletedVertices, #MatchVertices]},
				<|
					"Input" -> input,
					"Output" -> Union @@ Append[Replace[#NewEdges, (edge_ -> _) :> edge, {1}], #NewVertices],
					"Rule" -> i[[1]],
					"Position" -> Join[Lookup[vertexIndex, input], #MatchEdgePositions + VertexCount[h]],
					"Match" -> Iconize[#, "Match"]
				|> -> ToLinkedHypergraph[#Hypergraph, "WIHypergraph"]
			 ] &, rule[h]],
		wrap[rules]
	]
]

WIHypergraphMulti[init_ ? HypergraphQ, rule_, opts : OptionsPattern[]] := With[{
	rules = wrap[rule], hopts = FilterRules[Options[init], Except[VertexLabels | EdgeLabels]]
},
    Multi[
		{ToLinkedHypergraph[init, "WIHypergraph"]},
		RuleDelayed @@ Hold[\[FormalCapitalH]_, ApplyWIHypergraphRules[\[FormalCapitalH], rules, hopts]],
		{1},
		FilterRules[{opts}, $MultiOptions],
		"DeepMultiEvaluate" -> False,
		"ExtraOptions" -> hopts
	]
]

WIHypergraphToLinkedHypergraph[hg_ ? HypergraphQ] := Block[{vertices, edges},
	vertices = Reap[
		edges = MapIndexed[
			Replace[#1, Labeled[vs_, {_, edgeLabel_}] :>
				{\[FormalE][#2[[1]]], edgeLabel, Splice @ Replace[vs, Labeled[v_, Labeled[l_, _]] :> (Sow[{v, l}]; v), {1}]}] &,
			ToLabeledEdges[hg]
		]
	][[2]];
	Join[Union @@ vertices, edges]
]

LinkedHypergraphWIHypergraph[hg_, OptionsPattern[]] := Block[{vertices, edges},
	{vertices, edges} = Lookup[GroupBy[hg, MatchQ[#[[1]], \[FormalE][_]] || MatchQ[#[[2]], None] &], {False, True}, {}];
	Hypergraph[vertices[[All, 1]], edges[[All, 3 ;;]], VertexLabels -> Rule @@@ vertices, EdgeLabels -> Thread[edges[[All, 3 ;;]] -> edges[[All, 2]]]]
]



(* CA *)

RulePlot;
$SowRuleEnumerator = True;
PrependTo[
	DownValues[NKSSpecialFunctions`RulePlot`Dump`RPRuleMaker1], 
  	HoldPattern[NKSSpecialFunctions`RulePlot`Dump`RPRuleMaker1[args : PatternSequence[{___, k_, r_}, _, {___, enumerator_}, rn_, ___]] /; $SowRuleEnumerator] :>
   		Block[{$SowRuleEnumerator = False},
    		Sow[enumerator[rn, k, r]];
    		NKSSpecialFunctions`RulePlot`Dump`RPRuleMaker1[args]
    	]
]

CARuleRange[rule_CellularAutomaton] := Replace[Reap[RulePlot[rule]][[2, 1, 1, 1, 1]], {_, _, cell_, _} | cell_ :> (Dimensions[cell] - 1) / 2]

ArrayTokens[rules_, range_ ? VectorQ] :=
	Append[Last[rules]] @* Sort /@ WeaklyConnectedComponents @ RelationGraph[
		AllTrue[Abs[#1[[1]] - #2[[1]]] - 2 range, # <= 0 &] &,
		Most[rules]
	]

ArrayTokens[array_ ? SparseArrayQ, range_ ? VectorQ] := ArrayTokens[ArrayRules[array], range]

CAToLinkedHypergraph[array_ ? SparseArrayQ, defRange_ : Automatic] := MapIndexed[Append[#2, #1] &, ArrayTokens[array, Replace[defRange, Automatic :> ConstantArray[1, ArrayDepth[array]]]]]

LinkedHypergraphToCA[hg_, OptionsPattern[]] := Enclose @ With[{rules = ConfirmMatch[Catenate[hg[[All, 2, ;; -2]]], {__Rule}]}, {bounds = Confirm[CoordinateBounds[rules[[All, 1]]]][[All, 1]]},
	SparseArray[Join[MapAt[# - bounds + 1 &, rules, {All, 1}], hg[[All, 2, -1]]]]
]

ApplyCARules[hg_, rules_] := Enclose @ Block[{input = Confirm @ FromLinkedHypergraph[hg, "CA"], bounds = Confirm[CoordinateBounds[Catenate @ hg[[All, 2, ;; -2, 1]]]][[All, 1]], events},
	Association @ MapIndexed[{rule, i} |->
		With[
			{range = CARuleRange[rule], output = SparseArray[rule[{input, 0}][[1]]]},
			{newHg = Confirm @ MapAt[# - range + bounds - 1 &, CAToLinkedHypergraph[output, range], {All, 2, ;; -2, 1}]},
			
			{diff = SymmetricDifference[Catenate[hg[[All, 2]]], Catenate[newHg[[All, 2]]]]},
			events = If[diff === {}, {}, ArrayTokens[SparseArray[diff], range][[All, ;; -2, 1]]];
			events =
			DeleteDuplicatesBy[#[[1, {"Input", "Output"}]] &] @
			Map[
				event |-> With[{rhs = Select[newHg, IntersectingQ[#[[2, ;; -2, 1]], event] &]},
					<|
						"Input" -> Select[hg, AnyTrue[#[[2, ;; -2, 1]], AnyTrue[Abs[event - Threaded[#]] - Threaded[range], Max[#] <= 0 &] &] &][[All, 1]],
						"Output" -> Select[newHg, AnyTrue[#[[2, ;; -2, 1]], AnyTrue[Abs[event - Threaded[#]] - Threaded[range], Max[#] <= 0 &] &] &][[All, 1]]
					|> -> rhs
						(* Sort @ Join[Select[hg, ! IntersectingQ[#[[2, ;; -2, 1]], event] &], rhs] *)
				],
			 	events
			];
			<|
				"Input" -> hg[[All, 1]],
				"Output" -> newHg[[All, 1]],
				"Rule" -> i[[1]],
				"SubEvents" -> events
			|> -> newHg
		],
		wrap[rules]
	]
]

CAMulti[init_, rule_, opts : OptionsPattern[]] := With[{rules = wrap[rule]}, {range = MapThread[Max, CARuleRange /@ rules]},
	Multi[
		CAToLinkedHypergraph[#, range] & /@ wrap[init],
		RuleDelayed @@ Hold[\[FormalCapitalH]_, ApplyCARules[\[FormalCapitalH], rules]],
		{1},
		FilterRules[{opts}, $MultiOptions],
		"DeepMultiEvaluate" -> False
	]
]


