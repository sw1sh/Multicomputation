Package["Wolfram`Multicomputation`"]

PackageScope["MultiProp"]



MultiProp[_, "Properties"] := {
    "Expression", "HoldExpression",
    "Data",
    "Placeholders",
    "Bindings",
    "ListValues", "HoldListValues",
    "EvaluateList", "HoldEvaluateList",
    "EvaluateListOnce", "HoldEvaluateListOnce",
    "Evaluate", "HoldEvaluate",
    "EvaluateOnce", "HoldEvaluateOnce",
    "MultiList",
    "ListEvaluate", "HoldListEvaluate",
    "MultiListEvaluate","HoldMultiListEvaluate",
    "MultiEvaluate","HoldMultiEvaluate",
    "Edges", "HoldEdges",
    "Graph", "HoldGraph"
}


Multi::undefprop = "property `` is undefined";

Multi::failprop = "property `` failed with ``";


multi_Multi[prop_String, args___] /; multi["ValidQ"] := With[{result = Hold @ Evaluate @ MultiProp[multi, prop, args]},
    ReleaseHold @ result /; (! MatchQ[result, Hold[_MultiProp]] || Message[Multi::undefprop, "\"" <> prop <> "\""])
]

SetAttributes[MultiProp, HoldFirst]

MultiProp[multi_, prop_String] /; MemberQ[$MultiKeys, prop] := multi["Data"][prop]

MultiProp[multi_, "ModifyData", f_] := Multi[f @ multi["Data"]]

MultiProp[multi_, "HoldExpression"] := Extract[multi["Data"], "Expression", HoldComplete]

MultiProp[multi_, "Keys"] := Join[Keys @ multi["Values"], Keys @ multi["Matches"]]

MultiProp[multi_, "Positions"] := Replace[{{pos : {{___Integer} ...}, __} :> pos, pos : {{___Integer} ...} :> pos}] /@
    Join[Keys @ multi["Values"], Keys @ multi["Matches"]]

MultiProp[multi_, "Placeholders"] := Association @ KeyValueMap[
    {pos, holdValues} |-> Function[Null, pos :> #, HoldFirst] @@ FlattenAt[holdValues, 1],
    Merge[KeyValueMap[Function[Null, Thread[Extract[multi["HoldExpression"], Prepend[1] /@ #1] :> #2, List, 1], HoldRest], multi["Values"]], Hold]
]

WithPlaceholdersValues[placeholders_] := Function[multi,
    Multi[<|multi["Data"], "Values" -> Function[expr, Association @ DeleteCases[{} -> _] @ KeyValueMap[Position[Unevaluated[expr], #1] :> #2 &, placeholders], HoldFirst] @@ multi["HoldExpression"]|>],
    HoldFirst
]


MultiProp[multi_, "Size"] := Length @ multi["Keys"]

MultiProp[multi_, "ValueCount"] := Times @@ Replace[Function[Null, Length @ Unevaluated @ #, HoldFirst] /@ multi["Values"], <||> -> {0}]

MultiProp[multi_, "MatchCount"] := Total[Function[Null, Length @ Unevaluated @ #, HoldFirst] /@ multi["Matches"]]


MultiProp[multi_, "Options"] := Sequence @@ Join[multi["EvaluateOptions"], multi["ReplaceOptions"], multi["ExtraOptions"]]

MultiProp[multi_, "AllReplaceArguments"] := Sequence[multi["Rules"], multi["ReplaceArguments"], FilterRules[{multi["Options"]}, Options[Multi]], "ExtraOptions" -> FilterRules[{multi["Options"]}, Except[Options[Multi]]]]

MultiProp[multi_, "ReplaceHead"] := Enclose[ConfirmBy[{multi["ReplaceArguments"]}, MatchQ[{_, _Symbol, ___}]][[2]], List &]


MultiProp[multi_, "ListValues"] := With[{expr = Unevaluated @@ multi["HoldExpression"]},
    KeyValueMap[
        Which[
            MatchQ[#1, {{{___Integer}}, "Eval"}],
            Extract[expr, First @ #1],
            MatchQ[#1, {{___Integer} ...}],
            #2,
            MatchQ[#1, {_, "Rule", _}],
            Flatten[ReleaseHold @ #2, 1],
            True,
            Function[x, ReplaceList[Unevaluated @ x, #2], HoldAllComplete] @@
            First @ Extract[expr, First @ #1, HoldComplete]
        ] &,
        Join[multi["Values"], multi["Matches"]]
    ]
]

MultiProp[multi_, "HoldListValues"] := With[{expr = Unevaluated @@ multi["HoldExpression"]},
    KeyValueMap[
        Function[
            Null,
            Which[
                MatchQ[#1, {{{___Integer}}, "Eval"}],
                Extract[expr, First @ #1],
                MatchQ[#1, {{___Integer} ...}],
                HoldForm /@ Unevaluated @ #2,
                MatchQ[#1, {_, "Rule", _}],
                Catenate[#2],
                True,
                Function[x, ReplaceList[HoldForm @ x, MapAt[HoldForm, Unevaluated[#2], {All, All}]], HoldAllComplete] @@
                First @ Extract[expr, First @ #1, HoldComplete]
            ],
            HoldAll
        ],
        Join[multi["Values"], multi["Matches"]]
   ]
]


MultiProp[multi_, "Tuples", m_ : All] := TakeTuples[multi["ListValues"], m]

MultiProp[multi_, "HoldTuples", m_ : All] := TakeTuples[multi["HoldListValues"], m]

MultiProp[multi_, "MatchBindings"] := If[MatchQ[#, {_, "Rule", _}], #[[3, 2]], {<||>}] & /@ Keys @ multi["Matches"]

MultiProp[multi_, "Bindings"] := Join[Table[<||>, #] & /@ Values[Length /@ multi["Values"]], multi["MatchBindings"]]

MultiProp[multi_, "EvaluateList", m_ : All] := With[{expr = Unevaluated @@ (multi["HoldExpression"] /. Join @@ Catenate @ multi["Bindings"]), pos = multi["Positions"]},
    Map[ReplacePart[expr, Thread[pos -> #]] &, multi["Tuples", m]]
]

MultiProp[multi_, "EvaluateListOnce", m_ : All, k_ : All] := With[{pos = Take[multi["Positions"], m]},
    If[
        Length[pos] > 0,
        MapThread[
            MapThread[
                {pos, subExprs, subst} |-> With[{
                    expr = Unevaluated @@ (multi["HoldExpression"] /. subst)
                },
                    ReplacePart[expr, pos -> #] & /@ Developer`ToList[subExprs]
                ],
                {multi["Positions"], #1, #2}
            ] &,
            With[{
                values = Take[multi["ListValues"], m]
            }, {
                TakeTuples[values, k],
                TakeTuples[MapThread[PadRight[#1, Length @ #2, <||>] &, {Take[multi["Bindings"], m], values}], k]
            }
            ]
        ],
        {{multi["Expression"]}}
   ]
]

MultiProp[multi_, "HoldEvaluateList", m_ : All] := With[{
    expr = multi["HoldExpression"] /. Join @@ Catenate @ multi["Bindings"],
    pos = Map[Prepend[1], multi["Positions"], {2}]
},
    Map[
        ReplacePart[
            expr,
            Thread[pos -> #]
        ] &,
        multi["HoldTuples", m]
    ]
]

MultiProp[multi_, "HoldEvaluateListOnce", m_ : All, k_ : All] := With[{pos = Take[Map[Prepend[1], multi["Positions"], {2}], m]},
    If[
        Length[pos] > 0,
        MapThread[
            MapThread[
                {pos, subExprs, subst} |->
                With[{value = Unevaluated @@ #},
                    ReplacePart[multi["HoldExpression"] /. subst, pos :> value]
                ] & /@ Replace[subExprs, e : Except[_List] :> {e}],
                {pos, #1, #2}
            ] &,
            With[{
                values = Take[multi["HoldListValues"], m]
            },
                {
                    TakeTuples[values, k],
                    TakeTuples[MapThread[PadRight[#1, Length@#2, <||>] &, {Take[multi["Bindings"], m], values}], k]
                }
            ]
        ],
        {{multi["HoldExpression"]}}
    ]
]


MultiProp[multi_, "Evaluate", 1, m_ : All] := Multi[multi["EvaluateList", m] /. a_Association :> Splice @ Values[a], multi["AllReplaceArguments"]]

MultiProp[multi_, "HoldEvaluate", m_ : All] := With[{
    results = Map[If[FreeQ[#, _Multi], #, Evaluate /@ #] &, multi["HoldEvaluateList", m]]
},
    Function[Null, Multi[Unevaluated @ #, multi["AllReplaceArguments"]], HoldFirst] @@
        Delete[HoldForm[results], Prepend[1] @* Append[0] /@ Position[results, _HoldForm]]
]

MultiProp[multi_, "Evaluate", n : _Integer ? Positive : 1, m : _Integer | All : All, k : _Integer | All : All] := If[Length[multi["Data"]] == 0, multi, multi["Evaluate", 1, m]["Evaluate", n - 1, m, k]]
MultiProp[multi_, "Evaluate", _Integer ? NonPositive, ___] := multi

MultiProp[multi_, "EvaluateOnce", 1, m_ : All, k_ : All] := WithPlaceholdersValues[multi["Placeholders"]] @ Multi[Catenate @ multi["EvaluateListOnce", m, k] /. a_Association :> Splice @ Values[a], multi["AllReplaceArguments"]]

MultiProp[multi_, "HoldEvaluateOnce", m_ : All, k_ : All] := WithPlaceholdersValues[multi["Placeholders"]] @ With[{results = Map[If[FreeQ[#, _Multi], #, Evaluate /@ #] &, Catenate @ multi["HoldEvaluateListOnce", m, k]]},
    Function[Null, Multi[Unevaluated @ #, multi["AllReplaceArguments"]], HoldFirst] @@
        Delete[HoldForm[results], Prepend[1] @* Append[0] /@ Position[results, _HoldForm]]
]

MultiProp[multi_, "EvaluateOnce", n : _Integer ? Positive : 1, m : _Integer | All : All, k : _Integer | All : All] := multi["EvaluateOnce", 1, m, k]["EvaluateOnce", n - 1, m, k]
MultiProp[multi_, "EvaluateOnce", _Integer ? NonPositive, ___] := multi


MultiProp[multi_, "MultiList"] := With[{
    expr = Unevaluated @@ multi["HoldExpression"],
    head = multi["ReplaceHead"]
},
    If[ MatchQ[expr, _head],
        head @@@ Hold @ Evaluate @ Flatten[Hold @@ MapIndexed[
            Function[
                {v, k},
                With[{newData = <|
                    multi["Data"],
                    "Values" -> KeySelect[# =!= {} &] @ KeyMap[Map[If[Take[#, UpTo[1]] == k, Drop[#, 1], Nothing] &], multi["Values"]],
                    "Matches" -> KeyMap[MapAt[Drop[#, UpTo[1]] &, {1, All}]] @ KeySelect[multi["Matches"], MemberQ[#[[1, All, ;; 1]], k] &]
                    |>
                },
                    With[{data = <|newData, "Expression" :> v|>},
                        Hold @ Multi[data]
                    ]
                ],
                HoldAll
            ],
            expr
        ]],
        Hold[head[multi]]
    ]
]

MultiProp[multi_, "Apply", f_, pos : {(_Integer | All | _Span) ...} : {}] := With[{
    newExpr = Unevaluated @@ ResourceFunction["MapAtEvaluate"][f, multi["HoldExpression"], Prepend[pos, 1]]
},
    WithPlaceholdersValues[multi["Placeholders"]] @ Multi[newExpr, multi["AllReplaceArguments"]]
]

MultiProp[multi_, "HoldApply", f_, pos : {(_Integer | All | _Span) ...} : {}] := multi["Apply", Function[Null, f[Unevaluated @ #], HoldAllComplete], pos]

MultiProp[multi_, "ListApply", f_] := multi["HoldApply", Unevaluated @@@ f[Hold /@ Unevaluated @ #] &]

MultiProp[multi_, "DeleteDuplicates"] := multi["ListApply", DeleteDuplicates]

MultiProp[multi_, "ReplacePart", rules_List] := With[{unevalMulti = Unevaluated @ Evaluate @ multi},
    Fold[Function[Null, #1["ReplacePart", #2], HoldAllComplete], unevalMulti, rules]]

MultiProp[multi_, "ReplacePart", (Rule | RuleDelayed)[pos_, x_]] := multi["Apply", Function[Null, Unevaluated @ x, HoldAllComplete], pos]


MultiProp[multi_, "ListEvaluate", m_ : All, k_ : All] := With[{head = multi["ReplaceHead"]},
If[
    MatchQ[multi["HoldExpression"], _[_head]],
    ReleaseHold @ ResourceFunction["MapAtEvaluate"][
        Function[Null, #["EvaluateListOnce"], HoldAllComplete],
        multi["MultiList"],
        {1, All}
    ],
    head[multi["EvaluateListOnce", m, k]]
]
]

MultiProp[multi_, "HoldListEvaluate", m_ : All, k_ : All] := With[{head = multi["ReplaceHead"]},
If[
    MatchQ[multi["HoldExpression"], _[_head]],
    ReleaseHold @ ResourceFunction["MapAtEvaluate"][
        Function[Null, #["HoldEvaluateListOnce", m], HoldAllComplete],
        multi["MultiList"],
        {1, All}
    ],
    head[multi["HoldEvaluateListOnce", m, k]]
]
]

MultiProp[multi_, "ListEvaluateWithKeys", m_ : All, k_ : All] := With[{head = multi["ReplaceHead"]},
If[
    MatchQ[multi["HoldExpression"], _[_head]],
    Module[{i = 1},
        ReleaseHold @ ResourceFunction["MapAtEvaluate"][
            Function[Null, {Replace[#["Keys"], {pos : {{___Integer}} :> Map[Prepend[i++], pos], key : {{{___Integer}}, __} :> MapAt[Prepend[i++], key, {1, All}]}, {1}], #["EvaluateListOnce", m, k]}, HoldAllComplete],
            multi["MultiList"],
            {1, All}
        ]
    ],
    head[{multi["Keys"], multi["EvaluateListOnce", m, k]}]
]
]

MultiProp[multi_, "HoldListEvaluateWithKeys", m_ : All, k_ : All] := With[{head = multi["ReplaceHead"]},
If[
    MatchQ[multi["HoldExpression"], _[_head]],
    Module[{i = 1},
        ReleaseHold @ ResourceFunction["MapAtEvaluate"][
            Function[Null, {Replace[#["Keys"], {pos : {{___Integer}} :> Map[Prepend[i++], pos], key : {{{___Integer}}, __} :> MapAt[Prepend[i++], key, {1, All}]}, {1}], #["HoldEvaluateListOnce", m, k]}, HoldAllComplete],
            multi["MultiList"],
            {1, All}
        ]
    ],
    {{multi["Keys"], multi["HoldEvaluateListOnce", m, k]}}
]
]

MultiProp[multi_, "HoldMultiListEvaluate", 1, m_ : All, k_ : All] := Function[Null, Multi[Unevaluated[{##}], multi["AllReplaceArguments"]], HoldAllComplete] @@
    Flatten[HoldForm @@ Flatten[multi["HoldListEvaluate", m, k], 3]]

MultiProp[multi_, "HoldMultiListEvaluate", n : _Integer ? NonNegative : 1, m : _Integer | All : All, k : _Integer | All : All] := If[
    n > 0,
    multi["HoldMultiListEvaluate", 1, m, k]["HoldMultiListEvaluate", n - 1, m, k],
    multi
]

MultiProp[multi_, "MultiListEvaluate", 1, m_ : All, k_ : All] := WithPlaceholdersValues[multi["Placeholders"]] @ Multi[Flatten[(multi["ListEvaluate", m, k] /. a_Association :> Splice @ Values[a]), 3], multi["AllReplaceArguments"]]

MultiProp[multi_, "MultiListEvaluate", n : _Integer ? NonNegative : 1, m : _Integer | All : All, k : _Integer | All : All] := If[n > 0, multi["MultiListEvaluate", 1, m, k]["MultiListEvaluate", n - 1, m, k], multi]

MultiProp[multi_, "HoldMultiEvaluate", 1, m_ : All, k_ : All] := WithPlaceholdersValues[multi["Placeholders"]][HoldApply[Multi, multi["AllReplaceArguments"]] @@@ Flatten[multi["HoldEvaluateListOnce", m, k], 2]]

MultiProp[multi_, "HoldMultiEvaluate", n : _Integer ? NonNegative : 1, m : _Integer | All : All, k : _Integer | All : All] := If[n > 0, multi["HoldMultiEvaluate", 1, m, k]["HoldMultiEvaluate", n - 1, m, k], multi]

MultiProp[multi_, "MultiEvaluate", 1, m_ : All, k_ : All] := WithPlaceholdersValues[multi["Placeholders"]] @ Map[Multi[#, multi["AllReplaceArguments"]] &, Flatten[multi["EvaluateListOnce", m, k] /. a_Association :> Splice @ Values[a], 2]]

MultiProp[multi_, "MultiEvaluate", n : _Integer ? NonNegative : 1, m : _Integer | All : All, k : _Integer | All : All] := If[n > 0, multi["MultiEvaluate", 1, m, k]["MultiEvaluate", n - 1, m, k], multi]

MultiProp[multi_, "MultiEvaluateList", args___] := multi["MultiEvaluate", args]["Expression"]

MultiProp[multi_, "HoldMultiEvaluateList", args___] := multi["HoldMultiEvaluate", args]["HoldExpression"]

MultiProp[multi_, "Events", lvl_Integer : 1, args___] := MapThread[{keys, tos} |->
    Catenate @ Map[to |->
        Catenate @ MapThread[{elem, key} |->
            With[{
                tag = Replace[key, {{pos : _Integer ...} :> {pos}, {{pos : _Integer ...}, rest___} :> {{pos}, rest}}]
            },
                If[ FreeQ[elem, _Association],
                    With[{
                        depth = HeadDepth[elem]
                    },
                        Map[
                            tag -> # &,
                            If[
                                depth < lvl,
                                Nest[List, elem, lvl - depth],
                                elem
                            ],
                            {lvl}
                        ]
                    ],
                    Module[{
                        array = elem /. a_Association :> Values[a],
                        extraKeys = elem /. a_Association :> Keys[a],
                        depth
                    },
                        depth = HeadDepth[array];
                        MapThread[
                            Append[tag, #2] -> #1 &,
                            {
                                If[depth < lvl, Nest[List, array, lvl - depth], array],
                                If[depth < lvl, Nest[List, extraKeys, lvl - depth], extraKeys]
                            },
                            lvl
                        ]
                    ]
                ]
            ],
            With[{elems = wrap[to]}, {elems, PadRight[keys, Length @ elems, Append[keys, Missing[]]]}],
            1
        ],
        tos
    ],
    Thread @ multi["ListEvaluateWithKeys", args]
]

MultiProp[multi_, "HoldEvents", lvl_Integer : 1, args___] :=
    MapThread[{keys, tos} |->
        Catenate @ Map[to |->
            Catenate @ MapThread[{elem, key} |->
                With[{
                    tag = Replace[key, {{pos : _Integer ...} :> {pos}, {{pos : _Integer ...}, rest___} :> {{pos}, rest}}],
                    depth = HeadDepth[elem]
                },
                    Map[tag -> # &, If[depth < lvl, Nest[List, elem, lvl - depth], elem], {lvl}]
                ],
                With[{elems = wrap[to]}, {elems, PadRight[keys, Length @ elems, Append[keys, Missing[]]]}],
                1
            ],
            tos
        ],
        Thread @ multi["HoldListEvaluateWithKeys", args]
    ]

MultiProp[multi_, "Edges", args___] := With[{expr = wrap[multi["Expression"]]},
    Catenate @ MapThread[{elem, events} |-> DirectedEdge[elem, Sequence @@ Reverse @ #] & /@ Flatten @ events, {expr, multi["Events", args]}]
]

MultiProp[multi_, "HoldEdges", args___] := With[{
    expr = If[ MatchQ[multi["HoldExpression"], _[_List]], First @ Map[HoldForm, multi["HoldExpression"], {2}], {multi["HoldExpression"]}]
},
    Catenate @ MapThread[{elem, events} |-> DirectedEdge[elem, Sequence @@ Reverse @ #] & /@ Flatten @ events, {expr, multi["HoldEvents", args]}]
]

MultiProp[multi_, "Branches", args___] := multi["Edges", args][[All, 2]]
MultiProp[multi_, "HoldBranches", args___] := multi["HoldEdges", args][[All, 2]]

MultiProp[multi_, "BranchPairs", args___] := Catenate[Subsets[#, {2}] & /@ multi["Branches", args]]
MultiProp[multi_, "HoldBranchPairs", args___] := Catenate[Subsets[#, {2}] & /@ multi["HoldBranches", args]]

MultiProp[multi_, prop : "Foliations" | "HoldFoliations", steps_Integer : 1, lvl_Integer : 2, f_ : Identity] := Module[{counts = <||>},
    {NestList[
        Function[
            Block[{
                eventOutputs = Map[f, (Flatten /@ ReleaseHold[#[[1]]][If[prop === "Foliations", "Events", "HoldEvents"], lvl])[[All, All, 2]], {2}],
                eventOutputCounts,
                newOutputs
            },
                eventOutputCounts = Counts[Catenate @ eventOutputs];
                newOutputs = Complement[Keys[eventOutputCounts], Keys[counts]];
                counts = Merge[{counts, eventOutputCounts}, Total];
                {
                    If[ Length @ newOutputs > 0,
                        Hold[
                            Evaluate @ If[
                                prop === "Foliations",
                                Multi[#, multi["AllReplaceArguments"]] &,
                                Function[
                                    Null,
                                    Multi[Unevaluated @ {##}, multi["AllReplaceArguments"]],
                                    HoldAll
                                ] @@ Flatten[HoldForm @@ #] &] @ newOutputs
                        ],
                        #[[1]]
                    ],
                    eventOutputs
                }
            ]
        ],
        {Hold[multi], {}},
        steps
    ],
    counts
    }
]

MultiProp[multi_, "RematchRules"] := With[{expr = Unevaluated @@ multi["HoldExpression"]}, Multi[expr, multi["AllReplaceArguments"]]]

MultiProp[multi_, "Graph", n_Integer : 1, opts___] := EvolutionGraph[multi, n, 2, opts]

MultiProp[multi_, "HoldGraph", n_Integer : 1, opts___] := EvolutionGraph[multi, n, 2, "Hold" -> True, opts]


MultiProp[multi_, "CausalGraph", n_Integer : 1, opts___] := CausalGraph[multi["Graph", n, FilterRules[{opts}, FilterRules[Options[EvolutionGraph], Except[Options[Graph]]]]], FilterRules[{opts}, Options[CausalGraph]]]

MultiProp[multi_, "TokenEventGraph", n_Integer : 1, opts___] := MultiTokenEventGraph[
    multi["CausalGraph", n, FilterRules[{opts}, FilterRules[Options[EvolutionGraph], Except[Options[Graph]]]]],
    FilterRules[{opts}, Options[MultiTokenEventGraph]]
]

MultiProp[multi_, "EvolutionCausalGraph", n_Integer : 1, opts___] := EvolutionCausalGraph[
    multi["CausalGraph", n, FilterRules[{opts}, FilterRules[Options[EvolutionGraph], Except[Options[Graph]]]]],
    FilterRules[{opts}, Options[EvolutionCausalGraph]]
]

MultiProp[multi_, "BranchialGraph", n_Integer : 1, opts___] := BranchialGraph[multi["Graph", n, FilterRules[{opts}, FilterRules[Options[EvolutionGraph], Except[Options[Graph]]]]], FilterRules[{opts}, Options[BranchialGraph]]]

MultiProp[multi_, "CausalBranchialGraph", n_Integer : 1, opts___] :=
    CausalBranchialGraph[multi["CausalGraph", n, FilterRules[{opts}, FilterRules[Options[EvolutionGraph], Except[Options[Graph]]]]], FilterRules[{opts}, Options[CausalBranchialGraph]]]

MultiProp[multi_, "CausalStatesGraph", n_Integer : 1, opts___] :=
    CausalStatesGraph[multi["CausalGraph", n, FilterRules[{opts}, FilterRules[Options[EvolutionGraph], Except[Options[Graph]]]]], FilterRules[{opts}, Options[CausalStatesGraph]]]

