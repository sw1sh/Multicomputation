Package["Wolfram`Multicomputation`"]

PackageScope["MultiProp"]



MultiProp[multi_, "Properties"] := {
    "Expression", "HoldExpression",
    "Data",
    "ListValues", "HoldListValues",
    "EvaluateList", "HoldEvaluateList",
    "EvaluateListOnce", "HoldEvaluateListOnce",
    "Evaluate", "HoldEvaluate",
    "EvaluateOnce", "HoldEvaluateOnce",
    "MultiList",
    "ListEvaluate", "HoldListEvaluate",
    (*"MultiListEvaluate","HoldMultiListEvaluate",
    "MultiEvaluate","HoldMultiEvaluate",*)
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

MultiProp[multi_, "HoldExpression"] := Extract[multi["Data"], "Expression", HoldForm]

MultiProp[multi_, "Keys"] := Join[List /@ Keys @ multi["Values"], Keys @ multi["Matches"]]

MultiProp[multi_, "Positions"] := Replace[{{pos : {_Integer ...}, __} :> pos, pos : {_Integer ...} :> pos}] /@
    Join[Keys @ multi["Values"], Keys @ multi["Matches"]]


MultiProp[multi_, "Size"] := Length@multi["Keys"]

MultiProp[multi_, "ValueCount"] := Times @@ Replace[Function[Null, Length @ Unevaluated @ #, HoldFirst] /@ multi["Values"], <||> -> {0}]

MultiProp[multi_, "MatchCount"] := Total[Function[Null, Length @ Unevaluated @ #, HoldFirst] /@ multi["Matches"]]


MultiProp[multi_, "Options"] := Sequence @@ Join[multi["EvaluateOptions"], multi["ReplaceOptions"]]

MultiProp[multi_, "AllReplaceArguments"] := Sequence[multi["Rules"], multi["ReplaceArguments"], multi["Options"]]

MultiProp[multi_, "ReplaceHead"] := Enclose[ConfirmBy[{multi["ReplaceArguments"]}, MatchQ[{_, _Symbol, ___}]][[2]], List &]


MultiProp[multi_, "ListValues"] := With[{expr = Unevaluated @@ multi["HoldExpression"]},
    KeyValueMap[
        Which[
            MatchQ[#1, {{_Integer ...}, "Eval"}],
            Extract[expr, {First @ #1}],
            MatchQ[#1, {_Integer ...}],
            #2,
            MatchQ[#1, {_, "Rule", _}],
            ReleaseHold @ #2,
            True,
            Function[x, ReplaceList[Unevaluated @ x, #2], HoldAllComplete] @@
            First @ Extract[expr, {First @ #1}, HoldComplete]
        ] &,
        Join[multi["Values"], multi["Matches"]]
    ]
]

MultiProp[multi_, "HoldListValues"] := With[{expr = Unevaluated @@ multi["HoldExpression"]},
    KeyValueMap[
        Function[
            Null,
            Which[
                MatchQ[#1, {{_Integer ...}, "Eval"}],
                Extract[expr, {First @ #1}],
                MatchQ[#1, {_Integer ...}],
                HoldForm /@ Unevaluated @ #2,
                MatchQ[#1, {_, "Rule", _}],
                #2,
                True,
                Function[x, ReplaceList[HoldForm @ x, MapAt[HoldForm, Unevaluated@#2, {All, All}]], HoldAllComplete] @@ 
                First @ Extract[expr, {First @ #1}, HoldComplete]
            ],
            HoldAll
        ],
        Join[multi["Values"], multi["Matches"]]
   ]
]

MultiProp[multi_, "Tuples"] := Tuples @ multi["ListValues"]

MultiProp[multi_, "HoldTuples"] := Tuples @ multi["HoldListValues"]

MultiProp[multi_, "MatchSubstitutions"] := If[MatchQ[#, {_, "Rule", _}], #[[3, 2]], {<||>}] & /@ Keys @ multi["Matches"]

MultiProp[multi_, "Substitutions"] := Join[Table[<||>, #] & /@ Values[Length /@ multi["Values"]], multi["MatchSubstitutions"]]

MultiProp[multi_, "EvaluateList"] := With[{expr = Unevaluated @@ (multi["HoldExpression"] /. Join @@ Catenate @ multi["Substitutions"])},
    Map[ReplacePart[expr, Thread[Replace[{} -> {{}}] /@ multi["Positions"] -> #]] &, multi["Tuples"]]
]

MultiProp[multi_, "EvaluateListOnce"] := With[{pos = multi["Positions"]},
    If[
        Length[pos] > 0,
        MapThread[
            MapThread[
                {pos, subExprs, subst} |-> With[{
                    expr = Unevaluated @@ (multi["HoldExpression"] /. subst)
                },
                    ReplacePart[expr, pos -> #] & /@ Replace[subExprs, e : Except[_List] :> {e}]
                ],
                {Replace[{} -> {{}}] /@ multi["Positions"], #1, #2}
            ] &,
            With[{
                values = multi["ListValues"]
            }, {
                Tuples @ values,
                Tuples @ MapThread[PadRight[#1, Length @ #2, <||>] &, {multi["Substitutions"], values}]
            }
            ]
        ],
        {{multi["Expression"]}}
   ]
]

MultiProp[multi_, "HoldEvaluateList"] := With[{
    expr = multi["HoldExpression"] /. Join @@ Catenate @ multi["Substitutions"]
},
    Map[
        ReplacePart[
            expr,
            MapThread[
                With[{values = Unevaluated @@ Delete[HoldForm[#2], Prepend[1] @* Append[0] /@ Position[#2, _HoldForm]]}, #1 :> values] &,
                {Map[Prepend[1], multi["Positions"]], #}
            ]
        ] &,
        multi["HoldTuples"]
    ]
]

MultiProp[multi_, "HoldEvaluateListOnce"] := With[{pos = Map[Prepend[1], multi["Positions"]]},
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
                values = multi["HoldListValues"]
            },
                {
                    Tuples @ values,
                    Tuples @ MapThread[PadRight[#1, Length@#2, <||>] &, {multi["Substitutions"], values}]
                }
            ]
        ],
        {{multi["HoldExpression"]}}
    ]
]


MultiProp[multi_, "Evaluate"] := Multi[multi["EvaluateList"] /. a_Association :> Splice @ Values[a], multi["AllReplaceArguments"]]

MultiProp[multi_, "HoldEvaluate"] := With[{
    results = Map[If[FreeQ[#, _Multi], #, Evaluate /@ #] &, multi["HoldEvaluateList"]]
},
    Function[Null, Multi[Unevaluated @ #, multi["AllReplaceArguments"]], HoldFirst] @@
        Delete[HoldForm[results], Prepend[1] @* Append[0] /@ Position[results, _HoldForm]]
]

MultiProp[multi_, "Evaluate", n_Integer] := If[Length[multi["Data"]] == 0, multi, multi["Evaluate"]["Evaluate", n - 1]]
MultiProp[multi_, "Evaluate", _Integer ? NonPositive] := multi

MultiProp[multi_, "EvaluateOnce"] := Multi[Catenate @ multi["EvaluateListOnce"] /. a_Association :> Splice @ Values[a], multi["AllReplaceArguments"]]

MultiProp[multi_, "HoldEvaluateOnce"] := With[{results = Map[If[FreeQ[#, _Multi], #, Evaluate /@ #] &, Catenate @ multi["HoldEvaluateListOnce"]]},
    Function[Null, Multi[Unevaluated @ #, multi["AllReplaceArguments"]], HoldFirst] @@
        Delete[HoldForm[results], Prepend[1] @* Append[0] /@ Position[results, _HoldForm]]
]

MultiProp[multi_, "EvaluateOnce", _Integer ? NonPositive] := multi

MultiProp[multi_, "EvaluateOnce", n_Integer?Positive] := multi["EvaluateOnce"]["EvaluateOnce", n - 1]


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
                    "Values" -> KeyMap[Drop[#, 1] &] @ KeySelect[multi["Values"], Take[#, 1] == k &],
                    "Matches" -> KeyMap[MapAt[Drop[#, 1] &, {1}]] @ KeySelect[multi["Matches"], Take[First @ #, 1] == k &]
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
    Multi[newExpr, multi["AllReplaceArguments"]]
]

MultiProp[multi_, "HoldApply", f_, pos : {(_Integer | All | _Span) ...} : {}] := multi["Apply", Function[Null, f[Unevaluated @ #], HoldAllComplete], pos]

MultiProp[multi_, "ListApply", f_] := multi["HoldApply", Unevaluated @@@ f[Hold /@ Unevaluated @ #] &]

MultiProp[multi_, "DeleteDuplicates"] := multi["ListApply", DeleteDuplicates]

MultiProp[multi_, "ReplacePart", rules_List] := With[{unevalMulti = Unevaluated @ Evaluate @ multi},
    Fold[Function[Null, #1["ReplacePart", #2], HoldAllComplete], unevalMulti, rules]]

MultiProp[multi_, "ReplacePart", (Rule | RuleDelayed)[pos_, x_]] := multi["Apply", Function[Null, Unevaluated @ x, HoldAllComplete], pos]


MultiProp[multi_, "ListEvaluate"] := With[{head = multi["ReplaceHead"]},
If[
    MatchQ[multi["HoldExpression"], _[_head]],
    ReleaseHold @ ResourceFunction["MapAtEvaluate"][
        Function[Null, #["EvaluateListOnce"], HoldAllComplete],
        multi["MultiList"],
        {1, All}
    ],
    head[multi["EvaluateListOnce"]]
]
]

MultiProp[multi_, "HoldListEvaluate"] := With[{head = multi["ReplaceHead"]},
If[
    MatchQ[multi["HoldExpression"], _[_head]],
    ReleaseHold @ ResourceFunction["MapAtEvaluate"][
        Function[Null, #["HoldEvaluateListOnce"], HoldAllComplete],
        multi["MultiList"],
        {1, All}
    ],
    head[multi["HoldEvaluateListOnce"]]
]
]

MultiProp[multi_, "ListEvaluateWithKeys"] := With[{head = multi["ReplaceHead"]},
If[
    MatchQ[multi["HoldExpression"], _[_head]],
    Module[{i = 1},
        ReleaseHold @ ResourceFunction["MapAtEvaluate"][
            Function[Null, {MapAt[Prepend[i++], #["Keys"], {All, 1}], #["EvaluateListOnce"]}, HoldAllComplete],
            multi["MultiList"],
            {1, All}
        ]
    ],
    head[{multi["Keys"], multi["EvaluateListOnce"]}]
]
]

MultiProp[multi_, "HoldListEvaluateWithKeys"] := With[{head = multi["ReplaceHead"]},
If[
    MatchQ[multi["HoldExpression"], _[_head]],
    ReleaseHold @ ResourceFunction["MapAtEvaluate"][
        Function[Null, {#["Keys"], #["HoldEvaluateListOnce"]}, HoldAllComplete],
        multi["MultiList"],
        {1, All}
    ],
    {{multi["Keys"], multi["HoldEvaluateListOnce"]}}
]
]

MultiProp[multi_, "HoldMultiListEvaluate"] := Function[Null, Multi[Unevaluated[{##}], multi["AllReplaceArguments"]], HoldAllComplete] @@
    Flatten[HoldForm @@ Catenate @ Catenate @ multi["HoldListEvaluate"]]

MultiProp[multi_, "HoldMultiListEvaluate", n_Integer] := If[
    n > 0,
    multi["HoldMultiListEvaluate"]["HoldMultiListEvaluate", n - 1],
    multi
]

MultiProp[multi_, "MultiListEvaluate"] := Multi[Flatten[(multi["ListEvaluate"] /. a_Association :> Splice @ Values[a]), 3], multi["AllReplaceArguments"]]

MultiProp[multi_, "MultiListEvaluate", n_Integer] := If[n > 0, multi["MultiListEvaluate"]["MultiListEvaluate", n - 1], multi]

MultiProp[multi_, "HoldMultiEvaluate"] := HoldApply[Multi, multi["AllReplaceArguments"]] @@@ Catenate @ multi["HoldEvaluateListOnce"]

MultiProp[multi_, "HoldMultiEvaluate", n_Integer] := If[n > 0, multi["HoldMultiEvaluate"]["HoldMultiEvaluate", n - 1], multi]

MultiProp[multi_, "MultiEvaluate"] := Map[Multi[#, multi["AllReplaceArguments"]] &, Flatten[multi["EvaluateListOnce"] /. a_Association :> Splice @ Values[a], 2]]

MultiProp[multi_, "MultiEvaluate", n_Integer] := If[n > 0, multi["MultiEvaluate"]["MultiEvaluate", n - 1], multi]

MultiProp[multi_, "Events", lvl_Integer : 1] := MapThread[{keys, tos} |->
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
    Thread @ multi["ListEvaluateWithKeys"]
]

MultiProp[multi_, "HoldEvents", lvl_Integer : 1] :=
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
        Thread @ multi["HoldListEvaluateWithKeys"]
    ]

MultiProp[multi_, "Edges", lvl_Integer : 1] := With[{expr = wrap[multi["Expression"]]},
    Catenate @ MapThread[{elem, events} |-> DirectedEdge[elem, Sequence @@ Reverse @ #] & /@ Flatten @ events, {expr, multi["Events", lvl]}]
]

MultiProp[multi_, "HoldEdges", lvl_Integer : 1] := With[{
    expr = If[ MatchQ[multi["HoldExpression"], _[_List]], First @ Map[HoldForm, multi["HoldExpression"], {2}], {multi["HoldExpression"]}]
},
    Catenate @ MapThread[{elem, events} |-> DirectedEdge[elem, Sequence @@ Reverse @ #] & /@ Flatten @ events, {expr, multi["HoldEvents", lvl]}]
]

MultiProp[multi_, prop : "Graph" | "HoldGraph", steps_Integer : 1, lvl_Integer : 2, opts : OptionsPattern[Graph]] :=
Graph[
    DeleteCases[Except[_DirectedEdge]] @ Flatten @ Reap[
        Nest[
            Function[
                With[{
                    edges = Sow[DeleteCases[DirectedEdge[_, _, _Missing]] @ ReleaseHold[#][If[prop === "Graph", "Edges", "HoldEdges"], lvl]]
                },
                    If[ Length @ edges > 0,
                        Hold[
                            Evaluate @ If[
                                prop === "Graph",
                                Multi[#, multi["AllReplaceArguments"]] &,
                                Function[
                                    Null,
                                    Multi[Unevaluated @ {##}, multi["AllReplaceArguments"]],
                                    HoldAll
                                ] @@ Flatten[HoldForm @@ #] &] @ DeleteDuplicates @ edges[[All, 2]]
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


MultiProp[multi_, "RematchRules"] := With[{expr = Unevaluated @@ multi["HoldExpression"]}, Multi[expr, multi["AllReplaceArguments"]]]


MultiProp[multi_, "CausalGraph", n_Integer : 1, opts___] := CausalGraph[multi["Graph", n, 2], opts]

