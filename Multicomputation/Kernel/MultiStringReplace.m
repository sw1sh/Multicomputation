Package["Wolfram`Multicomputation`"]

PackageExport["MultiStringReplace"]



MultiStringReplace[s_String, rule_Rule, OptionsPattern[]] := Module[{
    lhs = wrap[First[rule]],
    rhs = wrap[Last[rule]],
    pos
},
    pos = StringPosition[s, #] & /@ lhs;
    AssociationMap[
        StringReplacePart[s, Take[rhs, UpTo[Length[#]]], Take[#, UpTo[Length[rhs]]]] &,
        ResourceFunction["SelectTuples"][pos, Length[#] === 1 || IntervalIntersection @@ (Interval /@ #) === Interval[] &]
    ]
]

MultiStringReplace[s_String, rules : {_Rule...}, opts : OptionsPattern[]] :=
    Association @ MapIndexed[KeyMap[List /* Prepend[First[#2]]] @ MultiStringReplace[s, #1, opts] &, rules]

