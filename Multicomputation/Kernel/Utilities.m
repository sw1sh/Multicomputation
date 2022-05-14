Package["Wolfram`Multicomputation`"]

PackageScope["wrap"]
PackageScope["HoldPosition"]
PackageScope["HeadDepth"]
PackageScope["HoldApply"]
PackageScope["HoldMapApply"]
PackageScope["ApplyHold"]
PackageScope["OwnValueHead"]
PackageScope["ElementPositions"]
PackageScope["MultiPlaceholder"]



wrap[expr_, head_ : List] := Replace[expr, x : Except[_head] :> head[x]]

HoldPosition[sym_Symbol[args___]] := With[{attrs = Attributes[sym], len = Length @ Unevaluated @ {args}},
    Which[
        MemberQ[attrs, HoldFirst],
        {1},
        MemberQ[attrs, HoldRest],
        Range[2, len],
        MemberQ[attrs, HoldAll | HoldAllComplete],
        Range @ len,
        True,
        {}
    ]
]

SetAttributes[HoldPosition, HoldAllComplete]


HeadDepth[expr_, head_ : List] := If[
	MatchQ[Unevaluated @ expr, _head],
	1 + Min[Function[elem, HeadDepth[Unevaluated @ elem, head], HoldFirst] /@ Unevaluated @ expr],
	0
]


HoldApply[f_, args___] :=  Function[Null, f[Unevaluated@#, args], HoldAllComplete]

HoldMapApply[f_] := Function[Null, f @@ Unevaluated @ #, HoldAllComplete]

ApplyHold[f_, args___] := Apply[Function[Null, f @@ Unevaluated /@ Hold[##, args], HoldAllComplete]]


OwnValueHead[sym_Symbol] := With[{values = OwnValues[sym]},
    If[
        Length @ values > 0,
        With[{rhs = Unevaluated @@ Extract[values, {1, 2}, Hold]}, Head[rhs]],
        $Failed
    ]
]

SetAttributes[OwnValueHead, HoldFirst]


ElementPositions[expr_, head_Symbol : List] := Catenate @ MapIndexed[
   Function[Null, If[MatchQ[Unevaluated @ #1, _head], Prepend[First @ #2] /@ ElementPositions[Unevaluated @ #1, head], {#2}], HoldFirst],
   Unevaluated @ expr
]
ElementPositions[_] := {}


MultiPlaceholder /: MakeBoxes[MultiPlaceholder[n_], form_] := "\[Placeholder]"

MultiPlaceholder[n_] := (
    Unprotect[MultiPlaceholder];
    MultiPlaceholder[n] = With[{sym = Symbol["$MultiPlaceholder" <> ToString[n]]}, Format[sym] = "\[Placeholder]"; sym];
    Protect[MultiPlaceholder];
    MultiPlaceholder[n]
)

