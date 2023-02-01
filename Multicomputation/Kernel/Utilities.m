Package["Wolfram`Multicomputation`"]

PackageScope["wrap"]
PackageScope["HoldPosition"]
PackageScope["HoldPositionQ"]
PackageScope["SequenceHoldQ"]
PackageScope["HeadDepth"]
PackageScope["HoldApply"]
PackageScope["HoldMapApply"]
PackageScope["ApplyHold"]
PackageScope["OwnValueHead"]
PackageScope["ElementPositions"]
PackageScope["MultiPlaceholder"]
PackageScope["PatternHead"]



wrap[expr_, head_ : List] := Replace[expr, x : Except[_head] :> head[x]]

HoldPositionFromAttributes[attrs_, len_] :=
    Which[
        MemberQ[attrs, HoldAll | HoldAllComplete] || ContainsAll[attrs, {HoldFirst, HoldRest}],
        Range @ len,
        MemberQ[attrs, HoldFirst],
        {1},
        MemberQ[attrs, HoldRest],
        Range[2, len],
        True,
        {}
    ]

HoldPosition[sym_Symbol[args___]] := HoldPositionFromAttributes[Attributes[sym], Length[HoldComplete[args]]]
HoldPosition[Verbatim[Function][_, _, attrs___][args___]] := HoldPositionFromAttributes[Flatten[{attrs}], Length[HoldComplete[args]]]
HoldPosition[_[___]] := {}
HoldPosition[___] := Missing["Position"]

HoldPositionQ[expr_, i_] := With[{pos = HoldPosition[Unevaluated[expr]]}, MissingQ[pos] || MemberQ[pos, i]]


SequenceHoldQ[sym_Symbol[___]] := MemberQ[Attributes[sym], SequenceHold]
SequenceHoldQ[Verbatim[Function][_, _, attrs___][args___]] := MemberQ[attrs, SequenceHold]
SequenceHoldQ[_] := False


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


ElementPositions[expr_, head_Symbol : List] := List @@ Flatten[
    MapIndexed[
        Function[Null, If[MatchQ[Unevaluated @ #1, _head], Prepend[First @ #2] /@ ElementPositions[Unevaluated @ #1, head], {#2}], HoldFirst],
        Unevaluated @ expr
    ],
    1
]
ElementPositions[_] := {}


MultiPlaceholder /: MakeBoxes[MultiPlaceholder[n_], form_] := "\[Placeholder]"

MultiPlaceholder[n_] := (
    Unprotect[MultiPlaceholder];
    MultiPlaceholder[n] = With[{sym = Symbol["$MultiPlaceholder" <> ToString[n]]}, Format[sym] = "\[Placeholder]"; sym];
    Protect[MultiPlaceholder];
    MultiPlaceholder[n]
)


PatternHead = Pattern |
	Blank | BlankSequence | BlankNullSequence |
	HoldPattern | Verbatim |
	Alternatives | Except |
	PatternSequence | OrderlessPatternSequence | Repeated |
	Longest | Shortest |
	PatternTest | Condition |
	Optional | OptionsPattern | KeyValuePattern | IgnoringInactive;
