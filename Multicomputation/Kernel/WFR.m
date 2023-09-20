$WFR = {
    <|
        "Name" -> "MultiReplace",
        "ResourceType" -> "Function",
        "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/nikm/Resources/e95/e95e886d-0871-40ac-9167-a03825e05f6b/download/DefinitionData"],
        "SymbolName" -> "FunctionRepository`$e95e886d087140ac9167a03825e05f6b`MultiReplace"
    |>,
    <|
        "Name" -> "MultiEvaluate",
        "ResourceType" -> "Function",
        "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/nikm/Resources/135/13505f23-b475-4250-a08a-1e2d6a9a88ad/download/DefinitionData"],
        "SymbolName" -> "FunctionRepository`$13505f23b4754250a08a1e2d6a9a88ad`MultiEvaluate"
    |>,
    <|
        "Name" -> "TokenEventGraph",
        "ResourceType" -> "Function",
        "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/cb8a4383-cdcb-4ef0-9b7e-5acd4edbc4e2"],
        "SymbolName" -> "FunctionRepository`$a01ca9049fdd4b21abf697f2e4a860d8`TokenEventGraph"
    |>
}

Scan[data |->
    With[{sym = Symbol[$Context <> data["Name"]],
        wfr = If[MissingQ[data["FunctionLocation"]], ResourceFunction[data["Name"]], ResourceFunction[ResourceObject[data]]]
    },
        sym;
        sym[args___] := wfr[args];
        Options[sym] = Options[wfr];
        SetAttributes[sym, HoldAllComplete]
    ],
    $WFR
]

