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
        "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/nikm/Resources/3f6/3f6e2e2e-f3a1-452d-9246-3e40bfee86de/download/DefinitionData"],
        "SymbolName" -> "FunctionRepository`$3f6e2e2ef3a1452d92463e40bfee86de`MultiEvaluate"
    |>,
    <|
        "Name" -> "TokenEventGraph",
        "ResourceType" -> "Function",
        "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/nikm/Resources/815/81552af8-c6fa-496d-8b9c-e0097811b1ce/download/DefinitionData"],
        "SymbolName" -> "FunctionRepository`$81552af8c6fa496d8b9ce0097811b1ce`TokenEventGraph"
    |>
}

Scan[data |->
    With[{sym = Symbol[data["Name"]],
        wfr = If[MissingQ[data["FunctionLocation"]], ResourceFunction[data["Name"]], ResourceFunction[ResourceObject[data]]]
    },
        sym;
        sym[args___] := wfr[args];
        Options[sym] = Options[wfr];
        SetAttributes[sym, HoldAllComplete]
    ],
    $WFR
]

