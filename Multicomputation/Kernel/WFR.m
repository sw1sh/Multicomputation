$WFR = {
    <|
        "Name" -> "MultiReplace",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/nikm/Resources/e95/e95e886d-0871-40ac-9167-a03825e05f6b/download/DefinitionData"
    |>,
    <|
        "Name" -> "MultiEvaluate",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/nikm/Resources/db9/db9d6451-6da9-4f52-8464-4fb2f1d88184/download/DefinitionData"
    |>,
    <|
        "Name" -> "TokenEventGraph",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/cb8a4383-cdcb-4ef0-9b7e-5acd4edbc4e2"
    |>,
    <|
        "Name" -> "TraceCausalGraph",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/nikm/Resources/297/2979a6b7-6a0d-421e-a09c-70d779019894/download/DefinitionData"
    |>
}

makeSymbolName[uuid_String, name_String] := "FunctionRepository`$" <> StringDelete[uuid, "-"] <> "`" <> name;

$WFR = Append[#, {"ResourceType" -> "Function", "SymbolName" -> makeSymbolName[Import[#["FunctionLocation"]]["UUID"], #["Name"]]}] & /@ $WFR

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

