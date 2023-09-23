$WFR = {
    <|
        "Name" -> "MultiReplace",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/nikm/Resources/e95/e95e886d-0871-40ac-9167-a03825e05f6b/download/DefinitionData"
    |>,
    <|
        "Name" -> "MultiEvaluate",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/nikm/Resources/750/750c617b-fcac-40f3-9dc8-80cafbd5e8ac/download/DefinitionData"
    |>,
    <|
        "Name" -> "TokenEventGraph",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/cb8a4383-cdcb-4ef0-9b7e-5acd4edbc4e2"
    |>,
    <|
        "Name" -> "TraceCausalGraph",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/nikm/Resources/575/575e1866-276b-4229-98f0-6a3f265b4b50/download/DefinitionData"
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

