$WFR = {
    <|
        "Name" -> "MultiReplace",
        "ResourceType" -> "Function",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/nikm/Resources/e95/e95e886d-0871-40ac-9167-a03825e05f6b/download/DefinitionData"
    |>,
    <|
        "Name" -> "MultiEvaluate",
        "ResourceType" -> "Function",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/nikm/Resources/135/13505f23-b475-4250-a08a-1e2d6a9a88ad/download/DefinitionData"
    |>,
    <|
        "Name" -> "TokenEventGraph",
        "ResourceType" -> "Function",
        "FunctionLocation" -> "https://www.wolframcloud.com/obj/cb8a4383-cdcb-4ef0-9b7e-5acd4edbc4e2"
    |>
}

makeSymbolName[uuid_String, name_String] := "FunctionRepository`$" <> StringDelete[uuid, "-"] <> "`" <> name;

$WFR = Append[#, "SymbolName" -> makeSymbolName[Import[#["FunctionLocation"]]["UUID"], #["Name"]]] & /@ $WFR

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

