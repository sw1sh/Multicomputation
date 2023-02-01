Package["Wolfram`Multicomputation`"]

PackageExport["MultiReplace"]
PackageExport["MultiEvaluate"]
PackageExport["TokenEventGraph"]

$WFR = {
    <|
        "Name" -> "MultiReplace",
        "ResourceType" -> "Function",
        "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/nikm/Resources/46f/46fb8cf0-fb3c-42e2-bb1a-b5e3b779acd5/download/DefinitionData"],
        "SymbolName" -> "FunctionRepository`$46fb8cf0fb3c42e2bb1ab5e3b779acd5`MultiReplace"
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
    With[{sym = Symbol @ data["Name"], wfr = If[MissingQ[data["FunctionLocation"]], ResourceFunction[data["Name"]], ResourceFunction[ResourceObject[data]]]},
        sym[args___] := wfr[args];
        Options[sym] = Options[wfr];
        SetAttributes[sym, HoldAllComplete]
    ],
    $WFR
]

(*
MultiReplace[args___] := With[{rf = MultiReplaceRF}, rf[args]]

Options[MultiReplace] = Options[MultiReplaceRF]

SetAttributes[MultiReplace, HoldAllComplete]


MultiEvaluateRF = ResourceFunction[ResourceObject[<|
    "Name" -> "MultiEvaluate",
    "ResourceType" -> "Function",
    "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/nikm/Resources/3f6/3f6e2e2e-f3a1-452d-9246-3e40bfee86de/download/DefinitionData"],
    "SymbolName" -> "FunctionRepository`$3f6e2e2ef3a1452d92463e40bfee86de`MultiEvaluate"
|>]]

MultiEvaluate[args___] := With[{rf = MultiEvaluateRF}, rf[args]]

Options[MultiEvaluate] = Options[MultiEvaluateRF]

SetAttributes[MultiEvaluate, HoldAllComplete]



TokenEventGraphRF = ResourceFunction[ResourceObject[<|
    "Name" -> "TokenEventGraph",
    "ResourceType" -> "Function",
    "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/nikm/Resources/815/81552af8-c6fa-496d-8b9c-e0097811b1ce/download/DefinitionData"],
    "SymbolName" -> "FunctionRepository`$81552af8c6fa496d8b9ce0097811b1ce`TokenEventGraph"
|>]]

TokenEventGraph[args___] := With[{rf = TokenEventGraphRF}, rf[args]]

Options[TokenEventGraph] = Options[TokenEventGraphRF]

SetAttributes[TokenEventGraph, HoldAllComplete]
 *)
