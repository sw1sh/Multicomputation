Package["Wolfram`Multicomputation`"]



Multi /: MakeBoxes[multi : HoldPattern[Multi[data_ /; AssociationQ[Unevaluated @ data]]], form_] /; multi["ValidQ"] :=
    Function[Null, With[{values = multi["ValueCount"], matches = multi["MatchCount"], expr := #},
        BoxForm`ArrangeSummaryBox[
            "Multi",
            Defer @ multi,
            None,
            {
                {
                    BoxForm`SummaryItem[{
                        ReplaceAll[
                            HoldForm[DisableFormatting @ expr],
                            sym_Symbol /; StringStartsQ[SymbolName[Unevaluated @ sym], "$MultiPlaceholder"] :> System`\[Placeholder]
                        ]
                    }]
                },
                If[values > 0, {BoxForm`SummaryItem[{"Values: ", values}]}, Nothing],
                If[matches > 0, {BoxForm`SummaryItem[{"Matches: ", matches}]}, Nothing]
            },
            {},
            form,
            "Interpretable" -> Automatic
        ]
    ], HoldFirst] @@ multi["HoldExpression"]

