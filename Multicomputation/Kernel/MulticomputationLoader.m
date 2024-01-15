BeginPackage["Wolfram`MulticomputationInit`"]

Quiet @ Get[FileNameJoin[{DirectoryName[$InputFileName], "WFR.m"}]]

PacletDependency[paclet_, version : _String | None : None] := With[{paclets = Through[PacletFind[paclet]["Version"]]},
    If[ ! If[version === None, Length[paclets] > 0, AnyTrue[paclets, ResourceFunction["VersionOrder"][#, version] >= 0 &]],
        PacletDirectoryLoad[PacletInstall[If[version === None, paclet, paclet -> version], "InstallLocation" -> "Cached"]["Location"]]
    ]
]

PacletDependency["WolframInstitute/Hypergraph"]

EndPackage[]

PacletManager`Package`loadWolframLanguageCode[
    "Wolfram`Multicomputation",
    "Wolfram`Multicomputation`",
    ParentDirectory[DirectoryName[$InputFileName]],
    "Kernel/Multicomputation.m",
    "AutoUpdate" -> False,
    "AutoloadSymbols" -> {
        "Wolfram`Multicomputation`Multi",
        "Wolfram`Multicomputation`MultiwaySystem"
    },
    "HiddenImports" -> {}
];
