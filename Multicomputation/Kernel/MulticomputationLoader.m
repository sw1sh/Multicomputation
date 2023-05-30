Package["Wolfram`MulticomputationLoader`"]

PackageImport["DocumentationSearch`"]



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


$PacletPath = ExpandFileName[FileNameJoin[{DirectoryName[$InputFileName], ".."}]]

Quiet[CreateDocumentationIndex[FileNameJoin[{$PacletPath, "Documentation", "English"}]]]

