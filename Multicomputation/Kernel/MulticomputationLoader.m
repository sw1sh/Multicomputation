(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   SemanticImport does its autoloading defs via sysinit.m, so this list must match the DeclareLoad call in that file.
*)



PacletManager`Package`loadWolframLanguageCode[
    "Wolfram`Multicomputation",
    "Wolfram`Multicomputation`",
    ParentDirectory[DirectoryName[$InputFileName]],
    "Kernel/Multicomputation.m",
    "AutoUpdate" -> False,
    "AutoloadSymbols" -> {
        "Wolfram`Multicomputation`Multi"
    },
    "HiddenImports" -> {}
];

