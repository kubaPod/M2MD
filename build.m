(* ::Package:: *)

BeginPackage["M2MDBuild`"];


Needs["M2MD`"];


  $project = DirectoryName[$InputFileName /. "" :> NotebookFileName[]];
  $projectName = FileBaseName @ $project;

  M2MDBild;

Begin["`Private`"];

  M2MDBild[]:= buildMainPalette[];

  buildMainPalette[]:=Module[{nb}
    , nb = mainPalette[]
    ; NotebookSave[
      nb
      , FileNameJoin[{$project, $projectName, "FrontEnd", "Palettes", $projectName <> ".nb"}]
    ]
    ; NotebookClose[nb]
  ]

  mainPalette[]:= CreatePalette[
    Button[
      "Export to Markdown"
    , Needs["M2MD`"]
    ; CreateDocument[
        Cell[
          M2MD @ InputNotebook[]
        , "Program"
        ]
      ]
    , Method -> "Queued"
    , ImageSize -> CurrentValue @ "DefaultButtonSize"
    ]
  , WindowTitle -> $projectName
  ];





End[];
EndPackage[];
