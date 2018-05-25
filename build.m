(* ::Package:: *)

BeginPackage["M2MDBuild`"];


Needs["M2MD`"];


  

  M2MDBild;

Begin["`Private`"];

  $project = DirectoryName[$InputFileName /. "" :> NotebookFileName[]];
  $projectName = FileBaseName @ $project;

  M2MDBild[]:= buildMainPalette[];

  needDirectory[dir_] :=  If[ DirectoryQ @ dir,dir,    CreateDirectory[dir, CreateIntermediateDirectories -> True]];

  buildMainPalette[]:=Module[{nb, deployDir}
    , nb = mainPalette[]
    ; deployDir = needDirectory @ FileNameJoin[{$project, $projectName, "FrontEnd", "Palettes"}]
    ; NotebookSave[
      nb
    , FileNameJoin[{deployDir, $projectName <> ".nb"}]
    ]
    ; NotebookClose[nb]
  ]

  mainPalette[]:= CreatePalette[
    Button[
      "Convert Notebook to Markdown"
    , Needs["M2MD`"]
    ; CreateDocument[
        Cell[
          M2MD @ InputNotebook[]
        , "Program"
        ]
      ]
    , Method -> "Queued"
    , FrameMargins -> 15
    , ImageMargins -> 15
    ]
  , WindowTitle -> $projectName
  ];





End[];
EndPackage[];
