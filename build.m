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
  
  DynamicModule[{processing = False}
    , With[{
    progressBar = ProgressIndicator[Appearance -> "Indeterminate"]
    , button = Button[
      "Convert Notebook to Markdown"
      , processing = True
      ; Needs["M2MD`"]
      ; CreateDocument[ Cell[ M2MD @ InputNotebook[], "Program" ] ]
      ; processing = False
      , Method -> "Queued"
      , FrameMargins -> 15
      , ImageMargins -> 15
    ]
  }
    , Pane[
      Dynamic[
        If[
          TrueQ @ processing,
          Overlay[{progressBar, Invisible@button}, All, 1, Alignment -> {Center,Center}, ImageSize ->All],
          button
        ]
      ]
      , ImageSize->All
    ]
  
  ]
  ]
  , WindowTitle -> $projectName

];





End[];
EndPackage[];
