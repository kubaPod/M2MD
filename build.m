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
];

mainPalette[]:= CreatePalette[
  DynamicModule[{processing = False, path = None}
    , With[{
    progressBar = ProgressIndicator[Appearance -> "Indeterminate"]
    , button = Button[
      "Convert Notebook to Markdown"
      , processing = True
      ; Needs["M2MD`"]
      ; path = If[!StringQ[path], $HomeDirectory, path]
      ; path = SystemDialogInput["FileSave", {path, {"Markdown files"->{"*.md"},"Text files"->{"*.md","*.txt"},"All files"->{"*"}}}]
      ; If[path != $Canceled,
          SetDirectory@DirectoryName[path]
        ; MDExport[path,InputNotebook[]]]
      ; processing = False
      , Method -> "Queued"
      , FrameMargins -> 15
      , ImageMargins -> 15
    ]
  }
    , PaneSelector[
        {True -> progressBar, False -> button}
      , Dynamic[processing + 0] (* XD https://mathematica.stackexchange.com/q/173940/5478*)
      , Alignment -> {Center,Center}
      ]
  
  ]
  ]
  , WindowTitle -> $projectName

];





End[];
EndPackage[];
