(* ::Package:: *)

(* ::Chapter:: *)
(* Metadata*)


(* Mathematica Package *)

(* :Title: M2MD *)
(* :Context: M2MD` *)
(* :Author: Kuba (kuba.pod@gmail.com) *)
(* :Date: Thu 24 May 2018 12:44:21 *)

(* :Keywords: *)
(* :Discussion: *)



(*TODO: inline cells support*)


(* ::Chapter:: *)
(* Begin package*)


BeginPackage["M2MD`"];

Unprotect["`*", "`*`*"]
ClearAll["`*", "`*`*"]

M2MD::usage = "M2MD[obj] converts object to markdown string";
MDExport::usage = "MDExport[\"path/to.md\", obj]"

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


(* ::Subsection:: *)
(*MDExport*)


MDExport // Options = {
  "ImagesExportURL" -> Automatic, (*Automatic | None | path_String*)
  "ImagesFetchURL" -> "Relative", (*Automatic | "Relative" | path_String*)    
  "IgnoredStyles" -> None,
  "ImageNameFunction" -> Automatic,
  "OverwriteImages" -> True, (*boole*)
  "MDElementTemplates" -> Automatic (* _String | template_ *)
}


MDExport[path_String , obj_, patt : OptionsPattern[]]:= Export[
  path
, M2MD[obj
  , "ImagesExportURL" -> FileNameJoin[{FileNameDrop @ ExpandFileName @ path, "img"}]
  , "ImagesFetchURL"  -> "Relative"
  , patt (*will overwrite that path if needed*)
  ] 
, "Text"
]

MDEnvironment // Options = Options @ MDExport;

MDEnvironment[___, OptionsPattern[] ]:= Function[
  expr
, Internal`InheritedBlock[
    { MDElement, $MDMonitor = PrintTemporary }
  , MDElementInit @ Association @ OptionValue @ "MDElementTemplates"
  ; expr
  ]
, HoldAll

]


(* ::Subsection:: *)
(*M2MD*)


M2MD // Attributes = {HoldAllComplete};

M2MD[args___] /; Not @ TrueQ @ $MDEnvironment := Internal`InheritedBlock[
  {$MDEnvironment = True, M2MD}
, Attributes[M2MD] = {}
; Update[M2MD]
; MDEnvironment[args] @ M2MD[args]
]




M2MD // Options = MDExport // Options


M2MD[nb_NotebookObject, patt: OptionsPattern[]] :=  Module[
  { cells, ignoredStyles = OptionValue["IgnoredStyles"] }
, cells = Cells @ nb
; If[ MatchQ[ignoredStyles, {__String}]
  , cells = Complement[cells, Cells[nb, CellStyle->ignoredStyles]]
  ]
  
; ProcessMDString @
  CombineMDCells  @ 
  Map[ M2MD[#, patt]& ] @ 
  cells
]


CombineMDCells = StringJoin @ Map[ToString] @ Riffle[#, "\n\n"]& 


M2MD[cellObj_CellObject, patt: OptionsPattern[]] :=  M2MD[NotebookRead[cellObj], cellObj, patt];


M2MD[ Cell[content_, style_, ___], src : _CellObject : Missing[], patt:OptionsPattern[]] := M2MD[style, content, src, patt];


M2MD[style_, data_, cellObj:_CellObject:Missing[], ___] := Module[{spec = StyleToElement[style] }
, If[
    StringQ @ spec
  , MDElement[spec,  parseData[data] ]
  , MDElement[spec[[1]], spec[[2]] @ data]
  ]
]


StyleToElement[style_]:= Switch[style
, "Title",         "h1"
, "Subtitle",      "Bold"
, "Subsubtitle",   "Bold"
, "Section",       "h2"
, "Subsection",    "h3"
, "Subsubsection", "h4"
, "Subsubsubsection", "h5"
, "Subsubsubsubsection", "h6"
, "Code" | "Input", {"CodeBlock", parseCodeData}
, _ , "Text"
]


(*TODO: update*)
M2MD[style_?itemStyleQ, data_, cellObj_CellObject, ___] := StringJoin @ {
  prefix["items"][cellObj, style]
, parseData@data
};


itemStyleQ = (StringCount[#, "item", IgnoreCase -> True] >  0) &;


M2MD["Output", BoxData[FormBox[boxes_, TraditionalForm]], cellObj_CellObject, ___] := MDElement["LaTeXBlock", BoxesToTeX@boxes ];


M2MD["Output", data:BoxData[_?simpleOutputQ], cellObj_CellObject, OptionsPattern[]] := MDElement["Output", BoxesToString @ data]


M2MD["Output", data:_BoxData, cellObj_CellObject, patt:OptionsPattern[]] := ToImageElement[cellObj, patt]


    (*default behaviour for cell styles*)
M2MD[s_, data___] := MDElement["Comment", s, Head @ data];

M2MD[box_]:= parseData[box];



(* ::Subsection::Closed:: *)
(*ToImageElement*)


ToImageElement // Options = M2MD // Options

ToImageElement[source_, patt : OptionsPattern[]]:=Module[{ baseName, exportDir, exportPath, fetchDir, fetchPath, res, fromCellQ, overwriteQ}

, fromCellQ = MatchQ[source, _CellObject]
; overwriteQ = OptionValue["OverwriteImages"]

; baseName = ToImageName[source, patt]


; exportDir = Switch[ OptionValue["ImagesExportURL"]
  , Automatic      , FileNameJoin[{Directory[], "img"}]
  , _String | _File, OptionValue["ImagesExportURL"] /. File -> Identity
  , None | _       , Return["", Module]
  ]  
; exportPath = FileNameJoin[{exportDir, baseName<>".png"}]

; fetchDir  = Switch[ OptionValue["ImagesFetchURL"]
, Automatic             , exportDir
, "Relative"            , FileNameTake[ exportDir ] (*img/*)
, _String | _URL | _File, OptionValue["ImagesFetchURL"]
, _                     , Return["", Module]
  ]
; fetchPath = urlNameJoin[{fetchDir, baseName<>".png"}]

; If[overwriteQ && FileExistsQ[exportPath]
  , $MDMonitor["Skipping ", baseName]; Return[MDElement["Image", baseName, fetchPath], Module]
  ]


; If[ Not @ DirectoryQ @ exportDir, CreateDirectory[exportDir, CreateIntermediateDirectories->True]]

; res = Export[exportPath, If[fromCellQ, source, Cell@BoxData @ source]]
; If[ res === $Failed, Return[ MDElement["Comment", "Failed to export image"], Module] ]

; MDElement["Image", baseName, fetchPath]
]


simpleOutputQ = FreeQ @ Except[List|RowBox|SuperscriptBox, _Symbol]


urlNameJoin[list_List ? (MemberQ[_URL]) ] := URLBuild[list /. URL -> Identity]
urlNameJoin[list_List ] := FileNameJoin[ list /. File -> Identity]


ToImageName // Options = Options @ MDExport;

ToImageName[source_, OptionsPattern[] ]:= ToImageName[source, OptionValue["ImageNameFunction"]]

ToImageName[source_        , Automatic]:= ToImageName[source, "ExpressionHash"]
ToImageName[cell_CellObject, Automatic]:= FirstCellTag @ cell // Replace[{} :> ToImageName[cell, "ExpressionHash"] ]
ToImageName[source_        , foo_]     := foo @ source // Replace[Except[_String] :> ToImageName[source, "ExpressionHash"] ]

(*TODO: can we avoid reading it twice? export to png probably re-does it*)
ToImageName[cell_CellObject, "ExpressionHash"]:= Hash[First @ NotebookRead @ cell, "Expression", "Base36String"]
ToImageName[source_, "ExpressionHash"]        := Hash[source, "Expression", "Base36String"]



FirstCellTag[cell_CellObject]:= FirstCellTag @ CurrentValue[EvaluationCell[], CellTags]
FirstCellTag[tag_String]:=tag;
FirstCellTag[{}]:={};
FirstCellTag[{tag_String, ___}]:=tag;


(* ::Subsection::Closed:: *)
(*prefixes*)


addPrefix[style_][expr : Except[_String]] := expr;


addPrefix[style_][s_String] :=  StringReplace[s, "\n" -> "\n" <> prefix[style]];


itemIndent = ConstantArray[" ", 3];
codeIndent = ConstantArray[" ", 4];
itemMark = "+ ";


itemPrefix[cellObj_, style_]:=Module[
  { ind, depth, numberedQ, paragraphQ}
, ind = ToString@CurrentValue[cellObj, {"CounterValue", style}]

; depth = StringCount[style, "sub", IgnoreCase -> True]

; numberedQ = StringCount[style, "numbered", IgnoreCase -> True] > 0

; paragraphQ = StringCount[style, "paragraph", IgnoreCase -> True] > 0

; StringJoin @ Flatten @ {
    ConstantArray[itemIndent, depth + If[paragraphQ, 2, 1]]
  , Which[
      numberedQ, {ind, ". "}
    , paragraphQ, ""
    , True, itemMark
    ]
  }
];


prefix[styleName_] := Switch[styleName

, "items",         itemPrefix
, "code",          codeIndent
, _  , ""
];


(* ::Subsection::Closed:: *)
(*style wrapper*)


ToStyleElementFunction[opts___] := Module[
  {italic, bold, wrapper }

, bold = MemberQ[{opts}, Verbatim[Rule][FontWeight, "Bold"]]
; If[ bold, Return @ MDElement["Bold", # ]& ]

; italic = MemberQ[{opts}, Verbatim[Rule][FontSlant, "Italic"]]
; If[ italic, Return @ MDElement["Italic", # ]&]

; Identity
];


(* ::Subsection:: *)
(*parse cell data*)


parseData[list_List] := StringJoin[parseData /@ list];


parseData[string_String] := string;


parseData[cell_Cell] :=  parseData@First@cell; (*inline cells style skipped*)


parseData[Cell[boxes_BoxData, ___]]:=MDElement["CodeInline", BoxesToString @  boxes]


parseData[ Cell[BoxData[FormBox[boxes_, TraditionalForm]], ___] ]:= MDElement["LaTeXInline", BoxesToTeX @ boxes]


parseData[data:(_BoxData | _TextData)] := parseData @ First @ data;


parseData[StyleBox[expr_, opts___]] := ToStyleElementFunction[opts] @ parseData[expr];


parseData[FormBox[boxes : Except[_TagBox], TraditionalForm, ___]] :=  MDElement["LaTeXInline", BoxesToTeX@boxes]


parseData[ TemplateBox[{lbl_String, {url_String, tag_}, note_}, "HyperlinkDefault", ___]] := MDElement["Hyperlink", parseData @ lbl, url]
parseData[ TemplateBox[{lbl_String, url_}, "HyperlinkURL", ___]]                          := MDElement["Hyperlink", parseData @ lbl, url]
parseData[ bbox:ButtonBox[lbl_String, ___, BaseStyle -> "Hyperlink", ___]]                := MDElement["Hyperlink", parseData @ lbl, ToExpression[bbox][[2]] ]

parseData[ bbox:ButtonBox[lbl_, ___, BaseStyle -> "Hyperlink", ___]]                := MDElement["Hyperlink", ToString@#, ToString@#2 ]& @@ ToExpression[bbox]

parseData[ ButtonBox[lbl_, ___, ButtonData -> (s_String ? (StringStartsQ["paclet:"])), ___] ]:=
  MDElement["Hyperlink", parseData @ lbl, "https://reference.wolfram.com/language/" <> StringTrim[s, "paclet:"]]


parseData[ graphics:(_GraphicsBox| _GraphicsBox3D) ]:=ToImageElement[graphics]


   (*default behaviour for boxes*)
parseData[boxes_] := ToImageElement[boxes];


(* ::Subsection::Closed:: *)
(*MDElement*)


$MDElementTemplates = <|
    "LaTeXBlock" -> "$$``$$"
  , "LaTeXInline"-> "$``$"
  , "Image"      -> "![``](``)"
  , "Hyperlink"  -> "[``](``)"
  , "Text"       -> "``"
  , "Bold"       -> "**``**"
  , "Italic"     -> "*``*"

  , "h1" -> "# <*StringReplace[#, \"\n\"->\"<br>\"]*>"
  , "h2" -> "## <*StringReplace[#, \"\n\"->\"<br>\"]*>"
  , "h3" -> "### <*StringReplace[#, \"\n\"->\"<br>\"]*>"
  , "h4" -> "#### <*StringReplace[#, \"\n\"->\"<br>\"]*>"
  , "h5" -> "##### <*StringReplace[#, \"\n\"->\"<br>\"]*>"
  , "h6" -> "###### <*StringReplace[#, \"\n\"->\"<br>\"]*>"

  , "Comment"   -> "[//]: # (``)"
  , "CodeBlock" -> TemplateExpression @ StringJoin["```mathematica\n", TemplateSlot[1], "\n```"]
  , "CodeInline" -> TemplateExpression @ StringJoin["``", TemplateSlot[1], "``"]
  , "Output"    -> TemplateExpression @ StringJoin["```\n(*", TemplateSlot[1], "*)\n```"]

|>;


MDElement::unknownTag = "Unknown MDElement tag: ``.";

MDElement[args___]:= ( Message[MDElement::unknownTag, args]; "");


MDElementInit[ r : KeyValuePattern[{}] ]:= KeyValueMap[MDElementInit, r];

MDElementInit[ tag_String, template_String]:= MDElementInit[tag, StringTemplate @ template];

MDElementInit[ tag_String, template_]:= (MDElement[tag, args__]:=TemplateApply[template, {args}] )

MDElementInit[Automatic]:={};


MDElementInit @ $MDElementTemplates;

  




(* ::Subsection:: *)
(*BoxToString*)


BoxesToTeX[boxes_] := ToString[ DisplayForm[boxes], TeXForm];


BoxesToString[ boxData_]:= BoxesToString[boxData, "PlainText"]
BoxesToString[ boxData_, type_]:= First @ FrontEndExecute @ FrontEnd`ExportPacket[boxData, type]


parseCodeData[data_] := StringReplace[
  BoxesToString[data]
, "\r\n"|"\n" -> "\n" <> codeIndent
];


(* ::Subsection:: *)
(*ProcessMDString*)


ProcessMDString[ md_String ]:= StringReplace[md, 
  { 
    FromCharacterCode[8232] -> "\n"     (*line separator*)
  , "```"~~ ("\n"...)~~"```\n" -> "\n" (*merge next output and input cells*)

    (*TODO: restrict it to pre v12.1 and maybe only include it in BoxesToString?*)
  , "\\[Rule]"           -> "->"
  , "\\[RuleDelayed]"    -> ":>"
  , "\\[LessEqual]"      -> "<="
  , "\\[GreaterEqual]"   -> ">="
  , "\\[NotEqual]"       -> "!="
  , "\\[Equal]"          -> "=="
  , "\\[InlinePart]"     -> "@>"
  , "\\[TwoWayRule]"     -> "<->"
  , "\\[LongRightArrow]" -> "-->"
  }
] 


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
