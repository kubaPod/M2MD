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


$mdExportSpec = <|
  "codeBreak" -> "\n<!-- SE friendly -->\n\n"(*"\n[//]: <> (code break)\n\n"*) (* "\n\n<!-- SE friendly -->\n\n"*)
  
, "ignoredCells" -> {}
  
|>  



(* ::Subsection:: *)
(*M2MD*)


MDExport // Options = {
  "ImagesExportURL" -> Automatic, (*Automatic | None | path_String*)
  "ImagesFetchURL" -> Automatic (*Automatic | "Relative" | path_String*)    
}


MDExport[path_String , obj_, patt : OptionsPattern[]]:= Export[
  path
, M2MD[obj
  , "ImagesExportURL" -> FileNameJoin[{FileNameDrop @ AbsoluteFileName @ path, "img"}]
  , patt (*will overwrite that path if needed*)
  ] 
, "Text"  
]



M2MD // Options = {
  "ImagesExportURL" -> None, 
  "ImagesFetchURL" -> Automatic
}



M2MD[nb_NotebookObject, patt: OptionsPattern[]] :=  StringJoin @ Flatten @ Map[ M2MD[#, patt]& ] @ Cells @ nb ;


M2MD[cellObj_CellObject, patt: OptionsPattern[]] :=  M2MD[NotebookRead[cellObj], cellObj, patt];


(*TODO: multistyle support*)
M2MD[Cell[content_, style_, ___], cellObj_CellObject, patt:OptionsPattern[]] := M2MD[style, content, cellObj, patt];


M2MD[style_?textStyleQ, data_, cellObj_CellObject, ___] := {
  prefix[style]
, addPrefix[style] /@ Flatten@{parseData[data]}
, "\n\n"
};


M2MD[style_?itemStyleQ, data_, cellObj_CellObject, ___] := {
  prefix["items"][cellObj, style]
, parseData@data
, "\n\n"
};


M2MD[style_?codeStyleQ, data_, cellObj_CellObject, ___] := {
  "\n<!-- new code cell break -->\n"
, codeIndent
, parseCodeData@data
, "\n"
};


M2MD["Output", BoxData[FormBox[boxes_, TraditionalForm]], cellObj_CellObject, ___] := TemplateApply["$$``$$\n\n", {boxesToTeX@boxes} ];


M2MD["Output", data:BoxData[_?simpleOutputQ], cellObj_CellObject, OptionsPattern[]] := {
  codeIndent, "(*", BoxesToPlainText@data, "*)\n"
};


M2MD["Output", data:_BoxData, cellObj_CellObject, OptionsPattern[]] := Module[{ baseName, exportDir, exportPath, fetchDir, fetchPath, res}
, baseName = CurrentValue[cellObj, CellTags] // List // Flatten // ReplaceAll[{} :> {CreateUUID["image-"]}] // First

; exportDir = Switch[ OptionValue["ImagesExportURL"]
  , Automatic      , FileNameJoin[{Directory[], "img"}]
  , _String | _File, OptionValue["ImagesExportURL"] /. File -> Identity
  , None | _       , Return[{}, Module]
  ]  
; exportPath = FileNameJoin[{exportDir, baseName<>".png"}]

; fetchDir  = Switch[ OptionValue["ImagesFetchURL"]
  , Automatic             , exportDir
  , "Relative"            , FileNameTake[ exportDir ] (*img/``*) 
  , _String | _URL | _File, OptionValue["ImagesFetchURL"]
  , _                     , Return[{}, Module]  
  ]
; fetchPath = urlNameJoin[{fetchDir, baseName<>".png"}]

; If[ Not @ DirectoryQ @ exportDir, CreateDirectory[exportDir, CreateIntermediateDirectories->True]]

; res = Export[exportPath, cellObj]
; If[ res === $Failed, Return[ {}, Module] ]

; StringTemplate["![``](``)\n"][baseName, fetchPath]
]


simpleOutputQ = FreeQ @ Except[List|RowBox|SuperscriptBox, _Symbol]


urlNameJoin[list_List ? (MemberQ[_URL]) ] := URLBuild[list /. URL -> Identity]
urlNameJoin[list_List ] := FileNameJoin[ list /. File -> Identity]


    (*default behaviour for cell styles*)
M2MD[s_, data_, ___] := StringTemplate["[//]: # (No rules defined for ``:``)\n\n"][s, Head @ data];


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
, "Title",         "# "
, "Subtitle",      "## "
, "Subsubtitle",   "### "
, "Section",       "#### "
, "Subsection",    "##### "
, "Subsubsection", "###### "
, "Text",          ""
, "items",         itemPrefix
, "code",          codeIndent
];


(* ::Subsection::Closed:: *)
(*style wrapper*)


styleWrapper[opts___] := Module[
  {italic, bold, wrapper }
, italic = MemberQ[{opts}, Verbatim[Rule][FontSlant, "Italic"]]
; bold = MemberQ[{opts}, Verbatim[Rule][FontWeight, "Bold"]]
; wrapper = Which[
    bold, "**"
  , italic, "*"
  , True, ""
  ]
; wrapper <> # <> wrapper &
];


(* ::Subsection::Closed:: *)
(*cell type Q*)


textStyleQ = (StringCount[#, "title" | "section" | "text", IgnoreCase -> True] > 0) &;


itemStyleQ = (StringCount[#, "item", IgnoreCase -> True] >  0) &;


codeStyleQ = MemberQ[{"Code", "Input"}, #] &;


(* ::Subsection::Closed:: *)
(*parse cell data*)


parseData[list_List] := parseData /@ list;


parseData[string_String] := string;


parseData[data_ (BoxData | TextData)] := List @@ (parseData /@ data);


parseData[cell_Cell] :=  parseData@First@cell; (*inline cells style skipped*)


parseData[StyleBox[expr_, opts___]] := styleWrapper[opts]@parseData[expr];


parseData[FormBox[boxes : Except[_TagBox], TraditionalForm, ___]] :=  Module[{teXForm}
, teXForm = boxesToTeX@boxes
; "$" <> teXForm <> "$"
];


parseData[ box : ButtonBox[_, ___, BaseStyle -> "Hyperlink", ___]] := Module[{label, url}
, {label, url} = {#, #2} & @@ ToExpression[box]
;  TemplateApply["[``](``)", {StringJoin@Flatten@{parseData@label}, url}]
];


   (*default behaviour for boxes*)
parseData[boxes_] := parseData@First@boxes;


(* ::Subsection:: *)
(*boxesToTeX*)


boxesToTeX = ToString[ToExpression@#, TeXForm] &;


(* ::Subsection:: *)
(*parseCodeData*)


BoxesToPlainText[ boxData_]:= First @ FrontEndExecute @ FrontEnd`ExportPacket[boxData, "PlainText"]


parseCodeData[data_] := StringReplace[
  BoxesToPlainText[data]
, "\r\n"|"\n" -> "\n" <> codeIndent
];


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
