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
  "ImagesFetchURL" -> "Relative" (*Automatic | "Relative" | path_String*)    
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



M2MD // Options = {
  "ImagesExportURL" -> None, 
  "ImagesFetchURL" -> "Relative"
}



M2MD[nb_NotebookObject, patt: OptionsPattern[]] :=  ProcessMDString @ StringJoin @ Flatten @ (Riffle[#, "\n\n"]& @ Map[ M2MD[#, patt]& ] @ Cells @ nb );


ProcessMDString[ md_String ]:= StringReplace[md, 
  { FromCharacterCode[8232] -> "\n" (*line separator*)
  , "```"~~ ("\n"...)~~"```\n" -> "\n" (*merge next output and input cells*)
  }
] 


M2MD[cellObj_CellObject, patt: OptionsPattern[]] :=  M2MD[NotebookRead[cellObj], cellObj, patt];


(*TODO: multistyle support*)
M2MD[Cell[content_, style_, ___], cellObj_CellObject, patt:OptionsPattern[]] := M2MD[style, content, cellObj, patt];


M2MD[style_?textStyleQ, data_, cellObj_CellObject, ___] := {
  prefix[style]
, addPrefix[style] /@ Flatten@{parseData[data]}
};


M2MD[style_?itemStyleQ, data_, cellObj_CellObject, ___] := {
  prefix["items"][cellObj, style]
, parseData@data
};


M2MD[style_?codeStyleQ, data_, cellObj_CellObject, ___] := {
  "```mathematica\n"
, parseCodeData@data
, "\n```"
};


M2MD["Output", BoxData[FormBox[boxes_, TraditionalForm]], cellObj_CellObject, ___] := TemplateApply["$$``$$", {boxesToTeX@boxes} ];


M2MD["Output", data:BoxData[_?simpleOutputQ], cellObj_CellObject, OptionsPattern[]] := {
  "```\n(*", BoxesToPlainText@data, "*)\n```"
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
  , "Relative"            , FileNameTake[ exportDir ] (*img/*) 
  , _String | _URL | _File, OptionValue["ImagesFetchURL"]
  , _                     , Return[{}, Module]  
  ]
; fetchPath = urlNameJoin[{fetchDir, baseName<>".png"}]

; If[ Not @ DirectoryQ @ exportDir, CreateDirectory[exportDir, CreateIntermediateDirectories->True]]

; res = Export[exportPath, cellObj]
; If[ res === $Failed, Return[ {}, Module] ]

; StringTemplate["![``](``)"][baseName, fetchPath]
]


simpleOutputQ = FreeQ @ Except[List|RowBox|SuperscriptBox, _Symbol]


urlNameJoin[list_List ? (MemberQ[_URL]) ] := URLBuild[list /. URL -> Identity]
urlNameJoin[list_List ] := FileNameJoin[ list /. File -> Identity]


    (*default behaviour for cell styles*)
M2MD[s_, data_, ___] := StringTemplate["[//]: # (No rules defined for ``:``)"][s, Head @ data];


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


(* ::Subsection:: *)
(*parse cell data*)


parseData[list_List] := parseData /@ list;


parseData[string_String] := string;


parseData[data:(_BoxData | _TextData)] := List @@ (parseData /@ data);


parseData[cell_Cell] :=  parseData@First@cell; (*inline cells style skipped*)


parseData[StyleBox[expr_, opts___]] := styleWrapper[opts]@parseData[expr];


parseData[FormBox[boxes : Except[_TagBox], TraditionalForm, ___]] :=  Module[{teXForm}
, teXForm = boxesToTeX@boxes
; "$" <> teXForm <> "$"
];


parseData[ TemplateBox[{lbl_String, {url_String, tag_}, note_}, "HyperlinkDefault", ___]] := MDElement["Hyperlink", parseData @ lbl, url]
parseData[ TemplateBox[{lbl_String, url_}, "HyperlinkURL", ___]]                          := MDElement["Hyperlink", parseData @ lbl, url]
parseData[ bbox:ButtonBox[lbl_String, ___, BaseStyle -> "Hyperlink", ___]]                := MDElement["Hyperlink", parseData @ lbl, ToExpression[bbox][[2]] ]

parseData[ bbox:ButtonBox[lbl_, ___, BaseStyle -> "Hyperlink", ___]]                := MDElement["Hyperlink", ToString@#, ToString@#2 ]& @@ ToExpression[bbox]


   (*default behaviour for boxes*)
parseData[boxes_] := ToString @ boxes;


(* ::Subsection:: *)
(*MDElement*)


MDElement["Hyperlink", label_, url_String]:= MDElement["Hyperlink", StringJoin @ Flatten @ { label}, url];

MDElement["Hyperlink", label_String, url_String]:= StringTemplate["[``](``)"][  label, url ];


MDElement::missingRule = "Malformed MDElement! (``)";
MDElement[args___]:=(Message[MDElement::missingRule, args];"");



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
