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
  "IgnoredStyles" -> None
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



(* ::Subsection:: *)
(*M2MD*)


M2MD // Options = MDExport // Options


M2MD[nb_NotebookObject, patt: OptionsPattern[]] :=  Module[
  { cells, ignoredStyles = OptionValue["IgnoredStyles"] }
, cells = Cells @ nb
; If[ MatchQ[ignoredStyles, {__String}]
  , cells = Complement[cells, Cells[nb, CellStyle->ignoredStyles]]
  ]
  
; ProcessMDString @ 
  StringJoin @ Map[ToString] @ Riffle[#, "\n\n"]& @ 
  Map[ M2MD[#, patt]& ] @ 
  cells
]


M2MD[cellObj_CellObject, patt: OptionsPattern[]] :=  M2MD[NotebookRead[cellObj], cellObj, patt];


(*TODO: multistyle support*)
M2MD[Cell[content_, style_, ___], cellObj_CellObject, patt:OptionsPattern[]] := M2MD[style, content, cellObj, patt];


M2MD[style_, data_, cellObj_CellObject, ___] := MDElement[StyleToElement@style,  parseData[data] ]


StyleToElement[style_]:= Switch[style
, "Title",         "h1"
, "Subtitle",      "h2"
, "Subsubtitle",   "h3"
, "Section",       "h4"
, "Subsection",    "h5"
, "Subsubsection", "h6"
, _ , "Text"
]


(*TODO: update*)
M2MD[style_?itemStyleQ, data_, cellObj_CellObject, ___] := StringJoin @ {
  prefix["items"][cellObj, style]
, parseData@data
};


M2MD[style_?codeStyleQ, data_, cellObj_CellObject, ___] := MDElement["CodeBlock", parseCodeData@data];


textStyleQ = (StringCount[#, "title" | "section" | "text", IgnoreCase -> True] > 0) &;


itemStyleQ = (StringCount[#, "item", IgnoreCase -> True] >  0) &;


codeStyleQ = MemberQ[{"Code", "Input"}, #] &;


M2MD["Output", BoxData[FormBox[boxes_, TraditionalForm]], cellObj_CellObject, ___] := MDElement["LaTeXBlock", boxesToTeX@boxes ];


M2MD["Output", data:BoxData[_?simpleOutputQ], cellObj_CellObject, OptionsPattern[]] := MDElement["Output", BoxesToPlainText@data]


M2MD["Output", data:_BoxData, cellObj_CellObject, patt:OptionsPattern[]] := ToImageElement[cellObj, patt]


ToImageElement // Options = M2MD // Options

ToImageElement[source_, OptionsPattern[]]:=Module[{ baseName, exportDir, exportPath, fetchDir, fetchPath, res, fromCellQ}
, fromCellQ = MatchQ[source, _CellObject]
; baseName = If[ fromCellQ,
    CurrentValue[source, CellTags] // List // Flatten // ReplaceAll[{} :> {CreateUUID["image-"]}] // First
  , CreateUUID["image-"]
  ]  

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

; If[ Not @ DirectoryQ @ exportDir, CreateDirectory[exportDir, CreateIntermediateDirectories->True]]

; res = Export[exportPath, If[fromCellQ, source, Cell@BoxData @ source]]
; If[ res === $Failed, Return[ MDElement["Comment", "Failed to export image"], Module] ]

; MDElement["Image", baseName, fetchPath]
]


simpleOutputQ = FreeQ @ Except[List|RowBox|SuperscriptBox, _Symbol]


urlNameJoin[list_List ? (MemberQ[_URL]) ] := URLBuild[list /. URL -> Identity]
urlNameJoin[list_List ] := FileNameJoin[ list /. File -> Identity]


    (*default behaviour for cell styles*)
M2MD[s_, data_, ___] := MDElement["Comment", s, Head @ data]


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*parse cell data*)


parseData[list_List] := StringJoin[parseData /@ list];


parseData[string_String] := string;


parseData[cell_Cell] :=  parseData@First@cell; (*inline cells style skipped*)


parseData[data:(_BoxData | _TextData)] := parseData @ First @ data;


parseData[StyleBox[expr_, opts___]] := styleWrapper[opts]@parseData[expr];


parseData[FormBox[boxes : Except[_TagBox], TraditionalForm, ___]] :=  MDElement["LaTeXInline", boxesToTeX@boxes]


parseData[ TemplateBox[{lbl_String, {url_String, tag_}, note_}, "HyperlinkDefault", ___]] := MDElement["Hyperlink", parseData @ lbl, url]
parseData[ TemplateBox[{lbl_String, url_}, "HyperlinkURL", ___]]                          := MDElement["Hyperlink", parseData @ lbl, url]
parseData[ bbox:ButtonBox[lbl_String, ___, BaseStyle -> "Hyperlink", ___]]                := MDElement["Hyperlink", parseData @ lbl, ToExpression[bbox][[2]] ]

parseData[ bbox:ButtonBox[lbl_, ___, BaseStyle -> "Hyperlink", ___]]                := MDElement["Hyperlink", ToString@#, ToString@#2 ]& @@ ToExpression[bbox]

parseData[ ButtonBox[lbl_, ___, ButtonData -> (s_String ? (StringStartsQ["paclet:"])), ___] ]:=
  MDElement["Hyperlink", parseData @ lbl, "https://reference.wolfram.com/language/" <> StringTrim[s, "paclet:"]]


parseData[ graphics:(_GraphicsBox| _GraphicsBox3D) ]:=ToImageElement[graphics]


   (*default behaviour for boxes*)
parseData[boxes_] := ToImageElement[boxes];


(* ::Subsection:: *)
(*MDElement*)


MDElement::missingRule = "Malformed MDElement! (``, ``)";

MDElement[tag_, args___]:= Module[{template} 
, template = Lookup[$MDElementTemplates, tag, Message[MDElement::missingRule, tag, args]; Return["", Module]]
; template // Replace[ s_String :> StringTemplate[s] ]
; TemplateApply[template, {args}]
] 
  


$MDElementTemplates = <|
  "LaTeXBlock" -> "$$``$$"
, "LaTeXInline"-> "$``$"  
, "Image"      -> "![``](``)"
, "Hyperlink"  -> "[``](``)"
, "Text"       -> "``"

, "h1" -> "# <*StringReplace[#, \"\n\"->\"<br>\"]*>"
, "h2" -> "## <*StringReplace[#, \"\n\"->\"<br>\"]*>"
, "h3" -> "### <*StringReplace[#, \"\n\"->\"<br>\"]*>"
, "h4" -> "#### <*StringReplace[#, \"\n\"->\"<br>\"]*>"
, "h5" -> "##### <*StringReplace[#, \"\n\"->\"<br>\"]*>"
, "h6" -> "###### <*StringReplace[#, \"\n\"->\"<br>\"]*>"

, "Comment"   -> "[//]: # (``)"
, "CodeBlock" -> TemplateExpression @ StringJoin["```mathematica\n", TemplateSlot[1], "\n```"]   
, "Output"    -> TemplateExpression @ StringJoin["```\n(*", TemplateSlot[1], "*)\n```"]

|>


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


(* ::Subsection::Closed:: *)
(*ProcessMDString*)


ProcessMDString[ md_String ]:= StringReplace[md, 
  { FromCharacterCode[8232] -> "\n" (*line separator*)
  , "```"~~ ("\n"...)~~"```\n" -> "\n" (*merge next output and input cells*)
  , "\[Rule]" -> "->"
  , "\[RuleDelayed]" -> ":>"
  , "\[LessEqual]" -> "<="
  , "\[GreaterEqual]" -> ">="
  , "\[NotEqual]" -> "!="
  , "\[Equal]" -> "=="
  , "\[InlinePart]" -> "@>"
  , "\[TwoWayRule]" -> "<->"
  , "\[LongRightArrow]" -> "-->"
  }
] 


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
