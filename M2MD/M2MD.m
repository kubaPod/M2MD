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

WithLineBreaks::usage = "WithLineBreaks[string] is a helper function that prepends double space to new lines, md parsers consider is a line break."

BoxesToMDString;
BoxesToInputString;
ToImageElement;
BoxesToTeXString;

SmartWrap;


Begin["`Private`"];


(* ::Chapter:: *)
(* Implementation code*)


(* ::Section:: *)
(*MDExport*)


$BoxesToStringType = "InputText";
$CodeLanguage = "mathematica";

MDExport // Options = {
  "ImagesExportURL"   -> Automatic, (*Automatic | None | path_String*)
  "ImagesFetchURL"    -> "Relative", (*Automatic | "Relative" | path_String*)    
  "ImageNameFunction" -> Automatic,
  "OverwriteImages"   -> True, (*boole*)
  
  "IgnoredStyles"     -> None,
  "CellStyleRules"    -> Automatic, (* style_ \[Rule] tag_ | style_ \[Rule] {tag, Function[{style, _Cell}, _]..}*)
  
  "CodeLanguage"      -> $CodeLanguage,
      
  "BoxesToStringType" -> $BoxesToStringType, (*whatever ExportPacket supports*)  
  
  "MDElementTemplates"-> Automatic (* _String | template_ *)
}


MDExport[path_String , obj_, patt : OptionsPattern[]]:=
Export[
  path
, M2MD[obj
  , Normal@Merge[
      {
          "ImagesExportURL" -> FileNameJoin[{FileNameDrop @ ExpandFileName @ path, "img"}]
        , "ImagesFetchURL"  -> "Relative"
        , patt (*will overwrite that path if needed*)
      },
      Last
    ]
  ]
, "Text"
, CharacterEncoding -> "UTF8"
]

MDEnvironment // Options = Options @ MDExport;

MDEnvironment[___, OptionsPattern[] ]:= Function[
  expr
, Internal`InheritedBlock[
    { M2MD, MDElement, $MDMonitor = Hold, $BoxesToStringType, $CodeLanguage }
    
  , MDElementLoad @ OptionValue @ "MDElementTemplates"  
  ; M2MDLoad      @ OptionValue @ "CellStyleRules"

  ; $BoxesToStringType = OptionValue["BoxesToStringType"]
  ; $CodeLanguage      = OptionValue["CodeLanguage"]
    
  ; expr
  ]
, HoldAll

]


(* ::Section:: *)
(*LoadDefinitions*)


LoadDefinitions // Options = {
  "StandardizationFunction" -> Identity,
  "DefinitionFunction" -> Hold
};

LoadDefinitions[ definitions : KeyValuePattern[{}], OptionsPattern[] ]:= With[
  { standardize = OptionValue["StandardizationFunction"] 
  , define = OptionValue["DefinitionFunction"]  
  }
, Module[{std}  
  , std = standardize /@ Normal @ definitions
  ; std = ToDownValue /@ std
  ; std /. RuleDelayed[ _[rhs_], lhs_] :> define[rhs, lhs]  
  ]
]

ToDownValue[ rule_[ rhs:Except[_HoldPattern] , lhs_] ] := HoldPattern[rhs] :> lhs;
ToDownValue[ r_Rule ] := RuleDelayed @@ r


(* ::Section:: *)
(*M2MD (whatever to MD)*)


(* ::Subsection:: *)
(*sugar*)


(*M2MD // Attributes = {HoldAllComplete}; again at the end*)

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
  , cells = DeleteCases[cells, Alternatives @@ Cells[nb, CellStyle->ignoredStyles]]
  ]
  
; ProcessMDString @
  CombineMDCells  @ 
  Map[ M2MD[#, patt]& ] @ 
  cells
]


CombineMDCells = StringJoin @ Map[ToString] @ Riffle[#, "\n\n"]&


(* Syntactic sugar *)

(*TODO: cell groups, raw data, style data *)



M2MD[ contents_TextData, opt: OptionsPattern[]]:= M2MD[ Cell[contents, "Text"],  opt]
M2MD[ contents_BoxData, opt: OptionsPattern[]] :=  M2MD[ Cell[contents, "Output"], opt]

M2MD[ str_String, opt: OptionsPattern[]]:= str;

M2MD[ boxes_, opt : OptionsPattern[] ]:= BoxesToMDString[boxes, False, opt]

M2MD[cellObj_CellObject, opt : OptionsPattern[] ] :=  M2MD[NotebookRead[cellObj], opt];


M2MD[ cell : Cell[_, style_, ___], opt : OptionsPattern[] ] := M2MD[style, cell, opt];

(*Convertions*)


(* ::Subsection:: *)
(*style rules*)


$CellStyleRules = {
  "Title"->"h1",
  "Subtitle"->"Bold",
  "Subsubtitle"->"Bold",
  "Section"->"h2",
  "Subsection"->"h3",
  "Subsubsection"->"h4",
  "Subsubsubsection"->"h5",
  "Subsubsubsubsection"->"h6"
}


M2MDSet[style_String , tag_String]:= M2MDSet[style , {tag, BoxesToMDString[#[[1]]]& }]

M2MDSet[style_String , {tag_String, parsers___}]:= DownValues[M2MD] = Insert[
  DownValues[M2MD]
, HoldPattern @ M2MD[style, cell_Cell, opt: OptionsPattern[] ] :> MDElement[tag, Sequence @@ Through[{parsers}[cell, style] ] ]
, 2
]
(*M2MDSet[style_String , {tag_, parsers__}]:= M2MD[style, cell_, opt: OptionsPattern[] ] := MDElement[tag, BoxesToMDString @ First @ cell]*)


M2MDLoad[defs_]:=LoadDefinitions[   defs, "DefinitionFunction" -> M2MDSet ] 


M2MDLoad @ $CellStyleRules


   (*etc*)

M2MD[ "SlideShowNavigationBar", ___]:= MDElement["ThematicBreak"]

M2MD[ "Output", Cell[inputForm_String, ___], ___]:= MDElement["CodeBlock", inputForm, $CodeLanguage];

M2MD[ style_?InputStyleQ    , cell:_[_?InputFormQ, ___], opts: OptionsPattern[]
] := MDElement["CodeBlock", BoxesToInputString @ cell,  CodeLanguage[style, cell] ]

M2MD[ style_?NumberedStyleQ , cell_, opts: OptionsPattern[]]:= MDElement["NumberedItem", ItemLevel[style], BoxesToMDString[cell, False, opts] ]
M2MD[ style_?ParagraphStyleQ, cell_, opts: OptionsPattern[]]:= MDElement["Paragraph"   , ItemLevel[style], BoxesToMDString[cell, False, opts] ]
M2MD[ style_?ItemStyleQ     , cell_, opts: OptionsPattern[]]:= MDElement["Item"        , ItemLevel[style], BoxesToMDString[cell, False, opts] ]



CodeLanguage["Input" | "Code", _]:=$CodeLanguage;
CodeLanguage["ExternalLanguage", cell_]:= Lookup[Rest @ Rest @ Apply[List] @ cell, CellEvaluationLanguage, "Python"] /. "NodeJS" -> "javascript" // ToLowerCase
CodeLanguage[___]:= ""


InputStyleQ     = MemberQ[{"Input", "Code", "Program", "ExternalLanguage"}, #]& (*TODO, language*)
NumberedStyleQ  = StringContainsQ["itemNumbered", IgnoreCase -> True]
ParagraphStyleQ = StringContainsQ["itemParagraph", IgnoreCase -> True]
ItemStyleQ      = StringContainsQ["item", IgnoreCase -> True]

ItemLevel = StringCount[#, "sub", IgnoreCase -> True]&


(* ::Subsection:: *)
(*content rules*)


M2MD[style_, cell:_[_TextData|_String, ___], opt : OptionsPattern[]
] := MDElement["Text", BoxesToMDString[ cell, False, opt ] ]

M2MD[style_, cell:_[_?OutputFormQ, ___],  OptionsPattern[]
]:= MDElement["Output", BoxesToInputString @ cell ]

M2MD[style_, cell:_[BoxData @ FormBox[_, TraditionalForm], ___], OptionsPattern[]
] := MDElement["LaTeXBlock", BoxesToTeXString @ cell ];

M2MD[style_, cell:_[_BoxData, ___], opt : OptionsPattern[]
] := ToImageElement[cell, opt]

(*default behaviour for cell styles*)
M2MD[args___] := MDElement["Comment", ToString[ Head /@ {args}] ];

M2MD // Attributes = {HoldAllComplete};

InputFormQ = OutputFormQ = FreeQ @ Except[BoxData|TextData|List|RowBox|SuperscriptBox, _Symbol]


BoxesToMDString[cell_Cell, inlineCell_:True, opt : OptionsPattern[]]:= parseData @ First @ cell

BoxesToMDString[boxes_, inlineCell_:True, opt : OptionsPattern[]]:= parseData[boxes]




(* ::Section::Closed:: *)
(*ToImageElement*)


ToImageElement // Options = M2MD // Options

ToImageElement[box:Except[_Cell],  patt : OptionsPattern[] ]:=ToImageElement[ Cell[BoxData @ box, "Output"], patt]

ToImageElement[cell_,  patt : OptionsPattern[]]:=
  With[
  { overwriteQ = OptionValue["OverwriteImages"]
  , exportURL  = OptionValue["ImagesExportURL"]
  , fetchURL   = OptionValue["ImagesFetchURL"]
  }
, Module[{ baseName, exportDir, exportPath, fetchDir, fetchPath, res}

, baseName = ToImageName[False, cell, patt]

; exportDir = Switch[ exportURL
  , Automatic      , FileNameJoin[{Directory[], "img"}]
  , _String | _File, exportURL /. File -> Identity
  , None | _       , Return["", Module]
  ]  
; exportPath = FileNameJoin[{exportDir, baseName<>".png"}]

; fetchDir  = Switch[ fetchURL
  , Automatic             , exportDir
  , "Relative"            , FileNameTake @ exportDir  (*img/*)
  , _String | _URL | _File, fetchURL
  , _                     , Return["", Module]
  ]
; fetchPath = urlNameJoin[{fetchDir, baseName<>".png"}]

; If[
    overwriteQ && FileExistsQ[exportPath]
  , $MDMonitor["Skipping existing image:", baseName]
  ; Return[MDElement["Image", baseName, fetchPath], Module]
  ]


; If[ Not @ DirectoryQ @ exportDir, CreateDirectory[exportDir, CreateIntermediateDirectories->True]]

; res = Export[exportPath, cell]

; If[ res === $Failed
  , Return[ MDElement["Comment", "Failed to export image"], Module]
  ]

; MDElement["Image", baseName, fetchPath]
]]





urlNameJoin[list_List ? (MemberQ[_URL]) ] := URLBuild[list /. URL -> Identity]
urlNameJoin[list_List ] := FileNameJoin[ list /. File -> Identity]


ToImageName // Options = Options @ MDExport;

ToImageName[boxes_         , "ExpressionHash"]:= Hash[boxes, "Expression", "Base36String"]
ToImageName[cell_CellObject, "ExpressionHash"]:= Hash[First @ NotebookRead @ cell, "Expression", "Base36String"]

ToImageName[cellObj_:False , boxes_, OptionsPattern[]     ]:= ToImageName[cellObj, boxes, OptionValue["ImageNameFunction"]]

ToImageName[_, boxes_              , Automatic]:= ToImageName[boxes, "ExpressionHash"]
ToImageName[cell_CellObject, boxes_, Automatic]:= FirstCellTag @ cell // Replace[{} :> ToImageName[boxes, "ExpressionHash"] ]
ToImageName[cell_, boxes_          , foo_]     := foo[cell, boxes] // Replace[Except[_String] :> ToImageName[boxes, "ExpressionHash"] ]



FirstCellTag[cell_CellObject]:= FirstCellTag @ CurrentValue[EvaluationCell[], CellTags]
FirstCellTag[tag_String]:=tag;
FirstCellTag[{}]:={};
FirstCellTag[{tag_String, ___}]:=tag;





(* ::Section:: *)
(*Cell Contents To MD*)


parseData[ cell_Cell ]:= parseData @ First @ cell
parseData[TextData[boxes_]]:= parseData @ boxes
parseData[RowBox[row_]]:= parseData @ row;
parseData[list_List] := StringJoin[parseData /@ list];

parseData[string_String] := string;

(*all cells below are inline cells*)
parseData[ Cell[BoxData[boxes_?inlineImageQ], ___] ]:= ToImageElement[boxes]
inlineImageQ = Not @* FreeQ[GraphicsBox | Graphics3DBox | DynamicModuleBox ]


parseData[ Cell[BoxData[boxes_?InputFormQ], ___]]:= MDElement["CodeInline", BoxesToInputString @ boxes]

parseData[ Cell[BoxData[FormBox[boxes_, TraditionalForm]], ___] ]:= MDElement["LaTeXInline", BoxesToTeXString @ boxes]




parseData[ Cell[BoxData[boxes_], ___] ] := parseData @ boxes (*TODO: with box replacements*)


parseData[InterpretationBox[boxes_, ___] ]:= parseData @ boxes


(* ::Subsection:: *)
(*forms/styles*)


parseData[StyleBox[expr_, a___, FontWeight -> "Bold", b___]]   := MDElement["Bold", parseData @ StyleBox[expr, a, b]]
parseData[StyleBox[expr_, a___, FontSlant  -> "Italic", b___]] := MDElement["Italic", parseData @ StyleBox[expr, a, b]]
parseData[StyleBox[expr_, ___]]                               := MDElement["Text", parseData @ expr]


parseData[FormBox[boxes : Except[_TagBox], TraditionalForm, ___]] :=  MDElement["LaTeXInline", BoxesToTeXString@boxes]


(* ::Subsection:: *)
(*hyperlinks*)


parseData[ TemplateBox[{lbl_, {url_, tag_}, note_}, "HyperlinkDefault", ___]
] := MDElement["Hyperlink", lbl, url]

parseData[ TemplateBox[{lbl_, url_}, "HyperlinkURL", ___]
] := MDElement["Hyperlink", lbl, url]


(* Insert > Hyperlink interpretation, the problem is that the label is a plain string rather than a string
which underwent ToBoxes so I will assume it is 'ready'. I am not sure how many edge case we have here. *)
parseData[ ButtonBox[lbl_, ___, BaseStyle -> "Hyperlink", ButtonData -> { url_String | URL[url_], ___ }, ___]
] := MDElement["Hyperlink", lbl, url ]

parseData[ ButtonBox[lbl_, ___, ButtonData -> (s_String ? (StringStartsQ["paclet:"])), ___] ]:=
  MDElement["Hyperlink", lbl, "https://reference.wolfram.com/language/" <> StringTrim[s, "paclet:"]]

parseData[ TemplateBox[{lbl_, ref_}, "RefLink"|"RefLinkPlain"|"StringTypeLink", ___]
]:= MDElement["Hyperlink", lbl, "https://reference.wolfram.com/language/" <> StringTrim[ref, "paclet:"]]


(* ::Subsection::Closed:: *)
(*defaults*)


   (*default behaviour for boxes*)
parseData[boxes_] := ToImageElement[boxes];


(* ::Section:: *)
(*MDElement*)


(* ::Subsection:: *)
(*$MDElementTemplates*)


(* Functions used in templates should be package exported symbols *)


$MDElementTemplates = <|
    "LaTeXBlock" -> "$$``$$"
  , "LaTeXInline"-> "$``$"
  , "Image"      -> "![``](``)"
  , "Hyperlink"  -> "[``](``)"
  , "Text"       -> "``"
  , "Bold"       -> TemplateExpression @ SmartWrap[TemplateSlot[1], "**"]
  , "Italic"     -> TemplateExpression @ SmartWrap[TemplateSlot[1], "*"]
  
  , "ThematicBreak"-> "---"

  , "h1" -> "# <*WithLineBreaks @ #*>"
  , "h2" -> "## <*WithLineBreaks @ #*>"
  , "h3" -> "### <*WithLineBreaks @ #*>"
  , "h4" -> "#### <*WithLineBreaks @ #*>"
  , "h5" -> "##### <*WithLineBreaks @ #*>"
  , "h6" -> "###### <*WithLineBreaks @ #*>"

  , "Item"         -> "<*StringRepeat[\" \", 4 # ]*>- <*WithLineBreaks @ #2*>"
  , "NumberedItem" -> "<*StringRepeat[\" \", 4 # ]*>1. <*WithLineBreaks @ #2*>"
  , "Paragraph"    -> "<*StringRepeat[\" \", 4(#+1) ]*><*WithLineBreaks @ #2*>"


  , "Comment"   -> "[//]: # (``)"
  , "CodeBlock" -> TemplateExpression @ StringJoin["```", TemplateSlot[2], "\n", TemplateSlot[1], "\n```"]
  , "CodeInline"-> TemplateExpression @ StringJoin["`", TemplateSlot[1], "`"]
  , "Output"    -> TemplateExpression @ StringJoin["```\n(*", TemplateSlot[1], "*)\n```"]

|>;


SmartWrap[body_String, wrap_String]:= Module[{once=False}, StringReplace[
  body
, { StartOfString ~~ p:(" "...) :> p <> wrap
  , p:(" "...) ~~ EndOfString   :> If[!once, once=True; wrap <>p, ""] (*because otherwise it matches twice  O_o *)
  }
  ]
]


(* ::Subsection:: *)
(*sugar*)


MDElement::unknownTag = "Unknown MDElement tag: ``.";

MDElement["Hyperlink", a___, b:Except[_String], c___] := MDElement["Hyperlink", a, parseData @ b, c]

MDElement["CodeBlock", code_]:=MDElement["CodeBlock", code, ""]

MDElement[args___]:= ( Message[MDElement::unknownTag, args]; "");





(* ::Subsection::Closed:: *)
(*loading*)


MDElementDefine[tag_String , lhs_String]:= MDElement[tag, args___]:= Block[
  { WithLineBreaks = StringReplace[#, "\n" -> "  \n"]& }
, StringTemplate[lhs][args]
]

MDElementDefine[tag_String , lhs_]      := MDElement[tag, args___]:= Block[
  { WithLineBreaks = StringReplace[#, "\n" -> "  \n"]& }
, TemplateApply[lhs, {args}]
]

MDElementLoad[defs_]:=LoadDefinitions[   defs, "DefinitionFunction" -> MDElementDefine ] 


MDElementLoad @ $MDElementTemplates


(* ::Section:: *)
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

  }
] 


(* ::Section:: *)
(*Utilities*)


(* ::Subsection:: *)
(*BoxToString*)


BoxesToTeXString[boxes_] := Check[
  Convert`TeX`BoxesToTeX[boxes],
  ToImageElement @ boxes
]





(*InputText is nice but has a fixed page width...*)

BoxesToInputString[ boxData_]:=  Module[
  {tagged, mark = "ORYGINALMARK", iNL = FromCharacterCode@{62371}}
  ,
  tagged = boxData /. (n : "\n" | iNL) :> mark <> n;
  tagged = First @ FrontEndExecute @  FrontEnd`ExportPacket[tagged, "InputText"];

  StringReplace[
   tagged,
   {
    (mark ~~ "\r"...~~"\n") :> "\n",
    "\\"~~"\r"...~~"\n" -> "", (* in strings *)
    ("\r"...) ~~ "\n" ~~ " " ... -> ""
    }
   ]
]




BoxesToString[ boxes:Except[_BoxData|_Cell], type_:"InputText"]:=BoxesToString[BoxData @ boxes, type]
BoxesToString[ boxes_, type_:"InputText"]:= First @ FrontEndExecute @ FrontEnd`ExportPacket[boxes, type]


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
