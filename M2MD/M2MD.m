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

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


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
, "code",           codeIndent
];


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


parseCodeData[data_] := StringReplace[
  First[FrontEndExecute[FrontEnd`ExportPacket[data, "InputText"]]]
, "\n" -> "\n" <> codeIndent
];


textStyleQ = (StringCount[#, "title" | "section" | "text", IgnoreCase -> True] > 0) &;


itemStyleQ = (StringCount[#, "item", IgnoreCase -> True] >  0) &;


codeStyleQ = MemberQ[{"Code", "Input"}, #] &;


M2MD[nb_NotebookObject] :=  StringJoin@Flatten[M2MD /@ Cells[nb]];


M2MD[cellObj_CellObject] :=  M2MD[NotebookRead[cellObj], cellObj];


M2MD[cell_Cell, cellObj_CellObject] := M2MD[#2, #, cellObj] & @@ cell;


M2MD[style_?textStyleQ, data_, cellObj_CellObject] := {
  prefix[style]
, addPrefix[style] /@ Flatten@{parseData[data]}
, "\n\n"
};


addPrefix[style_][expr : Except[_String]] := expr;


addPrefix[style_][s_String] :=  StringReplace[s, "\n" -> "\n" <> prefix[style]];


M2MD[style_?itemStyleQ, data_, cellObj_CellObject] := {
  prefix["items"][cellObj, style]
, parseData@data
, "\n\n"
};


M2MD[style_?codeStyleQ, data_, cellObj_CellObject] := {
  "\n\n----------\n\n"
, codeIndent
, parseCodeData@data
, "\n\n"
};


M2MD["Output", BoxData[FormBox[boxes_, TraditionalForm]], cellObj_CellObject] := TemplateApply["$$``$$\n\n", {boxesToTeX@boxes} ];


parseData[list_List] := parseData /@ list;


parseData[string_String] := string;


parseData[data_ (BoxData | TextData)] := List @@ (parseData /@ data);


parseData[cell_Cell] :=  parseData@First@cell; (*inlince cells style skipped*)


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


    (*default behaviour for cell styles*)
M2MD[s_, ___] := TemplateApply["[//]: # (No rules defined for ``)\n\n", {s}];


boxesToTeX = ToString[ToExpression@#, TeXForm] &;


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
