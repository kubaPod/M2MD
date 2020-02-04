(* ::Package:: *)

(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: user *)
(* :Date: 2020-01-20 *)


Needs @ "M2MD`";
AppendTo[$ContextPath, "M2MD`Private`"];


VerificationTest[
  ToImageName @ "test"
, "0bleddx8vw5yk"
, TestID -> "ToImageName basic"
]


VerificationTest[
 M2MD @ Cell[TextData[{
 "Use ",
 Cell[BoxData[  RowBox[{"Print", "[",   RowBox[{"1", "+", "1"}], "]"}]]], " to print stuff."}], "Text"]
, "Use `Print[1 + 1]` to print stuff."
, TestID -> "CodeInline"
]


VerificationTest[
  M2MD @ Cell[TextData[{
 "test ",
 Cell[BoxData[  FormBox[   RowBox[{"1", "+", "1"}], TraditionalForm]],  FormatType->"TraditionalForm"]
}], "Subsection"]
, "### test $1+1$"
, TestID -> "LaTeXInline"
]


VerificationTest[
  Block[{ToImageElement, MDElement,StringJoin},
ToString @ M2MD @ Cell[TextData[{
 "test ",
 Cell[BoxData[
  FormBox[
   GraphicsBox[DiskBox[{0, 0}],
    ImageSize->20], TraditionalForm]],
  FormatType->"TraditionalForm"]
}], "Section"]

]
, "MDElement[h2, StringJoin[{test , ToImageElement[FormBox[GraphicsBox[DiskBox[{0, 0}], ImageSize -> 20], TraditionalForm]]}]]"
, TestID -> "Inline image"
]


VerificationTest[
  ToImageName @ RowBox[{}]
, "10tvi4mw3rg8l"
, TestID -> "ToImageName[boxes]"
]


VerificationTest[
  Block[{ToImageElement, MDElement,StringJoin},
ToString@M2MD@Cell[TextData[{
 Cell[BoxData[  GraphicsBox[{    Disk[{0,0}]    }   ]]  ],
 " IGraph/M"
}], "Title"]
]
, "MDElement[h1, StringJoin[{ToImageElement[GraphicsBox[{Disk[{0, 0}]}]],  IGraph/M}]]"
, TestID -> "Inline image in std form"
]


VerificationTest[
  $CellData= Cell[BoxData @ ToBoxes @ TraditionalForm[HypergeometricPFQ[{Subscript[a, 1],Subscript[a, 2]},{Subscript[b, 1],Subscript[b, 2]},x]], "Output"];

  ToString[DisplayForm@$CellData,TeXForm]===
  ToString[RawBoxes@$CellData,TeXForm]===
  ToString[TeXForm@RawBoxes@$CellData]===
  ToString[TeXForm@RawBoxes@$CellData[[1]]]=== (*boxdata stripped*)
  ToString[TeXForm@RawBoxes@$CellData[[1,1]]]===
  ToString[TeXForm@RawBoxes@$CellData[[1,1,1,1]]]===
  Convert`TeX`BoxesToTeX @ $CellData
  
, True
, TestID -> "BoxesToTeX"
]




VerificationTest[
  M2MD @ Cell[TextData @ {"Inline code: ", Cell[BoxData[{"1+1"}]]}, "Title"]
, "# Inline code: `1 + 1`"
, TestID -> "Inline code"
]



VerificationTest[  M2MD @ "string", "string", TestID -> "String"]


VerificationTest[
  M2MD @ Cell[BoxData["\"E:\\\\Idea Projects\\\\M2MD\""],"Output",CellLabel->"Out[6]="]
, "```\n(*\"E:\\\\Idea Projects\\\\M2MD\"*)\n```"
, TestID -> "Simple output"
]


VerificationTest[  M2MD @ Cell["asdasd", "Text"], "asdasd", TestID -> "simple text"]
VerificationTest[  M2MD @ Cell[TextData@"asdasd", "Text"], "asdasd", TestID -> "simple text data"]
VerificationTest[  M2MD @ Cell[TextData@"asdasd", "Whatever"], "asdasd", TestID -> "unknown text style"]


VerificationTest[
  parseData@ButtonBox["the C/igraph documentation",
  BaseStyle->"Hyperlink",
  ButtonData->{
    URL["http://igraph.org/c/doc/"], None},
  ButtonNote->"http://igraph.org/c/doc/"]
, "[the C/igraph documentation](http://igraph.org/c/doc/)"
, TestID -> "parseData@ButtonBox[\"the C/igraph documentation\",BaseStyle->\"Hyp..."
]


VerificationTest[
  M2MD @ Cell["test","Output",CellLabel->"Out[79]//InputForm="]
, "```mathematica\ntest\n```"
, TestID -> "InputForm output"
]


VerificationTest[
  M2MD@Cell[TextData[ButtonBox["National Renewable Energy Laboratory (NREL) data",
 BaseStyle->"Hyperlink",
 ButtonData->{
   URL["www.wolfram.com"], None},
 ButtonNote->"www.wolfram.com"]], "Subsection"]
, "### [National Renewable Energy Laboratory (NREL) data](www.wolfram.com)"
, TestID -> "Subsection>Hyperlink"
]


(* ::Section:: *)
(*CodeBlocks*)


VerificationTest[
  M2MD@Cell["asd", "Code"]
, "```mathematica\nasd\n```"
, TestID -> "Code Language"
]


VerificationTest[
  M2MD[ Cell["asd", "Code"], "CodeLanguage" -> "whatever"]
, "```whatever\nasd\n```"
, TestID -> "Custom Language"
]


VerificationTest[
  M2MD[ Cell["asd", "Program"]]
, "```\nasd\n```"
, TestID -> "ProgramLanguage"
]


VerificationTest[
  M2MD@Cell["asd", "ExternalLanguage", CellEvaluationLanguage->"Julia"]
, "```julia\nasd\n```"
, TestID -> "Julia language"
]


VerificationTest[
  M2MD@Cell["asd", "ExternalLanguage", CellEvaluationLanguage->"NodeJS"]
, "```javascript\nasd\n```"
, TestID -> "NodeJS language"
]


VerificationTest[
  M2MD @ Cell["1+\n2", "Program"]
, "```\n1 +\n 2\n```"
, TestID -> "Program cell"
]


VerificationTest[
  M2MD @ Cell[BoxData[ RowBox[{"<<", "M2MD`"}]], "Input"]
, "```mathematica\n<< M2MD`\n```"
, TestID -> "InputBlock"
]


VerificationTest[
  M2MD @ Cell[BoxData[RowBox[{"M2MD"," ","@"}]],"Code", CellLabel->"TEST"]
, "```mathematica\nM2MD @\n```"
, TestID -> "Cell to InputCode"
]


$inputCell = Cell[BoxData[RowBox[{"foo","[","\[IndentingNewLine]",RowBox[{"bar","[","\[IndentingNewLine]",RowBox[{"1",",","2"}],"\[IndentingNewLine]","]"}],"\[IndentingNewLine]","]"}]],"Input"];

VerificationTest[
  M2MD[$inputCell, "BoxesToStringType" -> "InputText"]
, "```mathematica\nfoo[\n bar[\n  1, 2\n  ]\n ]\n```"
, TestID -> "BoxesToStringType"
]


(* ::Section:: *)
(*Items*)


VerificationTest[  M2MD @ Cell["Test", "Item"] , "- Test", TestID -> "Item"]
VerificationTest[  M2MD @ Cell["Test", "Subitem"] , "    - Test", TestID -> "SubItem"]
VerificationTest[  M2MD @ Cell["Test\nTest", "Subsubitem"] , "        - Test  \nTest", TestID -> "Subsubitem linebreaks" ]


VerificationTest[  M2MD @ Cell["Test", "ItemParagraph"], "    Test", TestID -> "ItemParagraph"]
VerificationTest[  M2MD @ Cell["Test", "SubitemParagraph"], "        Test", TestID -> "SubitemParagraph"]
VerificationTest[  M2MD @ Cell["Test\nTest", "SubsubitemParagraph"], "            Test  \nTest", TestID -> "SubsubitemParagraph"]


VerificationTest[  M2MD @ Cell["Test", "ItemNumbered"], "1. Test", TestID -> "ItemNumbered"]
VerificationTest[  M2MD @ Cell["Test", "SubitemNumbered"], "    1. Test", TestID -> "SubitemNumbered"]
VerificationTest[  M2MD @ Cell["Test\nTest", "SubsubitemNumbered"], "        1. Test  \nTest", TestID -> "SubsubitemNumbered"]


(* ::Section:: *)
(*Inline cells*)


VerificationTest[
  M2MD[
  Cell[
  TextData[{
    "asdasd ",
    StyleBox["adsd",FontWeight->"Bold"],
    
    Cell[BoxData[RowBox[{"1","*","1"}]]]
  }]
,"Text"
],
 "ImagesExportURL" -> None ]
, "asdasd **adsd**`1*1`"
, TestID -> "Inline cells"
]


(* ::Section:: *)
(*StyleRules*)


(* ::Section:: *)
(*end*)


$ContextPath = DeleteCases["M2MD`Private`"] @ $ContextPath;
