(* ::Package:: *)

(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: user *)
(* :Date: 2020-01-20 *)


Needs @ "M2MD`";
Once @ AppendTo[$ContextPath, "M2MD`Private`"]


VerificationTest[
  ToImageName @ "test"
, "0bleddx8vw5yk"
, TestID -> "ToImageName basic"
]


VerificationTest[
  M2MD @ Cell[TextData[{
 "Use ",
 Cell[BoxData[  RowBox[{"Print", "[",   RowBox[{"1", "+", "1"}], "]"}]]], " to print stuff."}], "Text"]
, "Use `Print[1+1]` to print stuff."
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
, "# Inline code: `1+1`"
, TestID -> "Inline code"
]



VerificationTest[  M2MD @ "string", "string", TestID -> "String"]


VerificationTest[
  M2MD @ Cell["1+\n2", "Program"]
, "```\n1+\n2\n```"
, TestID -> "Program cell"
]


VerificationTest[
  M2MD @ Cell[BoxData[ RowBox[{"<<", "M2MD`"}]], "Input"]
, "```\n<<M2MD`\n```"
, TestID -> "InputBlock"
]


VerificationTest[
  M2MD @ Cell[BoxData["\<\"E:\\\\Idea Projects\\\\M2MD\"\>"], "Output"]
, "```\n(*E:\\Idea Projects\\M2MD*)\n```"
, TestID -> "Simple output"
]


VerificationTest[  M2MD @ Cell["asdasd", "Text"], "asdasd", TestID -> "simple text"]
VerificationTest[  M2MD @ Cell[TextData@"asdasd", "Text"], "asdasd", TestID -> "simple text data"]
VerificationTest[  M2MD @ Cell[TextData@"asdasd", "Whatever"], "asdasd", TestID -> "unknown text style"]


M2MD @ Cell[BoxData[
 RowBox[{  RowBox[{"test", "=", " ",    GraphicsBox[DiskBox[{0, 0}]]}], ";"}]], "Code" ]


M2MD @ Cell[TextData[{
 "test ",
 Cell[BoxData[
  GraphicsBox[DiskBox[{0, 0}], ImageSize -> 20]]  ]
}], "Text"]


Import@"img\1wm4d46lfhvv2.png"


(* ::Text:: *)
(*Cell[TextData[{*)
(* "test ",*)
(* Cell[BoxData[*)
(*  GraphicsBox[DiskBox[{0, 0}], ImageSize -> 20]]  ]*)
(*}], "Text",*)
(* CellChangeTimes->{{3.789380678711265*^9, 3.789380685243788*^9}}]*)


(* ::Section::Closed:: *)
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


Block[{M2MD`Private`$MDEnvironment = True},
  Hold @ M2MD @ Cell["Test", "Item"] /. DownValues[M2MD] 
]


M2MD`Private`ItemStyleQ@"Item"


PrintDefinitions @ M2MD


(* ::Section:: *)
(*help*)


Internal`InheritedBlock[{$MDEnvironment = True, M2MD}, Attributes[M2MD]={};
  Nest[
    ReplaceAll[DownValues[M2MD]]
  , Hold @ M2MD @ Cell["1+\n2", "Program"]
  , 2
  ]
]


Internal`InheritedBlock[{$MDEnvironment = True, M2MD}, Attributes[M2MD]={};
  MatchQ[
    Hold @ M2MD["Program", Cell["1+\n2", "Program"] ],
    Verbatim[Hold] @ Verbatim[M2MD][ _?(Echo@*InputStyleQ@*Echo), cell_]
  ]
]
