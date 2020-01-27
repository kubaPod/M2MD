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
, "MDElement[h2, StringJoin[{test , ToImageElement[GraphicsBox[DiskBox[{0, 0}], ImageSize -> 20]]}]]"
, TestID -> "Inline image"
]
