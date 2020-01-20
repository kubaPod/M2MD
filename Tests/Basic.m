(* ::Package:: *)

(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: user *)
(* :Date: 2020-01-20 *)


AppendTo[$ContextPath, "M2MD`Private`"]


VerificationTest[
  ToImageName @ "test"
, "0bleddx8vw5yk"
, TestID -> "ToImageName basic"
]


VerificationTest[
  ToImageName @ PreviousCell @ PreviousCell[]
, "06u9zuleucwll"
, TestID -> "ToImageName cell obj default"
]


SetOptions[PreviousCell@PreviousCell[], CellTags -> {"test-tag"}];
ToImageName @ PreviousCell @ PreviousCell[]
