(* ::Package:: *)

(* ::Title:: *)
(*M2MD Wiki*)


(* ::Text:: *)
(*Welcome to M2MD Wiki / Documentation*)


(* ::Text:: *)
(*I am not planning to make a proper Mathematica Documentation for this paclet, this should not come as a surprise to anyone who ever tried to do it.*)


(* ::Text:: *)
(*I am aware this documentation is minimal, I will try to improve it in future. *)


(* ::Text:: *)
(*The paclet comes with a palette. It is outdated and the only available button there will convert a selected notebook to a MD string. The updates are coming.*)


(* ::Title:: *)
(*Quick documentation*)


(* ::Text:: *)
(*There are two main functions in M2MD`: *)


(* ::Text:: *)
(*- `MDExport` which exports to a file and *)


(* ::Text:: *)
(*- `M2MD` which returns a Markdown string.*)


(* ::Text:: *)
(*They share the same options.*)


(* ::Section:: *)
(*MDExport*)


(* ::Subsection:: *)
(*Usage*)


MDExport[ fileName_String, source_, OptionsPattern[] ]


(* ::Text:: *)
(*source can be a `NotebookObject`, `CellObject` or a `Cell`*)


(* ::Subsection:: *)
(*Options*)


(* ::Subsubsection:: *)
(*`"IgnoredStyles"` - cells of which styles should be ignored *)


"IgnoredStyles" -> None | {__String}


M2MD[EvaluationNotebook[], "IgnoredStyles" -> {"Code", "Input", "Output"}]


(* ::Subsubsection:: *)
(*`"MDElementTemplates"`*)


(* ::Text:: *)
(*The converter first creates a symbolic representation of a markdown e.g.: `MDElement["h1", "a title"]` and this option allows us to overwrite existing rules by which it is translated to the final string.*)


"MDElementTemplates" -> _Association


M2MD[ EvaluationCell[] ]


M2MD[ EvaluationCell[], "MDElementTemplates" -> <|"CodeBlock" -> "**``**"|> ]


(* ::Text:: *)
(*Default tag/templates are:*)


<|
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
  , "Output"    -> TemplateExpression @ StringJoin["```\n(*", TemplateSlot[1], "*)\n```"]

|>


(* ::Subsection:: *)
(*Options - image export related*)


(* ::Text:: *)
(*Images are created for:*)
(*- `Graphics/Graphics3D` boxes*)
(*- `"Output"` cells that are not `TraditionalForm` or simple enough to export as input string*)
(*- `StandardForm` boxes that have no built in MD interpretation*)
(**)
(*MD images end up in as `![name][url]`*)


(* ::Subsubsection:: *)
(*`"ImageNameFunction"`*)


"ImagesExportURL" -> Automatic | _


(* ::Text:: *)
(*If an image is based on a `CellObject` then the `Automatic` setting means the name will be the first `CellTag`. If there's no tag then the name is based on the content's expression hash.*)
(*This way re-exporting won't create new files each time it is run.*)
(**)
(*If anything else than `Automatic` is provided then it will be applied to the subject of image export and it is expected to return a string that can be used as a name.*)


(* ::Subsubsection:: *)
(*`"ImagesExportURL"`*)


"ImagesExportURL" -> Automatic | None | path_String


(* ::Text:: *)
(*By default images are exported  to `./img`. *)
(*None means no image is created and a `path_` is expected directory.*)


(* ::Subsubsection:: *)
(*`"ImagesFetchURL"`*)


"ImagesFetchURL" -> "Relative" | Automatic | _String | _URL | _File


(* ::Text:: *)
(*This option specifies what gets to the URL part of MD element. *)
(**)
(*- "ImagesFetchURL" -> "Relative"` means the final `url` will be `FileNameJoin[{FileNameTake[exportDir], "name.png"}]`.*)
(**)
(*- `Automatic` means the absolute path to the exported file.*)
(**)
(*- `path : _String | _File` means the final  `url` will be `FileNameJoin[{ path, name<>extensions}]`.*)
(**)
(*- `_URL` is an important one because it acts the same way as above but the path will be created using `URLBuild` instead of `FileNameJoin`*)
(**)


(* ::Subsubsection:: *)
(*`"OverwriteImages"`*)


"OverwriteImages" -> True | False


(* ::Text:: *)
(*If the subject to export as image was not changed then a new image won't be created by default.*)


(* ::Subsection:: *)
(*Examples*)


"ImagesExportURL" -> Automatic


(* ::Text:: *)
(*\!\[image-76f31cc4-1dc6-4bc7-bbe8-0631e2681af6\]\(C:\Users\user\Documents\img\image-76f31cc4-1dc6-4bc7-bbe8-0631e2681af6.png\)*)


"ImagesExportURL" -> Automatic, "ImagesFetchURL" -> "Relative"


(* ::Text:: *)
(*\!\[image-303bcb72-bb88-430f-9ba1-62ee0d928d33\]\(img\image-303bcb72-bb88-430f-9ba1-62ee0d928d33.png\)*)


"ImagesExportURL" -> Automatic, "ImagesFetchURL" -> URL["Test"]


(* ::Text:: *)
(*\!\[image-822b8334-f5fd-43b2-8274-3a40165fe89c\]\(Test/image-822b8334-f5fd-43b2-8274-3a40165fe89c.png\)*)


SetOptions[PreviousCell[], CellTags -> {"my-name"}];
M2MD[PreviousCell[], "ImagesExportURL" -> $TemporaryDirectory, "ImagesFetchURL" -> URL@"online"]


(* ::Text:: *)
(*\!\[my-name\]\(online/my-name.png\)*)


(* ::Section:: *)
(*Possible issues*)


(* ::Text:: *)
(*- while repeatedly exported images should be exported to the same path keep in mind that no one will (yet) clean up those which were not overwritten.*)


SetDirectory@NotebookDirectory[];


MDExport["Home.md", EvaluationNotebook[]]


SystemOpen@%
