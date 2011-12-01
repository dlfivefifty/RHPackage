(* Mathematica Test File *)


if2 = IFun[Exp, Line[{1, 2}]];

Test[
	FromValueList[if2, 
    IntegrateMatrix[if2].Values[if2]] - (if2 // Integrate) // 
  Values // Norm
	,
	0
	,
	TestID->"SpectralMatrixTest-OtherInterval-IntegrateMatrix"
	,
	EquivalenceFunction -> NEqual
]