(* Mathematica Test File *)

maxMemory = 256*1024*1024;

Test[
	MemoryConstrained[
		PainleveII[{1, 2, 1/3}, -10,Contour->3],
		maxMemory
	]
	,
	0.20260521541996596` + 0.05802873462335968` I
	,
	TestID->"PainleveII-20101222-ScaledNegative-Complex"
	,
	EquivalenceFunction -> NEqual
]

Test[
	MemoryConstrained[
		PainleveII[{1, 2, 1/3}, -10,Contour->3]+PainleveII[-{1, 2, 1/3}, -10,Contour->3],
		maxMemory
	]
	,
	0
	,
	TestID->"PainleveII-20101222-ScaledNegative-Complex-Stokes"
	,
	EquivalenceFunction -> NEqual
]