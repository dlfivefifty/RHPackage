(* Mathematica Test File *)

memBefore = MemoryInUse[];

Test[
	PainleveII[{1, 2, 1/3}, -10]
	,
	0.20260521567114906` + 0.058028735267373156` I
	,
	TestID->"PainleveII-20101202-NegativeContour1-Complex"
	,
	EquivalenceFunction -> NEqual
]
	
Test[
	PainleveII[{1+I, -2, 1-I}, -10]
	,
	3.1863205322185744` + 1.2173731417435634`*^-9 I
	,
	TestID->"PainleveII-20101202-NegativeContour1-Real"
	,
	EquivalenceFunction -> NEqual
]

(* Test if Cached Results are cleared correctly. 
   As a memory increase of 0 is sort of hard to achieve 
   we are currently happy with a increase below 15%. *)
Test[
	Block[{},ClearPainleveDatabase[];
	memAfter = MemoryInUse[];
	(memAfter-memBefore)/memBefore//N]
	,
	0.15
	,
	TestID->"PainleveII-20101202-NegativeContour1-ClearDatabase"
	,
	EquivalenceFunction -> Less 
]
