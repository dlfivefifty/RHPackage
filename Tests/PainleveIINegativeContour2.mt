(* Mathematica Test File *)

memBefore = MemoryInUse[];
maxMemory = 256*1024*1024;

Test[
	MemoryConstrained[
		PainleveII[{1, 2, 1/3}, -10,Contour->2],
		maxMemory
	]
	,
	0.20260521542364737` + 0.05802873462301145` I
	,
	TestID->"PainleveII-20101202-NegativeContour2-Complex"
	,
	EquivalenceFunction -> NEqual
]
	
Test[ 
	MemoryConstrained[
		PainleveII[{1+I, -2, 1-I}, -10,Contour->2],
		maxMemory
	]
	,
	3.1863205264886156` + 3.1710170721554516`*^-13 I
	,
	TestID->"PainleveII-20101202-NegativeContour2-Real"
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
	TestID->"PainleveII-20101202-NegativeContour2-ClearDatabase"
	,
	EquivalenceFunction -> Less 
]
