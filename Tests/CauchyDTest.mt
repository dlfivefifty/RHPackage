(* Mathematica Test File *)


Test[
	NZeroQ[CauchyBasisD[Line[{1, \[Infinity]}], 1, 0.]]
	,
	True
	,
	TestID->"CauchyBasisDInf"
]


Test[
	NZeroQ[CauchyBasisD[Line[{\[Infinity],1}], 1, 0.]]
	,
	True
	,
	TestID->"CauchyBasisDInfR"
]


Test[
	Chop[CauchyBasisD[Line[{1, \[Infinity]}], 2, 0.]
		-CauchyBasisD[Line[{1, \[Infinity]}], 2, 0.000001],10^-4]
	,
	0
	,
	TestID->"CauchyBasisDInf2"
]

f = Fun[Exp[-#] &, Line[{1, \[Infinity]}]];

Test[
	Chop[CauchyD[f, 0.]
		-CauchyD[f, 0.00001],10^-4]
	,
	0
	,
	TestID->"CauchyDInf"
]


Test[
	Chop[CauchyD[f, {0.,.1}]
		-CauchyD[f, {0.00001,.1}]//Norm,10^-4]
	,
	0
	,
	TestID->"CauchyDInfList"
]
