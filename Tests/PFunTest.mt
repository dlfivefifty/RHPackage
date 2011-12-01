(* Mathematica Test File *)

pf = Fun[1.+I, Point[1 + I]];
if = Fun[Exp, UnitInterval];


Test[
	Cauchy[{if, pf}, 10.]
	,
	Cauchy[if, 10.]+Cauchy[pf,10.]
	,
	TestID->"PFun-Cauchy-List"
	,
	EquivalenceFunction -> NEqual
]

Test[
	FPCauchyBasis[+1, pf // Domain, 1, if] // Head
	,
	IFun
	,
	TestID->"PFun-CauchyBasis-Head"
]

Test[
	CauchyMatrix[+1, if, pf] // Dimensions
	,
	CauchyMatrix[+1, pf,if] // Dimensions//Reverse
	,
	TestID->"PFun-CauchyMatrix-Dimensions"
]

Test[
	CauchyMatrix[+1, pf, if].Values[pf]
	,
	Cauchy[pf,if//Points]
	,
	TestID->"PFun-CauchyMatrix-pf"
		,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyMatrix[+1, if, pf].Values[if]
	,
	Cauchy[if,pf//Points]
	,
	TestID->"PFun-CauchyMatrix-if"
		,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyMatrix[+1, pf]-CauchyMatrix[-1, pf]
	,
	{{1}}
	,
	TestID->"PFun-CauchyMatrix-Difference"
		,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyMatrix[+1, {pf,if}]-CauchyMatrix[-1, {pf,if}]
	,
	IdentityMatrix[1+Length[if]]
	,
	TestID->"PFun-CauchyMatrix-Difference-List"
		,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyMatrix[+1, pf].ToValueList[pf]
	,
{	Cauchy[+1,pf,pf//Domain//First]}
	,
	TestID->"PFun-CauchyMatrix-Cauchy+Difference"
		,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyMatrix[-1,pf].ToValueList[pf]
	,
	{Cauchy[-1,pf,pf//Domain//First]}
	,
	TestID->"PFun-CauchyMatrix-Cauchy-Difference"
		,
	EquivalenceFunction -> NEqual
]
