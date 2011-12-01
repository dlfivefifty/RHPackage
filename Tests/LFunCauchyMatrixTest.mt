(* Mathematica Test File *)


(* ::Section:: *)
(* Unit Circle *)


lf = LFun[Exp[# + 1/#] &, UnitCircle]; 

Test[
	ToList[FFT[lf]] 
	,
	TransformMatrix[lf].Values[lf]
	,
	TestID->"LFun-TransformMatrix-Is-FFT"
	,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyBasis[lf // Domain, Span @@ (lf // FFT // IndexRange), 
   0.1].ToList[FFT[lf]] 
	,
	Cauchy[lf, 0.1]
	,
	TestID->"LFun-CauchyBasis-Is-Cauchy"
	,
	EquivalenceFunction -> NEqual
]

Test[
	MapDot[CauchyBasisS[+1, UnitCircle, #, 0.1] &, FFT[lf]]
	,
	Cauchy[lf, 0.1]
	,
	TestID->"LFun-CauchyBasisS-Is-Cauchy"
	,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyBasis[lf // Domain, Span @@ (lf // FFT // IndexRange), 
   0.].ToList[FFT[lf]] 
	,
	Cauchy[lf, 0.]
	,
	TestID->"LFun-CauchyBasis-Is-Cauchy-0"
	,
	EquivalenceFunction -> NEqual
]

Test[
	MapDot[CauchyBasisS[+1, UnitCircle, #, 0.] &, FFT[lf]]
	,
	Cauchy[lf, 0.]
	,
	TestID->"LFun-CauchyBasisS-Is-Cauchy-0"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Transpose[
  CauchyBasisS[-1, UnitCircle, Span @@ (lf // FFT // IndexRange), 
   zl = {1.1, 2., 3., 4.}]].ToList[lf // FFT]
	,
	Cauchy[lf, zl]
	,
	TestID->"LFun-CauchyBasisS-Is-Cauchy-Listable"
	,
	EquivalenceFunction -> NEqual
]

Test[
LFun[Transpose[
   Values /@ 
    FPCauchyBasis[-1, lf // Domain, Span @@ (lf // FFT // IndexRange),
      lf]].ToList[lf // FFT], UnitCircle]
	,
	Cauchy[-1,lf]
	,
	TestID->"LFun-FPCauchyBasis-Is-Cauchy-On-Circle"
	,
	EquivalenceFunction -> NEqual
]

Test[
	lf // Domain // RightEndpointInfinityQ
	,
	False
	,
	TestID->"LFun-Domain-NotInfinitye"
]

Test[
	DomainMemberQ[lf, 1.]
	,
	True
	,
	TestID->"LFun-1-In-Domain"
]

Test[
	DomainMemberQ[lf, .1]
	,
	False
	,
	TestID->"LFun-.1-NotIn-Domain"
]

Test[
	Norm[(CauchyMatrix[1, lf, lf] - CauchyMatrix[-1, lf, lf]) - 
  IdentityMatrix[lf // Length]]
	,
	0
	,
	TestID->"CauchyMatrix-Subtract-Is-Id"
	,
	EquivalenceFunction -> NEqual
]

Test[
	lf // Abs // Max
	,
	7.313622073856341`
	,
	TestID->"LFun-Max"
	,
	EquivalenceFunction -> NEqual
]


(* ::Section:: *)
(* Real Line *)


lf=LFun[Sech, RealLine]; 

Test[
	lf // Domain // CircleDomainQ
	,
	True
	,
	TestID->"LFun-RealLine-Is-CircleDomainQ"
]

Test[
	ToList[FFT[lf]] 
	,
	TransformMatrix[lf].Values[lf]
	,
	TestID->"LFun-RealLine-TransformMatrix-Is-FFT"
	,
	EquivalenceFunction -> NEqual
]

Test[
	MapToCircle[lf, \[Infinity]]
	,
	-1
	,
	TestID->"LFun-RealLine-°-Mapped-To--1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyBasis[lf, Span @@ (lf // FFT // IndexRange), 10. I].ToList[
  FFT[lf]]
	,
	Cauchy[lf, 10. I]
	,
	TestID->"LFun-RealLine-CauchyBasis-Is-Cauchy"
	,
	EquivalenceFunction -> NEqual
]


Test[
	Norm[FiniteTransformMatrix[lf].FiniteValues[lf] - ToList[FFT[lf]]]
	,
	0
	,
	TestID->"LFun-RealLine-FiniteTransformMatrix"
	,
	EquivalenceFunction -> NEqual
]



Test[
	ZeroAtInfinityFun[CauchyMatrix[+1, lf, lf].FiniteValues[lf], lf // Domain]
	,
	Cauchy[+1, lf]
	,
	TestID->"LFun-RealLine-CauchyMatrixIsCauch"
	,
	EquivalenceFunction -> NEqual
]

 
(* ::Section:: *)
(* Curve *)

fd = LFun[Exp[1/#] &, UnitCircle];
lf = LFun[1/((#^2 + 100) (# - 1)) &, fd // Curve];
z//Clear;
t//Clear;


Test[
	(fd[ComplexMapToCircle[lf, 1.5]] - 1.5 // Norm)/1000000 
	,
	0
	,
	TestID->"LFun-Curve-ComplexMapToCircle"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Cauchy[lf, 1.5]
	,
	ComplexContourNIntegrate[(1/((z^2 + 100) (z - 1))) /(z - 1.5), {z, Exp[1/Exp[I t]]}, {t, -\[Pi], \[Pi]}]/(2 \[Pi] I)
	,
	TestID->"LFun-Curve-Cauchy"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Cauchy[-1, lf, E]
	,
	Cauchy[lf, E - 0.000000000001]
	,
	TestID->"LFun-Curve-Cauchy--1-equals-perts"
	,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyBasis[lf, Span @@ (lf // FFT // IndexRange), 1.5].ToList[FFT[lf]]
  ,
	Cauchy[lf, 1.5]
	,
	TestID->"LFun-Curve-Cauchy"
	,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyBasis[-1,lf, Span @@ (lf // FFT // IndexRange), E].ToList[FFT[lf]]
  ,
	Cauchy[-1,lf, E]
	,
	TestID->"LFun-Curve-Cauchy-E"
	,
	EquivalenceFunction -> NEqual
]


Test[
	And @@ (DomainMemberQ[lf // Domain, #] & /@ (lf // Points))
  ,
	True
	,
	TestID->"LFun-Curve-DomainMemberQ"
]



Test[
	Cauchy[1, lf, lf // Points] - Cauchy[-1, lf, lf // Points]
  ,
	Values[lf]
	,
	TestID->"LFun-Curve-Cauchy-Minus"
	,
	EquivalenceFunction -> NEqual
]

Test[
	 (CauchyMatrix[1, lf, lf]-CauchyMatrix[-1, lf, lf]-IdentityMatrix[lf//Length]//Norm)/1000
  ,
	0
	,
	TestID->"LFun-Curve-CauchyMatrix-Minus-Id"
	,
	EquivalenceFunction -> NEqual
]


Test[
	 Cauchy[-1, lf, lf // Points]
  ,
	(Cauchy[-1, lf] // Values)
	,
	TestID->"LFun-Curve-Cauchy-WholeFun"
	,
	EquivalenceFunction -> NEqual
]




(* ::Section:: *)
(* Mixed *)

fl = {Fun[{Exp[#], Sin[#]} &, UnitInterval], 
   Fun[{Exp[#], Sin[#]} &, Circle[1. I, 0.5]]};
   
   
Test[
	 Values[FromValueList[fl[[2]], 
   CauchyMatrix[+1, fl[[1]], fl[[2]]].ToValueList[fl[[1]]]]]
  ,
	Cauchy[fl[[1]], Points[fl[[2]]]]
	,
	TestID->"Mixed-CauchyMat-12"
	,
	EquivalenceFunction -> NEqual
]

Test[
	 Values[FromValueList[fl[[2]], 
   CauchyMatrix[+1, fl, fl[[2]]].ToValueList[fl]]]
  ,
	Cauchy[+1,fl, Points[fl[[2]]]]
	,
	TestID->"Mixed-CauchyMat-All2"
	,
	EquivalenceFunction -> NEqual
]

