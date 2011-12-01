(* Mathematica Test File *)



(* ::Section:: *)
(* Interval *)

d = Line[{.1 + 3.2 I, -.2 - .1 I}];
n = 300; hf = IFun[Cos, d, n];
hf2 = IFun[Exp[-#^2] /. Underflow[] -> 0 &, d, n];
x = MapFromInterval[hf, 0.1];


(* ::Subsection:: *)
(* Basic Operations *)


Test[
	(hf + hf2)[x] - (Cos[x] + Exp[-x^2])
	,
	0
	,
	TestID->"Fun-Addition"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(hf hf2)[x] - (Cos[x] Exp[-x^2])//Chop
	,
	0
	,
	TestID->"Fun-Multiplication"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Sin[hf][x] - Sin[Cos[x]]
	,
	0
	,
	TestID->"Fun-Sin"
	,
	EquivalenceFunction -> NEqual
]


Test[
	ToArrayFun[hf]
	,
	hf
	,
	TestID->"Fun-ToArrayFun"
	,
	EquivalenceFunction -> NEqual
]


(* ::Subsection:: *)
(* values of derivatives and integrals *)

Test[
	hf[x] - Cos[x]
	,
	0
	,
	TestID->"Fun-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf'[x] - Cos'[x]//Chop
	,
	0
	,
	TestID->"Fun-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf''[x] - Cos''[x]//Chop
	,
	0
	,
	TestID->"Fun-SecondDerivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[(hf // ReduceDimensionIntegrate // Derivative) - hf//Values//Abs//Max,10^(-9)]
	,
	0
	,
	TestID->"Fun-IntegrateThenDerivative"
	,
	EquivalenceFunction -> NEqual
]

(* ::Subsection:: *)
(* Adaptivity *)

Test[
	Chop[IFun[Exp, Line[{0, 1}], InterpolationPrecision -> 10.^(-5)][0.2] -
  Exp[0.2],10^(-4)]
	,
	0
	,
	TestID->"Fun-Adaptive"
	,
	EquivalenceFunction -> NEqual
]

Test[
	IFun[Sech, Line[{0, \[Infinity]}]][0.4] - Sech[0.4]
	,
	0
	,
	TestID->"Fun-HalfLineValue"
	,
	EquivalenceFunction -> NEqual
]


(* ::Section:: *)
(* Half Line *)

n = 100; hf = 
 IFun[Sech, Line[{0, \[Infinity]}, Stretch -> 1], n];
hf2 = IFun[Exp[-#^2] &, Line[{0, \[Infinity]}, Stretch -> 1], n];


(* ::Subsection:: *)
(* Basic Operations *)

Test[
	(hf + hf2)[2.] - (Sech[2.] + Exp[-2.^2])
	,
	0
	,
	TestID->"Fun-Ray-Addition"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(hf hf2)[2.] - (Sech[2.] Exp[-2.^2])
	,
	0
	,
	TestID->"Fun-Ray-Multiplication"
	,
	EquivalenceFunction -> NEqual
]


Test[
	Sin[hf][2.] - Sin[Sech[2.]]
	,
	0
	,
	TestID->"Fun-Ray-Sine"
	,
	EquivalenceFunction -> NEqual
]


(* ::Subsection:: *)
(* Derivative and Integrals *)


Test[
	hf[3.] - Sech[3.]
	,
	0
	,
	TestID->"Fun-Ray-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf'[3.] - Sech'[3.]//Chop
	,
	0
	,
	TestID->"Fun-Ray-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf''[3.] - Sech''[3.] // Chop
	,
	0
	,
	TestID->"Fun-Ray-SecondDerivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Integrate[hf][3.] + 2 ArcTan[(1 - E^3)/(1 + E^3)]
	,
	0
	,
	TestID->"Fun-Ray-Integral"
	,
	EquivalenceFunction -> NEqual
]


(* ::Subsection:: *)
(* Stretch *)

hf3 = IFun[Sech, Line[{0, \[Infinity]}, Stretch -> 1.1], n];

Test[
	hf[3.] - hf3[3.]
	,
	0
	,
	TestID->"Fun-StretchRay-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf'[3.] - hf3'[3.]//Chop
	,
	0
	,
	TestID->"Fun-StretchRay-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Integrate[hf][3.] - Integrate[hf3][3.]
	,
	0
	,
	TestID->"Fun-StretchRay-Integrate"
	,
	EquivalenceFunction -> NEqual
]

(* ::Subsection:: *)
(* Complex Ray *)

d = Line[{.1 + 3.2 I, Exp[I 0.1] \[Infinity]}, Stretch -> 1.42];
n = 500; hf = IFun[Sech, d, n];
hf2 = IFun[Exp[-#^2] /. Underflow[] -> 0 &, d, n];
x = .1 + 3.2 I + 2. Exp[I 0.1];


Test[
	((hf + hf2)[x] - (Sech[x] + Exp[-x^2]))/n
	,
	0
	,
	TestID->"Fun-ComplexRay-Addition"
	,
	EquivalenceFunction -> NEqual
]

Test[
	((hf hf2)[x] - (Sech[x] Exp[-x^2]) )/n
	,
	0
	,
	TestID->"Fun-ComplexRay-Multiplication"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Sin[hf][x] - Sin[Sech[x]]
	,
	0
	,
	TestID->"Fun-ComplexRay-Sine"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf[x] - Sech[x]
	,
	0
	,
	TestID->"Fun-ComplexRay-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf'[x] - Sech'[x]// Chop
	,
	0
	,
	TestID->"Fun-ComplexRay-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf''[x] - Sech''[x]// Chop
	,
	0
	,
	TestID->"Fun-ComplexRay-SecondDerivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(hf // ReduceDimensionIntegrate // Derivative) - hf//Values//Abs//Max // Chop
	,
	0
	,
	TestID->"Fun-ComplexRay-IntegrateThenDerivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Integrate[hf][x] - 
 NIntegrate[
  Sech[MapFromInterval[hf, z]] MapFromIntervalD[hf, z], {z, -1, 
   MapToInterval[hf, x]}]
	,
	0
	,
	TestID->"Fun-ComplexRay-Integrate"
	,
	EquivalenceFunction -> NEqual
]


(* ::Section:: *)
(* Vector Interval *)


d = Line[{.1 + 3.2 I, -.2 - .1 I}];
f = {Cos[#], AiryAi[#]} &;
n = 300; hf = IFun[f, d, n];
f2 = {Exp[-#^2], BesselJ[0, #]} &;
hf2 = IFun[f2, d, n];
x = MapFromInterval[hf, 0.1];

Test[
	(hf + hf2)[x] - (f[x] + f2[x])//Norm
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-Addition"
	,
	EquivalenceFunction -> NEqual
]

Test[
	((hf hf2)[x] - (f[x] f2[x])//Norm)/n
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-Multiplication"
	,
	EquivalenceFunction -> NEqual
]


Test[
	Sin[hf][x] - Sin[f[x]]//Norm
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-Sine"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf[x] - f[x]//Norm
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(Derivative[1] /@ (hf // ToArrayOfFuns) // ToArrayFun)[x] - f'[x]//Norm//Chop
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-ToArrayOfFuns-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf'[x] - f'[x]//Norm//Chop
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf''[x] - f''[x]//Norm//Chop
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-SecondDerivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(hf // Integrate) - ToArrayFun[Integrate /@ (hf // ToArrayOfFuns)]//Values//Abs//Flatten//Max
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-IntegrateVsIndividual"
	,
	EquivalenceFunction -> NEqual
]

Test[
	((hf // ReduceDimensionIntegrate // Derivative) - hf//Values//Abs//Flatten//Max)/n//Chop
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-IntegrateDerivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Integrate[hf][x] - 
 NIntegrate[
  f[MapFromInterval[hf, z]] MapFromIntervalD[hf, z], {z, -1, 
   MapToInterval[hf, x]}]//Norm
	,
	0
	,
	TestID->"Fun-Vector-ComplexLine-Integrate"
	,
	EquivalenceFunction -> NEqual
]



(* ::Section:: *)
(* Matrix Interval *)

d = Line[{.1 + 3.2 I, -.2 - .1 I}];
f = ( {
     {Cos[#], AiryAi[#]},
     {Exp[#], AiryBi[#]}
    } ) &;
n = 300; hf = IFun[f, d, n];
f2 = ( {
     {Exp[-#^2], BesselJ[0, #]},
     {Cos[#], Sin[#]}
    } ) &;
hf2 = IFun[f2, d, n];
x = MapFromInterval[hf, 0.1];


Test[
	(hf + hf2)[x] - (f[x] + f2[x])//Norm
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-Addition"
	,
	EquivalenceFunction -> NEqual
]

Test[
	((hf hf2)[x] - (f[x] f2[x])//Norm)/n
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-Multiplication"
	,
	EquivalenceFunction -> NEqual
]


Test[
	Sin[hf][x] - Sin[f[x]]//Norm
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-Sine"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf[x] - f[x]//Norm
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(MatrixMap[Derivative[1], (hf // ToArrayOfFuns)] // ToArrayFun)[x] - 
 f'[x]//Norm//Chop
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-ToArrayOfFuns-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf'[x] - f'[x]//Norm//Chop
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	hf''[x] - f''[x]//Norm//Chop
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-SecondDerivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(hf // Integrate) - ToArrayFun[MatrixMap[Integrate, (hf // ToArrayOfFuns)]]//Values//Abs//Flatten//Max
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-IntegrateVsIndividual"
	,
	EquivalenceFunction -> NEqual
]

Test[
	((hf // ReduceDimensionIntegrate // Derivative) - hf//Values//Abs//Flatten//Max)/n//Chop
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-IntegrateDerivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Integrate[hf][x] - 
 NIntegrate[
  f[MapFromInterval[hf, z]] MapFromIntervalD[hf, z], {z, -1, 
   MapToInterval[hf, x]}]//Norm
	,
	0
	,
	TestID->"Fun-Matrix-ComplexLine-Integrate"
	,
	EquivalenceFunction -> NEqual
]


(* ::Section:: *)
(* Unit Circle *)

f = Exp;
lf = LFun[f, Circle[0, 1.], 200];
f2 = Sech[#] Sin[1/#] &;
lf2 = LFun[f2, Circle[0, 1.], 200];
x = Exp[I 0.1];

(* ::Subsection:: *)
(* Basic Operations *)

Test[
	(lf + lf2)[x] - (f[x] + f2[x])
	,
	0
	,
	TestID->"LFun-Addition"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(lf lf2)[x] - (f[x] f2[x])
	,
	0
	,
	TestID->"LFun-Multiplication"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Sin[lf][x] - Sin[f[x]]
	,
	0
	,
	TestID->"LFun-Sin"
	,
	EquivalenceFunction -> NEqual
]

Test[
	ToArrayFun[lf]
	,
	lf
	,
	TestID->"LFun-ArrayFun"
	,
	EquivalenceFunction -> NEqual
]

(* ::Subsection:: *)
(* values of derivatives and integrals *)

Test[
	lf[x] - f[x]
	,
	0
	,
	TestID->"LFun-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	lf2'[x] - f2'[x]//Chop
	,
	0
	,
	TestID->"LFun-Derivative"
	,
	EquivalenceFunction -> NEqual
]

Test[
	lf2''[x] - f2''[x]//Chop
	,
	0
	,
	TestID->"LFun-SecondDerivative"
	,
	EquivalenceFunction -> NEqual
]



(* ::Section:: *)
(* Real Line *)

f = Sech;
rf = LFun[f, RealLine];
x= 0.3;


Test[
	rf[x] - f[x]
	,
	0
	,
	TestID->"LFun-RealLine-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	rf'[x] - f'[x]//Chop
	,
	0
	,
	TestID->"LFun-RealLine-Derivative"
	,
	EquivalenceFunction -> NEqual
]



(* ::Section:: *)
(* Matrix *)


lf = LFun[{{Exp[#],Sin[#]},{AiryAi[#],BesselJ[0,#]}}&, Circle[0, 1.],40];
lf2 = LFun[{{Exp[#],#},{Exp[#],Cos[#]}}&, Circle[0, 1.],40];


Test[
	Values[lf.lf2] - (#[[1]].#[[2]]&/@Thread[{Values[lf],Values[lf2]}])//Flatten//Norm
	,
	0
	,
	TestID->"LFun-Matrix-Dot"
	,
	EquivalenceFunction -> NEqual
]



Test[
	NegativePart[lf] + NonNegativePart[lf] - lf // Norm
	,
	0
	,
	TestID->"LFun-Matrix-NegativePlusNonNegative"
	,
	EquivalenceFunction -> NEqual
]




Test[
	Cauchy[1, lf] - Cauchy[-1, lf] - lf // Norm
	,
	0
	,
	TestID->"LFun-Matrix-CauchyDifference"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Norm/@(Cauchy[1, {lf,lf2}] - Cauchy[-1, {lf,lf2}] - {lf,lf2}) //Norm
	,
	0
	,
	TestID->"LFun-Matrix-List-CauchyDifference"
	,
	EquivalenceFunction -> NEqual
]




(* ::Section:: *)
(* Arc *)


f = Sech;
af = IFun[f, Arc[0., 1., {-0.1, 0.1}]];
x= Exp[I 0.001];


Test[
	af[1.] - f[1.]
	,
	0
	,
	TestID->"Arc-Value-1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	af[x] - f[x]
	,
	0
	,
	TestID->"Arc-Value"
	,
	EquivalenceFunction -> NEqual
]

Test[
	af'[x] - f'[x]//Chop
	,
	0
	,
	TestID->"Arc-Derivative"
	,
	EquivalenceFunction -> NEqual
]


(* ::Section:: *)
(* Value List *)





(* ::Subsection:: *)
(* Scalar Tests *)

l = {IFun[Sech, Line[{-\[Infinity], -1.}]], 
   IFun[Sech, Line[{-1., 1.}]], IFun[Sech, Line[{1., \[Infinity]}]]};


Test[
	(l // ToValueList) - (Flatten[Values /@ l] // Rest // Most) // Norm
	,
	0
	,
	TestID->"ValueList-ScalarList-ToValueListDefinition"
	,
	EquivalenceFunction -> NEqual
]

Test[
	FromValueList[l[[1]], l[[1]] // ToValueList] - l[[1]]//Values // Norm
	,
	0
	,
	TestID->"ValueList-Scalar-ToFromValueList"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Values/@(FromValueList[l, l // ToValueList] - l) //Flatten// Norm
	,
	0
	,
	TestID->"ValueList-ScalarList-ToFromValueList"
	,
	EquivalenceFunction -> NEqual
]


(* ::Subsection:: *)
(* Vector Tests *)

Clear[f,l];

f = {Sech[#], Exp[-#^2]} &;
l = {IFun[f, Line[{-\[Infinity], -1.}]], IFun[f, Line[{-1., 1.}]], 
   IFun[f, Line[{1., \[Infinity]}]]};


Test[
	FromValueList[l[[1]], l[[1]] // ToValueList] - l[[1]]//Values // Norm
	,
	0
	,
	TestID->"ValueList-Vector-ToFromValueList"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Values/@(FromValueList[l, l // ToValueList] - l) //Flatten// Norm
	,
	0
	,
	TestID->"ValueList-VectorList-ToFromValueList"
	,
	EquivalenceFunction -> NEqual
]

(* ::Subsection:: *)
(* Matrix Tests *)

Clear[f,l,lV,lf];

f[z_] := ( {
    {Sech[z], Exp[-z^2]},
    {z Sech[z], z^2 Exp[-z^2]}
   } );
f[_?InfinityQ] := ( {
    {0, 0},
    {0, 0}
   } );
l = {IFun[f, Line[{-\[Infinity], -1.}]], IFun[f, Line[{-1., 1.}]], IFun[f, Line[{1., \[Infinity]}]]};
lV = #[[All, 1]] & /@ l;

lf= LFun[{{Sech[#],Exp[-#^2]},{Exp[-#^2],Sech[#]}}&,RealLine,100];


Test[
	FromValueList[l[[1]], l[[1]] // ToValueList] - l[[1]]//Values//Flatten // Norm
	,
	0
	,
	TestID->"ValueList-Matrix-ToFromValueList"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Values/@(FromValueList[l, l // ToValueList] - l) //Flatten// Norm
	,
	0
	,
	TestID->"ValueList-MatrixList-ToFromValueList"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Length[Normal[MatrixMultVectorFun[l]]]
	,
	Length[ToValueList[lV]]
	,
	TestID->"ValueList-MatrixList-MatrixMultVectorFun-Dimensions"
]


Test[
	(FromValueList[lV, MatrixMultVectorFun[l].ToValueList[lV]] - (#[[1]].#[[2]] & /@ Thread[{l, lV}]))//ToValueList//Flatten// Norm
	,
	0
	,
	TestID->"ValueList-MatrixList-MatrixMultVectorFun"
	,
	EquivalenceFunction -> NEqual
]


Test[
	FromValueList[lf, lf//ToValueList]
	,
	lf
	,
	TestID->"ValueList-MatrixList-LFun"
	,
	EquivalenceFunction -> NEqual
]

Test[
	FromValueList[{lf}, {lf}//ToValueList]
	,
	{lf}
	,
	TestID->"ValueList-MatrixList-LFun-Set"
		,
	EquivalenceFunction -> NEqual
]

Test[
	FromValueList[{lf[[1]]}, {lf[[1]]}//ToValueList]
	,
	{lf[[1]]}
	,
	TestID->"ValueList-VectorList-LFun-Set"
	,
	EquivalenceFunction -> NEqual
]

Test[
	FromValueList[lf[[1]], lf[[1]]//ToValueList]
	,
	lf[[1]]
	,
	TestID->"ValueList-VectorList-LFun"
	,
	EquivalenceFunction -> NEqual
]



(* ::Section:: *)
(* Mixing IFun's and LFun's *)

fl = {Fun[{Exp[#], Sin[#]} &, UnitInterval], 
   Fun[{Exp[#], Sin[#]} &, Circle[1. I, 0.5]]};
   
Test[
	Head[fl[[1]]]
	,
	IFun
	,
	TestID->"ValueList-Mixed-Head1"
]

Test[
	Head[fl[[2]]]
	,
	LFun
	,
	TestID->"ValueList-Mixed-Head1"
]

Test[
	FromValueList[fl[[1]],fl[[1]]//ToValueList]
	,
	fl[[1]]
	,
	TestID->"ValueList-Mixed-From-To-1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	FromValueList[fl[[2]],fl[[2]]//ToValueList]
	,
	fl[[2]]
	,
	TestID->"ValueList-Mixed-From-To-2"
	,
	EquivalenceFunction -> NEqual
]

Test[
	FromValueList[{fl[[2]]},{fl[[2]]}//ToValueList]
	,
	{fl[[2]]}
	,
	TestID->"ValueList-Mixed-From-To-2-List"
	,
	EquivalenceFunction -> NEqual
]


Test[
	FromValueList[fl,fl//ToValueList]
	,
	fl
	,
	TestID->"ValueList-Mixed-From-To"
	,
	EquivalenceFunction -> NEqual
]


(* ::Section:: *)
(* Misc *)


Test[
		(ZeroAtInfinityFun[0 &, Line[{0, \[Infinity] Exp[I \[Pi]/6]}], 10] // 
    FiniteTransformMatrix // Dimensions)
    ,
     {10, 9}
     ,
     TestID->"FiniteTransformMatrix"
]
    
    