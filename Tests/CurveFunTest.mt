(* Mathematica Test File *)

f = Fun[Exp[Exp[I (#)]] &, Line[\[Pi] Range[-1, 1, 1/4]]];
q[z_] := E^Re[z] Sin[Im[z]];
qv = IFun[q, Curve[# // ToUnitInterval]] & /@ f;

Test[
	Cauchy[+1, qv[[1]], Points[qv[[1]]][[2]]] - Cauchy[-1, qv[[1]], Points[qv[[1]]][[2]]]
	,
	Values[qv[[1]]][[2]]
	,
	TestID->"CurveFun-SinglePoint"
	,
	EquivalenceFunction -> NEqual
]

Test[
	And @@ (Function[qvv, 
    And @@ (DomainMemberQ[qvv, #] & /@ Points[qvv])] /@ qv)
	,
	True
	,
	TestID->"CurveFun-SinglePointDomain"
]


lf = Fun[q[#]/(I #) &, UnitCircle];

Test[
	(lf // BoundedIntegrate // Derivative)[Exp[I 0.1]]
	,
	 (lf[Exp[I 0.1]] - FFT[lf][[-1]] Exp[- I 0.1]) 
	,
	TestID->"Circle-BoundedIntegrate"
		,
	EquivalenceFunction -> NEqual
]


