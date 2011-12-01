(* Mathematica Test File *)

Test[
	Parametrix[( {
   {0, 1},
   {-1, 0}
  } ), Line[{-1,1}], 0.1-$MachineTolerance I]
	,
	ParametrixBranch[( {
   {0, 1},
   {-1, 0}
  } ), Line[{-1,1}], 0.1,- 2 Pi]
	,
	TestID->"Skew-Parametrix--"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Parametrix[( {
   {0, 1},
   {-1, 0}
  } ), Line[{-1,1}], 0.1+$MachineTolerance I]
	,
	ParametrixBranch[( {
   {0, 1},
   {-1, 0}
  } ), Line[{-1,1}], 0.1, 0]
	,
	TestID->"Skew-Parametrix-+"
	,
	EquivalenceFunction -> NEqual
]