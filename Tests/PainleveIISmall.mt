(* Mathematica Test File *)

Test[
	PainleveII[{1, 2, 1/3}, 0]-(-0.500684038196442` + 0.11748452476102944` I)//Chop
	,
	0
	,
	TestID->"PainleveII-20101202-Small"
	,
	EquivalenceFunction -> NEqual
]

Test[
	PainleveII[{I, 0, -I}, 0]-(-0.3670615515480784)//Chop
	(* Hastings McLeod at zero, from Prohefer and Spohn *)
	,
	0
	,
	TestID->"PainleveII-20101202-HastingsMcLeod-0"
	,
	EquivalenceFunction -> NEqual
]
