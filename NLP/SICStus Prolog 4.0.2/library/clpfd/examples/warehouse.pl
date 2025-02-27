/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Warehouse Location Problem
 * Author    : Mats Carlsson
 *
 */

:- module(warehouse, [warehouse/2]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

warehouse(cost, Key) :-
	warehouse(Key, Warehouses, Cost, _),
	varorder(posofmin, Key, Warehouses, Vars),
	cost_labeling(Vars, Cost),
	format('warehouses=~w, cost=~w\n', [Warehouses,Cost]).
warehouse(lex, Key) :-
	warehouse(Key, Warehouses, Cost, _),
	varorder(posofmin, Key, Warehouses, Vars),
	lex_minimize(Vars, Cost),
	format('warehouses=~w, cost=~w\n', [Warehouses,Cost]).
warehouse(heur, Key) :-
	warehouse(Key, Warehouses, Cost, LBW),
	varorder(maxregret, Key, Warehouses, VarRows),
	minimize(heur_labeling(VarRows, [], 0, LBW), Cost),
	format('warehouses=~w, cost=~w\n', [Warehouses,Cost]).

cost_labeling(Vars, Cost) :-
	labeling([bisect], [Cost]),
	labeling([], Vars).

% heur_labeling([], _, _).
% heur_labeling([Var-Row|Rest], Set, LBW) :-
% 	member(_-Var, Row),
% 	heur_labeling(Rest, Set, LBW).

heur_labeling([], _, _, _).
heur_labeling([Var-Row|Rest], Set, Size, LBW) :-
	Size < LBW, !,
	member(_-Var, Row),
	(   fdset_member(Var, Set) ->
	    Set2 = Set,
	    Size2 = Size
	;   fdset_add_element(Set, Var, Set2),
	    Size2 is Size+1
	),
	heur_labeling(Rest, Set2, Size2, LBW).
heur_labeling([Var-Row|Rest], Set, Size, LBW) :-
	member(_-Var, Row),
	fdset_member(Var, Set),
	heur_labeling(Rest, Set, Size, LBW).
heur_labeling([Var-Row|Rest], Set, Size, LBW) :-
	member(_-Var, Row),
	\+fdset_member(Var, Set),
	fdset_add_element(Set, Var, Set2),
	Size2 is Size+1,
	heur_labeling(Rest, Set2, Size2, LBW).


warehouse(Key, Warehouses, Cost, LBW) :-
	problem(Key, Capacities, BuildCost, Matrix),
	length(Matrix, NStores),
	length(Warehouses, NStores),
	capacities_costs(Capacities, Keylist, Binaries, 0),
	build_cost_lb(Capacities, NStores, BuildCost, LBW, LBC),
	BCost #>= LBC,
	BCost + GCCOST #= Cost,
	listof(Binaries, BuildCost, BuildCosts),
	scalar_product(BuildCosts, Binaries, #=, BCost),
	global_cardinality(Warehouses, Keylist, [cost(GCCOST,Matrix)]).

build_cost_lb(Cap1, Demand, BC, LBW, LBC) :-
	tag_neg(Cap1, Cap2),
	keysort(Cap2, Cap3),
	build_cost_lb(Demand, Cap3, Cap4),
	length(Cap3, Len3),
	length(Cap4, Len4),
	LBW is Len3-Len4,
	LBC is BC*LBW.

build_cost_lb(Demand) --> {Demand=<0}, !.
build_cost_lb(Demand1) --> [C-_],
	{Demand2 is Demand1+C},
	build_cost_lb(Demand2).

tag_neg([], []).
tag_neg([X|Xs], [Y-0|Ys]) :-
	Y is -X,
	tag_neg(Xs, Ys).
	

listof([], _, []).
listof([_|Xs], N, [N|Ns]) :-
	listof(Xs, N, Ns).

varorder(posofmin, Key, Warehouses, Vars) :-
	problem(Key, _, _, Matrix),
	rank_posofmin(Matrix, Keys),
	keys_and_values(KL1, Keys, Warehouses),
	keysort(KL1, KL2),
	keys_and_values(KL2, _, Vars), !.
varorder(maxregret, Key, Warehouses, VarRows2) :-
	problem(Key, _, _, Matrix1),
	rank_matrix(Matrix1, Matrix2),
	keys_and_values(VarRows1, Warehouses, Matrix2),
	rank_maxregret(Matrix2, Keys),
	keys_and_values(KL1, Keys, VarRows1),
	keysort(KL1, KL2),
	keys_and_values(KL2, _, VarRows2), !.

rank_matrix([], []).
rank_matrix([Row1|Rows1], [Row3|Rows2]) :-
	tag_row(Row1, 0, Row2),
	keysort(Row2, Row3),
	rank_matrix(Rows1, Rows2).

tag_row([], _, []).
tag_row([V|Vs], I, [V-J|Ws]) :-
	J is I+1,
	tag_row(Vs, J, Ws).

% Rank = (index of smallest element,neg. regret)
rank_posofmin([], []).
rank_posofmin([Row|Rows], [(Ix,NegRegret)|Ranks]) :-
	min_member(Min, Row),
	nth1(Ix, Row, Min),
	select(Min, Row, Rest), !,
	min_member(Min2, Rest),
	NegRegret is Min-Min2,
	rank_posofmin(Rows, Ranks).

% Rank = (neg. regret)
rank_maxregret([], []).
rank_maxregret([[V1-_,V2-_|_]|Rows], [NegRegret|Ranks]) :-
	NegRegret is V1-V2,
	rank_maxregret(Rows, Ranks).

capacities_costs([], [], [], _).
capacities_costs([Cap|Caps], [J-N|KL], [B|Bs], I) :-
	J is I+1,
	N in 0..Cap,
	B #= min(N,1),
	capacities_costs(Caps, KL, Bs, J).

lex_minimize(Vars, Cost) :-
	findall(Vars-Cost, (labeling([],Vars) -> true), [Best-UB]),
	lex_minimize(Vars, Cost, Best, UB).

lex_minimize(Vars, Cost, Best, UB) :-
	Cost #< UB,
	findall(Vars-Cost, (lex_chain([Best,Vars],[op(#<)]), labeling([],Vars) -> true), [Best1-UB1]), !,
	lex_minimize(Vars, Cost, Best1, UB1).
lex_minimize(Vars, Cost, Vars, Cost).



% problem(ID, Capacities, BuildCosts, CostMatrix).
:- dynamic problem/4.
problem(p1,
	[1,4,2,1,3],
	30,
	[[20,24,11,25,30],
	 [28,27,82,83,74],
	 [74,97,71,96,70],
	 [ 2,55,73,69,61],
	 [46,96,59,83, 4],
	 [42,22,29,67,59],
	 [ 1, 5,73,59,56],
	 [10,73,13,43,96],
	 [93,35,63,85,46],
	 [47,65,55,71,95]]).

problem(cap44,
	[4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4],
	25000,
	[[ 6739,10355,7650,5219,5776,6641,4374,3847,6429,5396,5219,4182,7391,5038,10349,6051],
	[ 3204,5457,3845,2396,2628,3220,1838,2266,3117,2582,2296,1779,5115,2189,5399,2838],
	[ 4914,26409,19622,13876,9147,14977,21848,35330,15111,23679,9828,19303,57472,11180,22957,15489],
	[ 32372,29982,21024,29681,21275,20071,64292,80186,25921,69206,23096,48700,135170,40527,60515,52911],
	[ 1715,2152,1577,1061,1250,1363,1524,955,1318,1789,1133,1015,2005,1379,2512,1823],
	[ 6421,23701,16197,10383,7483,12332,15840,27251,12444,17769,7029,13919,45474,6966,20326,10956],
	[ 81972,28499,43134,65767,58805,48555,138615,155294,53176,147325,56998,102384,259515,96429,131920,118381],
	[ 33391,26544,6370,16770,13571,8861,51550,57907,10985,57376,12741,33595,105796,32220,60071,46527],
	[ 2020,2480,1869,1324,1525,1646,1817,1211,1593,2099,1395,1275,1940,1663,2869,2135],
	[ 1459,1995,1402,869,1050,1181,1133,546,1134,1406,928,768,1950,1145,2314,1500],
	[ 141015,205925,104130,12638,46089,66146,198300,220212,58178,241573,25277,97536,461992,106122,288418,185456],
	[ 17684,32069,15322,8429,1231,9073,32781,41335,9254,37595,2463,19775,79235,16712,41956,28589],
	[ 38207,42477,15319,15832,11526,5185,62653,71210,11563,70496,10371,38482,135275,36631,77569,55891],
	[ 1953,5044,4089,3428,2289,3530,5553,8308,3558,5947,2434,4897,13134,3283,4634,4204],
	[ 17181,36054,25399,16297,15828,21148,7310,21709,20779,12569,14621,11931,39913,11169,32479,14375],
	[ 25640,35602,25154,15763,18421,21255,18478,8135,20437,23300,16271,12619,35094,19253,39867,24957],
	[ 7031,10492,6305,2542,3918,4743,6856,7119,4415,8788,3062,2712,17458,5031,13093,8294],
	[ 78453,92515,36644,27445,23562,23034,126332,141375,11423,144881,22846,74042,274079,75211,159433,114834],
	[ 9452,12441,7754,3542,5082,6005,10274,10214,5638,12438,4123,5635,21789,8073,16239,11726],
	[ 8597,14113,10500,7254,7875,9152,5467,6371,8870,6729,7312,5869,9506,6695,13554,7707],
	[ 1581,2030,1326,693,924,1063,1628,1619,1008,1953,780,931,3066,1298,2600,1846],
	[ 23170,48702,36072,26166,23493,30494,14919,33813,30655,11166,22333,20982,56025,17380,40178,15978],
	[ 12087,19877,9670,3801,2252,5847,19650,23844,6260,22873,0,10703,46945,10145,26881,17728],
	[ 4883,12851,10822,8930,6798,9435,11943,18148,9496,11616,7106,10898,28226,7117,7725,7911],
	[ 24063,39682,24603,11050,13644,18976,20197,24684,17796,27157,10541,5270,59259,14845,44597,26495],
	[ 4124,12148,8180,5611,2952,5851,11613,17111,5918,12852,3268,9073,29479,5623,13172,8745],
	[ 281463,406770,325852,253234,264755,294457,211356,210756,289434,239639,248102,222222,124051,238875,392519,261534],
	[ 11056,22113,11424,5582,2430,7436,19279,25460,7551,22351,1601,11698,49650,9022,26549,16963],
	[ 8585,22449,14122,7458,6609,10790,11525,20448,10887,14092,5916,8953,37089,2958,20575,9917],
	[ 12480,25455,22151,19069,15598,19892,23976,34080,19991,23444,16099,22275,50490,16118,8049,17411],
	[ 3727,11116,8229,5826,4628,6632,6476,11884,6678,6101,4440,6843,18722,3693,8596,3285],
	[ 4673,13346,7880,4330,2861,5655,9623,14610,5719,11338,2398,6931,26701,3900,13310,7961],
	[ 13451,35106,25927,17347,15249,21192,16808,32845,21329,15695,14693,19385,53121,10951,27888,7346],
	[ 372672,229188,203364,322800,261306,229995,681269,810550,258078,728398,261790,512606,1361570,451435,644793,575875],
	[ 9745,18070,12049,7198,7592,9802,5780,10692,9607,8559,6353,4891,21336,5951,17830,9514],
	[ 12055,18181,11400,5307,7379,8870,10865,10202,8340,13994,5984,4154,26946,8459,21470,13697],
	[ 97602,73603,59561,83331,65940,56946,185247,222003,73007,198738,65986,137295,378755,119995,174969,155375],
	[ 60774,63568,27330,30982,20497,15076,97731,114578,24757,109515,20525,63513,209073,58395,114439,87468],
	[ 54470,65177,52117,40378,44494,47243,47631,34351,46820,53659,42775,39250,40960,48318,73399,55712],
	[ 7146,8618,6428,7822,5211,5621,15256,19762,7047,16305,5658,11939,32644,9417,13583,12308],
	[ 38011,70728,39587,15801,16494,27925,49862,62659,26917,61965,10086,22567,133135,23134,81255,47404],
	[ 39723,52917,32225,13627,20427,24504,41119,40854,22884,50669,16196,20636,91957,31401,69686,47528],
	[ 16111,20714,15620,11041,12598,13719,12502,7885,13320,14853,11550,10071,13193,13052,23103,15654],
	[ 16981,32575,23312,16250,16268,19856,9331,20750,19556,9543,14550,13087,32087,12481,27518,12525],
	[ 168663,210766,169251,131938,144628,153760,134768,109304,150511,149278,136084,124879,89976,142499,224408,160511],
	[ 57109,66703,53124,40919,45381,48057,52583,39050,46994,58814,42605,40553,45922,49175,75966,61003],
	[ 15576,18481,14368,10672,12024,12834,14205,10134,12512,16103,11183,10561,14940,13172,21287,16755],
	[ 2542,3928,3020,2205,2361,2682,1756,1983,2611,2073,2174,1857,2006,2064,3788,2318],
	[ 34056,34221,24448,31329,21905,20807,69009,86632,27212,74352,23899,51935,147937,42986,64873,56510],
	[ 7095,11999,7886,4190,4847,6351,4903,6421,6030,6801,4001,2614,14979,4503,12617,7448]]).
