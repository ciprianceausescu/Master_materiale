/* Copyright(C) 1992, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : UGRAPHS.PL							      %
%   Maintainer : Mats Carlsson						      %
%            New versions of transpose/2, reduce/2, top_sort/2 by Dan Sahlin  %
%   Updated: 3 September 1999						      %
%   Purpose: Unweighted graph-processing utilities			      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  Adapted from shared code written by Richard A O'Keefe */

%@  This library module provides operations on directed graphs.
%@  An unweighted directed graph (ugraph) is represented as a list of
%@  @var{(vertex-neighbors)} pairs, where the pairs are in standard order
%@  (as produced by @code{keysort/2} with unique keys) and the neighbors of
%@  each vertex are also in standard order (as produced by @code{sort/2}), and
%@  every neighbor appears as a vertex even if it has no neighbors
%@  itself.
%@  
%@  An undirected graph is represented as a directed graph where for
%@  each edge @var{(U,V)} there is a symmetric edge @var{(V,U)}.
%@  
%@  An edge @var{(U,V)} is represented as the term @var{U-V}.
%@  
%@  A vertex can be any term.  Two vertices are distinct iff they are
%@  not identical (@code{==}).
%@  
%@  A path is represented as a list of vertices.  
%@  No vertex can appear twice in a path.
%@  

:- module(ugraphs, [
	vertices_edges_to_ugraph/3,
	vertices/2,
	edges/2,
	add_vertices/3,
	del_vertices/3,
	add_edges/3,
	del_edges/3,
	transpose_ugraph/2,
	neighbors/3,
	neighbours/3,
	complement/2,
	compose/3,
	transitive_closure/2,
	symmetric_closure/2,
	top_sort/2,
	max_path/5,
	min_path/5,
	min_paths/3,
	path/3,
	reduce/2,
	reachable/3,
	random_ugraph/3,
	min_tree/3
   ]).

:- use_module(library(ordsets), [
	ord_add_element/3,
	ord_del_element/3,
	ord_disjoint/2,
	ord_intersection/3,
	ord_subset/2,
	ord_subtract/3,
	ord_union/3,
	ord_union/4
   ]).

:- use_module(library(lists), [
	reverse/2
   ]).

:- use_module(library(avl), [
	list_to_avl/2,
	ord_list_to_avl/2,
	avl_fetch/3,
	avl_change/5
   ]).

:- use_module(library(random), [
	random/1
   ]).

:- use_module(library(ugraphs), [
	illarg/4
   ]).

%@  Exported predicates:
%@  
%@  @table @code

%@  @item vertices_edges_to_ugraph(@var{+Vertices}, @var{+Edges}, @var{-Graph})
%@  @PLXindex {vertices_edges_to_ugraph/3 (ugraphs)}
%@  is true if @var{Vertices} is a list of vertices, @var{Edges} is a list of edges,
%@  and @var{Graph} is a graph built from @var{Vertices} and @var{Edges}.  @var{Vertices} and
%@  @var{Edges} may be in any order.  The vertices mentioned in @var{Edges} do not
%@  have to occur explicitly in @var{Vertices}.  @var{Vertices} may be used to
%@  specify vertices that are not connected to any edges.

vertices_edges_to_ugraph(Vertices0, Edges, Graph) :-
	sort(Vertices0, Vertices1),
	sort(Edges, EdgeSet),
	edges_vertices(EdgeSet, Bag),
	sort(Bag, Vertices2),
	ord_union(Vertices1, Vertices2, VertexSet),
	group_edges(VertexSet, EdgeSet, Graph).

edges_vertices([], []).
edges_vertices([From-To|Edges], [From,To|Vertices]) :-
	edges_vertices(Edges, Vertices).

group_edges([], _, []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
	group_edges(Edges, Vertex, Neibs, RestEdges),
	group_edges(Vertices, RestEdges, G).

group_edges([V0-X|Edges], V, [X|Neibs], RestEdges) :- V0==V, !,
	group_edges(Edges, V, Neibs, RestEdges).
group_edges(Edges, _, [], Edges).



%@  @item vertices(@var{+Graph}, @var{-Vertices})
%@  @PLXindex {vertices/2 (ugraphs)}
%@  unifies @var{Vertices} with the vertices in @var{Graph}.

vertices([], []).
vertices([Vertex-_|Graph], [Vertex|Vertices]) :- vertices(Graph, Vertices).



%@  @item edges(@var{+Graph}, @var{-Edges})
%@  @PLXindex {edges/2 (ugraphs)}
%@  unifies @var{Edges} with the edges in @var{Graph}.

edges([], []).
edges([Vertex-Neibs|G], Edges) :-
	edges(Neibs, Vertex, Edges, MoreEdges),
	edges(G, MoreEdges).

edges([], _, Edges, Edges).
edges([Neib|Neibs], Vertex, [Vertex-Neib|Edges], MoreEdges) :-
	edges(Neibs, Vertex, Edges, MoreEdges).



%@  @item add_vertices(@var{+Graph1}, @var{+Vertices}, @var{-Graph2})
%@  @PLXindex {add_vertices/3 (ugraphs)}
%@  is true if @var{Graph2} is @var{Graph1} with @var{Vertices} added to it.

add_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	vertex_units(Vs, Graph1),
	graph_union(Graph0, Graph1, Graph).



%@  @item del_vertices(@var{+Graph1}, @var{+Vertices}, @var{-Graph2})
%@  @PLXindex {del_vertices/3 (ugraphs)}
%@  is true if @var{Graph2} is @var{Graph1} with @var{Vertices} and all edges to and from
%@  @var{Vertices} removed from it.

del_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	graph_del_vertices(Graph0, Vs, Vs, Graph).



%@  @item add_edges(@var{+Graph1}, @var{+Edges}, @var{-Graph2}) 
%@  @PLXindex {add_edges/3 (ugraphs)}
%@  is true if @var{Graph2} is @var{Graph1} with @var{Edges} and their "to" and "from"
%@  vertices added to it.

add_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_union(Graph0, Graph1, Graph).



%@  @item del_edges(@var{+Graph1}, @var{+Edges}, @var{-Graph2})
%@  @PLXindex {del_edges/3 (ugraphs)}
%@  is true if @var{Graph2} is @var{Graph1} with @var{Edges} removed from it.

del_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_difference(Graph0, Graph1, Graph).



vertex_units([], []).
vertex_units([V|Vs], [V-[]|Us]) :- vertex_units(Vs, Us).


graph_union(G0, [], G) :- !, G = G0.
graph_union([], G, G).
graph_union([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_union(C, V1, N1, G1, V2, N2, G2, G).

graph_union(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_union(G1, [V2-N2|G2], G).
graph_union(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	ord_union(N1, N2, N),
	graph_union(G1, G2, G).
graph_union(>, V1, N1, G1, V2, N2, G2, [V2-N2|G]) :-
	graph_union([V1-N1|G1], G2, G).



graph_difference(G0, [], G) :- !, G = G0.
graph_difference([], _, []).
graph_difference([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_difference(C, V1, N1, G1, V2, N2, G2, G).

graph_difference(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_difference(G1, [V2-N2|G2], G).
graph_difference(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	ord_subtract(N1, N2, N),
	graph_difference(G1, G2, G).
graph_difference(>, V1, N1, G1, _, _, G2, G) :-
	graph_difference([V1-N1|G1], G2, G).


graph_del_vertices(G1, [], Set, G) :- !,
	graph_del_vertices(G1, Set, G).
graph_del_vertices([], _, _, []).
graph_del_vertices([V1-N1|G1], [V2|Vs], Set, G) :-
	compare(C, V1, V2),
	graph_del_vertices(C, V1, N1, G1, V2, Vs, Set, G).

graph_del_vertices(<, V1, N1, G1, V2, Vs, Set, [V1-N|G]) :-
	ord_subtract(N1, Set, N),
	graph_del_vertices(G1, [V2|Vs], Set, G).
graph_del_vertices(=, _, _, G1, _, Vs, Set, G) :-
	graph_del_vertices(G1, Vs, Set, G).
graph_del_vertices(>, V1, N1, G1, _, Vs, Set, G) :-
	graph_del_vertices([V1-N1|G1], Vs, Set, G).

graph_del_vertices([], _, []).
graph_del_vertices([V1-N1|G1], Set, [V1-N|G]) :-
	ord_subtract(N1, Set, N),
	graph_del_vertices(G1, Set, G).



%@  @item transpose_ugraph(@var{+Graph}, @var{-Transpose})
%@  @PLXindex {transpose_ugraph/2 (ugraphs)}
%@  is true if @var{Transpose} is the graph computed by replacing each edge
%@  @var{(u,v)} in @var{Graph} by its symmetric edge @var{(v,u)}.  It can only be used
%@  one way around.  The cost is @var{O(N log N)}.

transpose_ugraph(Graph, Transpose) :-
	transpose_edges(Graph, TEdges, []),
	sort(TEdges, TEdges2),
	vertices(Graph, Vertices),
	group_edges(Vertices, TEdges2, Transpose).

transpose_edges([]) --> [].
transpose_edges([Vertex-Neibs|G]) -->
        transpose_edges(Neibs, Vertex),
	transpose_edges(G).

transpose_edges([], _) --> [].
transpose_edges([Neib|Neibs], Vertex) --> [Neib-Vertex],
	transpose_edges(Neibs, Vertex).


%@  @item neighbors(@var{+Vertex}, @var{+Graph}, @var{-Neighbors})
%@  @itemx neighbours(@var{+Vertex}, @var{+Graph}, @var{-Neighbors})
%@  @PLXindex {neighbors/3 (ugraphs)}
%@  @PLXindex {neighbours/3 (ugraphs)}
%@  is true if @var{Vertex} is a vertex in @var{Graph} and @var{Neighbors} are its neighbors.

neighbours(Vertex, Graph, Neighbors) :-
	neighbors(Vertex, Graph, Neighbors).

neighbors(V, [V0-Neighbors|_], Neighbors) :- V0==V, !.
neighbors(V, [_|Graph], Neighbors) :- neighbors(V, Graph, Neighbors).



%@  @item complement(@var{+Graph}, @var{-Complement})
%@  @PLXindex {complement/2 (ugraphs)}
%@  @var{Complement} is the complement graph of @var{Graph}, i.e. the graph that has
%@  the same vertices as @var{Graph} but only the edges that are not in @var{Graph}.

complement(Graph, Complement) :-
	vertices(Graph, Vertices),
	complement(Graph, Vertices, Complement).

complement([], _, []).
complement([V-Neibs|Graph], Vertices, [V-Others|Complement]) :-
	ord_add_element(Neibs, V, Neibs1),
	ord_subtract(Vertices, Neibs1, Others),
	complement(Graph, Vertices, Complement).



%@  @item compose(@var{+G1}, @var{+G2}, @var{-Composition})
%@  @PLXindex {compose/3 (ugraphs)}
%@  computes @var{Composition} as the composition of two graphs, which need
%@  not have the same set of vertices.

compose(G1, G2, Composition) :-
	vertices(G1, V1),
	vertices(G2, V2),
	ord_union(V1, V2, V),
	compose(V, G1, G2, Composition).

compose([], _, _, []).
compose([V0|Vertices], [V-Neibs|G1], G2, [V-Comp|Composition]) :- V==V0, !,
	compose1(Neibs, G2, [], Comp),
	compose(Vertices, G1, G2, Composition).
compose([V|Vertices], G1, G2, [V-[]|Composition]) :-
	compose(Vertices, G1, G2, Composition).

compose1([V1|Vs1], [V2-N2|G2], SoFar, Comp) :- !,
	compare(Rel, V1, V2),
	compose1(Rel, V1, Vs1, V2, N2, G2, SoFar, Comp).
compose1(_, _, Comp, Comp).

compose1(<, _, Vs1, V2, N2, G2, SoFar, Comp) :-
	compose1(Vs1, [V2-N2|G2], SoFar, Comp).
compose1(>, V1, Vs1, _, _, G2, SoFar, Comp) :-
	compose1([V1|Vs1], G2, SoFar, Comp).
compose1(=, V1, Vs1, V1, N2, G2, SoFar, Comp) :-
	ord_union(N2, SoFar, Next),
	compose1(Vs1, G2, Next, Comp).



%@  @item transitive_closure(@var{+Graph}, @var{-Closure}) 
%@  @PLXindex {transitive_closure/2 (ugraphs)}
%@  computes @var{Closure} as the transitive closure of @var{Graph} in @var{O(N^3)} time.

transitive_closure(Graph, Closure) :-
	warshall(Graph, Graph, Closure).

warshall([], Closure, Closure).
warshall([V-_|G], E, Closure) :-
	neighbors(V, E, Y),
	warshall(E, V, Y, NewE),
	warshall(G, NewE, Closure).

warshall([], _, _, []).
warshall([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	ord_subset([V], Neibs), !,
	ord_del_element(Y, X, Y1),
	ord_union(Neibs, Y1, NewNeibs),
	warshall(G, V, Y, NewG).
warshall([X-Neibs|G], V, Y, [X-Neibs|NewG]) :-
	warshall(G, V, Y, NewG).



%@  @item symmetric_closure(@var{+Graph}, @var{-Closure}) 
%@  @PLXindex {symmetric_closure/2 (ugraphs)}
%@  computes @var{Closure} as the symmetric closure of @var{Graph}, i.e. for each
%@  edge @var{(u,v)} in @var{Graph}, add its symmetric edge @var{(v,u)}.  Approx. @var{O(N log N)}
%@  time.  This is useful for making a directed graph undirected.

symmetric_closure(Graph, Closure) :-
	transpose_ugraph(Graph, Transpose),
	symmetric_closure(Graph, Transpose, Closure).

symmetric_closure([], [], []).
symmetric_closure([V-Neibs1|Graph], [V-Neibs2|Transpose], [V-Neibs|Closure]) :-
	ord_union(Neibs1, Neibs2, Neibs),
	symmetric_closure(Graph, Transpose, Closure).



%@  @item top_sort(@var{+Graph}, @var{-Sorted})
%@  @PLXindex {top_sort/2 (ugraphs)}
%@  finds a topological ordering of @var{Graph} and returns the ordering
%@  as a list of @var{Sorted} vertices.  Fails iff no ordering exists, i.e.
%@  iff the graph contains cycles.  Approx. @var{O(N log N)} time.

top_sort(Graph, Sorted) :-
	fanin_counts(Graph, Counts),
	get_top_elements(Counts, Top, 0, I),
	ord_list_to_avl(Counts, Map),
	top_sort(Top, I, Map, Sorted).

top_sort([], 0, _, []).
top_sort([V-VN|Top0], I, Map0, [V|Sorted]) :-
	dec_counts(VN, I, J, Map0, Map, Top0, Top),
	top_sort(Top, J, Map, Sorted).

dec_counts([], I, I, Map, Map, Top, Top).
dec_counts([N|Ns], I, K, Map0, Map, Top0, Top) :-
	avl_change(N, Map0, NN-C0, Map1, NN-C),
	C is C0-1,
	(C=:=0 -> J is I-1, Top1 = [N-NN|Top0]; J = I, Top1 = Top0),
	dec_counts(Ns, J, K, Map1, Map, Top1, Top).

get_top_elements([], [], I, I).
get_top_elements([V-(VN-C)|Counts], Top0, I, K) :-
	(C=:=0 -> J = I, Top0 = [V-VN|Top1]; J is I+1, Top0 = Top1),
	get_top_elements(Counts, Top1, J, K).

fanin_counts(Graph, Counts) :-
	transpose_edges(Graph, Edges0, []),
	keysort(Edges0, Edges),
	fanin_counts(Graph, Edges, Counts).

fanin_counts([], [], []).
fanin_counts([V-VN|Graph], Edges, [V-(VN-C)|Counts]) :-
	fanin_counts(Edges, V, 0, C, Edges1),
	fanin_counts(Graph, Edges1, Counts).

fanin_counts([V-_|Edges0], V0, C0, C, Edges) :-
	V==V0, !,
	C1 is C0+1,
	fanin_counts(Edges0, V0, C1, C, Edges).
fanin_counts(Edges, _, C, C, Edges).


%@  @item max_path(@var{+V1}, @var{+V2}, @var{+Graph}, @var{-Path}, @var{-Cost})
%@  @PLXindex {max_path/5 (ugraphs)}
%@  is true if @var{Path} is a list of vertices constituting a longest path
%@  of cost @var{Cost} from @var{V1} to @var{V2} in @var{Graph}, there being no cyclic paths from
%@  @var{V1} to @var{V2}.  Takes @var{O(N^2)} time.

max_path(Initial, Final, Graph, Path, Cost) :-
	transpose_ugraph(Graph, TGraph),
	max_path_init(Initial, Final, Graph, TGraph, TGraph2, Order),
	max_path_init(TGraph2, Val0),
	max_path(Order, TGraph2, Val0, Val),
	max_path_select(Val, Path, Cost).

max_path_init(Initial, Final, Graph, TGraph, TGraph2, Order) :-
	reachable(Initial, Graph, InitialReachable),
	reachable(Final, TGraph, FinalReachable),
	ord_intersection(InitialReachable, FinalReachable, Reachable),
	subgraph(TGraph, Reachable, TGraph2),
	top_sort(TGraph2, Order).

max_path_init([], []).
max_path_init([V-_|G], [V-([]-0)|Val]) :- max_path_init(G, Val).

max_path_select([V-(Longest-Max)|Val], Path, Cost) :-
	max_path_select(Val, V, Longest, Path, Max, Cost).

max_path_select([], V, Path, [V|Path], Cost, Cost).
max_path_select([V1-(Path1-Cost1)|Val], V2, Path2, Path, Cost2, Cost) :-
	(   Cost1>Cost2 -> 
	    max_path_select(Val, V1, Path1, Path, Cost1, Cost)
	;   max_path_select(Val, V2, Path2, Path, Cost2, Cost)
	).

max_path([], _, Val, Val).
max_path([V|Order], Graph, Val0, Val) :-
	neighbors(V, Graph, Neibs),
	neighbors(V, Val0, Item),
	max_path_update(Neibs, V-Item, Val0, Val1),
	max_path(Order, Graph, Val1, Val).

%% [MC] 3.8.6: made determinate
max_path_update([], _, Val, Val).
max_path_update([N|Neibs], Item, [Item0|Val0], Val) :-
	Item0 = V0-(_-Cost0),
	N==V0, !,
	Item = V-(Path-Cost),
	Cost1 is Cost+1,
	(   Cost1>Cost0 -> Val = [V0-([V|Path]-Cost1)|Val1]
	;   Val = [Item0|Val1]
	),
	max_path_update(Neibs, Item, Val0, Val1).
max_path_update(Neibs, Item, [X|Val0], [X|Val]) :-
	Neibs = [_|_],
	max_path_update(Neibs, Item, Val0, Val).

subgraph([], _, []).
subgraph([V-Neibs|Graph], Vs, [V-Neibs1|Subgraph]) :-
	ord_subset([V], Vs), !,
	ord_intersection(Neibs, Vs, Neibs1),
	subgraph(Graph, Vs, Subgraph).
subgraph([_|Graph], Vs, Subgraph) :-
	subgraph(Graph, Vs, Subgraph).



%@  @item min_path(@var{+V1}, @var{+V2}, @var{+Graph}, @var{-Path}, @var{-Length})
%@  @PLXindex {min_path/5 (ugraphs)}
%@  is true if @var{Path} is a list of vertices constituting a shortest path
%@  of length @var{Length} from @var{V1} to @var{V2} in @var{Graph}.  Takes @var{O(N^2)} time.

min_path(Initial, Final, Graph, Path, Length) :-
	min_path([[Initial]|Q], Q, [Initial], Final, Graph, Rev),
	reverse(Rev, Path),
	length(Path, N),
	Length is N-1.

min_path(Head0, Tail0, Closed0, Final, Graph, Rev) :-
	Head0 \== Tail0,
	Head0 = [Sofar|Head],
	Sofar = [V|_],
	(   V==Final -> Rev = Sofar
	;   neighbors(V, Graph, Neibs),
	    ord_union(Closed0, Neibs, Closed, Neibs1),
	    enqueue(Neibs1, Sofar, Tail0, Tail),
	    min_path(Head, Tail, Closed, Final, Graph, Rev)
	).

enqueue([], _) --> [].
enqueue([V|Vs], Sofar) --> [[V|Sofar]], enqueue(Vs, Sofar).



%@  @item min_paths(@var{+Vertex}, @var{+Graph}, @var{-Tree})
%@  @PLXindex {min_paths/3 (ugraphs)}
%@  is true if @var{Tree} is a tree of all the shortest paths from @var{Vertex} to
%@  every other vertex in @var{Graph}.  This is the single-source shortest
%@  paths problem.  The algorithm is straightforward.

min_paths(Vertex, Graph, Tree) :-
	min_paths([Vertex], Graph, [Vertex], List),
	keysort(List, Tree).

min_paths([], _, _, []).
min_paths([Q|R], Graph, Reach0, [Q-New|List]) :-
	neighbors(Q, Graph, Neibs),
	ord_union(Reach0, Neibs, Reach, New),
	append(R, New, S),
	min_paths(S, Graph, Reach, List).



%@  @item path(@var{+Vertex}, @var{+Graph}, @var{-Path})
%@  @PLXindex {path/3 (ugraphs)}
%@  is given a @var{Graph} and a @var{Vertex} of that @var{Graph}, and returns a maximal
%@  @var{Path} rooted at @var{Vertex}, enumerating more @var{Paths} on backtracking.

path(Initial, Graph, Path) :-
	path([Initial], [], Graph, Path).

path(Q, Not, Graph, Path) :-
	Q = [Qhead|_],
	neighbors(Qhead, Graph, Neibs),
	ord_subtract(Neibs, Not, Neibs1),
	(   Neibs1 = [] -> reverse(Q, Path)
	;   ord_add_element(Not, Qhead, Not1),
	    member(N, Neibs1),
	    path([N|Q], Not1, Graph, Path)
	).



%@  @item reduce(@var{+Graph}, @var{-Reduced})
%@  @PLXindex {reduce/2 (ugraphs)}
%@  is true if @var{Reduced} is the reduced graph for @var{Graph}. The vertices of
%@  the reduced graph are the strongly connected components of @var{Graph}.
%@  There is an edge in @var{Reduced} from @var{u} to @var{v} iff there is an edge in
%@  @var{Graph} from one of the vertices in @var{u} to one of the vertices in @var{v}. A
%@  strongly connected component is a maximal set of vertices where
%@  each vertex has a path to every other vertex.
%@  Algorithm from "Algorithms" by Sedgewick, page 482, Tarjan's algorithm.
%   Approximately linear in the maximum of arcs and nodes (O(N log N)).

reduce(Graph, Reduced) :-
	strong_components(Graph, SCCS, Map),
	reduced_vertices_edges(Graph, Vertices, Map, Edges, []),
	sort(Vertices, Vertices1),
	sort(Edges, Edges1),
	group_edges(Vertices1, Edges1, Reduced),
	sort(SCCS, Vertices1).

strong_components(Graph, SCCS, A) :-
	nodeinfo(Graph, Nodeinfo, Vertices), 
	ord_list_to_avl(Nodeinfo, A0), 
	visit(Vertices, 0, _, A0, A, 0, _, [], _, SCCS, []).

visit([], Min, Min, A, A, I, I, Stk, Stk) --> [].
visit([V|Vs], Min0, Min, A0, A, I, M, Stk0, Stk) -->
	{avl_change(V, A0, node(Ns,J,Eq), A1, node(Ns,K,Eq))},
	(   {J>0} ->
	    {J=K, J=Min1, A1=A3, I=L, Stk0=Stk2}
	;   {K is I+1},
	    visit(Ns, K, Min1, A1, A2, K, L, [V|Stk0], Stk1),
	    (   {K>Min1} -> {A2=A3, Stk1=Stk2}
	    ;   pop(V, Eq, A2, A3, Stk1, Stk2, [])
	    )
	),
	{Min2 is min(Min0,Min1)},
	visit(Vs, Min2, Min, A3, A, L, M, Stk2, Stk).

pop(V, Eq, A0, A, [V1|Stk0], Stk, SCC0) -->
	{avl_change(V1, A0, node(Ns,_,Eq), A1, node(Ns,0x100000,Eq))},
	(   {V==V1} -> [SCC], {A1=A, Stk0=Stk, sort([V1|SCC0], SCC)}
	;   pop(V, Eq, A1, A, Stk0, Stk, [V1|SCC0])
	).

nodeinfo([], [], []).
nodeinfo([V-Ns|G], [V-node(Ns,0,_)|Nodeinfo], [V|Vs]) :-
	nodeinfo(G, Nodeinfo, Vs).

reduced_vertices_edges([], [], _) --> [].
reduced_vertices_edges([V-Neibs|Graph], [V1|Vs], Map) -->
	{avl_fetch(V, Map, N), N=node(_,_,V1)},
	reduced_edges(Neibs, V1, Map),
	reduced_vertices_edges(Graph, Vs, Map).

reduced_edges([], _, _) --> [].
reduced_edges([V|Vs], V1, Map) -->
	{avl_fetch(V, Map, N), N=node(_,_,V2)},
	({V1==V2} -> []; [V1-V2]),
	reduced_edges(Vs, V1, Map).


%@  @item reachable(@var{+Vertex}, @var{+Graph}, @var{-Reachable})
%@  @PLXindex {reachable/3 (ugraphs)}
%@  is given a Graph and a @var{Vertex} of that @var{Graph}, and returns the set
%@  of vertices that are @var{Reachable} from that @var{Vertex}.  Takes @var{O(N^2)}
%@  time.

reachable(Initial, Graph, Reachable) :-
	reachable([Initial], Graph, [Initial], Reachable).

reachable([], _, Reachable, Reachable).
reachable([Q|R], Graph, Reach0, Reachable) :-
	neighbors(Q, Graph, Neighbors),
	ord_union(Reach0, Neighbors, Reach1, New),
	append(R, New, S),
	reachable(S, Graph, Reach1, Reachable).



%@  @item random_ugraph(@var{+P}, @var{+N}, @var{-Graph})
%@  @PLXindex {random_ugraph/3 (ugraphs)}
%@  where @var{P} is a probability, unifies @var{Graph} with a random graph of @var{N}
%@  vertices where each possible edge is included with probability @var{P}.

random_ugraph(P, N, Graph) :-
	(   float(P), P >= 0.0, P =< 1.0 -> true
	;   illarg(domain(float,between(0.0,1.0)),
		   random_ugraph(P,N,Graph), 1, P)
	),
	(   integer(N), N >= 0 -> true
	;   illarg(domain(integer,>=(0)),
		   random_ugraph(P,N,Graph), 2, P)
	),
	random_ugraph(0, N, P, Graph).

random_ugraph(N, N, _, Graph) :- !, Graph = [].
random_ugraph(I, N, P, [J-List|Graph]) :-
	J is I+1,
	random_neighbors(N, J, P, List, []),
	random_ugraph(J, N, P, Graph).

random_neighbors(0, _, _, S0, S) :- !, S = S0.
random_neighbors(N, J, P, S0, S) :-
	(   N==J -> S1 = S
	;   random(X), X > P -> S1 = S
	;   S1 = [N|S]
	),
	M is N-1,
	random_neighbors(M, J, P, S0, S1).



%@  @item min_tree(@var{+Graph}, @var{-Tree}, @var{-Cost})
%@  @PLXindex {min_tree/3 (ugraphs)}
%@  is true if @var{Tree} is a spanning tree of an @emph{undirected} @var{Graph} with
%@  cost @var{Cost}, if it exists.  Using a version of Prim's algorithm.

min_tree([V-Neibs|Graph], Tree, Cost) :-
	length(Graph, Cost),
	prim(Cost, Neibs, Graph, [V], Edges),
	vertices_edges_to_ugraph([], Edges, Tree).	

%% [MC] 3.8.6: made determinate
prim(0, [], _, _, []) :- !.
prim(I, [V|Vs], Graph, Reach, [V-W,W-V|Edges]) :-
	neighbors(V, Graph, Neibs),
	ord_subtract(Neibs, Reach, Neibs1),
	ord_subtract(Neibs, Neibs1, [W|_]),
	ord_add_element(Reach, V, Reach1),
	ord_union(Vs, Neibs1, Vs1),
	J is I-1,
	prim(J, Vs1, Graph, Reach1, Edges).

%@  @end table
