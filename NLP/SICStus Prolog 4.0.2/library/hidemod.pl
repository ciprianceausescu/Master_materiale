/* A trick for making all library modules hidden in the distribution. */

:- multifile user:term_expansion/6.
user:term_expansion((:- module(M,E)), _Lay1, Ids,
		      (:- module(M,E,[hidden(true)])), [], [hidemod|Ids]) :-
	nonmember(hidemod, Ids).


