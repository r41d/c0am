:- module(helper, [zipN/3, zip1L/3, zip/3, zip3/4, zipE/3,
                  melt/2, meltStr/2,
                  gen123/2, nxt/3, fst/2, snd/2, intersperse/3]).

%%%%%%%%%%%%%%%%%%% Hier wird sich ne Tonne an Hilfsprädikaten ansammeln...

% zippe die Liste derart: [a,b,c] -> [(a,1),(b,2),(c,3)] wobei man den Start (hier 1) angeben kann
zipN([], _, []).
zipN([H|T], N, [(H,N)|TT]) :- N1 is N+1, zipN(T, N1, TT).

% zippe die Liste derart:
% [a,b,c] -> [(1,a),(2,b),(3,c)]
% wobei man den Start (hier 1) angeben kann
zip1L([], _, []).
zip1L([H|T], N, [(N,H)|TT]) :- N1 is N+1, zip1L(T, N1, TT).

% was soll man dazu noch erklären
zip([], [], []).
zip([X|Xs], [Y|Ys], [(X,Y)|Zs]) :- zip(Xs,Ys,Zs).
zip3([], [], [], []).
zip3([X|Xs], [Y|Ys], [Z|Zs], [(X,Y,Z)|As]) :- zip3(Xs,Ys,Zs,As).
% zipE: second argument is not a list but a constant parameter which becomes the second part of all tuples
zipE([], _, []).
zipE([X|Xs], Y, [(X,Y)|Zs]) :- zipE(Xs,Y,Zs).

melt(ListOfLists, Result) :-
  reverse(ListOfLists, Reversed),
  foldl(append, Reversed, [], Result).
% foldl(append, ListOfLists, [], CmdList)
%   This flattens the ListOfLists IN REVERSE ORDER into CmdList
%   So, foldl(append, [[a],[b,c],[d]], [], [d,b,c,a]) is true
%   (This is used many times)

meltStr(ListOfStrings, Result) :-
  reverse(ListOfStrings, Reversed),
  foldl(string_concat, Reversed, "", Result).


% Haskell-Äquivalent: L = [1..N]
gen123(N, L) :- gen321(N, R), reverse(R, L), !.
gen321(0, []).
gen321(N, [N|T]) :- ND is N-1, gen321(ND, T).

%nxt :: Counter -> Int -> Counter
%nxt((c(X,e)), N, c(X,(c(N,e)))  ) :- !.
%nxt((c(X,C)), N, c(X,Inner)     ) :- nxt(C,N,Inner).
%nxt( e      , _, e              ).

% next(CounterList, NextInt, NewCounterList)/3

% well, that doesn't work anymore
%:- use_module(library(arithmetic)).
%:- arithmetic_function(nxt/2).
nxt(C, I, NewC) :- append(C, [I], NewC).

fst( (X,_Y), X).
snd( (_X,Y), Y).

% intersperse :: a -> [a] -> [a]
%intersperse(Elem, List, ListReturn)
intersperse(_E, [], []).
intersperse(_E, [X], [X]).
intersperse(E, [H|T1], [H,E|T2]) :- intersperse(E, T1, T2).


