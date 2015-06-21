:- module(trans, [c0trans/2]).
:- use_module(library(apply)).

:- dynamic symtab/2.


%%%%%%%%%%%%%%%%%%% Export
c0trans(P,C) :- trans(P,C).


%%%%%%%%%%%%%%%%%%% AM Operations
%   read(Int), write(Int),
%   load(Int), store(Int), lit(BracketlessInt),
%   add, mul, sub, div, mod,
%   lt, eq, ne, gt, le, ge,
%   jmp(Counter), jmc(Counter),
%   nop

% opAddSub: addOP, subOP können direkt übernommen werden aus AST
% opMulDivMod: mulOP, divOP, modOP können direkt übernommen werden aus AST
% relation: lt, eq, ne, gt, le, ge können direkt übernommen werden aus AST

% counter repräsentation:
%   e | c(Num,Counter)


% General Notes:
%   foldl(append, ListOfLists, [], CmdList)
%     This flattens the ListOfLists IN REVERSE ORDER into CmdList
%     So, foldl(append, [[a],[b,c],[d]], [], [d,b,c,a]) is true
%     (This is used many times)


% trans DONE
trans(p(B),CmdList) :- blocktrans(B,CmdList).
%trans(_, []) :- write("trans FAILED!\n").

% blocktrans DONE
blocktrans(b(Vardecl,Stseq), CmdList) :-
  mksymtab(Vardecl, SymTab),
  assertsymtab(SymTab),
  stseqtrans(Stseq, [1], CmdList).  %  [1] = StartCounter

% mksymtab DONE
mksymtab(v(Vardecl), SymTab) :-
  zipN(Vardecl, 1, SymTab).
assertsymtab(SymTab) :-
  retractall(symtab(_,_)),
  forall(
    member((Name,ID), SymTab),
    assertz(symtab(Name, ID))
  ).

%stseqtrans DONE
stseqtrans(s([]), _, _, []).
stseqtrans(s(SSS), Counter, CmdList) :-
  zipN(SSS, 1, SSSzip),
  zipE(SSSzip, Counter, InnerInput),  % one hell of a workaround...
  maplist(innerthing, InnerInput, PreFlat),
  flatten(PreFlat, CmdList).
%% workaround for stuff like _G2239 in end of SSS
%% maybe fix parser later xD
innerthing( ((M,_),_), []) :- var(M), !.
innerthing([], []).
innerthing( ((M,N),A) , Res) :- % M=statement N=statementNr A=counter
  nxt(A,N,C),
  sttrans(M, C, Res).

% sttrans DONE
sttrans(ss(Ident), _, [(e, read(ID))]) :- symtab(Ident, ID).
sttrans(sp(Ident), _, [(e, write(ID))]) :- symtab(Ident, ID).
sttrans(sa(a(Ident,Exp)), _, CmdList) :-
  simpleexptrans(Exp, E1),
  symtab(Ident, ID),
  append(E1, [(e, store(ID))], CmdList).
sttrans(si(i(BoolExp,Stmt)), A, CmdList) :-
  boolexptrans(BoolExp, BE),
  nxt(A,1,C),
  sttrans(Stmt, C, ST),
  foldl(append,
    [ [(A,nop)], ST, [(e,jmc(A))], BE ],
    [], CmdList).
sttrans(si(i(BoolExp,Stmt,StmtElse)), A, CmdList) :-
  boolexptrans(BoolExp, BE),
  nxt(A,1,C1),  sttrans(Stmt, C1, ST),
  nxt(A,2,C2),  sttrans(StmtElse, C2, STE),
  nxt(A,3,C3),
  foldl(append,
    [ [(C3,nop)], STE, [(A,nop)], [(e,jmp(C3))], ST, [(e,jmc(A))], BE ],
    [], CmdList).
sttrans(sw(w(Exp,Stmt)), A, CmdList) :-
  boolexptrans(Exp, E),
  nxt(A,1,C1),  sttrans(Stmt, C1, S),
  nxt(A,2,C2),
  foldl(append,
    [ [(A,nop)], [(e,jmp(C2))], S, [(e,jmc(A))], E, [(C2,nop)] ],
    [], CmdList).
sttrans(sss(SSS), A, CmdList) :- stseqtrans(SSS, A, CmdList).

% boolexptrans DONE
boolexptrans(bool(SimpleExp1, Rel, SimpleExp2), CmdList) :-
  simpleexptrans(SimpleExp1, SE1),
  simpleexptrans(SimpleExp2, SE2),
  foldl(append, [ [(e,Rel)], SE2, SE1 ], [], CmdList).

% simpleexptrans DONE
simpleexptrans(simple(Term, []), Res) :- termtrans(Term, Res).
simpleexptrans(simple(Term1, [(OP,Term2)|Tail]), Res) :-
  termtrans(Term1, T1),
  termtrans(Term2, T2),
  simpleexptrans2(simplemore(Tail), SE2),
  foldl(append, [ SE2, [(e,OP)], T2, T1 ], [], Res).
simpleexptrans2(simplemore([]), []).
simpleexptrans2(simplemore([(OP,Term)|Tail]), Res) :-
  termtrans(Term, T),
  simpleexptrans2(simplemore(Tail), SE),
  foldl(append, [ SE, [(e,OP)], T ], [], Res).

% termtrans DONE
termtrans(t(Factor, []), Res) :- factortrans(Factor, Res).
termtrans(t(F1, [(OP,F2)|MoreFactors]), Res) :-
  factortrans(F1, F1T),
  factortrans(F2, F2T),
  termtrans2(t(F2, MoreFactors), TT),
  foldl(append, [TT, [(e, OP)], F2T, F1T], [], Res).
termtrans2(t(_,[]), []).
termtrans2(t(_,[(OP,F)|T]), Res) :-
  factortrans(F, FT),
  termtrans2(t(F,T), TT),
  foldl(append, [TT, [(e,OP)], FT], [], Res).

% factortrans DONE
factortrans(fi(Ident), [(e, load(ID))]) :- symtab(Ident, ID).
factortrans(fn(Number), [(e, lit(Number))]).
factortrans(fs(SimpleExp), Ret) :- simpleexptrans(SimpleExp, Ret).



%%%%%%%%%%%%%%%%%%% Hier wird sich ne Tonne an Hilfsprädikaten ansammeln...

% zippe die Liste derart: [a,b,c] -> [(a,1),(b,2),(c,3)] wobei man den Start (hier 1) angeben kann
zipN([], _, []).
zipN([H|T], N, [(H,N)|TT]) :- N1 is N+1, zipN(T, N1, TT).

elem2endlessList(E, [E|T]) :- elem2endlessList(E, T).

% was soll man dazu noch erklären
zip([], [], []).
zip([X|Xs], [Y|Ys], [(X,Y)|Zs]) :- zip(Xs,Ys,Zs).
zip3([], [], [], []).
zip3([X|Xs], [Y|Ys], [Z|Zs], [(X,Y,Z)|As]) :- zip3(Xs,Ys,Zs,As).
% zipE: second argument is not a list but a constant parameter which becomes the second part of all tuples
zipE([], _, []).
zipE([X|Xs], Y, [(X,Y)|Zs]) :- zipE(Xs,Y,Zs).

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


