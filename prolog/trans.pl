:- module(trans, [c0trans/2]).
:- use_module(helper).

:- dynamic symtab/2. % symbol table is being held globally


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
  melt([ BE, [(e,jmc(A))], ST, [(A,nop)] ], CmdList).
sttrans(si(i(BoolExp,Stmt,StmtElse)), A, CmdList) :-
  boolexptrans(BoolExp, BE),
  nxt(A,1,C1),  sttrans(Stmt, C1, ST),
  nxt(A,2,C2),  sttrans(StmtElse, C2, STE),
  nxt(A,3,C3),
  melt( [ BE, [(e,jmc(A))], ST, [(e,jmp(C3)), (A,nop)], STE, [(C3,nop)] ], CmdList).
sttrans(sw(w(Exp,Stmt)), A, CmdList) :-
  boolexptrans(Exp, E),
  nxt(A,1,C1),  sttrans(Stmt, C1, S),
  nxt(A,2,C2),
  melt([ [(C2,nop)], E, [(e,jmc(A))], S, [(e,jmp(C2)), (A,nop)] ], CmdList).
sttrans(sss(SSS), A, CmdList) :- stseqtrans(SSS, A, CmdList).

% boolexptrans DONE
boolexptrans(bool(SimpleExp1, Rel, SimpleExp2), CmdList) :-
  simpleexptrans(SimpleExp1, SE1),
  simpleexptrans(SimpleExp2, SE2),
  melt([SE1, SE2, [(e,Rel)]], CmdList).

% simpleexptrans DONE
simpleexptrans(simple(Term, []), Res) :- termtrans(Term, Res).
simpleexptrans(simple(Term1, [(OP,Term2)|Tail]), Res) :-
  termtrans(Term1, T1),
  termtrans(Term2, T2),
  simpleexptrans2(simplemore(Tail), SE2),
  melt([T1, T2, [(e,OP)], SE2], Res).
simpleexptrans2(simplemore([]), []).
simpleexptrans2(simplemore([(OP,Term)|Tail]), Res) :-
  termtrans(Term, T),
  simpleexptrans2(simplemore(Tail), SE),
  melt([T, [(e,OP)], SE], Res).

% termtrans DONE
termtrans(t(Factor, []), Res) :- factortrans(Factor, Res).
termtrans(t(F1, [(OP,F2)|MoreFactors]), Res) :-
  factortrans(F1, F1T),
  factortrans(F2, F2T),
  termtrans2(t(F2, MoreFactors), TT),
  melt([F1T, F2T, [(e, OP)], TT], Res).
termtrans2(t(_,[]), []).
termtrans2(t(_,[(OP,F)|T]), Res) :-
  factortrans(F, FT),
  termtrans2(t(F,T), TT),
  melt([FT, [(e,OP)], TT], Res).

% factortrans DONE
factortrans(fi(Ident), [(e, load(ID))]) :- symtab(Ident, ID).
factortrans(fn(Number), [(e, lit(Number))]).
factortrans(fs(SimpleExp), Ret) :- simpleexptrans(SimpleExp, Ret).

