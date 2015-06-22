:- module(format, [c0format/2]).
:- use_module(helper).

:- dynamic double/2. % determined identical Counters
:- dynamic lin/2. % linear commands


%%%%%%%%%%%%%%%%%%% Export
c0format(C,F) :-
  retractall(double(_,_)), % important
  doubles(C), % important
  unifyCounters(C,U),
  eraseNOPs(U, E),
  adjustJumps(E, A),
  finalize(A, F).


%prog2am :: Program -> String
%prog2am = finalize . adjustJumps . eraseNOPs . unifyCounters . trans


% determines identical Counters
doubles([]).
doubles([(A1,nop),(B1,nop)|Cs]) :-
  assert(double(A1,B1)),
  doubles([(B1,nop)|Cs]).
doubles([_|Cs]) :-
  doubles(Cs).

% unify duplicate counters
exchangeCounter( (C,Cmd)     , (CC,Cmd)        ) :- double(C, CC).
exchangeCounter( (C,jmp(Jmp)), (C,jmp(NewJmp)) ) :- double(Jmp, NewJmp).
exchangeCounter( (C,jmc(Jmp)), (C,jmc(NewJmp)) ) :- double(Jmp, NewJmp).
exchangeCounter( (C,jmp(C))  , (CC,jmp(CC))    ) :- double(C, CC).
exchangeCounter( (C,jmc(C))  , (CC,jmc(CC))    ) :- double(C, CC).
exchangeCounter(Cmd, Cmd).

% apply exchangeCounter on a whole list of Commands
unifyCounters(Cmds, UnifiedCmds) :- % dammit, why Singleton ?!?!?
  maplist(exchangeCounter, Cmds, Exchanged),
    maplist(exchangeCounter, Exchanged, Ex2), Ex2=Exchanged
    -> UnifiedCmds = Exchanged ; unifyCounters(Exchanged, UnifiedCmds).


% erase NOPs, shift the address to the next command
eraseNOPs([], []).
eraseNOPs([X], [X]).
eraseNOPs([(CntA,nop),(_CntB,CmdB)|Xs], Ys) :-
  !,
  eraseNOPs([(CntA,CmdB)|Xs], Ys).
eraseNOPs([X|Xs], [X|Ys]) :-
  %CmdA \= nop % not necessary because of cut in the above predicate
  eraseNOPs(Xs, Ys).


genLinList(Cmds) :- genLinList(Cmds, 1).
genLinList([], _N).
genLinList([(e,_Cmd)|Rs], N) :-
  !,
  N1 is N+1,
  genLinList(Rs, N1).
genLinList([(Cnt,_Cmd)|Rs], N) :-
  %Cnt \= e, % not needed because of cut in upper predicate
  assert(lin(Cnt,N)),
  N1 is N+1,
  genLinList(Rs, N1).


adjustJumps(Cmds, ResCmds) :-
  retractall(lin(_,_)), % important
  genLinList(Cmds),
  maplist(adjustJump, Cmds, ResCmds).
adjustJump( (Cnt,jmp(Jmp)), (Cnt,jmp(JmpLin)) ) :- lin(Jmp,JmpLin), !.
adjustJump( (Cnt,jmc(Jmc)), (Cnt,jmc(JmcLin)) ) :- lin(Jmc,JmcLin), !.
adjustJump( R             , R                 ).


finalize(Cmds, Final) :-
  maplist(snd, Cmds, Snds),
  zip1L(Snds, 1, Zips),
  maplist(lin2str, Zips, Lines),
  intersperse("\n", Lines, WithNL),
  meltStr(WithNL, Final).

lin2str((Int,Cmd), StrCmd) :-
  atom_number(N, Int),
  cmd2str(Cmd, C),
  meltStr([N, ": ", C, ";"], StrCmd).

cmd2str(read(I) , Res) :- atom_number(N, I), meltStr(["READ ",  N], Res).
cmd2str(write(I), Res) :- atom_number(N, I), meltStr(["WRITE ", N], Res).
cmd2str(load(I) , Res) :- atom_number(N, I), meltStr(["LOAD ",  N], Res).
cmd2str(store(I), Res) :- atom_number(N, I), meltStr(["STORE ", N], Res).
cmd2str(lit(I)  , Res) :- atom_number(N, I), meltStr(["LIT ",   N], Res).
cmd2str(addOP, "ADD").
cmd2str(mulOP, "MUL").
cmd2str(subOP, "SUB").
cmd2str(divOP, "DIV").
cmd2str(modOP, "MOD").
cmd2str(lt, "LT").
cmd2str(eq, "EQ").
cmd2str(ne, "NE").
cmd2str(gt, "GT").
cmd2str(le, "LE").
cmd2str(ge, "GE").
cmd2str(jmp(C), Res) :- atom_number(N, C), meltStr(["JMP ", N], Res).
cmd2str(jmc(C), Res) :- atom_number(N, C), meltStr(["JMC ", N], Res).


