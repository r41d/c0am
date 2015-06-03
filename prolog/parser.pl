

use_module(library(pio)).


runnn() :-
  %phrase(DcgBody, List)
  phrase_from_file(pProgram(Code), 'simplesample.c0'), !,
  %write(Code),
  print_term(Code, []). % pretty printing


%%%%%%%%%%%%%%%%%%% ws
ws --> [] ; " ", ws ; "\t", ws ; "\n", ws.
%wsOrCmts --> " ", wsOrCmts
cmt --> "//", anything, "\n".
cmt --> "/*", anything, "*/".


%%%%%%%%%%%%%%%%%%% Program
%pProgram(p(Block)) --> anything.
pProgram(p(Block)) --> % {print("parsing Program...\n")},
  ws, "#include", ws, "<", ws, "stdio.h", ws, ">", ws,
  "int", ws, "main", ws, "(", ws, ")", ws,
  pBlock(Block), ws.


%%%%%%%%%%%%%%%%%%% Block
%pBlock(b(VariableDeclaration,StatementSequence)) --> anything.
pBlock(b(VariableDeclaration,StatementSequence)) --> % {print("parsing Block...\n")},
  ws, "{",
  ws, (pVariableDeclaration(VariableDeclaration) ; []),
  ws, (pStatementSequence(StatementSequence) ; []),
  ws, "return 0;",
  ws, "}", ws.


%%%%%%%%%%%%%%%%%%% VariableDeclaration
%pVariableDeclaration(v(IdentList)) --> anything.
pVariableDeclaration(v(IdentList)) --> % {print("parsing VariableDeclaration...\n")},
  "int", ws,
  vardeclVars(IdentList), ws,
  ";".

	vardeclVars([I]) -->
	  pIdent(I). % TODO: assert(validIdent(I)) oder sowas in der Richtung
	  %pIdent(CharIdent), {string_codes(StringIdent,  CharIdent)}.
	vardeclVars([I|IS]) -->
	  pIdent(I), ws, ",", ws, vardeclVars(IS).
	  %pIdent(CharIdent), {string_codes(StringIdent,  CharIdent)}, ws, ",", ws, vardeclVars(IS).


%%%%%%%%%%%%%%%%%%% StatementSequence
%pStatementSequence(s([S|SS])) --> anything.
pStatementSequence(s([S|SS])) --> % {print("parsing StatementSequence...\n")},
  pStatement(S), ws,
  ([] ; pStatementSequence(SS)).


%%%%%%%%%%%%%%%%%%% Statement
%pStatement(x) --> anything.
pStatement(ss(Ident)) --> "scanf", ws, "(", ws, "\"%d\",", ws, "&", ws, pIdent(Ident), ws, ")", ws, ";".
pStatement(sp(Ident)) --> "printf", ws, "(", ws, "\"%d\",", ws, pIdent(Ident), ws, ")", ws, ";".
pStatement(sa(Assignment)) --> pAssignment(Assignment).
pStatement(si(IfStatement)) --> pIfStatement(IfStatement).
pStatement(sw(WhileStatement)) --> pWhileStatement(WhileStatement).
pStatement(sss(StatementSequence)) --> ws, "{", ws, pStatementSequence(StatementSequence), ws, "}", ws.


%%%%%%%%%%%%%%%%%%% Assignment
pAssignment(a(Ident,SimpleExp)) -->
  ws, pIdent(Ident), ws, "=",
  ws, pSimpleExpression(SimpleExp), ws, ";", ws.


%%%%%%%%%%%%%%%%%%% IfStatement
pIfStatement(i(BoolExp,Statement)) -->
  ws, "if", ws, "(", ws, pBoolExpression(BoolExp), ws, ")",
  ws, pStatement(Statement), ws.
pIfStatement(i(BoolExp,Statement,StatementElse)) --> 
  ws, "if", ws, "(", ws, pBoolExpression(BoolExp), ws, ")",
  ws, pStatement(Statement),
  ws, "else", ws, pStatement(StatementElse), ws.



%%%%%%%%%%%%%%%%%%% WhileStatement
pWhileStatement(w(BoolExp,Statement)) -->
  ws, "while", ws, "(", ws, pBoolExpression(BoolExp), ws, ")",
  ws, pStatement(Statement), ws.


%%%%%%%%%%%%%%%%%%% BoolExpression
pBoolExpression(bool(SimpleExpression1,Relation,SimpleExpression2)) -->
  ws, pSimpleExpression(SimpleExpression1),
  ws, pRelation(Relation),
  ws, pSimpleExpression(SimpleExpression2).


%%%%%%%%%%%%%%%%%%% Relation
pRelation(eq) --> "==".
pRelation(ne) --> "!=".
pRelation(lt) --> "<".
pRelation(gt) --> ">".
pRelation(le) --> "<=".
pRelation(ge) --> ">=".


%%%%%%%%%%%%%%%%%%% SimpleExpression
%pSimpleExpression(simple(Term, MoreTerms)) --> anything.
pSimpleExpression(simple(Term, MoreTerms)) --> % a la Seidel
  ws,
  pTerm(Term),
  pMoreTerms(MoreTerms).


%%%%%%%%%%%%%%%%%%% Term
pTerm(t(Factor, MoreFactors)) -->
  pFactor(Factor),
  pMoreFactors(MoreFactors).
pMoreTerms([]) --> [].
pMoreTerms([[Op,Term]|Tail]) -->
  pOpAddSub(Op),
  pTerm(Term),
  pMoreTerms(Tail).
pOpAddSub(addOP) --> ws, "+".
pOpAddSub(subOP) --> ws, "-".


%%%%%%%%%%%%%%%%%%% Factor
pFactor(fi(Ident)) --> ws, pIdent(Ident). % TODO: prüfe ob validIdent(Ident) gilt
pFactor(fn(Number)) --> ws, pNumber(Number).
pFactor(fs(SimpleExp)) --> ws, "(", ws, pSimpleExpression(SimpleExp), ws, ")", ws.
pMoreFactors([]) --> [].
pMoreFactors([[Op,Fac]|Tail]) -->
  pOpMulDivMod(Op), pFactor(Fac), pMoreFactors(Tail).
pOpMulDivMod(mulOP) --> ws, "*".
pOpMulDivMod(divOP) --> ws, "/".
pOpMulDivMod(modOP) --> ws, "%".


%%%%%%%%%%%%%%%%%%% Ident
pIdent(Ident) -->
  ident(Parsed), % parse
  { string_codes(Ident, Parsed) }. % convert to "string"
ident([H|T]) --> [H], { char_type(H, alpha) }, ident(T).
ident([H])   --> [H], { char_type(H, alpha) }.
%pIdent(I) --> {I =~ "[a-zA-Z_][0-9a-zA-Z_]*"}.
%pIdent(I) --> I, { maplist(is_alpha, I) }. % match alpha identifier
%pIdent(I) --> I, { I="m" }. % match alpha identifier
%pIdent(Id) --> {validID(Id)}, Id.
%validID(I) :- I =~ "[a-zA-Z_][0-9a-zA-Z_]*".
%validID([H]) :- char_type(H, alpha), !.
%validID([H|T]) :- char_type(H, alpha), validID(T).


%%%%%%%%%%%%%%%%%%% Number
%pNumber(Number) --> anything.
pNumber(Number) --> % This is extremely ugly, but hey, it works
  ws, num(Parsed), % parse
  {
    string_codes(StringNumber, Parsed), % convert to "string"
    atom_number(StringNumber, Number) % convert to number
  }.
num([H|T]) --> [H], { char_type(H, digit) }, num(T).
num([H])   --> [H], { char_type(H, digit) }.


%%%%%%%%%%%%%%%%%%% anything
anything --> [].
anything --> [_], anything.


