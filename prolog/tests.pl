:- module(tests, [doAllTheC0AMTests/0]).

:- use_module(library(pio)).
:- use_module(library(readutil)). % read_file_to_codes

:- use_module(c0am).
:- use_module(helper).


doAllTheC0AMTests :-
  forall(
    member(T, ["47", "48", "49", "50", "vorlesung", "simplesample"]),
    (testC0AM(T) -> write(T),write(" SUCCESS\n") ; write(T),write(" FAIL\n"))
  ).

testC0AM(Name) :-
  meltStr(["../c0samples/", Name, ".c0"], C0file),
  meltStr(["../c0samples/", Name, ".am"], AMfile),
  c0am(C0file, AMcode), !,
  string_concat(AMcode, "\n", AMcode2),
  read_file_to_codes(AMfile, AMcodes, []),
  string_codes(AMstr, AMcodes),
  AMcode2 == AMstr. % -> write("EQUAL") ; write("NOT EQUAL").

