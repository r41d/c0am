:- module(tests, []).

:- use_module(library(pio)).
:- use_module(library(readutil)). % read_file_to_codes
:- use_module(parser).
:- use_module(trans).
:- use_module(format).
:- use_module(helper).


doAllTheC0AMTests(_) :-
  forall(
    member(T, ["47", "48", "49", "50", "vorlesung", "simplesample"]),
    (testC0AM(T) -> write(T),write(" SUCCESS\n") ; write(T),write(" FAIL\n"))
  ).


testC0AM(Name) :-
  meltStr(["../c0samples/", Name, ".c0"], C0file),
  meltStr(["../c0samples/", Name, ".am"], AMfile),
  performC0AM(C0file, AMcode), !,
  string_concat(AMcode, "\n", AMcode2),
  read_file_to_codes(AMfile, AMcodes, []),
  string_codes(AMstr, AMcodes),
  AMcode2 == AMstr. % -> write("EQUAL") ; write("NOT EQUAL").



performC0AM(File, Final) :-
  phrase_from_file(c0parser(Code), File), !,
  c0trans(Code, Commands),
  c0format(Commands, Final).



