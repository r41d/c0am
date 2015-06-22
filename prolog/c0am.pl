:- module(c0am, [c0am/2]).

:- use_module(library(pio)).
:- use_module(parser).
:- use_module(trans).
:- use_module(format).


c0amSample :-
  c0amdebug('../c0samples/simplesample.c0').

c0amdebug(File) :-
  phrase_from_file(c0parser(Code), File), !,
  write("c0parse:\n"),
  write(Code),
  %print_term(Code, []), % pretty printing

  c0trans(Code, Commands),
  write("\nc0trans:\n"), write(Commands),
  %print_term(Commands, []), % pretty printing

  c0format(Commands, Final),
  write("\nc0format:\n"),
  write(Final).

c0am(File, Final) :-
  phrase_from_file(c0parser(Code), File), !,
  c0trans(Code, Commands),
  c0format(Commands, Final).

