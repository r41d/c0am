:- module(c0am, [c0am/0]).

:- use_module(library(pio)).
:- use_module(parser).
:- use_module(trans).
:- use_module(format).

c0am() :-

  phrase_from_file(c0parser(Code), 'simplesample.c0'), !,

  write("c0parse:\n"), !,
  write(Code),
  %print_term(Code, []), % pretty printing

  c0trans(Code, Commands),
  write("\nc0trans:\n"),
  write(Commands),
  %print_term(Commands, []), % pretty printing

  c0format(Commands, Final),
  write("\nc0format:\n"),
  write(Final),

  true.


c0amdebug(File) :-
  phrase_from_file(c0parser(Code), File), !, write("c0parse:\n"), write(Code),
  c0trans(Code, Commands), write("\nc0trans:\n"), write(Commands),
  c0format(Commands, Final), write("\nc0format:\n"), write(Final).


c0am(File) :-
  phrase_from_file(c0parser(Code), File), !,
  c0trans(Code, Commands),
  c0format(Commands, Final),
  write(Final).

