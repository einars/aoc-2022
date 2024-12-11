#!/usr/bin/env -S swipl -O

:- table stone/3.

split_even_digits(N, H1, H2) :-
  number_chars(N, Digits),
  length(Digits, Len),
  Len mod 2 =:= 0,
  Half is Len // 2,
  length(NH1, Half),
  append(NH1, NH2, Digits),
  number_chars(H1, NH1),
  number_chars(H2, NH2).


stone(_, 0, 1) :- !.

stone(0, G, X) :-
  G > 0,
  NG is G-1,
  /* format('NG1 0 ~a ~n', G), */
  stone(1, NG, X).

stone(N, G, X) :-
  N > 0,
  G > 0,
  split_even_digits(N, H1, H2),
  NG is G-1,
  /* format('NG2 ~a ~a > [~a ~a]~n', [N, G, H1, H2]), */
  stone(H1, NG, NX1),
  stone(H2, NG, NX2),
  X is NX1+NX2.

stone(N, G, X) :-
  N > 0,
  G > 0,
  \+ split_even_digits(N, _, _),
  NG is G-1,
  NN is 2024*N,
  /* format('NG3 ~a ~a -> ~a~n', [N, G, NN]), */
  stone(NN, NG, X).

pt1(N, Res) :-
  stone(N, 25, Res).

pt2(N, Res) :-
  stone(N, 75, Res).

main(_) :-
  maplist(pt1, [1117, 0, 8, 21078, 2389032, 142881, 93, 385], N1),
  foldl(plus, N1, 0, Result1),
  format('N=~a~n', Result1),

  maplist(pt2, [1117, 0, 8, 21078, 2389032, 142881, 93, 385], N2),
  foldl(plus, N2, 0, Result2),
  format('N=~a~n', Result2).
  
