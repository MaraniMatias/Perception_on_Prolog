:- dynamic(totalEpoch/1). % For epoch info
:- dynamic(weight/3).
:- dynamic(error/2).
:- dynamic(data/2).

% data([], label).
openDataSet :-
  retractall(data(_, _)),
% consult('./database_par_min.pl').
%  consult('./database_and.pl').
% consult('./database_par.pl').
% consult('./database_impar.pl').
% consult('./database_mayor_5.pl').
consult('./database_xor.pl').
:- openDataSet.

weight(p1, [], synaptic).
weight(p2, [], synaptic).
weight(p3, [], synaptic).
weight(p1, 0, bias).
weight(p2, 0, bias).
weight(p3, 0, bias).
error(e1, 0).

% Retract and Asserta
updata_err(E, Val) :-
  retract(error(E, _)),
  asserta(error(E, Val)).
updata_weight(W, Val, bias) :-
  retract(weight(W, _, bias)),
  asserta(weight(W, Val, bias)).
updata_weight(W, Val, synaptic) :-
  retract(weight(W, _, synaptic)),
  asserta(weight(W, Val, synaptic)).

% Re-set weight values
clenaer() :-
  retract(totalEpoch(_)),
  updata_weight(p1, [], synaptic),
  updata_weight(p1, 0, bias),
  updata_weight(p2, [], synaptic),
  updata_weight(p2, 0, bias),
  updata_weight(p3, [], synaptic),
  updata_weight(p3, 0, bias),
  updata_err(e1, 0).

% Random List
random_list(0, []).
random_list(C, Rta) :-
  C > 0,
  C1 is C-1,
  random(H),
  Rta = [H|T],
  random_list(C1, T).

% Longitud de una lista
length_list([], 0).
length_list([_|T], Length) :-
  length_list(T, S1),
  Length is S1 + 1.

% Producto punto para dos vectores
produc_dot([], [], 0).
produc_dot([H1|T1], [H2|T2], Rta) :-
 produc_dot(T1, T2, Rta1),
 Rta is Rta1 + H1 * H2.

% Funcion de activacion
% https://en.wikipedia.org/wiki/Activation_function
% Step
step(X, 1) :-
  X > 0.
step(_, 0). % 0 or -1

% perception
perception(Name, X, Rta) :-
  X \= [],
  weight(Name, W, synaptic),
  W \= [],
  produc_dot(X, W, Mrta),
  weight(Name, B, bias),
  MB is Mrta + B,
  step(MB, Rta).
perception(Name, X, Rta) :-
  X \= [],
  length_list(X, LenX),
  random_list(LenX, W),
  updata_weight(Name, W, synaptic),
  random(B),
  updata_weight(Name, B, bias),
  perception(Name, X, Rta).

% adjust
adjust_weights(Name, Err) :-
  weight(Name, W, synaptic),
  adjust_weights(W, Err, NewW),
  updata_weight(Name, NewW, synaptic),
  weight(Name, B, bias),
  NewB is B + Err,
  updata_weight(Name, NewB, bias).

adjust_weights([], _, []).
adjust_weights([Hw|Tw], Err, [H|T]) :-
  adjust_weights(Tw, Err, T),
  H is Hw + Err.

% epoch
info(Epoch) :-
  totalEpoch(TotalEpoch),
  weight(p1, W1, synaptic),
  weight(p2, W2, synaptic),
  weight(p3, W3, synaptic),
  weight(p1, B1, bias),
  weight(p2, B2, bias),
  weight(p3, B3, bias),
  error(e1, Error),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w --- Error: ~w
  Weights of perception 1: ~w
  Bias: ~w
  Weights of perception 2: ~w
  Bias: ~w
  Weights of perception 3: ~w
  Bias: ~w
  ', [Run, Error, W1, B1, W2, B2, W3, B3]).

info(Epoch) :-
  TotalEpoch is Epoch,
  asserta(totalEpoch(TotalEpoch)),
  info(Epoch).

epoch(0) :-
  clenaer().
% loop by data
epoch(Epoch) :-
  Epoch \= 0,
  data(X, Label),
  retractall(data(X, Label)),
  perception(p1, X, P1),
  perception(p2, X, P2),
  perception(p3, [P1, P2], P3),
  Err is Label - P3,
  adjust_weights(p1, Err),
  adjust_weights(p2, Err),
  adjust_weights(p3, Err),
  info(Epoch),
  updata_err(e1, Err),
  epoch(Epoch).
% loop by Epoch
epoch(Epoch) :-
  Epoch \= 0,
  openDataSet,
  NextEpoch is Epoch - 1,
  epoch(NextEpoch).
