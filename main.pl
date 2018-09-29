% data([], label).
% :- ['./database_par.pl'].
% :- ['./database_impar.pl'].
:- ['./database_mayor_5.pl'].
% :- ['./database_xor.pl'].
:- dynamic(totalEpoch/1). % For epoch info
:- dynamic(weight/3).
:- dynamic(error/2).

weight(p1, [], synaptic).
weight(p1, 0, bias).
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
step(X,1) :-
  X > 0.
step(_,0). % 0 or -1

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
adjust_weights(GetW, Err) :-
  weight(GetW, W, synaptic),
  adjust_weights(W, Err, [], NewW),
  updata_weight(GetW, NewW, synaptic),
  weight(GetW, B1, bias),
  NewB is B1 + Err,
  updata_weight(GetW, NewB, bias).

adjust_weights([], _, Rta, Rta).
adjust_weights([Hw|Tw], Err, T, Rta) :-
  H is Hw + Err,
  adjust_weights(Tw, Err, [H|T], Rta).

% epoch
info(Epoch) :-
  totalEpoch(TotalEpoch),
  weight(p1, W1, synaptic),
  error(e1, Error),

  Run is TotalEpoch - Epoch + 2,
  format('
  --- Summary  Epoch ~w ---
  Weights: ~w
  Error: ~w
  ', [Run, W1, Error]).

info(Epoch) :-
  TotalEpoch is Epoch - 1,
  asserta(totalEpoch(TotalEpoch)),
  info(Epoch).

epoch(1) :-
  info(1),
  clenaer().

epoch(Epoch) :-
  Epoch \= 1,
  data(X, Label),
  perception(p1, X, P1),
  Err is Label - P1,
  adjust_weights(p1, Err),
  NextEpoch is Epoch - 1,
  updata_err(e1, Err),
  info(Epoch),
  epoch(NextEpoch).
