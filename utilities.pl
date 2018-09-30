% Retract and Asserta
updata_err(E, Val) :-
  % writeln([E,Val]),
  retract(error(E, _)),
  asserta(error(E, Val)).
updata_weight(W, Val, bias) :-
  % writeln([b,Val]),
  retract(weight(W, _, bias)),
  asserta(weight(W, Val, bias)).
updata_weight(W, Val, synaptic) :-
  % writeln([W,Val]),
  retract(weight(W, _, synaptic)),
  asserta(weight(W, Val, synaptic)).

% Re-set weight values
clenaer() :-
  retract(totalEpoch(_)),
  updata_weight(p1, [], synaptic),
  updata_weight(p1, inf, bias),
  updata_err(e1, inf).

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
