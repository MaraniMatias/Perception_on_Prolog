:- dynamic(totalEpoch/1). % For epoch info
:- dynamic(weight/3).
:- dynamic(error/2).
:- dynamic(data/2).
:- dynamic(loss/1).

% Retract and Asserta
save_err(E, Val) :-
  % writeln(['E: ',Val]),
  retract(error(E, _)),
  asserta(error(E, Val)).
save_weight(W, Val, bias) :-
  % writeln(['B: ',Val]),
  retract(weight(W, _, bias)),
  asserta(weight(W, Val, bias)).
save_weight(W, Val, synaptic) :-
  % writeln(['W: ',Val]),
  retract(weight(W, _, synaptic)),
  asserta(weight(W, Val, synaptic)).

% Re-set weight values
clenaer() :-
  retract(totalEpoch(_)),
  save_weight(p1, [], synaptic),
  save_weight(p1, inf, bias),
  save_err(e1, 0),
  retract(loss(_)),
  asserta(loss(inf)).

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
% perception init weight
perception(Name, X, Rta) :-
  X \= [],
  length_list(X, LenX),
  random_list(LenX, W),
  save_weight(Name, W, synaptic),
  random(B),
  save_weight(Name, B, bias),
  perception(Name, X, Rta).

% adjust
adjust_weights(Name, XElist) :-
  weight(Name, W, synaptic),
  sum_ele_by_ele_in_list(W, XElist, NewW),
  save_weight(Name, NewW, synaptic).

% Multipluca elemento por elemento.
sum_ele_by_ele_in_list([], [], []).
sum_ele_by_ele_in_list([H1|T1], [H2|T2], [H|Rta]) :-
  H is H1 + H2,
  sum_ele_by_ele_in_list(T1, T2, Rta).

% Multipluca elemento por elemento.
multi_ele_to_list([], [], []).
multi_ele_to_list([H1|T1], [H2|T2], [H|Rta]) :-
  H is H1 * H2,
  multi_ele_to_list(T1, T2, Rta).

% Add a value to the items in the list
% add_value_to_items_list([], _, []).
% add_value_to_items_list([Hw|Tw], Err, [H|T]) :-
%   add_value_to_items_list(Tw, Err, T),
%   H is Hw + Err.

make_list_of_ele(0, _, []).
make_list_of_ele(C, Err, Rta) :-
  C > 0,
  C1 is C - 1,
  learning_rate(Lr),
  H is Lr * Err,
  Rta = [H|T],
  make_list_of_ele(C1, Err, T).

calc_loss(Name) :-
  error(Name, E),
  data(X, _),
  length_list(X, LenX),
  Loss is E div LenX,
  retract(loss(_)),
  asserta(loss(Loss)).
