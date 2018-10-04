:- dynamic(totalEpoch/1). % For epoch info
:- dynamic(weight/3).
:- dynamic(error/2).
:- dynamic(data/2).

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
  retractall(data(_,_)),
  retract(totalEpoch(_)),
  save_weight(p1, [], synaptic),
  save_weight(p1, 0, bias),
  save_err(e1, 0).

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

% Calculate the mean square error
calc_loss(Err, Loss) :-
  error(e1, SE),
  SErr is SE + Err * Err,
  save_err(e1, SErr),
  data_length(Count),
  Loss is SErr rdiv Count.

% Funcion de activacion
% https://en.wikipedia.org/wiki/Activation_function
% Step
step(X, 1) :-
  X > 0.
step(_, 0). % 0 or -1

perceptron(_, [], _) :-
  fail.
% perceptron init weight
perceptron(Name, X, Rta) :-
  weight(Name, [], synaptic),
  length_list(X, LenX),
  random_list(LenX, W),
  save_weight(Name, W, synaptic),
  random(B),
  save_weight(Name, B, bias),
  perceptron(Name, X, Rta).
% perception
perceptron(Name, X, Rta) :-
  weight(Name, W, synaptic),
  produc_dot(X, W, Mrta),
  weight(Name, B, bias),
  MB is Mrta + B,
  step(MB, Rta).

% adjust
adjust_weights(Name, XElist) :-
  weight(Name, W, synaptic),
  sum_ele_by_ele_in_list(W, XElist, NewW),
  save_weight(Name, NewW, synaptic).

% Add the elements of the lists that are in
% the same position and create another list
sum_ele_by_ele_in_list([], [], []).
sum_ele_by_ele_in_list([H1|T1], [H2|T2], [H|Rta]) :-
  H is H1 + H2,
  sum_ele_by_ele_in_list(T1, T2, Rta).

% Multiply the elements of the lists that are
% in the same position and create another list
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
  learning_rate(LR),
  H is Err * LR,
  Rta = [H|T],
  make_list_of_ele(C1, Err, T).
