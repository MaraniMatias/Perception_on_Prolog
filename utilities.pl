:- dynamic totalEpoch/1. % For epoch info
:- dynamic weight/3.
:- dynamic error/1.
:- dynamic data/2.
:- dynamic loss/1.
loss(inf).
error(0).
% totalEpoch(0).

% Retract and Asserta
save_err(Val) :-
  % writeln(['E: ',Val]),
  retract(error(_)),
  asserta(error(Val)).

save_weight(P, Val, bias) :-
  % writeln([P, 'B: ',Val]),
  retract(weight(P, _, bias)),
  asserta(weight(P, Val, bias)).
save_weight(P, Val, bias) :-
  asserta(weight(P, Val, bias)).

save_weight(P, Val, synaptic) :-
  % writeln([P, 'W: ',Val]),
  retract(weight(P, _, synaptic)),
  asserta(weight(P, Val, synaptic)).
save_weight(P, Val, synaptic) :-
  asserta(weight(P, Val, synaptic)).

save_loss(Val) :-
  % writeln(['loss: ',Val]),
  retract(loss(_)),
  asserta(loss(Val)).

save_to_file(Fact) :-
  tell('./weight.data'),
  listing(Fact),
  told.

% Re-set weight values
clenaer([]) :-
  retractall(data(_, _)),
  retract(totalEpoch(_)),
  save_err(0),
  save_loss(inf).
clenaer([P|T]) :-
  save_weight(P, [], synaptic),
  save_weight(P, 0, bias),
  clenaer(T).

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
calc_loss(Err) :-
  error(SE),
  SErr is SE + Err * Err,
  save_err(SErr),
  data_length(Count),
  Loss is SErr rdiv Count,
  save_loss(Loss).

pow(_, 0, 1).
pow(X, Y, Z) :-
  Y1 is Y - 1,
  pow(X, Y1, Z1),
  Z is Z1 * X.

% activation function
% https://en.wikipedia.org/wiki/Activation_function

% Step
step(X, 1) :-
  X > 0.
step(_, 0). % 0 or -1

% Relu
relu(X, X) :-
  X > 0.
relu(_, 0).

% Sigmiod
sigmoid(X, Y) :-
  Y is 1 rdiv (1 + e**(-X)).

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
  % relu(MB, Rta).
  % sigmoid(MB, Rta).

% adjust
adjust_weights(Name, XElist) :-
  weight(Name, W, synaptic),
  sum_ele_by_ele_in_list(W, XElist, NewW),
  % writeln(W),  writeln(NewW),
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

% Ajusta pesos de una lista de perceptron
adjust_net_weights([], _, _).
adjust_net_weights([Perceptron|T], X, Err) :-
  length_list(X, LenX),
  % Make list Elist is [Err*LR,Err*LR,...]
  make_list_of_ele(LenX, Err, Elist),
  % Make list XElist is [X1*Err,X2*Err,...]
  multi_ele_to_list(X, Elist, XElist),
  adjust_weights(Perceptron, XElist),
  weight(Perceptron, B, bias),
  NewB is B + Err,
  save_weight(Perceptron, NewB, bias),
  adjust_net_weights(T, X, Err).
