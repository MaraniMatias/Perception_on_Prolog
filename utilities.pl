% TODO:https://github.com/CodingTrain/Toy-Neural-Network-JS/blob/master/lib/nn.js
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
step(X, _, 1) :-
  X > 0.
step(_, _, 0). % 0 or -1

% Relu
relu(X, _, X) :-
  X > 0.
relu(_, _, 0).

% Sigmiod
sigmoid(X, _, Y) :-
  Y is 1 rdiv (1 + e**(-X)).
d_sigmoid(Y, _, Rta) :-
  Rta is Y * (1 - Y).

perceptron(_, _, [], _) :-
  fail.
% perceptron init weight
perceptron(Name, Afun, X, Rta) :-
  weight(Name, [], synaptic),
  length_list(X, LenX),
  random_list(LenX, W),
  save_weight(Name, W, synaptic),
  random(B),
  save_weight(Name, B, bias),
  perceptron(Name, Afun, X, Rta).
% perception
perceptron(Name, Afun, X, Rta) :-
  weight(Name, W, synaptic),
  produc_dot(X, W, Mrta),
  weight(Name, B, bias),
  MB is Mrta + B,
  foldl(Afun, [MB], 0, Rta).

% Add the elements of the lists that are in
% the same position and create another list
sum_ele_by_ele_in_list([], [], []).
sum_ele_by_ele_in_list([H1|T1], [H2|T2], [H|Rta]) :-
  H is H1 + H2,
  sum_ele_by_ele_in_list(T1, T2, Rta).

% Multiply the elements of the lists that are
% in the same position and create another list
matrix_multiply([], [], []).
matrix_multiply([H1|T1], [H2|T2], [H|Rta]) :-
  H is H1 * H2,
  matrix_multiply(T1, T2, Rta).

% Add a value to the items in the list
add_value_to_items_list([], _, []).
add_value_to_items_list([H1|T1], Val, [H|T]) :-
  add_value_to_items_list(T1, Val, T),
  H is H1 + Val.

make_list_of_ele(0, _, []).
make_list_of_ele(C, Err, Rta) :-
  C > 0,
  C1 is C - 1,
  learning_rate(LR),
  H is Err * LR,
  Rta = [H|T],
  make_list_of_ele(C1, Err, T).

calculate_gradient(_, [], []).
calculate_gradient(Dfun, [Htarget|Ttarget], [H|T]) :-
  calculate_gradient(Dfun, Ttarget, T),
  foldl(Dfun, [Htarget], 0, H).

adjust_weights([], _).
adjust_weights([Perceptron|PT], Delta) :-
  weight(Perceptron, W, synaptic),
  sum_ele_by_ele_in_list(W, Delta, NewW)
  save_weight(Perceptron, NewW, synaptic),
  % FIXME
  % weight(Perceptron, bias, B),
  % learning_rate(LR),
  % NewB is B + LR * Err,
  % save_weight(bias, NewB),
  adjust_weights(PT, Delta).

%  Hidden layers and Ouputs layers
backpropagation([Hidden|Outputs], Inputs, Targets, Outputs_err) :-
  % Generating the Hidden Outputs
  calculate_gradients(d_sigmoid, Targets, Gradients1),
  learning_rate(LR),
  % Make list Elist is [Err*LR,Err*LR,...]
  add_value_to_items_list(Gradients1, LR, Gradients2),
  % Make list XElist is [X1*Err,X2*Err,...]
  matrix_multiply(Gradients2, Outputs_err, Gradients),
  % Calculate deltas
  matrix_multiply(Gradients, Hidden, Weight_ho_deltas),
  %  Adjust the weights by deltas
  adjust_weights(Hidden, Weight_ho_deltas),


