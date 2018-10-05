:- dynamic(data_length/1).
:- dynamic(totalEpoch/1). % For epoch info
:- dynamic(weight/2).
:- dynamic(error/1).
:- dynamic(data/2).

% data([], label).
openDataSet :-
  retractall(data(_,_)),
  retract(data_length(_)),
% consult('./database/and.pl'),
% consult('./database/or.pl'),
% consult('./database/par.pl'),
  consult('./database/impar.pl'),
% consult('./database/mayor_5.pl'),
% consult('./database/xor.pl'),
  aggregate_all(count, data(_,_), Count),
  asserta(data_length(Count)).

data_length(0).
learning_rate(0.5).
weight(synaptic, []).
weight(bias, 1).
error(0).

% Retract and Asserta
save_err(Val) :-
  % writeln(['E: ',Val]),
  retract(error(_)),
  asserta(error(Val)).
save_weight(Type, Val) :-
  % writeln([Type,': ',Val]),
  retract(weight(Type, _)),
  asserta(weight(Type, Val)).

save_to_file(Fact) :-
  tell('./weight.data'),
  listing(Fact),
  told.

% Re-set weight values
clenaer() :-
  retractall(data(_,_)),
  retract(totalEpoch(_)),
  save_weight(synaptic, []),
  save_weight(bias, 1),
  save_err(0).

% Random List
random_list(0, []).
random_list(C, [H|T]) :-
  C > 0,
  C1 is C-1,
  random(H),
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
  error(SE),
  SErr is SE + Err * Err,
  save_err(SErr),
  data_length(Count),
  Loss is SErr rdiv Count.

% Funcion de activacion
% https://en.wikipedia.org/wiki/Activation_function
% Step
step(X, 1) :-
  X > 0.
step(_, 0). % 0 or -1

perceptron([], _) :-
  fail.
% perceptron init weight
perceptron(X, Rta) :-
  weight(synaptic, []),
  length_list(X, LenX),
  random_list(LenX, W),
  save_weight(synaptic, W),
  perceptron(X, Rta).
% perceptron
perceptron(X, Rta) :-
  weight(synaptic, W),
  produc_dot(X, W, Mrta),
  weight(bias, B),
  MB is Mrta + B,
  step(MB, Rta).

% adjust
% Elist is [Err*LR,Err*LR,...]
adjust_weights(Elist) :-
  weight(synaptic, W),
  sum_ele_by_ele_in_list(W, Elist, NewW),
  save_weight(synaptic, NewW).

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

make_list_of_ele(0, _, []).
make_list_of_ele(C, Err, [H|T]) :-
  C > 0,
  C1 is C - 1,
  learning_rate(LR),
  H is Err * LR,
  make_list_of_ele(C1, Err, T).

% info only if epoch change
info(_, _) :-
  data(_, _).
info(Epoch, Loss) :-
  totalEpoch(TotalEpoch),
  weight(synaptic, W),
  weight(bias, B),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w ---
  Weights: ~w
  Bias: ~w
  Loss: ~4f
  ~n', [Run, W, B, Loss]).
% Run one times
info(Epoch, Loss) :-
  asserta(totalEpoch(Epoch)),
  info(Epoch, Loss).

% epoch
epoch(-1) :-
  save_to_file(weight),
  clenaer().
% loop by data
epoch(Epoch) :-
  Epoch >= 0,

  data(X, Label),
  retract(data(X, Label)),
  perceptron(X, P1),
  writeln(['Real:', Label,'Predic:',P1]),
  Err is Label - P1,

  length_list(X, LenX),
  % Make list Elist is [Err*LR,Err*LR,...]
  make_list_of_ele(LenX, Err, Elist),
  % Make list XElist is [X1*Err,X2*Err,...]
  multi_ele_to_list(X, Elist, XElist),
  adjust_weights(XElist),

  % Logic AND need this code
  % weight(bias, B),
  % NewB is B + Err,
  % save_weight(bias, NewB),

  calc_loss(Err, Loss), % Loss or MSE
  info(Epoch, Loss),
  epoch(Epoch).
% loop by Epoch
epoch(Epoch) :-
  Epoch >= 0,
  openDataSet,
  NextEpoch is Epoch - 1,
  save_err(0),
  epoch(NextEpoch).
