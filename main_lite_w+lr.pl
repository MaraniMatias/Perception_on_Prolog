% Make a log file
:- protocola('perceptron.log').

:- dynamic data_length/1.
:- dynamic totalEpoch/1. % For epoch info
:- dynamic weight/2.
:- dynamic error/1. % by epoch
:- dynamic data/2.
:- dynamic loss/2.

% data([], label).
openDataSet :-
  retractall(data(_, _)),
  retract(data_length(_)),
  consult('./database/and.pl'),
% consult('./database/or.pl'),
% consult('./database/par.pl'),
% consult('./database/impar.pl'),
% consult('./database/mayor_5.pl'),
% consult('./database/xor.pl'),
  aggregate_all(count, data(_,_), Count),
  asserta(data_length(Count)).

data_length(0).
learning_rate(0.5).
weight(synaptic, []).
weight(bias, 1).
error(0). % by epoch
loss(previous, inf). % by run
loss(now, inf). % by run

% Retract and Asserta
save_err(Val) :-
  % writeln(['E: ',Val]),
  retract(error(_)),
  asserta(error(Val)).
save_weight(Type, Val) :-
  % writeln(['Weight: ', Type,Val]),
  retract(weight(Type, _)),
  asserta(weight(Type, Val)).
save_loss(Type, Val) :-
  % writeln(['loss: ',Type,Val]),
  retract(loss(Type, _)),
  asserta(loss(Type, Val)).

save_to_file(Fact) :-
  tell('./weight.data'),
  listing(Fact),
  told.

% Re-set weight values
clenaer() :-
  retractall(data(_, _)),
  retract(totalEpoch(_)),
  save_weight(synaptic, []),
  save_weight(bias, 1),
  save_err(0),
  save_loss(previous, inf),
  save_loss(now, inf).

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
calc_loss(Err) :-
  error(SE),
  SErr is SE + Err * Err,
  save_err(SErr),
  data_length(Count),
  Loss is SErr rdiv Count,
  save_loss(now, Loss).

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
adjust_weights('-') :-
  learning_rate(LR),
  % error(Err),
  weight(synaptic, W),
  length_list(W, LenX),
  % ErrLR is LR * Err,
  % make_list_of_ele(LenX, ErrLR, List),
  make_list_of_ele(LenX, LR, List),
  subtract_ele_by_ele_in_list(W, List, NewW),
  save_weight(synaptic, NewW).

adjust_weights('+') :-
  learning_rate(LR),
  % error(Err),
  weight(synaptic, W),
  length_list(W, LenX),
  % ErrLR is LR * Err,
  % make_list_of_ele(LenX, ErrLR, List),
  make_list_of_ele(LenX, LR, List),
  add_ele_by_ele_in_list(W, List, NewW),
  save_weight(synaptic, NewW).

% Add the elements of the lists that are in
% the same position and create another list
add_ele_by_ele_in_list([], [], []).
add_ele_by_ele_in_list([H1|T1], [H2|T2], [H|Rta]) :-
  H is H1 + H2,
  add_ele_by_ele_in_list(T1, T2, Rta).

subtract_ele_by_ele_in_list([], [], []).
subtract_ele_by_ele_in_list([H1|T1], [H2|T2], [H|Rta]) :-
  H is H1 - H2,
  subtract_ele_by_ele_in_list(T1, T2, Rta).

% Multiply the elements of the lists that are
% in the same position and create another list
multi_ele_to_list([], [], []).
multi_ele_to_list([H1|T1], [H2|T2], [H|Rta]) :-
  H is H1 * H2,
  multi_ele_to_list(T1, T2, Rta).

% Make list like this [LR,LR,...]
make_list_of_ele(0, _, []).
make_list_of_ele(Length, LR, [LR|T]) :-
  Length > 0,
  L1 is Length - 1,
  make_list_of_ele(L1, LR, T).

% info only if epoch change
info(_) :-
  data(_, _).
info(Epoch) :-
  totalEpoch(TotalEpoch),
  weight(synaptic, W),
  weight(bias, B),
  loss(now, Loss),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w ---
  Weights: ~w
  Bias: ~w
  Loss: ~4f
  ~n', [Run, W, B, Loss]).
% run one only the first time.
info(Epoch) :-
  asserta(totalEpoch(Epoch)),
  info(Epoch).

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
  format('~t[INFO] predic: ~w - real: ~w~n', [P1, Label]),
  Err is Label - P1,

  calc_loss(Err), % Loss or MSE
  info(Epoch),
  epoch(Epoch).
% if loos is 0, stop loop
epoch(_) :-
  loss(now, 0),
  epoch(-1).
% loop by Epoch
epoch(Epoch) :-
  Epoch >= 0,
  loss(previous, LossPre),
  loss(now, LossNow),
  LossPre < LossNow,
  save_loss(previous, LossPre),
  adjust_weights('+'),
  nextEpoch(Epoch).

% loop by Epoch
epoch(Epoch) :-
  Epoch >= 0,
  loss(previous, LossPre),
  loss(now, LossNow),
  LossPre > LossNow,
  save_loss(previous, LossNow),
  adjust_weights('-'),
  nextEpoch(Epoch).

% loop by Epoch
epoch(Epoch) :-
  Epoch >= 0,
  adjust_weights('-'),
  nextEpoch(Epoch).

nextEpoch(Epoch) :-
  openDataSet,
  NextEpoch is Epoch - 1,
  save_err(0), % by epoch
  epoch(NextEpoch).

