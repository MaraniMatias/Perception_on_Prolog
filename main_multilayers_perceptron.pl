/*
% Activation Function
  step, relu, sigmoid

*/
% Make a log file
:- protocola('log_main_multilayers_perceptron.log').
:- dynamic data_length/1 .
:- ['./utilities.pl'].

learning_rate(0.1).

data_length(0).
% data([], label).
openDataSet :-
  retractall(data(_, _)),
  retract(data_length(_)),
% consult('./database/and.pl'),
% consult('./database/or.pl'),
% consult('./database/par.pl'),
% consult('./database/impar.pl'),
% consult('./database/mayor_5.pl'),
  consult('./database/xor.pl'),
  aggregate_all(count, data(_, _), Count),
  asserta(data_length(Count)).

% info only if epoch change
info(_) :-
  % weight(p1, W1, synaptic), writeln(['W1', W1]),
  % weight(p1, B1, bias), writeln(['B1', B1]),
  % error(e1, Error), writeln(['Err', Error]),
  data(_, _).
info(Epoch) :-
  totalEpoch(TotalEpoch),
  loss(Loss),
  Run is TotalEpoch - Epoch + 1,
  format('[INFO] Epoch ~w - Loss: ~4f ~n', [Run, Loss]).
% Run one times
info(Epoch) :-
  asserta(totalEpoch(Epoch)),
  info(Epoch).

% epoch
epoch(-1) :-
  save_to_file(weight),
  clenaer([
    p1_c1, p2_c1,
    p1_ouput
  ]).
% loop by data
epoch(Epoch) :-
  Epoch >= 0,

  data(X, Target),
  retract(data(X, Target)),
  perceptron(p1_c1, sigmoid, X, P1_C1),
  perceptron(p2_c1, sigmoid, X, P2_C1),
  perceptron(p1_ouput, sigmoid, [P1_C1, P2_C1], P_Output),

  Predic is P_Output + 0.0,
  format('~t[INFO] predic: ~w - real: ~w~n', [Predic, Target]),
  Err is Target - P_Output,

  backpropagation([[p1_c1, p2_c1]], [p1_ouput], X, [Target], [Err]),

  calc_loss(Err), % Loss or MSE
  info(Epoch),
  epoch(Epoch).
% if loos is 0 or less, stop loop
epoch(_) :-
  loss(0),
  epoch(-1).
% loop by Epoch
epoch(Epoch) :-
  Epoch >= 0,
  openDataSet,
  NextEpoch is Epoch - 1,
  save_err(0),
  epoch(NextEpoch).
% Load weights for preceptron
:- epoch(-1).
