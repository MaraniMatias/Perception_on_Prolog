% Make a log file
:- protocola('log_main_multilayers_perceptron.log').
:- ['./utilities.pl'].
% Program setting
predic_info(on). % on/off

% dataSet('./database/and.pl').
% dataSet('./database/or.pl').
% dataSet('./database/par.pl').
% dataSet('./database/impar.pl').
% dataSet('./database/mayor_5.pl').
  dataSet('./database/xor.pl').

learning_rate(0.1).
deep_net([
  [
    perceptron(p1_inputs, sigmoid),
    perceptron(p2_inputs, sigmoid)
  ],
  [
    perceptron(p1_output, sigmoid)
  ]
]).

% epoch
epoch(-1) :-
  save_to_file(weight),
  deep_net(DeepNet),
  clenaer(DeepNet).
% loop by data
epoch(Epoch) :-
  Epoch >= 0,
  data(X, Target),
  retract(data(X, Target)),
  deep_net(DeepNet),
  train(DeepNet, X, Target),
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
