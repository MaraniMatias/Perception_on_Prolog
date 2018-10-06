% Make a log file
:- protocola('main.log').
:- dynamic data_length/1 .
:- ['./utilities.pl'].

learning_rate(0.9).

data_length(0).
% data([], label).
openDataSet :-
  retractall(data(_, _)),
  retract(data_length(_)),
% consult('./database/and.pl'),
% consult('./database/or.pl'),
% consult('./database/par.pl'),
  consult('./database/impar.pl'),
% consult('./database/mayor_5.pl'),
% consult('./database/xor.pl'),
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
  weight(p1, W1, synaptic),
  weight(p1, B1, bias),
  loss(Loss),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w ---
  Weights: ~w
  Bias: ~w
  Loss: ~4f
  ~n', [Run, W1, B1, Loss]).
% Run one times
info(Epoch) :-
  asserta(totalEpoch(Epoch)),
  info(Epoch).

% epoch
epoch(-1) :-
  clenaer([p1]).
% loop by data
epoch(Epoch) :-
  Epoch >= 0,

  data(X, Label),
  retract(data(X, Label)),
  perceptron(p1, X, P1),
  % writeln(['Real:', Label,'Predic:',P1]),
  Err is Label - P1,

  length_list(X, LenX),
  % Make list Elist is [Err,Err,...]
  make_list_of_ele(LenX, Err, Elist),
  % Make list XElist is [X1*Err,X2*Err,...]
  multi_ele_to_list(X, Elist, XElist),
  adjust_weights(p1, XElist),

  weight(p1, B, bias),
  NewB is B + Err,
  save_weight(p1, NewB, bias),

  % adjust_net_weights([p1], X, Err),

  calc_loss(Err), % Loss or MSE
  info(Epoch),
  epoch(Epoch).
% if loos is 0, stop loop
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
