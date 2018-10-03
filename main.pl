:- dynamic(data_length/1).
:- ['./utilities.pl'].

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
weight(p1, [], synaptic).
weight(p1, 0, bias).
error(e1, 0).
learning_rate(0.9).

% info only if epoch change
info(_, _) :-
  % weight(p1, W1, synaptic), writeln(['W1', W1]),
  % weight(p1, B1, bias), writeln(['B1', B1]),
  % error(e1, Error), writeln(['Err', Error]),
  data(_, _).
info(Epoch, Loss) :-
  totalEpoch(TotalEpoch),
  weight(p1, W1, synaptic),
  weight(p1, B1, bias),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w ---
  Weights: ~w
  Bias: ~w
  Loss: ~4f
  ~n', [Run, W1, B1, Loss]).
% Run one times
info(Epoch, Loss) :-
  asserta(totalEpoch(Epoch)),
  info(Epoch, Loss).

% epoch
epoch(-1) :-
  clenaer().
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

  calc_loss(Err, Loss), % Loss or MSE
  info(Epoch, Loss),
  epoch(Epoch).
% loop by Epoch
epoch(Epoch) :-
  Epoch >= 0,
  openDataSet,
  NextEpoch is Epoch - 1,
  save_err(e1, 0),
  epoch(NextEpoch).
