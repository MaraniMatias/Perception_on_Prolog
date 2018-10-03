:- dynamic(data_length/1).
:- ['./utilities.pl'].

% data([], label).
openDataSet :-
  retractall(data(_,_)),
  retract(data_length(_)),
% consult('./database/and.pl'),
% consult('./database/or.pl'),
% consult('./database/par.pl'),
% consult('./database/impar.pl'),
% consult('./database/mayor_5.pl'),
  consult('./database/xor.pl'),
  aggregate_all(count, data(_,_), Count),
  asserta(data_length(Count)).

data_length(0).
weight(p1, [], synaptic).
weight(p2, [], synaptic).
weight(p3, [], synaptic).
weight(p1, 0, bias).
weight(p2, 0, bias).
weight(p3, 0, bias).
error(e1, 0).
learning_rate(0.9).
loss(inf).

% info only if epoch change
info(_, _) :-
  % weight(p1, W1, synaptic), writeln(['W1', W1]),
  % weight(p1, B1, bias), writeln(['B1', B1]),
  % error(e1, Error), writeln(['Err', Error]),
  data(_, _).
info(Epoch, Loss) :-
  totalEpoch(TotalEpoch),
  weight(p1, W1, synaptic),
  weight(p2, W2, synaptic),
  weight(p3, W3, synaptic),
  weight(p1, B1, bias),
  weight(p2, B2, bias),
  weight(p3, B3, bias),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w ---
  Weights of perception 1: ~w
  Bias: ~w
  Weights of perception 2: ~w
  Bias: ~w
  Weights of perception 3: ~w
  Bias: ~w
  Loss: ~4f
  ', [Run, W1, B1, W2, B2, W3, B3, Loss]).
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
  perception(p1, X, P1),
  perception(p2, X, P2),
  perception(p3, [P1, P2], P3),
  writeln(['Real:', Label,'Predic:',P3]),
  Err is Label - P3,

  length_list(X, LenX),
  % Make list Elist is [Err,Err,...]
  make_list_of_ele(LenX, Err, Elist),
  % Make list XElist is [X1*Err,X2*Err,...]
  multi_ele_to_list(X, Elist, XElist),
  adjust_weights(p1, XElist),
  adjust_weights(p2, XElist),

  % Make list Elist is [Err,Err,...]
  make_list_of_ele(2, Err, Elist2),
  % Make list XElist is [X1*Err,X2*Err,...]
  multi_ele_to_list([P1, P2], Elist2, XElist2),
  adjust_weights(p3, XElist2),

  weight(p1, B1, bias),
  NewB1 is B1 + Err,
  save_weight(p1, NewB1, bias),
  weight(p2, B2, bias),
  NewB2 is B2 + Err,
  save_weight(p2, NewB2, bias),
  weight(p3, B3, bias),
  NewB3 is B3 + Err,
  save_weight(p3, NewB3, bias),

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
