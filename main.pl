:- ['./utilities.pl'].

% data([], label).
openDataSet :-
  retractall(data(_,_)),
  consult('./database/and.pl').
% consult('./database/par.pl').
% consult('./database/impar.pl').
% consult('./database/mayor_5.pl').
% consult('./database/xor.pl').
% consult('./database/mini.pl').
:- openDataSet.

weight(p1, [], synaptic).
weight(p1, 0, bias).
error(e1, 0).
learning_rate(0.01).
loss(inf).

% info only if epoch change
info(Epoch) :-
  weight(p1, W1, synaptic), writeln(['W1', W1]),
  weight(p1, B1, bias), writeln(['B1', B1]),
  % error(e1, Error), writeln(['Err', Error]),
  data(_, _).
info(Epoch) :-
  totalEpoch(TotalEpoch),
  weight(p1, W1, synaptic),
  weight(p1, B1, bias),
  error(e1, Error),
  loss(Loss),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w ---
  Weights: ~w
  Bias: ~w
  Error: ~w
  Loss: ~w
  ~n', [Run, W1, B1, Error, Loss]).

info(Epoch) :-
  asserta(totalEpoch(Epoch)),
  info(Epoch).

% epoch
epoch(0) :-
  clenaer().
% loop by data
epoch(Epoch) :-
  Epoch \= 0,
  data(X, Label),
  retract(data(X, Label)),
  perception(p1, X, P1), writeln(['Predic:', P1,'Real:', Label]),
  Err is Label - P1,

  length_list(X, LenX),
  % Make list Elist is [Err,Err,...]
  make_list_of_ele(LenX, Err, Elist),
  % Make list XElist is [X1*Err,X2*Err,...]
  multi_ele_to_list(X, Elist, XElist),
  adjust_weights(p1, XElist),

  weight(p1, B, bias),
  NewB is B + Err,
  updata_weight(Name, NewB, bias),

  info(Epoch),

  % error(e1, E),
  % SErr is E + Err * Err,
  % updata_err(e1, SErr),

  epoch(Epoch).
% loop by Epoch
epoch(Epoch) :-
  Epoch \= 0,
  openDataSet,
  % calc_loss(e1),
  NextEpoch is Epoch - 1,
  epoch(NextEpoch).
