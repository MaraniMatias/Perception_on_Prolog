:- dynamic(totalEpoch/1). % For epoch info
:- dynamic(weight/3).
:- dynamic(error/2).
:- dynamic(data/2).
:- ['./utilities.pl'].

% data([], label).
openDataSet :-
  retractall(data(_, _)),
%  consult('./database/and.pl').
% consult('./database/par.pl').
% consult('./database/impar.pl').
% consult('./database/mayor_5.pl').
consult('./database/xor.pl').
:- openDataSet.

weight(p1, [], synaptic).
weight(p2, [], synaptic).
weight(p3, [], synaptic).
weight(p1, 0, bias).
weight(p2, 0, bias).
weight(p3, 0, bias).
error(e1, 0).

% info only if epoch change
info(Epoch) :-
  data(_, _).
info(Epoch) :-
  totalEpoch(TotalEpoch),
  weight(p1, W1, synaptic),
  weight(p2, W2, synaptic),
  weight(p3, W3, synaptic),
  weight(p1, B1, bias),
  weight(p2, B2, bias),
  weight(p3, B3, bias),
  error(e1, Error),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w --- Error: ~w
  Weights of perception 1: ~w
  Bias: ~w
  Weights of perception 2: ~w
  Bias: ~w
  Weights of perception 3: ~w
  Bias: ~w
  ', [Run, Error, W1, B1, W2, B2, W3, B3]).

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
  retractall(data(X, Label)),
  perception(p1, X, P1),
  perception(p2, X, P2),
  perception(p3, [P1, P2], P3),
  Err is Label - P3,
  adjust_weights(p1, Err),
  adjust_weights(p2, Err),
  adjust_weights(p3, Err),
  info(Epoch),
  updata_err(e1, Err),
  epoch(Epoch).
% loop by Epoch
epoch(Epoch) :-
  Epoch \= 0,
  openDataSet,
  NextEpoch is Epoch - 1,
  epoch(NextEpoch).
