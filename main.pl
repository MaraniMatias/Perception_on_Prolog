:- dynamic(totalEpoch/1). % For epoch info
:- dynamic(weight/3).
:- dynamic(error/2).
:- dynamic(data/2).
:- ['./utilities.pl'].

% data([], label).
openDataSet :-
  retractall(data(_,_)),
% consult('./database/and.pl').
% consult('./database/par.pl').
% consult('./database/impar.pl').
% consult('./database/mayor_5.pl').
  consult('./database/xor.pl').
:- openDataSet.

weight(p1, [], synaptic).
weight(p1, 0, bias).
error(e1, inf).

% info only if epoch change
info(Epoch) :-
  data(_, _).
info(Epoch) :-
  totalEpoch(TotalEpoch),
  weight(p1, W1, synaptic),
  weight(p1, B1, bias),
  error(e1, Error),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w --- Error: ~w
  Weights: ~w
  Bias: ~w
  ~n', [Run, Error, W1, B1]).

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
  Err is Label - P1,
  adjust_weights(p1, Err),
  info(Epoch),
  updata_err(e1, Err),
  epoch(Epoch).
% loop by Epoch
epoch(Epoch) :-
  Epoch \= 0,
  openDataSet,
  NextEpoch is Epoch - 1,
  epoch(NextEpoch).
