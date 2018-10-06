:- dynamic data_length/1 .
:- ['./utilities.pl'].

data_length(0).
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
  aggregate_all(count, data(_, _), Count),
  asserta(data_length(Count)).

weight(p1_c1, [], synaptic).
weight(p2_c1, [], synaptic).
weight(p3_c1, [], synaptic).
weight(p4_c1, [], synaptic).
weight(p5_c1, [], synaptic).
weight(p1_c2, [], synaptic).
weight(p2_c2, [], synaptic).
weight(p3_c2, [], synaptic).
weight(p1_ouput, [], synaptic).

weight(p1_c1, 0, bias).
weight(p2_c1, 0, bias).
weight(p3_c1, 0, bias).
weight(p4_c1, 0, bias).
weight(p5_c1, 0, bias).
weight(p1_c2, 0, bias).
weight(p2_c2, 0, bias).
weight(p3_c2, 0, bias).
weight(p1_ouput, 0, bias).

learning_rate(0.9).

% info only if epoch change
info(_) :-
  % weight(p1, W1, synaptic), writeln(['W1', W1]),
  % weight(p1, B1, bias), writeln(['B1', B1]),
  % error(e1, Error), writeln(['Err', Error]),
  data(_, _).
info(Epoch) :-
  totalEpoch(TotalEpoch),
  weight(p1_ouput, W, synaptic),
  weight(p1_ouput, B, bias),
  loss(Loss),

  Run is TotalEpoch - Epoch + 1,
  format('
  --- Summary  Epoch ~w ---
  Weights: ~w
  Bias: ~w
  Loss: ~4f
  ~n', [Run, W, B, Loss]).
% Run one times
info(Epoch) :-
  asserta(totalEpoch(Epoch)),
  info(Epoch).

% epoch
epoch(-1) :-
  clenaer([
    p1_c1, p2_c1, p3_c1, p4_c1, p5_c1,
    p1_c2, p2_c2, p3_c2,
    p1_ouput
  ]).
% loop by data
epoch(Epoch) :-
  Epoch >= 0,

  data(X, Label),
  retract(data(X, Label)),
  perceptron(p1_c1, X, P1_C1),
  perceptron(p2_c1, X, P2_C1),
  perceptron(p3_c1, X, P3_C1),
  perceptron(p4_c1, X, P4_C1),
  perceptron(p5_c1, X, P5_C1),

  perceptron(p1_c2, [P1_C1, P2_C1, P3_C1, P4_C1, P5_C1], P1_C2),
  perceptron(p2_c2, [P1_C1, P2_C1, P3_C1, P4_C1, P5_C1], P2_C2),
  perceptron(p3_c2, [P1_C1, P2_C1, P3_C1, P4_C1, P5_C1], P3_C2),

  perceptron(p1_ouput, [P1_C2, P2_C2, P3_C2], P_Output),
  format('~t[INFO] predic: ~w - real: ~w~n', [P_Output, Label]),
  Err is Label - P_Output,

  Err1 is 0.5 * Err,
  adjust_net_weights([p1_c1, p2_c1, p3_c1, p4_c1, p5_c1], X, Err1),
  Err1 is 0.1 * Err,
  adjust_net_weights([p1_c2, p2_c2, p3_c2], [P1_C1, P2_C1, P3_C1, P4_C1, P5_C1], Err1),
  % Err2 is 0.9 * Err,
  % adjust_net_weights([p1_ouput], [P1_C2, P2_C2, P3_C2], Err2),

  calc_loss(Err), % Loss or MSE
  info(Epoch),
  epoch(Epoch).
% loop by Epoch
epoch(Epoch) :-
  Epoch >= 0,
  openDataSet,
  NextEpoch is Epoch - 1,
  save_err(0),
  epoch(NextEpoch).
