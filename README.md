# Perceptron On Prolog
Implementation of a simple perceptron in Prolog.

## Perceptron

![Perceptron Image](https://raw.githubusercontent.com/MaraniMatias/Perceptron_on_Prolog/image/percepto.png)

## Problem

Sort numbers in even, odd, or greater than 5

![7 Segment Display Image](https://raw.githubusercontent.com/MaraniMatias/Perceptron_on_Prolog/image/7_segment_display.png)

The number seven is formed on the leds A, B, C and the other off.

We represent the data in this way.
Where label marks the value to predict.

```prolog
% data([],label)
%     a  b  c  d  e  f  g
data([1, 1, 1, 1, 1, 1, 0], 1).% 0
data([0, 1, 1, 0, 0, 0, 0], 0).% 1
data([1, 1, 0, 1, 1, 0, 1], 1).% 2
data([1, 1, 1, 1, 0, 0, 1], 0).% 3
data([0, 1, 1, 0, 0, 1, 1], 1).% 4
data([1, 0, 1, 1, 0, 1, 1], 0).% 5
data([1, 0, 1, 1, 1, 1, 1], 1).% 6
data([1, 1, 1, 0, 0, 0, 0], 0).% 7
data([1, 1, 1, 1, 1, 1, 1], 1).% 8
data([1, 1, 1, 1, 0, 1, 1], 0).% 9
```

## Run

```prolog
?- epoch(20).
```
