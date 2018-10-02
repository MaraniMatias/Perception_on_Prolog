# Perceptron en Prolog
Simple implementación de un un perceptron con  Prolog

## Perceptron, ![info](https://en.wikipedia.org/wiki/Perceptron)

![Perceptron Image](https://raw.githubusercontent.com/MaraniMatias/Perceptron_on_Prolog/image/percepto.png)

## Problemas

### Problema: Números pares, Números impares

Indicar si el numero es `par`, `impar` o `mayor 5`

![7 Segment Display Image](https://raw.githubusercontent.com/MaraniMatias/Perceptron_on_Prolog/image/7_segment_display.png)

Para formar un numero con este componente electrónico, tenes que encender los leds correspondientes,
por ejemplo para el número siete encenderíamos los led A, B, C.

Representaremos esta información de esta forma `data(Numero, Label)`, donde numero es una array
indicando que leds se enciende y label el a aprender.

```prolog
% data([], label).
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

### Correr

En el archivo `main.pl`
```prolog
?- epoch(300).
```

## Problema XOR

Con un perceptron solo podemos resolver problemas que se comporte con las compuerta lógica AND o
OR.
Para estos problemas necesitamos tres preceptrones

![Tables and graphs adapted from Kevin Swingler.](https://raw.githubusercontent.com/MaraniMatias/Perceptron_on_Prolog/image/and_or_xor.png)

![Three perceptrons](https://raw.githubusercontent.com/MaraniMatias/Perceptron_on_Prolog/image/three_perceptrons.jpg)

### Correr

En el archivo `main-three-perception.pl`
```prolog
?- epoch(300).
```
