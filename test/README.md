# How to generate [reference.csv](reference.csv)

```Mathematica
Power[10,Range[-30,30]/10]
Sqrt[Power[10,Range[-30,30]/10]]
ArithmeticGeometricMean[1,Power[10,Range[-30,30]/10]]
(1+Power[10,Range[-30,30]/10])/2
```

```Mathematica
ScientificForm[N[%,39], NumberFormat -> (Row[{#1, "e", #3}] &)]
```
