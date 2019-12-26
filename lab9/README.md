# Линейные алгоритмы классификации
Линейный классификатор - классификатор вида <a href="https://www.codecogs.com/eqnedit.php?latex=a(x,w)&space;=&space;sign&space;(\langle&space;w,x&space;\rangle&space;-&space;w_0)&space;=&space;sign(\sum_{j=1}^n&space;w_j&space;f_j(x)&space;-&space;w_0)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x,w)&space;=&space;sign&space;(\langle&space;w,x&space;\rangle&space;-&space;w_0)&space;=&space;sign(\sum_{j=1}^n&space;w_j&space;f_j(x)&space;-&space;w_0)" title="a(x,w) = sign (\langle w,x \rangle - w_0) = sign(\sum_{j=1}^n w_j f_j(x) - w_0)" /></a> ,
где
<a href="https://www.codecogs.com/eqnedit.php?latex=w" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w" title="w" /></a> - вектор весов, <a href="https://www.codecogs.com/eqnedit.php?latex=w_0" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w_0" title="w_0" /></a> - порог принятия решения, <a href="https://www.codecogs.com/eqnedit.php?latex=w_j" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w_j" title="w_j" /></a>- вес j-го признака

<a href="https://www.codecogs.com/eqnedit.php?latex=M(x_i)=y_i&space;\langle&space;x_i,&space;w&space;\rangle" target="_blank"><img src="https://latex.codecogs.com/gif.latex?M(x_i)=y_i&space;\langle&space;x_i,&space;w&space;\rangle" title="M(x_i)=y_i \langle x_i, w \rangle" /></a> - отступ объекта x, если отступ отрицателен, то алгоритм допускает ошибку на выбраном элементе.

Минимизация суммарных потерь <a href="https://www.codecogs.com/eqnedit.php?latex=Q(w,X^l)&space;=&space;\sum_{i=1}^l&space;L(M_i(w))&space;\rightarrow&space;\min_{w}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Q(w,X^l)&space;=&space;\sum_{i=1}^l&space;L(M_i(w))&space;\rightarrow&space;\min_{w}" title="Q(w,X^l) = \sum_{i=1}^l L(M_i(w)) \rightarrow \min_{w}" /></a>,
для минимизации <a href="https://www.codecogs.com/eqnedit.php?latex=Q(w)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Q(w)" title="Q(w)" /></a>
применим метод стохастического градиентного спуска

# ADALINE
ADALINE(Адаптивны линейный элемент) - линейный алгоритм классификации, в котором используется квадратичная функция потерь:
<a href="https://www.codecogs.com/eqnedit.php?latex=$L(M)&space;=&space;(M&space;-&space;1)^2&space;=&space;(\langle&space;w,x_i\rangle-1&space;)^2$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$L(M)&space;=&space;(M&space;-&space;1)^2&space;=&space;(\langle&space;w,x_i\rangle-1&space;)^2$" title="$L(M) = (M - 1)^2 = (\langle w,x_i\rangle-1 )^2$" /></a>

обновление весов происходит по формуле:
<a href="https://www.codecogs.com/eqnedit.php?latex=w&space;=&space;w&space;-&space;\eta(\langle&space;w,x_i&space;\rangle-y_i)x_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w&space;=&space;w&space;-&space;\eta(\langle&space;w,x_i&space;\rangle-y_i)x_i" title="w = w - \eta(\langle w,x_i \rangle-y_i)x_i" /></a>

Реализация на R
```R
adaline_loss <- function(x) {
  l <- (x-1)^2
  return(l)
}
adaline_upd <- function(xi, yi, w, eta) {
  wx <- c(crossprod(w, xi))
  l <- (wx - yi) * xi
  nextW <- w - eta * l
  return(nextW)
}
```
Пример работы алгоритма:

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab9/ADALINE.png)

# Персептрон Розенблатта
Персептрон Розенблатта — линейный классификатор, в котором используется кусочно-линейная функция потерь:
<a href="https://www.codecogs.com/eqnedit.php?latex=$L=(-M)_&plus;$=$max(-M,0)$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$L=(-M)_&plus;$=$max(-M,0)$" title="$L=(-M)_+$=$max(-M,0)$" /></a>

обновление весов происходит по правилу Хебба:
<a href="https://www.codecogs.com/eqnedit.php?latex=$w$&space;=&space;$&space;w&plus;&space;\eta&space;x_iy_i$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$w$&space;=&space;$&space;w&plus;&space;\eta&space;x_iy_i$" title="$w$ = $ w+ \eta x_iy_i$" /></a>

Реализация на R
```R
hebb_loss <- function(x) {
  return (max(-x, 0))
}
hebb_upd <- function(xi, yi, w, eta) {
  nextW <- w + eta * yi * xi
  return (nextW)
}
```
Пример работы алгоритма:

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab9/hebb.png)

# Логистическая регрессия
Логистическая регрессия - линейный байесовский классификатор, использующий логарифмическую функцию потерь:
<a href="https://www.codecogs.com/eqnedit.php?latex=$L(M)=log_2(1&plus;e^{-M})$$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$L(M)=log_2(1&plus;e^{-M})$$" title="$L(M)=log_2(1+e^{-M})$$" /></a>

функция обновления весов:
<a href="https://www.codecogs.com/eqnedit.php?latex=$w$=$w&plus;\eta&space;x_i&space;y_i&space;\sigma(-&space;\langle&space;w,x_i&space;\rangle&space;y_i)$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$w$=$w&plus;\eta&space;x_i&space;y_i&space;\sigma(-&space;\langle&space;w,x_i&space;\rangle&space;y_i)$" title="$w$=$w+\eta x_i y_i \sigma(- \langle w,x_i \rangle y_i)$" /></a>, где
<a href="https://www.codecogs.com/eqnedit.php?latex=\sigma&space;=&space;\frac{1}{1&plus;e^{-z}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\sigma&space;=&space;\frac{1}{1&plus;e^{-z}}" title="\sigma = \frac{1}{1+e^{-z}}" /></a>

вероятность принадлежности элемента x к классу y
<a href="https://www.codecogs.com/eqnedit.php?latex=$P(y&space;\mid&space;x)$=$\sigma(\langle&space;w,x\rangle&space;y)$" target="_blank"><img src="https://latex.codecogs.com/gif.latex?$P(y&space;\mid&space;x)$=$\sigma(\langle&space;w,x\rangle&space;y)$" title="$P(y \mid x)$=$\sigma(\langle w,x\rangle y)$" /></a>

Реализация на R
```R
logregss_loss <- function(x) {
  return(log2(1+exp(-x)))
}
logress_upd <- function(xi, yi, w, eta) {
  nextW <- w + eta * xi * yi * sigmoid(-yi * c(crossprod(w, xi)))
  return (nextW)
}
sigmoid <- function(z) {
  return(1 / (1 + exp(-z)))
}
```

Пример работы алгоритма:

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab9/logress.png)

Пример работы алгоритмов:

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab9/add_hebb_log.png)

