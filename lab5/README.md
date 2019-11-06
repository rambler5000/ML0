## Линии уровня нормального распределения

Вероятностное распределение с плотностью 
<a href="https://www.codecogs.com/eqnedit.php?latex=N(x,\mu,\Sigma)&space;=&space;\frac{1}{(2\pi)^n\left&space;|&space;\Sigma&space;\right&space;|}exp(-\frac{1}{2}(x-\mu)^T&space;\Sigma^{-1}(x-\mu))" target="_blank"><img src="https://latex.codecogs.com/gif.latex?N(x,\mu,\Sigma)&space;=&space;\frac{1}{(2\pi)^n\left&space;|&space;\Sigma&space;\right&space;|}exp(-\frac{1}{2}(x-\mu)^T&space;\Sigma^{-1}(x-\mu))" title="N(x,\mu,\Sigma) = \frac{1}{(2\pi)^n\left | \Sigma \right |}exp(-\frac{1}{2}(x-\mu)^T \Sigma^{-1}(x-\mu))" /></a>

называется  n-мерным многомерным нормальным распределением с математическим ожиданием 
<a href="https://www.codecogs.com/eqnedit.php?latex=\mu&space;\in&space;R^n" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu&space;\in&space;R^n" title="\mu \in R^n" /></a>

и ковариоционной матрицей 
<a href="https://www.codecogs.com/eqnedit.php?latex=\Sigma&space;\in&space;R^{n*n}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\Sigma&space;\in&space;R^{n*n}" title="\Sigma \in R^{n*n}" /></a>

Предполагается, что матрица симетрична, невырожденная, положительно определенная.

# Георметрия нормальной плоскости

1. Если признаки некорелированы, то линия уровня плотности распределения имеют форму элипсойдов.

2. Если признаки имеют одинакоые дисперсии, то элипсойды являются сферами.

3. Если признаки корелированы, то матрица не диагональна и линии уровня имеют форму элипсойдов, оси которых повернуты  относительно исходной системы координат. 

# Реализация на R
```
line_norm <- function(center,A)
{
  det <- det(A)
  a <- A[2,2]/det
  b <- -A[2,1]/det
  c <- -A[1,2]/det
  d <- A[1,1]/det
  
  x0 <- center[1]
  y0 <- center[2]
  
  X <- seq(-2.5, 2.5, 0.1)
  Y <- seq(-2.5, 2.5, 0.1)
  
  
  A <- d
  B <- a
  C <- -c -b
  D <- -2*d*x0 + y0*(c+b)
  E <- -2*a*y0 + x0*(c+b)
  F <- d*x0^2 + a*y0^2 + x0*y0*(-c-b)
  
  func <- function(x, y) {
    1/(2*pi*sqrt(det))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  }
  Z <- outer(X, Y, func)
  
  contour(X, Y, Z)
}
```

# Некорелированы
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab5/elips.png)

# Одинаковая дисперсия
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab5/sfera.png)

# Корелированы
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab5/elips_povorot.png)

