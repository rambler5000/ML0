## Подстановочный алгоритм (plug-in)

Алгоритм заключается в восстановлении параметров нормального распределения <a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y,\Sigma_y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y,\Sigma_y" title="\mu_y,\Sigma_y" /></a> ,  для каждого класса  <a href="https://www.codecogs.com/eqnedit.php?latex=y&space;\in&space;Y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?y&space;\in&space;Y" title="y \in Y" /></a> и подстановке их в формулу оптимального байесовского классификатора. Предполагается, что ковариационные матрицы классов не равны.

Параметры нормального распределения оценивают согласно принципа максимума правдоподобия:

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y&space;=&space;\frac{1}{l_y}\sum_{x_i;y_i=y}x_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y&space;=&space;\frac{1}{l_y}\sum_{x_i;y_i=y}x_i" title="\mu_y = \frac{1}{l_y}\sum_{x_i;y_i=y}x_i" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y&space;=&space;\frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y&space;=&space;\frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" title="\mu_y = \frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" /></a>

Разделяющая поверхность между двумя классами s и t задаётся следующим образом:

<a href="https://www.codecogs.com/eqnedit.php?latex=\lambda_sP_s\rho_s(x)&space;=&space;\lambda_tP_t\rho_t(x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\lambda_sP_s\rho_s(x)&space;=&space;\lambda_tP_t\rho_t(x)" title="\lambda_sP_s\rho_s(x) = \lambda_tP_t\rho_t(x)" /></a>

Прологарифмируя обе части выражения и проведя преобразования получим уровнение разделяющей поверхности.

# Реализация на R
```
vostanovleniemu <- function(x)
{
  m <- dim(x)[2]
  mu <- matrix(NA, 1, m)
  for(i in 1:m)
  {
    mu[1,i] <- mean(x[,i])
  }
  return(mu)
}

vostanovlenieсovariance <- function(x,mu)
{
  n <- dim(x)[1]
  m <- dim(x)[2]
  sigma <- matrix(0, m, m)
  for(i in 1:n)
  {
    sigma <- sigma + (t(x[i,]-mu)%*%(x[i,]-mu))
  }
  return(sigma/(n-1))
}

coef <- function(mu1,mu2,sigma1,sigma2)
{
  
  invsigma1 <- solve(sigma1)
  invsigma2 <- solve(sigma2)

  a <- invsigma1 - invsigma2
  A <- a[1,1]
  B <- a[2,2]
  C <- 2*a[1,2]
  
  b <- invsigma1%*%t(mu1) - invsigma2%*%t(mu2)
  D <- -2*b[1,1]
  E <- -2*b[2,1]
  
  F <- c(log(det(sigma1)) - log(det(sigma2)) + mu1%*%invsigma1%*%t(mu1) - mu2%*%invsigma2%*%t(mu2))

  func <- function(x, y) {
    x^2*A + y^2*B + x*y*C + x*D + y*E + F
  }
  return(func)
}

```

# Примеры работы подстановочного алгоритма:
# Элипс
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/elips.png)

# Прямая
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/prymay.png)

# Гипербола
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/giperbola.png)

