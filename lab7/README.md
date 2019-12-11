## Подстановочный алгоритм (plug-in)

Алгоритм заключается в восстановлении параметров нормального распределения <a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y,\Sigma_y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y,\Sigma_y" title="\mu_y,\Sigma_y" /></a> ,  для каждого класса  <a href="https://www.codecogs.com/eqnedit.php?latex=y&space;\in&space;Y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?y&space;\in&space;Y" title="y \in Y" /></a> и подстановке их в формулу оптимального байесовского классификатора <a href="https://www.codecogs.com/eqnedit.php?latex=a(x)&space;=&space;\arg\max_{y&space;\in&space;Y}\lambda_yP_yp_y(x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x)&space;=&space;\arg\max_{y&space;\in&space;Y}\lambda_yP_yp_y(x)" title="a(x) = \arg\max_{y \in Y}\lambda_yP_yp_y(x)" /></a>, прологорифмируя получим следующую формулу <a href="https://www.codecogs.com/eqnedit.php?latex=a(x)&space;=&space;\arg\max_{y&space;\in&space;Y}(\ln(\lambda_yP_y)-\frac{n}{2}\ln(2\pi)-\frac{1}{2}\ln(|\Sigma_y^{-1}|)-\frac{1}{2}(x-\mu_y)^T\Sigma_y^{-1}(x-\mu_y))" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x)&space;=&space;\arg\max_{y&space;\in&space;Y}(\ln(\lambda_yP_y)-\frac{n}{2}\ln(2\pi)-\frac{1}{2}\ln(|\Sigma_y^{-1}|)-\frac{1}{2}(x-\mu_y)^T\Sigma_y^{-1}(x-\mu_y))" title="a(x) = \arg\max_{y \in Y}(\ln(\lambda_yP_y)-\frac{n}{2}\ln(2\pi)-\frac{1}{2}\ln(|\Sigma_y^{-1}|)-\frac{1}{2}(x-\mu_y)^T\Sigma_y^{-1}(x-\mu_y))" /></a>. 

Предполагается, что ковариационные матрицы классов не равны.

Параметры нормального распределения оценивают согласно принципа максимума правдоподобия:

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y&space;=&space;\frac{1}{l_y}\sum_{x_i;y_i=y}x_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y&space;=&space;\frac{1}{l_y}\sum_{x_i;y_i=y}x_i" title="\mu_y = \frac{1}{l_y}\sum_{x_i;y_i=y}x_i" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y&space;=&space;\frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y&space;=&space;\frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" title="\mu_y = \frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" /></a>

Разделяющая поверхность между двумя классами s и t задаётся следующим образом:

<a href="https://www.codecogs.com/eqnedit.php?latex=\lambda_sP_s\rho_s(x)&space;=&space;\lambda_tP_t\rho_t(x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\lambda_sP_s\rho_s(x)&space;=&space;\lambda_tP_t\rho_t(x)" title="\lambda_sP_s\rho_s(x) = \lambda_tP_t\rho_t(x)" /></a>

Прологарифмируя обе части выражения и проведя преобразования получим уровнение разделяющей поверхности.

# Реализация на R
```R
library(MASS)

estimateMu <- function(x)
{
  m <- dim(x)[2]
  mu <- matrix(NA, 1, m)
  for(i in 1:m)
  {
    mu[1,i] <- mean(x[,i])
  }
  return(mu)
}

estimateSigma <- function(x,mu)
{
  n <- dim(x)[1]
  m <- dim(x)[2]
  sigma <- matrix(0, m, m)
  for(i in 1:n)
  {
    
    sigma <- sigma + (t(x[i,]-mu) %*% (x[i,]-mu))
    
  }
  return(sigma/(n - 1))
}

coef <- function(mu1,mu2,sigma1,sigma2)
{
  
  invsigma1 <- solve(sigma1)
  invsigma2 <- solve(sigma2)
  
  a <- invsigma1 - invsigma2
  A <- a[1,1]
  B <- a[2,2]
  C <- 2 * a[1,2]
  
  b <- invsigma1 %*% t(mu1) - invsigma2 %*% t(mu2)
  D <- -2 * b[1,1]
  E <- -2 * b[2,1]
  
  F <- c(log(det(sigma1)) - log(det(sigma2)) + mu1 %*% invsigma1 %*% t(mu1) - mu2 %*% invsigma2 %*% t(mu2))
  
  func <- function(x, y) {
    x^2*A + y^2*B + x*y*C + x*D + y*E + F
  }
  return(func)
}

classifier <- function(xy,m,s,lymda,PP)
{
  n <- dim(mu)[2]
  p <- rep(0,n)
  a <- matrix(c(0,0,0,0),2,2)
  for(i in 1:n)
  {
    sigma <- matrix(c(s[i*2-1,1],s[i*2-1,2],s[i*2,1],s[i*2,2]),2,2)
    mu <- matrix(c(m[i,1],m[i,2]),1,2)
    l <- lymda[i]
    P <- PP[i]
    det <- det(sigma)
    invsigma <- solve(sigma)
    a <- invsigma 
    A <- a[1,1]
    B <- a[2,2]
    C <- 2 * a[1,2]
    
    b <- invsigma %*% t(mu) 
    D <- -2 * b[1,1]
    E <- -2 * b[2,1]
    
    F <- c(log(det(sigma))) + mu %*% invsigma %*% t(mu) 
    
    func <- function(x, y) {
      f <- x^2*A + y^2*B + x*y*C + x*D + y*E + F
    }
    f <- func(xy[1],xy[2])
    p[i] <- log(l*P) - f
  }
  if(p[1] > p[2])
  {
    class <- colors[1]
  }
  else
  {
    class <- colors[2]
  }
  return(class)
}
```

# Примеры работы подстановочного алгоритма:
# Элипс
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/elips.png)

карта классификации
<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_1(10,&space;15),\mu2(15,15),\Sigma_1(5,0,0,5),\Sigma2(15,0,0,20)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_1(10,&space;15),\mu2(15,15),\Sigma_1(5,0,0,5),\Sigma2(15,0,0,20)" title="\mu_1(10, 15),\mu2(15,15),\Sigma_1(5,0,0,5),\Sigma2(15,0,0,20)" /></a>
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/elipse_map.png)

# Прямая
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/prymay.png)

карта классификации
<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_1(5,&space;25),\mu2(15,15),\Sigma_1(5,0,0,5),\Sigma_2(5,0,0,5)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_1(5,&space;25),\mu2(15,15),\Sigma_1(5,0,0,5),\Sigma_2(5,0,0,5)" title="\mu_1(5, 25),\mu2(15,15),\Sigma_1(5,0,0,5),\Sigma_2(5,0,0,5)" /></a>
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/line_map.png)

# Гипербола
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/giperbola.png)

карта классификации
<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_1(10,&space;15),\mu2(15,15),\Sigma_1(5,0,0,20),\Sigma_2(15,0,0,10)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_1(10,&space;15),\mu2(15,15),\Sigma_1(5,0,0,20),\Sigma_2(15,0,0,10)" title="\mu_1(10, 15),\mu2(15,15),\Sigma_1(5,0,0,20),\Sigma_2(15,0,0,10)" /></a>
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/giperbola_map.png)
