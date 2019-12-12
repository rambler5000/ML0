## Линейный дискриминант Фишера - ЛДФ

Линейный дискриминант Фишера похож на подстановочный алгоритм, но имеет отличие в том, что мы предполагаем равенство ковариационных матриц, тогда алгоритм классификации примет вид:

<a href="https://www.codecogs.com/eqnedit.php?latex=a(x)&space;=&space;\arg&space;\max_{y&space;\in&space;Y}(\ln(\lambda_y&space;P_y)&space;-&space;\frac{1}{2}\mu^T_y\Sigma^{-1}\mu_y&space;&plus;&space;x^T\Sigma^{-1}\mu_y)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x)&space;=&space;\arg&space;\max_{y&space;\in&space;Y}(\ln(\lambda_y&space;P_y)&space;-&space;\frac{1}{2}\mu^T_y\Sigma^{-1}\mu_y&space;&plus;&space;x^T\Sigma^{-1}\mu_y)" title="a(x) = \arg \max_{y \in Y}(\ln(\lambda_y P_y) - \frac{1}{2}\mu^T_y\Sigma^{-1}\mu_y + x^T\Sigma^{-1}\mu_y)" /></a> или

<a href="https://www.codecogs.com/eqnedit.php?latex=a(x)&space;=&space;\arg&space;\max_{y&space;\in&space;Y}(\beta_y&space;&plus;&space;x^T\alpha_y)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?a(x)&space;=&space;\arg&space;\max_{y&space;\in&space;Y}(\beta_y&space;&plus;&space;x^T\alpha_y)" title="a(x) = \arg \max_{y \in Y}(\beta_y + x^T\alpha_y)" /></a>

Параметры нормального распределения оценивают согласно принципа максимума правдоподобия:

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y&space;=&space;\frac{1}{l_y}\sum_{x_i;y_i=y}x_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y&space;=&space;\frac{1}{l_y}\sum_{x_i;y_i=y}x_i" title="\mu_y = \frac{1}{l_y}\sum_{x_i;y_i=y}x_i" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y&space;=&space;\frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y&space;=&space;\frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" title="\mu_y = \frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" /></a><a href="https://www.codecogs.com/eqnedit.php?latex=\Sigma&space;=&space;\frac{1}{l-|Y|}\sum_{i=1}^l(x_i&space;-&space;\mu_{y_i})(x_i&space;-&space;\mu_{y_i})^T" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\Sigma&space;=&space;\frac{1}{l-|Y|}\sum_{i=1}^l(x_i&space;-&space;\mu_{y_i})(x_i&space;-&space;\mu_{y_i})^T" title="\Sigma = \frac{1}{l-|Y|}\sum_{i=1}^l(x_i - \mu_{y_i})(x_i - \mu_{y_i})^T" /></a>

Разделяющая поверхность задается так же как в подстановочном алгоритме, но имеет отличие в том что используются две одинаковые ковариационные матрицы для нахождение коэффициентов уровнения разделяющей поверхности.

# Реализация на R
```R
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

estimateSigma <- function(x1,x2,mu1,mu2)
{
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  n <- n1 + n2
  m <- dim(x1)[2]
  sigma <- matrix(0, m, m)
  for(i in 1:n1)
  {
    
    sigma <- sigma + (t(x1[i,] - mu1) %*% (x1[i,] - mu1))
    
  }
  for(i in 1:n2)
  {
    
    sigma <- sigma + (t(x2[i,] - mu2) %*% (x2[i,] - mu2))
    
  }
  return(sigma/(n - 2))
}

coef <- function(mu1,mu2,sigma)
{
  invsigma <- solve(sigma)
  
  b <- invsigma %*% t(mu1) - invsigma %*% t(mu2)
  D <- -2*b[1,1]
  E <- -2*b[2,1]
  
  F <- c(mu1 %*% invsigma %*% t(mu1) - mu2 %*% invsigma %*% t(mu2))

  func <- function(x, y) {
    x*D + y*E + F
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
    mu <- matrix(c(m[i,1],m[i,2]),1,2)
    l <- lymda[i]
    P <- PP[i]
    det <- det(sigma)
    invsigma <- solve(sigma)
    
    b <- invsigma %*% t(mu) 
    D <- -2*b[1,1]
    E <- -2*b[2,1]
    
    F <- c(mu %*% invsigma %*% t(mu)) 
    
    func <- function(x, y) {
      f<-x*D + y*E + F
    }
    f<-func(xy[1],xy[2])
    p[i] <- log(l*P) - f
  }
  if(p[1] > p[2])
  {
    class<-colors[1]
  }
  else
  {
    class<-colors[2]
  }
  return(class)
}

getRisk <- function(mu1, mu2, sigma) {
  mah <- (mu1 - mu2) %*% solve(sigma) %*% t(mu1 - mu2)
  mah <- mah * -0.5
  res <- gausian(mah, 0, 1)
}
gausian <- function(x, mu, sigma){
  return( (1/(sigma*sqrt(2*pi))) * exp(-(x - mu)^2/2*sigma^2) )
}
```
# Примеры работы алгоритма:

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_1(0,&space;30),&space;\mu_2(15,10),&space;\Sigma_1(5,&space;0,&space;0,&space;5),&space;\Sigma_2(5,&space;0,&space;0,&space;5)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_1(0,&space;30),&space;\mu_2(15,10),&space;\Sigma_1(5,&space;0,&space;0,&space;5),&space;\Sigma_2(5,&space;0,&space;0,&space;5)" title="\mu_1(0, 30), \mu_2(15,10), \Sigma_1(5, 0, 0, 5), \Sigma_2(5, 0, 0, 5)" /></a>

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab8/ldf.png)

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab8/ldf2.png)

 Одним из недостатков подстановочного алгоритма является плохая обусловленность или вырожденность матрицы ковариаций <a href="https://www.codecogs.com/eqnedit.php?latex=\Sigma_y" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\Sigma_y" title="\Sigma_y" /></a> при малом количестве обучающих элементов класса  y, вследствие чего при обращении данной матрицы <a href="https://www.codecogs.com/eqnedit.php?latex=\Sigma_y^{-1}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\Sigma_y^{-1}" title="\Sigma_y^{-1}" /></a> может получиться сильно искаженный результат, и весь алгоритм классификации окажется неустойчивым. Линейный дискриминант Фишера решает данную проблему. Наиболее целесообразно пользоваться линейным дискриминантом Фишера, когда данных для обучения недостаточно.
