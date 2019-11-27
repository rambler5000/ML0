library(MASS)

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

vostanovlenieсovariance <- function(x1,x2,mu1,mu2)
{
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  n <- n1 + n2
  m <- dim(x1)[2]
  sigma <- matrix(0, m, m)
  for(i in 1:n1)
  {

    sigma <- sigma + (t(x1[i,]-mu1)%*%(x1[i,]-mu1))
    
  }
  for(i in 1:n2)
  {

    sigma <- sigma + (t(x2[i,]-mu2)%*%(x2[i,]-mu2))

  }
  return(sigma/(n))
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

coefs <- function(xy,mu,sigma)
{
  
  invsigma <- solve(sigma)
  
  a <- invsigma
  A <- a[1,1]
  B <- a[2,2]
  C <- 2*a[1,2]
  
  b <- invsigma%*%t(mu)
  D <- -2*b[1,1]
  E <- -2*b[2,1]
  
  F <- log(det(sigma)) + mu%*%invsigma%*%t(mu)
  
  func <- function(x, y) {
    p <- x^2*A + y^2*B + x*y*C + x*D + y*E + F
    return(p)
  }
  p <- func(xy[1],xy[2])
  return(p)
}


n<-300
sigma1 <- matrix(c(3,0, 0, 3), 2, 2)
sigma2 <- matrix(c(10, 0,0, 20), 2, 2)

mu1 <- c(0, 30)
mu2 <- c(15, 10)

xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)

plotxmin <- min(xy1[,1], xy2[,1]) - 1
plotymin <- min(xy1[,2], xy2[,2]) - 1
plotxmax <- max(xy1[,1], xy2[,1]) + 1
plotymax <- max(xy1[,2], xy2[,2]) + 1
plot(xy1[,1],xy2[,2], type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))

colors <- c("tan1", "royalblue")
points(xy1, pch=21, col=colors[1], bg=colors[1])
points(xy2, pch=21, col=colors[2], bg=colors[2])

mu1 <- vostanovleniemu(xy1)
mu2 <- vostanovleniemu(xy2)

sigma <-vostanovlenieсovariance(xy1,xy2,mu1,mu2)

X <- seq(plotxmin-5, plotxmax+5, len = 500)
Y <- seq(plotymin-5, plotymax+5, len = 500)

f <- coef(mu1,mu2,sigma1,sigma2)
Z <- outer(X, Y, f)


inverseSigma <- solve(sigma)
print(sigma)
print(inverseSigma)
print(mu1)
alpha <- inverseSigma %*% t(mu1 - mu2)
mu_st <- (mu1 + mu2) / 2
beta <- mu_st %*% alpha
## Рисуем ЛДФ
abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col ="red", lwd = 3)
print(beta)
print(alpha[2,1])
print(-alpha[1,1])
print(alpha[2,1])
print(alpha)
print(beta / alpha[2,1])
print(-alpha[1,1]/alpha[2,1])

#contour(X, Y, Z, levels = 0, add = TRUE, drawlabels = TRUE, lwd = 2.5,col="green")

# x <- 0
# while(x < 25)
# {
#   y <- 0
#   while(y < 25)
#   {
#     p <- coef(mu1,mu2,sigma1,sigma2)
#     xy <- c(x,y)
#     p1 <- coefs(xy,mu1,sigma1)
#     p2 <- coefs(xy,mu2,sigma2)
# 
#     if(p1<p2)
#     {
#       points(xy[1],xy[2], col=colors[1])
#     }
#     if(p2<p1)
#     {
#       points(xy[1],xy[2],pch=21, col=colors[2])
#     }
#     y <- y+0.5
#   }
#   x <- x+0.5
# } 