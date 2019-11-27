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
  return(sigma/(n + 2))
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


n<-300
sigma1 <- matrix(c(5,0, 0, 5), 2, 2)
sigma2 <- matrix(c(5, 0,0, 5), 2, 2)

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

mu1 <- estimateMu(xy1)
mu2 <- estimateMu(xy2)

sigma <-estimateSigma(xy1,xy2,mu1,mu2)

X <- seq(plotxmin-5, plotxmax+5, len = 500)
Y <- seq(plotymin-5, plotymax+5, len = 500)

f <- coef(mu1,mu2,sigma)
Z <- outer(X, Y, f)

contour(X, Y, Z, levels = 0, add = TRUE, drawlabels = TRUE, lwd = 2.5,col="green")



