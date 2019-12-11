
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
  return(sigma/(n - 2))
}

getRisk <- function(mu1, mu2, sigma) {
  mah <- (mu1 - mu2) %*% solve(sigma) %*% t(mu1 - mu2)
  mah <- mah * -0.5
  res <- gausian(mah, 0, 1)
}
gausian <- function(x, mu, sigma){
  return( (1/(sigma*sqrt(2*pi))) * exp(-(x - mu)^2/2*sigma^2) )
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

n<-300
sigma1 <- matrix(c(5, 0, 0, 5), 2, 2)
sigma2 <- matrix(c(5, 0, 0, 5), 2, 2)

mu1 <- c(10, 15)
mu2 <- c(20, 15)

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

l1<-20
l2<-6

P1<-dim(xy1)[1]/(dim(xy1)[1]+dim(xy2)[1])
P2<-dim(xy2)[1]/(dim(xy1)[1]+dim(xy2)[1])

mu1 <- estimateMu(xy1)
mu2 <- estimateMu(xy2)

sigma <-estimateSigma(xy1,xy2,mu1,mu2)

X <- seq(plotxmin-5, plotxmax+5, len = 500)
Y <- seq(plotymin-5, plotymax+5, len = 500)

f <- coef(mu1,mu2,sigma)
Z <- outer(X, Y, f)

contour(X, Y, Z, levels=log((l1*P1)/(l2*P2)), add = TRUE, drawlabels = FALSE, lwd = 2.5,col="green")

mu<-rbind(mu1,mu2)
l<-rbind(l1,l2)
P<-rbind(P1,P2)

# x <- plotxmin
# while(x < 40)
# {
#   y <- plotymin
#   while(y < 40)
#   {
#     xy <- c(x,y)
#     c <- classifier(xy,mu,sigma,l,P)
#     points(xy[1],xy[2], col=c)
#     y <- y+0.5
#   }
#   x <- x+0.5
# }

risk <- getRisk(mu1, mu2, sigma)
cat("risk:", risk, "\n")
text( plotxmin,plotymin-1, sprintf("risk = %s", risk ), adj = c( 0, -1 ), col = "blue" )
