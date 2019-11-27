library("MASS")

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
  sigma <- matrix(0, 1, 1)
  for(i in 1:n)
  {
    sigma <- sigma + ((x[i,]-mu)%*%(t((x[i,]-mu))))
    
    
  }
  return(sigma/(n-1))
}

naiv <- function(x,mu,sigma,P)
{
  p <- log(1,P)
  p <- 0
  sigma <- as.numeric(sigma)
  pyj <- (1/(sqrt(2*pi*sigma^2)))*exp(-((x-mu)^2)/(2*sigma^2))
  p <- p+log(pyj[1,1])+log(pyj[1,2])
  
  return(p)
}

n <- 300
sigma1 <- matrix(c(2, 0, 0, 2),2,2)
sigma2 <- matrix(c(1, 0, 0, 1),2,2)

mu1 <- c(0, 0)
mu2 <- c(5, 5)

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

sigma1 <-estimateSigma(xy1,mu1)
sigma2 <-estimateSigma(xy2,mu2)

x <- -10
while(x < 10) 
{
  y<--10
  while (y < 10) 
  {
    z <- c(x,y)  
    p1 <- naiv(z,mu1,sigma1,0.5)
    p2 <- naiv(z,mu2,sigma2,0.5)
    if(p1 > p2)  
    {
      class <- 1
    }else{
      class <- 2
    }
    points(z[1], z[2], pch = 21, col = colors[class])
    y <- y+0.3
  }
  x <- x+0.3
}
