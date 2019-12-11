library("MASS")

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
  sigma <- matrix(0, 1, 1)
  for(i in 1:n)
  {
    sigma <- sigma + ((x[i,]-mu)%*%(t((x[i,]-mu))))
    
    
  }
  return(sigma/(n-1))
}

naiv <- function(x,m,s,lamda,PP)
{
  
  
  n<-dim(mu)[2]
  p<-rep(0,n)
  a<-matrix(c(0,0,0,0),2,2)
  for(i in 1:n)
  {

    sigma<-s[i]
    mu<-matrix(c(m[i,1],m[i,2]),1,2)
    l<-lamda[i]
    P<-PP[i]
    
    pyj <- (1/(sqrt(2*pi*sigma^2)))*exp(-((x-mu)^2)/(2*sigma^2))
    p[i] <- log(lamda*P)+log(pyj[1,1])+log(pyj[1,2])
    
  }
  
  
  if(p[1]>p[2])
  {
    
    class<-colors[1]
  }
  else
  {
    class<-colors[2]
  }
  return(class)
  

  
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

mu1 <- vostanovleniemu(xy1)
mu2 <- vostanovleniemu(xy2)

sigma1 <-vostanovlenieсovariance(xy1,mu1)
sigma2 <-vostanovlenieсovariance(xy2,mu2)

l1<-20
l2<-6

P1<-dim(xy1)[1]/(dim(xy1)[1]+dim(xy2)[1])
P2<-dim(xy2)[1]/(dim(xy1)[1]+dim(xy2)[1])

sigma<-rbind(sigma1,sigma2)
mu<-rbind(mu1,mu2)
l<-rbind(l1,l2)
P<-rbind(P1,P2)

x <- -10
while(x < 10) 
{
  y<--10
  while (y < 10) 
  {
    z <- c(x,y)  
    
    c <- naiv(z,mu,sigma,l,P)
    points(z[1], z[2], pch = 21, col = c)

    y <- y+0.3
  }
  x <- x+0.3
}
