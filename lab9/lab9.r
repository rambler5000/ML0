library("MASS")


Normalization <- function(xl) {
  n <- dim(xl)[1]
  m <- dim(xl)[2]
  #print(x)
  for (i in seq(m)) {
    xmin <- min(xl[,i])
    xmax <- max(xl[,i])
    xl[,i] <- (xl[,i] - xmin)/(xmax - xmin)
  }
  return(xl)
}

drawLine <- function(w, col, lwd, xmin, xmax) {
  x <- seq(xmin, xmax, len = 100)
  f <- function(x) {
    return( - x*w[1]/w[2] + w[3]/w[2] )
  }
  y <- f(x)
  lines(x, y, type="l", lwd=lwd, col=col, xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))
}

adaline_loss <- function(x) {
  l <- (x-1)^2
  return(l)
}
adaline_upd <- function(xi, yi, w, eta) {
  wx <- c(crossprod(w, xi))
  #ld <- 2 * (wx - yi) * xi
  l <- (wx - yi) * xi
  nextW <- w - eta * l
  return(nextW)
}

hebb_loss <- function(x) {
  return (max(-x, 0))
}
hebb_upd <- function(xi, yi, w, eta) {
  nextW <- w + eta * yi * xi
  return (nextW)
}

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

stgrad <- function(xl, eta = 1, lambda = 1/6, eps = 1e-5, loss, upd, lwd, col, xmin, xmax) {
  n <- dim(xl)[1]
  m <- dim(xl)[2] - 1
  w <- rep(1/2, m)
  Q <- 0
  Qprev <- Q

  for (i in 1:n) {
    xi <- xl[i, 1:m]
    yi <- xl[i, m+1]

    wx <- crossprod(w, xi)
    l <- wx * yi

    Q <- Q + loss(l)
  }
  
  iter <- 0
  repeat {
    iter <- iter + 1
    if (iter > 1000) {
      break;
    }
    
    mis <- array(dim = n)

    for (i in 1:n) {
      xi <- xl[i, 1:m]
      yi <- xl[i, m + 1]
      mis[i] <- crossprod(w, xi) * yi
    }

    errorIndexes <- which(mis <= 0)
    if (length(errorIndexes) > 0)
    {
      i <- sample(errorIndexes, 1)
      xi <- xl[i, 1:m]
      yi <- xl[i, m + 1]
    
      wx <- crossprod(w, xi)
      l <- wx * yi
      ex <- loss(l)
      
      w <- upd(xi, yi, w, eta)
      
      Qprev <- Q
      Q <- (1 - lambda) * Q + lambda * ex
      if (abs(Q - Qprev) < eps)
      {
        break
      }
     # drawLine(w, col, lwd, xmin, xmax)
    }
    else
    {
      break
    }
  }
  
  return(w)
}

colors = c("tan1", "blue")
n <- 100
m <- 100

sigma1 <- matrix(c(1, 0, 0, 1), 2, 2)
sigma2 <- matrix(c(1, 0, 0, 1), 2, 2)

mu1 <- c(5, 10)
mu2 <- c(7, 10)

xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=m, mu = mu2, Sigma = sigma2)

xy <- rbind(xy1, xy2)
xy <- Normalization(xy)

xy <- cbind(xy, rep(-1, n+m))

xy <- cbind(xy, c(rep(-1, n), rep(1, m)))

plotxmin <- min(xy[,1], xy[,1]) - 0.3
plotxmax <- max(xy[,1], xy[,1]) + 0.3
plotymin <- min(xy[,2], xy[,2]) - 0.5
plotymax <- max(xy[,2], xy[,2]) + 0.5
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="LOGRESS")

points(xy, pch=21, col=colors[ifelse(xy[,4] == -1, 1, 2)], bg=colors[ifelse(xy[,4] == -1, 1, 2)])

# adaline
#resW <- stgrad(xy, loss = adaline_loss, upd = adaline_upd, lwd = 0.5, col = 'chartreuse', xmin = plotxmin, xmax = plotxmax)
#drawLine(resW, col = 'chartreuse3', lwd = 3, xmin = plotxmin, xmax = plotxmax)

#resW <- stgrad(xy, loss = hebb_loss, upd = hebb_upd, lwd = 0.5, col = 'red', xmin = plotxmin, xmax = plotxmax)
#drawLine(resW, lwd = 3, col = 'red3', xmin = plotxmin, xmax = plotxmax)

resW <- stgrad(xy, loss = logregss_loss, upd = logress_upd, lwd = 0.5, col = 'magenta', xmin = plotxmin, xmax = plotxmax)
drawLine(resW, lwd = 3, col = 'magenta3', xmin = plotxmin, xmax = plotxmax)

w<-resW
x <- plotxmin
while(x < 2)
{
  y <- plotymin
  while(y < 2)
  {
    xy <- c(x,y)
    s <- sigmoid(c(crossprod(w, c(x, y, -1))) * -1) - sigmoid(c(crossprod(w, c(x, y, -1))) * 1)
    if(s>0)
    {
      c <- adjustcolor(colors[1], alpha.f = s)
    }
    else
    {
      c <- adjustcolor(colors[2], alpha.f = -s)
    }
    draw.circle(x, y, radius = 0.005, col = c, border = c)
    y <- y+0.05
  }
  x <- x+0.02
}