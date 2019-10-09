euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

kwNN <- function(xl, z, k,q)
{
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  xl <- sortObjectsByDist(xl, z)
  n <- dim(xl)[2] - 1
  classes <- xl[1:k, n + 1]
  for(i in 1:k)
  {
    w<-q ^ i
    m[classes[i]]<-m[classes[i]]+w
  }
  class <- names(which.max(m))
  return (class)
}



colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
z <- c(2.5, 1) 
xl <- iris[, 3:5] 
class <- kwNN(xl, z, k=6,0.9)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)

x<-0.8
while(x<7) 
{
    y<--0.2
    while (y<2.9) 
    {
      z<-c(x,y)  
      class <- kNN(xl, z, k=6)

      points(z[1], z[2], pch = 21, col = colors[class])
      y<-y+0.1
    }
    x<-x+0.1
}
