
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
# Епанечникова
c1 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return ((3/4) * (1 - r*r))
}
# Квартическое
c2 <- function(r) 
{
  if (abs(r) > 1)
  {
    return (0)
  }
  return ((15/16) * (1 - r*r)^2)
}
# Треугольное
c3 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return (1 - abs(r))
}
# Гауссовское
c4 <- function(r) 
{
  (2*pi)^0.5 * exp(-0.5 * r*r)
}
# Прямоугольное
c5 <- function(r) 
{
  if (abs(r) > 1) 
  {
    return (0)
  }
  return (0.5)
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
  return (distances);
}


parsen <- function(xl, z, h,K)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- sortObjectsByDist(xl, z)
  
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  
  classes <- xl[1:150, n + 1]
  for(i in 1:150)
  {
    w<-K(distances[i,2]/h)
    m[classes[i]]<-m[classes[i]]+w
  }
  if(m[1]!=0 || m[2]!=0 || m[3]!=0)class <- names(which.max(m))
  else class<-"not_class"
  if(class==0)class<-"grey"
  return (class)
}


colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue", "not_class" = "grey")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
xl <- iris[, 3:5] 
class<-parsen(xl,z,0.35,kR)
x<-0.8
while(x<7) 
{
  y<--0.2
  while (y<2.9) 
  {
    z<-c(x,y)
    cl <- parsen(xl,z,0.35,c5)
    
    points(z[1],z[2], pch = 21, col = colors[cl])
    y<-y+0.1
  }
  x<-x+0.1
}

