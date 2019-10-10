require("plotrix")

euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
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






potential <- function(xl, z,gamma, h) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- sortObjectsByDist(xl, z)
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  classes <- xl[1:l, n + 1]
  for(i in 1:l)
  {
    w<-gamma[i]*c4(distances[i,2]/h)
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
n<-150
z <- c(4.9, 1.6) 
xl <- iris[, 3:5] 
kR<- function(r) (1 - abs(r)) * (abs(r) <= 1)

h<-1
gamma<-rep(0,n)
p<-c(0,n)
E<-5
Q<-E+1
while(Q>E)
{
  t<-1
  while(t)
  {
    i<-sample(1:n, 1)

    z<-c(xl[i,1],xl[i,2])
    class<-potential(xl,z,gamma,h)
    if(class!=xl[i,3])
    {
      gamma[i]<-gamma[i]+1
      t<-0
    }
  }
  Q<-0
  for(i in 1:n)
  {
    z<-c(xl[i,1],xl[i,2])
    class<-potential(xl,z,gamma,h)
    if(class!=xl[i,3])Q<-Q+1
  }
}
n<-length(gamma)
gammamax<-max(gamma)
for(i in 1:n)
{
  x<-xl[i,1]
  y<-xl[i,2]
  r<-h
  
  if(gamma[i]>0)
  {
    color<-adjustcolor(colors[xl[i,3]],gamma[i]/E/gammamax)
    draw.circle(x,y,r,50,border = color, col = color)
  }
}
readline(prompt="Enter to continue")
x<-0.8
while(x<7) 
{
  y<--0.2
  while (y<2.9) 
  {
    z<-c(x,y)  
    class<-potential(xl,z,gamma,h)
    points(z[1], z[2], pch = 21, col = colors[class])
    y<-y+0.1
  }
  x<-x+0.1
}

