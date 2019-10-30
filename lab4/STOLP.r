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

kwNN <- function(xl, z, k=6,q=0.5)
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
  return (m)
}

margins <- function(xl,func)
{
  n <- dim(xl)[1]
  margin <- rep(0, n)
  for(i in 1:n)
  {
    z <- c(xl[i,1],xl[i,2])
    w <- func(xl[, 1:3][-i,],z)
    margin[i] <- w[xl[i,3]]- max(w[which(names(w)!=xl[i,3])])
  }
  return(margin)
}

stolp <- function(xl, l, delta, func,classes)
{
 
  margin <- margins(xl,func)
  vibros <- which(margin<delta) 
  xl <- xl[-vibros,]
  margin <- margin[-vibros]
  omega<-xl[1,]
  for(i in seq(length(classes)))
  {
    omega[i,]<-(xl[which(xl[,3]==classes[i]),])[which.max(margin),]
  }
  margin<-order(margin)
  while(dim(omega)[1]<dim(xl)[1])
  {
    n<-dim(xl)[1]
    err<-0
    for(i in 1:n)
    {
      z <- c(xl[i,1],xl[i,2])
      class<-names(which.max(func(omega,z)))
      if(class != xl[i,3])
      {
        err<-err+1
      }
    }
    if(err==0)
    {
      return(omega)
    }
    
    omega[dim(omega)[1]+1,]<-xl[margin[1],]
    margin<-margin[-1]
    
  }
  
  return(omega)
}

colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")

xl <- iris[, 3:5] 
classes<-c("setosa", "versicolor", "virginica")
etaloni<-stolp(xl,0,-0.1,kwNN,classes)
readline(prompt="Enter to continue")
plot(iris[, 3:4], pch = 21,  col = colors[iris$Species], asp = 1)
for(i in 1:dim(etaloni)[1])
{
  z<-c(etaloni[i,1],etaloni[i,2])
  points(z[1], z[2], pch = 21, col = colors[etaloni[i,3]],bg = colors[etaloni[i,3]])
}
readline(prompt="Enter to continue")

plot(etaloni[, 1:2], pch = 21,  col = colors[etaloni$Species], bg = colors[etaloni$Species],asp = 1)
start_time <- Sys.time()
x<-0.8
while(x<7)
{
  y<--0.2
  while (y<2.9)
  {
    z<-c(x,y)
    class <- names(which.max(kwNN(etaloni, z)))
    points(z[1], z[2], pch = 21, col = colors[class])
    y<-y+0.1
  }
  x<-x+0.1
}
end_time <- Sys.time()
time <- end_time - start_time
print(time)
readline(prompt="Enter to continue")
plot(etaloni[, 1:2], pch = 21,  col = colors[etaloni$Species], bg = colors[etaloni$Species],asp = 1)
start_time <- Sys.time()
x<-0.8
while(x<7)
{
  y<--0.2
  while (y<2.9)
  {
    z<-c(x,y)
    class <- names(which.max(kwNN(xl, z)))

    points(z[1], z[2], pch = 21, col = colors[class])
    y<-y+0.1
  }
  x<-x+0.1
}
end_time <- Sys.time()
time <- end_time - start_time
print(time)