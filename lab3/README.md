# Метод потенциальных функций
Если в методе Парзеновского окна центр окна поместить в классифицируемый объект, то получим метод:

<a href="https://www.codecogs.com/eqnedit.php?latex=a(u)&space;=&space;arg&space;\max\limits_{y&space;\in&space;Y}&space;\sum\limits_{i=1}^m[y(x_u^i)=y]w(i,u)" target="_blank"><img src="https://latex.codecogs.com/png.latex?a(u)&space;=&space;arg&space;\max\limits_{y&space;\in&space;Y}&space;\sum\limits_{i=1}^m[y(x_u^i)=y]w(i,u)" title="a(u) = arg \max\limits_{y \in Y} \sum\limits_{i=1}^m[y(x_u^i)=y]w(i,u)" /></a>

где <a href="https://www.codecogs.com/eqnedit.php?latex=w(i,u)=&space;\gamma(x_u^i)K(p(u,x_u^i)/h_i)" target="_blank"><img src="https://latex.codecogs.com/png.latex?w(i,u)=&space;\gamma(x_u^i)K(p(u,x_u^i)/h_i)" title="w(i,u)= \gamma(x_u^i)K(p(u,x_u^i)/h_i)" /></a>

реализация функции:

```
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
    print(gamma[i]/E/gammamax)
    color<-adjustcolor(colors[xl[i,3]],gamma[i]/E/gammamax)
    draw.circle(x,y,r,50,border = color, col = color)
  }
}
```

# Карты классификации и визуализация потенциалов


# Гауссовское максимум ошибок = 5 
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab3/gaus_map.png)

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab3/gaus.png)


# Треугольное максимум ошибок =5
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab3/triygolnik.png)


![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab3/triygolnik_map.png)
