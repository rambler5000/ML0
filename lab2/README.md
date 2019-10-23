# Метод парзеновского окна
Метод Парзеновского окна метрический алгорит классификации,основанный на оценивании сходства объектов.
Формула алгоритма:

где w(i,u) - весовая функция, которая оценивает расстояние. 

<a href="https://www.codecogs.com/eqnedit.php?latex=w(i,u)&space;=&space;K(p(u,x_u^i)/h)" target="_blank"><img src="https://latex.codecogs.com/png.latex?w(i,u)&space;=&space;K(p(u,x_u^i)/h)" title="w(i,u) = K(p(u,x_u^i)/h)" /></a>


Реализация функции

```
parsen <- function(xl, z, h,K)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- sortObjectsByDist(xl, z)
  
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  
  classes <- xl[1:150, n + 1]
  for(i in 1:150)
  {
    w <- K(distances[i,2]/h)
    m[classes[i]] <- m[classes[i]] + w
  }
  if(m[1]!=0 || m[2]!=0 || m[3]!=0)class <- names(which.max(m))
  else class <- "not_class"
  if(class==0)class <- "grey"
  return (class)
}
```

# Выберем оптимальное h, воспользовавшись критерием скользящего контроля LOO для некоторых функций ядер и построим карту классификации


# Епаничникова
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/epanechnikov_map.png)

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/LOO_for_epanechnikov.png)



# Квадратичное
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/qvad_map.png)

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/LOO_for_qvad.png)

# Треугольное
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/triygolnik_map.png)

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/LOO_for_triygolnik.png)

# Гауссовское
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/gaus_map.png)


![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/LOO_for_gaus.png)


# Прямоугольное
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/primoygolnik_map.png)

![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab2/LOO_for_primoygolnik.png)
