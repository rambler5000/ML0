# kNN
Метод k ближайших соседей(kNN).
Алгоритм kNN - метрический алгоритм классификации, основанный на оценивании сходства объектов.
Относит классифицируемый обьект к тому классу, элементов которого больше среди K ближайших соседей.
Формула алгоритма kNN: 

<a href="https://www.codecogs.com/eqnedit.php?latex=w(i,u)&space;=&space;[i\leqslant&space;k]\&space;a(u,x^{l},k)&space;=&space;arg\max\limits_{y\in&space;Y}\sum\limits_{i=1}^{k}[y^i_u&space;=&space;y]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w(i,u)&space;=&space;[i\leqslant&space;k]\&space;a(u,x^{l},k)&space;=&space;arg\max\limits_{y\in&space;Y}\sum\limits_{i=1}^{k}[y^i_u&space;=&space;y]" title="w(i,u) = [i\leqslant k]\ a(u,x^{l},k) = arg\max\limits_{y\in Y}\sum\limits_{i=1}^{k}[y^i_u = y]" /></a>

где k -параметр
Реализация kNN функции
```
kNN <- function(xl, z, k)
{
	orderedXl <- sortObjectsByDist(xl, z)
	n <- dim(orderedXl)[2] - 1
	classes <- orderedXl[1:k, n + 1]
	counts <- table(classes)
	class <- names(which.max(counts))
	return (class)
}
```
# Пример
Рассмотрим точку Z(2.5, 1) на выборке "Ирисы Фишера". 
Применим метод kNN и получим , что Z принадлежит к классу "зеленых кружочков".

![Image alt](https://github.com/KOCTYN/ML0/blob/master/knn.png)
# Карта классификации
![Image alt](https://github.com/KOCTYN/ML0/blob/master/kNN_map.png)
# Выберем оптимальноё k, воспользовавшись критерием скользящего контроля LOO
![Image alt](https://github.com/KOCTYN/ML0/blob/master/LOO_kNN.png)

Таким образом оптимальное k=6
# kwNN
Метод k-взвешеных ближайших соседей(kwNN).
По сравнению с kNN, kwNN принимает во внимание не только колличество соседей определенного класса но и удаленность от классифицируемого обьекта. Для каждого класса определяется оценка близости, у какого класса больше оценка близости тот класс и присваивается классифицируемому обьекту.

Формула алгоритма kwNN:

<a href="https://www.codecogs.com/eqnedit.php?latex=w(i,u)&space;=&space;[i\leqslant&space;k]w(i)\&space;a(u,x^{l},k)&space;=&space;arg\max\limits_{y\in&space;Y}\sum\limits_{i=1}^{k}[y^i_u&space;=&space;y]w(i)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?w(i,u)&space;=&space;[i\leqslant&space;k]w(i)\&space;a(u,x^{l},k)&space;=&space;arg\max\limits_{y\in&space;Y}\sum\limits_{i=1}^{k}[y^i_u&space;=&space;y]w(i)" title="w(i,u) = [i\leqslant k]w(i)\ a(u,x^{l},k) = arg\max\limits_{y\in Y}\sum\limits_{i=1}^{k}[y^i_u = y]w(i)" /></a>

где w(i) = q^i - геометрическая прогрессия с параметром q

Реализация kwNN фунции
```
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
```
# Пример
Рассмотрим точку Z(2.5, 1) на выборке "Ирисы Фишера". 
Применим метод kwNN и получим , что Z принадлежит к классу "зеленых кружочков".

![Image alt](https://github.com/KOCTYN/ML0/blob/master/kwNN.png)

# Карта классификации kwNN
![Image alt](https://github.com/KOCTYN/ML0/blob/master/kwNN_map.png)

# Пример,	показывающий	преимущество	метода kwNN над kNN
kNN

![Image alt](https://github.com/KOCTYN/ML0/blob/master/kNN_example.png)
kwNN

![Image alt](https://github.com/KOCTYN/ML0/blob/master/kwNN_example.png)
