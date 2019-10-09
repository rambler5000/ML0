# kNN
Метод k ближайших соседей(kNN).
Алгоритм kNN - метрический алгоритм классификации, основанный на оценивании сходства объектов.
Относит классифицируемый обьект к тому классу, элементов которого больше среди K ближайших соседей.
Формула алгоритма kNN: 

<a href="https://www.codecogs.com/eqnedit.php?latex=$a(u)&space;=&space;arg&space;\max\limits_{y&space;\in&space;Y}[x_u^{(i)}=y]w(i,u)$" target="_blank"><img src="https://latex.codecogs.com/png.latex?$a(u)&space;=&space;arg&space;\max\limits_{y&space;\in&space;Y}[x_u^{(i)}=y]w(i,u)$" title="$a(u) = arg \max\limits_{y \in Y}[x_u^{(i)}=y]w(i,u)$" /></a>

где k -параметр определяющий колличество соседей

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
# Карта классификации kNN
![Image alt](https://github.com/KOCTYN/ML0/blob/master/kNN_map.png)
# Выберем оптимальное k, воспользовавшись критерием скользящего контроля LOO
![Image alt](https://github.com/KOCTYN/ML0/blob/master/LOO_kNN.png)

Таким образом оптимальное k=6
# kwNN
Метод k-взвешеных ближайших соседей(kwNN).
По сравнению с kNN, kwNN принимает во внимание не только колличество соседей определенного класса но и удаленность от классифицируемого обьекта. Выберается k ближайших соседей. Каждому соседу присваивается вес(мера удаленности соседа от классифицируемого обьекта). Объекту присваивается класс вес которого больше.

Формула алгоритма kwNN:

<a href="https://www.codecogs.com/eqnedit.php?latex=$a(u)&space;=&space;arg&space;\max\limits_{y&space;\in&space;Y}[x_u^{(i)}=y]w(i,u)$" target="_blank"><img src="https://latex.codecogs.com/png.latex?$a(u)&space;=&space;arg&space;\max\limits_{y&space;\in&space;Y}[x_u^{(i)}=y]w(i,u)$" title="$a(u) = arg \max\limits_{y \in Y}[x_u^{(i)}=y]w(i,u)$" /></a>

где w(i) функция веса, строго убывающая последовательность вещественных весов, задающая вклад i-го соседа при классификации оюъекта u.
Например: q^i, где q из диапазона (0,1)

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
