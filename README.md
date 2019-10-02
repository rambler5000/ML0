# kNN
Метод k ближайших соседей(kNN).
Алгоритм kNN - метрический алгоритм классификации, основанный на оценивании сходства объектов.
Относит классифицируемый обьект к тому классу, элементов которого больше среди K ближайших соседей.
Формула алгоритма kNN: 

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
![Image alt](https://github.com/KOCTYN/ML0/blob/master/LOO.png)
Таким образом оптимальное k=6
# kwNN
Метод k-взвешеных ближайших соседей(kwNN).
По сравнению с kNN, kwNN принимает во внимание не только колличество соседей определенного класса но и удаленность от классифицируемого обьекта. Для каждого класса определяется оценка близости, у какого класса больше оценка близости тот класс и присваивается классифицируемому обьекту.

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
# Карта классификации kwNN
![Image alt](https://github.com/KOCTYN/ML0/blob/master/kwNN_map.png)
