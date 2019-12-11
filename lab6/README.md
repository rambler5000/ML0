## Наивный нормальный байесовский классификатор

Наивный байесовский алгоритм – это алгоритм классификации, основанный на теореме Байеса с допущением о независимости признаков.
Наивный байесовский классификатор максимизирует апостериорную вероятность класса, в этом случае классификатор имеет вид:

<a href="https://www.codecogs.com/eqnedit.php?latex=h(x)=\arg\max_{y&space;\in&space;Y}p(y|x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?h(x)=\arg\max_{y&space;\in&space;Y}p(y|x)" title="h(x)=\arg\max_{y \in Y}p(y|x)" /></a>

По теореме байеса формулу можно переписать в виде:

<a href="https://www.codecogs.com/eqnedit.php?latex=h(x)=\arg\max_{y&space;\in&space;Y}p(y|x)=\arg\max_{y&space;\in&space;Y}\frac{p(x|y)p(y)}{p(x)}=\arg\max_{y&space;\in&space;Y}p(x|y)p(y)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?h(x)=\arg\max_{y&space;\in&space;Y}p(y|x)=\arg\max_{y&space;\in&space;Y}\frac{p(x|y)p(y)}{p(x)}=\arg\max_{y&space;\in&space;Y}p(x|y)p(y)" title="h(x)=\arg\max_{y \in Y}p(y|x)=\arg\max_{y \in Y}\frac{p(x|y)p(y)}{p(x)}=\arg\max_{y \in Y}p(x|y)p(y)" /></a>

Пусть объект x описывается n признаковыми функциями <a href="https://www.codecogs.com/eqnedit.php?latex=f_1(x),\dots,f_n(x)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?f_1(x),\dots,f_n(x)" title="f_1(x),\dots,f_n(x)" /></a>,
предположим, что значения данных функций являются независимыми случайными велечинами. Тогда получим классификатор вида:

<a href="https://www.codecogs.com/eqnedit.php?latex=h(x)=\arg\max_{y&space;\in&space;Y}\prod_{i=1}^np(f_i(x)=x_i|y)p(y)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?h(x)=\arg\max_{y&space;\in&space;Y}\prod_{i=1}^np(f_i(x)=x_i|y)p(y)" title="h(x)=\arg\max_{y \in Y}\prod_{i=1}^np(f_i(x)=x_i|y)p(y)" /></a>

Данный классификатор называется наивным байесовским.

В гауссовом наивном байесовском методе предполагается, что непрерывные значения, связанные с каждым признаком, распределены в соответствии с распределением Гаусса . Гауссово распределение также называется нормальным распределением.
Вероятность признаков предполагается гауссовой, следовательно, условная вероятность определяется как:

<a href="https://www.codecogs.com/eqnedit.php?latex=P(x_i|y)=\frac{1}{\sqrt[]{2\pi\sigma^2_y}}exp(-\frac{(x_i-\mu_y)^2}{2\sigma^2_y})" target="_blank"><img src="https://latex.codecogs.com/gif.latex?P(x_i|y)=\frac{1}{\sqrt[]{2\pi\sigma^2_y}}exp(-\frac{(x_i-\mu_y)^2}{2\sigma^2_y})" title="P(x_i|y)=\frac{1}{\sqrt[]{2\pi\sigma^2_y}}exp(-\frac{(x_i-\mu_y)^2}{2\sigma^2_y})" /></a>

Достоинством наивного байесовского классификатора является малое количество данных необходимых для обучения, оценки параметров и классификации.

# Реализация на R

```R
naiv <- function(x,m,s,lamda,PP)
{
  n <- dim(mu)[2]
  p <- rep(0,n)
  a <- matrix(c(0,0,0,0),2,2)
  for(i in 1:n)
  {
    sigma <- s[i]
    mu <- matrix(c(m[i,1],m[i,2]),1,2)
    l <- lamda[i]
    P <- PP[i]
    pyj <- (1/(sqrt(2*pi*sigma^2)))*exp(-((x-mu)^2)/(2*sigma^2))
    p[i] <- log(lamda*P)+log(pyj[1,1])+log(pyj[1,2])
    
  }
  if(p[1] > p[2])
  {
    class <- colors[1]
  }
  else
  {
    class <- colors[2]
  }
  return(class)
}
```

# Пример работы классификатора

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_1(0,0),\mu_2(5,5),\Sigma_1(2,0,0,2),\Sigma_2(1,0,0,1)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_1(0,0),\mu_2(5,5),\Sigma_1(2,0,0,2),\Sigma_2(1,0,0,1)" title="\mu_1(0,0),\mu_2(5,5),\Sigma_1(2,0,0,2),\Sigma_2(1,0,0,1)" /></a>
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab6/naiv.png)
