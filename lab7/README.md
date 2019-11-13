## Подстановочный алгоритм (plug-in)

Алгоритм заключается в восстановлении параметров нормального распределения ,  для каждого класса  и подстановке их в формулу оптимального байесовского классификатора. Предполагается, что ковариационные матрицы классов не равны

Параметры нормального распределения оценивают согласно принципа максимума правдоподобия:

Разделяющая поверхность между двумя классами s и t задаётся следующим образом:

Прологарифмируя обе части выражения и проведя преобразования получим уровнение разделяющей поверхности.
Вероятностное распределение с плотностью 
<a href="https://www.codecogs.com/eqnedit.php?latex=N(x,\mu,\Sigma)&space;=&space;\frac{1}{(2\pi)^n\left&space;|&space;\Sigma&space;\right&space;|}exp(-\frac{1}{2}(x-\mu)^T&space;\Sigma^{-1}(x-\mu))" target="_blank"><img src="https://latex.codecogs.com/gif.latex?N(x,\mu,\Sigma)&space;=&space;\frac{1}{(2\pi)^n\left&space;|&space;\Sigma&space;\right&space;|}exp(-\frac{1}{2}(x-\mu)^T&space;\Sigma^{-1}(x-\mu))" title="N(x,\mu,\Sigma) = \frac{1}{(2\pi)^n\left | \Sigma \right |}exp(-\frac{1}{2}(x-\mu)^T \Sigma^{-1}(x-\mu))" /></a>

Примеры работы подстановочного алгоритма:
# Элипс
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/elips.png)

# Прямая
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/prymay.png)

# Гипербола
![Image alt](https://github.com/KOCTYN/ML0/blob/master/lab7/giperbola.png)

