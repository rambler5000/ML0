## Линейный дискриминант Фишера - ЛДФ

Линейный дискриминант Фишера похож на подстановочный алгоритм, но имеет отличие в том, что мы предполагаем равенство ковариационных матриц, тогда алгоритм классификации примет вид

или

Параметры нормального распределения оценивают согласно принципа максимума правдоподобия:

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y&space;=&space;\frac{1}{l_y}\sum_{x_i;y_i=y}x_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y&space;=&space;\frac{1}{l_y}\sum_{x_i;y_i=y}x_i" title="\mu_y = \frac{1}{l_y}\sum_{x_i;y_i=y}x_i" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\mu_y&space;=&space;\frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\mu_y&space;=&space;\frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" title="\mu_y = \frac{1}{l_y-1}\sum_{x_i;y_i=y}(x_i-\mu_y)(x_i-\mu_y)^T" /></a>

Разделяющая поверхность задается так же как в подстановочном алгоритме.