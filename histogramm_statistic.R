
histogramm_statistic<-function(n,mean_x,sd_x,mean_e,sd_e)##выводит гистограмму, функцию плотности распределения;выводит фрейм с коэффициентом вариации; параметры: количество наблюдений, средние значения переменных x y и их дисперсии
{
set.seed(6)
x<-rnorm(n,mean_x,sd_x)##параметры x
e<-rnorm(n,mean_e,sd_e)##параметры e
y<-100-12*x+e
plot (x,y)
attach(mtcars)
layout(matrix(2:1,1, 2, byrow = T))
hist(x, col="peachpuff", border="black", prob = TRUE, xlab="temp", main = "Histogramm #2")
lines(density(x), col="red", lwd =2)
curve(dnorm(x, mean = mean(x), sd = sd(x)), col = "darkblue", lwd = 2, add = T)
hist(y, col="peachpuff", border="black", prob = TRUE, xlab="temp", main = "Histogramm #1")
lines(density(y), col="red", lwd =2)
curve(dnorm(y, mean = mean(y), sd = sd(y)), col = "darkblue", lwd = 2, add = T, xname = "y")
detach(mtcars)

 v<-data.frame((sqrt(sd_x)/mean_x)*100)##коэффициент вариации
 v
}
