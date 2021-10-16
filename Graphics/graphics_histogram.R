mycars <- mtcars

hist(mycars$hp, main = "Статистика мощности автомобилей", xlab = "Мощность авто (в лс)", ylab = "Кол-во машин", col = "skyblue",
     border = "blue")
sorted <- sort(mycars$hp)
#Среднее значение мощности
abline(v=mean(mycars$hp), col="red", lty=4, lwd=3)
#Медиана по мощности
abline(v=sorted[length(sorted)/2], col="black", lty=3, lwd=3)

