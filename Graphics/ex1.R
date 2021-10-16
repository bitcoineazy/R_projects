######## Базовая графика ########
n <- c(2, 5, 6, 7, 9)
plot(n)
plot(n ^ 2)

x <- seq(-4, 4, 0.1)
x

plot(x, log(x))
plot(x, log(x), type = "l")
?plot

plot(x, log(x), type = 'l')
lines(x, exp(x), col = 'red')

plot(x, log(x), type = "l", xlab = "x", ylab = "f(x)")
lines(x, exp(x), col = "red")

######## Добавление легенды ########
legend(-4, -1.5, 
       c("log(x)", "exp(x)"), 
       lwd = c(1, 1), 
       col = c("black", "red"), 
       cex = 0.7)

######## Сохранение графика ########
dev.copy(png, "graph.png")
plot(x, log(x), type = "l", 
     xlab = "x", 
     ylab = "f(x)")
lines(x, exp(x), col = "red")
legend(-4, -1.5, c("log(x)", "exp(x)"), 
       lwd = c(1, 1), 
       col = c("black", "red"), 
       cex = 0.7)
dev.off()

getwd()
