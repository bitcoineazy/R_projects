z <- c(-10, 10, 0.2)

y <- z ^ 2
y
y_2 <- z ^ 3
x <- c(0, 10, 50)

jpeg("plot.jpg", width = 800, height = 800)
plot(x, y, type = "l", col = "red")
lines(x, y_2, type = "l", col = "green", lty=2)

legend(x = "topright", legend = c("z^2", "z^3"), lty = c(1, 2), col = c(2,3), lwd = 2)
dev.off()
#plot(y, type = "l")

