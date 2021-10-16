# 1.
print(67 ^ 3 - 112 ^ 2)
print(log(125))
print(log(81, 3))

# 2.
flights_d <- c(140, 150, 100, 90, 230, 240, 165)
flights_a <- c(65, 145, 80, 87, 220, 268, 216)

print(flights_d[3])
print(flights_d[2] - flights_a[2])
print(sum(flights_d))
print(which(flights_a <= 220))

# 3.
pos <- c(4.765, 3.230, 1.256, 1.780, 2.583, 2.781, 3.945, 2.345)

ax <- length(pos)
ay <- max(pos)
az <- min(pos)

pos.round <- floor(pos)
pos_g <- pos * 1000

print(cat(ax, ay, az, pos.round, pos_g, sep=" "))

# 4.
milk <- c(89.5, 50.5, 31.5, 21.0, 22.1, 27.4)

sum <- sum(milk)
len <- length(milk)

average <- (sum / len)

dz <- (milk - average) ^ 2
dz_s <- sum(dz)

d_2 <- dz_s / (len - 1)
dispersion <- sqrt(d_2)

print(paste("Выборочная дисперсия равна:", dispersion))

# 5.
?euro
euro

e1 <- 100 / (euro["FIM"])
e2 <- 50 / (euro["BEF"])
print(e1)
print(e2)

w_currency <- max(euro)
w_index <- which(euro == w_currency)
currency <- euro[w_index]

print(paste("Индекс самой слабой валюты по отношению к евро:", w_index))
print(currency)

# 6.
countries <- c(rep("France", 5), rep("Italy", 5), rep("Spain", 5))
years <- rep(2000:2004, 3)

print(paste("Вектор country с названиями стран: ", countries))
print(paste("Вектор year с годами: ", years))
