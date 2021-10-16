# Загрузка диаграммы из файла
dat <- read.csv("/home/noble6/DEV/R_projects/Graphics/financials.csv")
tab <- table(dat$Sector)
tab

# Процентная доля секторов
perc <- tab / sum(tab) * 100
perc
perc_round <- round(perc, 2)
perc_labs <- paste0(perc_round, "%")
perc_labs

# Вектор с названиями секторов
sects <- names(tab)
sects

# Цвета для круговой диаграммы
sect_cols <- c("thistle1", "plum1", "palevioletred3", "maroon4",
               "purple2", "paleturquoise",
               "cornflowerblue", "lightcyan3", "royalblue1",
               "seagreen2", "navy")

pdf("pie.pdf")
pie(tab, main = "S&P 500", col = sect_cols, labels = perc_labs)
legend(x=-1.2, y = -0.8, sects, cex = 0.7,
       fill = sect_cols, ncol = 3, bty = "n")
dev.off()