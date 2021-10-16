###### BAR CHARTS ######
dat <- read.csv("/home/noble6/DEV/R_projects/Graphics/financials.csv")
tab <- table(dat$Sector)
tab

barplot(tab)
barplot(tab, main = "Sectors", las = 2)
barplot(tab, main = "Sectors", las = 2, 
        cex.names = 0.65, col = "red")

cols <- rep(c("red", "blue"), 5)
cols <- c(cols, "red")
cols

barplot(tab, main = "Sectors", las = 2, cex.names = 0.65, 
        col = cols, ylab = "counts")

###### PIE CHARTS ######

pie(tab)

###### PRACTICE 3 ######

tab <- table(dat$Sector)
tab
# создадим таблицу с частотами в процентах
perc <- tab / sum(tab) * 100
perc
#округляем значения до 2 знаков и приклеиваем знак %
perc_round <- round(perc, 2)
perc_labs <- paste0(perc_round, "%")
perc_labs

sects <- names(tab)
sects

sect_cols <- c("thistle1", "plum1", "palevioletred3", "maroon4", 
               "purple2", "paleturquoise",
               "cornflowerblue", "lightcyan3", "royalblue1",
               "seagreen2", "navy")

#dev.copy(pdf, "pie.pdf")

pie(tab, main = "S&P 500", col = sect_cols, labels = perc_labs)
legend(x=-1.2, y = -0.8, sects, cex = 0.7,
       fill = sect_cols, ncol = 3, bty = "n")
#dev.off()





