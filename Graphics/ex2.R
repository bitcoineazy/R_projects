###### HISTOGRAMS ######
#загрузка данных
dat <- read.csv("/home/noble6/DEV/R_projects/Graphics/financials.csv")
View(dat)
#Строим гистограмму розового цвета с заголовком и подписями

hist(dat$Dividend.Yield)

hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts")
     
# таблица цветов    
colors()
#
?hist()
# breaks отвечает за фикс количество столбцов 
hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20)

# (a, b] right = TRUE
# (10, 20] (20, 30]
# [a, b) right = FALSE
# [10, 20) [20, 30)
# right открытый или закрытый интервал
hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20, 
     right  = FALSE)
# freq по оси y не частота а значение плотности  
hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20, 
     right = FALSE, freq = FALSE)
#отметим границу столбцов синим 
hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20, 
     right = FALSE, freq = FALSE, border = "navy")
#отметим на гистограмме среднее значение зеленой линией

hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20, 
     right = FALSE, freq = FALSE, border = "navy")
abline(v = mean(dat$Dividend.Yield), col = "limegreen", 
       lty = 2, lwd = 2)

??lty
