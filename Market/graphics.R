num <- 1 #номер магазина для анализа
work_dir <- "~/DEV/R_projects/Market"
marketplaces <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
marketplaces.names <- marketplaces
for (i in 1:10) {
  marketplaces[i] <- glue("{work_dir}/Marketplace_{i}/")
  marketplaces.names[i] <- glue("Marketplace_{i}")
}

input1 <- read.table(file = glue("{marketplaces[num]}/import_all.in"), header = TRUE)
output1 <- read.table(file = glue("{marketplaces[num]}/export_all.out"), header = TRUE)

BUY <- 150 # Цена закупки
SALE <- 300 # Цена продажи
UTIL <- 10 # Цена утилизации

#названия товаров
goods <- names(input1)
goods <- goods[2:length(goods)]

#подсчеты
for (i in goods){
  print(i)
  o <- output1[, i] #магазин объем продаж
  s <- (input1[, i] - output1[, i]) #списания
  name <- paste0("Объем продаж и списаний в магазине ", num, ". Товар: ", i, ", шт")
  plot(x = o, type = "o", col = "red", xlab = "Дни", ylab = name, pch = CIRCLE <- 16)
  grid(col="lightblue", nx=30)
  lines(x = s,type="o", col="blue")
  legend("topright", legend=c("Списание","Объем продаж"),col=c("blue","red"), pch=c(1,16))
  v <- output1[, i]*SALE # Объём продаж
  p <- (output1[, i]*SALE) - (input1[, i]*BUY) - ((input1[, i] - output1[, i])*UTIL) #прибыль
  name <- paste0("Объём продаж и прибыль в магазине ", num, ". Товар: ", i, ", руб")
  plot(x = p, ylim=c(min(min(p),min(v)),max(max(p),max(v))), type = "o", col = "purple", xlab = "Дни", ylab = name, pch = CIRCLE <- 16)
  grid(col="lightblue", nx=30)
  lines(x = v,type="o", col="green",pch=1)
  legend("topright", legend=c("Объём продаж","Прибыль"),col=c("green","purple"), pch=c(1,16))
  r <- 100 * (p/v)
  name <- paste0("Рентабельность в магазине ", num, ". Товар: ", i, ", %")
  plot(x = r, type = "o", col = "red", xlab = "Дни", ylab = name, pch = CIRCLE <- 16)
  grid(col="lightblue", nx=30)
}

