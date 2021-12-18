work_dir <- "~/DEV/R_projects/Market"
num <- 1 # магазин для анализа
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

#вектора нужных графиков
colors <- c("red", "purple", "blue", "yellow", "gold1",
            "lightcoral", "mediumvioletred", "navyblue",
            "tan1", "turquoise1", "chocolate1", "blue",
            "black", "brown", "darkseagreen")

#подсчеты 1 пункта
for(j in 1:5){
  line <- c()
  c <- 1
  num <- 0
  for (i in goods){
    funct <- switch(j,
                    output1[, i],
                    (input1[,i] - output1[, i]),
                    (output1[, i]*SALE) - (input1[, i]*BUY) - ((input1[, i] - output1[,i])*UTIL),
                    output1[, i]*SALE,
                    100*(((output1[, i]*SALE) - (input1[, i]*BUY) - ((input1[, i] - output1[,i])*UTIL))/(output1[, i]*SALE)))
    line <- append(line, funct)
    num <- length(funct)
    c <- c+1

  }
  c <- c-1

  names <- switch(j, "Объем продаж", "Списания", "Прибыль", "Объём продаж", "Рентабельность")
  plot(x = line[1:num], type = "o", col = "red", xlab = "Дни", ylab = names, pch = CIRCLE <- 1)
  for (i in range(2:c)){
    lines(x = line[((i-1)*num):(i*num)],type="o", col=colors[i],pch=pcc[i])
    legend("topleft", legend=c("Молоко","Хлеб"),col=colors, pch=pcc)
  }

}
#подсчеты 2 пункта
v <- c()
for (i in 1:10){
  assign(paste0("input", as.character(i)), read.table(paste0("~/DEV/R_projects/Market/Marketplace_",as.character(i),"/import_all.in"), head=TRUE))
  assign(paste0("output", as.character(i)), read.table(paste0("~/DEV/R_projects/Market/Marketplace_",as.character(i),"/export_all.out"), head=TRUE))
  v <- append(v, eval(parse(text = paste0("output", as.character(i))))[, goods[1]])
  if(i==1){
    plot(x = v, type = "o", col = "red", xlab = "Дни",ylim =c(0,80), ylab = paste0("Объем продаж, шт. Товар: ",goods[1]), pch = pcc[i])
  }
  if(i==10){
    for(j in  2:10){
      lines(x = v[((j-1)*length(v)/10):(j*length(v)/10)], type = "o", col = colors[j], xlab = "Дни", ylab = paste0("Объем продаж, шт. Товар: ",goods[i]))
      legend("topright", legend=marketplaces.names, col=colors, pch=pcc[i])
    }
  }
}
#подсчеты 3 пункта
v <- c()
for (i in 1:10) {
  assign(paste0("input", as.character(i)), read.table(paste0("~/DEV/R_projects/Market/Marketplace_",as.character(i),"/import_all.in"), head=TRUE))
  assign(paste0("output", as.character(i)), read.table(paste0("~/DEV/R_projects/Market/Marketplace_",as.character(i),"/export_all.out"), head=TRUE))
  v <- append(v, eval(parse(text = paste0("output", as.character(i)))[,goods[2]]))
  v <- append(v, eval(parse(text = paste0("output", as.character(i)))[,goods[2]]))

  if (i==1) {
    plot(x = v[1:(length(v)/2)], type = "o", col = colors[i], xlab = "Дни",ylim =c(0,80), ylab = paste0("Объем продаж, шт. Товары: ",goods), pch = 1)
    print(i)
    lines(x = v[(length(v)/2):length(v)], type = "o", col = colors[i],  pch =2)
  }
  if(i==10){
    for(j in  2:10){
      lines(x = v[(length(v)*(j-1)/10):(length(v)*(j-1)/20)], type = "o", col = colors[j],  pch =1)
      lines(x = v[(length(v)*(j-1)/20):(length(v)*j/20)], type = "o", col = colors[j],  pch =2)
      legend("topright", legend=c(goods[1],goods[2]))
      legend("topleft", legend=marketplaces.names, col=colors)
    }
  }
}
