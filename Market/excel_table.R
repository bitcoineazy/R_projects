review_directory <- "~/DEV/R_projects/Market/Review"
#подгружаем данные из магазинов
for (z in 1:10) {
  file.copy(glue("~/DEV/R_projects/Market/Marketplace_{z}/import.in"), glue("{review_directory}/marketplace_{z}.in"))
  file.copy(glue("~/DEV/R_projects/Market/Marketplace_{z}/export.out"), glue("{review_directory}/marketplace_{z}.out"))
}
#запись в переменные
for (i in 1:10){
  assign(paste0("in", as.character(i)), read.table(glue("{review_directory}/marketplace_{i}.in"), head=TRUE, encoding="UTF-8"))
  assign(paste0("out", as.character(i)), read.table(glue("{review_directory}/marketplace_{i}.out"), head=TRUE, encoding="UTF-8"))
}
print(in1)
rev <- rep(0, 12)
profit <- rep(0, length(rev))
res.tab <- data.frame("Объём продаж" = rev, "Прибыль" = profit)
sale <-rep(0, nrow(res.tab))
res.tab$"Реализация" <- sale
res.tab[,"Реализация"] <- sale
res.tab["Реализация"] <- sale
res.tab$"Списание, конт." <- 0
res.tab$"Равномерность продаж" <- 0
res.tab$"Продажи макс" <- 0
res.tab$"День макс" <- 0
res.tab$"Продажи мин" <- 0
res.tab$"День мин" <- 0
res.tab$"Списание макс" <- 0
res.tab$"День" <- 0

price <- 900 # Цена продажи
Bprice <- 450 # Цена закупки
Uprice <- 10 # Цена утилизации

for (i in 1:10){
  tr <- sum(eval(parse(text = paste0("out", as.character(i))))[2])*price
  print(tr)
  tc <- sum(eval(parse(text = paste0("in", as.character(i))))[2])*Bprice+
    (sum(eval(parse(text = paste0("in", as.character(i))))[2])-
       sum(eval(parse(text = paste0("out", as.character(i))))[2]))*Uprice
  pr <- tr-tc
  res.tab[i,2] <- pr
  res.tab[i,1] <- tr
  # Реализация
  res.tab[i,3] <- sum(eval(parse(text = paste0("in", as.character(i))))[2])
  # Списание конт.
  res.tab[i,4] <- sum(eval(parse(text = paste0("in", as.character(i))))[2])-
    sum(eval(parse(text = paste0("out", as.character(i))))[2])
  #Равномерность продаж
  res.tab[i,5] <- sd(eval(parse(text = paste0("out", as.character(i))))[1:7,2])
  # Продажи макс.
  res.tab[i,6] <- max(eval(parse(text = paste0("out", as.character(i))))[2])
  #День макс.
  res.tab[i,7] <- which.max(unlist(eval(parse(text = paste0("out", as.character(i))))[2]))
  #Продажи мин.
  res.tab[i,8] <- min(eval(parse(text = paste0("out", as.character(i))))[2])
  # День мин.
  res.tab[i,9] <- which.min(unlist(eval(parse(text = paste0("out", as.character(i))))[2]))
  #Списание макс.
  decline <- eval(parse(text = paste0("in", as.character(i))))[1:7,2]-
    eval(parse(text = paste0("out", as.character(i))))[1:7,2]
  res.tab[i,10] <- max(decline)
  #День макс списания
  res.tab[i,11] <- which.max(decline)
}


#сумма и среднее зачение
res.tab[11,1] <- sum(res.tab[1:10, 1])
res.tab[12,1] <- mean(res.tab[1:10, 1])

res.tab
setwd("~/DEV/R_projects/Market/")
write.table(res.tab, file="result.xlsx", col.names = TRUE, row.names = FALSE, sep = ";", dec = ",")