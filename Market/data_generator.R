generate_in <- function(nDays=7,min=50,max=120) {
  return(as.integer(runif(n=nDays,min=min,max=max)))
}

generate_out <- function(data.in,min) {
  data.out <- 0
  for(i in seq_along(data.in)){
    data.out[i] <- as.integer(runif(n=1,min=min,data.in[i]))
    if (is.na(data.out[i])){
      data.out[i] <- 0
    }
  }
  return(data.out)
}
#генерация по saleLevel
generate_out_level <- function(data.in, saleLevel = 50) {
  data.out <- 0

  sum.in <- sum(data.in)
  n <- 0
  repeat{
    n <- n+1
    for(i in seq_along(data.in)){
      data.out[i] <- as.integer(runif(n=1, min=0, data.in[i]))
    }
    sum.out <- sum(data.out)
    if (sum.out==as.integer(sum.in * saleLevel/100)) {
      break
    }
  }
  return(data.out)
}
#вспомогательная функция рандома
rand_num <- function(a, b){
  rand <- runif(1)
  return(round(rand * (b-a)+a))
}
#генерация файлов импорта/экспорта
generate.supply.sale <- function(name="~/DEV/R_projects/Marketplace_1/", type=".in", min=rand_num(2,10), max=rand_num(50,100), flag=T){
  dataout <- ""
  datain <- generate_in(7, min, max)
  if (type == "export.out"){
    datain <- read.table(paste0(standart_way, as.character(num), "/", "import.in"))
    print(datain)
    print(datain[,"Поставка"])
    dataout <- generate_out(datain[, "Поставка"], min)
    print(dataout)
  }
  title <- ifelse(type=="import.in", "Поставка", "Продажа")
  dio <- data.frame("День" = 1:7)
  if (type =="import.in"){

    dio[, title] <- as.integer(datain)
  }
  else{
    dio[, title] <- as.integer(dataout)
  }
  days <- c()
  for (i in dio$День) {

    day <- i %% 7
    if (day == 0){
      day <- 7
    }
    day_out <- switch (day, "понедельник", "вторник", "среда",
                       "четверг", "пятница", "суббота", "воскресенье")
    days <- c(days, day_out)
  }

  dio$День <- days
  write.table(x = dio, file = paste0(name,type),
              row.names = flag, col.names = flag)
}
#генерация файлов с большими товарами
generate.supply.sale.many <- function(name, type="import_all.in", min=5, max=100, flag=T,
                                      days_num=7, goods) {
  days <- c()
  result <- data.frame("День" = 1:days_num)
  names <- c("День")
  names(result) <- names
  for (i in result$День) {

    day <- i %% 7
    if (day == 0){
      day <- 7
    }
    day_out <- switch (day, "понедельник", "вторник", "среда",
                       "четверг", "пятница", "суббота", "воскресенье")
    days <- c(days, day_out)
  }

  result$День <- days
  result
  # Определим название заголовка столбцов
  for (good in goods) {
    if(type=="import_all.in"){
      datain <- generate_in(days_num, min, max)
      result$Item <- datain
    }
    else{
      datain <- read.table(paste0(standart_way, as.character(num), "/", "import_all.in"))
      dataout <- generate_out(datain[, good], min)
      result$Item <- dataout

    }
    names[length(names)+1] <- good
    names(result) <- names

  }
  print(result)
  write.table(x = result, file =  paste0(name,type),
              row.names = flag, col.names = flag)

}
get_sale_value <- function(value,saleLevel){
  return (value*saleLevel/100)
}
#генерация  файлов с заданным уровнем saleLevel
sale.level <- function(way, name="import.in", saleLevel=50, filename="outlevel.txt",flag=T) {
  if(name=="import.in"){
    title <- ("Продажа")
    datain <- read.table(paste0(standart_way, as.character(num), "/", name))[, "Поставка"]
    dataout <- generate_out_level(datain, saleLevel)
    days <- c()
    dio <- data.frame("День" = 1:7)
    for (i in dio$День) {

      day <- i %% 7
      if (day == 0){
        day <- 7
      }
      day_out <- switch (day, "понедельник", "вторник", "среда",
                         "четверг", "пятница", "суббота", "воскресенье")
      days <- c(days, day_out)
    }

    dio$День <- days
    dio[, title] <- dataout
    write.table(x = dio, file =  paste0(way,filename),
                row.names = flag, col.names = flag)

  }
  if(name=="import_all.in"){
    filename <- "outlevelall.txt"
    datain <- read.table(paste0(standart_way, as.character(num), "/", name))
    ndays <- nrow(datain)
    days <- c()
    result <- data.frame("День" = 1:ndays)
    names <- c("День")
    names(result) <- names
    for (i in result$День) {

      day <- i %% 7
      if (day == 0){
        day <- 7
      }
      day_out <- switch (day, "понедельник", "вторник", "среда",
                         "четверг", "пятница", "суббота", "воскресенье")
      days <- c(days, day_out)
    }

    result$День <- days
    for(i in 2:ncol(datain)){
      data <- datain[i][,]
      dataout <- generate_out_level(data, saleLevel)
      #print(dataout)
      nameout <- colnames(datain)[i]
      result$Item <- dataout
      names[length(names)+1] <- nameout
      names(result) <- names
    }
    write.table(x = result, file =  paste0(way,filename),
                row.names = flag, col.names = flag)
  }


}

standart_way <- "~/DEV/R_projects/Market/Marketplace_"
goods <- c("Молоко", "Хлеб", "Каша", "Йогурт", "Апельсины", "Огурцы")
for (num in 1:10) {
  way <- paste0(standart_way, as.character(num), "/")
  generate.supply.sale(name=way,type="import.in")
  generate.supply.sale(name=way,type="export.out")
  generate.supply.sale.many(way, min <- rand_num(2, 10),
                            max <- rand_num(50, 100),
                            type="import_all.in",days_num = 30, goods = goods)
  generate.supply.sale.many(way, min <- rand_num(2, 10),
                            max <- rand_num(50, 100),
                            type="export_all.out",days_num = 30, goods = goods)
  sale.level(way)
  sale.level(way,name="import.in",saleLevel = 50)
  sale.level(way,name="import_all.in",saleLevel = 60)
}
