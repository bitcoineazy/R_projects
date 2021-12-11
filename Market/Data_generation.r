# Цикл создания директорий и файлов
try(
  for (i in 1:10) {
  dir.create(glue({"{work_dir}/{i}/"}))
  file.create(glue("{work_dir}/{i}/Цены.txt"))
  file.create(glue("{work_dir}/{i}/Цены.in"))
  file.create(glue("{work_dir}/{i}/Цены.out"))
})


#---- В качестве начального этапа мы генерируем данные при помощи заранее прописанной функции ----

# также мы в функции генерации данных будем сравнивать значения из файла поставок и продаж

gener_mag_data <- function (way = '', is.sup.sale = TRUE,
                            file.name = 'Поставка', goods = h,
                            days = 7, sale.level = 100, lim = NaN) {
  
  col <- list()
  sale.ratio <-  sale.level / 100
  
  tabl <-  data.frame('День' = 1:days)
  
  h <- list(
    list(name = 'Торт, шт.', min = 30, max = 70, ratio.min = (0.43 * sale.ratio)),
    list(name = 'Молоко, уп.', min =  600, max = 800, ratio.min = (0.75 * sale.ratio)),
    list(name = 'Кефир, уп.', min = 200, max = 300, ratio.min = (0.67 * sale.ratio)),
    list(name = 'Соль, пачка', min = 17, max = 25, ratio.min = (0.68 * sale.ratio)),
    list(name = 'Гречка, пачка', min = 50, max = 60, ratio.min = (0.83 * sale.ratio)),
    list(name = 'Хлеб, шт.', min = 30, max = 50, ratio.min = (0.6 * sale.ratio)),
    list(name = 'Вода, бут.', min = 400, max = 430, ratio.min = (0.83 * sale.ratio))
  )
  
  # вычисляем необходимые данные и генерируем случайные значения поставок и продаж
  
  for (i in 1:length(goods)) {
    
    # здесь мы сравниваем как раз-таки, для какой цели мы и генерируем данные (продажа или поставка)
    # также проводим сравнение, исходя из изначального флага в объявлении функции
    
    if (is.sup.sale == TRUE) {
      col[i] <- list(as.integer(runif(n = days, min = goods[[i]]$min, max = goods[[i]]$max)))
    }
    else {
      col[i] <- list(as.integer(lim[[i]] * runif(n = days, min = goods[[i]]$ratio.min, max = sale.ratio)))
    }
    
    tabl[i + 1] = col[i]
    colnames(x = tabl)[i + 1] = goods[[i]]$name
  }
  
  print(tabl)
  
  name_file <- ifelse(is.sup.sale == TRUE,
                      paste0(way, file.name, '.in'),
                      paste0(way, file.name, '.out'))
  
  # записываем получившиеся данные в файлы и возвращаем их переменным в конце тела функции
  
  write.table(x = tabl,
              file = name_file,
              col.names = TRUE,
              row.names = FALSE)
  
  return(col)
}


#---- Далее приступаем к самой генерации данных в in-out файлы магазинов сети ----


path_work <- '/home/noble6/DEV/R_projects/Market/'

dir_work <- list()
net_name <- 'Перекрёсток'

for (k in 1:10) {
  dir_work[k] <- paste0(path_work, k, '/')
}

for (i in 1:10) {
  val <- gener_mag_data(way = dir_work[i], file.name = net_name)
  gener_mag_data(way = dir_work[i], is.sup.sale = FALSE, file.name = net_name, lim = val)
}



# ---- Установим изначально файл со всеми ценами на товары (у поставщика и для продажи в магазинах) ----

sales_price <- c(1250, 60, 75, 15, 78, 30, 25)

provider_price <- sales_price * 0.60

path_file <- '/home/noble6/DEV/R_projects/Market/'
name_file <- 'Цены.txt'

tabl.prices = data.frame("Цена_продажи" = sales_price, "Цена_у_поставщика" = provider_price)

write.table(x = tabl.prices, 
          file = paste0(path_file, name_file),
          dec = ".",
          col.names = TRUE,
          row.names = FALSE)


# ---- Теперь перейдём к созданию файла при помощи бат-скрипта с ценами продуктов и заполним его -----

#---- Уже потом через командную строку копируем и переименовываем файлы в папку "Анализ" при помощи Bat-файла ----





