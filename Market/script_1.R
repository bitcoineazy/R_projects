days <- 261
range_1 <- c(500, 760)
price_1.in <- 200
price_1.sale <- 270 # цена 1 шт
price_1.util <- 30 # затраты (цена утилизации)

period <- 1:days
period
input_1 <- sample(x = range_1[1]:range_1[2])
input_1
sale_1 <- as.integer(input_1 * runif(min = 0.4, max = 1, n = days))
sale_1

# не продано
utils_1 <- input_1 - sale_1
# прибыль
profit_1 <- sale_1 * price_1.sale - input_1 * price_1.in - utils_1 * price_1.util
profit_1

tab <- data.frame(
  "День" = period,
  "Батон, шт. Поставка" = input_1,
  "Батон, шт. Продажа" = sale_1,
  "Батон, шт. Списание" = utils_1,
  "Батон, шт. Прибыль" = profit_1
)

View(tab)

for (i in 1:10) {
  dir.create(glue({"{work_dir}/{i}/"}))
  file.create(glue("{work_dir}/{i}/Цены.txt"))
  file.create(glue("{work_dir}/{i}/Цены.in"))
  file.create(glue("{work_dir}/{i}/Цены.out"))
}
