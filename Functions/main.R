setwd("/home/noble6/DEV/R_projects/Functions")
source("functions.R")

DEBUG_ON <- TRUE
DEBUG_OFF <- FALSE

p <- printArgument(10)
z <- pow(3, 3)
print(summ_1 <- sum3Numb())

sessionInfo()

# Задание №1
pow_2 <- pow2(55, 5, 20)
pow_2_2 <- pow2(55, 10, 0)
pow_2_3 <- pow2(10, 44, "char")

# Задание №2
day_1 <- dayofWeek(1)
day_2 <- dayofWeek(5, DEBUG_ON)
day_3 <- dayofWeek(0)
day_4 <- dayofWeek("char")

# Задание №3
day_t_1 <- dayofWeekTranslate(6)
day_t_2 <- dayofWeekTranslate(5, "eng")
day_t_3 <- dayofWeekTranslate(3, "Англ", DEBUG_ON)

# Задание №4
day_s_1 <- dayofWeekTranslateWithShort(2, "англ")
day_s_2 <- dayofWeekTranslateWithShort(3, "рус", short=TRUE)
day_s_3 <- dayofWeekTranslateWithShort(4, "english", short=TRUE)

# Задание №5
day_z_1 <- dayofWeekVector(c(1, 2, 3, 4, 1, 5, 6, 7), "eng")
day_z_2 <- dayofWeekVector(c(5, 4, 3, 2, 1, 5), "рус", short=TRUE, DEBUG_ON)
day_z_3 <- dayofWeekVector(c(5, 1, 7, 1), "Англ")

