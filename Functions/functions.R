# Объявление функции
printArgument <- function (argument) {
  print(argument)
}

pow <- function (x, y) {
  result <- x^y
  print(paste(x, "в степени", y, "равно", result))
}

sum3Numb <- function (x1 = 10, x2 = 20, x3 = 30) {
  return(x1 + x2 + x3)
}

# Задание №1
pow2 <- function (x, y, z) {
  if (typeof(x) != "double" | typeof(y) != "double" | typeof(z) != "double") {
    return(print("Параметры должны быть числом!"))
  }
  if (z == 0) {
    return(print("Нельзя делить на 0!"))
  }
  result <- (x ^ y) / z
  print(paste(x, "в степени", y, "делить на", z, "=", result))
}

# Задание №2
dayofWeek <- function (N, isDebug = FALSE) {
  if (N < 1) {
    return(print(" "))
  } else if (typeof(N) != "double") {
    return(print("День недели должен быть числом"))
  }
  N <- round(N,0)
  day <- switch(N, "Понедельник", "Вторник", "Среда",
         "Четверг", "Пятница", "Суббота", "Воскресенье")
  if (isDebug) {
    return(print(paste("Параметры: N =", N, "isDebug =", isDebug, "день недели", day)))
  } else {
    return(print(paste(day, "- это", N, "день недели")))
  }
}

# Задание №3
dayofWeekTranslate <- function (N, language="rus", isDebug=FALSE) {
  if (N < 1) {
    return(print(" "))
  } else if (typeof(N) != "double") {
    return(print("День недели должен быть числом"))
  }
  N <- round(N,0)
  if (language == "rus" | language == "ru" | language == "RU" | language == "рус" | language == "ру") {
    day <- switch(N, "Понедельник", "Вторник", "Среда",
         "Четверг", "Пятница", "Суббота", "Воскресенье")
  } else if (language == "eng" | language == "Eng" |
    language == "English" | language == "english" | language == "англ" | language == "Англ") {
    day <- switch(N, "Monday", "Tuesday", "Wendesday", "Thursday", "Friday", "Saturday", "Sunday")
  }
  if (isDebug) {
    return(print(paste("Параметры: N =", N, "isDebug =", isDebug,
                       "язык =", language, "день недели", day)))
  } else {
    return(print(paste(day, "- это", N, "день недели")))
  }
}

# Задание №4
dayofWeekTranslateWithShort <- function (N, language="rus", short=FALSE, isDebug=FALSE) {
  if (N < 1) {
    return(print(" "))
  } else if (typeof(N) != "double") {
    return(print("День недели должен быть числом"))
  }
  N <- round(N,0)
  if (short == FALSE) {
    if (language == "rus" | language == "ru" | language == "RU" | language == "рус" | language == "ру") {
    day <- switch(N, "Понедельник", "Вторник", "Среда",
         "Четверг", "Пятница", "Суббота", "Воскресенье")
    } else if (language == "eng" | language == "Eng" |
    language == "English" | language == "english" | language == "англ" | language == "Англ") {
    day <- switch(N, "Monday", "Tuesday", "Wendesday", "Thursday", "Friday", "Saturday", "Sunday")
    }
  } else if (short == TRUE) {
    if (language == "rus" | language == "ru" | language == "RU" | language == "рус" | language == "ру") {
    day <- switch(N, "ПН", "ВТ", "СР",
         "ЧТ", "ПТ", "СБ", "ВС")
    } else if (language == "eng" | language == "Eng" |
    language == "English" | language == "english" | language == "англ" | language == "Англ") {
    day <- switch(N, "MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
    }
  }

  if (isDebug) {
    return(print(paste("Параметры: N =", N, "isDebug =", isDebug,
                       "язык =", language, "сокращение =", short, "день недели", day)))
  } else {
    return(print(paste(day, "- это", N, "день недели")))
  }
}

# Задание №5
dayofWeekVector <- function (N, language="rus", short=FALSE, isDebug=FALSE) {
  if (short == FALSE) {
    if (language == "rus" | language == "ru" | language == "RU" | language == "рус" | language == "ру") {
      week_vector <- c("Понедельник", "Вторник", "Среда",
               "Четверг", "Пятница", "Суббота", "Воскресенье")
      print(week_vector[N])
    } else if (language == "eng" | language == "Eng" |
      language == "English" | language == "english" | language == "англ" | language == "Англ") {
      week_vector <- c("Monday", "Tuesday", "Wendesday",
             "Thursday", "Friday", "Saturday", "Sunday")
      print(week_vector[N])
    }
  } else if (short == TRUE) {
    if (language == "rus" | language == "ru" | language == "RU" | language == "рус" | language == "ру") {
      week_vector <- c("ПН", "ВТ", "СР",
           "ЧТ", "ПТ", "СБ", "ВС")
      print(week_vector[N])
    } else if (language == "eng" | language == "Eng" |
      language == "English" | language == "english" | language == "англ" | language == "Англ") {
      week_vector <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")
      print(week_vector[N])
    }
  }
  if (isDebug) {
    return(print(paste("Параметры: N =", N, "isDebug =", isDebug,
                       "язык =", language, "сокращение =", short, "день недели", week_vector)))
  } else {
    return(print(paste(week_vector, "- это", N, "день недели")))
  }
}
