if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
  }

# **********************************#
#     Расчет зоны поражения АХОВ
# тип прогноза: 1 - прогноз, 2 - авария, 3 - взрыв
predict_type <- 1

# наименование ХОВ
hov_name <- ""

# агрегатное состояние вещества: 1 - газ, 2 - жидкость
hov_state <- 1

# толщина слоя разлива - h
hov_h <- 0.05

# высота поддона - H
hov_hh <- 0.5

# площадь разлива - F
hov_ff <- 5

# тип розлива: 1 - свободный, 2 - индивидуальный поддон, 3 - общий поддон
spill_type <- c(1, hov_h, hov_hh, hov_ff)

# количество емкостей с ХОВ или отрезков трубопровода
tanks <- 2

# количество ХОВ в каждой емкости или отрезке трубопровода
tanks_q <- numeric(tanks)

# количество выброса ХОВ
hov_v <- 0

# координаты источника заражения: x,y
object_x <- 0
object_y <- 0

# время от начала аварии
emergency_time <- 1

# расстояние от объекта до жилой зоны
object_distance <- 0

# время суток: 1 - ночь, 2 - утро, 3 - день, 4 - вечер
day_period <- 1

# Облачность: 1 - ясно, 2 - переменная, 3 - сплошная
weather_cloudly <- 1

# направление ветра: 1 - С, 2 - В, 3 - Ю, 4 - З, 5 - СВ, 6 - СЗ, 7 - ЮЗ, 8 - ЮВ
wind_direction <- 1

# скорость ветра
wind_speed <- 0

# температура
temperature <- 20

# дата фрейм для хранения текущего сценария
df_scenario <- data.frame(predict_type, hov_name, hov_state, spill_type[1],
                          hov_v, object_x, object_y, emergency_time,
                          object_distance, day_period, weather_cloudly,
                          wind_direction, wind_speed, temperature)




# dt_hov35 - таблица параметров ХОВ
dt_hov35 <- read_excel(
  "data/HOV_Param.xlsx", sheet = "HOV_Param", col_names = TRUE,
  col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric"), na = "NA")

# df_hov_predict - фрэйм текущих параметров параметров выбранного АХОВ
df_hov_predict <- list(dt_hov35)
  
  
#  
#  "Номер" = dt_paramraw, "АХОВ" = hov_name,
#  "Плотность АХОВ, т/м3 (сжатый газ)" = density[1],
#  "Плотность АХОВ, т/м3 (сжиженный газ)" = density[2],
#  "Плотность АХОВ, т/м3 (ядовитый газ)" = density[3],
#  "Температура кипения, С" = boiling_temp,
#  "Пороговая токсидоза, мг/мин" = toxic,
#  "K1" = k1, "K2" = k2, "K3" = k3, "K7, для температуры воздуха" = k7)

#df_hov_predict[1, 1] <- (3)

# *****Толщина слоя жидкости h, м ********************************************
layerThicknesscallc <- function(hov_q, density, spill_case) {
  if (spill_case[spill_type] < 1) {
    return(pipline_q)
  } else {
    if (spill_case[spill_type] < 2) {
      return(0.05)
    } else {
      if (spill_case[spill_type] < 3) {
        return(spill_case[spill_h] - 0.2)
      }
      return(hov_q / (spill_case[spill_f] * density))
    }
  }
}

# Определение количественных характеристик выброса АХОВ

# Вычисляем эквивалентное количество Qэ1 (т) (equal_1) АХОВ, перешедшее
# в первичное облако по формуле:
#
#  Qэ1 = К1 ×К3 ×К5 × К7 × QО
# K1 - коэффициент, зависящий от условий хранения АХОВ (для сжатых газов = 1)
# K2 - коэффициент зависящий от физико-химических свойств АХОВ
# K3 - коэффициент, равный отношению пороговой токсичности дозы хлора к
# пороговой токсодозе другого АХОВ
# K4 - коэффициент, учитывающий скорость ветра
# K5 - коэффициент, учитывающий степень вертикальной устойчивости атмосферы
#     при инверсии = 1
#     при изотермии = 0,23
#     при конвенции = 0,08
# K6 - коэффициент, зависящий от времени N, прошедшего после начала аварии
# (на которое делается прогноз)
# K7 - коэффициент, учитывающий температуру воздуха (для сжатых газов = 1)
#
# Вычисляем эквивалентное количество вещества equal_2 во вторичном облаке по 
# формуле:
# Qэ2 = (1-K1) * K2 * K3 * K4 * K5 * K6 * K7 * (Q0/(h*d))

k1 <- 1
k2 <- 0
k3 <- 0
k4 <- 0
k5 <- 0
k6 <- 0
k7 <- 1

if (substate == 1) {
  k1 <- 1
  k7 <- 1
}

# вычисляем высоту слоя разлива h, м

spill_h <- layerThicknesscallc(hov_q, density, spill_case)
