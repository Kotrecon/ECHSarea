if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
  }

# **********************************#
#     Расчет зоны поражения АХОВ
#
#
#
# Вид прогноза predict_type
# 0 - авария(online_predict)
# 1 - прогнозирование (offline_predict)
predict_type <- TRUE
# Количество выброса ХОВ (HOV) в тоннах Q0 - hov_q0
# Плотность ХОВ - density в т/м3
# с учетом агрегатного состояния ХОВ substate, дает номер значения плотности
# через вектор density
# 1 - сжатый газ (compressed)
# 2 - сжиженный газ (liquid)
# 3 - ядовитый газ (toxic):
# pipeline_q - максимальное количество ХОВ в трубопроводе между отсекателями т
# hov_name -  наименование ХОВ
# equal_1- эквивалентное количество ХОВ для первичного облака
# equal_2 - эквивалентное количество ХОВ для вторичного облака
# toxic - токсичность ХОВ
hov_q0 <- 0
hov_q <- 0
hov_name <- 0
equal_1 <- 0
equal_2 <- 0
boiling_temp <- 0
toxic <- 0
dt_paramraw <- 0
substate <- 1
compressed <- 0
liquid <- 0
toxic <- 0
density <- c(compressed, liquid, toxic)
pipline_q <- 250

# Объем емкости tanks_capacity (вектор) м3 и количество емкостей tanks
tanks <- 1
tanks_capacity <- c(1:tanks)

# Толщина слоя разлива h по умолчанию 0,05 м
# Условия разлива spill_case (1 - тип разлива, 2 - высота H, 3 - площадь F)
# Тип разлива spill_type:
# 0 - трубопровод (spill_pipeline)
# 1 - свободный разлив (spill_free)
# 2 - в поддон одной емкости (spill_pallet)
# 3 - в поддон  N емкостей (spill_npallet)
# Высота разлива H в м -  spill_h
# Реальная площадь F разлива в паддон/обвалок spill_f м2

h <- 0.05
spill_f <- 0
spill_h <- 0
spill_type <- 1
spill_pipline <- 0
spill_free <- 0
spill_pallet <- 0
spill_npallet <- 0
spill_case <- c(spill_type, spill_h, spill_f)

k1 <- 1
k2 <- 0
k3 <- 0
k4 <- 0
k5 <- 0
k6 <- 0
k7 <- numeric(10)

names(k7) <- c("-40", "-20", "0", "20", "40", "-40", "-20", "0", "20", "40")

# dt_hov35 - таблица параметров ХОВ
dt_hov35 <- read_excel(
  "data/HOV_Param.xlsx", sheet = "HOV_Param", col_names = TRUE,
  col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric"), na = "NA")

# df_hov_predict - фрэйм текущих параметров параметров АХОВ
df_hov_predict <- data.frame(
  "Номер" = dt_paramraw, "АХОВ" = hov_name,
  "Плотность АХОВ, т/м3 (сжатый газ)" = density[1],
  "Плотность АХОВ, т/м3 (сжиженный газ)" = density[2],
  "Плотность АХОВ, т/м3 (ядовитый газ)" = density[3],
  "Температура кипения, С" = boiling_temp,
  "Пороговая токсидоза, мг/мин" = toxic,
  "K1" = k1, "K2" = k2, "K3" = k3, "K7, для температуры воздуха" = k7)

df_hov_predict[1, 1] <- (3)

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
