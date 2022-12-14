# **********************************#
#     Расчет зоны поражения АХОВ
#
#
#

# Вид прогноза predictType
# 0 - авария(realTimePredictType
# 1 - прогнозирование (predictTimePredictType)
predictType <- TRUE

# Количество выброса ХОВ (CDS) в тоннах Q0 Плотность ХОВ densityCDS в т/м3 d с
# учетом грегатного состояние ХОВ gasSubstance дает номер значения плотности в
# векторе densityCDS
# 1 - сжатый газ (gasCompressed)
# 2 - сжиженный газ (gasLiquid)
# 3 - ядовитый газ (gasToxic):
# piplineCDS - максимальное количество АХОВ в трубопроводе между отсекателями т
# nameCDS -  наименование ХОВ
# equalCDS_1- эквивалентное количество ХОВ для первичного облака
# equalCDS_2 - эквивалентное количество ХОВ для вторичного облака

name_cds <- 0
equalCDS_1 <- 0
equalCDS_2 <- 0
boilingTempCDS <- 0
toxicCDS <- 0
numCDS <- 0

gasSubstance <- 1
gasCompressed <- 0
gasLiquid <- 0
gasToxic <- 0
density_CDS <- c(gasCompressed, gasLiquid, gasToxic)

piplineCDS <- 250

# Объем емкости tanksCDSCapacity (вектор) м3 и количество емкостей tanksCDS
tanksCDS <- 1
tanksCDSCapacity <- c(1:tanksCDS)

# Толщина слоя разлива h - spillThickness по умолчанию 0,05 м
# Реальная площадь F разлива в паддон/обвалок spillPalletArea м2
# Условия разлива spillCase (1 - тип разлива, 2 - высота H, 3 - площадь F)
# Тип разлива spillType:
# 0 - трубопровод (spillPipeline)
# 1 - свободный разлив (spillFree)
# 2 - в поддон одной емкости (spillPallet)
# 3 - в поддон  N емкостей spillPalletN
# Высота разлива H   в м -  spillHeight
spillThickness <- 0.05
spillPalletArea <- 0
spillHeight <- 0
spillType <- 1
spillCase <- c(spillType, spillHeight, spillPalletArea)

# df_CDS - таблица параметров АХОВ
#
K1 <- 1
K2 <- 0
K3 <- 0
K4 <- 0
K5 <- 0
K6 <- 0
K7 <- numeric(10)

names(K7) <- c("-40", "-20", "0", "20", "40", "-40", "-20", "0", "20", "40")


df_CDS <- data.frame(
  "Номер" = numCDS,
  "АХОВ" = nameCDS,
  "Плотность АХОВ, т/м3 (сжатый газ)" = densityCDS[1],
  "Плотность АХОВ, т/м3 (сжиженный газ)" = densityCDS[2],
  "Плотность АХОВ, т/м3 (ядовитый газ)" = densityCDS[3],
  "Температура кипения, С" = boilingTempCDS,
  "Пороговая токсидоза, мг/мин" = toxicCDS,
  "K1" = K1,
  "K2" = K2,
  "K3" = K3,
  "K7, для температуры воздуха" = K7
)



df_CDS[1, 1] <- (3)

# *****Толщина слоя жидкости h, м ********************************************
LayerThicknesscallc <- function(CDS, densityCDS, spillCase) {
  if (spillCase[spillType] < 1) {
    return(piplineCDS)
  } else {
    if (spillCase[spillType] < 2) {
      return(0.05)
    } else {
      if (spillCase[spillType] < 3) {
        return(spillCase[spillHeight] - 0.2)
      }
      return(CDS / (spillCase[spillPalletArea] * densityCDS))
    }
  }
}

# Определение количественных характеристик выброса АХОВ

# Вычисляем эквивалентное количество Qэ1 (т) (primaryCloudCSD) АХОВ, перешедшее
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
# Вычисляем эквивалентное количество вещества во вторичном облаке по формуле:
#
# Qэ2 = (1-K1) * K2 * K3 * K4 * K5 * K6 * K7 * (Q0/(h*d))

K1 <- 1
K2 <- 0
K3 <- 0
K4 <- 0
K5 <- 0
K6 <- 0
K7 <- 1



if (gasSubstance == 1) {
  K1 <- 1
  K7 <- 1
}






# вычисляем высоту слоя разлива h, м

spillThickness <- layerThicknesscallc(CDS, densityCDS, spillCase)

