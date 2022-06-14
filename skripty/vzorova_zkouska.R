### Příklad 4 ###
library(readxl)
library(dplyr)
library(ggplot2)
library(rstatix)

file_path <- "./data/data_zkouska_vzor.xlsx"
measurements_by_sheet <- lapply(excel_sheets(file_path), function(sheet) read_excel(file_path, sheet))
names(measurements_by_sheet) <- excel_sheets(file_path)
measurements <- bind_rows(measurements_by_sheet, .id = "technologie")
colnames(measurements) <- c("technologie", "id", "rychlost_pred", "rychlost_po")

# výpočet relativní změny rychlosti (v procentech)
measurements <- measurements %>%
  mutate(
    rychlost_zmena = 100 * (rychlost_po - rychlost_pred) / rychlost_pred
  )

# nalezení odlehlých hodnot
outliers <-
  measurements %>%
    group_by(technologie) %>%
    identify_outliers(rychlost_zmena)

# odfiltrování odlehlých hodnot
measurements_bez_op <- measurements %>%
  filter(!(id %in% outliers$id))

# test normality
tapply(measurements_bez_op$rychlost_zmena, measurements_bez_op$technologie, shapiro.test)

# 95% levostranný odhad střední hodnoty
tapply(measurements_bez_op$rychlost_zmena, measurements_bez_op$technologie, t.test, mu=0, alternative="greater", conf.level=0.95)

# shoda rozptylů
var.test(
  measurements_bez_op$rychlost_zmena[measurements_bez_op$technologie == "ADSL"],
  measurements_bez_op$rychlost_zmena[measurements_bez_op$technologie == "SATELIT"],
  ratio=1,
  alternative="two.sided",
  conf.level=0.95
)

# apin-welch test o významu rozdílu
t.test(
  measurements_bez_op$rychlost_zmena[measurements_bez_op$technologie == "ADSL"],
  measurements_bez_op$rychlost_zmena[measurements_bez_op$technologie == "SATELIT"],
  var.equal = F,
  alternative="two.sided",
  conf.level=0.95
)

# ověření normality rozdílu
tapply(measurements_bez_op$rychlost_pred, measurements_bez_op$technologie, shapiro.test)

# statistická významnost rozdílu
tapply(measurements_bez_op$rychlost_pred, measurements_bez_op$technologie, t.test, mu=0, alternative="two.sided", conf.level=0.95)