#### setup ####
library(dplyr)

iDigits <- function(x) {
  sapply(trunc(log10(abs(x)) + 1), function (x) max(1, x))
}

dDigits <- function(x, k = NULL) {
  digits <- ifelse(is.null(k), nchar(abs(x)) - iDigits(x) - 1, k - iDigits(x))
  sapply(digits, function(x) max(0, x))
}

signifUp <- function(x, dig) {
  aux <- signif(x, digits = dig)
  i_dig <- iDigits(x)
  d_dig <- dDigits(x, dig)
  ifelse(aux >= x,
         aux,
         aux + 10^{ifelse(d_dig == 0, i_dig - dig, -d_dig)}
  )
}

sd_signif_digits <- function(x) {
  l <- length(x)
  case_when(
    l <= 10 ~ 1,
    l <= 30 ~ 2,
    l <= 2000 ~ 3,
    TRUE ~ 4
  )
}

sd_decim_digits <- function(x) {
  dDigits(sd(x), sd_signif_digits(x))
}

### usage ####
#library(dplyr)
#library(readxl)
#
#data <- read_excel("data_hraci.xlsx")
#ukazatele <-
#  data %>%
#    group_by(system) %>%
#    mutate(pokles = odehrane_hod_2018 - odehrane_hod_2019) %>%
#    summarise(
#      pocet_platnych_cifer_sd = sd_signif_digits(pokles),
#      pocet_desetinnych_mist_sd = sd_decim_digits(pokles),
#      smerodatna_odchylka = sd(pokles),
#      zaokrouhlena_smerodatna_odchylka = signifUp(smerodatna_odchylka, pocet_platnych_cifer_sd),
#      formatovana_smerodatna_odchylka = formatC(zaokrouhlena_smerodatna_odchylka, pocet_platnych_cifer_sd, format = "g")
#    )