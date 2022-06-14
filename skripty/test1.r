# 1. Kombinatorika

## 1.1. Uspořádané výběry

#' Variace bez opakování
variace <- function(n, k) {
  factorial(n) / factorial(n - k)
}

#' Permutace bez opakování
permutace <- function(n) {
  factorial(n)
}

#' Variace s opakováním
variace_s_opak <- function(n, k) {
  n^k
}

#' Permutace s opakováním
permutace_s_opak <- function(k) {
  factorial(sum(k)) / prod(factorial(k))
}

#' Kombinace bez opakování
kombinace <- function(n, k) {
  factorial(n) / (factorial(k) * factorial(n - k))
}

#' Kombinace s opakováním
kombinace_s_opak <- function(n, k) {
  factorial(n + k - 1) / (factorial(n - 1) * factorial(k))
}

# 1. Pravděpodobnosti

#' Podmíněná pravděpodobnost
P_pod <- function(p_pod, p_jev) {
  (p_pod * p_jev) / p_jev
}

## 1.1. rakovina

#' Pravděpodobnost skutečného výskytu jevu
#'
#' @param p_root_positive Pravděpodobnost celkového výskytu jevu v populaci
#' @param p_true_positive Pravděpodobnost pozitivního testu u pozitivního pacienta
#' @param p_false_negative Pravdběpodobnost negativního testu u negativního pacienta
#'
#' @return Pravděpodobnost pozitivity u pacienta, pokud je v rizikové skupině
P_tree <- function(p_root_positive, p_true_positive, p_true_negative) {
  (p_root_positive * p_true_positive) / (p_root_positive * p_true_positive + (1 - p_root_positive) * (1 - p_true_negative))
}

# 2. Náhodná veličina
# 2. Dvourozměrný náhodný vektor

## tvorba vektoru
nahodny_vektor <- matrix(c(0.01, 0.02, 0.03, 0.25, 0.04, 0.16, 0, 0.05, 0.12, 0.07, 0.06, 0.01), byrow = TRUE, nrow = 3)
nahodny_vektor_X <- c(3, 5, 7)
nahodny_vektor_Y <- c(1, 2, 3, 4)
dimnames(nahodny_vektor) <- list(nahodny_vektor_X, nahodny_vektor_Y)

## doplnění prázdné hodnoty
nahodny_vektor_p_5_3 <- 1 - sum(nahodny_vektor)
nahodny_vektor["5", "3"] <- nahodny_vektor_p_5_3

## hodnota distribuční funkce F(A, B) -> P(X < A, X < B)
sum(nahodny_vektor[nahodny_vektor_X < 7.1, nahodny_vektor_Y < 2.8])

## marginální rozdělení
nahodny_vektor_P_x <- rowSums(nahodny_vektor)
nahodny_vektor_P_y <- colSums(nahodny_vektor)

## podmíněná pravděpodobnost
### P(Y > 2.1 | X < 5.3)
sum(nahodny_vektor[nahodny_vektor_X < 5.3, nahodny_vektor_Y > 2.1]) / sum(nahodny_vektor[nahodny_vektor_X < 5.3,])

## základní charakteristiky
### střední hodnota
nahodny_vektor_E_X <- sum(nahodny_vektor_P_x * nahodny_vektor_X)
# druhý moment
nahodny_vektor_E_X2 <- sum(nahodny_vektor_P_x * (nahodny_vektor_X ^ 2))
### rozptyl
nahodny_vektor_D_X <- sum(nahodny_vektor_E_X2 - nahodny_vektor_E_X ^ 2)

# 3. Rozdělení
# 3. Výběrové charakteristiky

## odfiltrovani odlehlych pozorovani
library(outliers)
c(1, 2, 3) %>%
  group_by(groupName) %>%
  filter(!valueName %in% outlier(valueName))

charakteristiky <- function(x, valueCol, groupCol) {
  library(dplyr)
  library(moments)
  source("./sd_round.R")
  x %>%
    group_by(!!sym(groupCol)) %>%
    summarize(
      prumer = mean(!!sym(valueCol)),
      median = median(!!sym(valueCol)),
      min = min(!!sym(valueCol)),
      max = max(!!sym(valueCol)),
      rozptyl = var(!!sym(valueCol)),
      sikmost = skewness(!!sym(valueCol)),
      spicatost = kurtosis(!!sym(valueCol)) - 3,
      kvantil_25 = quantile(!!sym(valueCol), .25),
      kvantil_50 = quantile(!!sym(valueCol), .5),
      kvantil_75 = quantile(!!sym(valueCol), .75),
      pocet_platnych_cifer_sd = sd_signif_digits(!!sym(valueCol)),
      pocet_desetinnych_mist_sd = sd_decim_digits(!!sym(valueCol)),
      smerodatna_odchylka = sd(!!sym(valueCol)),
      zaokrouhlena_smerodatna_odchylka = signifUp(smerodatna_odchylka, pocet_platnych_cifer_sd),
      formatovana_smerodatna_odchylka = formatC(zaokrouhlena_smerodatna_odchylka, pocet_platnych_cifer_sd, format = "g")
    )
}

# 4. Intervalové odhady
# 4. Testování hypotéz

# 5. ANOVA
# 5. Test dobré shody
# 5. Analýza závislosti

####################################
p <- c(0.12, 0.04, 0.3, 0.48, 0.06)
n <- 150+90*p
sd(n)

plot(10:70, dpois(10:70, 40))

(1-ppois(42, 40))/(1-ppois(32, 40))

ppois(42, 40, lower.tail = FALSE)*40

####################################
library(readxl)
library(dplyr)
library(rstatix)

file_path <- "./data/data_ver220516A.xlsx"
data_raw <- read_excel(file_path)

data <- data_raw %>%
  mutate(pokles = m0 - m1)

data_pokles_op <-
  data %>%
    group_by(skupina) %>%
    identify_outliers(pokles)

# odfiltrování odlehlých hodnot
data_bez_op <- data %>%
  filter(!(ID %in% data_pokles_op$ID))

tapply(data_bez_op$pokles, data_bez_op$skupina, shapiro.test)

tapply(data_bez_op$pokles, data_bez_op$skupina, t.test, mu=0, alternative="greater", conf.level=0.95)

tapply(data_bez_op$pokles, data_bez_op$skupina, t.test, mu=0, alternative="two.sided", conf.level=0.95)

var.test(data_bez_op$pokles[data_bez_op$skupina == "KETO"], data_bez_op$pokles[data_bez_op$skupina == "KETO+HIIT"],ratio=1,alternative="two.sided",conf.level=0.95)

t.test(data_bez_op$pokles[data_bez_op$skupina == "KETO"], data_bez_op$pokles[data_bez_op$skupina == "KETO+HIIT"],alternative="two.sided",var.equal=T,conf.level=0.95)

kruskal.test(data_bez_op$pokles, data_bez_op$skupina)

charakteristiky(data_bez_op, "pokles", "skupina")


library(FSA)
dunnTest(pokles ~ skupina, data = data_bez_op, method = "bonferroni")

pnorm(42, mean = 40)