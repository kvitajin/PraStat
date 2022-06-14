# ** Výpočet a graf pravděpodobnostní funkce ####
k=5
max = 20  # odhadnutý maximální počet pokusů (teoreticky nekonečno)
x = k:max # nastavení možných hodnot x (minimálně k, maximálně nekonečno)
P_x = dnbinom(x - k, k, p)
P_x
plot(x, P_x)
grid()
x = k:max
tmp=dnbinom(x-5, 5, 0.69)
tmp
plot(x, tmp)
grid()
mean(tmp)
median(tmp)
sd(tmp)
x=8
bd=1-pnbinom(3, 5, 0.69)
pnbinom(3,2, bd)

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


file_path <- "./data/data_ver220523A.xlsx"
data_raw <- read_excel(file_path)
data <- data_raw %>%
  mutate(zlepseni = Hod_konec-Hod_zacatek)


data_pokles_op <-
  data %>%
    group_by(Rocnik) %>%
    identify_outliers(zlepseni)

# odfiltrování odlehlých hodnot
data_bez_op <- data %>%
  filter(!(ID %in% data_pokles_op$ID))

boxplot.stats(data_bez_op$zlepseni)

tapply(data_bez_op$Hod_konec, data_bez_op$Rocnik, shapiro.test)

t.test(data_bez_op$Hod_konec[data_bez_op$Rocnik == "prima"],
       conf.level = 0.90,
       alternative = "greater")

t.test(data_bez_op$Hod_konec[data_bez_op$Rocnik == "tercie"],
       conf.level = 0.90,
       alternative = "greater")

var.test(
  data_bez_op$Hod_konec[data_bez_op$Rocnik == "prima"],
  data_bez_op$Hod_konec[data_bez_op$Rocnik == "tercie"],
  ratio=1,
  alternative="two.sided",
  conf.level=0.90
)

t.test(data_bez_op$Hod_konec[data_bez_op$Rocnik == "kvarta"],
       conf.level = 0.90,
       alternative = "greater")


kv=qt(0.9, )


#1A jablicka
#a) kolik cervenych? (c+)
#     75c+     a
#30%j<25c-   b c
#a<
#70%g<20c+   d e
#     80c-     f
#' Podmíněná pravděpodobnost
P_pod <- function(p_pod, p_jev) {
  (p_pod * p_jev) / p_jev
}
a1=0.75
b1=0.3
c1=0.25
d1=0.7
e1=0.2
f1=0.8

b1*P_pod(a1,b1)+d1*P_pod(e1,d1)

#b) P(J|C+) (pokud mám červení, tak je od jendy)
P_pod(b1, a1)

#2a urci C a hustotu pravd
#c(40x-x^2)   0<x<=20
#pro 20:
#1=c*(40*20-20^2)
40*20-20^2
x2=seq(0,20,0.1)
prub= 1/400*(40*x2-x2^2)
0.1*0.95+0.9*0.08
plot(x2, prub
)




#----------------------------------------------------


#1A jablicka
#a) kolik cervenych? (c+)
#     75c+     a
#30%j<25c-   b c
#a<
#70%g<20c+   d e
#     80c-     f
#' Podmíněná pravděpodobnost
P_pod <- function(p_pod, p_jev) {
  (p_pod * p_jev) / p_jev
}
a1=0.8
b1=1/90
c1=0.2
d1=89/90
e1=0.2
f1=0.8

b1*P_pod(a1,b1)+d1*P_pod(e1,d1)


P_pod(c1, b1)


combC<-function (n, k){
  factorial(n)/(factorial(k)*factorial(n-k))
}

combC(7,3)*combC(3,0)+
combC(7,2)*combC(3,1)+
combC(7,1)*combC(3,2)+
combC(7,0)*combC(3,3)


combC(7,3)*combC(3,0)/120

combC(7,2)*combC(3,1)/120
combC(7,1)*combC(3,2)/120
combC(7,0)*combC(3,3)/120

1-(0.29+0.53+0.18)

prav=c(0.29, 0.53, 0.17, 0.01)
druhy= c(3,2,1,0)
### střední hodnota
nahodny_vektor_E_X <- sum(prav * druhy)
# druhý moment
nahodny_vektor_E_X2 <- sum(nahodny_vektor_P_x * (nahodny_vektor_X ^ 2))
### rozptyl
nahodny_vektor_D_X <- sum(nahodny_vektor_E_X2 - nahodny_vektor_E_X ^ 2)

nahodny_vektor_E_X
sd(prav)

library(dplyr)
library(moments)
lambda = 5

x = seq(0,20,0.01)
f = dexp(x,lambda)
F = pexp(x,lambda)
data = data.frame(x,f,F) # příprava data.frame

# Generování grafů
a = ggplot(data = data, aes(x = x, y = f)) +
  geom_line() +
  theme_bw() +
  labs(x="délka remise (měsíce)",
       y = "hustota p-sti")

1-pexp(10, lambda)

qnorm(0.75,mu,sigma)


getwd()


library(readxl)
library(dplyr)
library(rstatix)

file_path <- "./data/data_ver220530B.xlsx"
data_raw <- read_excel(file_path)
data <- data_raw %>%
  mutate(zlepseni = m0-m1)


data_zacatek_op <-
  data %>%
    group_by(skupina) %>%
    identify_outliers(m0)

# odfiltrování odlehlých hodnot
data_bez_op <- data %>%
  filter(!(ID %in% data_zacatek_op$ID))

boxplot.stats(data_bez_op$m0)
boxplot.stats()


tapply(data_bez_op$m0, data_bez_op$skupina, shapiro.test)

t.test(data_bez_op$m0[data_bez_op$skupina == "KETO+HIIT"],
       conf.level = 0.95,
       alternative = "two.sided")
t.test(data_bez_op$m0[data_bez_op$skupina == "KONTROL"],
       conf.level = 0.95,
       alternative = "two.sided")

var.test(data_bez_op$m0[data_bez_op$skupina == "KETO+HIIT"], data_bez_op$m0[data_bez_op$skupina == "KONTROL"],ratio=1,alternative="two.sided",conf.level=0.95)

t.test(data_bez_op$m0[data_bez_op$skupina == "KETO+HIIT"], data_bez_op$m0[data_bez_op$skupina == "KONTROL"],alternative="two.sided",var.equal=T,conf.level=0.95)

tapply(data_bez_op$m0, data_bez_op$skupina, t.test, mu=0, alternative="two.sided", conf.level=0.95)


nahodny_vektor <- matrix(c(1/4,1/6,1/12,1/8,1/4,1/8), byrow = TRUE, nrow = 2)
nahodny_vektor_X <- c(0, 20, 40)
nahodny_vektor_Y <- c(3, 2)
dimnames(nahodny_vektor) <- list(nahodny_vektor_Y, nahodny_vektor_X)

nahodny_vektor


sum(nahodny_vektor[nahodny_vektor_X < 25, nahodny_vektor_Y < 3])

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


1/12



library(readxl)
library(dplyr)
library(rstatix)
library(lawstat)
library(FSA)
getwd()
# načtení dat
file_path <- "./data/data_ver220613B.xlsx"
data_raw <- read_excel(file_path)

# nalezení odlehlých pozorování
data_delka_op <- data_raw %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)

# odfiltrování odlehlých hodnot
data_bez_op <- data_raw %>%
  filter(!(ID %in% data_delka_op$ID))

data_odlehla<- data_raw %>%
filter((ID %in% data_delka_op$ID))


# test normálního rozdělení
tapply(data_bez_op$Sepal.Length, data_bez_op$Species, shapiro.test)

# test shody rozptylů
bartlett.test(data_bez_op$Sepal.Length ~ data_bez_op$Species)


kruskal.test(data_bez_op$Sepal.Length ~ data_bez_op$Species)

median(data_bez_op$Sepal.Length ~ data_bez_op$Species, na.rm=T)


tmp<-data_bez_op %>% group_by(Species) %>%summarise(median("Sepal.Length"))

median(data_bez_op$Sepal.Length[data_bez_op$Species=="setosa"])
median(data_bez_op$Sepal.Length[data_bez_op$Species=="versicolor"])
median(data_bez_op$Sepal.Length[data_bez_op$Species=="virginica"])


# seřazení podle mediánu
data_bez_op %>%
  group_by(Species) %>%
  summarize(median = median("Sepal.Length")) %>%
  arrange(desc(median))

dunnTest(Sepal.Length ~ Species, data = data_bez_op, method = "bonferroni")

file_path <- "./data/data_ver220613B.xlsx"
data_raw <- read_excel(file_path)

# nalezení odlehlých pozorování
data_delka_op <- data_raw %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)

# odfiltrování odlehlých hodnot
data_bez_op <- data_raw %>%
  filter(!(ID %in% data_delka_op$ID))



data_bez_op$dlouhe<-ifelse(data_bez_op$Sepal.Length > 5.8, 1, 0)
boxplot(data_bez_op$dlouhe~data_bez_op$Species)

boxplot(data_bez_op$dlouhe[data_bez_op$skupina=="virginica"],
       data_bez_op$dlouhe[data_bez_op$skupina=="setosa"] )
zacatek_shapiro <- tapply(data_bez_op$m0, data_bez_op$skupina, shapiro.test)
# byla splněna  normalita
bodovKh<-mean(data_bez_op$m0[data_bez_op$skupina == "virginica"], na.rm=T)
bodovKo<-mean(data_bez_op$m0[data_bez_op$skupina == "KONTROL"], na.rm=T)
khttest<-t.test(data_bez_op$m0[data_bez_op$skupina == "virginica"],
                conf.level = 0.95,
                alternative = "two.sided")
kottest<-t.test(data_bez_op$m0[data_bez_op$skupina == "KONTROL"],
                conf.level = 0.95,
                alternative = "two.sided")