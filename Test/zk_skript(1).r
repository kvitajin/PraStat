# Zkouškový script
install.packages("lawstat")
install.packages("FSA")
install.packages("rcompanion")
install.packages("EnvStats")
install.packages("lsr")
install.packages("epiR")

library(readxl)
library(moments)
library(dplyr)
library(ggplot2)
library(ggpubr)

library(EnvStats) #varTest

library(readxl)  # načtení xlsx souboru
library(dplyr)   # pro efektivní preprocessing (práci s datovým rámcem)
library(ggplot2) # pro hezčí grafiku
library(tibble)  # umožňuje nastavení počtu des. míst ve výstupech balíčku dplyr
library(moments) # funkce skewness(), kurtosis()
#library(dlookr)  # funkce normality() pro výpočet Shapirova-Wilkova testu pomocí dplyr
library(lawstat) # funkce symmetry.test() pro ověření symetrie
library(rstatix) # funkce identify_outliers()
library(FSA) # funkce dunnTest()
library(rcompanion) # písmenkové schéma
library(lsr)
library(epiR)

# Nastavení výstupů
options(scipen = 100, # číselné hodnoty zobrazovat jako desetinná čísla
        OutDec = ",", # oddělovač desetinných míst v tabulkách a grafech
        signif = 5, # počet platných cifer ve výstupech klasického Rka
        pillar.sigfig=5) # počet platných cifer ve výstupech balíčku dplyr


## Kombinatorika ####
# __V(n,k) - variace bez opakování 
# první argument bude celkový počet entit, druhý argument velikost výběru
variace = function(n,k) 
{
  citatel = factorial(n)  
  jmenovatel = factorial(n-k)
  return(citatel/jmenovatel)
}

# __V*(n,k) - variace s opakováním
variace_opak = function(n,k) {
  return(n^k)
}

# __P(n) - permutace (přesmyčky)
permutace = function(n){
  return(factorial(n))
}

# __P*(n1,n2,n3,....,nk) - permutace s opakováním 
# vstupní proměnné tvoří vektor s jednotlivými počty unikátních entit
permutace_opak = function(vec_n) # vec_n je vektor počtů hodnot př.: vec_n = c(2,2,2,4,3)
{
  n = sum(vec_n) # spočteme kolik máme hodnot celkem
  citatel = factorial(n) 
  jmenovatel = prod(factorial(vec_n)) # výstupem funkce prod() je součin položek vektoru, který je argumentem této funkce
  return(citatel/jmenovatel)
}

# __C(n,k) - kombinace
kombinace = function(n,k)
{
  return(choose(n,k)) 
}

# __C*(n,k) - kombinace s opakováním
kombinace_opak = function(n,k)
{
  return(choose(n+k-1,k)) 
}


a = ( (kombinace(4,1)*kombinace(28,2)) + (kombinace(4,2)*kombinace(28,1)) + (kombinace(4,3)) ) / kombinace(32,3)
a


#-------------------------------------------------------------------------------------------------#
## Diskrétní náhodná veličina ####
# __Zajímavý příklad ####
# V porodnici v jistém malém městě se dnes narodilo 6 donošených novorozenců. Popište rozdělení náhodné 
# veličiny X popisující počet novorozenců s hyperbilirubinemii mezi těmito novorozenci. Určete rovněž 
# střední hodnotu, směrodatnou odchylku a modus této náhodné veličiny.
# pravdepodobnost nemoci = 0.49
# pravdepodobnost bez nemoci = 0.51
x = 0:6 # Hodnoty náhodné veličiny
p = choose(6,x)*0.49^x*0.51^(6-x) # Určení pravděpodobností
p # Ukázka výpočtu
p = round(p,3)
p
sum(p)  # kontrola, zda nedošlo k zaokrouhlovací chybě
plot(x,p,pch = 3)
# __Výpočet charakteristik ####
EX = sum(x*p) # Střední hodnota
EX2 = sum(x*x*p)
DX = EX2-EX*EX
sigmaX = sqrt(DX) # Směrodatná odchylka
modus.X = x[p == max(p)] # Modus
#P(x >= 4)
sum(p[5:7]) # sečtení 5. - 7. hodnoty 

#-------------------------------------------------------------------------------------------------#
## Rozdělení diskrétní veličiny ####

# Předpony
# d- pro hustotu pravděpodobnosti nebo pravděpodobnostní funkce
# p- pro distribuční funkce
# q- pro kvantil


# __Binomické rozdělení ####
#X~Bi(n = 21, π = 0.5)
n = 21  # size - celkový počet
p = 0.5 # prob - pravděpodobnost jednoho
# ____Výpočet P(X = x) ####
x = 10   # hodnota, pro níž hledáme p-stní funkci 
dbinom(x, n, p)
# ____Výpočet a graf pravděpodobnostní funkce ####
x = 0:n # nastavení možných hodnot x (minimálně 0, maximálně n)
P_x = dbinom(x, n, p)
P_x
plot(x, P_x)
grid()
# ____Výpočet P(X <= x) ####
x = 10   # hodnota, pro níž hledáme kumulativní p-stní funkci 
pbinom(x, n, p)
# ____Výpočet F(x) = P(X < x) ####
x = 10   # hodnota, pro níž hledáme kumulativní p-stní funkci
pbinom(x - 1, n, p)
# ____Výpočet a graf distribuční funkce ####
x = 0:n # nastavení možných hodnot x (minimálně 0, maximálně n)
P_x = dbinom(x, n, p) # pravděpodobnostní funkce
F_x = cumsum(P_x)
plot(x, F_x, type='s')
grid()
# ____Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
q = 0.7  
qbinom(q, n, p)
# Ověření správnosti řešení
pbinom(11, n, p)
pbinom(12, n, p)
# ____Kvantilová funkce ####
# tj. nalezení nejmenšího x, pro které platí F(x)>=q (pro dané q)
q = 0.7   # pravděpodobnost pro kterou hledáme kvantil
qbinom(q, n, p) + 1
# Ověření správnosti řešení
pbinom(12, n, p) - dbinom(12, n, p)
pbinom(13, n, p) - dbinom(13, n, p)


# __Hypergeometrické rozdělení ####
# X~H(N = 20,M = 5, n = 10)
# Funkce v R bere jako parametry *hyper(x, M, N - M, n)
# parametry rozdělení
N = 20  # celkový počet objektů
M = 5   # z toho se zadanou vlastností
n = 10  # rozsah výběru
# ____Výpočet P(X = x) ####
x = 5   # hodnota, pro níž hledáme p-stní funkci 
dhyper(x, M, N - M, n)
# ____Výpočet a graf pravděpodobnostní funkce ####
x = 0:min(M,n) # nastavení možných hodnot x (minimálně 0, maximálně min(M, n))
P_x = dhyper(x, M, N - M, n)
P_x
plot(x, P_x)
grid()
# ____Výpočet P(X <= x) ####
x = 5   # hodnota, pro níž hledáme kumulativní p-stní funkci 
phyper(x, M, N - M, n)
#  ____Výpočet F(x) = P(X < x) ####
x = 5   # hodnota, pro níž hledáme distribuční funkci 
phyper(x-1, M, N - M, n)
# ____Výpočet a graf distribuční funkce ####
x = 0:min(M,n) # nastavení možných hodnot x (minimálně 0, maximálně min(M, n))
P_x = dhyper(x, M, N - M, n) # pravděpodobnostní funkce
F_x = cumsum(P_x)
plot(x, F_x, type='s')
grid()
# ____Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
q = 0.7  
qhyper(q, M, N - M, n)
# ____Kvantilová funkce ####
# tj. nalezení nejmenšího x, pro které platí F(x)>=q (pro dané q)
q = 0.7   # pravděpodobnost pro kterou hledáme kvantil
qhyper(q, M, N - M, n) + 1


# __Negativně binomické rozdělení ####
#X~NB(k = 5, π = 0.3)
# parametry rozdělení
k = 5     # požadovaný počet úspěchů
p = 0.3   # p-st úspěchu v jednotlivých pokusech
# ____Výpočet P(X = x) ####
x = 15   # hodnota, pro níž hledáme p-stní funkci 
# pozor první argument musí být počet neúspěchů, nikoliv celkový počet pokusů,
# tj. x - k, nikoliv x
dnbinom(x - k, k, p)
# ____Výpočet a graf pravděpodobnostní funkce ####
max = 40  # odhadnutý maximální počet pokusů (teoreticky nekonečno)
x = k:max # nastavení možných hodnot x (minimálně k, maximálně nekonečno)
P_x = dnbinom(x - k, k, p)
P_x
plot(x, P_x)
grid()
# ____Výpočet P(X <= x) ####
x = 15   # hodnota, pro níž hledáme kumulativní p-stní funkci 
pnbinom(x - k, k, p)
# ____Výpočet F(x) = P(X < x) ####
x = 15   # hodnota, pro níž hledáme distribuční funkci 
pnbinom(x - k - 1, k, p)
# ____Výpočet a graf distribuční funkce ####
max = 40  # odhadnutý maximální počet pokusů (teoreticky nekonečno)
x = k:max # nastavení možných hodnot x (minimálně k, maximálně nekonečno)
P_x = dnbinom(x - k, k, p) # pravděpodobnostní funkce
F_x = cumsum(P_x)
plot(x, F_x, type='s')
grid()
# ____Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
q = 0.7  
qnbinom(q, k, p) + k
# ____Kvantilová funkce ####
# tj. nalezení nejmenšího x, pro které platí F(x)>=q (pro dané q)
q = 0.7   # pravděpodobnost pro kterou hledáme kvantil
qnbinom(q, k, p) + k + 1


# __Poissonovo rozdělení ####
# X~Po(λt = 10)
# - počet událostí v Poissonově procesu v uzavřené oblasti (v čase, na ploše, v objemu)
# - proces musí splňovat podmínky ordinarity, stacionarity a beznáslednosti
# - parametr λt je roven střední hodnotě modelované NV 
# parametry rozdělení
lt = 10     # λt je průměrný počet událostí v Poissonově procesu v uzavřené oblasti (v čase, na ploše, v objemu)
# spočítám jako průměrný zadaný počet/průměrný zadaný čas nebo počet objektů*objem vzorku
# ____Výpočet P(X = x) ####
x = 15   # hodnota, pro níž hledáme p-stní funkci 
dpois(x, lt)
# ____Výpočet a graf pravděpodobnostní funkce ####
max = 30  # odhadnutý maximální počet události (teoreticky nekonečno)
x = 0:max # nastavení možných hodnot x (minimálně 0, maximálně nekonečno)
P_x = dpois(x, lt)
P_x
plot(x, P_x)
grid()
# ____Výpočet P(X <= x) ####
x = 15   # hodnota, pro níž hledáme kumulativní p-stní funkci 
ppois(x, lt)
# ____Výpočet F(x) = P(X < x) ####
x = 15   # hodnota, pro níž hledáme distribuční funkci 
ppois(x - 1, lt)
# ____Výpočet a graf distribuční funkce ####
max = 30  # odhadnutý maximální počet pokusů (teoreticky nekonečno)
x = 0:max # nastavení možných hodnot x (minimálně 0, maximálně nekonečno)
P_x = dpois(x, lt) # pravděpodobnostní funkce
F_x = cumsum(P_x)
plot(x, F_x, type='s')
grid()
# ____Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
q = 0.7  
qpois(q, lt)
# ____Kvantilová funkce ####
# tj. nalezení nejmenšího x, pro které platí F(x)>=q (pro dané q)
q = 0.7   # pravděpodobnost pro kterou hledáme kvantil
qpois(q, lt) + 1

#-------------------------------------------------------------------------------------------------#
## Rozdělení spojité veličiny ####
# Předpony
# d- pro hustotu pravděpodobnosti
# p- pro distribuční funkce
# q- pro kvantil

## __Rovnoměrné rozdělení ####
# - náhodná veličina nabývá pouze hodnot v rozsahu <a;b>
# - všechny hodnoty mají stejnou hustotu výskytu -> hustota pravděpodobnosti je
#   konstantní mezi a a b, jinde nulová
# parametry rozdělení
a = 2   # nejmenší možná hodnota 
b = 4   # největší možná hodnota
#____Výpočet f(x) ####
x = 3
dunif(x, a, b) # f(3)
# ____Graf hustoty pravděpodobnosti f(x) ####
x = seq(from = 0, to = 6, by = 0.01) # pamatujete-li si pořadí parametrů, není nutno jejich názvy uvádět
f_x = dunif(x, a, b)
plot(x, f_x, cex = 0.1) # cex je relativní velikost markerů (0.1 odpovídá 10 % původní velikosti)
grid()
# ____Výpočet a graf distribuční funkce F(x) = P(X < x) ####
x = 3   
punif(x, a, b) # U SNV platí: P(X<=x) = P(X<x) = F(x)
# Graf distribuční funkce
x = seq(from = 0, to = 6, by = 0.01)
F_x = punif(x, a, b)
plot(x, F_x, type = 'l')
grid()
# ____Výpočet a graf kvantilové funkce F^(-1)(p) = x ####
# tj. nalezení x splňujícího rovnici P(X<x)=p pro dané p
p = 0.75   
qunif(p, a, b)
# Graf kvantilové funkce F^(-1)(p) = x
p = seq(from=0, to=1, by=0.01)   
x = qunif(p, a, b)
plot(p, x, type = 'l')
grid()
# ____Intenzita poruch ####
# POZOR!!! Intenzitu poruch používáme pouze v případě, že jde o nezápornou NV!!!
x = 3
dunif(x,a,b)/(1-punif(x,a,b))
# Graf intenzity poruch
x = seq(from = a, to = b, by = 0.01)
lambda_x = dunif(x,a,b)/(1-punif(x,a,b))
plot(x,lambda_x,type = "l")


## __Exponenciální rozdělení ####
# X~Exp(λ = 2)
# - doba do 1. události, doba mezi událostmi (pouze v období stabilního života -
#   Poissonův proces)
# - parametr λ je tentýž co v Poissonově rozdělení
# - střední hodnota E(X) = λ (průměrná doba do 1. události / průměrná doba mezi událostmi)
# parametry rozdělení
lambda = 2 # Určíme jako 1/E(X)
# ____Výpočet f(x) ####
x = 1
dexp(x, lambda)
# ____Graf hustoty pravděpodobnosti f(x) ####
x = seq(from = 0, to = 3, by = 0.01)
f_x = dexp(x, lambda)
plot(x, f_x, type='l')
grid()
#  ____Výpočet a graf distribuční funkce F(x) = P(X < x) ####
x = 1
pexp(x, lambda) # U SNV platí: P(X<=x) = P(X<x) = F(x)
# Graf distribuční funkce
x = seq(from = 0, to = 3, by = 0.01)
F_x = pexp(x, lambda)
plot(x, F_x, type = 'l')
grid()
# ____Výpočet a graf kvantilové funkce F^(-1)(p) = x ####
# tj. nalezení x splňujícího rovnici P(X<x)=p pro dané p
p = 0.5   
qexp(p, a, b)
# Graf kvantilové funkce F^(-1)(p) = x
p = seq(from=0, to=1, by=0.001)   
x = qexp(p, lambda) # Kvantil
plot(p, x, type = 'l')
grid()
# ____Výpočet a graf intenzity poruch ####
# Exponenciální NV je nezáporná, intenzita poruch se používá k jejímu popisu.
x = 2
dexp(x,lambda)/(1-pexp(x,lambda))
# Graf intenzity poruch
x = seq(from = 0, to = 3, by = 0.1)
lambda_x = dexp(x,lambda)/(1-pexp(x,lambda))
plot(x,lambda_x,type = "l",
     ylim = c(0,3)) # nenastavíme-li rozsah osy y, vidíme vlivem zaokrouhlovací chyby
                    # oscilující funkci


## __Weibullovo rozdělení ####
# X~Wb(Θ=3, β=2)
# - doba do 1. události (poruchy)(vhodná volba β umožuje použití v libovolném období
#   intenzity poruch)
# - rozšíření exponenciálního rozdělení: Exp(λ) = Wb(Θ=1/λ, β=1)
# parametry rozdělení
theta = 3 # ekvivalent 1/lambda u exp. rozdělení
beta = 2  # (beta = 1 -> exponenciální rozdělení)
# ____Výpočet f(x) ####
x = 5
dweibull(x,shape=beta, scale=theta)
# ____Graf hustoty pravděpodobnosti f(x) ####
x = seq(from = 0, to = 7, by = 0.01)
f_x = dweibull(x,shape=beta, scale=theta)
plot(x, f_x, type='l')
grid()
# ____Výpočet a graf distribuční funkce F(x) = P(X < x) ####
x = 5
pweibull(x,shape=beta, scale=theta) # U SNV platí: P(X<=x) = P(X<x) = F(x)
# Graf distribuční funkce
x = seq(from = 0, to = 7, by = 0.01)
F_x = pweibull(x,shape=beta, scale=theta)
plot(x, F_x, type = 'l')
grid()
# ____Výpočet a graf kvantilové funkce F^(-1)(p) = x ####
# tj. nalezení x splňujícího rovnici P(X<x)=p pro dané p
p = 0.5   
qweibull(p,shape=beta, scale=theta)
# Graf kvantilové funkce F^(-1)(p) = x
p = seq(from=0, to=1, by=0.01)   
x = qweibull(p,shape=beta, scale=theta)
plot(p, x, type = 'l')
grid()
# ____Výpočet a graf intenzity poruch ####
# Weibullova NV je nezáporná, intenzita poruch se používá k jejímu popisu.
x = 2
dweibull(x,shape=beta, scale=theta)/(1-pweibull(x,shape=beta, scale=theta))
# Graf intenzity poruch
x = seq(from = 0, to = 7, by = 0.1)
lambda_x = dweibull(x,shape=beta, scale=theta)/(1-pweibull(x,shape=beta, scale=theta))
plot(x,lambda_x,type = "l")


## __Normální rozdělení ####
# X~N(µ = 2, σ = 3)
# - rozdělení modelující např. chyby měření, chování součtu/průměru mnoha jiných NV
# - µ je střední hodnota rozdělení: E(X)= µ
# - σ je směrodatná odchyla rozdělení: D(X)=σ^2
# - NV s normálním rozdělením s parametry µ=0,σ=1 se nazývá normovaná normální NV
# parametry rozdělení
mu = 2   # E(X) 
sigma = 3   # σ(X)
# ____Výpočet f(x) ####
x = 3
dnorm(x, mu, sigma) # f(3)
# ____Graf hustoty pravděpodobnosti f(x) ####
a = 2   # nejmenší možná hodnota 
b = 4   # největší možná hodnota
x = seq(from = mu-3.5*sigma, to = mu+3.5*sigma, by = 0.01) # pamatujete-li si pořadí parametrů, není nutno jejich názvy uvádět
f_x = dnorm(x, a, b)
plot(x, f_x, cex = 0.1) # cex je relativní velikost markerů (0.1 odpovídá 10 % původní velikosti)
grid()
# ____Výpočet a graf distribuční funkce F(x) = P(X < x) ####
x = 3   
pnorm(x, mu, sigma) # U SNV platí: P(X<=x) = P(X<x) = F(x)
# Graf distribuční funkce
x = seq(from = mu-3.5*sigma, to = mu+3.5*sigma, by = 0.01)
F_x = pnorm(x, mu, sigma)
plot(x, F_x, type = 'l')
grid()
# ____Výpočet a graf kvantilové funkce F^(-1)(p) = x ####
# tj. nalezení x splňujícího rovnici P(X<x)=p pro dané p
p = 0.75   
qnorm(p, mu, sigma)
# Graf kvantilové funkce F^(-1)(p) = x
p = seq(from=0, to=1, by=0.01)   
x = qnorm(p, mu, sigma)
plot(p, x, type = 'l')
grid()
# ____Intenzita poruch ####
# POZOR!!! Intenzitu poruch používáme pouze v případě, že jde o nezápornou NV!!!
# V tomto případě nemá její použití smysl


#-------------------------------------------------------------------------------------------------#
## Načtení souboru ####
getwd()
setwd("E:/Škola/Statistika/skripty/Ukol 1S")

data = read_excel("ukol_28.xlsx", 
                  sheet = "Vysledky mereni",   # specifikace listu v xlsx souboru
                  skip = 0)                    # řádky, které je potřeba přeskočit

# Změny typu objektu (pokud je to nutné)
class(data)   # Výpis typu třídy
data = as.data.frame(data)  # Změna na datový rámec
class(data)

# Vybereme data týkající se pouze hráčů s OS Windows a OS Linux
data = data %>% 
  filter(system == "WIN"|system == "Linux") %>% 
  droplevels()

# Výběr slupců
# Tohle se dělat nemusí, záleží na tom, jaké je zadání!!
sloupce = data[ , c(2, 3, 4, 5,6,7,8,9)] # Uložení sloupců do proměnné

# Data rozdelim do sloupcu podle nějakého kritéria (tady konkrétně podle toku při teplotě K)
hodnoty_22 = sloupce[ , c(1, 3, 5, 7)]  # Data pro teplotu 22
hodnoty_5 = sloupce[, c(2, 4, 6, 8)]     # Data pro teplotu 5

# Přejmenování sloupců
# To dělám kvůli funkce stack(), která pak sama přiřadí hodnotu do sloupce pro identifikaci výrobce
colnames(hodnoty_22) = c("A","B","C","D")
colnames(hodnoty_5) = c("A","B","C","D")

# Vytvoření jednoho sloupce pro každou hodnotu a přidání sloupce s identifikací výrobce
sloupec_22 = stack(hodnoty_22)
sloupec_5 = stack(hodnoty_5)
colnames(sloupec_22) = c("tok22", "vyrobce")
colnames(sloupec_5) = c("tok5", "vyrobce")

# Sloučení do jednoho
dataset = cbind(sloupec_22, sloupec_5)
dataset = dataset[,-2]  # Jeden sloupec pro výrobce je teď navíc, můžu ho proto odstranit
dataset = na.omit(dataset)  # Odstranění řádku s hodnotou NA (s tímhle opatrně)

dataset$pokles = dataset$tok22 - dataset$tok5

#-------------------------------------------------------------------------------------------------#
## Odstranění odlehlých pozorování ####

# Krabicový graf
boxplot(dataset$pokles[dataset$vyrobce == "A"], dataset$pokles[dataset$vyrobce == "B"],dataset$pokles[dataset$vyrobce == "C"],dataset$pokles[dataset$vyrobce == "D"],
        names = c("Amber", "Bright","Clear","Dim"),
        #main = "Pokles světelného toku (lm)", 
        ylab = "Světelný tok zářivek při teplotě 5°C (lm)",
        col = "gray")
# Nebo
boxplot(data$narust~data$system)

# Odstranění odlehlých pozorování
dataset$ID <- seq.int(nrow(dataset)) # Přidání sloupce ID

outliers = dataset %>% 
  group_by(vyrobce) %>% 
  identify_outliers(pokles)

dataset$pokles = ifelse(dataset$ID %in% outliers$ID,NA,dataset$pokles)

dataset = na.omit(dataset)  # Odstranění řádku s hodnotou NA (s tímhle opatrně)

# Ještě jednou vykreslím boxplot
boxplot(dataset$pokles[dataset$vyrobce == "A"], dataset$pokles[dataset$vyrobce == "B"],dataset$pokles[dataset$vyrobce == "C"],dataset$pokles[dataset$vyrobce == "D"],
        names = c("Amber", "Bright","Clear","Dim"),
        #main = "Pokles světelného toku (lm)", 
        ylab = "Světelný tok zářivek při teplotě 5°C (lm)",
        col = "gray")



#-------------------------------------------------------------------------------------------------#
## Testování normality a symetrie####
# Normalita
tapply(dataset$pokles,dataset$vyrobce, moments::skewness,na.rm = T)    # Šikmost
tapply(dataset$pokles,dataset$vyrobce, moments::kurtosis,na.rm = T)-3  # Špičatost
tapply(dataset$pokles,dataset$vyrobce, shapiro.test)                   # Shapirův-Wilkův test

# Symetrie
symmetry.test(dataset$pokles[dataset$vyrobce == "A"], boot = FALSE) # Testování symetrie pro pokles hodnot u výrobce "A"


#-------------------------------------------------------------------------------------------------#
## Stranové alternativy ####
# H0: u != 0   -> two.sided (oboustranný) (dolní mez; horní mez)
# H0: u > 0    -> greater (levostranný) (dolní mez; nekonečno)
# H0: u < 0    -> less (pravostranný) (nekonečno; horní mez)

#-------------------------------------------------------------------------------------------------#
## Jednovýběrové testy ####

# __Test rozptylu ####
#*Bodový odhad
sd(dataL$odehrane_hod_2018,na.rm = T) # zaokrouhlujeme dle výše uvedeného na desetiny
# Test
varTest(dataset$pokles[dataset$vyrobce == "A"], sigma.squared=25^2, alternative="two.sided", conf.level=0.95) # H0: rozptyl = 10
# Přepočet int. odhadu sm. odchylky (odmocnění mezí int. odhadu rozptylu)
sqrt(varTest(dataset$pokles[dataset$vyrobce == "A"], 
             sigma.squared = 25^2, 
             conf.level = 0.95,
             alternative = "less")$conf.int)
# __Studentův t-test ####
t.test(dataset$pokles[dataset$vyrobce == "A"], mu=5, alternative="two.sided", conf.level=0.95) # H0: mu = 5

# __Test parametru o bin. rozdělení ####
# (počet s danou vlastností, počet celkem, pravděpodobnost kterou srovnávám)
binom.test(16, 100, 0.18, alternative="two.sided",conf.level=0.95) # p = 16/100, H0: pi = 0.18

# __Jednovýběrový Wilcoxonův test ####
# (stejné jako t-test akorát conf.int=T)
wilcox.test(dataset$pokles, mu=10, alternative="two.sided", conf.level=0.95, conf.int=T) # H0: x0.5 = 10

# __Znaménkový test ####
SIGN.test(mydata$hodnoty, md=10, alternative="two.sided", conf.level=0.95) # H0: x0.5 = 10, potřeba doinstalovat knihovnu BSDA

# __Celkový postup pro bodový odhad + intervalový odhad + p-hodnotu ####
# (místo t.test se může dosadit libovolný test)
dataset %>% 
  group_by(vyrobce) %>% 
  summarise(počet_hodnot = length(na.omit(pokles)), # abychom věděli, jak zaokrouhlit směr. odchylku
            smer.odchylka = sd(pokles,na.rm = T),   # abychom věděli, jak zaokrouhlit odhady průměrů/mediánů
            průměr = mean(pokles,na.rm = T), # bodové odhady průměrů
            #median = median(tok,prob = 0.5,na.rm = T), # bodové odhady mediánů  
            dolni.mez = t.test(pokles,
                               alternative = "two.sided",      # levostranný odhad
                               conf.level = 0.95)$conf.int[1], # dolní mez IO průměrů
            horni.mez = t.test(pokles,
                               alternative = "two.sided", 
                               conf.level = 0.95)$conf.int[2], # horní mez IO průměrů
            p.value = t.test(pokles,
                             mu = 0,  # tady změnit požadovanou střední hodnotu
                             alternative = "two.sided")$p.value) # levostranný test, p-hodnota

#-------------------------------------------------------------------------------------------------#
## Dvouvýběrové testy ####

# __Test shody rozptylů ####
var.test(dataset$pokles[dataset$vyrobce == "A"],dataset$pokles[dataset$vyrobce == "B"], # nejdříve uvádím proměnnou s větším výběrovým rozptylem
         alternative = "two.sided",
         conf.level = 0.95)

# __Dvouvýběrový Studentův t-test ####
t.test(dataA,dataB,alternative="two.sided",var.equal=T,conf.level=0.95) # H0: mu1 - mu2 = 0

# __Aspinové-Welchův test ####
t.test(dataA,dataB,alternative="two.sided",var.equal=F,conf.level=0.95) # H0: mu1 - mu2 = 0 (ale rozptyly se nerovnají)

# __Mannův-Whitneyho test ####
wilcox.test(dataA,dataB, alternative="two.sided", conf.level=0.95, conf.int=T) # H0: x0,5 - x0,5 = 0 (shoda mediánů)

# __Test binomického rozdělení (pravděpodobnosti) ####
#x = počty s požadovanou vlastností
#y = celkové počty
prop.test(x = c(127,33),n = c(202,58),
          alternative = "two.sided",
          conf.level = 0.9)

# __Bodový odhad rozdílů středních hodnot ####
mean(dataset$pokles[dataset$vyrobce == "A"],na.rm = T) - mean(dataset$pokles[dataset$vyrobce == "B"],na.rm = T)

# __Bodový odhad rozdílů mediánů ####
median(dataset$pokles[dataset$vyrobce == "A"],prob = 0.5,na.rm = T) - median(dataset$pokles[dataset$vyrobce == "B"],prob = 0.5,na.rm = T)


#-------------------------------------------------------------------------------------------------#
## Vícevýběrové testy ####

# __Ověření homoskedasticity (shody rozptylů) ####
boxplot(dataset$pokles~dataset$vyrobce) # výsledek odhaduju dle "výšky" krabic
rozptyly = dataset %>% 
  group_by(vyrobce) %>% 
  summarise(var = var(pokles,na.rm = T))

rozptyly # výběrové rozptyly

max(rozptyly$var)/min(rozptyly$var)  # poměr je větší než 2, tj.očekáváme, že rozptyly se významně liší
# Pokud je normální rozdělení, použiju pro p-hodnotu Bartlettův test
# pokud není normální rozdělení, použiju pro p-hodnotu Leveneho test

# __Barlettův test (shoda rozptylů - nezamítnutná normalita) ####
bartlett.test(dataset$pokles∼dataset$vyrobce)

# __Leveneho test (shoda rozptylů - zamítnutá normalita) ####
leveneTest(dataset$pokles∼dataset$vyrobce) # Potřeba knihovna car

# __ANOVA (shoda středních hodnot) ####
vysledky = aov(dataset$pokles∼dataset$vyrobce)  # H0: mu1 = mu2 = mu3
summary(vysledky) # Poslední sloupec by měl být p-hodnota

# ____Tukeyho test (post hoc analýza pro ANOVA) ####
TukeyHSD(vysledky)
plot(TukeyHSD(vysledky)) # Vizualizace

# __Kruskalův-Wallisův test ####
kruskal.test(dataset$pokles∼dataset$vyrobce)

# ____Dunnové test (post hoc analýza pro Kruskalův-Wallisův test) ####
DT = dunnTest(pokles ~ vyrobce,       # knihovna FSA          
              data=dataset,
              method="bonferroni")
DT
PT = DT$res # úprava výstupu Dunnové testu do podoby vhodné jako vstup pro tvorbu písmenkového schématu
cldList(P.adj ~ Comparison, data = PT, threshold = 0.05) # písmenkové schéma

# Testy dobré shody ####
# __Tvorba tabulky ####
x = c(1,2,3,4,5,6) # Vrchní řádek, hodnoty měřené veličiny
n.obs = c(979,1002,1015,980,1040,984) # Observed hodnoty (Oi)
p.exp = c(1/6,1/6,1/6,1/6,1/6,1/6) # Relativní četnosti
n.exp = sum(n.obs) * p.exp # Očekávané četnosti (Ei)
# __Testová statistika
x.obs = sum(((n.obs-n.exp)^2)/n.exp)
x.obs
# __p-hodnota
pocet_odhadovanych_parametru = 0
df = length(x)  -pocet_odhadovanych_parametru - 1 # Stupně volnosti
p.hodnota = 1 - pchisq(x.obs, df)
p.hodnota
#__Příklad Poison ####
# Mám ze zadání
x = c(0,1,2,3,4)
n.obs = c(52,48,36,10,4)
# Potřebuju spočítat relativní četnosti
# Pokud neznám lt musím spočítat odhad
lt = weighted.mean(x, n.obs) # Odhad lt
lt = 1.2 # lt ze zadání
p.exp = dpois(x, lt)
sum(p.exp) # Kontrola jestli je suma = 1
p.exp[length(x)] = 1 - sum(p.exp[1:length(x)-1]) # Pokud suma není 1, doplním do posledního sloupce 1 - součet ostatních
sum(p.exp) # Znovu kontrola
# __Příklad Normal + spojita ####
dalnice = readxl::read_excel("data/neparametricke_hypotezy.xlsx", sheet=2) # Načtení souboru
colnames(dalnice)="hodnoty"
# Rovnou můžu použít shapiro-Wilk test
shapiro.test(dalnice$hodnoty) # p-hodnota

# Analýza závislost ####
#__Čtení souboru ####
tet = readxl::read_excel("data/neparametricke_hypotezy.xlsx", sheet=3) # načtení souboru z excelu
tet = tet[,c(6,10)] # vyříznu konkrétní sloupce které potřebuju
colnames(tet) = c("pohlavi","tetovani") # přejmenování sloupců
head(tet) # Můžu se kouknout na první data
#__Vytvoření tabulky ze souboru ####
kont.tab = table(tet$pohlavi, tet$tetovani)
kont.tab # Vypíše mi tabulku včetně četností
#__Vytvoření tabulky ručně ####
kont.tab = matrix(c(10,20,50,15,20,10,130,40), # Data po řádcích
                  nrow = 2,  # Počet řádků
                  byrow = T)
#Přejmenování řádků a sloupců
rownames(kont.tab) = c("Praha", "Venkov")
colnames(kont.tab) = c("velmi nespokojen", "spíše nespokojen",
                       "spíše sopkojen", "velmi spokojen")
kont.tab = as.table(kont.tab)
kont.tab # Vypíše mi tabulku včetně četností
#__Změna sloupců pro kontingenční tabulku ####
data$pa_norm2 = factor(data$pa_norm,
                       levels = c("nízká","střední","vysoká"),
                       labels = c("nízká nebo střední","nízká nebo střední","vysoká"))

prop.table(kont.tab)   # Sdružené relativní četnosti
prop.table(kont.tab,1) # Řádkové relativní četnosti
prop.table(kont.tab,2) # Sloupcové relativní četnosti
#__Mozaikový graf ####
mosaicplot(kont.tab, las=1, color=gray.colors(2))
#__Cramerovo V ####
cramersV(kont.tab)
#__Test dobré shody v kontingenční tabulce
pom = chisq.test(kont.tab)
# Pro kontrolu předpokladů si zobrazím
pom$expected # Očekávané četnosti
# p-hodnota
pom # Zobrazení výsledku již dříve provedeného testu
#__Zobrazení tabulky pomocí epiR ####
epi.2by2(kont.tab)