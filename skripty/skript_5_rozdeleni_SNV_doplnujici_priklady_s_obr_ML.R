# ......................................................................................
# ................Cvičení 6 - Vybraná rozdělení spojité náhodné veličiny................
# ................................... Doplňující příklady ..............................
# ........................... Martina Litschmannová, Adéla Vrtková .....................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

library(ggplot2)
library(ggpubr)
library(svglite)

# Příklad 1. ####
# Vláček v zoo vyjíždí ze stanice každých 30 minut. 
# *a)	####
# Určete rozdělení doby čekání na vláček (hustotu pravděpodobnosti i distribuční funkci), 
# grafy obou funkcí načrtněte. 

# X … doba čekání na vláček (min.)
# X ~ Ro(a=0, b=30)

# parametry rozdělení
a = 0
b = 30

x = seq(-10,40,0.01)
f = dunif(x,a,b)
F = punif(x,a,b)
data = data.frame(x,f,F) # příprava data.frame

# Generování grafů
a = ggplot(data = data, aes(x = x, y = f)) +
  geom_line() +
  theme_bw() +
  labs(x="doba čekání na vláček (min.)",
       y = "hustota p-sti")
b = ggplot(data = data, aes(x = x, y = F)) +
  geom_line() +
  theme_bw() +
  labs(x="doba čekání na vláček (min.)",
       y = "distribuční funkce")

# Zkombinujeme grafy dle svých představ
ggarrange(a,b,
          nrow = 2)

# Uložíme graf, který je aktuálně vytvořen v graf. okně do pracovního adresáře
ggsave("SNV_priklad1.svg",
       width = 15, height = 10,
       unit = "cm")

# *b)	####
# P(X>5)=1-P(X<=5)
1-punif(5,a,b)

# Příklad 2. ####
# Náhodná veličina popisující délku těhotenství ženy (ve dnech) 
# má normální rozdělení se střední hodnotou 266 dní a směrodatnou odchylkou 16 dní.

# *a)	####
# Načrtněte graf hustoty pravděpodobnosti uvažované náhodné veličiny.
# X ... délka těhotenství (dny)
# X~N(µ = 266, σ = 16)

# parametry rozdělení
mu = 266
sigma = 16

x = seq(mu-3.5*sigma,mu+3.5*sigma,0.01)
f = dnorm(x,mu,sigma)
data = data.frame(x,f) # příprava data.frame

# Generování grafu
ggplot(data = data, aes(x = x, y = f)) +
  geom_line() +
  theme_bw() +
  labs(x="délka těhotenství (dny)",
       y = "hustota p-sti")

# Uložíme graf, který je aktuálně vytvořen v graf. okně do pracovního adresáře
ggsave("SNV_priklad2.svg",
       width = 15, height = 5,
       unit = "cm")

# *b)	####
# U kolika procent žen je délka těhotenství nejvýše 246 dní?

# P(X<=246)
pnorm(246,mu,sigma)

# *c)	####
# U kolika procent žen je délka těhotenství v rozmezí 250-282 dní? 

# P(250<=X<=282)  - SNV, tj. ostrá/neostrá nerovnost nehraje roli
pnorm(282,mu,sigma)-pnorm(250,mu,sigma)

# *d)	####
# Jaká délka těhotenství je překročena maximálně u 10 % žen?

# P(X>T) = 0,1 -> P(X<T) = 0,9 - SNV, tj. ostrá/neostrá nerovnost nehraje roli
# Tj. hledáme 90% kvantil
qnorm(0.9,mu,sigma)

# *e)	####
# Určete dolní kvartil a horní kvartil dané náhodné veličiny. Slovně je interpretujte.

qnorm(0.25,mu,sigma)
# Čtvrtina žen vykazuje délku těhotenství kratší než 255 dní.

qnorm(0.75,mu,sigma)
# Tři čtvrtiny žen vykazují délku těhotenství kratší než 277 dní.

# Příklad 3. ####
# Uvažujte náhodnou veličinu popisující délku remise (měsíce) onkologických pacientů, 
# která má exponenciální rozdělení se střední hodnotou 1 rok.

# *a)	####
# Načrtněte grafy hustoty p-sti a distribuční funkce uvažované náhodné veličiny.
# X ... délka remise (měsíce)
# X~Exp(lambda = 1/12) - E(X) = 12 = 1/lambda

# parametry rozdělení
lambda = 1/12

x = seq(0,45,0.01)
f = dexp(x,lambda)
F = pexp(x,lambda)
data = data.frame(x,f,F) # příprava data.frame

# Generování grafů
a = ggplot(data = data, aes(x = x, y = f)) +
  geom_line() +
  theme_bw() +
  labs(x="délka remise (měsíce)",
       y = "hustota p-sti")
b = ggplot(data = data, aes(x = x, y = F)) +
  geom_line() +
  theme_bw() +
  labs(x="délka remise (měsíce)",
       y = "distribuční funkce")

# Zkombinujeme grafy dle svých představ
ggarrange(a,b,
          nrow = 2)

# Uložíme graf, který je aktuálně vytvořen v graf. okně do pracovního adresáře
ggsave("SNV_priklad3.svg",
       width = 15, height = 10,
       unit = "cm")

# *b)	####
# Určete medián dané náhodné veličiny a hodnotu interpretujte.

qexp(0.5,lambda)
# Polovina pacientů má dobu remise kratší než 8,3 měsíce.

# *d)	####
# Jakou dobu remise překročí pouze 10 % daných onkologických pacientů?

# P(X>T)=0,1 -> P(X<T)=0,9, tj.hledáme 90% kvantil
qexp(0.9,lambda)

# *e)	####
# Jaká je pravděpodobnost, že remise bude trvat alespoň dva roky? 

# P(X>24) = 1 - P(X<24) - POZOR! Modelujeme délku remise v měsících!
1-pexp(24,lambda)

# Příklad 4. ####
# Rychlost větru (m/s) má Weibullovo rozdělení s parametrem tvaru 2 
# a parametrem měřítka 7.

# *a)	####
# Načrtněte graf hustoty pravděpodobnosti a distribuční funkce.
# X ... rychlost větru (m/s)
# X~Wb(shape = 2, scale = 7)

# parametry rozdělení
shape = 2
scale = 7

x = seq(0,20,0.01)
f = dweibull(x,shape = shape, scale = scale)
F = pweibull(x,shape = shape, scale = scale)
data = data.frame(x,f,F) # příprava data.frame

# Generování grafů
a = ggplot(data = data, aes(x = x, y = f)) +
  geom_line() +
  theme_bw() +
  labs(x="rychlost větru (m/s)",
       y = "hustota p-sti")
b = ggplot(data = data, aes(x = x, y = F)) +
  geom_line() +
  theme_bw() +
  labs(x="rychlost větru (m/s)",
       y = "distribuční funkce")

# Zkombinujeme grafy dle svých představ
ggarrange(a,b,
          nrow = 2)

# Uložíme graf, který je aktuálně vytvořen v graf. okně do pracovního adresáře
ggsave("SNV_priklad4.svg",
       width = 15, height = 10,
       unit = "cm")

# *b)	####
# Jaká je pravděpodobnost, že rychlost větru překročí 9 m/s? 

# P(X>9) = 1-P(X<9)
1 - pweibull(9,shape = shape, scale = scale)

# *c)	####
# Jaká je pravděpodobnost, že rychlost větru dosáhne 5. stupně 
# Beaufortovy stupnice (tj. 8,0 m/s až 10,7 m/s)?

# P(8,0<=X<=10,7)
pweibull(10.7,shape = shape, scale = scale) - pweibull(8,shape = shape, scale = scale)

qnorm(0.25, 266,16)
qexp (0.5,1/12)
1-pexp(24,1/12)
1-pweibull(9, scale = 7, shape = 2)
pweibull(10.7, scale = 7, shape = 2)-pweibull(8, scale = 7, shape = 2)
pweibull(10.7, scale = 7, shape = 2)-pweibull(8.0, scale = 7, shape = 2)
pnorm(2.4,0,3)-pnorm(0,0,3)
1-pbinom(0,3,0.288)
# *d)	####
# Vítr pohybuje silnějšími větvemi, telegrafní dráty sviští, používání deštníku 
# se stává obtížným. To odpovídá tomu, že vítr má rychlost v rozmezí 
# 10,8 m/s až 13,8 m/s (6. stupeň Beaufortovy stupnice). S jakou pravděpodobnosti 
# rychlost větru překročila 12,0 m/s?

# P(X>12 | 10,8<=X<=13,8) = 
# = P(X>12 & 10,8<=X<=13,8)/P(10,8<=X<=13,8) =
# = P(12<=X<=13,8)/P(10,8<=X<=13,8)

# P(12<=X<=13,8)


citatel = pweibull(13.8,shape = shape, scale = scale) - pweibull(12,shape = shape, scale = scale)

# P(10,8<=X<=13,8)
jmenovatel = pweibull(13.8,shape = shape, scale = scale) - pweibull(10.8,shape = shape, scale = scale)

# P(12<=X<=13,8)/P(10,8<=X<=13,8)
citatel/jmenovatel

# Příklad 5. ####
# Výsledky měření jsou zatíženy jen normálně rozdělenou chybou 
# s nulovou střední hodnotou a se směrodatnou odchylkou 3 mm. 

# *a)	####
# Načrtněte graf hustoty pravděpodobnosti chyby měření.

# X ... chyba měření (mm)
# X~N(mu = 0,sigma = 3)

# parametry rozdělení
mu = 0
sigma = 3

x = seq(mu-3.5*sigma,mu+3.5*sigma,0.01)
f = dnorm(x,mu,sigma)
data = data.frame(x,f) # příprava data.frame

# Generování grafu
ggplot(data = data, aes(x = x, y = f)) +
  geom_line() +
  theme_bw() +
  labs(x="chyba měření (mm)",
       y = "hustota p-sti")

# Uložíme graf, který je aktuálně vytvořen v graf. okně do pracovního adresáře
ggsave("SNV_priklad5.svg",
       width = 15, height = 5,
       unit = "cm")

# *b)	####
# S jakou pravděpodobností bude chyba měření v rozsahu 0,0 mm až 2,4 mm?

# P(0,0<=X<=2,4)
pnorm(2.4,mu,sigma)-pnorm(0,mu,sigma)

# *c)	####
# Jaká velikost chyby měření nebude překročena s pravděpodobností 80 %?

# P(|X|<T)=0,8 -> P(-T<X<T) = 0,8
# Vzhledem k symetrii rozdělení: P(0<X<T)=0,4 -> P(X>T) = 0,1
# Tj. hledáme 90% kvantil
qnorm(0.9,mu,sigma)

# Kontrola: P(-3,9<X<3,9) - máme-li zajistit, že s min. 80% p-stí bude chyba 
# měření uvnitř daného intervalu, musíme zaokrouhlit nahoru
pnorm(3.9,mu,sigma)-pnorm(-3.9,mu,sigma)

# *d)	####
# Jaká je pravděpodobnost, že při 3 měřeních bude alespoň jednou chyba měření 
# v intervalu (0,0 mm; 2,4 mm)?

# Y ... počet chyb měření (ze 3), které budou v intervalu (0,0 mm; 2,4 mm)
# Y~Bi(n = 3,p = ?)

# Nutno dopočítat p-st, že chyba bude v intervalu (0,0 mm; 2,4 mm)
# P(0,0<=X<=2,4)
p = pnorm(2.4,mu,sigma)-pnorm(0,mu,sigma)
p
# Nyní se můžeme vrátit k položené otázce
# Y~Bi(n = 3,p = 0.288)

# parametry rozdělení
n = 3
p

# P(Y>=1) = 1-P(Y<1) = 1-P(Y=0) # Pozor! Modelujeme DNV - na ostré/neostré nerovnosti záleží
1-dbinom(0,n,p)

# *e)	####
# S jakou pravděpodobností budeme muset uskutečnit alespoň 5 měření do chvíle, 
# než detekujeme chybu měření větší než 3 mm?

# Z ... počet měření do chvíle, kdy bude detekována chyba měření větší než 3 mmm
# Z~NB(k = 1,p = ?)

# Nutno dopočítat p-st, že chyba měření bude větší než 3 mm
# P(X>3)
p = 1 - pnorm(3,mu,sigma)
p
# Nyní se můžeme vrátit k položené otázce
# Z~NB(k = 1,p = 0.159)

# parametry rozdělení
k = 1
p

# P(Z>=5) = 1-P(Z<5) = 1-P(Z<=4) # Pozor! U neg. binom.NV musíme převést na "počet neúspěchů před k. tým úspěchem".
1-pnbinom(4-k,k,p)