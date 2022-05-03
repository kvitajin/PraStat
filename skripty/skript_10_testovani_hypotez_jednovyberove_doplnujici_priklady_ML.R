# ......................................................................................
# ................Cvičení 10 - Testování hypotéz (jednovýběrové testy)..................
# ................................... Doplňující příklady ..............................
# ........................... Martina Litschmannová, Adéla Vrtková .....................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

# Aktivace všech potřebných balíčků (v případě potřeby nutno nainstalovat)
library(readxl) # načtení xlsx souboru
library(dplyr) # pro efektivní preprocessing (práci s datovým rámcem)
library(ggplot2) # pro hezčí grafiku
library(tibble) # umožňuje nastavení počtu des. míst ve výstupech balíčku dplyr
library(EnvStats) # funkce varTest() pro odhad a test rozptylu
library(moments) # funkce skewness(), kurtosis()
library(dlookr) # funkce normality() pro výpočet Shapirova-Wilkova testu pomocí dplyr
library(lawstat) # funkce symmetry.test() pro ověření symetrie

# Nastavení výstupů
options(scipen = 100, # číselné hodnoty zobrazovat jako desetinná čísla
        OutDec = ",", # oddělovač desetinných míst v tabulkách a grafech
        signif = 5, # počet platných cifer ve výstupech klasického Rka
        pillar.sigfig=5) # počet platných cifer ve výstupech balíčku dplyr

# Příklad 1. ####
# Výběrovým šetřením provedeným na dané VŠ byly získány informace o IQ 20 
# náhodně vybraných studentů této VŠ. Průměrné IQ těchto studentů bylo 110 bodů, 
# výběrová směrodatná odchylka byla 10 bodů. Předpokládejme, že IQ má normální rozdělení.
#*a)	####
# Na hladině významnosti 5 % ověřte, zda se střední hodnota IQ studentů na dané VŠ 
# statisticky významně liší od 108 bodů. Pro ověření využijte intervalový odhad středního IQ.


# Výpočet 97,5% (1-alfa/2) kvantilu Studentova rozdělení s 19 stupni volnosti
kv = qt(0.975,19)

# Výpočet mezí 95% intervalového odhadu střední hodnoty
110-kv*10/sqrt(20) # dolní mez
110+kv*10/sqrt(20) # horní mez

#*b)	####
# Na hladině významnosti 5 % ověřte, zda se střední hodnota IQ studentů na dané VŠ 
# statisticky významně liší od 108 bodů. Pro ověření využijte čistý test významnosti.

# Pozorovaná hodnota testové statistiky T(X)
x.obs = ((110-108)/10)*sqrt(20)
x.obs

# Výpočet p-hodnoty
# HA je ve tvaru "<>", proto p-hodnota = 2.min(F(x.obs),1-F(x.obs)),
# tj. p-hodnota = 2.min(P(T(X)<x.obs),P(T(X)>=x.obs))

# Testová statistika T(X) má Studentovo rozdělení s 19 stupni volnosti
F.x.obs = pt(x.obs,19)
F.x.obs
1-F.x.obs

p.hodnota = 2*min(F.x.obs,1-F.x.obs)
p.hodnota

#*c)	####
# Na hladině významnosti 5 % ověřte, zda střední hodnota IQ studentů na dané VŠ 
# statisticky významně převyšuje 108 bodů. Pro ověření využijte intervalový odhad středního IQ.

# HA je ve tvaru ">", proto hledáme levostranný intervalový odhad

# Výpočet 95% (1-alfa) kvantilu Studentova rozdělení s 19 stupni volnosti
kv = qt(0.95,19)

# Výpočet mezí 95% levostranného intervalového odhadu střední hodnoty
110-kv*10/sqrt(20) # dolní mez
# horní mez je nekonečno

#*d)	####
# Na hladině významnosti 5 % ověřte, zda střední hodnota IQ studentů na dané VŠ 
# statisticky významně převyšuje 108 bodů. Pro ověření využijte čistý test významnosti.

# Pozorovaná hodnota testové statistiky T(X)
x.obs = ((110-108)/10)*sqrt(20)
x.obs

# Výpočet p-hodnoty
# HA je ve tvaru ">", proto p-hodnota = 1-F(x.obs) = P(T(X)>=x.obs)

# Testová statistika T(X) má Studentovo rozdělení s 19 stupni volnosti
F.x.obs = pt(x.obs,19)
F.x.obs
1-F.x.obs

p.hodnota = 1-F.x.obs
p.hodnota

# Všimněte si, že p-hodnota u jednostranného testu vyšla poloviční oproti
# p-hodnotě testu oboustranného (viz b).

# Příklad 2. ####
# Data potřebná pro vlastní analýzu (data_hraci2.xlsx) najdete v LMS 
# (10. týden). Data jsou inspirována herní platformou Steam, kterou 
# pravděpodobně znáte. V souboru najdete ID sledovaných hráčů, jejich 
# celkové doby hraní za rok 2018 a 2019 a používané verze operačních 
# systémů (OS). 

#a) ####
# Na hladině významnosti 10 % ověřte, zda střední doba odehraná hráči 
# (příp. medián doby odehrané hráči) s OS Linux během roku 2018 statisticky 
# významně převyšuje 300 hodin. Pro ověření využijte příslušný intervalový odhad.

# Nastavte si pracovní adresář a do něj si nahrajte z LMS potřebná data
setwd(".....")

# Data načteme
data = read_excel("data_hraci2.xlsx")

#*Preprocessing ####
# Z dat vybereme pouze data potřebná pro příklad 2
dataL = data %>% 
  filter(system == "Linux") %>% 
  droplevels() # tímto příkazem zajistíme, že faktor system 
               # nebude obsahovat varianty obsažené v datovém rámci data 
               # a nyní nevyužívané (OSX, WIN)

# Zkontrolujeme, zda v datech nejsou odlehlá pozorování, 
# popř. je pro další analýzu odstraníme. 

boxplot(dataL$odehrane_hod_2018) # Data neobsahují odlehlá pozorování.

dataL %>% 
  summarise(počet_hodnot = length(na.omit(odehrane_hod_2018)), # abychom věděli, jak zaokrouhlit směr. odchylku
            smer.odchylka = sd(odehrane_hod_2018,na.rm = T),   # abychom věděli, jak zaokrouhlit odhady střední hodnoty
            prumer = mean(odehrane_hod_2018,na.rm = T),
            median = quantile(odehrane_hod_2018,prob = 0.5,na.rm = T),
            šikmost = skewness(odehrane_hod_2018,na.rm=TRUE),
            špičatost = moments::kurtosis(odehrane_hod_2018,na.rm=TRUE)-3)

#*Jak odhady zaokrouhlovat? ####
# Je-li rozsah výběru 30-2 000, zaokrouhlujeme směr. odchylku nahoru 
# na 3 platné cifry (dle Manuálu pro zaokrouhlování).

# V tomto případě 3 platné cifry odpovídá zaokrouhlení na desetiny.
# Odhady měr polohy (střední hodnoty / mediánu) a směr. odchylky budeme 
# zaokrouhlovat na stejný řád, tj. na desetiny.

#*Ověření předpokladů ####

#Ověření normality
# Kontrolujeme, zda šikmost a špičatost leží v intervalu (-2;2) (viz výše)

# Kontrolujeme vzhled Q-Q grafu
qqnorm(dataL$odehrane_hod_2018)
qqline(dataL$odehrane_hod_2018)

# Předpokládáme, že data jsou výběrem z normálního rozdělení (zatím
# pouze předpoklad na základě explorační analýzy). 

# Náš předpoklad o normalitě dat ověříme exaktním testem (na přednášce bude probíráno v 13. týdnu)
# H0: Data jsou výběrem z normálního rozdělení.
# HA: Data nejsou výběrem z normálního rozdělení.
shapiro.test(dataL$odehrane_hod_2018)
# nebo
dataL %>% normality(odehrane_hod_2018)

# Na hladině významnosti 0,10 nelze zamítnou předpoklad normality
# (Shapirův-Wilkův test, x.obs = 0,985, p-hodnota = 0,700).

# Tj. budeme odhadovat střední hodnotu (střední dobu odehranou během roku 2018).

#*Bodový odhad
mean(dataL$odehrane_hod_2018,na.rm = T) # zaokrouhlujeme dle výše uvedeného na desetiny
#*90% levostranný intervalový odhad (alternativa je ve tvaru ">")
t.test(dataL$odehrane_hod_2018,
       mu = 300,
       conf.level = 0.90,
       alternative = "greater") # zaokrouhlujeme stejně jako bodový odhad,
                                # dolní mez dolů, horní mez nahoru

#b) ####
# Na hladině významnosti 10 % ověřte, zda střední doba odehraná hráči 
# (příp. medián doby odehrané hráči) s OS Linux během roku 2018 statisticky 
# významně převyšuje 300 hodin. Pro ověření využijte čistý test významnosti.

# Preprocessing, včetně ověření předpokladů pro použití t testu, byl proveden
# v bodě a)

#*Souhrnný výpočet ####
# 90% levostranného intervalového odhadu stř. hodnoty a příslušného testu
t.test(dataL$odehrane_hod_2018,
       conf.level = 0.90, # spolehlivost odhadu
       mu = 300, # hodnota ověřovaná v H0
       alternative = "greater") #alternativa je ve tvaru ">"

#c) ####
# Na hladině významnosti 10 % ověřte, zda je směrodatná odchylka doby odehrané
# hráči s OS Linux během roku 2018 statisticky významně nižší než 25 hodin. 
# Pro ověření využijte příslušný intervalový odhad i čistý test významnosti.

# Preprocessing a ověření předpokladů (normality) bylo provedeno v bodě a)
#*Bodový odhad
sd(dataL$odehrane_hod_2018,na.rm = T) # zaokrouhlujeme dle výše uvedeného na desetiny
#*90% pravostranný intervalový odhad a test rozptylu(alternativa je ve tvaru "<")
varTest(dataL$odehrane_hod_2018, # nutno mít aktivován balíček EnVStats
        sigma.squared = 25^2,    # pozor - jde o test rozptylu
        conf.level = 0.90,
        alternative = "less") #alternativa je ve tvaru "<"

# Přepočet int. odhadu sm. odchylky (odmocnění mezí int. odhadu rozptylu)
sqrt(varTest(dataL$odehrane_hod_2018, 
             sigma.squared = 25^2, 
             conf.level = 0.90,
             alternative = "less")$conf.int)
# zaokrouhlujeme stejně jako bodový odhad - dolní mez dolů, horní mez nahoru

#d) ####
# Na hladině významnosti 10 % ověřte, zda podíl hráčů s OS Linux, 
# kteří během roku 2018 odehráli více než 280 hodin statisticky významně 
# převyšuje 60 %. Pro ověření využijte příslušný intervalový odhad i čistý test významnosti.

# Bodový odhad
length(na.omit(dataL$odehrane_hod_2018)) # rozsah výběru (n)
length(na.omit(dataL$odehrane_hod_2018[dataL$odehrane_hod_2018>280])) # rozsah výběru (n)

p = 42/58
p

# Ověření předpokladů pro IO
9/p/(1-p)
58>9/p/(1-p)

#90% levostranný Clopperův-Pearsonův intervalový odhad a příslušný test
binom.test(42,58,
           p = 0.6,                   # hodnota testovaná v nulové hypotéze
           alternative = "greater",   #alternativa je ve tvaru ">"
           conf.level = 0.9)
