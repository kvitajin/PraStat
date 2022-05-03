# ......................................................................................
# ................Cvičení 9 - Bodové a intervalové odhady (jednovýběrové)...............
# ................................... Doplňující příklady ..............................
# ........................... Martina Litschmannová, Adéla Vrtková .....................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

# Aktivace všech potřebných balíčků (v případě potřeby nutno nainstalovat)
library(readxl)   # načtení xlsx souboru
library(dplyr)    # pro efektivní preprocessing (práci s datovým rámcem)
library(ggplot2)  # pro hezčí grafiku
library(tibble)   # umožňuje nastavení počtu des. míst ve výstupech balíčku dplyr
library(EnvStats) # funkce varTest() pro odhad rozptylu
library(moments)  # funkce skewness(), kurtosis()
library(binom)    # funkce binom.confint() pro odhady p-stí
library(rstatix)  # funkce identify_outliers()
library(dlookr)   # funkce normality()

# Nastavení výstupů
options(scipen = 100, # číselné hodnoty zobrazovat jako desetinná čísla
        OutDec = ",", # oddělovač desetinných míst v tabulkách a grafech
        signif = 5, # počet platných cifer ve výstupech klasického Rka
        pillar.sigfig=5) # počet platných cifer ve výstupech balíčku dplyr

# Příklad 1. ####
# Určete bodový a 95% intervalový odhad střední hodnoty IQ 
# studentů jisté VŠ, víte-li, že výběrovým šetřením provedeným na dané VŠ
# byly získány informace o IQ 20 náhodně vybraných studentů této VŠ. 
# Průměrné IQ těchto studentů bylo 110 bodů, výběrová směrodatná odchylka
# byla 10 bodů. Vztah pro výpočet intervalového odhadu odvoďte. 
# Výsledky interpretujte.

# Výpočet kvantilů Studentova rozdělení s 19 stupni volnosti
qt(0.025,19)
qt(0.975,19)

# Výpočet mezí 95% intervalového odhadu střední hodnoty
110-qt(0.975,19)*10/sqrt(20)
110+qt(0.975,19)*10/sqrt(20)

# Příklad 2. ####
# Data potřebná pro vlastní analýzu (data_hraci2.xlsx) najdete v LMS 
# (9. týden). Data jsou inspirována herní platformou Steam, kterou 
# pravděpodobně znáte. V souboru najdete ID sledovaných hráčů, jejich 
# celkové doby hraní za rok 2018 a 2019 a používané verze operačních 
# systémů (OS). (Poznámka: Splnění předpokladů pro použití metod 
# statistické indukce posuďte pomocí explorační analýzy.)

#a) ####
# Určete bodový odhad a 90% oboustranný intervalový odhad střední doby 
# (příp. mediánu doby) odehrané hráči s OS Linux během roku 2018. 
# Výsledky interpretujte.

# Nastavte si pracovní adresář a do něj si nahrajte z LMS potřebná data
setwd("C:/Users/lit40/OneDrive - VSB-TUO/Vyuka/PASTA/Materialy/Doplnujici_priklady")

# Data načteme
data = read_excel("../data/data_hraci2.xlsx")

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

#*Jak odhady zaokrouhlovat? ####
length(na.omit(dataL$odehrane_hod_2018)) # rozsah výběru
# Je-li rozsah výběru 30-2 000, zaokrouhlujeme směr. odchylku nahoru 
# na 3 platné cifry (dle Manuálu pro zaokrouhlování).
sd(dataL$odehrane_hod_2018,na.rm = T) # směrodatná odchylka
# V tomto případě 3 platné cifry odpovídá zaokrouhlení na desetiny.
# Odhady měr polohy (střední hodnoty / mediánu) a směr. odchylky budeme 
# zaokrouhlovat na stejný řád, tj. na desetiny.

#*Ověření předpokladů ####

#Ověření normality
# Kontrolujeme, zda šikmost a špičatost leží v intervalu (-2;2)
moments::skewness(dataL$odehrane_hod_2018) 
moments::kurtosis(dataL$odehrane_hod_2018)-3
# Kontrolujeme vzhled Q-Q grafu
qqnorm(dataL$odehrane_hod_2018)
qqline(dataL$odehrane_hod_2018)

# Předpokládáme, že data jsou výběrem z normálního rozdělení (zatím
# pouze předpoklad na základě explorační analýzy). 
# Tj. budeme odhadovat střední hodnotu (střední dobu odehranou během roku 2018).

#*Bodový odhad
mean(dataL$odehrane_hod_2018,na.rm = T) # zaokrouhlujeme dle výše uvedeného na desetiny
#*90% oboustranný intervalový odhad
t.test(dataL$odehrane_hod_2018,
       conf.level = 0.90,
       alternative = "two.sided") # zaokrouhlujeme stejně jako bodový odhad,
                                  # dolní mez dolů, horní mez nahoru

#b) ####
# Určete bodový odhad a 90% levostranný intervalový odhad střední doby 
# (příp. mediánu doby) odehrané hráči s OS Linux během roku 2018. 
# Výsledky interpretujte.

# Preprocessing a ověření předpokladů bylo provedeno v bodě a)
#*Bodový odhad
mean(dataL$odehrane_hod_2018,na.rm = T) # zaokrouhlujeme dle výše uvedeného na desetiny
#*90% oboustranný intervalový odhad
t.test(dataL$odehrane_hod_2018,
       conf.level = 0.90,
       alternative = "greater") # zaokrouhlujeme stejně jako bodový odhad,
                                # dolní mez dolů, horní mez nahoru

#c) ####
# Určete bodový odhad a 90% pravostranný intervalový odhad střední doby 
# (příp. mediánu doby) odehrané hráči s OS Linux během roku 2018. 
# Výsledky interpretujte.

# Preprocessing a ověření předpokladů bylo provedeno v bodě a)
#*Bodový odhad
mean(dataL$odehrane_hod_2018,na.rm = T) # zaokrouhlujeme dle výše uvedeného na desetiny
#*90% oboustranný intervalový odhad
t.test(dataL$odehrane_hod_2018,
       conf.level = 0.90,
       alternative = "less") # zaokrouhlujeme stejně jako bodový odhad,
                             # dolní mez dolů, horní mez nahoru

#d) ####
# Určete bodový odhad a 90% oboustranný intervalový odhad 
# směrodatné odchylky doby odehrané hráči s OS Linux během roku 2018. 
# Výsledky interpretujte.

# Preprocessing a ověření předpokladů bylo provedeno v bodě a)

# Bodový odhad
sd(dataL$odehrane_hod_2018,na.rm = T) # zaokrouhlujeme dle výše uvedeného na desetiny
#*90% oboustranný intervalový odhad
#odhad rozptylu
varTest(dataL$odehrane_hod_2018,
        conf.level = 0.90,
        alternative = "two.sided")
#pouze intervalový odhad
varTest(dataL$odehrane_hod_2018,
        conf.level = 0.90,
        alternative = "two.sided")$conf.int

#odhad směrodatné odchylky
sqrt(varTest(dataL$odehrane_hod_2018,
        conf.level = 0.90,
        alternative = "two.sided")$conf.int)

#e) ####
# Určete bodový odhad a 90% oboustranný intervalový odhad p-sti, 
# že hráč s OS Linux během roku 2018 odehrál více než 280 hodin. 
# Výsledky interpretujte.

# Bodový odhad
length(na.omit(dataL$odehrane_hod_2018)) # rozsah výběru (n)
length(na.omit(dataL$odehrane_hod_2018[dataL$odehrane_hod_2018>280])) # rozsah výběru (n)

p = 42/58
p

# Ověření předpokladů pro IO
9/p/(1-p)
58>9/p/(1-p)

#90% (oboustranný) Clopperův-Pearsonův (intervalový odhad)
binom.test(42,58,
           alternative = "two.sided",
           conf.level = 0.9)

# 11 nejčastěji používaných IO p-stí
# Vzorce a tabulky uvádí Waldův (asymptotický) odhad,
# doporučujeme používat Clopperův-Pearsonův (exaktní) odhad
binom.confint(42,58,
              conf.level = 0.9)

# Příklad 3. ####
# V tomto příkladu budeme pracovat se stejnými daty jako v příkladě 2. 
# Určete bodové odhady a 90% oboustranné intervalové odhady střední doby 
# (příp. mediánu doby) odehrané hráči s jednotlivými OS během roku 2018. 
# Výsledky interpretujte.(Poznámka: Splnění předpokladů pro použití metod statistické indukce 
# posuďte pomocí explorační analýzy.)

# Data načteme
data = read_excel("data_hraci2.xlsx")

#*Preprocessing ####
# Ověříme, zda v datech nejsou odlehlá pozorování
boxplot(data$odehrane_hod_2018~data$system)

# Odlehlá pozorování odstraníme
outliers = data %>% 
  group_by(system) %>% 
  identify_outliers(odehrane_hod_2018)

data$odehrane_hod_2018_out = ifelse(data$IDhrace %in% outliers$IDhrace, 
                                    NA, 
                                    data$odehrane_hod_2018)

# Zkontrolujeme krab. graf po odstranění odlehlých pozorování
boxplot(data$odehrane_hod_2018_out~data$system)

#*Ověření předpokladů ####

# Ověření normality
data %>% 
  group_by(system) %>% 
  summarise(šikmost = moments::skewness(odehrane_hod_2018_out,na.rm = T),
             špičatost = moments::kurtosis(odehrane_hod_2018_out,na.rm = T)-3) 

data %>% 
  group_by(system) %>%
  normality(odehrane_hod_2018_out)

#Q-Q grafy
ggplot(data,
       aes(sample = odehrane_hod_2018_out)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  facet_wrap("system",nrow = 1)

# Předpoklad normality považujeme pro doby hraní u všech OS za splněný.
# Tj. budeme odhadovat střední hodnoty doby hraní v roce 2018.

# Bodové a intervalové odhady střední hodnoty, 
# včetně pomocných informací pro určení toho, jak zaokrouhlovat
data %>% 
  group_by(system) %>% 
  summarise(počet_hodnot = length(na.omit(odehrane_hod_2018_out)), # abychom věděli, jak zaokrouhlit směr. odchylku
            smer.odchylka = sd(odehrane_hod_2018_out,na.rm = T), # abychom věděli, jak zaokrouhlit odhady střední hodnoty
            prumer = mean(odehrane_hod_2018_out,na.rm = T), # bodové odhady střední hodnoty
            dolni.mez = t.test(odehrane_hod_2018_out,
                              alternative = "two.sided",
                              conf.level = 0.9)$conf.int[1], # dolní mez IO střední hodnoty
            horni.mez = t.test(odehrane_hod_2018_out,
                               alternative = "two.sided",
                               conf.level = 0.9)$conf.int[2]) # horní mez IO střední hodnoty

# Příklad 4. ####
# Zadání najdete v Doplňujících příkladech (pdf).
#b) ####
# Určete bodový a 95% intervalový odhad senzitivity 
# a specificity testované soupravy. Výsledky srovnejte 
# s údaji uvedenými v příbalovém letáku. 
# (Nápověda - zjednodušeně: Senzitivita je p-st, že test 
# správně rozpozná pozitivní jedince, specificita je p-st, 
# že test správně rozpozná negativní jedince)

#*Bodový odhad senzitivity
p.sens = 231/243
p.sens

#*Ověření předpokladů pro IO
9/p.sens/(1-p.sens)
243>9/p.sens/(1-p.sens)

#*IO odhady senzitivity
binom.confint(231,243,
              conf.level = 0.95)

#*Bodový odhad specificity
p.spec = 264/265
p.spec

#*Ověření předpokladů pro IO
9/p.spec/(1-p.spec)
265>9/p.spec/(1-p.spec)

#c) ####
# Určete bodový a 95% intervalový odhad falešně pozitivního 
# a falešně negativního výsledku při použití testované soupravy. 

#*Bodový odhad FP
p.FP = 1/265
p.FP
#*Ověření předpokladů pro IO FP
9/p.FP/(1-p.FP)
265>9/p.FP/(1-p.FP)

#*Bodový odhad FN
p.FN = 12/243
p.FN
#*Ověření předpokladů pro IO FN
9/p.FN/(1-p.FN)
243>9/p.FN/(1-p.FN)
#*IO odhady FN
binom.confint(12,243,
              conf.level = 0.95)

#d) ####
# Určete bodový a 95% intervalový odhad senzitivity 
# testované soupravy vzhledem k PCR metodě s 25, popř. 32 cykly. 
# Výsledky srovnejte s údaji uvedenými v příbalovém letáku.

#*Bodový odhad senzitivity vzhledem k PCR s 25 cykly
p.25 = 202/205
p.25
#*Ověření předpokladů pro IO
9/p.25/(1-p.25)
205>9/p.25/(1-p.25)
#*Intervalové odhady senzitivity vzhledem k PCR s 25 cykly
binom.confint(202,205,
              conf.level = 0.95)

#*Bodový odhad senzitivity vzhledem k PCR s 32 cykly
p.32 = 227/235
p.32
#*Ověření předpokladů pro IO
9/p.32/(1-p.32)
235>9/p.32/(1-p.32)
#*Intervalové odhady senzitivity vzhledem k PCR s 32 cykly
binom.confint(227,235,
              conf.level = 0.95)

#g) ####
# Lze z uvedených údajů odhadnout pravděpodobnost 
# falešně negativního výsledku soupravy při detekování 
# tzv. „superpřenašečů“? Pokud ano, určete bodový 
# i 95% intervalový odhad této pravděpodobnosti.

#*Bodový odhad
p.FN.25=3/202
p.FN.25
#*Ověření předpokladů pro IO
9/p.FN.25/(1-p.FN.25)
202>9/p.FN.25/(1-p.FN.25)