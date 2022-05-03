# ......................................................................................
# ................Cvičení 11 - Testování hypotéz (dvouvýběrové testy)...................
# ................................... Doplňující příklady ..............................
# ........................... Martina Litschmannová, Adéla Vrtková .....................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

# Aktivace všech potřebných balíčků (v případě potřeby nutno nainstalovat)
library(readxl)  # načtení xlsx souboru
library(dplyr)   # pro efektivní preprocessing (práci s datovým rámcem)
library(ggplot2) # pro hezčí grafiku
library(tibble)  # umožňuje nastavení počtu des. míst ve výstupech balíčku dplyr
library(moments) # funkce skewness(), kurtosis()
library(dlookr)  # funkce normality() pro výpočet Shapirova-Wilkova testu pomocí dplyr
library(lawstat) # funkce symmetry.test() pro ověření symetrie
library(rstatix) # funkce identify_outliers()

# Nastavení výstupů
options(scipen = 100, # číselné hodnoty zobrazovat jako desetinná čísla
        OutDec = ",", # oddělovač desetinných míst v tabulkách a grafech
        signif = 5, # počet platných cifer ve výstupech klasického Rka
        pillar.sigfig=5) # počet platných cifer ve výstupech balíčku dplyr

# Příklad 1. ####
# Data potřebná pro vlastní analýzu (data_dvouvyberove.xlsx, list hraci) najdete 
# v LMS (11. týden). Data jsou inspirována herní platformou Steam, kterou 
# pravděpodobně znáte. V souboru najdete ID sledovaných hráčů, jejich celkové 
# doby hraní za rok 2018 a 2019 a používané verze operačních systémů (OS). 

# Nastavte si pracovní adresář a do něj si nahrajte z LMS potřebná data
setwd("C:/Users/Martina/OneDrive - VŠB-TU Ostrava/Výuka/Pravděpodobnost a statistika/Materiály/Doplnujici_priklady")

# Data načteme
data = read_excel("../data/data_dvouvyberove.xlsx",
                  sheet = "hraci")

# Vybereme data týkající se pouze hráčů s OS Windows a OS Linux
data = data %>% 
  filter(system == "WIN"|system == "Linux") %>% 
  droplevels()

#*a) ####
# Určete bodové odhady a 90% levostranné intervalové odhady středního meziročního
# (2018-2019) nárustu doby (příp. mediánu nárustu doby) odehrané hráči OS Linux a OS Windows. 
# Výsledky interpretujte.


# Vypočteme nárůst odehrané doby v letech 2018 - 2019
data$narust = data$odehrane_hod_2019-data$odehrane_hod_2018


#*Preprocessing ####
# Ověříme, zda v datech nejsou odlehlá pozorování
boxplot(data$narust~data$system)

# Data obsahují odlehlá pozorování, z dalšího zpracování je vyřadíme
outliers = data %>% 
  group_by(system) %>% 
  identify_outliers(narust)

data$narust.out = ifelse(data$IDhrace %in% outliers$IDhrace,NA,data$narust)

boxplot(data$narust.out~data$system)

#*Ověření předpokladů ####

# Ověření normality
#Q-Q grafy
ggplot(data,
       aes(sample = narust.out)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  facet_wrap("system",nrow = 1)

tapply(data$narust.out,data$system,
       moments::skewness,na.rm = T)
tapply(data$narust.out,data$system,
       moments::kurtosis,na.rm = T)-3
tapply(data$narust.out,data$system,
       shapiro.test)

# nebo pomocí balíčku dplyr
data %>% 
  group_by(system) %>% 
  summarise(šikmost = moments::skewness(narust.out,na.rm = T),
             špičatost = moments::kurtosis(narust.out,na.rm = T)-3) 

data %>% 
  group_by(system) %>% 
  normality(narust.out)

# Na hladině významnosti 0,05 nezamítáme předpoklad normality (viz ...).

# Jak zaokrouhlovat výstupy?
data %>% 
  group_by(system) %>% 
  summarise(rozsah = length(na.omit(narust.out)),
            smer.odch. = sd(narust.out,na.rm = T))

# Rozsahy výběrů >30 a <2 000, proto zaokrouhlujeme směr. odchylku na 3 platné cifry, tj. na desetiny.
# Všechny míry polohy zaokrouhlujeme stejně.

# Bodové a intervalové odhady stř. hodnot, 
# včetně pomocných informací pro určení toho, jak zaokrouhlovat
data %>% 
  group_by(system) %>% 
  summarise(počet_hodnot = length(na.omit(narust.out)), # abychom věděli, jak zaokrouhlit směr. odchylku
            smer.odchylka = sd(narust.out,na.rm = T),   # abychom věděli, jak zaokrouhlit odhady mediánů
            průměr = mean(narust.out,na.rm = T), # bodové odhady průměrů
            dolni.mez = t.test(narust.out,
                               alternative = "greater",       # levostranný odhad
                               conf.level = 0.9)$conf.int[1], # dolní mez IO stř. hodnoty
            horni.mez = t.test(narust.out,
                               alternative = "greater",       # levostranný odhad
                               conf.level = 0.9)$conf.int[2]) # horní mez IO stř. hodnoty

#*b) ####
# Určete, zda jsou pozorované meziroční (2018-2019) nárůsty odehrané doby 
# statisticky významné. K ověření hypotéz použijte nalezené intervalové odhady (viz a) 
# i příslušné čisté testy významnosti.

# Preprocessing i ověření předpokladů proběhlo v rámci řešení a)
tapply(data$narust.out,data$system,sd,na.rm = T)
tapply(data$narust.out,data$system,mean,na.rm = T)
tapply(data$narust.out,data$system,t.test,
       conf.level = 0.9,
       alternative = "greater")
# nebo
data %>% 
  group_by(system) %>% 
  summarise(počet_hodnot = length(na.omit(narust.out)), # abychom věděli, jak zaokrouhlit směr. odchylku
            smer.odchylka = sd(narust.out,na.rm = T),   # abychom věděli, jak zaokrouhlit odhady mediánů
            průměr = mean(narust.out,prob = 0.5,na.rm = T), # bodové odhady průměrů
            dolni.mez = t.test(narust.out,
                               alternative = "greater",       # levostranný odhad
                               conf.level = 0.9)$conf.int[1], # dolní mez IO průměrů
            horni.mez = t.test(narust.out,
                               alternative = "greater", 
                               conf.level = 0.9)$conf.int[2], # horní mez IO průměrů
            p.value = t.test(narust.out,
                             alternative = "greater")$p.value) # levostranný test, p-hodnota

#*c) ####
# Určete bodový a 90% intervalový odhad rozdílu středních meziročních nárustů 
# (popř. mediánů meziročních nárustů) dob odehraných hráči s OS Linux a OS Windows. 
# Výsledky interpretujte.

# Na základě vizualizace si uděláme úsudek o rozdílu mezi pozorovanými nárusty
# u hráčů s OS Linux a OS Windows

boxplot(data$narust.out~data$system)

# Ověříme předpoklady pro volbu vhodného odhadu. (Odhad rozdílu středních hodnot (popř. mediánů),
# proto vybíráme mezi Studentovým odhadem (pomocí dvouvýběrové t statistiky), Aspinové-Welchovým odhadem 
# (odhady rozdílu středních hodnot) a Mannovým-Whitneyho odhadem rozdílu mediánů.)

# Ověření normality
data %>% 
  group_by(system) %>% 
  normality(narust.out) # nutná aktivace balíčku dlookr

# Normalitu nelze zamítnout, proto budeme testovat shodu středních hodnot,
# tj. vybíráme mezi Studentovým odhadem a Aspinové-Welchovým odhadem.

# Ověření shody rozptylů
# Empiricky - pomocí krabicového grafu a poměru rozptylů
boxplot(data$narust.out~data$system)
tapply(data$narust.out,data$system,var,na.rm = T)
# poměr rozptylů (větší ku menšímu)
max(tapply(data$narust.out,data$system,var,na.rm = T))/min(tapply(data$narust.out,data$system,var,na.rm = T))

# Exaktně pomocí testu shody rozptylů
var.test(data$narust.out[data$system == "WIN"],data$narust.out[data$system == "Linux"], # nejdříve uvádíme proměnnou s větším výběrovým rozptylem
         alternative = "two.sided",
         conf.level = 0.90)
# Všimněme si, že součástí výstupu je i bodový a intervalový odhad poměru rozptylů

# Zamítli jsme předpoklad o shodě rozptylů, proto provedeme Aspinové-Welchův odhad.

# Bodové odhady středních nárustů dob hraní
tapply(data$narust.out,data$system,mean,na.rm = T)

# Bodový odhad rozdílu středních nárustů (pro lepší interpretaci vhodné volit "větší průměr"-"menší průměr")
mean(data$narust.out[data$system == "WIN"],na.rm = T)-mean(data$narust.out[data$system == "Linux"],na.rm = T)

# Aspinové-Welchův 90% oboustranný intervalový odhad
t.test(data$narust.out[data$system == "WIN"],data$narust.out[data$system == "Linux"], # nejdříve uvádíme proměnnou s větším průměrem
       var.equal = FALSE, # volba mezi Studentovým odhadem a Aspinové-Welchovým odhadem
       alternative = "two.sided", # volba oboustranného odhadu
       conf.level = 0.9)

#*d) ####
# Určete, zda je pozorovaný rozdíl středních meziročních nárustů (popř. mediánů meziročních nárustů) 
# dob odehraných hráči s OS Linux a OS Windows statisticky významný (na hladině významnosti 10 %). 
# Ověřte dle intervalového odhadu i pomocí čistého testu významnosti.

# Rozhodování o volbě vhodného intervalového odhadu / testu již bylo provedeno v bodě c)
# Na základě ověření předpokladů jsme zvolili Aspinové-Welchův odhad / test.

# Aspinové-Welchův 90% oboustranný intervalový odhad + příslušný test
t.test(data$narust.out[data$system == "WIN"],data$narust.out[data$system == "Linux"], # nejdříve uvádíme proměnnou s větším průměrem
       var.equal = FALSE, # volba mezi Studentovým odhadem a Aspinové-Welchovým odhadem
       alternative = "two.sided", # volba oboustranného odhadu / testu
       conf.level = 0.9)

#*e) ####
# Určete, zda je pozorovaný střední meziroční nárust (popř. medián meziročních nárustů) 
# dob odehraných hráči s OS Windows statisticky významně větší než u hráčů s OS Linux (na hladině významnosti 10 %). 
# K ověření hypotézy použijte příslušný intervalový odhad i čistý test významnosti.

# Opět srovnávám dvě střední hodnoty (popř. mediány) pro nezávislá data. Jediný rozdíl je v tom,
# že nás nezajímá jen to, zda se střední hodnoty (popř. mediány) liší,
# ale to, zda je jeden z těchto parametrů větší než druhý.

# Na základě ověření předpokladů v bodě c) volíme Aspinové-Welchův odhad/test.

# Zajímá nás, zda je jedna ze středních hodnot větší než druhá,
# tj. z hlediska rozdílu těchto parametrů nás zajímá, zda je příslušný rozdíl větší než nula.

# Důležitá poznámka:
# Rozdíl parametrů je vždy vhodné definovat tak, aby bodový odhad tohoto rozdílu byl kladný
# a následně dle zadané otázky ověřovat / testovat, zda je tento rozdíl statisticky významně
# větší / menší než nula.

# Protože rozdíl
mean(data$narust.out[data$system == "WIN"],na.rm = T)-mean(data$narust.out[data$system == "Linux"],na.rm = T)
# je kladný, děláme v tomto případě úsudky o rozdílu μ_WIN-μ_Linux.

# Chceme ověřovat/testovat, zda je střední meziroční nárůst dob odehraných hráči 
# s OS Windows statisticky významně větší než u hráčů s OS Linux, 
# proto budeme ověřovat, zda μ_WIN-μ_Linux>0.
# Tj. volíme levostranný odhad / test.

# Aspinové-Welchův 90% levostranný intervalový odhad + příslušný test
t.test(data$narust.out[data$system == "WIN"],data$narust.out[data$system == "Linux"], # nejdříve uvádíme proměnnou s větším průměrem
       var.equal = FALSE, # volba mezi Studentovým odhadem a Aspinové-Welchovým odhadem
       alternative = "greater", # volba levostranného odhadu / testu
       conf.level = 0.9)

# Důležitá poznámka 2:
# Kdybychom chtěli ověřovat/testovat, zda je střední meziroční nárůst dob odehraných hráči 
# s OS Windows statisticky významně menší než u hráčů s OS Linux, 
# proto budeme ověřovat, zda μ_WIN-μ_Linux<0, tj. volili bychom pravostranný odhad / test.
# Ověřovaný rozdíl by zůstal definován jako μ_WIN-μ_Linux (tak, aby jeho bodový odhad byl kladný).

#*f) ####
# Pro každý ze srovnávaných OS určete bodový a 90% intervalový odhad pravděpodobnosti, 
# že meziroční nárust hrací doby byl u hráče větší než 10 hodin. 

# Bodový odhad daných pravděpodobností
# počet záznamů větších než 10 hodin
x = tapply(data$narust[data$narust>10],data$system[data$narust>10],length)
x
# počet všech záznamů
n = tapply(data$narust,data$system,length)
n
# bodový odhad pravděpodobností
p = x/n
p

# Ověření předpokladů
9/p/(1-p) # následuje "ruční" srovnání s rozsahy výběru (n)
# nebo
n>9/p/(1-p)

# 90% Clopperovy-Pearsonovy oboustranné odhady 
# Linux
binom.test(33,58,
           alternative = "two.sided",
           conf.level = 0.9)

# Windows
binom.test(127,202,
           alternative = "two.sided",
           conf.level = 0.9)

#*g) ####
# Určete, zda se pravděpodobnosti meziročního nárustu hrací doby o více než 10 hodin 
# statisticky významně (na hladině významnosti 10 %) liší v závislosti na používaném OS. 
# K ověření hypotézy použijte příslušný intervalový odhad i čistý test významnosti.

# Předpoklady pro použití intervalového odhadu / testu byly ověřeny v bodě f).

#
x # počty výskytů sledovaného jevu v jednotlivých výběrech
n # rozsahy výběrů
p # bodové odhady pravděpodobností

# Srovnáváme dva výběry, tj. opět k tomu využijeme intervalový odhad / test nějakého rozdílu parametrů.
# Pro snazší interpretaci je vhodné rozdíl opět definovat tak, aby jeho bodový odhad byl kladný.
# Tj. budeme dělat úsudky o π_WIN-π_Linux.

# Bodový odhad π_WIN-π_Linux
p[2]-p[1]

# 90% oboustranný intervalový odhad π_WIN-π_Linux
# Do funkce prop.test() zadáváme nejdříve informace vztažené k OS WIN a poté informace vztažené k OS Linux
prop.test(x = c(127,33),n = c(202,58),
          alternative = "two.sided",
          conf.level = 0.9)

# Příklad 2. ####
# Cílem analýzy je v rámci klinické studie prozkoumat efekt nového rehabilitačního postupu. 
# Data (data_dvouvyberove.xlsx, list pacienti) obsahují údaje o 60 probandech. U každého 
# probanda bylo po provedení komplexního testu (před a po rehabilitaci) zaznamenáno hodnocení 
# stavu probanda prostřednictvím skóre na škále 0–100 bodů (čím vyšší skóre, tím lepší stav). 

# Data načteme
data = read_excel("data_dvouvyberove.xlsx",
                  sheet = "pacienti")

#*a) ####
# Ověřte, zda zařazení probandů do skupin A a B bylo provedeno tak, že probandi ve skupině A 
# mají statisticky významně horší stav (před rehabilitací) než probandi skupiny B. 
# K ověření použijte vhodný intervalový odhad i příslušný čistý test významnosti.

# Prozkoumáme data z hlediska odlehlých pozorování a uděláme si úsudek o dané otázce.
boxplot(data$pred_rehab~data$skupina)

# Data obsahují odlehlá pozorování, ta před další analýzou odstraníme.
outliers = data %>% 
  group_by(skupina) %>% 
  identify_outliers(pred_rehab)

data$pred_rehab.out = ifelse(data$ID %in% outliers$ID,NA,data$pred_rehab)

boxplot(data$pred_rehab.out~data$skupina)

# Chceme porovnávat dvě skupiny (nezávislá data) z hlediska jejich hodnot, tj. budeme dělat úsudky
# o rozdílu jejich středních hodnot (popř. mediánu),
# tj. rozhodujeme se mezi Studentovým odhadem/testem, Aspinové-Welchovým odhadem/testem a Mannovým-Whitneyho odhadem/testem.

# Hladina významnosti není v zadání spedifikována, proto volíme standardní α = 0,05.

# Ověření normality
data %>% 
  group_by(skupina) %>% 
  normality(pred_rehab.out)

# Na hladině významnosti 0,05 zamítáme předpoklad normality (viz ...), proto nelze dělat úsudky o střední hodnotě.

# Musíme ověřit symetrii dat, popř. alespoň stejný tvar rozdělení (empiricky).
# histogramy
ggplot(data,aes(x = pred_rehab.out))+
  geom_histogram()+
  facet_wrap(~skupina)
# šikmost
data %>% 
  group_by(skupina) %>% 
  summarise(šikmost = moments::skewness(pred_rehab.out,na.rm = T))

# H0: Data jsou výběrem ze symetrického rozdělení.
# HA: Data nejsou výběrem ze symetrického rozdělení.
tapply(data$pred_rehab.out,data$skupina,
       symmetry.test, boot = F)
# nebo
data %>% 
  group_by(skupina) %>% 
  summarise(sym.p.value = symmetry.test(pred_rehab.out,boot = F)$p.value)
# Na hladině významnosti 0,05 nezamítáme předpoklad symetrie (viz ...).
# Tj. můžeme dělat úsudky o rozdílu mediánů pomocí Mannova-Whitneyho odhadu / testu.

# Pro snazší interpretaci je vhodné rozdíl opět definovat tak, aby jeho bodový odhad byl kladný.
data %>% 
  group_by(skupina) %>% 
  summarise(med = quantile(pred_rehab.out,prob = 0.5,na.rm = T))

# Tj. budeme dělat úsudky o med_B-med_A.

# Jak zaokrouhlovat výstupy?
data %>% 
  group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(pred_rehab.out)),
            smer.odch. = sd(pred_rehab.out,na.rm = T))

# Rozsahy výběrů <=30, proto zaokrouhlujeme směr. odchylku na 2 platné cifry, tj. na desetiny.
# Všechny míry polohy zaokrouhlujeme stejně.

# Bodový odhad med_B-med_A
quantile(data$pred_rehab.out[data$skupina == "B"],prob = 0.5,na.rm = T)-quantile(data$pred_rehab.out[data$skupina == "A"],prob = 0.5,na.rm = T)

# Máme ověřovat, zda probandi ve skupině A 
# mají statisticky významně horší stav (před rehabilitací) než probandi skupiny B, 
# tj. budeme zjišťovat, zda med_B-med_A>0 (horší stav = nižší hodnoty skóre),
# tj. budeme používat levostranný odhad / test.

# 95% intervalový odhad / test rozdílu mediánů med_B-med_A
wilcox.test(data$pred_rehab.out[data$skupina == "B"],data$pred_rehab.out[data$skupina == "A"],
            alternative = "greater",
            conf.level = 0.95,
            conf.int = T) # V této funkci je přednastaveno conf.int = F :-(

#*b) ####
# Ověřte, zda lze pozitivní efekt rehabilitace (vliv rehabilitace na zlepšení stavu probandů) 
# považovat za statisticky významný. Uvažujte všechny probandy (tj. nezkoumejte rozdíly mezi 
# skupinami A a B), k ověření využijte vhodný intervalový odhad i příslušný čistý test významnosti.

# Chceme posuzovat efekt rehabilitace, tj. musíme hodnotit "o kolik" se zlepšilo skóre probandů.
# Pokud došlo ke zlepšení stavu (zvýšení skóre), efekt rehabilitace byl pozitivní.

data$efekt = data$po_rehab - data$pred_rehab # kladná hodnota ... zlepšení stavu

# Prozkoumáme data, abychom identifikovali případná odlehlá pozorování a udělali si představu o dané otázce
boxplot(data$efekt)

# Data obsahují odlehlá pozorování, z dalšího zpracování je vyřadíme
outliers = data %>% 
  identify_outliers(efekt)

data$efekt.out = ifelse(data$ID %in% outliers$ID,NA,data$efekt)

boxplot(data$efekt.out)

# Chceme dělat úsudky o hodnotách efektu, tj. budeme dělat úsudky o jeho stř. hodnoty (popř. mediánu)
# Tj. zvažujeme Studentův odhad/test, popř. odhad/test mediánu (Wilcoxonův nebo znaménkový)

# Stále pracujeme s hladinou významnosti 0,05.

# Ověříme předpoklady
# Ověření normality
moments::skewness(data$efekt.out, na.rm = T)
moments::kurtosis(data$efekt.out, na.rm = T) - 3

ggplot(data,aes(x = efekt.out)) +
  geom_histogram()

ggplot(data,aes(sample = efekt.out)) +
  stat_qq() +
  stat_qq_line()

shapiro.test(data$efekt.out)

# Nezamítáme předpoklad normality, tj. budeme dělat úsudky o střední hodnotě.
# Chceme zjišťovat, zda došlo ke zlepšení stavu, tj. zda jsou efekty kladné, 
# tj. budeme ověřovat zda μ_efekt > 0 (levostranný odhad / test).

# Jak zaokrouhlovat výstupy?
data %>% 
 summarise(rozsah = length(na.omit(efekt.out)),
            smer.odch. = sd(efekt.out,na.rm = T))

# Rozsah výběru je větší než 30 a menší než 2 000, tj. směr. odchylku zaokrouhlujeme na 3 platné cifry,
# tj. na desetiny. Míry polohy zaokrouhlujeme na stejný řád.

# Bodový odhad
mean(data$efekt.out,na.rm = T)

# 95% levostranný intervalový odhad a příslušný test
t.test(data$efekt.out,
       alternative = "greater",
       conf.level = 0.95)

