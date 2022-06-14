# ......................................................................................
# ......Cvičení 13, 14 - Testování hypotéz (analýza závislosti v kont. tab.)............
# ................................... Doplňující příklady ..............................
# ................................. Martina Litschmannová ..............................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

# Poznámka: Pro bezproblémovou práci doporučujeme mít nainstalováno ggplot 2, verze alespoň 3.3.0 
# a plotly, verze alespoň 4.5.5 (viz záložka Packages). V případě potřeby spusťte znovu pro daný 
# balíček install.packages().

# Aktivace všech potřebných balíčků (v případě potřeby nutno nainstalovat)
library(readxl) # načtení xlsx souboru
library(dplyr) # pro efektivní preprocessing (práci s datovým rámcem)
library(ggplot2) # pro hezčí grafiku
library(tibble) # umožňuje nastavení počtu des. míst ve výstupech balíčku dplyr
library(ggmosaic) # geometrie geom_mosaic() pro mozaikový graf pomocí ggplot2
library(tidyr) # funkce uncount() pro převod long formátu kont.tabulky do standardního datového formátu
library(lsr) # funkce cramersV()
library(nortest) # funkce pearson.test()
library(epiR) # funkce epi.2by2()
library(svglite) # pro export obrázků ve formátu svg

# Nastavení výstupů
options(scipen = 100, # číselné hodnoty zobrazovat jako desetinná čísla
        OutDec = ",", # oddělovač desetinných míst v tabulkách a grafech
        signif = 5, # počet platných cifer ve výstupech klasického Rka
        pillar.sigfig=5) # počet platných cifer ve výstupech balíčku dplyr

# Nastavení pracovního adresáře
setwd("C:/Users/Martina/OneDrive - VŠB-TU Ostrava/Výuka/Pravděpodobnost a statistika/Materiály/Doplnujici_priklady")

# Příklad 1. ####
# V rámci sociologické studie (300 respondentů) byla zkoumána souvislost mezi úrovni vzdělání 
# a rodinným stavem mužské populace (viz níže).
kont.tab = matrix(c(18,12,6,3,
                    36,36,9,9,
                    21,45,9,9,
                    9,36,3,6,
                    6,21,3,3),
                  byrow = T,nrow = 5)
kont.tab = as.table(kont.tab)
kont.tab
rownames(kont.tab) = c("bez maturity","SŠ s maturitou","bakalářské","magisterské","doktorské")
colnames(kont.tab) = c("svobodný","ženatý","rozvedený","vdovec")
kont.tab = as.table(kont.tab)
kont.tab

#*a) ####
# Doplňte tabulku o sumární řádek a sloupec a o řádkové relativní četnosti.

#*Explorační analýza ####
prop.table(kont.tab)  # sdružené relativní četnosti
prop.table(kont.tab,1)  # řádkové relativní četnosti
prop.table(kont.tab,2)  # sloupcové relativní četnosti

# Příprava kont. tabulky doplněné o sumární řádek a sloupec a řádkové rel. četnosti

# Doplnění sumárního řádku a sloupce.
tab = addmargins(kont.tab)
tab
rownames(tab)[nrow(tab)] = "celkem"
colnames(tab)[ncol(tab)] = "celkem"
tab

# Dopočet řádkových rel. četností a jejich sloučení se sdruženými četnostmi
r.rel.cetnosti = round(100*tab/tab[,"celkem"],0) # Nutno nastavit vhodné zaokrouhlení!
r.rel.cetnosti 

# Kontrola případné zaokrouhlovací chyby
r.rel.cetnosti[,"celkem"] = rowSums(r.rel.cetnosti[,1:ncol(r.rel.cetnosti)-1])
r.rel.cetnosti # zaokrouhlovací chyba se projevila

# Odstranění vlivu zaokrouhlovací chyby
for (i in 1:nrow(r.rel.cetnosti)){             
  r.rel.cetnosti[i,ncol(r.rel.cetnosti)-1]= 
    100 - sum(r.rel.cetnosti[i,1:(ncol(r.rel.cetnosti)-2)]) 
}
r.rel.cetnosti[,"celkem"] = rowSums(r.rel.cetnosti[,1:ncol(r.rel.cetnosti)-1])
r.rel.cetnosti # vliv zaokrouhlovací chyby byl eliminován

tab2 = matrix(paste0(tab," (",sprintf("%.0f",r.rel.cetnosti)," %)"), # za des. tečkou v "%.0f" se uvádí počet des. míst
             nrow = nrow(tab),
             byrow = F)
tab2
rownames(tab2) = rownames(tab)
colnames(tab2) = colnames(tab)
tab2
tab2[,"celkem"] = tab[,"celkem"] # V sumárním sloupci není nutno uvádět "100 %"
tab2

# Po exportu nutno případně ošetřit oddělovač des. míst (v cz textu - des. čárka)
write.csv2(tab2,"tab_priklad1.csv")

#*b) ####
# Závislost mezi úrovni vzdělání a rodinným stavem mužské populace vizualizujte 
# a doplňte vhodnou míru kontingence. Komentujte závěry, k nimž jste na základě 
# explorační analýzy došli.

# *Vizualizace ve standardním R (bez "vyšperkování") #####
# **Shlukový sloupcový graf ####
barplot(t(kont.tab),
        legend = colnames(kont.tab),
        beside = T)
# srovnejte s následujícím grafem a rozhodněte o tom, který z grafů je pro prezentaci daných dat vhodnější
barplot(kont.tab,
        legend = rownames(kont.tab),
        beside = T)

# **Skládaný sloupcový graf ####
barplot(t(kont.tab),
        legend = colnames(kont.tab))
# srovnejte s následujícím grafem a rozhodněte o tom, který z grafů je pro prezentaci daných dat vhodnější
barplot(kont.tab,
        legend = rownames(kont.tab))

# **Mozaikový graf ####
mosaicplot(t(kont.tab),
           las = 1, # otočení popisků osy y
           color = gray.colors(5))

# srovnejte s následujícím grafem a rozhodněte o tom, který z grafů je pro prezentaci daných dat vhodnější
mosaicplot(kont.tab,
           las = 1,
           color = gray.colors(5))

# *Vizualizace pomocí ggplot2 (včetně "vyšperkování") #####
# pracnější, doporučujeme používat v případě potřeby doplnění popisků a sjednocení "stylů" grafických výstupů

# Pro přípravu shlukového a skládaného sloupcového grafu potřebujeme vstupní
# datový soubor v tzv. long formátu

# **Převod kont. tabulky do formátů potřebných pro přípravu sloupcových grafů a mozaikového grafu ####
kont.tab
kont.tab.long = as.data.frame(kont.tab) # POZOR! kont.tab musí být typu table
kont.tab.long 
colnames(kont.tab.long) = c("vzdelani","rod.stav","cetnost")
kont.tab.long

# Pro přípravu shlukového sloupcového grafu je nutné long formát kont. tabulky seřadit
kont.tab.long = kont.tab.long %>% 
  arrange(vzdelani,rod.stav) # seřazujeme nejdříve podle nezávislé proměnné, poté podle závislé proměnné
kont.tab.long # kont. tabulka v long formátu vhodném pro přípravu sloupcových grafů

# Kont. tabulka ve standardním datovém formátu potřebném pro přípravu mozaikového grafu
kont.tab.sdf = uncount(kont.tab.long,cetnost)
kont.tab.sdf

# **Příprava popisků grafů ####
tab2
lab = tab2[1:nrow(kont.tab),1:ncol(kont.tab)] # vypustíme údaje ze sumárního řádku a sloupce
lab1 = as.vector(lab) # změníme typ popisků na vector (po sloupcích)
lab2 = as.vector(t(lab)) # změníme typ popisků na vector (po řádcích)
# Popisky pro grafy - nutno vždy ověřit, který z popisků je správně
lab1
lab2

# **Sloupcový graf ####
bar = 
  ggplot(data = kont.tab.long,aes(x = vzdelani,y = cetnost,fill = rod.stav)) +
  geom_col(position = "dodge",
           colour = "black") +   
  labs(y="počet respondentů", x="", title = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "",
                    values = gray.colors(5)[2:5]) + # vyhýbáme se nejtmavšímu odstínu šedi z dané palety
  geom_text(aes(label = lab2), 
            vjust = -0.3, 
            size = 4,
            position = position_dodge(0.9)) +
  ylim(0,1.1*max(kont.tab.long$cetnost))

bar # Nezapomeňte zkontrolovat popisky! (lab1 vs lab2)

# **Shlukový sloupcový graf ####
# Výpočet souřadnice y pro popisky
kont.tab.long = kont.tab.long %>%
  group_by(vzdelani) %>%
  mutate(label_y = cumsum(cetnost) - 0.5 * cetnost)

# Graf
stack.bar = 
  ggplot(data = kont.tab.long,aes(x = vzdelani,y = cetnost,fill = rod.stav)) +
  geom_col(colour = "black",
           position = position_stack(reverse = T)) +   
  labs(y="kumulativní počet respondentů", x="", title = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "",
                    values = gray.colors(5)[2:5]) +
  geom_text(aes(label = lab2,y = label_y), 
            colour = "black")

stack.bar # Nezapomeňte zkontrolovat popisky! (lab1 vs lab2)

# **Mozaikový graf ####
mos = 
  ggplot(data = kont.tab.sdf) +
  geom_mosaic(aes(x = product(rod.stav, vzdelani), # na první místo ve funkci product() dáváme závislou proměnnou
                  fill = rod.stav),
              offset = 0.02,
              colour = "black") +   
  labs(y="", x="", title = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = gray.colors(5)[2:5])

mos

# Doplnění popisků
mos +
  geom_text(data = ggplot_build(mos)$data[[1]], 
            aes(label=lab2, x = (xmin+xmax)/2, y = (ymin+ymax)/2),
            colour = "black")
# Nezapomeňte zkontrolovat popisky! (lab1 vs lab2)

ggsave("mos_priklad1.svg",
       width = 18,height = 10,
       units = "cm")

#*Výpočet Cramerova V ####
cramersV(kont.tab)

#*c) ####
# Ověřte, zda mezi úrovni vzdělání a rodinným stavem mužské populace existuje 
# statisticky významná souvislost. Výsledek interpretujte. 
# (Nezapomeňte na ověření předpokladů příslušného testu.) 

#*Test nezávislosti v kontingenční tabulce ####
pom = chisq.test(kont.tab)
attributes(pom)
pom$expected # Nutné pro ověření předpokladů
# 4 (20 %) očekávané četnosti jsou menší než 5, všechny oč. četnosti jsou větší než 1,
# tj. předpoklady testu jsou splněny.
pom

#Na hladině významnosti 0,05 lze zamítnout HO (p-hodnota = 0,023, Chí-kvadrát test dobré shody,
# df = 1). Pozorovanou závislost lze hodnotit jako slabou (Cramerovo V = 0,162).

# Příklad 2. ####
# V souboru data_pa.xlsx jsou zaznamenány údaje ze studie týkající se pohybové aktivity 
# vysokoškolských studentů studijních programů zaměřených na tělovýchovu, sport 
# a lékařských oborů souvisejících s pohybem (sportovní vědy) a studentů humanitních 
# studijních programů. 

data = read_excel("data/data_pa.xlsx")

#*a) ####
# Vytvořte kontingenční tabulku (doplněnou o sumární řádek a sloupec a vhodné relativní četnosti) 
# shrnující výsledky šetření s ohledem na závislost míry pohybové aktivity v době klasické výuky 
# a studijního oboru.

# Analyzované kvalitativní proměnné definujeme jako typ factor
# a varianty proměnných uspořádáme tak, jak je chceme mít uspořádány v kont. tabulce.

data$stud_obor = factor(data$stud_obor)
levels(data$stud_obor) # výpis variant
data$stud_obor = factor(data$stud_obor,
                        levels = c("humanitní vědy","sportovní vědy")) # uspořádání variant

data$pa_norm = factor(data$pa_norm)
levels(data$pa_norm) # výpis variant
data$pa_norm = factor(data$pa_norm,
                      levels = c("nízká","střední","vysoká")) # uspořádání variant

kont.tab = table(data$stud_obor,data$pa_norm) # jako první uvádíme (pokud možno) nezávisle proměnnou
kont.tab = as.table(kont.tab)
kont.tab

# Již nyní se zdá, že počet probandů s nízkou mírou pohybové aktivity (3) je přílíš nízký,
# proto už nyní raději ověříme, zda bude možno provádět zobecnění na populaci (metody stat. indukce).
# Popř. kont. tabulku zjednodušíme.

pom = chisq.test(kont.tab)
pom$expected # očekávané četnosti
# 2 z 6 (33 %) očekávaných četností jsou menší než 5, tj. nejsou splněny předpoklady pro test nezávislosti
# v kont. tabulce.

# Proto kont. tabulku zjednodušíme - sloučíme stupně pohybové aktivity nízká a střední.
data$pa_norm2 = factor(data$pa_norm,
                       levels = c("nízká","střední","vysoká"),
                       labels = c("nízká nebo střední","nízká nebo střední","vysoká"))

kont.tab = table(data$stud_obor,data$pa_norm2)
kont.tab = as.table(kont.tab)
kont.tab
pom = chisq.test(kont.tab)
pom$expected # očekávané četnosti
# Všechny očekávané četnosti jsou větší než 5, tj. předpoklady pro test nezávislosti jsou splněny.
# Požadované analýzy budeme provádět pro "zjednodušená data".

#*Explorační analýza ####
prop.table(kont.tab)  # sdružené relativní četnosti
prop.table(kont.tab,1)  # řádkové relativní četnosti
prop.table(kont.tab,2)  # sloupcové relativní četnosti

# Příprava kont. tabulky doplněné o sumární řádek a sloupec a řádkové rel. četnosti

# Doplnění sumárního řádku a sloupce.
tab = addmargins(kont.tab)
tab
rownames(tab)[nrow(tab)] = "celkem"
colnames(tab)[ncol(tab)] = "celkem"
tab

# Dopočet řádkových rel. četností a jejich sloučení se sdruženými četnostmi
r.rel.cetnosti = round(100*tab/tab[,"celkem"],0) # Nutno nastavit vhodné zaokrouhlení!
r.rel.cetnosti 

# Kontrola případné zaokrouhlovací chyby
r.rel.cetnosti[,"celkem"] = rowSums(r.rel.cetnosti[,1:ncol(r.rel.cetnosti)-1])
r.rel.cetnosti # zaokrouhlovací chyba se neprojevila

tab2 = matrix(paste0(tab," (",sprintf("%.0f",r.rel.cetnosti)," %)"), # za des. tečkou v "%.0f" se uvádí počet des. míst
              nrow = nrow(tab),
              byrow = F)
tab2
rownames(tab2) = rownames(tab)
colnames(tab2) = colnames(tab)
tab2
tab2[,"celkem"] = tab[,"celkem"] # V sumárním sloupci není nutno uvádět "100 %"
tab2

# Po exportu nutno případně ošetřit oddělovač des. míst (v cz textu - des. čárka)
write.csv2(tab2,"tab_priklad2.csv")

#*b) ####
# Analyzovanou závislost vizualizujte a doplňte o vhodnou míru kontingence. 
# Komentujte závěry, k nimž jste na základě explorační analýzy došli.

# Připravíme si data ve formátu vhodném pro vizualizaci.
# *Příprava popisků grafů ####
tab2
lab = tab2[1:nrow(kont.tab),1:ncol(kont.tab)] # vypustíme údaje ze sumárního řádku a sloupce
lab1 = as.vector(lab) # změníme typ popisků na vector (po sloupcích)
lab2 = as.vector(t(lab)) # změníme typ popisků na vector (po řádcích)
# Popisky pro grafy - nutno vždy ověřit, který z popisků je správně
lab1
lab2

# *Vstupy pro sloupcový graf:
kont.tab
kont.tab.long = as.data.frame(kont.tab) # POZOR! kont.tab musí být typu table
kont.tab.long 
colnames(kont.tab.long) = c("stud.obor","pa","cetnost")
kont.tab.long

# *Sloupcový graf:
bar = 
  ggplot(data = kont.tab.long,aes(x = stud.obor,y = cetnost,fill = pa)) +
  geom_col(position = "dodge",
           colour = "black") +   
  labs(y="počet respondentů", x="", title = "") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "",
                    values = gray.colors(3)[2:3]) + # vyhýbáme se nejtmavšímu odstínu šedi z dané palety
  geom_text(aes(label = lab1), 
            vjust = -0.3, 
            size = 4,
            position = position_dodge(0.9)) +
  ylim(0,1.1*max(kont.tab.long$cetnost))

bar # Nezapomeňte na kontrolu popisků!

# *Vstupy ro mozaikový graf:
kont.tab.sdf = uncount(kont.tab.long,cetnost)
kont.tab.sdf # popř. máme-li data ve stand. dat. formátu, můžeme je využít přímo

# *Mozaikový graf ####
mos = 
  ggplot(data = kont.tab.sdf) +
  geom_mosaic(aes(x = product(pa, stud.obor), # na první místo ve funkci product() dáváme závislou proměnnou
                  fill = pa),
              offset = 0.02,
              colour = "black") +   
  labs(y="", x="", title = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = gray.colors(5)[2:5])

mos

# Doplnění popisků
mos +
  geom_text(data = ggplot_build(mos)$data[[1]], 
            aes(label=lab2, x = (xmin+xmax)/2, y = (ymin+ymax)/2),
            colour = "black")
# Nezapomeňte na kontrolu popisků!

ggsave("mos_priklad2.svg",
       width = 13,height = 6,
       units = "cm")

# Výpočet Cramerova V
cramersV(kont.tab)

#*c) ####
# Ověřte, zda mezi studijním oborem a mírou pohybové aktivity existuje 
# statisticky významná souvislost. Výsledek interpretujte. (Nezapomeňte na ověření předpokladů příslušného testu.) 
pom = chisq.test(kont.tab)
pom$expected # Všechny očekávané četnosti jsou větší než 5.
pom

#*d) ####
# Určete bodový a 95% intervalový odhad pravděpodobnosti, že student vykazuje 
# v době klasické výuky vysoký stupeň pohybové aktivity. 
# Danou pravděpodobnost odhadněte pro každý typ studia (studijní obor) samostatně. 
# (Nezapomeňte ověřit nutné předpoklady!)
tab2

# humanitní vědy
p = 27/57
p
9/p/(1-p) # n (57) > 36,1, tj.předpoklady jsou splněny
prop.test(x = 27,n = 57,alternative = "two.sided",conf.level = 0.95)

# sportovní vědy
p = 38/51
p
9/p/(1-p) # n (51) > 47,4, tj.předpoklady jsou splněny
prop.test(x = 38,n = 51,alternative = "two.sided",conf.level = 0.95)

#*e) ####
# Určete bodový a 95% intervalový odhad toho, o kolik procent více studentů studijních programů
# se sportovním zaměřením vykazuje v době klasické výuky vysokou míru pohybové aktivity oproti 
# studentů s humanitním zaměřením. (Nezapomeňte ověřit nutné předpoklady!)

# Předpoklady byly ověřeny v bodě e)
# Chceme ověřit, zda π_sport > π_hum, proto volíme levostranný odhad.
tab2
p_hum = 27/57
p_sport = 38/51
p_sport-p_hum # bodový odhad rozdílu
# 95% levostranný intervalový odhad rozdílu
prop.test(x = c(38,27),n = c(51,57),alternative = "greater",conf.level = 0.95)

# Příklad 3. ####
# V souboru data_pa.xlsx jsou zaznamenány údaje ze studie týkající se pohybové aktivity 
# vysokoškolských studentů studijních programů zaměřených na tělovýchovu, sport 
# a lékařských oborů souvisejících s pohybem (sportovní vědy) a studentů humanitních 
# studijních programů. Analyzujte souvislost mezi typem studia (studijní obor) 
# a BMI studentů. BMI přitom uvažujte pouze ve zjednodušené podobě: méně než 25 / 25 a více.

data = read_excel("data_pa.xlsx")
data
data$bmi = data$hmotnost/(data$vyska/100)^2
data$bmi.kat = "méně než 25"
data$bmi.kat[data$bmi>=25] = "25 a více"

#*a) ####
# Vytvořte asociační tabulku (doplněnou o sumární řádek a sloupec a vhodné relativní 
# četnosti) shrnující výsledky šetření s ohledem na sledovanou analýzu.

# Nejdříve uspořádáme varianty proměnné tak, jak mají být uspořádány v asociační tabulce.
# V případě asociační tabulky je toto obzvlášť důležité.
# Zjistíme-li, že jsme uspořádání zvolili špatně, musíme se k tomuto kroku vrátit
# a uspořádání proměnných upravit!!!

# Zkoumáme závislost BMI na studijním oboru, proto stud_obor je nezávisle proměnná,
# bmi.kat je závisle proměnná.
data$stud_obor = factor(data$stud_obor,
                        levels = c("humanitní vědy","sportovní vědy")) # na první místo dáváme "exponovanou" skupinu,
                                                                       # tj. skupinu, u níž pozorujeme vyšší riziko sledovaného jevu
data$bmi.kat = factor(data$bmi.kat,
                      levels = c("25 a více","méně než 25")) # na první místo dáváme výskyt jevu, který sledujeme

kont.tab = table(data$stud_obor,data$bmi.kat)
kont.tab
kont.tab = as.table(kont.tab)

# Ověříme, zda máme dostatek dat pro test nezávislosti (tj. ověříme předpoklady pro tento test)
pom = chisq.test(kont.tab)
pom$expected # Všechny očekávané četnosti jsou větší než 5.

#*Explorační analýza ####
prop.table(kont.tab)  # sdružené relativní četnosti
prop.table(kont.tab,1)  # řádkové relativní četnosti
prop.table(kont.tab,2)  # sloupcové relativní četnosti

# Doplníme asociační tabulku o sumární řádek a sloupec a o řádkové rel. četnosti.
tab = addmargins(kont.tab)
tab
rownames(tab)[nrow(tab)] = "celkem"
colnames(tab)[ncol(tab)] = "celkem"
tab

# Dopočet řádkových rel. četností a jejich sloučení se sdruženými četnostmi
r.rel.cetnosti = round(100*tab/tab[,"celkem"],0) # Nutno nastavit vhodné zaokrouhlení!
r.rel.cetnosti 

# Kontrola případné zaokrouhlovací chyby
r.rel.cetnosti[,"celkem"] = rowSums(r.rel.cetnosti[,1:ncol(r.rel.cetnosti)-1])
r.rel.cetnosti # zaokrouhlovací chyba se neprojevila

tab2 = matrix(paste0(tab," (",sprintf("%.0f",r.rel.cetnosti)," %)"), # za des. tečkou v "%.0f" se uvádí počet des. míst
              nrow = nrow(tab),
              byrow = F)
tab2
rownames(tab2) = rownames(tab)
colnames(tab2) = colnames(tab)
tab2
tab2[,"celkem"] = tab[,"celkem"] # V sumárním sloupci není nutno uvádět "100 %"
tab2

# Po exportu nutno ošetřit oddělovač des. míst (v cz textu - des. čárka)
write.csv2(tab2,"tab_priklad3.csv")

#*b) ####
# Analyzovanou závislost vizualizujte a doplňte o vhodnou míru kontingence. Komentujte závěry, 
# k nimž jste na základě explorační analýzy došli.

# Připravíme si data ve formátu vhodném pro vizualizaci.
# *Příprava popisků grafů ####
tab2
lab = tab2[1:nrow(kont.tab),1:ncol(kont.tab)] # vypustíme údaje ze sumárního řádku a sloupce
lab1 = as.vector(lab) # změníme typ popisků na vector (po řádcích)
lab2 = as.vector(t(lab)) # změníme typ popisků na vector (po řádcích)
# Popisky pro grafy - nutno vždy ověřit, který z popisků je správně
lab1
lab2

# *Vstupy pro mozaikový graf:
kont.tab.sdf = data %>% select(stud_obor,bmi.kat)
kont.tab.sdf # popř. jsme mohli získat přes "long" formát a f-ci uncount() z kont.tab

# *Mozaikový graf ####
mos = 
  ggplot(data = kont.tab.sdf) +
  geom_mosaic(aes(x = product(bmi.kat, stud_obor), # na první místo ve funkci product() dáváme závislou proměnnou
                  fill = bmi.kat),
              offset = 0.02,
              colour = "black") +   
  labs(y="Body mass index", x="", title = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = gray.colors(5)[2:5])

mos

# Doplnění popisků
mos +
  geom_text(data = ggplot_build(mos)$data[[1]], 
            aes(label=lab2, x = (xmin+xmax)/2, y = (ymin+ymax)/2),
            colour = "black")
# Nezapomeňte na kontrolu popisků!

ggsave("mos_priklad3.svg",
       width = 13,height = 6,
       units = "cm")

# Výpočet Cramerova V
cramersV(kont.tab)

#*c) ####
# Ověřte, zda mezi studijním oborem a hodnotou BMI (méně než 25 / 25 a více) existuje 
# statisticky významná souvislost. Výsledek interpretujte. (Nezapomeňte na ověření předpokladů příslušného testu.)
pom = chisq.test(kont.tab)
pom$expected # Všechny očekávané četnosti jsou větší než 5.
pom

#*d) ####
# Určete bodové a 95% intervalové odhady rizika zvýšeného BMI (25 a více) pro studenty 
# jednotlivých typů studia. Toto doplňte bodovým a 95% odhadem příslušného relativního rizika. 
# Výsledky interpretujte.
tab2

# humanitní vědy
p_hum = 14/57
p_hum
9/p_hum/(1-p_hum) # n (57) > 48,6
prop.test(14,57,alternative = "two.sided",conf.level = 0.95)

# sportovní vědy
p_sport = 9/51
p_sport
9/p_sport/(1-p_sport) # n (51) < 61,9!!! POZOR! Pro intervalový odhad této pravděpodobnosti máme málo dat!!!
prop.test(9,51,alternative = "two.sided",conf.level = 0.95) # nutno upozornit na nesplnění předpokladů

# bodový a 95% intervalový odhad relativního rizika
p_hum/p_sport
epi.2by2(kont.tab,conf.level = 0.95)

#*e) ####
# Určete bodové odhady šance na zvýšené BMI (25 a více) pro studenty 
# jednotlivých typů studia. Toto doplňte bodovým a 95% odhadem příslušného poměru šancí. 
# Výsledky interpretujte.
tab2

# humanitní vědy
odds_hum = 14/43
odds_hum

# sportovní vědy
odds_sport = 9/42
odds_sport

# bodový a 95% intervalový odhad poměru šancí
odds_hum/odds_sport
epi.2by2(kont.tab,conf.level = 0.95)

# Příklad 4. ####
# Společnost ATC tiskne a prodává sběratelské baseballové karty. V nabídce má tzv. Start balíček (100 ks karet), 
# který je tvořen náhodným výběrem karet z jejich produkce. Společnost tvrdí, že 30 % karet z jejich produkce 
# jsou nováčci, 60 % veteráni, ale ne hvězdní hráči (All-Stars), a 10 % hvězdní hráči. Start balíček, který jste 
# obdrželi obsahuje 50 karet nováčků, 45 veteránů a 5 hvězdných hráčů. Odpovídá tento balíček avizované 
# struktuře produkce firmy ACT? Ověřte na hladině významnosti 0,1. 

#H0: Zaslaný balíček je náhodným výběrem z produkce společnosti ACT.
#Ha: Zaslaný balíček není náhodným výběrem z produkce společnosti ACT.

x = c("nováčci","veteráni","all-stars")
n.obs = c(50,45,5) # pozorované (angl. observed) četnosti
names(n.obs) = x
n.obs # pozorované četnosti
p.obs = n.obs/sum(n.obs)
p.obs # pozorované rel. četnosti

p.exp = c(0.3,0.6,0.1)
names(p.exp) = x
p.exp # očekávané (angl. expected) pravděpodobnosti
n.exp = p.exp*sum(n.obs)
n.exp # očekávané četnosti - dle předpokladů chí-kvadrát testu dobré shody musí být všechny alespoň 1
      # a alespoň 80 % z nich větších než 5

# Chí-kvadrát test dobré shody
x.obs = sum(((n.obs-n.exp)^2)/n.exp)
x.obs # pozorovaná hodnota
p.hodnota = 1-pchisq(x.obs,length(n.exp)-1) # počet stupňů volnosti = počet variant (k) - 1
p.hodnota

# Na hladině významnosti 0,1 zamítáme HO (Chí-kvadrát test nezávislosti, X2 = 19,6, df = 2,
# p-hodnota < 0,001). Tj. dodaný balíček nelze (na hladině významnosti 0,1) považovat za náhodný
# výběr z produkce společnosti ACT.

# Příklad 5. (úplně specifikovaný test) ####
# V kasinu mají hru, jejíž výsledek závisí na počtu šestek, které padnou při hodu třemi klasickými kostkami. 
# Hráč po 100 hodech třemi kostkami zaznamenal následující výsledky:
x = 0:3
n.obs = c(54,35,10,1)
names(n.obs) = x
n.obs # pozorované četnosti počtu padlých šestek při hodu 3 kostkami

# Následně hráč pojal podezření, že kostky jsou cinknuté. Ověřte 
# hráčovo podezření vhodným čistým testem významnosti.

# X ... počet padlých šestek při hodu 3 kostkami
# X~Bi(n = 3, π = 1/6)

#H0: Kostky jsou férové. (Tj. počet padlých šestek při hodu třemi kostkami má bin. rozdělení s parametry n = 3, π = 1/6.)
#Ha: Kostky jsou cinknuté. (Tj. počet padlých šestek při hodu třemi kostkami nemá bin. rozdělení s parametry n = 3, π = 1/6.)

p.exp = dbinom(x,3,1/6)
p.exp
# zaokrouhlíme na 3 des. místa (desetiny procenta) a zkontrolujeme případnou zaokrouhlovací chybu
p.exp = round(p.exp,3)
p.exp # očekávané pravděpodobnosti
sum(p.exp) # zaokrouhlovací chyby jsme se nedopustili

n.exp = p.exp*sum(n.obs)
n.exp # očekávané četnosti - POZOR! 1 oč. četnost je menší než 1, tj.musíme data zjednodušit.

# Zjednodušení dat (dle ověření předpokladů)
x = c(0,1,"2 a více")
n.obs = c(54,35,11)
names(n.obs) = x
n.obs

p.exp[3] = sum(p.exp[3:4])
p.exp = p.exp[1:3]
p.exp # očekávané pravděpodobnosti

n.exp = p.exp*sum(n.obs)
n.exp # očekávané četnosti

# Chí-kvadrát test dobré shody
x.obs = sum(((n.obs-n.exp)^2)/n.exp)
x.obs # pozorovaná hodnota
p.hodnota = 1-pchisq(x.obs,length(n.exp)-1) # počet stupňů volnosti = počet variant (k) - 1
p.hodnota

# Na hladině významnosti 0,05 nelze zamítnout HO (Chí-kvadrát test nezávislosti, X2 = 2,0, df = 2,
# p-hodnota = 0,365). Tj. hrací kostky nelze (na dané hladině významnosti) označit za cinknuté.

# Příklad 6. (neúplně specifikovaný test) ####
# Předpokládejme, že chceme na základě aktuálních údajů (za rok 2020) najít vhodný model incidence jisté 
# vrozené nemoci detekovatelné ihned po narození. V příslušných databázích je dohledatelná incidence této 
# nemoci v jednotlivých státech (počet výskytů na 100 000 novorozenců). Výpisem záznamů z databází jsme 
# zjistili následující:
x = c(0,1,2,3,4,5,6,7)
n.obs = c(15,30,26,18,8,0,2,1)
names(n.obs) = x
n.obs # pozorované incidence (četnosti) daných výskytů nemoci na 100 000 novorozenců

# Lze incidenci dané nemoci na úrovní států modelovat Poissonovým rozdělením? 
# Ověřte na hladině významnosti 0,05

# H0: Incidenci (na 100 000 obyvatel) dané nemoci na úrovní států lze modelovat Poissonovým rozdělením.
# Ha: Incidenci (na 100 000 obyvatel) dané nemoci na úrovní států nelze modelovat Poissonovým rozdělením.

# Pro výpočet očekávaných p-stí musíme znát parametr Poissonova rozdělení. Protože není v zadání specifikován,
# musíme jej odhadnout z dat.

# X ... incidence dané nemoci na 100 000 obyvatel
# λt ... E(X), tj. průměrná incidence
lambda.t = weighted.mean(x,n.obs)
lambda.t # museli jsme odhadovat 1 parametr, tj. v testu dobré shody musíme odečíst 1 stupeň volnosti (od k-1)

p.exp = dpois(x,lambda.t)
p.exp
p.exp = round(p.exp,3)   # zaokrouhlení na 3 des.místa (desetiny procent)
p.exp # očekávané pravděpodobnosti
sum(p.exp) # nejde jen o zaokrouhlovací chybu, ale dle Poissonova modelu může být teoretická incidence až nekonečná
# tj. správně bychom nejvyšší variantu 7 měli považovat za 7 a více a její očekávanou p-st řešit dopočtem do 1

x.exp = c(0,1,2,3,4,5,6,"7 a více")
x.exp
names(p.exp) = x.exp
p.exp
p.exp["7 a více"] = 1 - sum(p.exp[1:6])
p.exp # očekávané pravdpěpodobnosti

n.exp = p.exp*sum(n.obs)
n.exp # očekávané četnosti
# 1 očekávaná četnost je menší než 1, 3 z 8 (37,5 %) oč. četností je menších než 5!!!

# Data musíme "zjednodušit - sloučit"
x = c(0,1,2,3,4,"5 a více")
p.exp[6] = 1-sum(p.exp[1:5])
p.exp = p.exp[1:6]
names(p.exp) = x
p.exp
sum(p.exp)

n.obs = c(15,30,26,18,8,3)
n.exp = p.exp*sum(n.obs)
n.exp = n.exp[1:6]
n.exp # 1 z 6 (16,6 %) oč. četností < 5 --> OK

# Chí-kvadrát test dobré shody
x.obs = sum(((n.obs-n.exp)^2)/n.exp)
x.obs # pozorovaná hodnota
p.hodnota = 1-pchisq(x.obs,length(n.exp)-1-1) # počet stupňů volnosti = počet variant (k) - 1 - 1 (počet odhadovaných parametrů)
p.hodnota

# Na hladině významnosti 0,05 nelze zamítnout HO (Chí-kvadrát test nezávislosti, X2 = 0,5, df = 4,
# p-hodnota = 0,971). Tj. incidenci (na 100 000 obyvatel) dané nemoci na úrovní států 
# lze modelovat Poissonovým rozdělením
