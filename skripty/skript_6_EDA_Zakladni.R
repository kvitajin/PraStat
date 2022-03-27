#//////////////////////////////////////////////////////////////////////////////////////
##############    Explorační analýza dat - Základní skript   ##########################
################       Adéla Vrtková, Martina Litschmannová    ########################
#//////////////////////////////////////////////////////////////////////////////////////

# Zobrazuje-li se vám skript s chybným kódováním, využijte příkaz File / Reopen with Encoding

# Základní skript obsahuje nezbytné minimum potřebné ke zvládnutí principů práce
# s jazykem R v kontextu Explorační analýzy dat.

# Rozšířený skript je obohacen o ukázky použití knihovny dplyr pro jednodušší práci
# s datovým souborem a knihovny ggplot2 pro hezčí grafiku.

# Lehkou nádstavbu k Základnímu skriptu tvoří skript "Grafika s ggplot2",
# kde jsou připraveny ukázky grafiky s tímto balíčkem. Ač to tak na první pohled nepůsobí,
# ggplot2 ve výsledku dokáže ušetřit spousty práce a času při vytváření pěkných grafů
# pro domácí úkoly, závěrečné práce, diplomky apod.

#//////////////////////////////////////////////////////////////////////////////////////
## 1. Příprava prostředí (knihovny, pracovní adresář) #################################

# Instalace knihoven (lze také provést přes okno Packages -> Install)
# Provádíme pouze jednou na daném počítači
install.packages("readxl")
install.packages("moments")

# Aktivace knihovny (nutno opakovat při každém novém spuštění Rka, vhodné mít na začátku skriptu)
library(readxl)
library(moments)

# Výpis pracovního adresáře (také je vidět v záhlaví Console)
getwd()

# Nastavení pracovního adresáře dle potřeby (lze také přes Session -> Set Working Directory -> Choose Directory)
# POZOR! Cesta musí obsahovat lomítka "/", nikoliv obrácené lomítko (backslash). 
setwd("C:/Users/Superman/SuperStatistika")

#//////////////////////////////////////////////////////////////////////////////////////
## 2. Import datového souboru #################################

# Základní funkce - read.table, read.csv, read.csv2, ...
# Záleží hlavně na formátu souboru (.txt, .csv), na tzv. oddělovači jednotlivých hodnot, desetinné čárce/tečce...

# Načtení a uložení datového souboru ve formátu csv2 z pracovního adresáře
data = read.csv2(file = "aku_st.csv")

# Načtení a uložení datového souboru ve formátu csv2 z lokálního disku do datového rámce data
data = read.csv2(file = "C:/Users/Moje/aku_st.csv")

# Načtení a uložení datového souboru ve formátu csv2 z internetu do datového rámce data
data = read.csv2(file = "http://am-nas.vsb.cz/lit40/DATA/aku.csv")

# Načtení a uložení datového souboru ve formátu xlsx z pracovního adresáře do datového rámce data
# Používáme funkci z balíčku readxl, který jsme v úvodu rozbalili
# Samotný xlsx soubor nesmí být otevřený, jinak spuštění importu hodí error
data = read_excel("../data/aku.xlsx",
                  sheet = "Data",           # specifikace listu v xlsx souboru
                  skip = 3)                 # řádky, které je potřeba přeskočit

# Přejmenování sloupců - je-li nutné
colnames(data) = c("A5","B5","C5","D5","A100","B100","C100","D100") 

## Poznámka (kterou je dobré dočíst až do konce....)
# Vždy je možné importovat pomocí "Import Dataset" z okna Environment bez nutnosti psát kód
# V tom případě ale nesmí být v "cestě" k souboru žádné speciální znaky (háčky, čárky). Jinak se objeví error.
# Objekt importovaný touto cestou bude v novém RStudiu jako typ "tibble".
# Jedná se o modernější "data.frame" a v některých funkcích může dělat problémy a házet errory!
# Jednoduše lze tento objekt převést na typ data.frame pomocí as.data.frame().

# Objekt typu "tibble" vznikne i importem dat pomocí funkce read_excel()
class(data)

# Proto provedeme změnu datové struktury na datový rámec - data.frame
data = as.data.frame(data)
class(data)

#//////////////////////////////////////////////////////////////////////////////////////
## 3. Preprocessing dat aneb cesta ke standardnímu datovému formátu ###############
#* 3.1 Výběr proměnných #######

# Zobrazení 3. sloupce - několik způsobů
data[ , 3]
# nebo (víme-li, jak se jmenuje proměnná zapsána ve 3. sloupci)
data[["C5"]]
# nebo
data$C5 # nejpoužívanější

# Uložení prvního a pátého sloupce dat. rámce data do dat. rámce pokus
pokus = data[ , c(1, 5)]

# Vyloučení prvního a pátého sloupce z dat. rámce data a uložení do dat. rámce pokus
pokus = data[ ,-c(1, 5)]

#/////////////////////////////////////////////////////////////////////////////////////
#* 3.2 Uložení menších logických celků #######

# Často se hodí provést vytvoření několika menších logických celků (např. uložit si zvlášť měření po 5 cyklech).
# Tyto menší logické celky pak můžou pomoct právě při vytváření std. datového formátu.
# Pozn. při ukládání dat mysleme na přehlednost v názvech!

data5 = data[,1:4] # z dat vybereme ty sloupce, které odpovídají měřením po 5 cyklech
colnames(data5) = c("A","B","C","D") # přejmenujeme sloupce

# Totéž provedeme pro měření provedené po 100 cyklech
data100 = data[,5:8] # z dat vybereme ty sloupce, které odpovídají měřením po 100 cyklech
colnames(data100) = c("A","B","C","D") # přejmenujeme sloupce

# Výše vytvořené soubory neodpovídají std. datovému formátu.
# O převedení se jednoduše postará funkce stack().

data5S = stack(data5)
colnames(data5S) = c("kap5", "vyrobce")

data100S = stack(data100)
colnames(data100S) = c("kap100", "vyrobce")

# Možná se ptáte, proč nepoužít funkci stack() na původní importovaná data (data).
# Funkce stack() funguje totiž tak, že všechny sloupce seřadí do jednoho sloupce
# a vytvoří k nim nový sloupec jako identifikátor toho, odkud původně (z jakého sloupce)
# pocházely. To se nám na původní data tedy nehodí.

# Z toho důvodu jsme si zcela záměrně cestu ke std. datovému formátu takto "rozkouskovali".
# V další části už připravené menší logické celky sloučíme.

#/////////////////////////////////////////////////////////////////////////////////////
#* 3.3 Sestavení std. datového formátu #######

# Sloučením dostaneme std. datový formát se všemi údaji (jupí!)
dataS = cbind(data5S, data100S)

# Provedeme ještě kosmetické úpravy (odstranění nadbytečného sloupce a vyřešení NA)
dataS = dataS[,-2] # vynecháme nadbytečný druhý sloupec
dataS = na.omit(dataS) # vynecháme řádky s NA hodnotami
# !!! S funkci na.omit zacházejte extrémně opatrně, abyste nechtěně nepřišli o data !!!

# Přímočará cesta vede skrze funkci reshape(), která je ale obtížnější na pochopení a použití.
# Její použití je demonstrováno v Rozšířeném skriptu ve stejně očíslované podkapitole.

# Na rovinu je potřeba přiznat, že tento preprocessing bychom zvládli i ručně v Excelu.
# To znamená není vůbec špatnou cestou si potřebné úpravy do std. datového formátu provést přímo v něm.

#/////////////////////////////////////////////////////////////////////////////////////
#* 3.4 Definování nové proměnné ve std. datovém formátu #######

# Definování nové proměnné pokles
dataS$pokles = dataS$kap5 - dataS$kap100

#/////////////////////////////////////////////////////////////////////////////////////
#* 3.5 Výběr ze std. datového formátu na základě dané podmínky #######

# Prozkoumejte následující příkazy a pochopte jejich strukturu
dataS[dataS$vyrobce == "A", ]
dataS$kap5[dataS$vyrobce == "A"]
dataS[dataS$vyrobce %in% c("A", "B"), c("kap100", "vyrobce")]

# Výše uvedené je vhodné, pokud si potřebujeme vytvořit zcela samostatné proměnné
# Např. proměnnou a5, která obsahuje kapacity po 5 cyklech akumulátorů od výrobce A
a5 = dataS$kap5[dataS$vyrobce=="A"]

# Analogicky lze vytvořit další různé potřebné "podvýběry" a uložit je.

# Podobné úpravy, které byly demonstrovány v této kapitole lze provádět také pomocí funkcí knihovny dplyr,
# která je pro "ne-programátory" možná intuitivnější na použití. Zájemce odkazujeme na Rozšířený skript.

#/////////////////////////////////////////////////////////////////////////////////////
## 4. Grafika v R - obecně ###########################################################
# Základem jsou tzv. high-level funkce, které vytvoří graf (tj. otevřou grafické oknou a vykreslí 
# dle zadaných parametrů).
# Na ně navazují tzv. low-level funkce, které něco do aktivního grafického okna přidají, 
# samy o sobě neotevřou nové, př. low-level funkcí - např. abline, points, lines, legend, 
# title, axis ... které přidají přímku, body, legendu...
# Tzn. před použitím "low-level" funkce je potřeba, volat "high-level" funkci (např. plot, boxplot, hist, barplot,...)

# Další grafické parametry naleznete v nápovědě
# nebo např. zde http://www.statmethods.net/advgraphs/parameters.html
# nebo zde https://flowingdata.com/2015/03/17/r-cheat-sheet-for-graphical-parameters/
# nebo http://bcb.dfci.harvard.edu/~aedin/courses/BiocDec2011/2.Plotting.pdf

## Barvy v R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

# Ukládání grafů lze např. pomocí funkce dev.print, jpeg, pdf a dalších.
# Jednodušeji pak v okně Plots -> Export

#/////////////////////////////////////////////////////////////////////////////////////
## 5. Explorační analýza a vizualizace kategoriální proměnné ##########################

class(dataS$vyrobce) # kontrola datového typu

# Pro práci s kategoriální proměnnou je nutné uložení proměnné jako typ factor
dataS$vyrobce = as.factor(dataS$vyrobce)

# Prohlédněme si varianty/kategorie proměnné
levels(dataS$vyrobce) # v uvedeném pořadí a podobě se budou také automaticky řadit v tabulkách i grafech

# NESPOUŠTĚJTE! Pokud by bylo potřeba provést přejmenování/překódování kategorií, lze využít příkazu
# dataS$vyrobce = factor(dataS$vyrobce,
#                        levels = c("A", "B", "C", "D"),  # současné varianty
#                        labels = c("Acko", "Becko", "Cecko", "Decko")) # nové varianty

# NESPOUŠTĚJTE! Pokud by bylo potřeba provést změnu pořadí kategorií bez překódování, lze využít příkazu
# dataS$vyrobce = factor(dataS$vyrobce,
#                        levels = c("D", "C", "B", "A")) # současné varianty v novém pořadí

# NESPOUŠTĚJTE! Pokud by bylo potřeba provést změnu pořadí kategorií i překódování, lze využít příkazu
# dataS$vyrobce = factor(dataS$vyrobce,
#                        levels = c("D", "C", "B", "A"), # současné varianty v novém pořadí
#                        labels = c("Decko", "Cecko", "Becko", "Acko")) # nové varianty v novém pořadí

#/////////////////////////////////////////////////////////////////////////////////////
#* 5.1 Tabulka četností (základní R) ##########

# Tabulka absolutních četností kategoriální proměnné výrobce
cetnosti = table(dataS$vyrobce)
cetnosti  # výpis - objekt typu "table"

# V případě nominální kategoriální proměnné je vhodné varianty proměnné seřadit dle jejich četností.
# Buď lze využít předchozího kódu a ručně si nastavit pořadí kategorií, nebo lze toto seřazení "zautomatizovat".

names(sort(table(dataS$vyrobce), decreasing = T)) # Uvědomte si, co je výstupem této "složené" funkce. V případě potřeby prozkoumejte její dílčí části.

dataS$vyrobce = factor(dataS$vyrobce,
                       levels = names(sort(table(dataS$vyrobce), decreasing = TRUE))
                       )

cetnosti = table(dataS$vyrobce)
cetnosti # Nyní jsou výstupem četnosti variant analyzované proměnné v požadovaném pořadí.

# Tabulka relativních četností a její přepočet na procenta
rel.cetnosti = prop.table(cetnosti)*100
rel.cetnosti # výpis

# U relativních četností v procentech je potřeba pohlídat zaokrouhlení a s ním spojené riziko zaokrouhlovací chyby.
rel.cetnosti = round(rel.cetnosti, digits = 1) # zaokrouhlení na 1 desetinné místo
rel.cetnosti
# Vysvětlete, proč bylo zvoleno toto zaokrouhlení!

rel.cetnosti[4] = 100 - sum(rel.cetnosti[1:3]) # ohlídání zaokrouhlovací chyby
rel.cetnosti

# Spojení tabulek a příprava pro export
tabulka = cbind(cetnosti,rel.cetnosti)  # spojení tabulek
colnames(tabulka) = c("četnost", "rel.četnost (%)") # změna názvů sloupců
tabulka

# Uložení tabulky do csv souboru pro export do MS Excel
write.csv2(tabulka, file="../data/tabulka.csv")

#/////////////////////////////////////////////////////////////////////////////////////
#* 5.2 Sloupcový graf (základní R) ##########

# Základní (tzn. nevyžadující žádný balíček) sloupcový graf vychází z tabulky četností, kterou máme nachystanou

# Funkcí par() lze nastavit parametry grafického okna, kam se graf vykreslí
par(mfrow = c(1,1),   # jednoduché rozdělení grafického okna - 1 řádek, 1 sloupec
    mar = c(2,2,2,2), # okraje kolem každého z grafů v počtech řádků - - c(dole, vlevo, nahoře, vpravo)
    oma = c(2,2,2,2)) # vnější okraje v počtech řádků - c(dole, vlevo, nahoře, vpravo)

barplot(cetnosti) # high-level funkce pro vykreslení sloupcového grafu

# Co grafu chybí? Pamatujme, že MÉNĚ JE VÍCE!!!

# Nastavení dalších parametrů funkce - změna barev, přidání názvu
barplot(cetnosti,
        col = c("blue", "green", "yellow", "red"),
        main = "Zastoupení výrobců ve výběru",
        space = 0.6) # parametr space vytvoří mezeru mezi sloupci

# Úprava popisků osy x, úprava rozsahu osy y
barplot(cetnosti,
        col = "lightblue",
        border = FALSE,				                   # nevykresluje čáru kolem sloupečků
        main = "Struktura souboru dle výrobce",
        ylim = c(0, 100),                      # rozsah osy y
        names.arg = paste0("Výrobce \n",names(cetnosti))) # Funkce paste0 umožňuje sloučit textové řetězce a hodnoty proměnných, symbol "\n" tvoří nový řádek v textu

# Přidání absolutních a relativních četností k odpovídajícím sloupcům pomocí low-level funkce text()
bp = barplot(cetnosti,
             col = "lightblue",
             border = FALSE,				                   # nevykresluje čáru kolem sloupečků
             main = "Struktura souboru dle výrobce",
             ylim = c(0, 100),                      # rozsah osy y
             names.arg = paste0("Výrobce \n",names(cetnosti))) # Funkce paste0 umožňuje sloučit textové řetězce a hodnoty proměnných, symbol "\n" tvoří nový řádek v textu

text(bp,
     cetnosti,paste0(cetnosti," (",rel.cetnosti," %)"),
     pos = 3)	# parametr pos udává, kde bude text uveden vzhledem k dané pozici (1 = pod, 2 = vlevo, 3 = nad, 4 = vpravo) 	

# Pokud nám vadí desetinná tečka místo desetinné čárky ve výstupu, lze spustit příkaz, který ve VÝSTUPECH změní tečku na čárku.
# Stále ale píšeme v příkazech desetinné tečky, příkaz změní jen VÝSTUP.
options(OutDec= ",")

bp = barplot(cetnosti,
             col = "lightblue",
             border = FALSE,				                   # nevykresluje čáru kolem sloupečků
             main = "Struktura souboru dle výrobce",
             ylim = c(0, 100),                      # rozsah osy y
             names.arg = paste0("Výrobce \n",names(cetnosti))) # Funkce paste0 umožňuje sloučit textové řetězce a hodnoty proměnných, symbol "\n" tvoří nový řádek v textu

text(bp,
     cetnosti,paste0(cetnosti," (",rel.cetnosti," %)"),
     pos = 3)	# parametr pos udává, kde bude text uveden vzhledem k dané pozici (1 = pod, 2 = vlevo, 3 = nad, 4 = vpravo) 	

# Po vykreslení vrátíme zpět na desetinnou tečku do původního nastavení.
options(OutDec= ".")

# Zkuste využít předešlého kódu a vytvořit si sloupcový graf pro proměnnou Výrobce podle sebe.

# Doporučení - v grafech by se neměla zbytečně duplikovat informace, máme-li jednoduchý sloupcový graf
# s řádně popsanými sloupci, není důvod vkládat např. legendu.
# Pokud graf vložíme do dokumentu, kde jej opatříme titulkem (např. Obr. 1: .....), pak není důvod mít 
# v samotném grafu velký název, jelikož bude ta stejná informace v titulku.

#/////////////////////////////////////////////////////////////////////////////////////
## 6. Explorační analýza a vizualizace kvantitativní proměnné ##########################

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.1 Výpočet číselných charakteristik ##########

summary(dataS$kap5)

# Výpočet průměru jedné proměnné
mean(dataS$kap5)

# Pozor na chybějící hodnoty
mean(data$C5)
mean(data$C5,na.rm=TRUE)

# Výpočet mediánu jedné proměnné
quantile(dataS$kap5, probs=0.5)

# Určení rozsahu výběru (tj. počet statistických jednotek - počet akumulátorů)
length(dataS$kap5)

# Další charakteristiky -> var(), sd(), min(), max(),...(viz R-tahák)

# Pozor! Funkce pro výpočet šikmosti (skewness) a špičatosti (kurtosis) nejsou součástí základního R, najdete je v balíčku moments
# Normálnímu rozdělení odpovídá špičatost 3, resp. špičatost v intervalu (1,5)
# Pro standardizaci špičatosti je nutno od vypočtené hodnoty odečíst 3.
# Napíšete-li před název funkce název balíčku a "::", zajistíte tím, že bude použita funkce z daného balíčku
# Nutno ohlídat, když jsou v různých balíčcích definovány různé funkce pod stejným jménem
moments::skewness(dataS$kap5)
moments::kurtosis(dataS$kap5)-3

# Chceme-li spočítat danou charakteristiku pro proměnnou kapacita po 5 cyklech podle výrobců, můžeme použít funkci tapply
tapply(dataS$kap5, dataS$vyrobce, mean, na.rm = T)

# Efektní výpočet číselných charakteristik (ať už s ohledem, či bez ohledu na výrobce) je v základním R
# poměrně "krkolomné" vytvořit. Z toho důvodu si zde vypůjčime funkce z balíčku dplyr,
# který, pro zájmce o jeho hlubší pochopení, je podrobněji popsán v Rozšířeném skriptu.

install.packages("dplyr")
library(dplyr)

# Výpočet číselných charakteristik proměnné kap5 bez ohledu na výrobce
dataS %>% 
  summarise(rozsah = length(kap5),
            minimum = min(kap5, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(kap5, 0.25, na.rm=T),
            prumer = mean(kap5, na.rm=T),
            median = median(kap5, na.rm=T),
            Q3 = quantile(kap5, 0.75, na.rm=T),
            maximum = max(kap5, na.rm=T),
            rozptyl = var(kap5, na.rm=T),
            smerodatna_odchylka = sd(kap5,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(kap5, na.rm=T)),       # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(kap5, na.rm=T)-3))

# Nezapoměňte na správné zaokrouhlení (viz Manuál) !!!
# Použijeme funkci group_by a dostaneme charakteristiky pro kapacitu po 5 cyklech podle výrobců
# Vzhledem k neúplnému výpisu je vhodné si výstup uložit a prohlédnout si jej v novém okně
charakteristiky_dle_vyrobce = 
  dataS %>%
    group_by(vyrobce) %>% 
    summarise(rozsah = length(kap5),
              minimum = min(kap5, na.rm=T),     # preventivní na.rm=T
              Q1 = quantile(kap5, 0.25, na.rm=T),
              prumer = mean(kap5, na.rm=T),
              median = median(kap5, na.rm=T),
              Q3 = quantile(kap5, 0.75, na.rm=T),
              maximum = max(kap5, na.rm=T),
              rozptyl = var(kap5, na.rm=T),
              smerodatna_odchylka = sd(kap5,na.rm=T),
              variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
              sikmost = (moments::skewness(kap5, na.rm=T)),       # preventivní specifikace balíčku moments
              spicatost = (moments::kurtosis(kap5, na.rm=T)-3))

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.2 Krabicový graf (základní R) ##########

par(mfrow = c(1,1),   # jednoduché rozdělení grafického okna - 1 řádek, 1 sloupec
    mar = c(4,4,3,2), # okraje kolem každého z grafů v počtech řádků - - c(dole, vlevo, nahoře, vpravo)
    oma = c(1,1,1,1)) # vnější okraje v počtech řádků - c(dole, vlevo, nahoře, vpravo)

# Jednoduché a rychlé vykreslení pomocí základní funkce boxplot()
boxplot(dataS$kap5)

# Další úprava grafu, využití funkce points pro zobrazení průměru
boxplot(dataS$kap5,
        main = "Kapacita po 5 cyklech (mAh)", 
        ylab = "kapacita (mAh)",
        col = "lightblue")
points(1, mean(dataS$kap5,na.rm=TRUE), pch=3) # do stávajícího grafu doplní bod znázorňující průměr

# Horizontální orientace, změna šířky krabice
boxplot(dataS$kap5,
        horizontal = TRUE,  # při horizontální orientaci je třeba si ohlídat opačné nastavení popisků
        xlab = "kapacita (mAh)",
        boxwex = 0.5)      # změní šířku krabice na 1/2

# A ještě vykreslení vícenásobného krabicového grafu (který by šel dále graficky upravit)
boxplot(dataS$kap5~dataS$vyrobce)

# Pro vykreslení vícenásobného krabicového grafu je na místě zvážit seřazení výrobců dle abecedy.

# Využijte předešlého kódu a vytvořte si krabicový graf podle sebe.

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.3 Histogram (základní R) ##########

# Jednoduché a rychlé vykreslení
hist(dataS$kap5)
hist(dataS$kap5, breaks=10) # Co dělají různé hodnoty parametru breaks s grafem?

# Již tradičně lze nastavit popisky, barvy a další parametry
hist(dataS$kap5, 
     main = "Histogram pro kapacitu akumulátorů po 5 cyklech", 
     xlab = "kapacita (mAh)",
     ylab = "četnost",
     ylim = c(0, 85),
     col = "lightblue",       # barva výplně
     border = "black",       # barva ohraničení sloupců
     labels = TRUE)         # přidá absolutní četnosti daných kategorií ve formě popisků

# Změna měřítka osy y, kvůli vykreslení odhadu hustoty pravděpodobnosti
hist(dataS$kap5, 
     main = "Histogram pro kapacitu akumulátorů po 5 cyklech", 
     xlab = "kapacita (mAh)",
     ylab = "f(x)",
     ylim = c(0, 0.006),
     col = "cadetblue1", 
     border = "grey",
     freq = FALSE)	       # změna měřítka na ose y --> f(x)
lines(density(dataS$kap5))        # připojí graf odhadu hustoty pravděpodobnosti

# Generování hustoty normálního rozdělení a přidání k histogramu
xfit = seq(min(dataS$kap5), max(dataS$kap5), length = 40)     # generování hodnot pro osu x
yfit = dnorm(xfit, mean = mean(dataS$kap5), sd = sd(dataS$kap5))  # generování hodnot pro osu y
lines(xfit, yfit, col = "black", lwd = 2)    # do posledního grafu přidání křivky na základě výše vygenerovaných hodnot
# Takto kombinovaný graf může posloužit k vizuálnímu posouzení normality.
# Kopíruje obálka histogramu vygenerovanou Gaussovu křivku?

# Využijte předešlého kódu a vytvořte si histogram podle sebe.

# Pro vykreslení histogramu pouze pro určitého výrobce lze využít drobné úpravy
hist(dataS$kap5[dataS$vyrobce == "A"], 
     main = "Histogram pro kapacitu akumulátorů po 5 cyklech, výrobce A", 
     xlab = "kapacita (mAh)",
     ylab = "četnost",
     xlim = c(1850, 2100),
     ylim = c(0, 30),
     col = "lightblue",       # barva výplně
     border = "black",       # barva ohraničení sloupců
     labels = TRUE)         # přidá absolutní četnosti daných kategorií ve formě popisků

# Takto bychom mohli vykreslit histogramy pro ostatní výrobce.
# POZOR! Mají-li být histogramy mezi sebou srovnatelné, musí mít sjednocené osy!!!

# Jednoduché pohlídání sjednocených os umí knihovna ggplot2 (viz Grafika s ggplot2 nebo Rozšířený skript).
# Ve chvíli, kdy je cílem vytvořit sadu histogramů, tak je opravdu na místě zvážit využití ggplot2.

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.4 QQ-graf (základní R) ##########

# Jednoduché a velmi rychlé vykreslení, např. pro kapacitu po 5 cyklech pro výrobce A
qqnorm(dataS$kap5[dataS$vyrobce == "A"])
qqline(dataS$kap5[dataS$vyrobce == "A"])

# ... s úpravou popisků os...
qqnorm(dataS$kap5[dataS$vyrobce == "A"], 
       xlab = "Teoretické normované kvantily",
       ylab = "Výběrové kvantily",
       main = "QQ-graf, kapacita po 5 cyklech, výrobce A")
qqline(dataS$kap5[dataS$vyrobce == "A"])

#/////////////////////////////////////////////////////////////////////////////////////
#* 6.5 Kombinace grafických výstupů do jednoho obrázku (základní R) ##########

# Využíváme-li základní funkce (barplot, boxplot, histogram), pak se využívá funkce par() nebo layout()
# V těchto funkcích specifikujeme strukturu - jak chceme více obrázků vykreslit

# Např. chceme vykreslit histogram i boxplot pro kapacitu po 5 cyklech akumulátorů od výrobce A
pom = layout(mat = matrix(1:2, nrow = 2, ncol = 1, byrow=FALSE), 
             height = c(2.5, 1)) # vytvoření struktury
layout.show(pom)  # kontrola struktury

par(oma = c(1,1,1,1),
    mar = c(4,2,1,2)) # nastavení velikosti okrajů

hist(dataS$kap5,
     xlab=" ",
     main = " ",
     ylab = "četnost", 
     ylim = c(0,100), 
     xlim = c(1650,2100))
boxplot(dataS$kap5, 
        horizontal = TRUE, 
        ylim = c(1650,2100), 
        boxwex = 1.5,
        xlab="kapacita (mAh) po 5 cyklech")

# Pokud je však cílem SROVNÁNÍ, pak není vhodné výše uvedené generovat pro každého výrobce zvlášť.
# V takovém případě je dobré sáhnout po jednom vícenásobném krabicovém grafu 
# a k němu vytvořit sadu histogramů a sadu QQ-grafů.

# Sadu histogramů lze vytvořit například takto:
pom = layout(mat = matrix(1:4, nrow = 4, ncol = 1)) # vytvoření struktury
par(oma = c(1,1,1,1),
    mar = c(4,2,1,2)) # nastavení velikosti okrajů

for (i in c("A", "B", "C", "D")){
  hist(dataS$kap5[dataS$vyrobce == i], 
       main = paste("Výrobce", i), 
       xlab = ifelse(i == "D", "kapacita (mAh) po 5 cyklech", ""), 
       ylab = "četnost", 
       xlim = c(min(dataS$kap5, na.rm=TRUE), max(dataS$kap5, na.rm=TRUE)), # sjednocená osa x
       ylim = c(0,35), # sjednocená osa y
       cex.axis = 1.4, # zvětšení popisků os
       cex.lab = 1.4) # zvětšení názvů os
}
# Všimněte si sjednocených os!!! Další úpravy jsou na vás.

# Sadu histogramů s přeškálovanou osou y na hustotu pravděpodobnosti lze vytvořit například takto:
pom = layout(mat = matrix(1:4, nrow = 4, ncol = 1)) # vytvoření struktury
par(oma = c(1,1,1,1),
    mar = c(4,2,1,2)) # nastavení velikosti okrajů

for (i in c("A", "B", "C", "D")){
  hist(dataS$kap5[dataS$vyrobce == i], 
       main = paste("Výrobce",i), 
       xlab = ifelse(i == "D", "kapacita (mAh) po 5 cyklech", ""), 
       ylab = "f(x)", 
       xlim = c(min(dataS$kap5, na.rm=TRUE), max(dataS$kap5 ,na.rm=TRUE)), 
       ylim = c(0, 0.037),
       freq = FALSE,
       cex.main = 1.4,
       cex.axis = 1.4, # zvětšení popisků os
       cex.lab = 1.4) # zvětšení názvů os
  
  lines(density(dataS$kap5[dataS$vyrobce == i], na.rm=TRUE))
  
  xfit=seq(min(dataS$kap5[dataS$vyrobce == i], na.rm=TRUE), 
           max(dataS$kap5[dataS$vyrobce == i], na.rm=TRUE), 
           length=40) 
  yfit=dnorm(xfit, 
             mean = mean(dataS$kap5[dataS$vyrobce == i], na.rm=TRUE), 
             sd = sd(dataS$kap5[dataS$vyrobce == i], na.rm=TRUE)) 
  lines(xfit, yfit, col = "blue", lty = 2)
}

# Sadu QQ-grafů lze vytvořit třeba takto:
pom = layout(mat = matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(oma = c(1,1,1,1), 
    mar = c(5,4,3,2))

for (i in c("A", "B", "C", "D")){
  qqnorm(dataS$kap5[dataS$vyrobce == i],
         main = paste0("Výrobce ", i),
         xlab = "Teoretické normované kvantily",
         ylab = "Výběrové kvantily",
         cex.axis = 1.3, # zvětšení popisků os
         cex.lab = 1.3) # zvětšení názvů os)
  qqline(dataS$kap5[dataS$vyrobce == i])
}

# MOTIVAČNÍ OKÉNKO ###
# Zkuste si, jak málo stačí ke "štěstí", tj. k sadé histogramů a sadě QQ-grafů, odvážíte-li se
# proniknout do ggplotu:
library(ggplot2)
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram()+                  
  facet_grid("vyrobce")

ggplot(dataS, 
       aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap("vyrobce", 
             scales = "free")

# Graf by zasloužil ještě trochu upravit, ale vidíte - žádné for-cykly, žádné layouty...

#/////////////////////////////////////////////////////////////////////////////////////
## 7. Identifikace odlehlých pozorování a jejich odstranění #######

#/////////////////////////////////////////////////////////////////////////////////////
#* 7.1 Individuální posouzení #######
# Zcela individuální posouzení - jednoduše si data seřadíme a podíváme se na "chvosty"
# Při větším počtu dat velmi nepraktické, ale při menším rozsahu se může hodit.

# Pomocí funkce order()
dataS[order(dataS$kap5[dataS$vyrobce == "A"]),]

#/////////////////////////////////////////////////////////////////////////////////////
#* 7.2 Využití krabicového grafu a ručního odstranění #######

# Ruční odstranění - využití funkce boxplot s parametrem plot = F - s ohledem na výrobce
pom = boxplot(dataS$kap5~dataS$vyrobce, plot = F)
pom   # v pom$out jsou uložena odlehlá pozorování detekována metodou vnitřních hradeb,
# v pom$group je info, ze které skupiny odlehlá pozorování jsou

# Tyto konkrétní hodnoty jsou pak označeny v novém sloupci kap5_out jako NA
# POZOR! Nikdy si nepřepisujeme původní proměnné!!!
dataS$kap5_out = dataS$kap5
dataS$kap5_out[dataS$vyrobce == "A" & dataS$kap5 >= 2023] = NA
dataS$kap5_out[dataS$vyrobce == "C" & dataS$kap5 == 1848.4] = NA
dataS$kap5_out[dataS$vyrobce == "D" & dataS$kap5 == 1650.3] = NA

#/////////////////////////////////////////////////////////////////////////////////////
#* 7.3 Využití metody vnitřních hradeb #######

# Použití vnitřních hradeb - obecnější postup - uvedeno bez ohledu na výrobce!!!
IQR = quantile(dataS$kap5, 0.75, na.rm=T) - quantile(dataS$kap5, 0.25, na.rm=T)  # mezikvartilové rozpěti
# nebo
IQR = IQR(dataS$kap5)

dolni_mez = quantile(dataS$kap5, 0.25, na.rm=T) - 1.5*IQR  # výpočet dolní mezi vnitřních hradeb
horni_mez = quantile(dataS$kap5, 0.75, na.rm=T) + 1.5*IQR  # výpočet horní mezi vnitřních hradeb

# Definování nového sloupce, ve kterém budou odlehlá pozorování odstraněna
dataS$kap5_out = dataS$kap5
dataS$kap5_out[dataS$kap5 >= horni_mez | dataS$kap5 <= dolni_mez] = NA

# Analytik může vždy říct, že odlehlá pozorování odstraňovat nebude, 
# ale tuto informaci musí do zápisu o analýze uvést a patřičně zdůvodnit!!!

