#######################################################################################
########################### Vícevýbìrové testy ########################################
######################### Martina Litschmannová, Adéla Vrtková ########################
#######################################################################################
library(readxl)
library(tidyr)
library(rstatix)
library(ggplot2)
library(moments)
library(dplyr)
library(FSA)
library(car) # Leveneho test
library(lawstat) # symmetry.test

################## Pøiklad 1. (Doplnìní tabulky ANOVA) ################################
#######################################################################################

n=c(40,40,42)          # rozsahy výbìrù
prum=c(300,290,310)    # prùmìry v jednotlivých skupinách / tøídách
s=c(33,34,31)          # smìrodatné odchylky v jednotlivých skupinách / tøídách
n.total=sum(n)         # celkový rozsah výbìrù
k=3                    # poèet tøíd
df.b=k-1               # poèet stupòù volnosti - meziskupinový
df.e=n.total-k         # poèet stupòù volnosti - reziduální

# celkový prùmìr
prum.total=weighted.mean(prum,n)
prum.total

# meziskupinový souèet ètvercù
ss.b=sum(n*(prum-prum.total)^2)
ss.b

# reziduální souèet ètvercù
ss.e=sum((n-1)*s^2)
ss.e

# celkový souèet ètvercù
ss.b+ss.e

# rozptyl mezi skupinami / tøídami
ms.b=ss.b/df.b
ms.b

# rozptyl uvnitø skupin / tøíd
ms.e=ss.e/df.e
ms.e

# F-pomìr
F=ms.b/ms.e
F

# p-hodnota
p=1-pf(F,df.b,df.e)
p

# Na hladinì významnosti 0,05 zamítáme hypotézu o shodì støedních hodnot (p-hodnota=0,024, ANOVA), 
# tj. støední hodnoty alespoò jedné dvojice skupin se statisticky významnì lií.

# odhady skupinových efektù
efekt=prum-prum.total
efekt

# Oproti celkovému prùmìru vykazuje nejvíce podprùmìrné výsledky skupina 2 
# (o cca 10 jednotek nií ne celkový prùmìr). Naopak prùmìr skupiny 3 je 
# o cca 10 jednotek vyí ne celkový prùmìr. Prùmìrné výsledky skupiny 1 
# odpovídají celkovému prùmìru.

################## Pøiklad 2. (Kyselina listová) ######################################
#######################################################################################
setwd("C:/Users/lit40/OneDrive - VSB-TUO/Vyuka/PASTA/DATA/aktualni")

kysel = read_excel("testy_vicevyberove.xlsx",
                     sheet=1)

colnames(kysel)=c("sk1","sk2","sk3")   # pøejmenování sloupcù

# pøevod do standardního datového formátu
kysel.s=pivot_longer(kysel,
                    cols = 1:3,
                    names_to = "skupina",
                    values_to = "hodnoty")
kysel.s=na.omit(kysel.s)

boxplot(hodnoty~skupina,data = kysel.s)
# nebo
boxplot(kysel)

# Data neobsahují odlehlá pozorování.

# Ovìøení normality
ggplot(kysel.s,aes(x = hodnoty)) +
  geom_histogram() +
  facet_wrap(~skupina,nrow = 3)

ggplot(kysel.s,aes(sample = hodnoty)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~skupina,scales = "free")

kysel.s %>% 
  group_by(skupina) %>% 
  summarise(
    ikmost = moments::skewness(hodnoty,na.rm = T),
    pièatost = moments::kurtosis(hodnoty,na.rm = T)-3,
    SW_test = shapiro.test(hodnoty)$p.value)

# Na hladinì významnosti 0,05 nezamítáme pøedpoklad normality.

# Informace potøebné pro nastavení zaokrouhlování
kysel.s %>% group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(hodnoty)),
            sm.odch = sd(hodnoty,na.rm = T))

# sd zaokrouhlujeme na 3 platné cifry
# sd a míry polohy zaokrouhlujeme na desetiny

# Ovìøení shody rozptylù
s2=kysel.s %>% 
  group_by(skupina) %>% 
  summarise(var = var(hodnoty))
s2 # výbìrové rozptyly

max(s2$var)/min(s2$var)
# Dle krabicového grafu a informace o pomìru nejvìtího a nejmeního rozptylù (<2) nepøedpokládáme,
# e se rozptyly statisticky významnì lií

# Pøedpoklad normality nebyl zamítnut -> Bartlettùv test
bartlett.test(hodnoty~skupina,data = kysel.s)
# nebo
bartlett.test(kysel) 

# Na hladinì významnosti 0,05 nelze zamítnout pøedpoklad o shodì rozptylù 
# (Bartlettùv test, x_OBS = 0,878, df = 2, p-hodnota = 0,645).

# Chceme srovnávat stø. hodnoty nezávislých výbìrù z normálních rozdìlení se stejnými rozptyly -> ANOVA
# pøíkaz aov() vyaduje data ve standardním datovém formátu

vysledky=aov(hodnoty~skupina,data = kysel.s) # POZOR! Nestaèí pouít pøíkaz aov(). Výstup pøíkazu musíme uloit do pomocné promìnné
                                             # a na tu aplikovat pøíkaz summary().
summary(vysledky)  

# Na hladinì významnosti 0,05 zamítáme hypotézu o shodì støedních hodnot (ANOVA, p-hodnota<<0,001) -> mnohonásobné porovnávání
TT = TukeyHSD(vysledky,"skupina",ordered = TRUE)
TT
### Konverze TukeyHSD na standardní data.frame
TUK = as.data.frame(TT$skupina)
names(TUK) = gsub(" ", ".", names(TUK))

HSD = data.frame(Comparison=row.names(TUK), 
                 diff=TUK$diff, lwr=TUK$lwr, lwr=TUK$lwr, p.adj=TUK$p.adj)
# písmenkové schéma
cldList(p.adj ~ Comparison, data = HSD,
        threshold = 0.05)

# prùmìry a skupinové efekty
kysel.s %>% 
  group_by(skupina) %>% 
  summarise(
    mean = mean(hodnoty,na.rm = T),
    nn = length(na.omit(hodnoty))) %>% 
  mutate(w.mean = weighted.mean(mean,nn),
         efect = mean-w.mean)
# Povaujeme-li vysoký obsah kyseliny listové za pozitivní, pak statisticky významnì nejlepích
# výsledkù dosáhli pacienti ze skupiny 1 (prùmìrný obsah kys. listové o cca 27 jednotek vyí ne
# prùmìrný obsah kys. listové v krvi vech testovaných pacientù) a statisticky významnì nejhorích výsledkù
# dosáhli pacienti ze skupiny 2 (prùmìrný obsah kys. listové o cca 27 jednotek nií ne
# prùmìrný obsah kys. listové v krvi vech testovaných pacientù). Obsah kys. listové v krvi pacientù ze skupiny 3
# odpovídá celkovému prùmìru. Vechny tøi skupiny pacientù jsou navzájem dle obsahu kys. listové
# v krvi statisticky významnì odliné.

# Dalí èást skriptu ji neobsahuje tak podrobné komentáøe =)

################## Pøiklad 3. (Králíci) ###############################################
#######################################################################################
kralici = read_excel("testy_vicevyberove.xlsx",
                   sheet=2)

colnames(kralici)=c("viden","cesky","kalif")   # pøejmenování sloupcù

# pøevod do standardního datového formátu
kralici.s=pivot_longer(kralici,
                       cols = 1:3,
                       names_to = "skupina",
                       values_to = "hodnoty")
kralici.s=na.omit(kralici.s)

boxplot(kralici)

# Odstranìní odlehlého pozorování
kralici.s$id = seq(1,length(kralici.s$skupina),1)
outliers = 
  kralici.s %>% 
  group_by(skupina) %>% 
  identify_outliers(hodnoty)
kralici.s$hodnoty.bez = ifelse(kralici.s$id %in% outliers$id,NA,kralici.s$hodnoty)

# Krabicový graf
boxplot(hodnoty.bez~skupina,data = kralici.s)

# Ovìøení normality
ggplot(kralici.s,aes(x = hodnoty.bez)) +
  geom_histogram() +
  facet_wrap(~skupina,nrow = 3)

ggplot(kralici.s,aes(sample = hodnoty.bez)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~skupina,scales = "free")

kralici.s %>% 
  group_by(skupina) %>% 
  summarise(
    ikmost = moments::skewness(hodnoty.bez,na.rm = T),
    pièatost = moments::kurtosis(hodnoty.bez,na.rm = T)-3,
    SW_test = shapiro.test(hodnoty.bez)$p.value)

# Na hladinì významnosti 0,05 nezamítáme pøedpoklad normality.

# Informace potøebné pro nastavení zaokrouhlování
kralici.s %>% group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(hodnoty.bez)),
            sm.odch = sd(hodnoty.bez,na.rm = T))

# sd zaokrouhlujeme na 2 platné cifry
# sd a míry polohy zaokrouhlujeme na setiny

# Ovìøení shody rozptylù
s2=kralici.s %>% 
  group_by(skupina) %>% 
  summarise(var = var(hodnoty.bez,na.rm = T))
s2 # výbìrové rozptyly

max(s2$var)/min(s2$var)
# Dle krabicového grafu a informace o pomìru nejvìtího a nejmeního rozptylù (>2) pøedpokládáme,
# e se rozptyly statisticky významnì lií

# Pøedpoklad normality nebyl zamítnut -> Bartlettùv test
bartlett.test(hodnoty.bez~skupina,data = kralici.s)
# nebo
bartlett.test(kralici) 

# Na hladinì významnosti 0,05 nelze zamítnout pøedpoklad o shodì rozptylù 
# (Bartlettùv test, x_OBS = 3,1, df = 2, p-hodnota = 0,217).

# Chceme srovnávat stø. hodnoty nezávislých výbìrù z normálních rozdìlení se stejnými rozptyly -> ANOVA
# pøíkaz aov() vyaduje data ve standardním datovém formátu

vysledky=aov(hodnoty.bez~skupina,data = kralici.s)
summary(vysledky)  

# Na hladinì významnosti 0,05 zamítáme hypotézu o shodì støedních hodnot (p-hodnota<<0,001, ANOVA) -> mnohonásobné porovnávání
TT = TukeyHSD(vysledky,"skupina",ordered = TRUE)
TT
### Konverze TukeyHSD na standardní data.frame
TUK = as.data.frame(TT$skupina)
names(TUK) = gsub(" ", ".", names(TUK))

HSD = data.frame(Comparison=row.names(TUK), 
                 diff=TUK$diff, lwr=TUK$lwr, lwr=TUK$lwr, p.adj=TUK$p.adj)
# písmenkové schéma
cldList(p.adj ~ Comparison, data = HSD,
        threshold = 0.05)

# prùmìry a skupinové efekty
kralici.s %>% 
  group_by(skupina) %>% 
  summarise(
    mean = mean(hodnoty.bez,na.rm = T),
    nn = length(na.omit(hodnoty.bez))) %>% 
  mutate(w.mean = weighted.mean(mean,nn),
         efect = mean-w.mean)
  
################## Pøiklad 4. (Jakost) ################################################
#######################################################################################
jakost.s = read_excel("testy_vicevyberove.xlsx",
                   sheet=3)

colnames(jakost.s)=c("poradi","skupina")   # pøejmenování sloupcù

boxplot(poradi~skupina, data = jakost.s)

# Ovìøení normality nemá smysl provádìt - z povahy jde o diskrétní data (poøadí)

# Informace potøebné pro nastavení zaokrouhlování
jakost.s %>% group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(poradi)),
            sm.odch = sd(poradi,na.rm = T))

# Ovìøení shody rozptylù
s2=jakost.s %>% 
  group_by(skupina) %>% 
  summarise(var = var(poradi,na.rm = T))
s2 # výbìrové rozptyly

max(s2$var)/min(s2$var)
# Dle krabicového grafu a informace o pomìru nejvìtího a nejmeního rozptylù (<2)
# ze pøedpokládat shodu rozptylù. (Kruskalùv - Wallisùv test má vìtí sílu testu, jsou-li data homoskedasticitní.)

# Jde o "poøadová" data, nemá smysl uvaovat o pøedpokladu normality -> Leveneho test

leveneTest(poradi~skupina, data = jakost.s) 

jakost.s$skupina=as.factor(jakost.s$skupina)  # Starí verze pøíkazu leveneTest vyaduje, aby "skupina" byla kódována jako faktor,
                                              # novì je výpoèet proveden, ale objevuje se doporuèení k pøekódování promìnné "skupina".
leveneTest(poradi~skupina, data = jakost.s)
# Na hladinì významnosti 0,05 nelze zamítnout pøedpoklad o shodì rozptylù 
# (Leveneho test, x_OBS = 0,4, df_num = 3, df_denom = 62, p-hodnota = 0,750).

# Ovìøení symetrie
boxplot(poradi~skupina, data = jakost.s)

jakost.s %>% 
  group_by(skupina) %>% 
  summarise(
    ikmost = moments::skewness(poradi,na.rm = T))

ggplot(jakost.s,aes(x = poradi)) +
  geom_histogram() +
  facet_wrap(~skupina,nrow = 2)


jakost.s %>% 
  group_by(skupina) %>% 
  summarise(sym.test = symmetry.test(poradi,boot = F)$p.value)

# Na hladinì významnosti 0,05 nelze zamítnout pøedpoklad symetrie.

# Chceme srovnávat mediány nezávislých výbìrù (data nelze povaovat za výbìry z norm. rozdìlení) -> Kruskalùv-Wallisùv test
kruskal.test(poradi~skupina,data = jakost.s)

# Na hladinì významnosti 0,05 nelze zamítnout hypotézu o shodì mediánù (Kruskalùv-Wallisùv test, x_OBS = 3,7, df = 3, p-hodnota=0,295). 
# Tj. statisticky významné rozdíly mezi výrobci (z hlediska poøadí výrobkù v soutìi) neexistují.

################## Pøiklad 5. (Trombin) ###############################################
#######################################################################################
trombin.s = read_excel("testy_vicevyberove.xlsx",
                      sheet=4,
                      skip = 1)

colnames(trombin.s)=c("hodnoty","skupina")   # pøejmenování sloupcù

boxplot(hodnoty~skupina,data = trombin.s)

# Ovìøení normality
ggplot(trombin.s,aes(x = hodnoty)) +
  geom_histogram() +
  facet_wrap(~skupina,nrow = 3)

ggplot(trombin.s,aes(sample = hodnoty)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~skupina,scales = "free")

trombin.s %>% 
  group_by(skupina) %>% 
  summarise(
    ikmost = moments::skewness(hodnoty,na.rm = T),
    pièatost = moments::kurtosis(hodnoty,na.rm = T)-3,
    SW_test = shapiro.test(hodnoty)$p.value)

# Na hladinì významnosti 0,05 zamítáme pøedpoklad normality.

# Informace potøebné pro nastavení zaokrouhlování
trombin.s %>% group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(hodnoty)),
            sm.odch = sd(hodnoty,na.rm = T))

# sd zaokrouhlujeme na 2 platné cifry
# sd a míry polohy zaokrouhlujeme na desetiny

# Ovìøení shody rozptylù
s2=trombin.s %>% 
  group_by(skupina) %>% 
  summarise(var = var(hodnoty,na.rm = T))
s2 # výbìrové rozptyly

max(s2$var)/min(s2$var)
# Dle krabicového grafu a informace o pomìru nejvìtího a nejmeního rozptylù (>>2)
# nelze pøedpokládat shodu rozptylù. Rozhodnout nám pomùe test.

# Pøedpoklad normality byl zamítnut -> Leveneho test
leveneTest(hodnoty~skupina,data = trombin.s) 

trombin.s$skupina=as.factor(trombin.s$skupina)
leveneTest(hodnoty~skupina,data = trombin.s)
# Na hladinì významnosti 0,05 zamítáme pøedpoklad o shodì rozptylù 
# (Leveneho test, p-hodnota << 0,001).

# Ovìøení symetrie
boxplot(hodnoty~skupina, data = trombin.s)

trombin.s %>% 
  group_by(skupina) %>% 
  summarise(
    ikmost = moments::skewness(hodnoty,na.rm = T),
    sym.test = symmetry.test(hodnoty,boot = F)$p.value)

ggplot(jakost.s,aes(x = poradi)) +
  geom_histogram() +
  facet_wrap(~skupina,nrow = 2)


jakost.s %>% 
  group_by(skupina) %>% 
  summarise(sym.test = symmetry.test(poradi,boot = F)$p.value)

tapply(trombin.s$hodnoty,trombin.s$skupina,moments::skewness)

skupiny = c("A","B","C")
par(mfrow=c(1,1))
for (i in 1:3){
  hist(trombin.s$hodnoty[trombin.s$skupina == skupiny[i]],
       main = skupiny[i],
       xlim = c(0.9*min(trombin.s$hodnoty),1.1*max(trombin.s$hodnoty)),
       ylim = c(1,5),
       xlab = "trombinový èas (s)",
       ylab = "èetnost")
}

library(dplyr)
trombin.s %>% 
  group_by(skupina) %>% 
  summarise(sym.test = symmetry.test(hodnoty,boot = F)$p.value)

# Na hladinì významnosti 0,05 nelze zamítnout pøedpoklad symetrie.

# Chceme srovnávat mediány nezávislých výbìrù, která nemají normální rozdìlení a mají rùzné rozptyly -> Kruskalùv - Wallisùv test

kruskal.test(trombin.s$hodnoty,trombin.s$skupina)
# Na hladinì významnosti 0,05 zamítáme hypotézu o shodì mediánù (p-hodnota<<0,001, Kruskalùv-Wallisùv test). Tj. trombinový èas je statisticky významnì
# ovlivnìn preparátem. -> mnohonásobné porovnávání

DT = dunnTest(hodnoty ~ skupina,                 
              data=trombin.s,
              method="bonferroni")
DT
PT = DT$res

# písmenkové schéma
cldList(P.adj ~ Comparison, data = PT,
        threshold = 0.05)

# mediány a skupinové efekty
trombin.s %>% 
  mutate(median.total = median(hodnoty)) %>% 
  group_by(skupina) %>% 
  summarise(
    median = median(hodnoty,na.rm = T),
    median.tot = median(median.total)) %>% 
  mutate(efect = median-median.tot)

