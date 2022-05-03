#######################################################################################
########################### Vícevýbìrové testy ########################################
######################### Martina Litschmannová, Adéla Vrtková ########################
#######################################################################################

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
# tj. støední hodnoty alespoò jedné dvojice skupin se statisticky významnì liší.

# odhady skupinových efektù
efekt=prum-prum.total
efekt

# Oproti celkovému prùmìru vykazuje nejvíce podprùmìrné výsledky skupina 2 
# (o cca 10 jednotek nižší než celkový prùmìr). Naopak prùmìr skupiny 3 je 
# o cca 10 jednotek vyšší než celkový prùmìr. Prùmìrné výsledky skupiny 1 
# odpovídají celkovému prùmìru.

################## Pøiklad 2. (Kyselina listová) ######################################
#######################################################################################
setwd("C:/Users/Martina/OneDrive - VŠB-TU Ostrava/Výuka/Pravdìpodobnost a statistika/DATA/aktualni/")
library(readxl)
kysel = read_excel("testy_vicevyberove.xlsx",
                     sheet=1)

colnames(kysel)=c("sk1","sk2","sk3")   # pøejmenování sloupcù

# pøevod do standardního datového formátu
kysel.s=stack(kysel)
colnames(kysel.s)=c("hodnoty","skupina")
kysel.s=na.omit(kysel.s)

par(mfrow = c(1,1))
boxplot(hodnoty~skupina,data = kysel.s)
# nebo
boxplot(kysel)

# Data neobsahují odlehlá pozorování.

# Ovìøení normality
skupiny = c("sk1","sk2","sk3")

par(mfrow=c(1,3))
for (i in 1:3){
  qqnorm(kysel.s$hodnoty[kysel.s$skupina == skupiny[i]],
         main = skupiny[i],
         xlab = "norm. teoretické kvantily",
         ylab = "výbìrové kvantily")
  qqline(kysel.s$hodnoty[kysel.s$skupina == skupiny[i]])
}

library(moments)
tapply(kysel.s$hodnoty,kysel.s$skupina,skewness,na.rm=TRUE)
tapply(kysel.s$hodnoty,kysel.s$skupina,moments::kurtosis,na.rm=TRUE)-3

tapply(kysel.s$hodnoty,kysel.s$skupina,shapiro.test)

# Na hladinì významnosti 0,05 nezamítáme pøedpoklad normality.

# Informace potøebné pro nastavení zaokrouhlování
tapply(kysel.s$hodnoty,kysel.s$skupina,length)# sd zaokrouhlujeme na 2 platné cifry
tapply(kysel.s$hodnoty,kysel.s$skupina,sd)# sd a míry polohy zaokrouhlujeme na celá èísla

# Ovìøení shody rozptylù
s2=tapply(kysel.s$hodnoty,kysel.s$skupina,var,na.rm=TRUE)
s2 # výbìrové rozptyly

max(s2)/min(s2)
# Dle krabicového grafu a informace o pomìru nejvìtšího a nejmenšího rozptylù (<2) nepøedpokládáme,
# že se rozptyly statisticky významnì liší

# Pøedpoklad normality nebyl zamítnut -> Bartlettùv test
bartlett.test(hodnoty~skupina,data = kysel.s)
# nebo
bartlett.test(kysel) 

# Na hladinì významnosti 0,05 nelze zamítnout pøedpoklad o shodì rozptylù 
# (Bartlettùv test, x_OBS = 0,878, df = 2, p-hodnota = 0,645).

# Chceme srovnávat stø. hodnoty nezávislých výbìrù z normálních rozdìlení se stejnými rozptyly -> ANOVA
# pøíkaz aov() vyžaduje data ve standardním datovém formátu

vysledky=aov(hodnoty~skupina,data = kysel.s) # POZOR! Nestaèí použít pøíkaz aov(). Výstup pøíkazu musíme uložit do pomocné promìnné
                                             # a na tu aplikovat pøíkaz summary().
summary(vysledky)  

# Na hladinì významnosti 0,05 zamítáme hypotézu o shodì støedních hodnot (ANOVA, p-hodnota<<0,001) -> mnohonásobné porovnávání
TukeyHSD(vysledky)

# prùmìry
prum=tapply(kysel.s$hodnoty,kysel.s$skupina,mean,na.rm=TRUE)
prum
# skupinové efekty
n=tapply(kysel.s$hodnoty,kysel.s$skupina,length)
prum.total=weighted.mean(prum,n)
efekt=prum-prum.total
efekt
# Považujeme-li vysoký obsah kyseliny listové za pozitivní, pak statisticky významnì nejlepších
# výsledkù dosáhli pacienti ze skupiny 1 (prùmìrný obsah kys. listové o cca 27 jednotek vyšší než
# prùmìrný obsah kys. listové v krvi všech testovaných pacientù) a statisticky významnì nejhorších výsledkù
# dosáhli pacienti ze skupiny 2 (prùmìrný obsah kys. listové o cca 26 jednotek nižší než
# prùmìrný obsah kys. listové v krvi všech testovaných pacientù). Obsah kys. listové v krvi pacientù ze skupiny 3
# odpovídá celkovému prùmìru. Všechny tøi skupiny pacientù jsou navzájem dle obsahu kys. listové
# v krvi statisticky významnì odlišné.

# Další èást skriptu již neobsahuje tak podrobné komentáøe =)

################## Pøiklad 3. (Králíci) ###############################################
#######################################################################################
library(readxl)
kralici = read_excel("testy_vicevyberove.xlsx",
                   sheet=2)

colnames(kralici)=c("viden","cesky","kalif")   # pøejmenování sloupcù

# pøevod do standardního datového formátu
kralici.s=stack(kralici)
colnames(kralici.s)=c("hodnoty","skupina")
kralici.s=na.omit(kralici.s)

par(mfrow=c(1,1))
boxplot(kralici)

# Odstranìní odlehlého pozorování
pom=boxplot(kralici)
pom
kralici.s$hodnoty.bez=kralici.s$hodnoty
kralici.s$hodnoty.bez[kralici.s$hodnoty.bez %in% pom$out]=NA

# Krabicový graf
boxplot(hodnoty.bez~skupina,data = kralici.s)

# Ovìøení normality
skupiny = c("viden","cesky","kalif")
nadpisy = c("Vídeòský bílý\nmodrooký","Èeský \nstrakáè","Kalifornský")

par(mfrow=c(1,3))
for (i in 1:3){
  qqnorm(kralici.s$hodnoty.bez[kralici.s$skupina == skupiny[i]],
         main = nadpisy[i],
         xlab = "norm. teoretické kvantily",
         ylab = "výbìrové kvantily")
  qqline(kralici.s$hodnoty.bez[kralici.s$skupina == skupiny[i]])
}

library(moments)
tapply(kralici.s$hodnoty.bez,kralici.s$skupina,skewness,na.rm=TRUE)
tapply(kralici.s$hodnoty.bez,kralici.s$skupina,moments::kurtosis,na.rm=TRUE)-3

tapply(kralici.s$hodnoty.bez,kralici.s$skupina,shapiro.test)

# Na hladinì významnosti 0,05 nezamítáme pøedpoklad normality.

# Informace potøebné pro nastavení zaokrouhlování
tapply(kralici.s$hodnoty,kralici.s$skupina,length)# sd zaokrouhlujeme na 2 platné cifry
tapply(kralici.s$hodnoty,kralici.s$skupina,sd)# sd a míry polohy zaokrouhlujeme na setiny (sjednocení napøíè druhy králíkù)

# Ovìøení shody rozptylù
s2=tapply(kralici.s$hodnoty.bez,kralici.s$skupina,var,na.rm=TRUE)
s2

max(s2)/min(s2)
# Dle krabicového grafu a informace o pomìru nejvìtšího a nejmenšího rozptylù (blízký 2, avšak rozsah výbìrù < 30)
# je tìžší odhadnout, zda lze pøedpokládat shodu rozptylù. Rozhodnout nám pomùže test.

# Pøedpoklad normality nebyl zamítnut -> Bartlettùv test
bartlett.test(hodnoty.bez~skupina,data = kralici.s) 

# Na hladinì významnosti 0,05 nelze zamítnout pøedpoklad o shodì rozptylù 
# (Bartlettùv test, x_OBS = 3,1, df = 2, p-hodnota = 0,217).

# Chceme srovnávat stø. hodnoty nezávislých výbìrù z normálních rozdìlení se stejnými rozptyly -> ANOVA
# pøíkaz aov() vyžaduje data ve standardním datovém formátu

vysledky=aov(hodnoty.bez~skupina,data = kralici.s)
summary(vysledky)  

# Na hladinì významnosti 0,05 zamítáme hypotézu o shodì støedních hodnot (p-hodnota<<0,001, ANOVA) -> mnohonásobné porovnávání
TukeyHSD(vysledky)

# prùmìry
prum=tapply(kralici.s$hodnoty.bez,kralici.s$skupina,mean,na.rm=TRUE)
prum

# skupinové efekty
# rozsahy výbìrù bez NA hodnot (POZOR! Funkce length() poèítá i NA hodnoty.)
library(dplyr)
n=tapply(kralici.s$hodnoty.bez,kralici.s$skupina,n_distinct,na.rm = T)
prum.total=weighted.mean(prum,n)
efekt=prum-prum.total
efekt

################## Pøiklad 4. (Jakost) ################################################
#######################################################################################
library(readxl)
jakost.s = read_excel("testy_vicevyberove.xlsx",
                   sheet=3)

colnames(jakost.s)=c("poradi","skupina")   # pøejmenování sloupcù

par(mfrow=c(1,1))
boxplot(poradi~skupina, data = jakost.s)

# Ovìøení normality nemá smysl provádìt - z povahy jde o diskrétní data (poøadí)

# Informace potøebné pro nastavení zaokrouhlování
tapply(jakost.s$poradi,jakost.s$skupina,length)# sd zaokrouhlujeme na 2 platné cifry
tapply(jakost.s$poradi,jakost.s$skupina,sd)# sd a míry polohy zaokrouhlujeme na celá èísla

# Ovìøení shody rozptylù
s2=tapply(jakost.s$poradi,jakost.s$skupina,var,na.rm=TRUE)
s2

max(s2)/min(s2)
# Dle krabicového grafu a informace o pomìru nejvìtšího a nejmenšího rozptylù (<2)
# ze pøedpokládat shodu rozptylù. (Kruskalùv - Wallisùv test má vìtší sílu testu, jsou-li data homoskedasticitní.)

# Jde o "poøadová" data, nemá smysl uvažovat o pøedpokladu normality -> Leveneùv test
library(car)
leveneTest(poradi~skupina, data = jakost.s) 

jakost.s$skupina=as.factor(jakost.s$skupina)  # Starší verze pøíkazu leveneTest vyžaduje, aby "skupina" byla kódována jako faktor,
                                              # novì je výpoèet proveden, ale objevuje se doporuèení k pøekódování promìnné "skupina".
leveneTest(poradi~skupina, data = jakost.s)
# Na hladinì významnosti 0,05 nelze zamítnout pøedpoklad o shodì rozptylù 
# (Leveneho test, x_OBS = 0,4, df_num = 3, df_denom = 62, p-hodnota = 0,750).

# Ovìøení symetrie
par(mfrow=c(1,1))
boxplot(poradi~skupina, data = jakost.s)

tapply(jakost.s$poradi,jakost.s$skupina,moments::skewness)

skupiny = c("A","B","C","D")
par(mfrow=c(1,1))
for (i in 1:4){
  hist(jakost.s$poradi[jakost.s$skupina == skupiny[i]],
         main = skupiny[i],
         xlim = c(1,70),
         ylim = c(0,6),
         xlab = "poøadí",
         ylab = "èetnost")
}

# Chceme srovnávat mediány nezávislých výbìrù (data nelze považovat za výbìry z norm. rozdìlení) -> Kruskalùv-Wallisùv test
kruskal.test(poradi~skupina,data = jakost.s)

# Na hladinì významnosti 0,05 nelze zamítnout hypotézu o shodì mediánù (Kruskalùv-Wallisùv test, x_OBS = 3,7, df = 3, p-hodnota=0,295). 
# Tj. statisticky významné rozdíly mezi výrobci (z hlediska poøadí výrobkù v soutìži) neexistují.

################## Pøiklad 5. (Trombin) ###############################################
#######################################################################################
library(readxl)
trombin.s = read_excel("testy_vicevyberove.xlsx",
                      sheet=4,
                      skip = 1)

colnames(trombin.s)=c("hodnoty","skupina")   # pøejmenování sloupcù

par(mfrow=c(1,1))
boxplot(hodnoty~skupina,data = trombin.s)

# Ovìøení normality
skupiny = c("A","B","C")

par(mfrow=c(1,3))
for (i in 1:3){
  qqnorm(trombin.s$hodnoty[trombin.s$skupina == skupiny[i]],
         main = skupiny[i],
         xlab = "norm. teoretické kvantily",
         ylab = "výbìrové kvantily")
  qqline(trombin.s$hodnoty[trombin.s$skupina == skupiny[i]])
}

library(moments)
tapply(trombin.s$hodnoty,trombin.s$skupina,skewness,na.rm=TRUE)
tapply(trombin.s$hodnoty,trombin.s$skupina,moments::kurtosis,na.rm=TRUE)-3

tapply(trombin.s$hodnoty,trombin.s$skupina,shapiro.test)

# Na hladinì významnosti 0,05 zamítáme pøedpoklad normality.

# Informace potøebné pro nastavení zaokrouhlování
tapply(trombin.s$hodnoty,trombin.s$skupina,length)# sd zaokrouhlujeme na 2 platné cifry
tapply(trombin.s$hodnoty,trombin.s$skupina,sd)# sd a míry polohy zaokrouhlujeme na setiny (sjednocení napøíè skupinami)

# Ovìøení shody rozptylù
s2=tapply(trombin.s$hodnoty,trombin.s$skupina,var,na.rm=TRUE)
s2

max(s2)/min(s2)
# Dle krabicového grafu a informace o pomìru nejvìtšího a nejmenšího rozptylù (>>2)
# nelze pøedpokládat shodu rozptylù. Rozhodnout nám pomùže test.

# Pøedpoklad normality byl zamítnut -> Leveneho test
library(car)
leveneTest(hodnoty~skupina,data = trombin.s) 

trombin.s$skupina=as.factor(trombin.s$skupina)
leveneTest(hodnoty~skupina,data = trombin.s)
# Na hladinì významnosti 0,05 zamítáme pøedpoklad o shodì rozptylù 
# (Leveneho test, p-hodnota << 0,001).

# Ovìøení symetrie
par(mfrow=c(1,1))
boxplot(hodnoty~skupina, data = trombin.s)

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

# Chceme srovnávat mediány nezávislých výbìrù, která nemají normální rozdìlení a mají rùzné rozptyly -> Kruskalùv - Wallisùv test

kruskal.test(trombin.s$hodnoty,trombin.s$skupina)
# Na hladinì významnosti 0,05 zamítáme hypotézu o shodì mediánù (p-hodnota<<0,001, Kruskalùv-Wallisùv test). Tj. trombinový èas je statisticky významnì
# ovlivnìn preparátem. -> mnohonásobné porovnávání
library(dunn.test)
dunn.test(trombin.s$hodnoty,trombin.s$skupina,
          method = "bonferroni",
          alpha = 0.05,  # nastavíme-li hodnotu alfa, jsou st. významné rozdíly oznaèeny * 
                         # (defaultnì: alpha = 0.05)
          altp = T)      # altP = T nastavuje p-hodnotu tak,aby se pøi rozhodování
                         # o statistické významnosti srovnávala s alfa (defaultnì: altp = F, pak srovnáváme s alfa/2)

# mediány
med=tapply(trombin.s$hodnoty,trombin.s$skupina,quantile,prob=0.5,na.rm=TRUE)
med

# skupinové efekty
med.total=quantile(trombin.s$hodnoty,probs=0.5)
efekt=med-med.total
efekt
