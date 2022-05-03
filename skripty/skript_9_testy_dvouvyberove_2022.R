#######################################################################################
########################### Dvouvýbìrové testy ########################################
######################### Martina Litschmannová #######################################
#######################################################################################

setwd("C:/Users/Martina/OneDrive - VŠB-TU Ostrava/Výuka/Pravdìpodobnost a statistika/DATA/aktualni/")

library(readxl)
library(tidyr)   # funkce pivot_longer()
library(ggplot2)
library(moments) # funkce skewness() a kurtosis()
library(dplyr)
library(dlookr)  # funkce normality()

################## Pøiklad 1. (Cholesterol) ###########################################
#######################################################################################

# Test o shodì støedních hodnot / mediánù

## Naètení dat z xlsx souboru (pomoci balíèku readxl)

chol = read_excel("testy_dvouvyberove.xlsx",
                  sheet = "cholesterol2",
                  skip = 1)   
colnames(chol)=c("mladsi","starsi")

## Pøevod do standardního datového formátu
chol.s = pivot_longer(chol,cols = c("mladsi","starsi"),
                      names_to = "skupina",
                      values_to = "hodnoty")
chol.s = na.omit(chol.s)   # Funkci na.omit() používejte velmi ooutøetnì!


## Exploraèní analýza
boxplot(chol.s$hodnoty~chol.s$skupina)

# Data obsahují odlehlá pozorování.
chol.s$ID = 1:length(chol.s$skupina)

outliers = chol.s %>% 
  group_by(skupina) %>% 
  identify_outliers(hodnoty)

chol.s$hodnoty.out = ifelse(chol.s$ID %in% outliers$ID,NA,chol.s$hodnoty)

boxplot(chol.s$hodnoty.out~chol.s$skupina)

# Samostatné promìnné
mladsi.out = chol.s$hodnoty.out[chol.s$skupina=="mladsi"]
starsi.out = chol.s$hodnoty.out[chol.s$skupina=="starsi"]


## Exploraèní analýza pro data out odlehlých pozorování
boxplot(chol.s$hodnoty.out~chol.s$skupina)

chol.s %>% 
  group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(hodnoty.out)),
            sm.odch = sd(hodnoty.out,na.rm = T))

# sd budeme zaokrouhlovat na 3 platné cifry, tj. sd i míry polohy budeme zaokrouhlovat na tisíciny


# Ovìøení normality
ggplot(chol.s,aes(x = hodnoty.out)) +
  geom_histogram() +
  facet_wrap(~skupina,nrow = 2)

ggplot(chol.s,aes(sample = hodnoty.out)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~skupina,scales = "free")

chol.s %>% 
  group_by(skupina) %>% 
    summarise(
    šikmost = moments::skewness(hodnoty.out,na.rm = T),
    špièatost = moments::kurtosis(hodnoty.out,na.rm = T)-3)

chol.s %>% 
  group_by(skupina) %>% 
  normality(hodnoty.out)
  
# Šikmost i špièatost odpovídá norm. rozdìlení. Pro koneèné rozhodnutí o normalitì dat použijeme
# test normality.

# Na hl. významnosti 0.05 nelze pøedpoklad normality zamítnout 
# (p-hodnota=0.464 (mladší), p-hodnota=0.940 (starší), Shapirùv-Wilkùv test).

# Ovìøení shody rozptylù

# Chceme-li ovìøení shody rozptylù øešit radìji pomocí intervalového odhadu, 
# je pro snazší interpretaci lepší zajistit, aby byl nalezen odhad pro pomìr
# max(rozptyl 1, rozptyl 2)/min(rozptyl 1, rozptyl 2), tj. vìtší rozptyl v èitateli.

boxplot(chol.s$hodnoty.out~chol.s$skupina) 

# V našem pøípadì volíme pomìr rozptyl.starsi/rozptyl.mladsi.

# H0: sigma.starsi = sigma.mladsi
# Ha: sigma.starsi <> sigma.mladsi
 
var.test(starsi.out,mladsi.out,ratio=1,conf.level=0.95)
# Na hl. významnosti 0.05 zamítáme pøedpoklad o shodì rozptylù (p-hodnota<<0.001,test o shodì rozptylù).
# Pozorovanou neshodu mezi rozptyly lze na hladinì významnosti 0,05 oznaèit 
# za statisticky významnou (F test, F = 12,2, df_num = 83, df_denom = 96, p-hodnota < 0,001).

# Doplnìní:
# Rozptyl hodnot cholesterolu ve skupinì starších mužù je cca 12x vìtší než 
# rozptyl hodnot cholesterolu ve skupinì mladších mužù. 95% intervalový odhad  
# tohota pomìru je (8;19). 

# Ovìøení shody støedních hodnot (Aspinové-Welchùv test)

# Pro snadnou interpretaci intervalového odhadu je vhodné poøadí výbìru stanovit tak,
# aby rozdíl jejich prùmìrù byl kladný.

# H0: mu.starsi = mu.mladsi (mu.starsi - mu.mladsi = 0)
# Ha: mu.starsi > mu.mladsi (mu.starsi - mu.mladsi > 0)
t.test(starsi.out,mladsi.out,mu=0,alternative="greater",
       var.equal=FALSE,conf.level=0.95)

# Dle výsledkù výbìrového šetøení oèekáváme, že støední obsah cholesterolu v krvi straších mužù
# bude cca o 0,524 mmol/l vyšší než støední obsah cholesterolu u mladších mužù.
# Dle 95% levostranný intervalového odhadu daného rozdílu oèekáváme støední obsah cholesterolu
# u starších mužù minimálnì o 0,457 mmol/l vìtší než stø. hodnota cholesterolu u mladších mužù. 

# Na hladinì významnosti 0,05 zamítáme pøedpoklad o shodì støedních hodnot cholesterolu ve skupinách 
# mladších a starších mužù ve prospìch alternativy, že starší muži mají vyšší støední hladinu 
# cholesterolu než muži mladší (t-test, t = 13,1, df = 95, p-hodnota < 0,001).

################## Pøiklad 2. (Deprese) ###############################################
#######################################################################################

# Test o shodì støedních hodnot / mediánù

## Naètení dat z xlsx souboru (pomoci balíèku readxl)
deprese = read_excel("testy_dvouvyberove.xlsx",
                     sheet = "deprese")   
colnames(deprese)=c("endo","neuro")

## Pøevod do standardního datového formátu
deprese.s = pivot_longer(deprese,cols = c("endo","neuro"),
                         names_to = "skupina",
                         values_to = "hodnoty")
deprese.s = na.omit(deprese.s)

## Exploraèní analýza
boxplot(deprese.s$hodnoty~deprese.s$skupina)

# Data neobsahují odlehlá pozorování.

# Samostatné promìnné
endo = deprese.s %>% filter(skupina == "endo") %>% droplevels()
neuro = deprese.s %>% filter(skupina == "neuro") %>% droplevels()

deprese.s %>% group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(hodnoty)),
            sm.odch = sd(hodnoty,na.rm = T))

# sd budeme zaokrouhlovat na 3 platné cifry, sd i míry polohy budeme zaokrouhlovat na jednotky


# Ovìøení normality
ggplot(deprese.s,aes(x = hodnoty)) +
  geom_histogram() +
  facet_wrap(~skupina,nrow = 2)

ggplot(deprese.s,aes(sample = hodnoty)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~skupina,scales = "free")

deprese.s %>% 
  group_by(skupina) %>% 
  summarise(
    šikmost = moments::skewness(hodnoty,na.rm = T),
    špièatost = moments::kurtosis(hodnoty,na.rm = T)-3)

chol.s %>% 
  group_by(skupina) %>% 
  normality(hodnoty)

# Šikmost i špièatost odpovídá norm. rozdìlení. Pro koneèné rozhodnutí o normalitì dat použijeme
# test normality.

# Na hl. významnosti 0,05 zamítáme pøedpoklad normality 
# (Shapirùv-Wilkùv test, p-hodnota_endo<<0,001, p-hodnota_neuro=0,281).

# Ovìøení shody mediánù (Mannùv - Whitneyho test)
# POZOR! V nulové hypotéze testujeme, zda se rozdìlení populací, z nichž
# byly výbìry poøízeny liší pouze svou polohou (konkrétnì mediánem).

# Dle histogramù pøedpokládáme, že data mají stejný typ rozdìlení.

# Pro snadnou interpretaci intervalového odhadu je vhodné poøadí výbìru stanovit tak,
# aby rozdíl jejich mediánù byl kladný.

# H0: med.neuro = med.endo (med.neuro - med.endo = 0)
# Ha: med.neuro > med.endo (med.neuro - med.endo > 0)
wilcox.test(neuro$hodnoty,endo$hodnoty,
            mu=0,
            exact=FALSE,
            alternative="greater",
            conf.level=0.95,
            conf.int = T) # POZOR! Pøednastavená hodnota conf.int je FALSE.

# Doba remise pacientù s neurotickou depresí je cca o 191 dnù delší než u pacientù 
# s endogenní depresí. Dle 95% levostranného intervalového odhadu oèekáváme,
# že pacienti s neurotickou depresí mají minimálnì o 168 dní delší dobu remise
# než pacienti s neurotickou depresí.

# Na hladinì významnosti 0,05 zamítáme hypotézu o shodì 
# mediánù dob do remise onemocnìní pro obì skupiny pacientù ve prospìch alternativy 
# (Wilcoxonùv test s korekcí na spojitost, W = 16 366, p-hodnota << 0,001).
# Medián doby remise je u pacientù s neurotickou depresí statisticky významnì
# vìtší než u pacientù s endogenní depresí.

################## Pøiklad 3. (Osmolalita - párová data) ##############################
#######################################################################################

# Naètení dat 
osmolalita = read_excel("testy_dvouvyberove.xlsx",
                        sheet = "osmolalita", skip = 1)  
osmolalita = osmolalita[,c(2,3)]
colnames(osmolalita)=c("o8","o11")

# Výpoèet nárùstu osmolality
osmolalita$narust = osmolalita$o11 -  osmolalita$o8

## Exploraèní analýza 
boxplot(osmolalita$narust)

# Data obsahují odlehlá pozorování.

# Odstranìní odlehlých hodnot
osmolalita$ID = 1:length(osmolalita$narust)

outliers = osmolalita %>% 
  identify_outliers(narust)

osmolalita$narust.out = ifelse(osmolalita$ID %in% outliers$ID,NA,osmolalita$narust)

## Exploraèní analýza pro data out odlehlých pozorování
boxplot(osmolalita$narust.out)
abline(h=0,lty=2) # pøímka definující nulový efekt

osmolalita %>% 
  summarise(rozsah = length(na.omit(narust)),
            sm.odch = sd(narust,na.rm = T),
            prumer = mean(narust,na.rm = T),
            median = median(narust,na.rm = T))

# sd zaokrouhlujeme na 2 platné cifry, tj. sd a míry polohy zaokrouhlujeme na jednotky

# Ovìøení normality
ggplot(osmolalita,aes(x = narust.out)) +
  geom_histogram() 

ggplot(osmolalita,aes(sample = narust.out)) +
  stat_qq() +
  stat_qq_line() 

osmolalita %>% 
  summarise(
    šikmost = moments::skewness(narust.out,na.rm = T),
    špièatost = moments::kurtosis(narust.out,na.rm = T)-3)

osmolalita %>% 
  normality(narust.out)

# Šikmost i špièatost odpovídají normálnímu rozdìlení.

# Na hl. významnosti 0.05 nelze pøedpoklad normality zamítnout 
# (Shapirùv-Wilkùv test, W = 0,949, p-hodnota=0,545).

# Párový t-test
# H0: mu.narust = 0 mm
# Ha: mu.narust > 0 mm

t.test(osmolalita$narust.out,mu=0,alternative="greater")

# Dle výbìrového šetøení lze oèekávat, že osmolalita moèi se
# mezi 8 a 11 hodinou zvýší o cca 24 mmol/kg. Dle 95% intervalového odhadu
# lze oèekávat, že dojde k navýšení osmolality minimálnì o 10 mmol/kg).
# Na hladinì významnosti 0,05 lze tento nárùst oznaèit za statisticky
# významný (párový t-test, t = 3,1, df = 13, p-hodnota = 0,005).

################## Pøiklad 4. (Porovnání kvality) #####################################
#######################################################################################
x.MM = 14
n.MM = 200
p.MM = x.MM/n.MM

x.PP = 15
n.PP = 150
p.PP = x.PP/n.PP


# Ovìøení pøedpokladù
n.MM > 9/(p.MM*(1-p.MM))
n.PP > 9/(p.PP*(1-p.PP))
# Dále pro obì firmy pøedpokládáme, že  n/N < 0.05, tj. že daná populace (souèástek) má rozsah 
# alespoò 20*n, tj. 20*200 (4 000), resp. 20*150 (3 000) souèástek, což je asi vcelku reálný pøedpoklad.

## Waldùv test #############################################################
# "Poøadí výbìrù" volíme kvùli snazší interpretaci tak, aby rozdíl "p.1 - p.2" vycházel kladný.
## H0: pi.PP = pi.MM
## Ha: pi.PP > pi.MM

x.obs = (p.PP - p.MM)/sqrt((p.PP*(1-p.PP)/n.PP)+(p.MM*(1-p.MM)/n.MM))
x.obs
p.value = 1-pnorm(x.obs,0,1)
p.value

# Pravostranný intervalový odhad
alpha = 0.05
p = (x.PP + x.MM)/(n.PP + n.MM)
s.error = qnorm(1-alpha/2)*sqrt(p*(1-p)*(1/n.MM + 1/n.PP))
dolni.mez = (p.PP - p.MM) - s.error
dolni.mez

# Dle výbìrového šetøení oèekáváme, že pravdìpodobnost výroby vadné souèástky
# je u firmy PP cca o 3 % vìtší než u firmy MM. Na základì 95% Waldova pravostranného intervalového odhadu
# (-0,028; 1,000) lze pozorovaný rozdíl v kvalitì výroby oznaèit za statisticky nevýznamný. Ke stejným závìrùm
# mùžeme dojít i na základì Waldova pravostranného testu (x_obs = 0,986,
# p-hodnota = 0,162).


## Pearsonùv X2 test #######################################################
## H0: pi.PP = pi.MM
## Ha: pi.PP > pi.MM

prop.test(c(x.PP,x.MM),c(n.PP,n.MM),alternative="greater",conf.level=0.95)

# Dle výbìrového šetøení oèekáváme, že pravdìpodobnost výroby vadné souèástky
# je u firmy PP cca o 3 % vìtší než u firmy MM. Na základì 95% Clopperova - Pearsonova pravostranného intervalového odhadu
# (-0,025; 1,000) lze pozorovaný rozdíl v kvalitì výroby oznaèit za statisticky nevýznamný. Ke stejným závìrùm
# mùžeme dojít i na základì Pearsonova pravostranného testu (x_obs = 0,659,
# df = 1, p-hodnota = 0,209).

################################# Pro zájemce #########################
################# Jiná grafická interpretace k pøíkladu 3 #############
#######################################################################

# Bodový graf doplnìný o pøímku y = x
plot(osmolalita$o8,osmolalita$o11,
     xlab = "osmolalita v 8 h (mmol/kg)",
     ylab = "osmolalita v 11 h (mmol/kg)")
abline(0,1,col = "red")

# Krabicový graf pro párová data
s = seq(length(osmolalita$o8)) #posloupnost 1,2,...,rozsah dat 
par(mfrow = c(1,1),bty="l") #bty - nastavení ohranièení grafu (jsou viditelné pouze osy x,y, nikoliv rámeèek kolem grafu)
boxplot(osmolalita$o8,osmolalita$o11,
        ylab="osmolalita (mmol/kg)",
        names=c("8 hodin","11 hodin"),
        cex.main = 0.9)
stripchart(list(osmolalita$o8,osmolalita$o11),
           vertical=T,
           pch=16,
           cex=0.7,
           add=T) # pøidání bodového grafu do boxplotu
#vykreslení spojnic mezi datovými páry
x0 = rep(1,length(osmolalita$o8))
y0 = osmolalita$o8
x1 = rep(2,length(osmolalita$o11))
y1 = osmolalita$o11
segments(x0,y0,x1,y1,
         col=1,
         lwd=0.5)

#Graf rozdílù párových dat doplnìný o bodový a intervalový odhad støední hodnoty,
# resp. mediánu, nárustu osmolality mezi 8 h a 11 h

#Intervalový odhad støední hodnoty (t.test) nebo mediánu (wilcox.test)
res = t.test(osmolalita$narust,conf.level=0.95)
#res = wilcox.test(osmolalita$narust,conf.level=0.95,conf.int = T)

par(mfrow = c(1,1),mar = c(2,7,2,2))
stripchart(osmolalita$narust,
           vertical=T,
           pch=16,
           method="jitter",
           main="Zmìna osmolality mezi 8 h a 11 h (mmol/kg)",
           ylab="Zmìna osmolality (mmol/kg):\n11 h - 8 h",
           cex.main = 0.8)
points(1,res$estimate,col="red",pch=16,cex=1.2) # bod definující medián
arrows(1,res$conf.int[1],
       1,res$conf.int[2],
       col="red",
       code=3, # definuje styl "šipek"
       lwd=2,
       angle=90,
       length = 0.1) # definuje délku krajních úseèek
abline(h=0,lty=2) # pøímka definující nulový efekt

########################################################
######## Blandùv - Altmanùv graf #######################
########################################################

BA_plot = function(x,y,x.lab = "(x + y)/2",y.lab = "y - x",pomer = F){
  # x ... mìøení 1
  # y ... mìøení 2
  # x.lab, y.lab ... popisky os
  # pomer ... TRUE/FALSE urèuje, zda má být na vertikální osu vynášen pomìr y/x
  
  if (pomer & y.lab == "y - x") {
    y.lab = "y/x"
  }
  
  diff = y - x
  diff_ln = log(y)-log(x)
  
  prumer_diff = mean(diff)
  prumer_ln = mean(diff_ln)
  sd_diff = sd(diff)
  sd_ln = sd(diff_ln)
  
  if (pomer) {
    prumer = exp(prumer_ln) # geometrický prùmìr pomìrù
  } else {
    prumer = prumer_diff  # prùmìr rozdílù
  }
  
  # Interval, v nìmž rozdíly leží s pravdìpodobností 95 %, tj. limity shody
  if (pomer) {
    lb = exp(prumer_ln - qnorm(0.975)*sd_ln)
  } else {
    lb = prumer_diff - qnorm(0.975)*sd_diff
  }
  if (pomer) {
    ub = exp(prumer_ln + qnorm(0.975)*sd_ln)
  } else {
    ub = prumer_diff + qnorm(0.975)*sd_diff
  }
  
  # Definice vykreslovaných hodnot dle pomer
  xx = (x + y)/2
  if (pomer) {
    yy = y/x
  } else {
    yy = diff
  }
  
  # Definice rozsahu osy y
  if (lb > 0){
    lo = min(0.9*lb,min(yy))
  } else {
    lo = min(1.1*lb,min(yy))
  }
  
  if (ub > 0){
    up = max(0.9*lb,max(yy))
  } else {
    up = max(1.1*lb,max(yy))
  }
  
  plot(xx,yy,
       xlab = x.lab,
       ylab = y.lab,
       pch = 16,
       ylim = c(lo,up))
  
  # Definice nulového efektu
  if (pomer) {
    e0 = 1
  } else {
    e0 = 0
  }
  
  abline(h=e0,lty = 2, col = "red") # pøímka definující nulový efekt
  abline(h=prumer,lty = 2) # pøímka definující prùmìr
  
  # Limity shody
  # pøímka definující horní mez intervalu, v nìmž rozdíly leží s 95% p-stí
  abline(h = ub,lty = 3) 
  # pøímka definující dolní mez intervalu, v nìmž rozdíly leží s 95% p-stí
  abline(h=lb,lty = 3) 
  
  print(paste0("Limity shody: (",round(lb,2),"; ",round(ub,2),")"))
  print(paste0("Prùmìr: ",round(prumer,2)))
}

########################################################

# Aplikace funkce BA_plot
par(mfrow = c(1,1),mar = c(5,5,2,2))
BA_plot(osmolalita$o8,osmolalita$o11,pomer = F,
        x.lab = "prùmìr osmolality v 8 a v 11 hodin (mmol/kg)",
        y.lab = "nárùst osmolality \nmezi 8 a 11 hodinou (mmol/kg)")

# Aplikace funkce BA_plot na logaritmovaná data
par(mfrow = c(1,1),mar = c(5,5,2,2))
BA_plot(log(osmolalita$o8),log(osmolalita$o11),
        x.lab = "prùmìr pøirozených logaritmù osmolality v 8 a v 11 hodin \n(mmol/kg)",
        y.lab = "rozdíl logaritmù osmolality \nv 8 a v 11 hodin (mmol/kg)")

# Aplikace funkce BA_plot pro závislost pomìrù mìøení ku geometrickému prùmìru mìøení
BA_plot(osmolalita$o8,osmolalita$o11,pomer = T,
        x.lab = "geometrický prùmìr osmolality v 8 a v 11 hodin \n(mmol/kg)",
        y.lab = "pomìr osmolality v 11 hodin \nku osmolalitì v 8 hodin")

