#######################################################################################
########################### Dvouv�b�rov� testy ########################################
######################### Martina Litschmannov� #######################################
#######################################################################################

setwd("C:/Users/Martina/OneDrive - V�B-TU Ostrava/V�uka/Pravd�podobnost a statistika/DATA/aktualni/")

library(readxl)
library(tidyr)   # funkce pivot_longer()
library(ggplot2)
library(moments) # funkce skewness() a kurtosis()
library(dplyr)
library(dlookr)  # funkce normality()

################## P�iklad 1. (Cholesterol) ###########################################
#######################################################################################

# Test o shod� st�edn�ch hodnot / medi�n�

## Na�ten� dat z xlsx souboru (pomoci bal��ku readxl)

chol = read_excel("testy_dvouvyberove.xlsx",
                  sheet = "cholesterol2",
                  skip = 1)   
colnames(chol)=c("mladsi","starsi")

## P�evod do standardn�ho datov�ho form�tu
chol.s = pivot_longer(chol,cols = c("mladsi","starsi"),
                      names_to = "skupina",
                      values_to = "hodnoty")
chol.s = na.omit(chol.s)   # Funkci na.omit() pou��vejte velmi oout�etn�!


## Explora�n� anal�za
boxplot(chol.s$hodnoty~chol.s$skupina)

# Data obsahuj� odlehl� pozorov�n�.
chol.s$ID = 1:length(chol.s$skupina)

outliers = chol.s %>% 
  group_by(skupina) %>% 
  identify_outliers(hodnoty)

chol.s$hodnoty.out = ifelse(chol.s$ID %in% outliers$ID,NA,chol.s$hodnoty)

boxplot(chol.s$hodnoty.out~chol.s$skupina)

# Samostatn� prom�nn�
mladsi.out = chol.s$hodnoty.out[chol.s$skupina=="mladsi"]
starsi.out = chol.s$hodnoty.out[chol.s$skupina=="starsi"]


## Explora�n� anal�za pro data out odlehl�ch pozorov�n�
boxplot(chol.s$hodnoty.out~chol.s$skupina)

chol.s %>% 
  group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(hodnoty.out)),
            sm.odch = sd(hodnoty.out,na.rm = T))

# sd budeme zaokrouhlovat na 3 platn� cifry, tj. sd i m�ry polohy budeme zaokrouhlovat na tis�ciny


# Ov��en� normality
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
    �ikmost = moments::skewness(hodnoty.out,na.rm = T),
    �pi�atost = moments::kurtosis(hodnoty.out,na.rm = T)-3)

chol.s %>% 
  group_by(skupina) %>% 
  normality(hodnoty.out)
  
# �ikmost i �pi�atost odpov�d� norm. rozd�len�. Pro kone�n� rozhodnut� o normalit� dat pou�ijeme
# test normality.

# Na hl. v�znamnosti 0.05 nelze p�edpoklad normality zam�tnout 
# (p-hodnota=0.464 (mlad��), p-hodnota=0.940 (star��), Shapir�v-Wilk�v test).

# Ov��en� shody rozptyl�

# Chceme-li ov��en� shody rozptyl� �e�it rad�ji pomoc� intervalov�ho odhadu, 
# je pro snaz�� interpretaci lep�� zajistit, aby byl nalezen odhad pro pom�r
# max(rozptyl 1, rozptyl 2)/min(rozptyl 1, rozptyl 2), tj. v�t�� rozptyl v �itateli.

boxplot(chol.s$hodnoty.out~chol.s$skupina) 

# V na�em p��pad� vol�me pom�r rozptyl.starsi/rozptyl.mladsi.

# H0: sigma.starsi = sigma.mladsi
# Ha: sigma.starsi <> sigma.mladsi
 
var.test(starsi.out,mladsi.out,ratio=1,conf.level=0.95)
# Na hl. v�znamnosti 0.05 zam�t�me p�edpoklad o shod� rozptyl� (p-hodnota<<0.001,test o shod� rozptyl�).
# Pozorovanou neshodu mezi rozptyly lze na hladin� v�znamnosti 0,05 ozna�it 
# za statisticky v�znamnou (F test, F = 12,2, df_num = 83, df_denom = 96, p-hodnota < 0,001).

# Dopln�n�:
# Rozptyl hodnot cholesterolu ve skupin� star��ch mu�� je cca 12x v�t�� ne� 
# rozptyl hodnot cholesterolu ve skupin� mlad��ch mu��. 95% intervalov� odhad  
# tohota pom�ru je (8;19). 

# Ov��en� shody st�edn�ch hodnot (Aspinov�-Welch�v test)

# Pro snadnou interpretaci intervalov�ho odhadu je vhodn� po�ad� v�b�ru stanovit tak,
# aby rozd�l jejich pr�m�r� byl kladn�.

# H0: mu.starsi = mu.mladsi (mu.starsi - mu.mladsi = 0)
# Ha: mu.starsi > mu.mladsi (mu.starsi - mu.mladsi > 0)
t.test(starsi.out,mladsi.out,mu=0,alternative="greater",
       var.equal=FALSE,conf.level=0.95)

# Dle v�sledk� v�b�rov�ho �et�en� o�ek�v�me, �e st�edn� obsah cholesterolu v krvi stra��ch mu��
# bude cca o 0,524 mmol/l vy��� ne� st�edn� obsah cholesterolu u mlad��ch mu��.
# Dle 95% levostrann� intervalov�ho odhadu dan�ho rozd�lu o�ek�v�me st�edn� obsah cholesterolu
# u star��ch mu�� minim�ln� o 0,457 mmol/l v�t�� ne� st�. hodnota cholesterolu u mlad��ch mu��. 

# Na hladin� v�znamnosti 0,05 zam�t�me p�edpoklad o shod� st�edn�ch hodnot cholesterolu ve skupin�ch 
# mlad��ch a star��ch mu�� ve prosp�ch alternativy, �e star�� mu�i maj� vy��� st�edn� hladinu 
# cholesterolu ne� mu�i mlad�� (t-test, t = 13,1, df = 95, p-hodnota < 0,001).

################## P�iklad 2. (Deprese) ###############################################
#######################################################################################

# Test o shod� st�edn�ch hodnot / medi�n�

## Na�ten� dat z xlsx souboru (pomoci bal��ku readxl)
deprese = read_excel("testy_dvouvyberove.xlsx",
                     sheet = "deprese")   
colnames(deprese)=c("endo","neuro")

## P�evod do standardn�ho datov�ho form�tu
deprese.s = pivot_longer(deprese,cols = c("endo","neuro"),
                         names_to = "skupina",
                         values_to = "hodnoty")
deprese.s = na.omit(deprese.s)

## Explora�n� anal�za
boxplot(deprese.s$hodnoty~deprese.s$skupina)

# Data neobsahuj� odlehl� pozorov�n�.

# Samostatn� prom�nn�
endo = deprese.s %>% filter(skupina == "endo") %>% droplevels()
neuro = deprese.s %>% filter(skupina == "neuro") %>% droplevels()

deprese.s %>% group_by(skupina) %>% 
  summarise(rozsah = length(na.omit(hodnoty)),
            sm.odch = sd(hodnoty,na.rm = T))

# sd budeme zaokrouhlovat na 3 platn� cifry, sd i m�ry polohy budeme zaokrouhlovat na jednotky


# Ov��en� normality
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
    �ikmost = moments::skewness(hodnoty,na.rm = T),
    �pi�atost = moments::kurtosis(hodnoty,na.rm = T)-3)

chol.s %>% 
  group_by(skupina) %>% 
  normality(hodnoty)

# �ikmost i �pi�atost odpov�d� norm. rozd�len�. Pro kone�n� rozhodnut� o normalit� dat pou�ijeme
# test normality.

# Na hl. v�znamnosti 0,05 zam�t�me p�edpoklad normality 
# (Shapir�v-Wilk�v test, p-hodnota_endo<<0,001, p-hodnota_neuro=0,281).

# Ov��en� shody medi�n� (Mann�v - Whitneyho test)
# POZOR! V nulov� hypot�ze testujeme, zda se rozd�len� populac�, z nich�
# byly v�b�ry po��zeny li�� pouze svou polohou (konkr�tn� medi�nem).

# Dle histogram� p�edpokl�d�me, �e data maj� stejn� typ rozd�len�.

# Pro snadnou interpretaci intervalov�ho odhadu je vhodn� po�ad� v�b�ru stanovit tak,
# aby rozd�l jejich medi�n� byl kladn�.

# H0: med.neuro = med.endo (med.neuro - med.endo = 0)
# Ha: med.neuro > med.endo (med.neuro - med.endo > 0)
wilcox.test(neuro$hodnoty,endo$hodnoty,
            mu=0,
            exact=FALSE,
            alternative="greater",
            conf.level=0.95,
            conf.int = T) # POZOR! P�ednastaven� hodnota conf.int je FALSE.

# Doba remise pacient� s neurotickou depres� je cca o 191 dn� del�� ne� u pacient� 
# s endogenn� depres�. Dle 95% levostrann�ho intervalov�ho odhadu o�ek�v�me,
# �e pacienti s neurotickou depres� maj� minim�ln� o 168 dn� del�� dobu remise
# ne� pacienti s neurotickou depres�.

# Na hladin� v�znamnosti 0,05 zam�t�me hypot�zu o shod� 
# medi�n� dob do remise onemocn�n� pro ob� skupiny pacient� ve prosp�ch alternativy 
# (Wilcoxon�v test s korekc� na spojitost, W = 16 366, p-hodnota << 0,001).
# Medi�n doby remise je u pacient� s neurotickou depres� statisticky v�znamn�
# v�t�� ne� u pacient� s endogenn� depres�.

################## P�iklad 3. (Osmolalita - p�rov� data) ##############################
#######################################################################################

# Na�ten� dat 
osmolalita = read_excel("testy_dvouvyberove.xlsx",
                        sheet = "osmolalita", skip = 1)  
osmolalita = osmolalita[,c(2,3)]
colnames(osmolalita)=c("o8","o11")

# V�po�et n�r�stu osmolality
osmolalita$narust = osmolalita$o11 -  osmolalita$o8

## Explora�n� anal�za 
boxplot(osmolalita$narust)

# Data obsahuj� odlehl� pozorov�n�.

# Odstran�n� odlehl�ch hodnot
osmolalita$ID = 1:length(osmolalita$narust)

outliers = osmolalita %>% 
  identify_outliers(narust)

osmolalita$narust.out = ifelse(osmolalita$ID %in% outliers$ID,NA,osmolalita$narust)

## Explora�n� anal�za pro data out odlehl�ch pozorov�n�
boxplot(osmolalita$narust.out)
abline(h=0,lty=2) # p��mka definuj�c� nulov� efekt

osmolalita %>% 
  summarise(rozsah = length(na.omit(narust)),
            sm.odch = sd(narust,na.rm = T),
            prumer = mean(narust,na.rm = T),
            median = median(narust,na.rm = T))

# sd zaokrouhlujeme na 2 platn� cifry, tj. sd a m�ry polohy zaokrouhlujeme na jednotky

# Ov��en� normality
ggplot(osmolalita,aes(x = narust.out)) +
  geom_histogram() 

ggplot(osmolalita,aes(sample = narust.out)) +
  stat_qq() +
  stat_qq_line() 

osmolalita %>% 
  summarise(
    �ikmost = moments::skewness(narust.out,na.rm = T),
    �pi�atost = moments::kurtosis(narust.out,na.rm = T)-3)

osmolalita %>% 
  normality(narust.out)

# �ikmost i �pi�atost odpov�daj� norm�ln�mu rozd�len�.

# Na hl. v�znamnosti 0.05 nelze p�edpoklad normality zam�tnout 
# (Shapir�v-Wilk�v test, W = 0,949, p-hodnota=0,545).

# P�rov� t-test
# H0: mu.narust = 0 mm
# Ha: mu.narust > 0 mm

t.test(osmolalita$narust.out,mu=0,alternative="greater")

# Dle v�b�rov�ho �et�en� lze o�ek�vat, �e osmolalita mo�i se
# mezi 8 a 11 hodinou zv��� o cca 24 mmol/kg. Dle 95% intervalov�ho odhadu
# lze o�ek�vat, �e dojde k nav��en� osmolality minim�ln� o 10 mmol/kg).
# Na hladin� v�znamnosti 0,05 lze tento n�r�st ozna�it za statisticky
# v�znamn� (p�rov� t-test, t = 3,1, df = 13, p-hodnota = 0,005).

################## P�iklad 4. (Porovn�n� kvality) #####################################
#######################################################################################
x.MM = 14
n.MM = 200
p.MM = x.MM/n.MM

x.PP = 15
n.PP = 150
p.PP = x.PP/n.PP


# Ov��en� p�edpoklad�
n.MM > 9/(p.MM*(1-p.MM))
n.PP > 9/(p.PP*(1-p.PP))
# D�le pro ob� firmy p�edpokl�d�me, �e  n/N < 0.05, tj. �e dan� populace (sou��stek) m� rozsah 
# alespo� 20*n, tj. 20*200 (4 000), resp. 20*150 (3 000) sou��stek, co� je asi vcelku re�ln� p�edpoklad.

## Wald�v test #############################################################
# "Po�ad� v�b�r�" vol�me kv�li snaz�� interpretaci tak, aby rozd�l "p.1 - p.2" vych�zel kladn�.
## H0: pi.PP = pi.MM
## Ha: pi.PP > pi.MM

x.obs = (p.PP - p.MM)/sqrt((p.PP*(1-p.PP)/n.PP)+(p.MM*(1-p.MM)/n.MM))
x.obs
p.value = 1-pnorm(x.obs,0,1)
p.value

# Pravostrann� intervalov� odhad
alpha = 0.05
p = (x.PP + x.MM)/(n.PP + n.MM)
s.error = qnorm(1-alpha/2)*sqrt(p*(1-p)*(1/n.MM + 1/n.PP))
dolni.mez = (p.PP - p.MM) - s.error
dolni.mez

# Dle v�b�rov�ho �et�en� o�ek�v�me, �e pravd�podobnost v�roby vadn� sou��stky
# je u firmy PP cca o 3 % v�t�� ne� u firmy MM. Na z�klad� 95% Waldova pravostrann�ho intervalov�ho odhadu
# (-0,028; 1,000) lze pozorovan� rozd�l v kvalit� v�roby ozna�it za statisticky nev�znamn�. Ke stejn�m z�v�r�m
# m��eme doj�t i na z�klad� Waldova pravostrann�ho testu (x_obs = 0,986,
# p-hodnota = 0,162).


## Pearson�v X2 test #######################################################
## H0: pi.PP = pi.MM
## Ha: pi.PP > pi.MM

prop.test(c(x.PP,x.MM),c(n.PP,n.MM),alternative="greater",conf.level=0.95)

# Dle v�b�rov�ho �et�en� o�ek�v�me, �e pravd�podobnost v�roby vadn� sou��stky
# je u firmy PP cca o 3 % v�t�� ne� u firmy MM. Na z�klad� 95% Clopperova - Pearsonova pravostrann�ho intervalov�ho odhadu
# (-0,025; 1,000) lze pozorovan� rozd�l v kvalit� v�roby ozna�it za statisticky nev�znamn�. Ke stejn�m z�v�r�m
# m��eme doj�t i na z�klad� Pearsonova pravostrann�ho testu (x_obs = 0,659,
# df = 1, p-hodnota = 0,209).

################################# Pro z�jemce #########################
################# Jin� grafick� interpretace k p��kladu 3 #############
#######################################################################

# Bodov� graf dopln�n� o p��mku y = x
plot(osmolalita$o8,osmolalita$o11,
     xlab = "osmolalita v 8 h (mmol/kg)",
     ylab = "osmolalita v 11 h (mmol/kg)")
abline(0,1,col = "red")

# Krabicov� graf pro p�rov� data
s = seq(length(osmolalita$o8)) #posloupnost 1,2,...,rozsah dat 
par(mfrow = c(1,1),bty="l") #bty - nastaven� ohrani�en� grafu (jsou viditeln� pouze osy x,y, nikoliv r�me�ek kolem grafu)
boxplot(osmolalita$o8,osmolalita$o11,
        ylab="osmolalita (mmol/kg)",
        names=c("8 hodin","11 hodin"),
        cex.main = 0.9)
stripchart(list(osmolalita$o8,osmolalita$o11),
           vertical=T,
           pch=16,
           cex=0.7,
           add=T) # p�id�n� bodov�ho grafu do boxplotu
#vykreslen� spojnic mezi datov�mi p�ry
x0 = rep(1,length(osmolalita$o8))
y0 = osmolalita$o8
x1 = rep(2,length(osmolalita$o11))
y1 = osmolalita$o11
segments(x0,y0,x1,y1,
         col=1,
         lwd=0.5)

#Graf rozd�l� p�rov�ch dat dopln�n� o bodov� a intervalov� odhad st�edn� hodnoty,
# resp. medi�nu, n�rustu osmolality mezi 8 h a 11 h

#Intervalov� odhad st�edn� hodnoty (t.test) nebo medi�nu (wilcox.test)
res = t.test(osmolalita$narust,conf.level=0.95)
#res = wilcox.test(osmolalita$narust,conf.level=0.95,conf.int = T)

par(mfrow = c(1,1),mar = c(2,7,2,2))
stripchart(osmolalita$narust,
           vertical=T,
           pch=16,
           method="jitter",
           main="Zm�na osmolality mezi 8 h a 11 h (mmol/kg)",
           ylab="Zm�na osmolality (mmol/kg):\n11 h - 8 h",
           cex.main = 0.8)
points(1,res$estimate,col="red",pch=16,cex=1.2) # bod definuj�c� medi�n
arrows(1,res$conf.int[1],
       1,res$conf.int[2],
       col="red",
       code=3, # definuje styl "�ipek"
       lwd=2,
       angle=90,
       length = 0.1) # definuje d�lku krajn�ch �se�ek
abline(h=0,lty=2) # p��mka definuj�c� nulov� efekt

########################################################
######## Bland�v - Altman�v graf #######################
########################################################

BA_plot = function(x,y,x.lab = "(x + y)/2",y.lab = "y - x",pomer = F){
  # x ... m��en� 1
  # y ... m��en� 2
  # x.lab, y.lab ... popisky os
  # pomer ... TRUE/FALSE ur�uje, zda m� b�t na vertik�ln� osu vyn�en pom�r y/x
  
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
    prumer = exp(prumer_ln) # geometrick� pr�m�r pom�r�
  } else {
    prumer = prumer_diff  # pr�m�r rozd�l�
  }
  
  # Interval, v n�m� rozd�ly le�� s pravd�podobnost� 95 %, tj. limity shody
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
  
  # Definice vykreslovan�ch hodnot dle pomer
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
  
  # Definice nulov�ho efektu
  if (pomer) {
    e0 = 1
  } else {
    e0 = 0
  }
  
  abline(h=e0,lty = 2, col = "red") # p��mka definuj�c� nulov� efekt
  abline(h=prumer,lty = 2) # p��mka definuj�c� pr�m�r
  
  # Limity shody
  # p��mka definuj�c� horn� mez intervalu, v n�m� rozd�ly le�� s 95% p-st�
  abline(h = ub,lty = 3) 
  # p��mka definuj�c� doln� mez intervalu, v n�m� rozd�ly le�� s 95% p-st�
  abline(h=lb,lty = 3) 
  
  print(paste0("Limity shody: (",round(lb,2),"; ",round(ub,2),")"))
  print(paste0("Pr�m�r: ",round(prumer,2)))
}

########################################################

# Aplikace funkce BA_plot
par(mfrow = c(1,1),mar = c(5,5,2,2))
BA_plot(osmolalita$o8,osmolalita$o11,pomer = F,
        x.lab = "pr�m�r osmolality v 8 a v 11 hodin (mmol/kg)",
        y.lab = "n�r�st osmolality \nmezi 8 a 11 hodinou (mmol/kg)")

# Aplikace funkce BA_plot na logaritmovan� data
par(mfrow = c(1,1),mar = c(5,5,2,2))
BA_plot(log(osmolalita$o8),log(osmolalita$o11),
        x.lab = "pr�m�r p�irozen�ch logaritm� osmolality v 8 a v 11 hodin \n(mmol/kg)",
        y.lab = "rozd�l logaritm� osmolality \nv 8 a v 11 hodin (mmol/kg)")

# Aplikace funkce BA_plot pro z�vislost pom�r� m��en� ku geometrick�mu pr�m�ru m��en�
BA_plot(osmolalita$o8,osmolalita$o11,pomer = T,
        x.lab = "geometrick� pr�m�r osmolality v 8 a v 11 hodin \n(mmol/kg)",
        y.lab = "pom�r osmolality v 11 hodin \nku osmolalit� v 8 hodin")

