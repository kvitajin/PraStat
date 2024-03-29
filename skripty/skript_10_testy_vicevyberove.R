#######################################################################################
########################### V�cev�b�rov� testy ########################################
######################### Martina Litschmannov�, Ad�la Vrtkov� ########################
#######################################################################################

################## P�iklad 1. (Dopln�n� tabulky ANOVA) ################################
#######################################################################################

n=c(40,40,42)          # rozsahy v�b�r�
prum=c(300,290,310)    # pr�m�ry v jednotliv�ch skupin�ch / t��d�ch
s=c(33,34,31)          # sm�rodatn� odchylky v jednotliv�ch skupin�ch / t��d�ch
n.total=sum(n)         # celkov� rozsah v�b�r�
k=3                    # po�et t��d
df.b=k-1               # po�et stup�� volnosti - meziskupinov�
df.e=n.total-k         # po�et stup�� volnosti - rezidu�ln�

# celkov� pr�m�r
prum.total=weighted.mean(prum,n)
prum.total

# meziskupinov� sou�et �tverc�
ss.b=sum(n*(prum-prum.total)^2)
ss.b

# rezidu�ln� sou�et �tverc�
ss.e=sum((n-1)*s^2)
ss.e

# celkov� sou�et �tverc�
ss.b+ss.e

# rozptyl mezi skupinami / t��dami
ms.b=ss.b/df.b
ms.b

# rozptyl uvnit� skupin / t��d
ms.e=ss.e/df.e
ms.e

# F-pom�r
F=ms.b/ms.e
F

# p-hodnota
p=1-pf(F,df.b,df.e)
p

# Na hladin� v�znamnosti 0,05 zam�t�me hypot�zu o shod� st�edn�ch hodnot (p-hodnota=0,024, ANOVA), 
# tj. st�edn� hodnoty alespo� jedn� dvojice skupin se statisticky v�znamn� li��.

# odhady skupinov�ch efekt�
efekt=prum-prum.total
efekt

# Oproti celkov�mu pr�m�ru vykazuje nejv�ce podpr�m�rn� v�sledky skupina 2 
# (o cca 10 jednotek ni��� ne� celkov� pr�m�r). Naopak pr�m�r skupiny 3 je 
# o cca 10 jednotek vy��� ne� celkov� pr�m�r. Pr�m�rn� v�sledky skupiny 1 
# odpov�daj� celkov�mu pr�m�ru.

################## P�iklad 2. (Kyselina listov�) ######################################
#######################################################################################
setwd("C:/Users/Martina/OneDrive - V�B-TU Ostrava/V�uka/Pravd�podobnost a statistika/DATA/aktualni/")
library(readxl)
kysel = read_excel("testy_vicevyberove.xlsx",
                     sheet=1)

colnames(kysel)=c("sk1","sk2","sk3")   # p�ejmenov�n� sloupc�

# p�evod do standardn�ho datov�ho form�tu
kysel.s=stack(kysel)
colnames(kysel.s)=c("hodnoty","skupina")
kysel.s=na.omit(kysel.s)

par(mfrow = c(1,1))
boxplot(hodnoty~skupina,data = kysel.s)
# nebo
boxplot(kysel)

# Data neobsahuj� odlehl� pozorov�n�.

# Ov��en� normality
skupiny = c("sk1","sk2","sk3")

par(mfrow=c(1,3))
for (i in 1:3){
  qqnorm(kysel.s$hodnoty[kysel.s$skupina == skupiny[i]],
         main = skupiny[i],
         xlab = "norm. teoretick� kvantily",
         ylab = "v�b�rov� kvantily")
  qqline(kysel.s$hodnoty[kysel.s$skupina == skupiny[i]])
}

library(moments)
tapply(kysel.s$hodnoty,kysel.s$skupina,skewness,na.rm=TRUE)
tapply(kysel.s$hodnoty,kysel.s$skupina,moments::kurtosis,na.rm=TRUE)-3

tapply(kysel.s$hodnoty,kysel.s$skupina,shapiro.test)

# Na hladin� v�znamnosti 0,05 nezam�t�me p�edpoklad normality.

# Informace pot�ebn� pro nastaven� zaokrouhlov�n�
tapply(kysel.s$hodnoty,kysel.s$skupina,length)# sd zaokrouhlujeme na 2 platn� cifry
tapply(kysel.s$hodnoty,kysel.s$skupina,sd)# sd a m�ry polohy zaokrouhlujeme na cel� ��sla

# Ov��en� shody rozptyl�
s2=tapply(kysel.s$hodnoty,kysel.s$skupina,var,na.rm=TRUE)
s2 # v�b�rov� rozptyly

max(s2)/min(s2)
# Dle krabicov�ho grafu a informace o pom�ru nejv�t��ho a nejmen��ho rozptyl� (<2) nep�edpokl�d�me,
# �e se rozptyly statisticky v�znamn� li��

# P�edpoklad normality nebyl zam�tnut -> Bartlett�v test
bartlett.test(hodnoty~skupina,data = kysel.s)
# nebo
bartlett.test(kysel) 

# Na hladin� v�znamnosti 0,05 nelze zam�tnout p�edpoklad o shod� rozptyl� 
# (Bartlett�v test, x_OBS = 0,878, df = 2, p-hodnota = 0,645).

# Chceme srovn�vat st�. hodnoty nez�visl�ch v�b�r� z norm�ln�ch rozd�len� se stejn�mi rozptyly -> ANOVA
# p��kaz aov() vy�aduje data ve standardn�m datov�m form�tu

vysledky=aov(hodnoty~skupina,data = kysel.s) # POZOR! Nesta�� pou��t p��kaz aov(). V�stup p��kazu mus�me ulo�it do pomocn� prom�nn�
                                             # a na tu aplikovat p��kaz summary().
summary(vysledky)  

# Na hladin� v�znamnosti 0,05 zam�t�me hypot�zu o shod� st�edn�ch hodnot (ANOVA, p-hodnota<<0,001) -> mnohon�sobn� porovn�v�n�
TukeyHSD(vysledky)

# pr�m�ry
prum=tapply(kysel.s$hodnoty,kysel.s$skupina,mean,na.rm=TRUE)
prum
# skupinov� efekty
n=tapply(kysel.s$hodnoty,kysel.s$skupina,length)
prum.total=weighted.mean(prum,n)
efekt=prum-prum.total
efekt
# Pova�ujeme-li vysok� obsah kyseliny listov� za pozitivn�, pak statisticky v�znamn� nejlep��ch
# v�sledk� dos�hli pacienti ze skupiny 1 (pr�m�rn� obsah kys. listov� o cca 27 jednotek vy��� ne�
# pr�m�rn� obsah kys. listov� v krvi v�ech testovan�ch pacient�) a statisticky v�znamn� nejhor��ch v�sledk�
# dos�hli pacienti ze skupiny 2 (pr�m�rn� obsah kys. listov� o cca 26 jednotek ni��� ne�
# pr�m�rn� obsah kys. listov� v krvi v�ech testovan�ch pacient�). Obsah kys. listov� v krvi pacient� ze skupiny 3
# odpov�d� celkov�mu pr�m�ru. V�echny t�i skupiny pacient� jsou navz�jem dle obsahu kys. listov�
# v krvi statisticky v�znamn� odli�n�.

# Dal�� ��st skriptu ji� neobsahuje tak podrobn� koment��e =)

################## P�iklad 3. (Kr�l�ci) ###############################################
#######################################################################################
library(readxl)
kralici = read_excel("testy_vicevyberove.xlsx",
                   sheet=2)

colnames(kralici)=c("viden","cesky","kalif")   # p�ejmenov�n� sloupc�

# p�evod do standardn�ho datov�ho form�tu
kralici.s=stack(kralici)
colnames(kralici.s)=c("hodnoty","skupina")
kralici.s=na.omit(kralici.s)

par(mfrow=c(1,1))
boxplot(kralici)

# Odstran�n� odlehl�ho pozorov�n�
pom=boxplot(kralici)
pom
kralici.s$hodnoty.bez=kralici.s$hodnoty
kralici.s$hodnoty.bez[kralici.s$hodnoty.bez %in% pom$out]=NA

# Krabicov� graf
boxplot(hodnoty.bez~skupina,data = kralici.s)

# Ov��en� normality
skupiny = c("viden","cesky","kalif")
nadpisy = c("V�de�sk� b�l�\nmodrook�","�esk� \nstrak��","Kalifornsk�")

par(mfrow=c(1,3))
for (i in 1:3){
  qqnorm(kralici.s$hodnoty.bez[kralici.s$skupina == skupiny[i]],
         main = nadpisy[i],
         xlab = "norm. teoretick� kvantily",
         ylab = "v�b�rov� kvantily")
  qqline(kralici.s$hodnoty.bez[kralici.s$skupina == skupiny[i]])
}

library(moments)
tapply(kralici.s$hodnoty.bez,kralici.s$skupina,skewness,na.rm=TRUE)
tapply(kralici.s$hodnoty.bez,kralici.s$skupina,moments::kurtosis,na.rm=TRUE)-3

tapply(kralici.s$hodnoty.bez,kralici.s$skupina,shapiro.test)

# Na hladin� v�znamnosti 0,05 nezam�t�me p�edpoklad normality.

# Informace pot�ebn� pro nastaven� zaokrouhlov�n�
tapply(kralici.s$hodnoty,kralici.s$skupina,length)# sd zaokrouhlujeme na 2 platn� cifry
tapply(kralici.s$hodnoty,kralici.s$skupina,sd)# sd a m�ry polohy zaokrouhlujeme na setiny (sjednocen� nap��� druhy kr�l�k�)

# Ov��en� shody rozptyl�
s2=tapply(kralici.s$hodnoty.bez,kralici.s$skupina,var,na.rm=TRUE)
s2

max(s2)/min(s2)
# Dle krabicov�ho grafu a informace o pom�ru nejv�t��ho a nejmen��ho rozptyl� (bl�zk� 2, av�ak rozsah v�b�r� < 30)
# je t잚� odhadnout, zda lze p�edpokl�dat shodu rozptyl�. Rozhodnout n�m pom��e test.

# P�edpoklad normality nebyl zam�tnut -> Bartlett�v test
bartlett.test(hodnoty.bez~skupina,data = kralici.s) 

# Na hladin� v�znamnosti 0,05 nelze zam�tnout p�edpoklad o shod� rozptyl� 
# (Bartlett�v test, x_OBS = 3,1, df = 2, p-hodnota = 0,217).

# Chceme srovn�vat st�. hodnoty nez�visl�ch v�b�r� z norm�ln�ch rozd�len� se stejn�mi rozptyly -> ANOVA
# p��kaz aov() vy�aduje data ve standardn�m datov�m form�tu

vysledky=aov(hodnoty.bez~skupina,data = kralici.s)
summary(vysledky)  

# Na hladin� v�znamnosti 0,05 zam�t�me hypot�zu o shod� st�edn�ch hodnot (p-hodnota<<0,001, ANOVA) -> mnohon�sobn� porovn�v�n�
TukeyHSD(vysledky)

# pr�m�ry
prum=tapply(kralici.s$hodnoty.bez,kralici.s$skupina,mean,na.rm=TRUE)
prum

# skupinov� efekty
# rozsahy v�b�r� bez NA hodnot (POZOR! Funkce length() po��t� i NA hodnoty.)
library(dplyr)
n=tapply(kralici.s$hodnoty.bez,kralici.s$skupina,n_distinct,na.rm = T)
prum.total=weighted.mean(prum,n)
efekt=prum-prum.total
efekt

################## P�iklad 4. (Jakost) ################################################
#######################################################################################
library(readxl)
jakost.s = read_excel("testy_vicevyberove.xlsx",
                   sheet=3)

colnames(jakost.s)=c("poradi","skupina")   # p�ejmenov�n� sloupc�

par(mfrow=c(1,1))
boxplot(poradi~skupina, data = jakost.s)

# Ov��en� normality nem� smysl prov�d�t - z povahy jde o diskr�tn� data (po�ad�)

# Informace pot�ebn� pro nastaven� zaokrouhlov�n�
tapply(jakost.s$poradi,jakost.s$skupina,length)# sd zaokrouhlujeme na 2 platn� cifry
tapply(jakost.s$poradi,jakost.s$skupina,sd)# sd a m�ry polohy zaokrouhlujeme na cel� ��sla

# Ov��en� shody rozptyl�
s2=tapply(jakost.s$poradi,jakost.s$skupina,var,na.rm=TRUE)
s2

max(s2)/min(s2)
# Dle krabicov�ho grafu a informace o pom�ru nejv�t��ho a nejmen��ho rozptyl� (<2)
# ze p�edpokl�dat shodu rozptyl�. (Kruskal�v - Wallis�v test m� v�t�� s�lu testu, jsou-li data homoskedasticitn�.)

# Jde o "po�adov�" data, nem� smysl uva�ovat o p�edpokladu normality -> Levene�v test
library(car)
leveneTest(poradi~skupina, data = jakost.s) 

jakost.s$skupina=as.factor(jakost.s$skupina)  # Star�� verze p��kazu leveneTest vy�aduje, aby "skupina" byla k�dov�na jako faktor,
                                              # nov� je v�po�et proveden, ale objevuje se doporu�en� k p�ek�dov�n� prom�nn� "skupina".
leveneTest(poradi~skupina, data = jakost.s)
# Na hladin� v�znamnosti 0,05 nelze zam�tnout p�edpoklad o shod� rozptyl� 
# (Leveneho test, x_OBS = 0,4, df_num = 3, df_denom = 62, p-hodnota = 0,750).

# Ov��en� symetrie
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
         xlab = "po�ad�",
         ylab = "�etnost")
}

# Chceme srovn�vat medi�ny nez�visl�ch v�b�r� (data nelze pova�ovat za v�b�ry z norm. rozd�len�) -> Kruskal�v-Wallis�v test
kruskal.test(poradi~skupina,data = jakost.s)

# Na hladin� v�znamnosti 0,05 nelze zam�tnout hypot�zu o shod� medi�n� (Kruskal�v-Wallis�v test, x_OBS = 3,7, df = 3, p-hodnota=0,295). 
# Tj. statisticky v�znamn� rozd�ly mezi v�robci (z hlediska po�ad� v�robk� v sout�i) neexistuj�.

################## P�iklad 5. (Trombin) ###############################################
#######################################################################################
library(readxl)
trombin.s = read_excel("testy_vicevyberove.xlsx",
                      sheet=4,
                      skip = 1)

colnames(trombin.s)=c("hodnoty","skupina")   # p�ejmenov�n� sloupc�

par(mfrow=c(1,1))
boxplot(hodnoty~skupina,data = trombin.s)

# Ov��en� normality
skupiny = c("A","B","C")

par(mfrow=c(1,3))
for (i in 1:3){
  qqnorm(trombin.s$hodnoty[trombin.s$skupina == skupiny[i]],
         main = skupiny[i],
         xlab = "norm. teoretick� kvantily",
         ylab = "v�b�rov� kvantily")
  qqline(trombin.s$hodnoty[trombin.s$skupina == skupiny[i]])
}

library(moments)
tapply(trombin.s$hodnoty,trombin.s$skupina,skewness,na.rm=TRUE)
tapply(trombin.s$hodnoty,trombin.s$skupina,moments::kurtosis,na.rm=TRUE)-3

tapply(trombin.s$hodnoty,trombin.s$skupina,shapiro.test)

# Na hladin� v�znamnosti 0,05 zam�t�me p�edpoklad normality.

# Informace pot�ebn� pro nastaven� zaokrouhlov�n�
tapply(trombin.s$hodnoty,trombin.s$skupina,length)# sd zaokrouhlujeme na 2 platn� cifry
tapply(trombin.s$hodnoty,trombin.s$skupina,sd)# sd a m�ry polohy zaokrouhlujeme na setiny (sjednocen� nap��� skupinami)

# Ov��en� shody rozptyl�
s2=tapply(trombin.s$hodnoty,trombin.s$skupina,var,na.rm=TRUE)
s2

max(s2)/min(s2)
# Dle krabicov�ho grafu a informace o pom�ru nejv�t��ho a nejmen��ho rozptyl� (>>2)
# nelze p�edpokl�dat shodu rozptyl�. Rozhodnout n�m pom��e test.

# P�edpoklad normality byl zam�tnut -> Leveneho test
library(car)
leveneTest(hodnoty~skupina,data = trombin.s) 

trombin.s$skupina=as.factor(trombin.s$skupina)
leveneTest(hodnoty~skupina,data = trombin.s)
# Na hladin� v�znamnosti 0,05 zam�t�me p�edpoklad o shod� rozptyl� 
# (Leveneho test, p-hodnota << 0,001).

# Ov��en� symetrie
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
       xlab = "trombinov� �as (s)",
       ylab = "�etnost")
}

# Chceme srovn�vat medi�ny nez�visl�ch v�b�r�, kter� nemaj� norm�ln� rozd�len� a maj� r�zn� rozptyly -> Kruskal�v - Wallis�v test

kruskal.test(trombin.s$hodnoty,trombin.s$skupina)
# Na hladin� v�znamnosti 0,05 zam�t�me hypot�zu o shod� medi�n� (p-hodnota<<0,001, Kruskal�v-Wallis�v test). Tj. trombinov� �as je statisticky v�znamn�
# ovlivn�n prepar�tem. -> mnohon�sobn� porovn�v�n�
library(dunn.test)
dunn.test(trombin.s$hodnoty,trombin.s$skupina,
          method = "bonferroni",
          alpha = 0.05,  # nastav�me-li hodnotu alfa, jsou st. v�znamn� rozd�ly ozna�eny * 
                         # (defaultn�: alpha = 0.05)
          altp = T)      # altP = T nastavuje p-hodnotu tak,aby se p�i rozhodov�n�
                         # o statistick� v�znamnosti srovn�vala s alfa (defaultn�: altp = F, pak srovn�v�me s alfa/2)

# medi�ny
med=tapply(trombin.s$hodnoty,trombin.s$skupina,quantile,prob=0.5,na.rm=TRUE)
med

# skupinov� efekty
med.total=quantile(trombin.s$hodnoty,probs=0.5)
efekt=med-med.total
efekt
