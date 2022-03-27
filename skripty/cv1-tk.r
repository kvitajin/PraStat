# Úvod do R a  kombinatorika ----
# upraveno podle skriptu M.Béreše 
# naposled  úpraveno 22.2.2021 
# kódování UTF8
#
# ## Krátký úvod do R

#zjištění pracovního adresáře
getwd()
# například: "C:/Users/Tereza_K"

#změna pracovního adresáře
setwd("/home/kvitajin/Codes/PS/data")
# například: setwd("C:/Users/Tereza_K/Documents")

#vyvolání nápovědy: ?název funkce/objektu/aj.
?matrix

# Jednoduché početní operace ----
2+4
5/2

# POZOR na závorky! Pro počítání se používají pouze kulaté! 
# Hranaté a složené mají v R jinou funkci!
(((10+2)*(340-33))-2)/3

## kombinační číslo, faktoriály
choose(10,2)
factorial(4)

#úkol př1
15*choose(10,4)+
  choose(10,3)*choose(15,2)+
  choose(10,2)*choose(15,3)+
  10*choose(15,4)
#úkol př2
factorial(9)/4-factorial(7)

#úkol př4
1-(0.95*0.95)
0.0975*0.0975
# datové typy -> numeric, character, logical, (complex)
# funkcí class zjišťujeme typ objektu

a=2+3
class(a)

b="pismenko"
class(b)

c=(1>3)
class(c)

d=3+1i
class(d)

# Datové struktury v R ----
# 
# - vector (rozumíme sloupcový vektor)
# - factor (speciální případ vektoru)
# - matrix (matice s rozměry n x m)
# - data.frame (datový rámec s rozměry n x p)
# - list (seznam)

#* vektory ----
## definování vektoru (sloupcový=column)
a = c(3,4,6,7)
a <- c(3,4,6,7)
a[2]

#další možnosti
rep(1,4) # vytvoří vektor se čtyřmi jedničkami

seq(1,10,2) #posloupnost od 1 do 10 s krokem 2

1:10  #posloupnost od 1 do 10 s krokem 1

b=c("A","B","C","D")
b

class(b)

#předefinování objektu na jiný typ - např. as.vector, as.matrix, as.factor,...
b=as.factor(b)
b

#práce s vektory - slučování podle sloupců/řádků
c<-cbind(a,b)
c
class(c)

r<-rbind(a,b)
r
class(r)

c(a,b)

#* matice ----
## definování matice
A=matrix(c(3,4,6,7,3,2),nrow=2,ncol=3)
B=matrix(c(3,4,6,7,3,2),nrow=2,ncol=3,byrow=TRUE)
C=matrix(c(3,4,6,7,3,2),nrow=3,ncol=2)

B

B[1,3]

A[1,]

A
A[,2:3]

#diagonální matice
diag(4)

diag(4,2)

## operace s maticemi
A+B

A-B

A*B



# ## Kombinatorika
# Variace ----
# ####Začneme s variacemi.
# 
# V(n,k) - variace bez opakování, první argument bude celkový počet entit, druhý argument velikost výběru
# funkce se vytváří příkazem fucntion, je to objekt jehož jméno je dáno až proměnnou  
# do které tento objekt přiřadím
variace = function(n,k) # zde zadávám počet parametrů a jejich jména
{ # celé tělo funkce je uzavřeno mezi závorkami {...}
    citatel = factorial(n)  # faktoriál v originálním Rku existuje tak jej použijeme
    jmenovatel = factorial(n-k)
    return(citatel/jmenovatel)    # to co funkce vrátí se dává do příkazu return(...)
}

# V*(n,k) - variace s opakováním

variace_opak = function(n,k)
{
  return(n^k)
}

# Permutace ----
# 
# P(n)=V(n,n) - permutace

permutace = function(n)
{
  return(variace(n,n))
}

# P*(n1,n2,n3,....,nk) - permutace s opakováním, vstup bude vektor s jednotlivými počty unikátních entit

permutace_opak = function(vec_n) # vec_n je vektro počtů hodnot př.: vec_n = c(2,2,2,4,3)
{
    n = sum(vec_n) # spočteme kolik máme hodnot celkem
    res_temp=factorial(n) #jejich faktoriál = hodnota v čitateli
    # jednoduchý cyklus začíná příkazem for, 
    # pak v závorkách následuje název iterátoru a z 
    # jakého seznamu bude brán
    for(pocet in vec_n) # pocet je iterátor a postupně bude nabývat hodnot z vektoru vec_n
    {
        # postupně dělíme faktoriálem každého počtu unikátních entit
        res_temp=res_temp/factorial(pocet) 
    }
    return(res_temp)
}

# Kombinace ----
# 
# C(n,k) - kombinace

kombinace = function(n,k)
{
  return(choose(n,k)) # funkce for kombinace už existuje v Rku a jmenuje se choose
}

# C*(n,k) - kombinace s opakováním

kombinace_opak = function(n,k)
{
  return(choose(n+k-1,k)) # použijeme známý vzorec
}

########################################################################š
# Úlohy z 1. cvičení ----
# 

#* Příklad 1. spriteři 1 ----
# 
# Sprinteři - stupně vítězů
# 1. způsob
Pm1 = 8*7*6
Pm1
# 2. způsob
Pm2=variace(8,3)
Pm2


#* Příklad 2. sprinteři  2 ----
 # 
# Sprinteři - dráhy

Pm=factorial(8)
Pm

permutace(8)

variace(8,8)

#* Příklad 3. semínka 1 ----
# 
# Zahadnictví - Výběr pěti různých sáčků se semínky 
# 

Pm = kombinace(15,5)
Pm

variace(15,5)/factorial(5)

#* Příklad 4. semínka 2 ----
# 
# Zahadnictví - Výběr pěti sáčků se semínky - druhy sáčků se mohou opakovat 
# 

kombinace_opak(15,5)
choose(19,5)


#* Příklad 5. hesla ----
# 
# hesla délky 5 znaků

# a) bez opakování znaků
variace(26,5)

# b) s opakováním znaků
variace_opak(26,5)

# C) složený výběr
26*10^3*52*4

#

#* Příklad 6. anagramy ----
# 
# anagramy - APPROPRIATE
# 

# a) všechny 
vec = c(3,2,2,1,1,1,1)
sum(vec)
Pa=factorial(sum(vec))/(6*2*2)
Pa


# pomocí implementované funkce
permutace_opak(vec)

# b) ty co neobsahují AA

vec2 = c(3,2,1,1,1,1,1)
Paa=permutace_opak(vec2)

Pb=permutace_opak(vec)-permutace_opak(vec2)
Pb

# pravděpodobnost

PR=Paa/Pa
PR


#* Příklad 7. zmrzliny ----
# 
# 
# 
# a) Kolik je možností jak objednat točenou zmrzlinu, nabízi-li dva druhy
#    zmrzliny a dva druhy polevy.
Pa=2*3
Pa
# b) Kolik je možností jak objednat kopečkovou zmrzlinu, nabízi-li 10 druhů
#    zmrzliny a budeme vybírat dva kopečky (mohou být i stejného druhu a 
#    je jedno v jakém pořadí je vybereme.)
Pb=kombinace_opak(10,2)
Pb

# c) Kolik možností celkem máme jak si vybrat zmrzlinu u obou stánků?
Pc=Pa+Pb
Pc


#* Příklad 8. Komise ----
# 
# 
# 
# a) Počet 5-členných komisí sestavených z 9 Čechů
Pa=kombinace(9,5)
Pa

# b) Počet všech 5-členných komisí z 9-Čechů a 13-Poláků
Pb=kombinace(22,5)
Pb

# c) Komise s alespoň jedním Polským komisařem
Pc=Pb-Pa
Pc

# d) Pravděpodobnost
PD = kombinace(9,2)*kombinace(13,3)/Pb
PD

# K příkladům 9,10,11,12 prvního cičení si zkuste výpočty provést sami.


#########################################################

# konec skriptu