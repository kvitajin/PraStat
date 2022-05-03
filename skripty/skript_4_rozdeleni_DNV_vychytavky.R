# ......................................................................................
# ...............Cvičení 5 - Vybraná rozdělení diskrétní náhodné veličiny...............
# ..................Martina Litschmannová, Adéla Vrtková, Michal Béreš..................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

#  Přehled rozdělení a jejich funkcí ####
# * Úvod: Pravděpodobnostní, kumulativní pravděpodobnostní a distribuční) a Kvantilová ####
# funkce
# ** Pravděpodobnostní funkce ####
# - začíná písmenkem **d**: P(X = x): d...(x, ...)
#
# ** Kumulativní pravděpodobnostní funkce ####
# - začíná písmenkem **p**: P(X<=x): p...(x, ...)
# - někteří autoři takto definují distribuční funkci
#
# ** Distribuční funkce ####
# - nutno dopočíst: F(x) = P(X<x) = P(X<=x)-P(X=x) nebo
# - v případě vybraných rozdělení DNV: F(x) = P(X<x) = P(X<=x-1)
#
# ** Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
# - začíná písmenkem **q**: q...(p,...)
#
# ** Kvantilová funkce (inverze dist. fce) pro vybraná rozdělení DNV ####
# - hledá nejmenší x, pro které je F(x)>=p (pro dané p),
#   tj. nejmenší x, pro které je P(X<x)>=p
# - funkce začínající písmenkem **q** zvýšená o 1: q...(p,...) + 1

# * Ukázka modelování binomické náhodné veličiny X~Bi(n = 21, π = 0.5) ####

# Binomické rozdělení: X~Bi(n, π)
# - počet úspěchů v n Bernoulliho pokusech
# - v každém pokusu je pravděpodobnost úspěchu π

# parametry rozdělení
n = 21   # rozsah výběru
p = 0.5  # pravděpodobnost úspěchu

# ** Výpočet P(X = x) ####
x = 10   # hodnota, pro níž hledáme p-stní funkci
dbinom(x, n, p)

dbinom(3.2, n, p) # ukázka výpočtu p-stní funkce v bodě,
# v němž je p-stní funkce nulová

options(warn=-1) # tímto se dají vypnout warningy

dbinom(3.2, n, p)

options(warn=0) # tímto se dají warningy zase zapnout

# ** Výpočet a graf pravděpodobnostní funkce ####
x = 0:n # nastavení možných hodnot x (minimálně 0, maximálně n)
P_x = dbinom(x, n, p)
P_x
plot(x, P_x)
grid()

# ** Výpočet P(X <= x) ####
x = 10   # hodnota, pro níž hledáme kumulativní p-stní funkci
pbinom(x, n, p)

# ** Výpočet F(x) = P(X < x) ####
x = 10   # hodnota, pro níž hledáme distribuční funkci
pbinom(x, n, p) - dbinom(x, n, p)
# nebo
pbinom(x - 1, n, p)

# ** Výpočet a graf distribuční funkce ####
x = 0:n # nastavení možných hodnot x (minimálně 0, maximálně n)
P_x = dbinom(x, n, p) # pravděpodobnostní funkce
F_x = cumsum(P_x)
plot(x, F_x, type='s')
grid()

# nebo
x = seq(0, 21, 0.01) # minimálně 0, maximálně n
F_x = pbinom(x, n, p) - dbinom(x,n,p)
plot(x, F_x, cex=0.3)
grid()


# Zkontrolujeme korektnost na okolí x = 10
x = seq(9.9, 10.1, 0.01) # minimálně 0, maximálně n
options(warn=-1)
F_x = pbinom(x, n, p) - dbinom(x, n, p)
options(warn=0)
plot(x, F_x, cex=0.5)
grid()

# ** Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
q = 0.7
qbinom(q, n, p)

# Ověření správnosti řešení
pbinom(11, n, p)
pbinom(12, n, p)


# ** Kvantilová funkce ####
# tj. nalezení nejmenšího x, pro které platí F(x)>=q (pro dané q)
q = 0.7   # pravděpodobnost pro kterou hledáme kvantil

qbinom(q, n, p) + 1

# Ověření správnosti řešení
pbinom(12, n, p) - dbinom(12, n, p)
pbinom(13, n, p) - dbinom(13, n, p)

# * Ukázka modelování hypergeometrické náhodné veličiny X~H(N = 20,M = 5, n = 10) ####

# Hypergeometrické rozdělení: X ~ H(N, M, n)
# - počet úspěchů v n závislých pokusech
# - závislost typu:
#  - Je dáno N objektů,
#  - z toho M objektů se zadanou vlastností,
#  - provádíme výběr (bez vracení) o rozsahu n
# Pro výběr bez vracení je charakteristické, že p-st výběru objektu s danou vlastností se
# s každým dalším vybraným objektem mění
# - Funkce v R bere jako parametry *hyper(x, M, N - M, n)**
#  - x je počet úspěchů pro které počítáme pravděpodobnost,
#  - M je počet objektů se zadanou vlastností,
#  - N-M je počet objektů bez zadané vlastnosti,
#  - n je rozsah výběru.

# parametry rozdělení
N = 20  # celkový počet objektů
M = 5   # z toho se zadanou vlastností
n = 10  # rozsah výběru


# ** Výpočet P(X = x) ####
x = 5   # hodnota, pro níž hledáme p-stní funkci
dhyper(x, M, N - M, n)

# ** Výpočet a graf pravděpodobnostní funkce ####
x = 0:min(M,n) # nastavení možných hodnot x (minimálně 0, maximálně min(M, n))
P_x = dhyper(x, M, N - M, n)
P_x
plot(x, P_x)
grid()

# ** Výpočet P(X <= x) ####
x = 5   # hodnota, pro níž hledáme kumulativní p-stní funkci
phyper(x, M, N - M, n)

# ** Výpočet F(x) = P(X < x) ####
x = 5   # hodnota, pro níž hledáme distribuční funkci
phyper(x, M, N - M, n) - dhyper(x, M, N - M, n)
# nebo
phyper(x-1, M, N - M, n)

# ** Výpočet a graf distribuční funkce ####
x = 0:min(M,n) # nastavení možných hodnot x (minimálně 0, maximálně min(M, n))
P_x = dhyper(x, M, N - M, n) # pravděpodobnostní funkce
F_x = cumsum(P_x)
plot(x, F_x, type='s')
grid()

# nebo
x = seq(0, min(n,M), 0.01) # minimálně 0, maximálně n
F_x = phyper(x, M, N - M, n) - dhyper(x, M, N - M, n)
plot(x, F_x, cex=0.3)
grid()

# ** Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
q = 0.7
qhyper(q, M, N - M, n)

# ** Kvantilová funkce ####
# tj. nalezení nejmenšího x, pro které platí F(x)>=q (pro dané q)
q = 0.7   # pravděpodobnost pro kterou hledáme kvantil

qhyper(q, M, N - M, n) + 1

# * Ukázka modelování negativně binomické náhodné veličiny X~NB(k = 5, π = 0.3)$ ####

# Negativně binomické rozdělení
# - počet nezávislých pokusů do k. úspěchu (včetně)
# - tj. v každém pokusu je p-st úspěchu π
# - POZOR! Negativně binomická NV je v R definována jako
#   počet neúspěchů před k. úspěchem, proto
#   P(X = x) budeme určovat jako dnbinom(x-k,k,p), atd.

# parametry rozdělení
k = 5     # požadovaný počet úspěchů
p = 0.3   # p-st úspěchu v jednotlivých pokusech

# ** Výpočet P(X = x) ####
x = 15   # hodnota, pro níž hledáme p-stní funkci
# pozor první argument musí být počet neúspěchů, nikoliv celkový počet pokusů,
# tj. x - k, nikoliv x
dnbinom(x - k, k, p)

# ** Výpočet a graf pravděpodobnostní funkce ####
max = 40  # odhadnutý maximální počet pokusů (teoreticky nekonečno)
x = k:max # nastavení možných hodnot x (minimálně k, maximálně nekonečno)
P_x = dnbinom(x - k, k, p)
P_x
plot(x, P_x)
grid()

# ** Výpočet P(X <= x) ####
x = 15   # hodnota, pro níž hledáme kumulativní p-stní funkci
pnbinom(x - k, k, p)

# ** Výpočet F(x) = P(X < x) ####
x = 15   # hodnota, pro níž hledáme distribuční funkci
pnbinom(x - k, k, p) - dnbinom(x - k, k, p)
# nebo
pnbinom(x - k - 1, k, p)


#ukol3.1
x=1:10
dat=dnbinom(x-2, 2,0.83)
plot(x, dat)
grid()
x=3
datb=1-pnbinom(x-2, 2, 0.83)
plot(x, datb)
grid()
#3.2
1-phyper(84, 300*0.9,300-(300*0.9), 100)
1-phyper(84, 300*0.9,300-(300*0.9), 100)
pnbinom(5-2, 2, 0.83) - pnbinom(2-2, 2, 0.83)
(1-phyper(89, 300*0.9, 300-(300*0.9), 100)) /(1-phyper(79, 300*0.9, 300-(300*0.9), 100))
1-pbinom(84, 100, 0.9)
1-pbinom(4, 10,  0.83)

1-pbinom(84, 100, 0.9)
#4
x=(85-3*12):(85+3*12)

hustota= function (x){
  dnorm(x, mean = 85, sd=12)
}
df = function (x){
  pnorm(x, mean = 85, sd=12)
}
plot(x, hustota(x), type = "l", xlab = "krevni tlak (mmHg)")
plot(x, df(x), type = "l", xlab = "krevni tlak (mmHg)")

1-pnorm(90, mean = 85, sd=12)
y=df(x)
polygon(c(90, x[x > 90], max(x)), c(0, y[x > 90], 0), col = "gray")

xfit = seq(min(dataS$kap5), max(dataS$kap5), length = 40)     # generování hodnot pro osu x
yfit = dnorm(xfit, mean = mean(dataS$kap5), sd = sd(dataS$kap5))  # generování hodnot pro osu y
lines(xfit, yfit, col = "black", lwd = 2)

x=(85-3*12):(85+3*12)
df = function (x){
  pnorm(x, mean = 85, sd=12)
}
plot(x, df(x), type = "l", xlab = "krevni tlak (mmHg)")
dec=qnorm(0.8,mean = 85,sd = 12)

polygon(c(min(x), x[x < dec], dec),c(0, y[x < dec], 0), col = "gray")
#dal ze skoly



pnbinom(5-2, 2, 0.83) - pnbinom(3-2, 2, 0.83)
pnbinom(10-4, 4, 0.83)
# ** Výpočet a graf distribuční funkce ####


max = 40  # odhadnutý maximální počet pokusů (teoreticky nekonečno)
x = k:max # nastavení možných hodnot x (minimálně k, maximálně nekonečno)
P_x = dnbinom(x - k, k, p) # pravděpodobnostní funkce
F_x = cumsum(P_x)
plot(x, F_x, type='s')
grid()

# nebo
x = seq(k, max, 0.01) # minimálně k, maximálně nekonečno
F_x = pnbinom(x - k, k, p) - dnbinom(x - k, k, p)
plot(x, F_x, cex=0.3)
grid()

# ** Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
q = 0.7
qnbinom(q, k, p) + k

# ** Kvantilová funkce ####
# tj. nalezení nejmenšího x, pro které platí F(x)>=q (pro dané q)
q = 0.7   # pravděpodobnost pro kterou hledáme kvantil
qnbinom(q, k, p) + k + 1

# * Ukázka modelování Poissonovy náhodné veličiny X~Po(λt = 10)$ ####

# Poissonovo rozdělení
# - počet událostí v Poissonově procesu v uzavřené oblasti (v čase, na ploše, v objemu)
# - proces musí splňovat podmínky ordinarity, stacionarity a beznáslednosti
# - parametr λt je roven střední hodnotě modelované NV

# parametry rozdělení
lt = 10     # λt je průměrný počet událostí v Poissonově procesu v uzavřené oblasti (v čase, na ploše, v objemu)

# ** Výpočet P(X = x) ####
x = 15   # hodnota, pro níž hledáme p-stní funkci
dpois(x, lt)

# ** Výpočet a graf pravděpodobnostní funkce ####
max = 30  # odhadnutý maximální počet události (teoreticky nekonečno)
x = 0:max # nastavení možných hodnot x (minimálně 0, maximálně nekonečno)
P_x = dpois(x, lt)
P_x
plot(x, P_x)
grid()

# ** Výpočet P(X <= x) ####
x = 15   # hodnota, pro níž hledáme kumulativní p-stní funkci
ppois(x, lt)

# ** Výpočet F(x) = P(X < x) ####
x = 15   # hodnota, pro níž hledáme distribuční funkci
ppois(x, lt) - dpois(x, lt)
# nebo
ppois(x - 1, lt)

# ** Výpočet a graf distribuční funkce ####
max = 30  # odhadnutý maximální počet pokusů (teoreticky nekonečno)
x = 0:max # nastavení možných hodnot x (minimálně 0, maximálně nekonečno)
P_x = dpois(x, lt) # pravděpodobnostní funkce
F_x = cumsum(P_x)
plot(x, F_x, type='s')
grid()

# nebo
x = seq(0, max, 0.01) # minimálně 0, maximálně nekonečno
F_x = ppois(x, lt) - dpois(x, lt)
plot(x, F_x, cex=0.3)
grid()

# ** Nalezení nejmenšího x, pro které platí P(X <= x)>=q (pro dané q) ####
q = 0.7
qpois(q, lt)

# ** Kvantilová funkce ####
# tj. nalezení nejmenšího x, pro které platí F(x)>=q (pro dané q)
q = 0.7   # pravděpodobnost pro kterou hledáme kvantil
qpois(q, lt) + 1

#  Příklady ####
# * Příklad 1. ####
# Bridž se hraje s 52 bridžovými kartami, které se rozdají mezi 4 hráče. Vždy 2 hráči
# hrají spolu. Při rozdávání (13 karet) jste dostali do rukou 2 esa. Jaká je
# pravděpodobnost, že váš partner bude mít zbývající dvě esa?


# X ... počet es mezi 13 kartami
# X ~ H(N = 39, M = 2, n = 13)
# parametry rozdělení
M = 2
N = 39 # 52-13
n = 13
# P(X = 2)
dhyper(2, M, N - M, n) # dhyper(2,2,37,13)

# graf pravděpodobnostní funkce
x = 0:M   # všechny možné realizace NV X
P_x = dhyper(x, M, N - M, n) # hodnoty pravděpodobnostní funkce pro x
plot(x, P_x)

# * Příklad 2. ####
# Pokusy se zjistilo, že radioaktivní látka vyzařuje během 7,5 s průměrně 3,87
# α-částice. Určete pravděpodobnost toho, že za 1 sekundu vyzáří tato látka alespoň
# jednu α-částici.

# X ... počet vyzářených alfa částic během 1 s
# X ~ Po(lt = 3.87/7.5)

# parametry rozdělení
lt = 3.87/7.5 # průměrný počet vyzářených alfa částic během 1 s

# P(X >= 1) = P(X > 0) = 1 - P(X <= 0)
1 - ppois(0, lt)

# graf pravděpodobnostní funkce
# teoreticky může být vyzářeno až nekonečně mnoho částic,
# od jisté hodnoty je pravděpodobnost zanedbatelná
max = 10 # teoreticky nekonečno
x = 0:max
P_x = dpois(x, lt) # hodnoty pravděpodobnostní funkce pro definovaná x
plot(x, P_x)

# * Příklad 3. ####
# Kamarád vás pošle do sklepa, abyste donesl(a) 4 lahvová piva - z toho dvě desítky a
# dvě dvanáctky. Nevíte, kde rozsvítit, proto vezmete z basy poslepu 4 láhve. S jakou
# pravděpodobností jste vyhověl(a), víte-li, že v base bylo celkem 10 desítek a 6
# dvanáctek?

# X ... počet 10°piv mezi 4 vybranými
# X ~ H(N = 16, M = 10, n = 4)

# parametry rozdělení
N = 16
M = 10
n = 4

# P(X = 2)
dhyper(2, M, N - M, n)

# graf pravděpodobnostní funkce
x = 0:4    # všechny možné realizace NV X
P_x = dhyper(x, M, N - M, n) # hodnoty pravděpodobnostní funkce pro definovaná x
plot(x, P_x)

# * Příklad 4. ####
# V jednom mililitru určitého dokonale rozmíchaného roztoku se v průměru nachází 15
# určitých mikroorganismů. Určete pravděpodobnost, že při náhodném výběru vzorku o
# objemu 1/2 mililitru bude ve zkumavce méně než 5 těchto mikroorganismu.

# X ... počet mikroorganismů v 0.5 ml roztoku
# X ~ Po(lt = 15/2)

# parametry rozdělení
lt = 15/2   # průměrný počet mikroorganismů v 0.5 ml roztoku

# P(X < 5) = P(X <= 4) = P(X <= 5) - P(X = 5)
ppois(4, lt)
# nebo
ppois(5,lt) - dpois(5,lt)

# graf pravděpodobnostní funkce
# teoreticky může být v roztoku až nekonečně mnoho mikroorganismů,
# od jisté hodnoty je pravděpodobnost zanedbatelná
max = 20 # teoreticky nekonečno
x = 0:max
p = dpois(x, lt) # hodnoty pravděpodobnostní funkce pro definovaná x
plot(x, p)

# * Příklad 5. ####
# Na stůl vysypeme 15 mincí. Jaká je pravděpodobnost, že počet mincí ležících lícem
# nahoře, je od 8 do 15?

# X ... počet mincí, které padnou lícem nahoru z celkového množství 15 mincí
# X ~ Bi(n = 15, p = 0.5)

# parametry rozdělení
n = 15
p = 0.5

# P(8 <= X <= 15) = P(X <= 15) - P(X < 8) = P(X <= 15) - P(X <= 7)
pbinom(15, n, p) - pbinom(7, n, p)

#jinak: P(8<=X<=15)=P(X>=8)= P(X>7)=1-P(X<=7) (podmínka X<=15 je zřejmá)
1 - pbinom(7, n, p)

# graf pravděpodobnostní funkce
x = 0:n    # všechny možné realizace NV X
P_x = dbinom(x, n, p) # hodnoty pravděpodobnostní funkce pro x
plot(x, P_x)

# * Příklad 6. ####
# Pravděpodobnost, že se dovoláme do studia rozhlasové stanice, která právě vyhlásila
# telefonickou soutěž je 0,08. Jaká je pravděpodobnost, že se dovoláme nejvýše na 4.
# pokus?

# X ... počet pokusů než se dovoláme do rozhlasového studia
# X ~ NB(k = 1,p = 0.08)

# parametry rozdělení
k = 1
p = 0.08

# P(X <= 4)
pnbinom(4 - k, k, p)

# graf pravděpodobnostní funkce
# teoreticky můžeme uskutečnit až nekonečně mnoho pokusů,
# od jisté hodnoty je pravděpodobnost zanedbatelná
max = 40 # teoreticky nekonečno
x = k:max
p = dnbinom(x - k, k, p) # hodnoty pravděpodobnostní funkce pro x
plot(x, p)

# * Příklad 7. ####
# V továrně se vyrobí denně 10 % vadných součástek. Jaká je pravděpodobnost, že
# vybereme-li třicet součástek z denní produkce, tak nejméně dvě budou vadné?

# X ... počet vadných součástek ze 30 vybraných
# X ~ Bi(n = 30, p = 0.1)

# parametry rozdělení
n = 30
p = 0.1

# P(X >= 2) = 1 - P(X < 2) = 1 - P(X <= 1)
1 - pbinom(1, n, p)

# nebo P(X >= 2) = 1 - (P(X=0) + P(X=1))
1 - (dbinom(0, n, p) + dbinom(1, n, p))

# graf pravděpodobnostní funkce
x = 0:n   # všechny možné realizace NV X
P_x = dbinom(x, n, p) # hodnoty pravděpodobnostní funkce pro x
plot(x,P_x)

# * Příklad 8. ####
# Ve skladu je 200 součástek. 10 % z nich je vadných. Jaká je pravděpodobnost, že
# vybereme-li ze skladu třicet součástek, tak nejméně dvě budou vadné?

# X ... počet vadných součástek ze 30 vybraných z 200
# X ~ H(N = 200, M = 20, n = 30)

# parametry rozdělení
N = 200
M = 20
n = 30

# P(X >= 2) = 1 - P(X < 2) = 1 - P(X <= 1)
1 - phyper(1, M, N - M, n)

# graf pravděpodobnostní funkce
x = 0:min(M,n)   # všechny možné realizace NV X
P_x = dhyper(x, M, N - M, n) # hodnoty pravděpodobnostní funkce pro x
plot(x, P_x)

# * Příklad 9. ####
# V určité firmě bylo zjištěno, že na 33 % počítačů je nainstalován nějaký nelegální
# software. Určete pravděpodobnostní a distribuční funkci počtu počítačů s nelegálním
# softwarem mezi třemi kontrolovanými počítači.

# X ... počet počítačů s nelegálním softwarem ze 3 kontrolovaných
# X ~ Bi(n = 3,p = 0.33)

# parametry rozdělení
n = 3
p = 0.33

# pravděpodobnostní funkce
x = 0:n  # všechny možné realizace NV X
P_x = dbinom(x, n, p) # hodnoty pravděpodobnostní funkce pro x

P_x = round(P_x, 3) # zaokrouhlení pravděpodobností na 3 des. místa
P_x[4] = 1 - sum(P_x[1:3]) # dopočet poslední hodnoty do 1

tab = rbind(x, P_x) # vytvoření tabulky pravděpodobnostní funkce
rownames(tab) = c("x", "P(x)")
tab

# graf pravděpodobnostní funkce
plot(x, P_x)

#distribuční funkce
cumsum(P_x) # zjednodušený výpis distribuční funkce

# * Příklad 10. ####
# Sportka je loterijní hra, v níž sázející tipuje šest čísel ze čtyřiceti devíti, která
# očekává, že padnou při budoucím slosování. K účasti ve hře je nutné zvolit alespoň
# jednu kombinaci 6 čísel (vždy 6 čísel na jeden sloupec) a pomocí křížků tato čísla
# označit na sázence společnosti Sazka a.s. do sloupců, počínaje sloupcem prvním.
# Sázející vyhrává v případě, že uhodne alespoň tři čísla z tažené šestice čísel. Jaká
# je pravděpodobnost, že proto, aby sázející vyhrál, bude muset vyplnit:

# ** a) ####
# právě tři sloupce,

# X … počet sloupců, které bude muset sázející vyplnit, aby vyhrál
# X ~ NB(k = 1, p = pp) pp neznáme, musíme je určit

# Nejdříve musíme určit p-st, že vyhrajeme při sázce v jednom sloupci (pp)

# Y ... počet uhádnutých čísel z 6 tažených ze 49
# Y ~ H(N = 49, M = 6, n = 6)

# parametry rozdělení
N = 49
M = 6
n = 6

# P-st uhádnutí alespoň 3 čísel v jednom sloupci
# P(Y >= 3) = 1 - P(Y < 3) = 1 - P(Y <= 2)
pp = 1 - phyper(2, M, N - M, n)
pp

# dále můžeme přistoupit k vlastnímu řešení zadané úlohy
# X ~ NB(k = 1, p = pp)

# parametry rozdělení
k = 1
p = pp

# P(X = 3)
dnbinom(3 - k, k, pp)

# ** b)  ####
# alespoň 5 sloupců,


# b) P(X >= 5) = 1 - P(X < 5) = 1 - P(X <= 4)

1 - pnbinom(4 - k, k, pp)

# ** c) ####
# méně než 10 sloupců,


# c) P(X < 10) = P(X <= 9)
pnbinom(9 - k, k, pp)

# ** d) ####
# více než 5 a nejvýše 10 sloupců?


# P(5 < X <= 10) = P(X <= 10) - P(X <= 5)
pnbinom(10 - k, k, pp)-pnbinom(5 - k, k, pp)