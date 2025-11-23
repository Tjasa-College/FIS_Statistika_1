1+1

x=5

##učimo se tukej samo vektorje in podatkovne okvirje
# spremenljivke označimo z vektorji
# več spremenljivk shranimo v podatkovne okvirje (bivarioantna analiza primer)
#seznam je kot vektor, samo da v seznamu ne rabi biti isti tip podatka

x=c(123, 456, 789)

# Manipuliranje podatkov

##install.packages("dplyr")
library(tidyverse)
library(dplyr)

#ko hočemo manipulirat s podatki uporabimo oznako %>%

starwars_data = starwars


##uvedemo fajl v R
getwd()
ibm_employees = read.csv("IBMHRAnalytics.csv", header= TRUE)
mesecna_placa = ibm_employees$MonthlyIncome

#kvantili, srednje vrednosti in mere variabilnosti
kvartili = quantile(mesecna_placa)

quantile(mesecna_placa, probs = 0.1) #prvi decil P=0.1
decili = quantile(mesecna_placa, probs = seq(0.1, 0.9, by=0.1)) #vsi decili s korakom za 0.1
centili = quantile(mesecna_placa, probs=seq(0.01, 0.9, by = 0.01)) # centili

mediana = median(mesecna_placa)
povprecje = mean(mesecna_placa)

vse_srednje_vrednosti = summary(mesecna_placa)

# mere variabilnosti

# 
install.packages("psych")

library(psych)
describe(mesecna_placa) #describe je v paketu psych -> naslednje podatke vse lahko dobimo iz describe:

#standardni odklon je okrog 4708, kar pomeni, da je večina zaposlenih v razponu
#mediana - st odklon in mediana + st odklon

6503 - 4708
6503 + 4708

# vidimo da imajo plače velik razpon

#skewnes = koeficient asimetrije
#prej smo predvidevali, da je asimetrična v desno
# skew = 1.37 > 0 kar pomeni, da je mesečna plača v IBM asimetrična v desno

#Kurtosis - koeficient sploščenosti
#kurtosis = 0.99 > 0 torej je koničasta


# histogram in box plot

hist(mesecna_placa, main = 'Histogram mesečne plače', xlab = 'Mesečna plača', col='pink', bins = 20) #nariše histogram in lahko exportaš s klikom na export nad histogramom
boxplot(mesecna_placa, main = 'Box plot mesečnih plač', col ='blue', ylab='Mesecna plača')
#v tem primeru vidimo, da ima največ zaposlenih plačo med 3000 in 4000 potem pa vidimo, da obstajajo nekatere zelo velike plače


##ggplot2 -> to bomo uporabili za plotanje namesto prejšnjih hist in boxplot funkcij

install.packages('ggplot2')
library(ggplot2)
ggplot(data=ibm_employees) + geom_histogram(mapping = aes(x=MonthlyIncome), colour = 'Black', fill='pink', bins = 20) +
  ggtitle('Histogram Mesečne plače') + xlab("Mesečna plača") + ylab("Število zaposlenih")

ggplot(data = ibm_employees)

#ZADNJE VAJE -> bivariantna analiza

#povezanost x in y in odvisnost Y od X

# ŠTEVILSKE SPREMENLJIVKE
starost = ibm_employees$Age;
mesecna_placa = ibm_employees$MonthlyIncome;

#Hipoteza -> s starostjo se viša plača. Pričakujemo torej pozitivno (smer), srednje močno (moč) povezanost med 
#starostjo in plačo

#X: starost (ker je starost neodvisna od plače)
#Y: plača (ker je plača odvisna od starosti)

#razsevni grafikon

library(ggplot2)
ggplot(data = ibm_employees)+
  geom_point(mapping = aes(x = Age, y = MonthlyIncome))+
  ggtitle('Razsevni grafikon')+
  xlab('Starost')+
  ylab('Placa')

##je linearna povezanost ker neko linearno premico lahko potegnemo čez

#pearsonov korelacijski koeficient

ro = cor(starost, mesecna_placa, method = 'pearson')
#ro = 0.498

#Zakljucek: med spremenljivkama starost in mesečna plača obstaja srednje močna povezanost (meji na močno) in pozitivna

#glede na našo hipotezo lahko rečemo, da smo jo potrdili

#linearna regresijska premica

#Y - plača
#X - starost

statisticni_model = lm(formula = mesecna_placa ~ starost, data=ibm_employees)

#y=a+bx
#Linearna regresijska premica je:
#mesecna placa = -2970.7 + 256.6*starost


#vriši premico v grafikon:
ggplot(data = ibm_employees, mapping = aes(x = Age, y = MonthlyIncome))+
  geom_point()+
  geom_smooth(method = 'lm')
  ggtitle('Razsevni grafikon')+
  xlab('Starost')+
  ylab('Placa')

#Zaposleni star 30 let, kakšno plačo mu napovemo glede na naš model?

-2970.7 + 256.6 * 30
  
  #kakovost modela
  summary(statisticni_model)
  
  #rezultat, ki ga iščemo: Adjusted R-squared:  0.2473 
  #standardna napaka: Residual standard error: 4084
  #Determinacijski koeficient R = 0.25 -> s pomočjo starosti lahko pojasnimo 25% varjance mesečne plače.
  #Ostalih 75% razlike prihaja iz drugih faktorjev.
  # standardna napaka modela je 4084 - kar pomeni, da pri napovedovanju plače s pomočjo
  #starosti se standardno zmotimo za +/- $4084 
  #glede na determinacijski koeficient in standardno napako sklepamo, da naš linearni model ni zanesljiv
  
  # OPISNE SPREMENLJIVKE
  
  #BUSINESS TRAVEL IN DEPARTMENT PRIMERJAMO
  
  sektor = ibm_employees$Department
  potovanja = ibm_employees$BusinessTravel
  
  #prikaz ki prikazuje frekvence in odstotke posameznih spremenljivk
  table(sektor) #frekvence
prop.table(table(sektor))*100 #*100 je zato da dobimo procente  
#dim(sektorja) = 3 in imamo 4% kadrovska, 65% raziskovalna delavnost in 30% prodaja

table(potovanja) #frekvence
prop.table(table(potovanja))*100

#dim(potovanja) = 3
# imamo 10% zaposlenih, ki nikoli ne potujejo, 19%pogosto in 71% ki redko

#histogram
ggplot(data = ibm_employees)+
  geom_bar(mapping = aes(x=BusinessTravel), color='black', fill='pink')

ggplot(data = ibm_employees)+
  geom_bar(mapping = aes(x=Department), color='black', fill='pink')

#interpretacija iz grafa: vidimo, da je večina zaposlena v R&D oddelku, kar je za pričakovati v IBM projektu
#imamo več kot 2x manj prodajalcev in najmanj zaposlenih v kadrovski službi

#kontigenčna tabela: tabela skupnih frekvenc

kontingencna_tabela = table(sektor, potovanja)

#hipoteza glede na vsebino naših spremenljivk:
# pričakujemo, da sektor in pogostost potovanja sta povezani
#ker prodajalci, ki imajo business deals pogosteje potujejo kot recimo kadrovska

#dvorazsežne strukture
#SEKTOR PO POTOVANJIH
ggplot(data = ibm_employees)+
  geom_bar(mapping = aes(x = sektor, fill = potovanja), color='black')
#vidimo:
#HR: večina jih potuje redko, nekaj jih ne potuje in nekaj jih veliko potuje
#ista situacija pri R&D in Sales

#V vseh stolpcih je ista situacija

#na sliki vidimo, da je po vseh sektorjih ista situacija
#iz tega lahko sklepamo, da potovanja in sektor NISTA povezani

ggplot(data = ibm_employees)+
  geom_bar(mapping = aes(x = sektor, fill = potovanja), color='black', position = 'fill')

#POTOVANJA PO SEKTORJIH
ggplot(data = ibm_employees)+
  geom_bar(mapping = aes(x = potovanja, fill = sektor), color='black')

ggplot(data = ibm_employees)+
  geom_bar(mapping = aes(x = potovanja, fill = sektor), color='black', position = 'fill')

#vidimo, da so stolpci identični, glede razporeditve barve, kar nakazuje, da povezanosti ni

#izračun hi kvadrat, kontingenčni in popravljen kointingenčni 

#HI^2

chisq.test(kontingencna_tabela)
#vrednost hi hvadrat = 0.2

#kontingenčni kvadrat in cpop

install.packages('grid')
install.packages('vcd')

library(grid)
library(vcd)

assocstats(kontingencna_tabela)
#dobimo Contingency Coeff.: 0.012 
#C = 0.012

C = 0.012

#ne sklepamo, ker moremo nrditi popravljen koeficient

C_pop = C / sqrt((k-1)/k)
k = 3 #najmanjša dimenzija v kontingencni tabeli
C_pop
#C_pop je 0.15, kar pomeni, da obstaja ŠIBKA (0.1 do 0.3) povezanost med sektorjem in potovanjih v
#podjetju IBM


#Primerjanje hipoteze in ugotovitvami
# naša hipoteza je bila, da sta sektor in potovanja povezano
#iz dvorazsežne strukture smo ugotoili, da povezanosti no, ker so stolpci bili enako obarvani
