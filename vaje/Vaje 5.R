#tukaj pisemo kodo

1+1

install.packages('evaluate')
library(evaluate)

install.packages('dplyr')


x = 2

x<-2

y = 6

x*y

x^2

#types

x = 'jelena'

z = TRUE


#data structures

#vektori
#matrike
#liste
#podatkovni okvirji

#Vektorji - kolekcija vrednosti istega tipa

#vektore uporabomo da oznacimo spremenljivke

starost = c(18, 20,19, 22, 25,30,35,40,33)
ime = c('nejc', 'tadeja', 'rok', 'mateja','nika','gregor','erik','jelena','doroteja')
vs = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
povprecna_ocena_iz_stat = c(9,7.5,6.9, 10, 8.3, 7.1, 8.8,9.2,9)

#podatkovni okvir (data frame, dataset)

studenti = data.frame(ime, starost, vs, povprecna_ocena_iz_stat)

#imamo 9 enot in 4 spremenljivke

View(studenti)


#filter

studenti %>% filter(vs == TRUE)
studenti %>% mutate(povprecna_ocena_pop = povprecna_ocena_iz_stat + 0.5)
studenti %>% arrange(desc(starost))


getwd()
setwd('../')
setwd('./Desktop/R excercises')

read.csv('IBMHRAnalytics.csv',header = TRUE)

ibm_employees = read.csv('IBMHRAnalytics.csv',header = TRUE)

View(ibm_employees)

str(ibm_employees)


placa = ibm_employees$MonthlyIncome
starost = ibm_employees$Age

frekventnost_potovanj = ibm_employees$BusinessTravel
izobrazba = ibm_employees$EducationField

anyNA(ibm_employees)


quantile(placa, probs = 0.5)
#Mediana je 4919, kar pomeni da 50% IBM zaposlenih ima manjšo plačo od 4919$, in 50% jih ima večjo


quantile(placa, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))


mean(placa)
#Srednja plača v IBM podjetju je okrog $6503.


install.packages('psych')
library(psych)

describe(placa)


#Razpodelitev place v podjetju IBM je asimetricna v desno in konicasta

#Minimalna placa v podjetju IBM je malo čez 1000$, dokler maksimalna pride do 20000, 50% zaposlenih ima plačo manjšo od
#4919, standardni odklon je 4708 kar pomeni da je večina zaposlenih v razponu plač od 1795 in 11211
#Koeficient asimetrije je 1.37 (>0), kar pomeni asimetricna v desno (več je zaposlenih z manjšimi plačami)
#Koeficienti sploščenosti je 0.99 (>0), kar pomeni da je razpodelitev plač koničasta


hist(placa, ylab = 'stevilo zaposlenih', col = 'red', main = 'Razpodelitev plac v IBMu (histogram)')

#Največ IBM zaposlenih ima plačo od 2000 do 3500 $, potem se 
#št zaposleih z večjimi plačami zelo zmanjša. 


#box-plot - okvir z ročaji - škatla z brki

boxplot(placa, xlab = 'Placa', col = 'blue', main = 'Kvartili za plačo zaposlenih')

help("boxplot")

#Osamelce imamo od $16k naprej, kar pomeni da so zaposleni z večjimi plačami od 16k
#izjeme
#50% zaposlenih z srednjimi plačami imajo plače od približno 4000 do 8000


install.packages('tidyverse')
install.packages("ggplot2")


#histogram


ggplot(data = ibm_employees) +
  geom_histogram(mapping = aes(x=MonthlyIncome), fill = 'red', colour = 'black', bins = 50)+
  xlab('Mesečna plača') +
  ylab('Število zaposlenih')+
  ggtitle('Histogram (razpodelitev plač)')



help("geom_histogram")


#box-plot, okvir z ročaji, grafik kvartilov

library(ggplot2)

ggplot(data = ibm_employees) +
  geom_boxplot(mapping = aes(x=MonthlyIncome)) +
  coord_flip()+
  xlab('Mesečna plača') +
  ggtitle('Okvir z ročaji za Mesečno plačo zaposlenih')


#zaposleni v 25% z največjimi plačami so bolj razpršeni med seboj, torej plače so precej različne,
#dokler v 25% z najmanjšimi plačami so zgosnjeni




ggplot(data = ibm_employees) +
  geom_histogram(mapping = aes(x=Age), fill = 'red', colour = 'black', bins = 20)+
  xlab('Starost') +
  ylab('Število zaposlenih')+
  ggtitle('Histogram (starosti)')


ggplot(data = ibm_employees) +
  geom_boxplot(mapping = aes(x=Age)) +
  coord_flip()+
  xlab('Starost') +
  ggtitle('Okvir z ročaji za Starost zaposlenih')



#Opisne spremenljivke: prikaz s stolpci (bar plot)


ggplot(data = ibm_employees) +
  geom_bar(mapping = aes(x=BusinessTravel), fill = 'red', colour = 'black')+
  xlab('Frekventnost potovanj') +
  ylab('Število zaposlenih')+
  ggtitle('Prikaz s stolpci')


ggplot(data = ibm_employees) +
  geom_bar(mapping = aes(x=EducationField), fill = 'red', colour = 'black')+
  xlab('Izobrazba') +
  ylab('Število zaposlenih')+
  ggtitle('Prikaz s stolpci')

#Največ zaposlenih v IBMu ima izobrazbo iz naravoslovnja (predvsem biologije in medicine), 
#in tehničnih ved, Nekaj manj jih je v družboslovni izobrazbe kot so marketing in kadrovska 
#služba. Za približno 80 zaposlenih nimamo podatka.



#Bivariantna analiza


#Številske - analiziramo povezanost dveh spremenljivk

#X - Age (neodvisna)
#Y - MonthlyIncome

#Pričakujemo povezanost med starostjo in plačo? Da, ker starejši ko si, dobiš višjo plačo.


#Scatterplot

ggplot(data = ibm_employees) +
  geom_point(mapping = aes(x = Age, y = MonthlyIncome))

#podatki ne nakazujejo linearno povezanost ker niso zbrani okoli ene premice

#linearna premice
ggplot(data = ibm_employees, mapping = aes(x = Age, y = MonthlyIncome)) +
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)


#Izračunaj Pearsonov koeficijent korelacije in pokaži če res obstaja povezanost 

RO_xy = cor(ibm_employees$Age, ibm_employees$MonthlyIncome, method = 'pearson')

RO_xy

#RO_xy = 0.4978546, kar pomeni da med starostjo in mesečno plačo obstaja srednje močna
#(ker je med 0.3 in 0.5), pozitivna linearna povezanost.


#Y' = a + b*X
#lm(Y~X, data = dataframe), (Intercept) je prosti koeficient, torej a

model = lm(MonthlyIncome ~ Age, data = ibm_employees)

#linearna regresijska premica: Y' = -2970.7 + 256.6*X
#Plača = -2970.7 + 256.6 * Starost


#Kakovost linearnega modela: Determinacijski koeficient R in standardna napaka modela

summary(model)

#Determinacijski koeficient R = 0.2479, kar pomeni da s pomočjo starosti lahko pojasnimo
#približno 25% variance mesečne plače. Ostalih 75% se drugi faktorji. 

#standardna napaka (se) modela je 4084, kar pomeni da pri napocedi plače glede na starost
#standardno zmotimo približno za $4084. 

#Končni zaključek glede linearnega modela: Model ni ravno zanestljiv, ker je R premajhno, dokler
#je se prevelik glede na vrednosti spremnljivke mesečna plača.
#Model ne bomo uporabljali naprej.




#Opisne 

frekventnost_potovanj = ibm_employees$BusinessTravel
izobrazba = ibm_employees$EducationField


table(frekventnost_potovanj)
table(izobrazba)


prop.table(table(frekventnost_potovanj))*100
prop.table(table(izobrazba))*100

#(bivariantna analiza) - Raziskovalno vprašanje: Ali je kakorkoli izobrazba zaposlenega v IBMu
#povezana z frenktnostjo potovanj?

#Hipoteza: Izobrazba zaposlenega je srednje močno povezana z frekventnostjo potovanj, ker
#lahko vpliva na delovno mesto, ampak ne direktno.


#kontingencna tabela: tabela skupnih frekvenc:

kont_tabela = table(frekventnost_potovanj, izobrazba)


#Strukturni odstotki (Drorazsežne strukture)

#strukturni odstotki frekventnosti potovanj po izobrazbi
ggplot(data = ibm_employees) + 
  geom_bar(mapping=aes(x = BusinessTravel, fill = EducationField), position = 'fill')

#strukturni odstotki izobrazbe po frekventnosti potovanj
ggplot(data = ibm_employees) + 
  geom_bar(mapping=aes(x = EducationField, fill = BusinessTravel), position = 'fill')

#Iz strukturnih odstotkov izobrazbe po frekventnosti potovanj sklepamo da 
#bo šibka povezanost, ker vidimo da HR ima še vedno največ tistih ki nikoli ne potujejo, v primerjavi
#y drugimi izobrazbami, dokler v marketingu je najmanj tistih ki nikoli ne potujejo 
#v primerjavi y drugimi izobrazbami


#Hi-kvadrat test

hi2 = chisq.test(kont_tabela)

#Hi kvadrat vrednost = 5.2407
#Kontigencni koeficient C


install.packages('grid')
install.packages('vcd')

library(grid)
library(vcd)

assocstats(kont_tabela)

#Contingency Coeff.: C = 0.059 


#Popravljeni kontigenčni koeficient: Cpop

#Cpop = C/sqrt((k-1)/k)

k = 3 #ker je 3 min{dim(Izobrazbe), dim{frekventnosti potovanj}} = min{6,3} = 3


c = 0.059 

c_pop = c/sqrt((k-1)/k)

c_pop

#c_pop = 0.07 kar pomeni da med izobrazbo zaposlenih in frekventnostjo potovanj NI povezanosti
# (c_pop < 0.1)

#Naša hipoteza je bila da bo povezanost srednje močna, glede na vsebino spremenljivk,
#strukturni stolci so nam povedali da bo mogoče bolj šibka, dokler po opravljeni analizi
# (hi kvadrat test) smo dobili da povezanosti ni. 













