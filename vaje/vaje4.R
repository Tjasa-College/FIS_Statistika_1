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

#Osamelce imamo od 16k naprej, kar pomeni da so zaposleni z večjimi plačami od 16k
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



































