#Pogledam working directory in jo nastavim na mapo v kateri delam
#to je zato, da lažje kličem .csv datoteko in dobim podatke iz nje
#pomagala sem si s tem virom: 
#https://stackoverflow.com/questions/62491103/error-setwd-cannot-change-working-directory
#.csv je v istem foldru kot tale .R
getwd()
setwd(file.path("C:", "Users", "tjasa", "Desktop", "FIS_Statistika_1", "naloga"))
getwd()

# Dodeljena datoteka za analizo: FavouriteIndianFood.csv

# DODELJENE SPREMENLJIVKE:
# Opisna 1: employment_status (zaposlitveni status)
# Opisna 2: covid_vaccine (COVID cepljenje)
# Številska 1: height (višina)
# številska 2: weight (teža)

#1. OPISOVANJE IN UREJANJE PODATKOV

#a) Podatki v .csv formatu uvoženi v .R
data = read.csv("FavouriteIndianFood.csv", header = TRUE)

#b) priprava podatkov za kasnejšo uporabo:

visina = data$height #številska
teza = data$weight #številska
zaposlitveni_status = data$employment_status #opisna
cepljenje_proti_COVIDu = data$covid_vaccine #opisna

data_urejeni = data.frame(zaposlitveni_status, cepljenje_proti_COVIDu, visina, teza)

#c) spletna stran: https://www.kaggle.com/datasets/abdulraheem625/food-choice-male-vs-female-indianpakistani-food

#2. ANALIZA ŠTEVILSKIH SPREMENLJIVK

# a)Izračun glavnih opisnih statistik in interpretacija

library(psych)
#višina:
mean(visina) #srednja vrednost
median(visina) #mediana
quantile(visina, probs=c(0.25, 0.5, 0.75)) #kvartili
describe(visina) #koeficient asimetrije in sploščenosti
165.69+13.92
165.69-13.92


#teža:
mean(teza)
median(teza)
quantile(teza, probs=c(0.25, 0.5, 0.75))
describe(teza)
71.97+18.94
71.97-18.94

#b)Grafična predstavitev številskih spremenljivk (histogram in okvir z ročaji)

library(ggplot2)

#višina:

#histogram
ggplot(data = data_urejeni) +
  geom_histogram(mapping = aes(x=visina), bins = 20, color='black', fill='white') + 
  xlab("Višina anketirancev") + 
  ylab("Število anketirancev") + 
  ggtitle("Histogram - razporeditev višine")
#okvir z ročaji
ggplot(data = data_urejeni) +
  geom_boxplot(mapping = aes(x=visina)) +
  xlab("Višina anketirancev") +
  ggtitle("Okvir z ročaji - razporeditev višine")


#teža:

#histogram
ggplot(data = data_urejeni) + 
  geom_histogram(mapping = aes(x=teza), bins = 20, color = 'black', fill = 'white') +
  xlab("Teža anketirancev") + 
  ylab("Število anketirancev") + 
  ggtitle("Histogram - razporeditev teže")
#okvir z ročaji
ggplot(data = data_urejeni) +
  geom_boxplot(mapping=aes(x=teza)) +
  xlab("Teza anketirancev") + 
  ggtitle("Okvir z ročaji - razporeditev teže")

#c) hipoteza
#Predpostavim, da povezanost med težo in višino ne obstaja, saj imata oba grafa popolnoma drugačno asimetrijo in razpršenost. 
#Vidim, da za različne višine dobimo dokaj ozek izbor tež (večinoma med 60kg in 80kg).


# d) Določite odvisno in neodvisno spremenljivko, narišite razsevni grafikon

# Y odvisna -> teža
# X neodvisna -> višina

#razsevni grafikon:
ggplot(data_urejeni, mapping = aes(x=visina, y=teza)) +
  geom_point() +
  xlab("Višina anketirancev") +
  ylab("Teža anketirancev") +
  ggtitle("Razsevni grafikon: odvisnost teže od višine") +
  theme(plot.title = element_text(hjust = 0.5))

#e) izračun Pearsonovega koeficienta
pearsonov_koeficient = cor(data_urejeni$visina, data_urejeni$teza, method="pearson")
pearsonov_koeficient

#f) Izračun linearne regresijske premice in vris premice v razsevni grafikon
model = lm(formula = teza ~ visina, data = data_urejeni)
model
ggplot(data_urejeni, mapping = aes(x=visina, y=teza)) + 
  geom_point() +
  xlab("Višina anketirancev") +
  ylab("Teža anketirancev") +
  ggtitle("Razsevni grafikon z vrisano regregresijsko premico: odvisnost teže od višine") +
  geom_smooth(method='lm', se = FALSE)
# 56.2498, 0.0949
# teza = 56.2498 + 0.0949 * visina

summary(model)
#Adjusted R-squared:  -0.006445 

#3. ANALIZA OPISNIH SPREMENLJIVK

# a) fk in fk% tabela
table(zaposlitveni_status)
prop.table(table(zaposlitveni_status))*100

table(cepljenje_proti_COVIDu)
prop.table(table(cepljenje_proti_COVIDu))*100

# b) Grafični prikaz s stolpci


ggplot(data = data_urejeni) +
  geom_bar(mapping = aes(x = zaposlitveni_status), color = 'black', fill = 'white') +
  xlab("Status zaposlitve") +
  ylab("Število udeležencev")

ggplot(data = data_urejeni) +
  geom_bar(mapping = aes(x = cepljenje_proti_COVIDu), color = 'black', fill = 'white') +
  xlab("Cepljen proti COVIDu") +
  ylab("Število udeležencev")


# c) Kontingenčna tabela

kont_tabela = table(zaposlitveni_status, cepljenje_proti_COVIDu)
kont_tabela


# e) Narišite oba grafa dvorazsežnih struktur

ggplot(data = data_urejeni) + 
  geom_bar(mapping = aes(x = zaposlitveni_status, fill = cepljenje_proti_COVIDu), position = 'fill') +
  xlab("Zaposlitveni status")

# na tem grafu opazimo, da so vsi zaposleni udeleženci cepljeni proti kovidu, 
ggplot(data = data_urejeni) +
  geom_bar(mapping = aes( x = cepljenje_proti_COVIDu, fill = zaposlitveni_status), position = 'fill') +
  xlab("Cepljen proti COVIDu")


# f) Izračunajte vrednost Hi-kvadrat testa in kontingenčne koeficiente (C in Cpop). 

HI2 =chisq.test(kont_tabela)
HI2
library(grid)
library(vcd)
assocstats(kont_tabela)

#k=2
C_pop = 0.218/sqrt((2-1)/2)
C_pop