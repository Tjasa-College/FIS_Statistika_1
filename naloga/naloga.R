
#Počekiram working directory in jo nastavim na mapo v kateri delam
#to je zato, da lažje kličem .csv datoteko in dobim podatke iz nje
#pomagala sem si s tem virom: https://stackoverflow.com/questions/62491103/error-setwd-cannot-change-working-directory
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

#1. Opisovanje in urejanje podatkov

#a) Uporabljen ukaz za prikaz  vseh spremenljivk in enot v dani datoteki
data = read.csv("FavouriteIndianFood.csv", header = TRUE)

#b) priprava podatkov za kasnejšo uporabo:

visina = data$height #številska
teza = data$weight #številska

data_urejeni = data.frame(zaposlitveni_status, cepljenje_proti_COVIDu, visina, teza)

#c) spletna stran: https://www.kaggle.com/datasets/abdulraheem625/food-choice-male-vs-female-indianpakistani-food

#populacija: Vsi moški in ženske, ki so se udeležili Data Visualization delavnice in oddali svoje podatke, za analizo, med Zoom predanajem
#udeleženci delavnice Data Visualization iz celega sveta, ki so oddali svoje podatke, za analizo, med delavnico.
#stvarna: vsi ljudje
#Krajevna: iz celega sveta
#časovna: v času Data Visualization delavnice (11.12.2022)


#statistična enota: en človek, ki se je udeležil Data Visualization delavnice in oddal svoje podatke za analizo med Zoom predavanjem.

#opredelitev spremenljivk:
# zaposlitveni status: opisna, nominalna
# cepljenje proti covidu: opisna, nominalna
# višina: številska, zvezna, razmernostna
# teža: številska, zvezna, razmernostna

#2. Analiza številskih spremenljivk

# a)Izračunajte glavne opisne statistike (srednje vrednosti in mere variabilnosti) za obe
# številski spremenljivki in jih prikažite (ustavite sliko rezultatov). Rezultate tudi vsebinsko
# interpretirajte.
library(psych)
#višina:
#srednje vrednosti: aritmeticna sredina, modus, mediana

mean(visina)
median(visina)
quantile(visina, probs=c(0.25, 0.5, 0.75))
describe(visina)
#srednja višina udeležencev je okoli 165.69cm
#mediana je 167.64cm, kar pomeni, da je 50% udeležencev višjih od 167.64cm, 50% udeležencev pa nižjih od 167.64cm
# iz rezultatov lahko sklepamo, da je povprečna višina udeležencev 165.69cm, 50% udeležencev je manjših od 167.64cm, 
# četrtina ljudi pa je celo manjših od 156.6cm. Standardna deviacija (sd) je 13.92, kar pomeni, da ima večina ljudi velikost v rangu
# razponu 151.77cm (mean-sd) in 179.61cm (mean+sd). 
# Koeficient asimetrije (skew) je -0.23 (<0) kar pomeni, da imamo asimetrijo v levo (rep na levi in vrh na desni strani).
#Asimetrija v levo pomeni, da je večje število ljudi višjih od povprečja
# Koeficient sploščenosti (kurtosis) pa je -0.15 (<0) kar pomeni, da je porazdelitev višine sploščena. 
# Razpršenost podatkov okoli povprečja je večja.
165.69+13.92
165.69-13.92


#teža:
mean(teza)
median(teza)
quantile(teza, probs=c(0.25, 0.5, 0.75))
describe(teza)
# iz rezultatov lahko sklepamo, da je povprečna teža udeležencev 71.97368 kg, 50% udeležencev ima manj kot 70.1 kg, 
# četrtina ljudi pa ima manj kot 60.0kg. Standardna deviacija (sd) je 18.94, kar pomeni, da ima večina ljudi velikost v rangu
# razponu 53.03kg (mean-sd) in 90.91kg (mean+sd). 
# Koeficient asimetrije (skew) je 1.39 (>0) kar pomeni, da imamo asimetrijo v desno (rep na desni in vrh na levi strani).
# Asimetrija v desno pomeni, da je večje število ljudi lažjih od povprečja
# Koeficient sploščenosti (kurtosis) pa je 5.17 (>0) kar pomeni, da je porazdelitev teže zelo koničasta. 
# Razpršenost podatkov okoli povprečja je manjša. 
# Ker je vrednost sploščenosti tako zelo visoka, predvidevam, da je v podatkh tudi kar nekaj mejnih vrednosti
# (uporabniki niso napisali prave teže, ampak neko zelo visoko ali nizko številko, kar lahko privede do dolgih repov in koničaste porazdelitve)
# vidimo tudi da je najmanjša teža 30kg in največja teža 163kg, kar lahko nakazuje na velika odstopanja in posledično tako velik
# koeficient razpršenosti.
71.97+18.94
71.97-18.94

#b)Številski spremenljivki predstavite tudi grafično s pomočjo histograma in okvirja z ročaji.
# Na osnovi grafičnih prikazov opišite značilnosti posamezne spremenljivke.

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
#Predpostavim, da povezanost med težo in višino ne obstaja, saj imata oba grafa popolnoma 
#drugačno asimetrijo in razpršenost. Vidim, da za različne višine dobimo dokaj ozek izbor tež 
#(večinoma med 60kg in 80kg).


# d) Glede na vsebino spremenljivk določite odvisno (Y) in neodvisno (X) spremenljivko.
# Nato narišite razsevni grafikon tako da je na x-osi spremenljivka X, na y-osi pa
# spremenljivka Y. Ustrezno preimenujte naslova osi, grafikonu dodajte tudi glavni naslov.
# Ali razsevni grafikon nakazuje na povezanost obravnavanih spremenljivk? Svoj odgovor
# pojasnite.

# Y odvisna -> teža
# X neodvisna -> višina

#razsevni grafikon:
ggplot(data_urejeni, mapping = aes(x=visina, y=teza)) +
  geom_point() +
  xlab("Višina anketirancev") +
  ylab("Teža anketirancev") +
  ggtitle("Razsevni grafikon: odvisnost teže od višine") +
  theme(plot.title = element_text(hjust = 0.5))


#razsevni grafikon nakazuje povezanost obravnavanih spremenljivk. 
#Glede na razsevni grafikon predvidevam da je pozevanost linearna, pozitivna in šibla

#Med obravnavanima spremenljivkama preverite njuno povezanost s pomočjo
#Pearsonovega korelacijskega koeficienta. Na osnovi njegove vrednosti opredelite smer in
#jakost povezanosti med obravnavanima spremenljivkama. Primerjajte dobljeni rezultat z
#vašo hipotezo iz točke c) te naloge.

pearsonov_koeficient = cor(data_urejeni$visina, data_urejeni$teza, method="pearson")
pearsonov_koeficient

#f) Izračunajte ustrezno linearno regresijsko premico in jo zapišite. Regresijsko premico
#vrišite tudi v razsevni grafikon iz točke d) te naloge.

model = lm(formula = teza ~ visina, data = data_urejeni)
model
ggplot(data_urejeni, mapping = aes(x=visina, y=teza)) + 
  geom_point() +
  geom_smooth(method='lm', se = FALSE)
# 56.2498, 0.0949
# teza = 56.2498 + 0.0949 * visina

summary(model)
#Adjusted R-squared:  -0.006445 

#3. ANALIZA OPISNIH SPREMENLJIVK
zaposlitveni_status = data$employment_status #opisna
cepljenje_proti_COVIDu = data$covid_vaccine #opisna

# a) Obe opisni spremenljivki najprej predstavite s pomočjo izpisa, ki prikazuje frekvence in
# odstotke posameznih vrednosti spremenljivk.
table(zaposlitveni_status)
prop.table(table(zaposlitveni_status))*100

table(cepljenje_proti_COVIDu)
prop.table(table(cepljenje_proti_COVIDu))*100

# b) Opisni spremenljivki predstavite tudi grafično (prikaz s stolpci), in na osnovi grafičnih
# prikazov opišite značilnosti posamezne spremenljivke.


ggplot(data = data_urejeni) +
  geom_bar(mapping = aes(x = zaposlitveni_status), color = 'black', fill = 'white') +
  xlab("Status zaposlitve") +
  ylab("Število udeležencev")

ggplot(data = data_urejeni) +
  geom_bar(mapping = aes(x = cepljenje_proti_COVIDu), color = 'black', fill = 'white') +
  xlab("Cepljen proti COVIDu") +
  ylab("Število udeležencev")


# c) Za obravnavani opisni spremenljivki izračunajte kontingenčno tabelo ter jo izpišite.

kont_tabela = table(zaposlitveni_status, cepljenje_proti_COVIDu)
kont_tabela


# e) Narišite oba grafa dvorazsežnih struktur: strukture ene opisne spremenljivke po stolcih
# druge na enem grafu in obratno na drugem. Sa slik opredelite če pričakujete povezanost
# spremenljivk in zakaj (zakaj ne).

ggplot(data = data_urejeni) + 
  geom_bar(mapping = aes(x = zaposlitveni_status, fill = cepljenje_proti_COVIDu), position = 'fill') +
  xlab("Zaposlitveni status")

# na tem grafu opazimo, da so vsi zaposleni udeleženci cepljeni proti kovidu, 
ggplot(data = data_urejeni) +
  geom_bar(mapping = aes( x = cepljenje_proti_COVIDu, fill = zaposlitveni_status), position = 'fill') +
  xlab("Cepljen proti COVIDu")

# d) Glede na vsebino spremenljivk, zapišite vašo hipotezo, oziroma če pričakujete njuno
# povezanost in zakaj (zakaj ne).

#Pričakujem šibko povezanost med spremenljivkama, saj opažam da so vsi zaposleni cepljeni, 
#največ necepljenih pa je med nezaposlenimi

# f) Izračunajte vrednost Hi-kvadrat testa in kontingenčne koeficiente (C in Cpop). Opredelite
# in obrazložite, ali med spremenljivkama obstaja povezava, tudi kakšna je jakost povezanosti
# med obravnavanima spremenljivkama.

HI2 =chisq.test(kont_tabela)
HI2
library(grid)
library(vcd)
assocstats(kont_tabela)

C_pop = 0.218/sqrt((2-1)/2)
C_pop