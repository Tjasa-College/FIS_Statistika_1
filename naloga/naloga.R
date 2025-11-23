
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

#c)na osnovi spletne strani, ki je navedena kot vir podatkov, natančno opredelite populacijo (stvarna, krajevna, časovna opredelitev)
#statistično enoto in vaše 4 dodeljene statistične spremenljivke. 
#posamezno statistično spremenljivko natančno opredelite (vrsta spremenljivke glede na tip izražanja in tip merjenja)

#a) Uporabljen ukaz za prikaz  vseh spremenljivk in enot v dani datoteki
data = read.csv("FavouriteIndianFood.csv", header = TRUE)

#b) priprava podatkov za kasnejšo uporabo:
zaposlitveni_status = data$employment_status #opisna
cepljenje_proti_COVIDu = data$covid_vaccine #opisna
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

