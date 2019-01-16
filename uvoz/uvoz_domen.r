library(readr)
library(dplyr)
library(tidyr)
library(reshape2)

loc <- locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ",")


#Tabela 1

prebivalstvo <- read_csv('podatki/prebivalstvo.csv', locale = loc, skip=1,
                         col_names = c('Leto', 'Drzava', 'sex', 'age', 'unit',
                                       'Stevilo', 'krnekaj'), na = c(':', ''))
prebivalstvo <- prebivalstvo[,-7]
prebivalstvo <- prebivalstvo[,-5]
prebivalstvo <- prebivalstvo[,-4]
prebivalstvo <- prebivalstvo[,-3]

prebivalstvo$Drzava <- gsub("^Germany.*", "Germany", prebivalstvo$Drzava)


#Tabela 2 
delez_ljudi <- read_csv('podatki/ljudje_delezi.csv', locale = loc, skip = 1,
                    col_names = c('Leto', 'Drzava', 'Prevozno_sredstvo', 'krnekaj',
                                  'Delez', 'krnekaj1'), na = c(':', ''))
delez_ljudi <- delez_ljudi[,-6]
delez_ljudi <- delez_ljudi[,-4]

delez_ljudi$Drzava <- gsub("^Germany.*", "Germany", delez_ljudi$Drzava)
delez_ljudi$Drzava <- gsub("^European Union.*", "European Union", delez_ljudi$Drzava)

delez_ljudi$Prevozno_sredstvo <- gsub("^Passenger cars.*", "Avto", delez_ljudi$Prevozno_sredstvo)
delez_ljudi$Prevozno_sredstvo <- gsub("^Motor.*", "Bus", delez_ljudi$Prevozno_sredstvo)
delez_ljudi$Prevozno_sredstvo <- gsub("^Trains.*", "Vlak", delez_ljudi$Prevozno_sredstvo)


#Tabela 2
delez_tovora <- read_csv('podatki/tovor_delezi.csv', locale = loc, skip = 1,
                        col_names = c('Leto', 'Drzava', 'Prevozno_sredstvo', 'krnekaj',
                                      'Delez', 'krnekaj1'), na = c(':', ''))
delez_tovora <- delez_tovora[,-6]
delez_tovora <- delez_tovora[,-4]

delez_tovora$Drzava <- gsub("^Germany.*", "Germany", delez_tovora$Drzava)
delez_tovora$Drzava <- gsub("^European Union.*", "European Union", delez_tovora$Drzava)

delez_tovora$Prevozno_sredstvo <- gsub("^Passenger cars.*", "Avto", delez_tovora$Prevozno_sredstvo)
delez_tovora$Prevozno_sredstvo <- gsub("^Motor.*", "Bus", delez_tovora$Prevozno_sredstvo)
delez_tovora$Prevozno_sredstvo <- gsub("^Trains.*", "Vlak", delez_tovora$Prevozno_sredstvo)


#Tabela 3
smrti_avti_bus <- read_csv('podatki/smrti_avti_busi.csv', locale = loc, skip=1,
                           col_names = c('Leto', 'Drzava', 'krnekaj1', 'Prevozno_sredstvo',
                                         'Stevilo', 'krnekaj2'), na = c(':', '', ''))
smrti_avti_bus <- smrti_avti_bus[,-6]
smrti_avti_bus <- smrti_avti_bus[,-3]


#Tabela 4
smrti_vlak <- read_csv('podatki/smrti_vlak.csv', locale=loc, skip=1,
                           col_names = c('Leto', 'Drzava', 'unit', 'accident', 'victim',
                                         'pers_inv', 'Stevilo', 'krnekaj'), na = c(':', ''))
smrti_vlak <- smrti_vlak[,-8]
smrti_vlak <- smrti_vlak[,-6]
smrti_vlak <- smrti_vlak[,-5]
smrti_vlak <- smrti_vlak[,-4]
smrti_vlak <- smrti_vlak[,-3]

smrti_vlak$Prevozno_sredstvo <- "Vlak"


#Tabela 5
greenhouse_gas <- read_csv('podatki/greenhouse_gas.csv', locale = loc, skip=1,
                       col_names = c('Leto', 'Drzava', 'airpol', 'nace', 'unit',
                                     'Kolicina_kg', 'krnekaj'), na = c(':', ''))
greenhouse_gas <- greenhouse_gas[,-7]
greenhouse_gas <- greenhouse_gas[,-5]
greenhouse_gas <- greenhouse_gas[,-4]
greenhouse_gas <- greenhouse_gas[,-3]

greenhouse_gas <- greenhouse_gas %>% filter(Drzava != "European Union (current composition)")

greenhouse_gas$Drzava <- gsub("^Germany.*", "Germany", greenhouse_gas$Drzava)


#Tabela 6

bdppc <- read_csv('podatki/bdppc.csv', locale=loc, skip = 1,
                           col_names = c('Leto', 'Drzava', 'unit', 'na', 'Kolicina_eur',
                                         'krnekaj'), na = c(':', ''))
bdppc <- bdppc[,-6]
bdppc <- bdppc[,-4]
bdppc <- bdppc[,-3]

bdppc <- bdppc %>% filter(Drzava != "European Union (current composition)")

bdppc$Drzava <- gsub("^Germany.*", "Germany", bdppc$Drzava)


#Tabela 7
smrti <- rbind(smrti_avti_bus, smrti_vlak) %>%
  filter(Drzava != "European Union (current composition)")

smrti$Drzava <- gsub("^Germany.*", "Germany", smrti$Drzava)

smrti$Prevozno_sredstvo <- gsub("Passenger cars", "Avto", smrti$Prevozno_sredstvo)
smrti$Prevozno_sredstvo <- gsub("Buses", "Bus", smrti$Prevozno_sredstvo)

grp <- group_by(smrti, Leto, Drzava)
smrti_sum <- summarise(grp, vsote=sum(Stevilo, na.rm = TRUE))


#Tabela 8

smrti_pmio <- left_join(smrti, prebivalstvo, by = c("Leto", "Drzava"))
names(smrti_pmio) <- c("Leto", "Drzava", 'Prevozno_sredstvo','Stevilo_smrti', 'Stevilo_ljudi')

smrti_pmio <- transform(smrti_pmio, Stevilo = ((Stevilo_smrti / Stevilo_ljudi) * 1000000))

smrti_pmio <- smrti_pmio[,-5]
smrti_pmio <- smrti_pmio[,-4]

smrti_pmio$Stevilo <- round(smrti_pmio$Stevilo, digits=2)


# smrti_pmio <- left_join(smrti_sum, prebivalstvo)
# names(smrti_pmio) <- c("Leto", "Drzava", 'Smrti', 'Stevilo_ljudi')
# 
# smrti_pmio <- transform(smrti_pmio, Stevilo = ((Smrti / Stevilo_ljudi) * 1000000))
# 
# smrti_pmio <- smrti_pmio[,-4]
# smrti_pmio <- smrti_pmio[,-3]
# 
# smrti_pmio$Stevilo <- round(smrti_pmio$Stevilo, digits=2)


#Tabela 10
gas_pmio <- left_join(greenhouse_gas, prebivalstvo)
names(gas_pmio) <- c("Leto", "Drzava", 'Kolicina_kg', 'Stevilo_ljudi')

gas_pmio <- transform(gas_pmio, Kolicina_kg_na_mio = ((Kolicina_kg / Stevilo_ljudi) * 1000000))

gas_pmio <- gas_pmio[,-4]
gas_pmio <- gas_pmio[,-3]

gas_pmio$Kolicina_kg_na_mio <- round(gas_pmio$Kolicina_kg_na_mio, digits=0)


#Shranjevanje tidy tabel

# write.csv(bdppc, file = "podatki/tidy/tidy_bdppc.csv", fileEncoding = 'UTF-8')
# write.csv(delez_ljudi, file = "podatki/tidy/tidy_delez_ljudi.csv", fileEncoding = 'UTF-8')
# write.csv(delez_tovora, file = "podatki/tidy/tidy_delez_tovora.csv", fileEncoding = 'UTF-8')
# write.csv(greenhouse_gas, file = "podatki/tidy/tidy_greenhouse_gas.csv", fileEncoding = 'UTF-8')
# write.csv(smrti, file = "podatki/tidy/tidy_smrti.csv", fileEncoding = 'UTF-8')
