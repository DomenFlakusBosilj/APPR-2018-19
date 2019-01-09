library(readr)
library(dplyr)
library(tidyr)
library(reshape2)

loc <- locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ",")

#Tabela 1 

delez_ljudi <- read.csv('podatki/ljudje_delezi.csv', encoding = 'windows-1250',
                    col.names = c('Leto', 'Drzava', 'Prevozno_sredstvo', 'krnekaj',
                                  'Delez', 'krnekaj'), na = c(':', ''))
delez_ljudi <- delez_ljudi[,-6]
delez_ljudi <- delez_ljudi[,-4]


#Tabela 2

delez_tovora <- read.csv('podatki/tovor_delezi.csv', encoding = 'windows-1250',
                        col.names = c('Leto', 'Drzava', 'Prevozno_sredstvo', 'krnekaj',
                                      'Delez', 'krnekaj'), na = c(':', ''))
delez_tovora <- delez_tovora[,-6]
delez_tovora <- delez_tovora[,-4]


#Tabela 3

smrti_avti_bus <- read_csv('podatki/smrti_avti_busi.csv', locale = loc, skip=1,
                           col_names = c('Leto', 'Drzava', 'krnekaj1', 'Prevozno_sredstvo',
                                         'Stevilo', 'krnekaj2'), na = c(':', '', ''))
smrti_avti_bus <- smrti_avti_bus[,-6]
smrti_avti_bus <- smrti_avti_bus[,-3]

#smrti_avti_bus <- smrti_avti_bus %>% spread(Prevozno_sredstvo, Stevilo)


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

greenhouse_gas <- read.csv('podatki/greenhouse_gas.csv', encoding = 'windows-1250',
                       col.names = c('Leto', 'Drzava', 'airpol', 'nace', 'unit',
                                     'Kolicina.kg', 'krnekaj'), na = c(':', ''))
greenhouse_gas <- greenhouse_gas[,-7]
greenhouse_gas <- greenhouse_gas[,-5]
greenhouse_gas <- greenhouse_gas[,-4]
greenhouse_gas <- greenhouse_gas[,-3]

greenhouse_gas <- greenhouse_gas %>% filter(Drzava != "European Union (current composition)")


#Tabela 6

bdppc <- read_csv('podatki/bdppc.csv', locale=loc, skip = 1,
                           col_names = c('Leto', 'Drzava', 'unit', 'na', 'Kolicina_eur',
                                         'krnekaj'), na = c(':', ''))
bdppc <- bdppc[,-6]
bdppc <- bdppc[,-4]
bdppc <- bdppc[,-3]

bdppc <- bdppc %>% filter(Drzava != "European Union (current composition)")


#Tabela 7

#smrti <- left_join(smrti_avti_bus, smrti_vlak)
#names(smrti) <- c("Leto", "Drzava", 'Avtobus', 'Avto', 'Vlak')

#smrti <- melt(smrti, variable.name = "Prevozno_sredstvo", value.name = 'Stevilo', measure.vars=3:5)

#smrti[,4] <- as.integer(smrti[,4])
#smrti[,2] <- as.character(smrti[,2])

smrti <- rbind(smrti_avti_bus, smrti_vlak) %>%
  filter(Drzava != "European Union (current composition)")

smrti$Drzava <- gsub("^Germany.*", "Germany", smrti$Drzava)


#Shranjevanje tidy tabel

write.csv(bdppc, file = "podatki/tidy/tidy_bdppc.csv", fileEncoding = 'UTF-8')
write.csv(delez_ljudi, file = "podatki/tidy/tidy_delez_ljudi.csv", fileEncoding = 'UTF-8')
write.csv(delez_tovora, file = "podatki/tidy/tidy_delez_tovora.csv", fileEncoding = 'UTF-8')
write.csv(greenhouse_gas, file = "podatki/tidy/tidy_greenhouse_gas.csv", fileEncoding = 'UTF-8')
write.csv(smrti, file = "podatki/tidy/tidy_smrti.csv", fileEncoding = 'UTF-8')
