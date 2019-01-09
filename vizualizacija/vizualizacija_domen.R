library(ggplot2)
library(ggvis)
library(dplyr)
library(rgdal)
library(mosaic)
library(maptools)
library(ggmap)
library(mapproj)
library(munsell)


#to se malo polepsaj

graf_bdppc <- ggplot(data = bdppc, mapping = aes(x=Leto, y=Kolicina_eur, fill=Drzava)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  theme(legend.text = element_text(size=8)) #+
  #scale_y_discrete(name="Kolicina(€)", breaks=c('10000','20000','30000','40000','50000',
  #                                              '60000','70000','80000','90000','100000'))
#plot(graf_bdppc)

ggplot(data = bdppc, mapping = aes(x=Leto, y=Kolicina_eur, color=Drzava)) +
  geom_line()

graf_greenhouse <- ggplot(data = greenhouse_gas, mapping = aes(x=Leto, y=Kolicina.kg, fill=Drzava)) +
  geom_bar(stat = 'identity', position = 'dodge')




graf_delez_ljudi <- ggplot(data = delez_ljudi, mapping = aes(x=Leto, y=Delez, 
                                                            fill=Prevozno_sredstvo, group=Drzava)) +
  geom_bar(stat = 'identity', position = 'dodge')



graf_smrti <- ggplot(data = smrti, mapping = aes(x=Drzava, y=Stevilo,
                                                 fill=Prevozno_sredstvo, group=Leto)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, size = 6))


# Uvozimo zemljevid Sveta
# source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")
source("lib/uvozi.zemljevid.r") #Nastavi pravo datoteko

svet <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries") %>% fortify()


# Zemljevid sveta skrčimo na zemljevid Evrope
europe <- filter(svet, CONTINENT == "Europe")
europe <- filter(europe, long < 55 & long > -45 & lat > 30 & lat < 85)

narisi_europe <- ggplot(europe, aes(x=long, y=lat, group=group, fill=NAME)) +
  geom_polygon() +
  labs(title="Evropa - osnovna slika") +
  theme(legend.position="none")


