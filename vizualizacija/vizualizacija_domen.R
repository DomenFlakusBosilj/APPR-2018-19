library(ggplot2)
library(ggvis)
library(dplyr)
library(rgdal)
library(mosaic)
library(maptools)
library(ggmap)
library(mapproj)
library(munsell)


#graf bdppc
graf_bdppc <- ggplot(data = bdppc, mapping = aes(x=Drzava, y=Kolicina_eur, group=Leto)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  theme(legend.text = element_text(size=8))

# ggplot(data = bdppc, mapping = aes(x=Leto, y=Kolicina_eur, color=Drzava)) +
#   geom_line()


#graf greenhouse gas
graf_greenhouse <- ggplot(data = greenhouse_gas, mapping = aes(x=Leto, y=Kolicina.kg, color=Drzava)) +
  geom_line(stat = 'identity', position = 'dodge')




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

ggplot(europe, aes(x=long, y=lat, group=group, fill=NAME)) +
  geom_polygon() +
  labs(title="Evropa - brez podatkov") +
  theme(legend.position="none")


