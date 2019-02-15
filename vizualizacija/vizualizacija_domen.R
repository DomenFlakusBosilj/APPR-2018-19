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


#graf greenhouse gas
graf_gas_pmio <- ggplot(data = gas_pmio, mapping = aes(x=Drzava, y=Kolicina_kg_na_mio,
                                                       fill=factor(Leto))) +
  labs(fill='Leto') +
  ggtitle('Količina toplogrednih plinov v ozračju') +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Država', y = 'kg na milijon prebivalcev') +
  theme(axis.text.x = element_text(angle = 90, size = 8))


#graf delez ljudi
graf_delez_ljudi <- ggplot(data = filter(delez_ljudi, Drzava == 'Austria' | Drzava == 'Bulgaria' |
                                         Drzava == 'Croatia' | Drzava == 'Czechia' | Drzava == 'Estonia' |
                                         Drzava == 'France' | Drzava == 'Hungary' | Drzava == 'Ireland' |
                                         Drzava == 'Lithuania' | Drzava == 'Sweden' | Drzava == 'Poland' |
                                         Drzava == 'Slovenia' | Drzava == 'Switzerland'),
                           mapping = aes(x=Leto, y=Delez, color=Prevozno_sredstvo)) +
  labs(color='Prevozno sredstvo') +
  ggtitle("Deleži uporabe prevoznih sredstev") +
  labs(x = 'Leto', y = 'Delež') +
  geom_line() +
  facet_grid(Drzava~.) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) 


#graf smrti
graf_smrti <- ggplot(data = filter(smrti_pmio, Drzava == 'Austria' | Drzava == 'Bulgaria' |
                                     Drzava == 'Croatia' | Drzava == 'Czechia' | Drzava == 'Estonia' |
                                     Drzava == 'France' | Drzava == 'Hungary' | Drzava == 'Ireland' |
                                     Drzava == 'Lithuania' | Drzava == 'Sweden' | Drzava == 'Poland' |
                                     Drzava == 'Slovenia' | Drzava == 'Switzerland'),
                     mapping = aes(x=Leto, y=Stevilo, color=Prevozno_sredstvo, group=Leto)) +
  labs(color='Prevozno sredstvo') +
  ggtitle('Smrti v prometu z različnimi prevoznimi sredstvi') +
  labs(x = 'Leto', y = 'Število') +
  geom_point(stat = 'identity', position = 'dodge') +
  facet_grid( Drzava~.) +
  theme(axis.text.x = element_text(angle = 90, size = 6))


# Uvozimo zemljevid Sveta
# source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")
source("lib/uvozi.zemljevid.r") #Nastavi pravo datoteko

svet <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries") %>% fortify()


# Zemljevid sveta skrčimo na zemljevid Evrope
europe <- filter(svet, CONTINENT == "Europe")
europe <- filter(europe, long < 55 & long > -35 & lat > 30 & lat < 85)

europe <- filter(europe, NAME != "Jersey")
europe <- filter(europe, NAME != "Russia")

# Drzave v zemljevidu Evrope
drzave <- unique(europe$NAME) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "Drzava"

skupaj <- left_join(drzave, smrti_sum_pmio_2015, by="Drzava")
#skupaj$Stevilo[is.na(skupaj$Stevilo)]<- 0

# Zemljevid s podatki
zemljevid_smrti_sum_pmio_2015 <- ggplot() + geom_polygon(data=left_join(europe, skupaj, by=c("NAME"="Drzava")),
                       aes(x=long, y=lat, group=group, fill=Stevilo)) +
  geom_line() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Število smrti")) +
  ggtitle("Število smrti v prometu na milijon prebivalcev leta 2015") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "white", high = "red",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill")