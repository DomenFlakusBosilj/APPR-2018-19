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
graf_gas_pmio <- ggplot(data = gas_pmio, mapping = aes(x=Drzava, y=Kolicina_kg_na_mio, fill=factor(Leto))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, size = 8))


#graf delez ljudi

graf_delez_ljudi <- ggplot(data = delez_ljudi, mapping = aes(x=Leto, y=Delez,
                                                             color=Prevozno_sredstvo)) +
  ggtitle("Delez uporabe prevoznih sredstev") +
  geom_line() +
  facet_grid( Drzava~.) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) 

# +
#   theme(legend.title = element_text('Prevozno sredstvo'))# graf_delez_ljudi <- ggplot(data = delez_ljudi, mapping = aes(x=Drzava, y=Delez,
#                                                             fill=Prevozno_sredstvo, group=Leto)) +
#   geom_bar(stat = 'identity',position = 'dodge') +
#   theme(axis.text.x = element_text(angle = 90, size = 8))


#graf smrti
graf_smrti <- ggplot(data = smrti_pmio, mapping = aes(x=Leto, y=Stevilo,
                                                 color=Prevozno_sredstvo, group=Leto)) +
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
europe <- filter(europe, long < 55 & long > -45 & lat > 30 & lat < 85)

europe <- filter(europe, NAME != "Jersey")
europe <- filter(europe, NAME != "Russia")
europe <- filter(europe, NAME != "Ukraine")
europe <- filter(europe, NAME != "Montenegro")

# Drzave v zemljevidu Evrope
drzave <- unique(europe$NAME) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "Drzava"

skupaj <- left_join(drzave, smrti_sum_pmio_2016, by="Drzava")
skupaj$Stevilo[is.na(skupaj$Stevilo)]<- 0

# Izrišem zemljevid Evrope, v katerem bo vsaka država pobarvana glede št. igralcev v ligi NBA
zemljevid_smrti_sum_pmio_2016 <- ggplot() + geom_polygon(data=left_join(europe, skupaj, by=c("NAME"="Drzava")),
                       aes(x=long, y=lat, group=group, fill=Stevilo)) +
  geom_line() +
  guides(fill=guide_colorbar(title="Število smrti")) +
  ggtitle("Število smrti v prometu na milijon prebivalcev leta 2016") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "white", high = "red",
                      space = "Lab", na.value = "grey", guide = "black",
                      aesthetics = "fill")


# europe <- filter(europe, SOVEREIGNT != "Albania")
# europe <- filter(europe, SOVEREIGNT != "Andorra")
# europe <- filter(europe, SOVEREIGNT != "Montenegro")
# europe <- filter(europe, SOVEREIGNT != "Moldova")
# europe <- filter(europe, SOVEREIGNT != "Macedonia")
# europe <- filter(europe, SOVEREIGNT != "Monaco")
# europe <- filter(europe, SOVEREIGNT != "Malta")
# europe <- filter(europe, SOVEREIGNT != "Liechtenstein")
# europe <- filter(europe, SOVEREIGNT != "Kosovo")
# europe <- filter(europe, SOVEREIGNT != "Liechtenstein")
# europe <- filter(europe, SOVEREIGNT != "Iceland")
# europe <- filter(europe, SOVEREIGNT != "Belarus")
# europe <- filter(europe, SOVEREIGNT != "Bosnia and Herz.")
# europe <- filter(europe, SOVEREIGNT != "Faeroe Is.")
# europe <- filter(europe, SOVEREIGNT != "Guernsey")
# europe <- filter(europe, SOVEREIGNT != "^Isle.*")
# europe <- filter(europe, SOVEREIGNT != "Jersey")
# europe <- filter(europe, SOVEREIGNT != "Russia")
# europe <- filter(europe, SOVEREIGNT != "Serbia")
# europe <- filter(europe, SOVEREIGNT != "San Marino")
# europe <- filter(europe, SOVEREIGNT != "Ukraine")
# europe <- filter(europe, SOVEREIGNT != "Vatican")
# 
# europe <- filter(europe, NAME != "Guernsey")
# europe <- filter(europe, NAME != "Jersey")
# europe <- filter(europe, NAME != "Ã…land")
# europe <- filter(europe, NAME != "^Bosnia.*")
# 
# graf_europe <- ggplot(europe, aes(x=long, y=lat, group=group, fill=NAME)) +
#   geom_polygon() +
#   labs(title="Evropa - brez podatkov") +
#   theme_classic()



# zemljevid_smrti <- ggplot() +
#   geom_polygon(data = data = povprecje %>% right_join(europe, by = c("regija" = "NAME_1")),
#                aes(x = long, y = lat, group = Prevozno_sredstvo, fill = Stevilo))+
#   xlab("") + ylab("") + ggtitle("Število porok po slovenskih regijah")
# 
# zemljevid_smrti + scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab",
#                                        na.value = "grey50", guide = "colourbar")
