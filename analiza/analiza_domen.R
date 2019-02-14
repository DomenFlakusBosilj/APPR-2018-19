source("lib/libraries.r", encoding="UTF-8")

#Regresijska premica za stevilo smrti v prometu v Evropi
grp <- group_by(smrti_sum, Leto)
smrti_europa <- summarise(grp, vsote1=sum(vsote, na.rm = TRUE))

grp <- group_by(prebivalstvo, Leto)
prebivalstvo_europa <- summarise(grp, vsote2=sum(Stevilo, na.rm = TRUE))

smrti_europa_pmio <- left_join(smrti_europa, prebivalstvo_europa, by = c("Leto"))
smrti_europa_pmio <- transform(smrti_europa_pmio, Stevilo = ((vsote1 / vsote2) * 1000000))
smrti_europa_pmio$Stevilo <- round(smrti_europa_pmio$Stevilo, digits=2)
smrti_europa_pmio <- smrti_europa_pmio[,-3]
smrti_europa_pmio <- smrti_europa_pmio[,-2]

prileganje <- lm(data = smrti_europa_pmio, Stevilo ~ Leto)

l <- data.frame(Leto=seq(2017, 2020, 1))
napoved <- mutate(l, Stevilo=predict(prileganje, l))

graf_regresija <- ggplot(smrti_europa_pmio, aes(x=Leto, y=Stevilo)) +
  geom_smooth(method=lm, fullrange = TRUE, color = 'blue') +
  geom_point(data=napoved, aes(x=Leto, y=Stevilo), color='red', size=2) +
  geom_point() +
  labs(title='Napoved števila smrti v prometu v naslednjih letih', y="Število")



#====================================================================================
#smrti_pmio_avto za vsa leta

smrti_pmio_avto <- filter(smrti_pmio, Prevozno_sredstvo == 'Avto', Leto != '2007',
                          Leto != '2016', Drzava != 'Ireland', Drzava != 'Lithuania',
                          Drzava != 'Slovakia')
smrti_pmio_avto <- smrti_pmio_avto[,-3]

grp <- group_by(smrti_pmio_avto, Drzava)
smrti_pmio_avto_sum <- summarise(grp, vsote=sum(Stevilo, na.rm = TRUE))

smrti_pmio_avto <- dcast(smrti_pmio_avto, Drzava~Leto, value.var = 'Stevilo')

smrti_pmio_avto <- left_join(smrti_pmio_avto, smrti_pmio_avto_sum, by = 'Drzava')

smrti_pmio_avto <- smrti_pmio_avto[order(smrti_pmio_avto$vsote),]

smrti_pmio_avto_a <- smrti_pmio_avto[,-10]
smrti_pmio_avto_a <- smrti_pmio_avto_a[,-1]

n <- 7
fit <- hclust(dist(scale(smrti_pmio_avto_a)))
skupine3 <- cutree(fit, n)

cluster3 <- mutate(smrti_pmio_avto, skupine3)
cluster3 <- cluster3[,-2:-10]
colnames(cluster3)<- c("Drzava","Stevilo")


#zemljevid
drzave <- unique(europe$NAME) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "Drzava"

skupaj <- left_join(drzave, cluster3, by="Drzava")

# Zemljevid s podatki
zemljevid_cluster3 <- ggplot() + geom_polygon(data=left_join(europe, skupaj, by=c("NAME"="Drzava")),
                                                         aes(x=long, y=lat, group=group, fill=Stevilo)) +
  geom_line() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Skupine")) +
  ggtitle('Razvrstitev držav v skupine glede na število smrti v prometu') +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "#cce6ff", high = "#005cb3",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill")


#==================================================================================
# bdppc za vsa leta

bdppc_a <- filter(bdppc, Leto != '2007', Leto != '2016', Drzava != 'Ireland',
                  Drzava != 'Lithuania', Drzava != 'Slovakia')

grp <- group_by(bdppc_a, Drzava)
bdppc_sum <- summarise(grp, vsote=sum(Kolicina_eur, na.rm = TRUE))

bdppc_a <- dcast(bdppc_a, Drzava~Leto, value.var = 'Kolicina_eur')

bdppc_a <- left_join(bdppc_a, bdppc_sum, by = 'Drzava')

bdppc_a <- bdppc_a[order(bdppc_a$vsote, decreasing = TRUE),]

bdppc_b <- bdppc_a[,-10]
bdppc_b <- bdppc_b[,-1]

n <- 7
fit <- hclust(dist(scale(bdppc_b)))
skupine4 <- cutree(fit, n)

cluster4 <- mutate(bdppc_a, skupine4)
cluster4 <- cluster4[,-2:-10]
colnames(cluster4)<- c("Drzava","Kolicina_eur")


#zemljevid
drzave <- unique(europe$NAME) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "Drzava"

skupaj <- left_join(drzave, cluster4, by="Drzava")

# Zemljevid s podatki
zemljevid_cluster4 <- ggplot() + geom_polygon(data=left_join(europe, skupaj, by=c("NAME"="Drzava")),
                                              aes(x=long, y=lat, group=group, fill=Kolicina_eur)) +
  geom_line() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Skupine")) +
  ggtitle('Razvrstitev držav v skupine glede na bdppc') +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "#e6f3ff", high = "#0069cc",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill")





# #=====================================================================================
# 
# #Clusters (smrti_pmio_avto)
# smrti_pmio_avto_2016 <- filter(smrti_pmio, Prevozno_sredstvo == 'Avto', Leto == '2016',
#                           Stevilo != 'NA')
# smrti_pmio_avto_2016 <- smrti_pmio_avto_2016[,-3]
# smrti_pmio_avto_2016 <- smrti_pmio_avto_2016[,-1]
# smrti_pmio_avto_2016 <- smrti_pmio_avto_2016[order(smrti_pmio_avto_2016$Stevilo),]
# 
# n <- 5
# fit <- hclust(dist(scale(smrti_pmio_avto_2016$Stevilo)))
# skupine1 <- cutree(fit, n)
# 
# cluster1 <- mutate(smrti_pmio_avto_2016, skupine1)
# 
# 
# #Clusters (bdppc)
# bdppc_2016 <- filter(bdppc, Leto == '2016')
# 
# bdppc_2016 <- bdppc_2016[, -1]
# 
# n <- 5
# fit <- hclust(dist(scale(bdppc_2016$Kolicina_eur)))
# skupine2 <- cutree(fit, n)
# 
# cluster2 <- mutate(bdppc_2016, skupine2)
# 
# 
# #Clustere zdruzim
# cluster <- left_join(cluster1, cluster2, by = c("Drzava"))
# cluster <- cluster[,-4]
# cluster <- cluster[,-2]
# colnames(cluster)<- c("Drzava","Glede na varnost","Glede na bdppc")