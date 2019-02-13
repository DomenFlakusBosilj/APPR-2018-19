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
  labs(title='Napoved števila smrti v prometu v naslednjih letih', y="Število smrti")

#Clusters (smrti_pmio_avto)
smrti_pmio_avto_2016 <- filter(smrti_pmio, Prevozno_sredstvo == 'Avto', Leto == '2016',
                          Stevilo != 'NA')
smrti_pmio_avto_2016 <- smrti_pmio_avto_2016[,-3]
smrti_pmio_avto_2016 <- smrti_pmio_avto_2016[,-1]
smrti_pmio_avto_2016 <- smrti_pmio_avto_2016[order(smrti_pmio_avto_2016$Stevilo),]

n <- 5
fit <- hclust(dist(scale(smrti_pmio_avto_2016$Stevilo)))
skupine1 <- cutree(fit, n)

cluster1 <- mutate(smrti_pmio_avto_2016, skupine1)

# plot(fit)
# rect.hclust(fit, k=5, border = 'red')


#Clusters (bdppc)
bdppc_2016 <- filter(bdppc, Leto == '2016')

bdppc_2016 <- bdppc_2016[, -1]

n <- 5
fit <- hclust(dist(scale(bdppc_2016$Kolicina_eur)))
skupine2 <- cutree(fit, n)

cluster2 <- mutate(bdppc_2016, skupine2)

#Clustere zdruzim

cluster <- left_join(cluster1, cluster2, by = c("Drzava"))
cluster <- cluster[,-4]
cluster <- cluster[,-2]
colnames(cluster)<- c("Drzava","Glede na varnost","Glede na bdppc")
