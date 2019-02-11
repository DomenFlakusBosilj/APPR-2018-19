source("lib/libraries.r", encoding="UTF-8")

# osnovni.podatki <- rbind(osnovni.podatki.poroke, osnovni.podatki.razveze)
# 
# ociscena <- subset(osnovni.podatki, spremenljivka == "Sklenitve zakonskih zvez - Skupaj")
# fit <- lm(data = ociscena, vrednost ~ leto)
# a <- data.frame(leto=seq(2016, 2030, 2))
# predict(fit, a)
# napoved <- a %>% mutate(vrednost=predict(fit, .))
# graf5 <- ggplot(ociscena, aes(x=leto, y=vrednost)) +
#   geom_smooth(method=lm, se=FALSE, fullrange = TRUE) +
#   geom_point(data=napoved, aes(x=leto, y=vrednost), color="orange", size=3) +
#   labs(title="Napoved števila porok", y="Število porok") + geom_point()

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