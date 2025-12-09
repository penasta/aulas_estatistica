riqinsetos<- read.csv("C:/Users/Acrol/Downloads/Riqueza de insetos.csv", sep=";")
View(riqinsetos)

tapply(riqinsetos$Espécies_insetos, riqinsetos$Habitat, shapiro.test)
bartlett.test(riqinsetos$Espécies_insetos, riqinsetos$Habitat)

res<-aov(riqinsetos$Espécies_insetos~riqinsetos$Habitat)
summary(res)

TukeyHSD(res)
kruskal.test(riqinsetos$Espécies_insetos~riqinsetos$Habitat)

library(FSA)

dunnTest(riqinsetos$Espécies_insetos~riqinsetos$Habitat)
riqinsetos$Habitat<-as.factor(riqinsetos$Habitat)

boxplot(riqinsetos$Espécies_insetos ~ riqinsetos$Habitat)
