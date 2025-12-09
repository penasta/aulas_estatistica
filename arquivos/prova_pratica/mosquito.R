dados_mosquitos <- read.csv("C:/Users/Acrol/Downloads/Mosquitos.txt", sep="\t") 
View(dados_mosquitos)

colnames(dados_mosquitos) <- c("Picadas", "Cabelo")
dados_mosquitos$Cabelo <- as.factor(dados_mosquitos$Cabelo)

tapply(dados_mosquitos$Picadas, dados_mosquitos$Cabelo, shapiro.test)

bartlett.test(dados_mosquitos$Picadas ~ dados_mosquitos$Cabelo)

res_anova <- aov(dados_mosquitos$Picadas ~ dados_mosquitos$Cabelo)
summary(res_anova)

TukeyHSD(res_anova)

kruskal.test(dados_mosquitos$Picadas ~ dados_mosquitos$Cabelo)

library(FSA)
dunnTest(dados_mosquitos$Picadas ~ dados_mosquitos$Cabelo, method="bonferroni")

boxplot(dados_mosquitos$Picadas ~ dados_mosquitos$Cabelo,
        main="Número de Picadas de Mosquito por Cor do Cabelo",
        xlab="Cor do Cabelo",
        ylab="Picadas")
