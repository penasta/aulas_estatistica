borboleta <- read.delim("C:/Users/Acrol/Downloads/Borboleta.txt")

library(readr)
borboleta <- read_table("dados/Borboleta.txt")

View(borboleta)

colnames(borboleta)<-c("Ovos", "Especie")

borboleta$Especie <- as.factor(borboleta$Especie)

tapply(borboleta$Ovos,borboleta$Especie, shapiro.test)

bartlett.test(borboleta$Ovos~borboleta$Especie)

res_anova_borboleta <- aov(borboleta$Ovos~borboleta$Especie)

summary(res_anova_borboleta)

TukeyHSD(res_anova_borboleta)

kruskal.test(borboleta$Ovos~borboleta$Especie)

library(FSA)

dunnTest(borboleta$Ovos~borboleta$Especie,method="bonferroni")

boxplot(borboleta$Ovos~borboleta$Especie,
        main="Ovos de H. erato por Espécie de Passiflora",
        xlab="Espécie de Passiflora",
        ylab="Número de Ovos Depositados")
