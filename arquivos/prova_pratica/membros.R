membros <- read.csv("C:/Users/Acrol/Downloads/membros.csv", sep=";")
View(membros)

tab <- table(membros$Sexo, membros$Membro.dominante)
chisq.test(tab)

cores <- c("orange", "darkgreen")

barplot(tab,
        beside = T,
        col = cores,
        xlab = "Membro Dominante",
        ylab = "Frequência",
        legend.text = rownames(tab))
