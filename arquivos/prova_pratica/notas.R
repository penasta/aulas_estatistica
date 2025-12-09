notas <- read.delim("C:/Users/Acrol/Downloads/Notas.txt")
View(notas)

library(readr)
notas <- read_table("dados/Notas.txt")

plot(notas$Matematica,
     notas$Bioestatistica,
     main="Gráfico de Dispersão das Notas",
     xlab="Nota de Matemática",
     ylab="Nota de Bioestatística",
     col="blue",
     pch=19)

abline(lm(notas$Bioestatistica ~ notas$Matematica), col="red", lwd=2)

shapiro.test(notas$Matematica)
shapiro.test(notas$Bioestatistica)

cor(notas$Matematica, notas$Bioestatistica)

cor.test(notas$Matematica, notas$Bioestatistica, method = "pearson")

# Não há evidências estatísticas significativas para afirmar que existe
# uma correlação linear entre as notas de matemática e bioestatística
# para este grupo de 40 alunos (r=0.18,p=0.28).

# Apesar da tendência de notas levemente maiores em bioestatística
# quando as de matemática aumentam, essa relação é muito fraca
# e pode ser devida ao acaso.
