ter <- read.csv2("C:/Users/Acrol/Downloads/Território.csv")
View(ter)

plot(ter$Território~ter$Tamanho_bico)
plot(ter$Território~ter$Nun_notas)
boxplot(ter$Território~ter$Sexo)
boxplot(ter$Território~ter$Idade)

modelo<-lm(ter$Território~ter$Tamanho_bico+ter$Nun_notas+ter$Sexo+ter$Idade)
summary(modelo)

modelo2<-lm(ter$Território~ter$Tamanho_bico+ter$Sexo+ter$Idade)
summary(modelo2)

modelo3<-lm(ter$Território~ter$Tamanho_bico+ter$Sexo*ter$Idade)
summary(modelo3)

