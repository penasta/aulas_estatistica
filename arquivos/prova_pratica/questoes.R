# questão 1 ----

# idade X cap pulmonar

# hipótese:
# h0) não tem diferença
# h1) existe diferença.

library(readr)
cp <- read_delim("dados/Capacidade pulmonar.csv",
                 delim = ";",
                 escape_double = FALSE,
                 trim_ws = TRUE)

colnames(cp) <- c("idade","capacidade")

attach(cp)
plot(idade~capacidade)

cor.test(idade,capacidade, method = "spearman")

shapiro.test(idade)
shapiro.test(capacidade)

cor.test(idade,capacidade, method = "pearson")


















cp<-read.csv("C:/Users/Acrol/Downloads/Capacidade pulmonar.csv", sep=";")
View(cp)
plot(cp)
apply(cp, 2, shapiro.test)
cor.test(cp$Idade, cp$VEF_m1, method = "spearman")
# (S = 30949; P < 0,001; rs = -0,90)

# Questão 2 ----

library(readr)
lagartos <- read_delim("dados/Lagartos.csv",
                       delim = ";",
                       escape_double = FALSE,
                       trim_ws = TRUE)

library(readr)
lagartos <- read_delim("arquivos/prova_pratica/Lagartos.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)


# hipóteses:
# h0) comprimento não difere entre espécies
# h1) difere

colnames(lagartos) <- c("especie","comprimento")
attach(lagartos)

# Normalidade 
tapply(comprimento,especie,shapiro.test)

# Homocedasticidade
bartlett.test(comprimento,especie)

anova <- aov(comprimento~especie)

summary(anova)

TukeyHSD(anova)

boxplot(comprimento~especie)

kw <- kruskal.test(comprimento,especie)

dunnTest(comprimento~especie)






lagartos<-read.csv2("C:/Users/Acrol/Downloads/Lagartos.csv")
View(lagartos)
boxplot(lagartos$Comprimento~lagartos$Espécie)
tapply(lagartos$Comprimento, lagartos$Espécie, shapiro.test)
bartlett.test(lagartos$Comprimento~lagartos$Espécie)
kruskal.test(lagartos$Comprimento~lagartos$Espécie)
# (x² = 12,83; P = 0,002)
library(FSA)
dunnTest(lagartos$Comprimento ~ lagartos$Espécie)


# Questão 3 ----

library(readr)
bac <- read_delim("dados/Bactérias.csv", 
                        delim = ";",
                  escape_double = FALSE,
                  trim_ws = TRUE)

library(readr)
bac <- read_delim("arquivos/prova_pratica/Bactérias.csv",
                  delim = ";",
                  escape_double = FALSE,
                  trim_ws = TRUE)


# Hipóteses:
# h0) temperatura NÃO influencia no crescimento
# h1) influencia

colnames(bac) <- c("crescimento","temperatura")
attach(bac)

mod <- lm(crescimento~temperatura, data = bac)

summary(mod)
(-28.6908) + (27.9 * 1.7858)
# qual o tamanho previsto para colônias mantidas a 27.9ºC

plot(mod)

shapiro.test(mod$residuals)

influence.measures(mod$residuals)

# Resposta: 21.13302 mm





bac<-read.csv2("C:/Users/Acrol/Downloads/Bactérias.csv")
View(bac)
plot(bac)
mod<-lm(bac$Crescimento_mm~bac$Temperatura)
summary(mod)
influence.measures(mod)

bac2 <- bac[-c(1), ]
plot(bac2)
mod2 <- lm(bac2$Crescimento_mm ~ bac2$Temperatura)
summary(mod2)
plot(mod2)
plot(bac2$Crescimento_mm ~ bac2$Temperatura, xlim=c(0,40), ylim=c(-40,45))
abline(mod2)
shapiro.test(mod2$residuals)

y = (-29.9482) + 1.9269*27.9

mod2$fitted.values
