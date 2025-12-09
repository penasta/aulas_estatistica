micro <- read.csv2("C:/Users/Acrol/Downloads/Microbiologia.csv")
View(micro)

library(readr)
micro <- read_delim("dados/Microbiologia.csv",
                    delim = ";",
                    escape_double = FALSE,
                    trim_ws = TRUE)

temp <- micro$Temperatura
cres <- micro$Tx_crescimento

plot(cres~temp,
     main = "Dispersão Taxa de Crescimento vs. Temperatura (Original)",
     xlab = "Temperatura",
     ylab = "Taxa de Crescimento")

hist(temp,
     main = "Histograma da Temperatura")

hist(cres,
     main = "Histograma da Taxa de Crescimento")

mod1 <- lm(cres~temp)
mod2 <- lm(Tx_crescimento~Temperatura, data = micro)
mod3 <- lm(micro$Tx_crescimento~micro$Temperatura)
attach(micro)
mod4 <- lm(Tx_crescimento~Temperatura)
detach(micro)

mod

summary(micro)

summary(mod)

plot(cres~temp,
     main = "Dispersão Taxa de Crescimento vs. Temperatura (Original)",
     xlab = "Temperatura",
     ylab = "Taxa de Crescimento")
abline(mod)

influence.measures(mod)

shapiro.test(mod$residuals)

plot(mod)

mod_final <- lm(cres~temp)

plot(mod_final, which = 1)
plot(mod_final, which = 4)

# Com base na análise estatística exploratória do conjunto de dados,
# concluímos que não há outliers significativos nas variáveis
# Temperatura e Tx_crescimento



