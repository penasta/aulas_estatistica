sementes <- read.csv("C:/Users/Acrol/Downloads/Sementes.csv", sep=";")
View(sementes)

sementes$Fenotipo <- paste(sementes$Cor,
                           sementes$Textura)

obs <- table(sementes$Fenotipo)
print(obs)

esp <- c("Amarela Lisa" = 9/16,
         "Amarela Rugosa" = 3/16,
         "Verde Lisa" = 3/16,
         "Verde Rugosa" = 1/16)

esp <- esp[names(obs)]
print(esp)

kiquadrado <- chisq.test(x = obs,
                         p = esp)

print(kiquadrado)

cores <- c("yellow", "orange", "green", "darkgreen")

rotulos <- paste(names(obs), "\n", obs)

pie(obs,
    labels = rotulos,
    main = "Distribuição de Sementes Observada",
    col = cores)
