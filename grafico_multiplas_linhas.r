# Nascimento de homem a cada decada
dados <- read.csv("./csvs/people-100.csv", stringsAsFactors = FALSE)

# Extrair ano de nascimento
dados$Ano <- as.numeric(format(as.Date(dados$Date.of.birth), "%Y"))

# Calcular a década
dados$Decada <- floor(dados$Ano / 10) * 10

# Contagem por década e sexo
tabela <- table(dados$Decada, dados$Sex)
decadas <- as.numeric(rownames(tabela))

# Criar gráfico
plot(decadas, tabela[, "Male"], type = "l", col = "blue", lwd = 2,
     xlab = "Década de nascimento", ylab = "Número de nascimentos",
     main = "Nascimentos por década (Homens vs Mulheres)",
     ylim = range(tabela))
lines(decadas, tabela[, "Female"], col = "red", lwd = 2)

legend("topright", legend = c("Homens", "Mulheres"),
       col = c("blue", "red"), lwd = 2)

# pdf("grafico_multiplas_linhas.pdf")
