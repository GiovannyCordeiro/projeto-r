# Lê os dados do CSV
data <- read.csv("./csvs/organizations-100.csv")
data$Decade <- floor(data$Founded / 10) * 10

# Calcular média de funcionários por década
avg_employees <- aggregate(data$Number.of.employees,
                           by = list(Decade = data$Decade),
                           FUN = mean)

# Plotar gráfico de linha
plot(avg_employees$Decade, avg_employees$x,
     type = "o", lwd = 2, col = "darkblue", pch = 16,
     xlab = "Década de Fundação",
     ylab = "Média de Funcionários",
     main = "Evolução do Porte Médio das Empresas por Década")

