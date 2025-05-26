library(ggplot2)

people <- read.csv("./csvs/people-100.csv", stringsAsFactors = FALSE)

df <- as.data.frame(table(people$Sex))
colnames(df) <- c("Sexo", "Quantidade")


df$Sexo <- as.character(df$Sexo)

df$Sexo[df$Sexo == "Female"] <- "Feminino"
df$Sexo[df$Sexo == "Male"] <- "Masculino"

limite_y <- max(df$Quantidade) * 1.1

png(filename = "grafico_barras_sexo.png", width = 600, height = 400, res = 150)

ggplot(df, aes(x = Sexo, y = Quantidade, fill = Sexo)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.7) +
  geom_text(aes(label = Quantidade), vjust = -0.5, size = 2.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Feminino" = "#FF69B4",
    "Masculino" = "#003366"
  )) +
  labs(title = "Quantidade de Pessoas por Sexo", x = "Sexo", y = "Quantidade") +
  ylim(0, limite_y) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

dev.off()