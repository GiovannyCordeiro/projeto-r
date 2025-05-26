# Análise: Receita Potencial por Categoria e Status de Disponibilidade
# Carregando os dados
data <- read.csv("./csvs/products-100.csv", stringsAsFactors = FALSE)

# Calculando receita potencial (Price * Stock)
data$Revenue_Potential <- data$Price * data$Stock

# Identificando as 10 categorias com maior receita potencial
top_categories <- aggregate(Revenue_Potential ~ Category, data = data, FUN = sum)
top_categories <- top_categories[order(top_categories$Revenue_Potential, decreasing = TRUE), ]
top_10_categories <- head(top_categories$Category, 10)

# Filtrando dados apenas para as top 10 categorias
data_filtered <- data[data$Category %in% top_10_categories, ]

# Agregando dados por categoria e disponibilidade (apenas top 10)
revenue_summary <- aggregate(Revenue_Potential ~ Category + Availability,
                           data = data_filtered, FUN = sum)

# Preparando dados para gráfico de barras múltiplas (apenas top 10)
categories <- top_10_categories
availability_types <- c("in_stock", "out_of_stock", "limited_stock", "discontinued")

# Criando matriz para o gráfico
revenue_matrix <- matrix(0, nrow = length(availability_types), ncol = length(categories))
rownames(revenue_matrix) <- availability_types
colnames(revenue_matrix) <- categories

# Preenchendo a matriz
for(i in 1:nrow(revenue_summary)) {
  cat <- revenue_summary$Category[i]
  avail <- revenue_summary$Availability[i]
  if(avail %in% availability_types) {
    row_idx <- which(availability_types == avail)
    col_idx <- which(categories == cat)
    revenue_matrix[row_idx, col_idx] <- revenue_summary$Revenue_Potential[i]
  }
}

# Definindo cores
colors <- c("in_stock" = "#2E8B57", "out_of_stock" = "#DC143C",
            "limited_stock" = "#FFD700", "discontinued" = "#696969")

# Criando o gráfico
par(mar = c(12, 5, 4, 8))

barplot(revenue_matrix / 1000,
        beside = TRUE,
        col = colors,
        main = "Top 10 Categorias - Receita Potencial (em milhares USD)",
        ylab = "Receita Potencial (milhares USD)",
        las = 2,
        cex.names = 0.7)

# Legenda
legend("topright", inset = c(-0.15, 0),
       legend = c("Em Estoque", "Fora de Estoque", "Estoque Limitado", "Descontinuado"),
       fill = colors,
       cex = 0.8)
