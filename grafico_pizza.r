library(ggplot2) #estlizar
library(dplyr) #manipulação de dados
library(RColorBrewer) #cor/paleta

dados <- read.csv("./csvs/products-100.csv")

tabela_categorias <- table(dados$Category)
percentuais <- prop.table(tabela_categorias) * 100

limite <- 5

df <- as.data.frame(percentuais)
colnames(df) <- c("Categoria", "Percentual")

df <- df %>%
  mutate(CategoriaSimplificada = ifelse(Percentual < limite, "Outros", as.character(Categoria))) %>%
  group_by(CategoriaSimplificada) %>%
  summarise(Percentual = sum(Percentual)) %>%
  arrange(desc(Percentual)) %>%
  mutate(Porcentagem = paste0(round(Percentual, 1), "%"))


traducao <- c(
  "Women's Clothing" = "Roupas Femininas",
  "Office Supplies" = "Material de escritório",
  "Kids' Clothing" = "Roupas infantis",
  "Health & Wellness" = "Saúde e bem-estar",
  "Cleaning Supplies" = "Materiais de limpeza",
  "Automotive" = "Automotivos",
  "Outros" = "Outros"
)

df$CategoriaTraduzida <- traducao[df$CategoriaSimplificada]
df$CategoriaTraduzida[is.na(df$CategoriaTraduzida)] <- df$CategoriaSimplificada[is.na(df$CategoriaTraduzida)]

png(filename = "grafico_pizza_categoria.png", width = 800, height = 800, res = 150)

ggplot(df, aes(x = "", y = Percentual, fill = CategoriaTraduzida)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = Porcentagem),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "black") +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  ggtitle("Produtos por Categoria (agrupado)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.position = "right"
  )

dev.off()
