# Análise Exploratória de Dados de Exportação

<!-- markdown -->
### Demonstra as 16 colunas
Abaixo encontra-se a inspeção inicial da estrutura do dataset utilizando a função `glimpse()`, revelando todas as colunas disponíveis para análise:

```r
glimpse(dados)
```

---

<!-- markdown -->
### Aqui confirmamos que não existem valores nulos na tabela, mas as variáveis de double apresentam uma média assimétrica positiva muito grande
Através da checagem de soma de valores nulos e do resumo estatístico descritivo:

```r
colSums(is.na(dados))
summary(dados)
```

---

<!-- markdown -->
### Demonstrando as maiores exportadoras
Calculando o ranking dos terminais com base no somatório das toneladas movimentadas:

```r
ranking_terminais <- dados %>%
  group_by(TERMINAIS) %>%
  summarise(SOMA_TONELADAS = sum(TOTAL_TONELADAS, na.rm = TRUE)) %>%
  arrange(desc(SOMA_TONELADAS))

head(ranking_terminais, 10)
```

---

<!-- markdown -->
### Este gráfico demonstra a grande diferença entre navegações internacionais e nacionais, filtrando 2026.

```r
evolucao_navegacao <- dados %>%
  filter(ANO < 2026) %>%
  group_by(ANO, TIPO_NAVEGACAO) %>%
  summarise(SOMA_TONELADAS = sum(TOTAL_TONELADAS, na.rm = TRUE), .groups = 'drop') %>%
  mutate(SOMA_TONELADAS_MILHOES = SOMA_TONELADAS / 1000000)

ggplot(evolucao_navegacao, aes(x = ANO, y = SOMA_TONELADAS_MILHOES, color = TIPO_NAVEGACAO)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Evolução Anual por Tipo de Navegação",
    subtitle = "Comparativo: Mercado Interno (Cabotagem) vs Internacional (Longo Curso)",
    x = "Ano", y = "Milhões de Toneladas", color = "Tipo de Navegação"
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, by = 2)) +
  scale_color_manual(values = c("CABOTAGEM" = "#1b9e77", "LONGO CURSO" = "#d95f02")) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

---

<!-- markdown -->
### Aqui demonstra o porquê da média ser muito acima da mediana, pois as cargas em granel "inflam" o resultado.
### Forte assimetria entre média e mediana
### Valores demonstrando teto sem os outliers

```r
par(mar = c(12, 4, 4, 2) + 0.1, pty="s")
dados_filtrados <- subset(dados, TOTAL_TONELADAS > 0)

boxplot(log10(dados_filtrados$TOTAL_TONELADAS) ~ dados_filtrados$NATUREZA_CARGA,
        col = "lightblue",
        main = "Boxplot: Toneladas por Natureza",
        ylab = "Log10(Toneladas)",
        xlab = "",
        las = 2,
        cex.axis = 0.8)
```

---

<!-- markdown -->
### A distribuição do log é simétrica, portanto é uma distribuição normal!

```r
resumo_assimetria <- dados %>%
  summarise(
    Media = mean(TOTAL_TONELADAS, na.rm = TRUE),
    Mediana = median(TOTAL_TONELADAS, na.rm = TRUE),
    Diferenca = mean(TOTAL_TONELADAS, na.rm = TRUE) - median(TOTAL_TONELADAS, na.rm = TRUE)
  )

resumo_assimetria
```
