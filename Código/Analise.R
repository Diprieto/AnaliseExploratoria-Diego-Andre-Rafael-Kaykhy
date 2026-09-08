# 1. Carregamento dos pacotes
install.packages(c("data.table", "dplyr", "ggplot2"))
library(data.table)
library(dplyr)
library(ggplot2)

# 2. Leitura do Corpus otimizado (.rds)
dados <- readRDS("../Corpus/corpus_exportacao.rds")

# (Opcional) Caso o seu fluxo exija ler o CSV original em algum momento, 
# certifique-se de que o CSV também está na pasta correta ou utilize o RDS diretamente.

# 3. Conversão otimizada de toneladas para número decimal
dados <- dados %>%
  mutate(TOTAL_TONELADAS = as.numeric(gsub(",", ".", TOTAL_TONELADAS, fixed = TRUE)))
# 4. Inspeção inicial da estrutura (Substitui o spec)
glimpse(dados)

# 5. Resumo estatístico
summary(dados)

# 6. Data mais antiga e mais recente
range(dados$ANO_MES, na.rm = TRUE)

# 7. Contagem de valores únicos para variáveis de texto
dados %>% summarise_if(is.character, n_distinct)

# 8. Contagem de valores nulos (NA)
colSums(is.na(dados))

# Criando um ranking dos terminais
ranking_terminais <- dados %>%
  group_by(TERMINAIS) %>%
  summarise(SOMA_TONELADAS = sum(TOTAL_TONELADAS, na.rm = TRUE)) %>%
  arrange(desc(SOMA_TONELADAS))

# Exibe os 10 primeiros colocados
head(ranking_terminais, 10)

library(ggplot2)
library(dplyr)

# 1. Preparar os dados: Filtrando 2026 e agrupando
evolucao_navegacao <- dados %>%
  filter(ANO < 2026) %>% # Corta qualquer dado de 2026 em diante
  group_by(ANO, TIPO_NAVEGACAO) %>%
  summarise(SOMA_TONELADAS = sum(TOTAL_TONELADAS, na.rm = TRUE), .groups = 'drop') %>%
  mutate(SOMA_TONELADAS_MILHOES = SOMA_TONELADAS / 1000000)

# 2. Criar o gráfico com duas linhas
ggplot(evolucao_navegacao, aes(x = ANO, y = SOMA_TONELADAS_MILHOES, color = TIPO_NAVEGACAO)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Evolução Anual por Tipo de Navegação",
    subtitle = "Comparativo: Mercado Interno (Cabotagem) vs Internacional (Longo Curso)",
    x = "Ano",
    y = "Milhões de Toneladas",
    color = "Tipo de Navegação"
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, by = 2)) +
  scale_color_manual(values = c("CABOTAGEM" = "#1b9e77", "LONGO CURSO" = "#d95f02")) +
  theme_minimal() +
  theme(legend.position = "bottom")

  # 1. Preparando a matemática de porcentagem ano a ano
evolucao_porcentagem <- dados %>%
  # PASSO NOVO: Filtra para remover 2026 antes de qualquer cálculo
  filter(ANO < 2026) %>%

  # Passo 1: Calcula o total de toneladas por Ano E por Categoria
  group_by(ANO, NATUREZA_CARGA) %>%
  summarise(SOMA_TONELADAS = sum(TOTAL_TONELADAS, na.rm = TRUE), .groups = 'drop') %>%

  # Passo 2: Agrupa APENAS por Ano para calcular a fatia (porcentagem) dentro daquele ano
  group_by(ANO) %>%
  mutate(PORCENTAGEM = (SOMA_TONELADAS / sum(SOMA_TONELADAS)) * 100)

# 2. Criando o gráfico de evolução percentual
ggplot(evolucao_porcentagem, aes(x = ANO, y = PORCENTAGEM, color = NATUREZA_CARGA)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +

  labs(
    title = "Evolução da Participação por Natureza da Carga",
    subtitle = "Como cada categoria dividiu o mercado de exportação (2005 a 2025)", # Subtítulo atualizado
    x = "Ano",
    y = "Participação no Ano (%)",
    color = "Natureza da Carga"
  ) +

  # Ajusta o eixo X para ir apenas até 2025
  scale_x_continuous(breaks = seq(2005, 2025, by = 2)) +

  # Define cores distintas e vibrantes usando a paleta 'Set1' do ggplot
  scale_color_brewer(palette = "Set1") +

  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

  # 1. Ajusta as margens (o '12' aumenta o espaço inferior) e mantém o formato quadrado
par(mar = c(12, 4, 4, 2) + 0.1, pty="s")

# 2. Filtra apenas valores maiores que zero
dados_filtrados <- subset(dados, TOTAL_TONELADAS > 0)

# 3. Cria o boxplot com as legendas na vertical
boxplot(log10(dados_filtrados$TOTAL_TONELADAS) ~ dados_filtrados$NATUREZA_CARGA,
        col = "lightblue",
        main = "Boxplot: Toneladas por Natureza",
        ylab = "Log10(Toneladas)",
        xlab = "",        # Deixamos vazio para o título não encavalar com os nomes das cargas
        las = 2,          # Deixa os textos do eixo X na vertical
        cex.axis = 0.8)   # Diminui a fonte levemente (20% menor) para caber melhor

# Calculando a média e a mediana exatas da base de dados
resumo_assimetria <- dados %>%
  summarise(
    Media = mean(TOTAL_TONELADAS, na.rm = TRUE),
    Mediana = median(TOTAL_TONELADAS, na.rm = TRUE),
    Diferenca = mean(TOTAL_TONELADAS, na.rm = TRUE) - median(TOTAL_TONELADAS, na.rm = TRUE)
  )

# Exibe o resultado na tela
resumo_assimetria