# Pacotes
library(dplyr)
library(tibble)
library(ggplot2)

# Carregar dataset
ceaps <- read.csv2("ceaps_22.csv",
                   sep = ";",
                   dec = ".",
                   header = FALSE)

# Removendo valores inconsistentes
ceaps <- ceaps[-c(1,2), ]

# Valores ausentes
ceaps[ceaps == ""] <- NA
sum(is.na(ceaps))
sum(is.null(ceaps))

# Ajustando formado dos dados
ceaps$V10 <- as.numeric(ceaps$V10)

# Renomear as variáveis 
ceaps <- ceaps %>%
  rename(ano = 1,
         mes = 2,
         senador = 3,
         tipo_despesa = 4,
         cnpj_cpf = 5,
         fornecedor = 6,
         documento = 7,
         data = 8,
         detalhamento = 9,
         valor_reembolso = 10,
         cod_documento = 11)

# Verificar valores unicos de algumas variáveis
unique(ceaps$senador) 
unique(ceaps$tipo_despesa) 
unique(ceaps$fornecedor) # 18274 fornecedores
unique(ceaps$valor_reembolso) # 67547 detalhamentos

# As observações da variável "tipo de despesa" estão muito extensas, irei encurtar para facilitar análises futuras
ceaps <- mutate(ceaps,
                nova_despesa = recode(tipo_despesa,
                                      "Aluguel de imóveis para escritório político, compreendendo despesas concernentes a eles." = "Aluguel imóveis escritórios",
                                      "Divulgação da atividade parlamentar" = "Divulgação atividade parlamentar",
                                      "Aquisição de material de consumo para uso no escritório político, inclusive aquisição ou locação de software, despesas postais, aquisição de publicações, locação de móveis e de equipamentos. " = "Aquisição material escritorio",
                                      "Passagens aéreas, aquáticas e terrestres nacionais" = "Passagens",
                                      "Contratação de consultorias, assessorias, pesquisas, trabalhos técnicos e outros serviços de apoio ao exercício do mandato parlamentar" = "Contratação serviço apoio",
                                      "Locomoção, hospedagem, alimentação, combustíveis e lubrificantes" = "Gastos viagens",
                                      "Serviços de Segurança Privada" =  "Serviços de Segurança"))

# Vou criar uma nova variável com os nomes dos meses para facilitar procedimentos futuros
ceaps <- mutate(ceaps,
                novo_mes = recode(mes,
                                  '1' = "Janeiro",
                                  '2' = "Fevereiro",
                                  '3' = "Março",
                                  '4' = "Abril",
                                  '5' = "Maio",
                                  '6' = "Junho",
                                  '7' = "Julho",
                                  '8' = "Agosto",
                                  '9' = "Setembro",
                                  '10' = "Outubro",
                                  '11' = "Novembro",
                                  '12' = "Dezembro"))
df <- ceaps[ ,-c(1,2,4,7,11)]

# Reordenando as variáveis
df <- df[ ,c(4,8,1,7,2,3,5,6)]

# Remoção simbolos de escrita variável fornecedor
df$fornecedor <- gsub("^[-:),.–'´\"]+", "", df$fornecedor)

# Remoção simbolos de escrita variável detalhamento
ceaps$detalhamento <- gsub("^[-:),.–´'\" ]+|[-:),.–'\" ]+$", "", ceaps$detalhamento)

# Verificando e removendo valores inconsistentes na variável reembolso
df_filtered <- df %>%
  filter(!is.na(valor_reembolso) & !is.null(valor_reembolso) & valor_reembolso >= 0)


# Como a variável detalhamento está com texto muito extenso, irei dividi-los em subcategorias
# Criar duas novas variáveis para as partes antes e depois da vírgula
df_filtered$antes_virgula <- sapply(df_filtered$detalhamento, function(texto) {
  partes <- unlist(strsplit(texto, ","))
  if (length(partes) > 1) {
    antes <- trimws(partes[1])
    return(antes)
  } else {
    return(texto) # Se não houver vírgula, mantenha o texto original
  }
})

df_filtered$apos_virgula <- sapply(df_filtered$detalhamento, function(texto) {
  partes <- unlist(strsplit(texto, ","))
  if (length(partes) > 1) {
    apos <- trimws(partes[2])
    return(apos)
  } else {
    return(NULL)
  }
})

# Remoção das palavras 'do senador' para padronização
extrair_palavra <- function(texto) {
  partes <- unlist(strsplit(texto, ","))
  if (length(partes) > 1) {
    antes <- trimws(partes[1])
    antes <- sub("do senador.*", "", antes)
    return(antes)
  } else {
    antes <- sub("do senador.*", "", texto)
    return(antes)
  }
}
df_filtered$antes_virgula <- sapply(df_filtered$detalhamento, extrair_palavra)

# Remoção de URL
remover_urls <- function(texto) {
  texto <- gsub("www\\.[a-zA-Z0-9]+\\.[a-zA-Z0-9]+(\\.[a-zA-Z0-9]+)*", "", texto)
  texto <- gsub(":www\\.[a-zA-Z0-9]+\\.[a-zA-Z0-9]+(\\.[a-zA-Z0-9]+)*", "", texto)
  texto <- gsub("no website:www\\.[a-zA-Z0-9]+\\.[a-zA-Z0-9]+(\\.[a-zA-Z0-9]+)*", "", texto)
  
  return(texto)
}
df_filtered$antes_virgula <- sapply(df_filtered$antes_virgula, remover_urls)

# Removendo as variáveis que não serão mais úteis
df_filtered <- df_filtered[ ,-c(7, 10)]
df_filtered <- df_filtered %>% 
  rename(detalhamento = antes_virgula) %>% 
  relocate(detalhamento, .after = fornecedor)

## Tabela Frequências
# Frequencia pedido reembolso por senador
freq_senador <- df_filtered %>%
  group_by(senador) %>%
  summarise(frequência = n()) %>%
  arrange(desc(frequência))

# Frequencia pedido reembolso por mês
freq_mês <- df_filtered %>%
  group_by(novo_mes) %>%
  summarise(frequência = n()) %>%
  arrange(desc(frequência))

# Frequencia pedido reembolso por tipo despesa
freq_tipo_despesa <- df_filtered %>%
  group_by(nova_despesa) %>%
  summarise(frequência = n()) %>%
  arrange(desc(frequência))

# Total gasto por senador
gasto_senador <- df_filtered %>%
  group_by(senador) %>%
  summarise(total_gasto = sum(valor_reembolso)) %>%
  arrange(desc(total_gasto))

# Total gasto por mês
gasto_mes <- df_filtered %>%
  group_by(novo_mes) %>%
  summarise(total_gasto = sum(valor_reembolso)) %>%
  arrange(desc(total_gasto))

# Total gasto por tipo despesa
gasto_despesa <- df_filtered %>%
  group_by(nova_despesa) %>%
  summarise(total_gasto = sum(valor_reembolso)) %>%
  arrange(desc(total_gasto))

## Estatística descritiva
# Estatística pedido reembolso por senador
descritiva_senador <- df_filtered %>%
  group_by(senador) %>%
  summarise(média = round(mean(valor_reembolso),2),
            mediana = median(valor_reembolso),
            mínimo = min(valor_reembolso),
            máximo = max(valor_reembolso))

# Estatística pedido reembolso por mês
descritiva_mes <- df_filtered %>%
  group_by(novo_mes) %>%
  summarise(média = round(mean(valor_reembolso),2),
            mediana = median(valor_reembolso),
            mínimo = min(valor_reembolso),
            máximo = max(valor_reembolso))

# Estatística pedido reembolso por tipo despesa
descritiva_despesa <- df_filtered %>%
  group_by(nova_despesa) %>%
  summarise(média = round(mean(valor_reembolso),2),
            mediana = median(valor_reembolso),
            mínimo = min(valor_reembolso),
            máximo = max(valor_reembolso)) 

## Gráficos
plot_senador <- descritiva_senador %>%
  arrange(desc(média)) %>%
  head(10)
ggplot(plot_senador, aes(x = reorder(senador, média), y = média)) +
  geom_bar(stat = "identity", fill = "coral2", width = 0.7) +
  labs(title = "Senadores com os maiores valores médios de reembolso",
       x = NULL,
       y = NULL,
       caption = "Ano: 2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste("R$", média)), vjust = -0.5, color = "black", size = 4)


plot_mes <- descritiva_mes %>%
  arrange(desc(média)) %>%
  head(12)
ggplot(plot_mes, aes(x = reorder(novo_mes, média), y = média)) +
  geom_bar(stat = "identity", fill = "cadetblue", width = 0.7) +
  labs(title = "Valor médio de reembolso por mês",
       x = NULL,
       y = NULL,
       caption = "Ano: 2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste("R$", média)), vjust = -0.5, color = "black", size = 4)


plot_despesa <- descritiva_despesa %>%
  arrange(desc(média)) %>%
  head(7)
ggplot(plot_despesa, aes(x = reorder(nova_despesa, média), y = média)) +
  geom_bar(stat = "identity", fill = "darkseagreen2", width = 0.7) +
  labs(title = "Valor médio de reembolso por tipo de despesa",
       x = NULL,
       y = NULL,
       caption = "Ano: 2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste("R$", média)), vjust = -0.5, color = "black", size = 4)
