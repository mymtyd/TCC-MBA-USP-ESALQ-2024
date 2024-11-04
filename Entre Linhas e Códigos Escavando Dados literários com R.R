### Lendo Edouard Louis pela primeira vez, sem ler uma palavra após a outra ou transformando livros em dataframes (Mayumi Toyoda)

### Capítulo 1 - De 'livro' para 'dataframe'

### Instalação e Carregamento dos Pacotes Necessários
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidytext")) install.packages("tidytext")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(tidytext)
library(ggplot2)

### Ajuste do Ambiente e Leitura do Arquivo

# Definir o caminho completo para o arquivo .txt
caminho_arquivo_txt <- "C:/Users/63427/OneDrive - Bain/Desktop/A_Womans_Battles_and_Transformations.txt"

# Ler o conteúdo do arquivo .txt
texto <- readLines(caminho_arquivo_txt, warn = FALSE)

# Combinar todas as linhas em um único texto
texto_combinado <- paste(texto, collapse = " ")

# Criar um dataframe para armazenar o resultado
textos_txt <- data.frame(FileName = basename(caminho_arquivo_txt), Text = texto_combinado, stringsAsFactors = FALSE)

# Mostrar os dados do arquivo
print("Dados do arquivo .txt:")
print(textos_txt)

### Capítulo 2 - A Análise de Frequência de Palavras

### Instalação e Carregamento dos Pacotes Necessários
library(dplyr)
library(tidytext)
library(ggplot2)
if (!require("wordcloud")) install.packages("wordcloud")
library(wordcloud)

# Transformar o texto em um formato tidy para análise de texto
dados_palavras <- textos_txt %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words, by = "word")

# Contar a frequência das palavras e ordenar
palavras_frequentes <- dados_palavras %>%
  count(word, sort = TRUE) %>%
  top_n(20, n)  # obter as top 20 palavras mais frequentes

# Criar um gráfico de barras para as palavras mais frequentes
g1 <- ggplot(palavras_frequentes, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 palavras mais frequentes", x = "Palavra", y = "Frequência") +
  theme_minimal()

# Exibir o gráfico das palavras mais frequentes
print(g1)

# Criar uma nuvem de palavras para as palavras mais frequentes
wordcloud(words = palavras_frequentes$word, 
          freq = palavras_frequentes$n, 
          min.freq = 1, 
          max.words = 100, 
          random.order = FALSE, 
          rot.per = 0.35, 
          scale = c(3, 0.5),  # Scale for word sizes: c(min.size, max.size)
          colors = brewer.pal(8, "Dark2"))

### Capítulo 3 - A Modelagem de Tópicos

### Instalação e Carregamento dos Pacotes Necessários

if (!require("broom")) install.packages("broom")
if (!require("topicmodels")) install.packages("topicmodels")
if (!require("LDAvis")) install.packages("LDAvis")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidytext")) install.packages("tidytext")

library(broom)
library(topicmodels)
library(LDAvis)
library(dplyr)
library(tidytext)

### Preparação dos Dados

# Transformar o texto em um formato tidy para análise de texto
dados_palavras <- textos_txt %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words, by = "word")  # remover palavras de parada

# Criar a matriz documento-termo
dtm <- dados_palavras %>%
  count(FileName, word) %>%
  cast_dtm(document = FileName, term = word, value = n)

### Aplicação do Modelo Latent Dirichlet Allocation (LDA)

# Definir o número de tópicos
k <- 5

# Aplicar o modelo LDA
lda_model <- LDA

### Visualização dos resultados

# Instalação e Carregamento dos Pacotes
if (!require("broom")) install.packages("broom")
if (!require("topicmodels")) install.packages("topicmodels")
if (!require("LDAvis")) install.packages("LDAvis")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidytext")) install.packages("tidytext")

library(broom)
library(topicmodels)
library(LDAvis)
library(dplyr)
library(tidytext)

library(topicmodels)
library(tidytext)
library(dplyr)

# Supondo que você já tenha uma matriz documento-termo (DTM)
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Vamos verificar como você pode usar `tidy()`:
tidy_lda <- tidy(lda_model)  # Isto deve funcionar agora se o modelo lda_model é válido

# Preparação para o LDAvis, sem usar `tidy()` diretamente aqui:
dados_json <- LDAvis::createJSON(
  phi = posterior(lda_model)$terms,
  theta = posterior(lda_model)$topics,
  vocab = colnames(dtm),
  doc.length = rowSums(as.matrix(dtm)),
  term.frequency = colSums(as.matrix(dtm))
)

# Visualizar no navegador
LDAvis::serVis(dados_json, open.browser = TRUE)

### Capítulo 4 - A Análise de Sentimentos

### Instalação e Carregamento dos Pacotes Necessários
if (!require("syuzhet")) install.packages("syuzhet")
library(syuzhet)

# Preparar os dados
dados_sentimentos <- textos_txt %>%
  unnest_tokens(sentence, Text, token = "sentences") %>%
  mutate(sentiment = get_sentiment(sentence, method = "syuzhet"))

# Sumarização da análise de sentimentos
sumario_sentimentos <- dados_sentimentos %>%
  group_by(FileName) %>%
  summarise(
    media_sentimento = mean(sentiment, na.rm = TRUE),
    mediana_sentimento = median(sentiment, na.rm = TRUE)
  )

# Visualização da distribuição de sentimentos
ggplot(dados_sentimentos, aes(x = sentiment)) +
  geom_histogram(bins = 30, fill = "cornflowerblue") +
  labs(title = "Distribuição de Sentimentos", x = "Sentimento", y = "Contagem") +
  theme_minimal()

# Mostrar o sumário dos sentimentos
print("Sumário da Análise de Sentimentos:")
print(sumario_sentimentos)
