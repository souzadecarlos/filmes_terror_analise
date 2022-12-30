# Ideias iniciais:
# Verificar quais filmes possuem maiores e menores notas
# Analisar como as notas dos filmes variam em relação ao tempo
# Analisar quais são as subcategorias de filmes que mais aparecem
# e como isso varia de acordo com o tempo


library(tidyverse)

data_basic_raw <- read_rds("dados/data_basic_horror")

# Como iremos analisar as notas e reviews dos filmes, é necessário unificar
# o data set dos dados básicos com a base de dados de notas: 

data_rating_raw <- read_rds("dados/data_rating_horror")

data_basic_rating <- left_join(data_basic_raw, data_rating_raw)


# Adicionando diretores:

data_director <- read_rds("dados/data_director_horror")

data_rating_director <- right_join(data_basic_rating, data_director) 

data_rating_director <- data_rating_director |> 
  filter(isAdult == 0) |> 
  select(-c(nconst, isAdult)) |> 
  rename(directorName = primaryName)

# Conhecendo as variáveis do dataset:

glimpse(data_rating_director)

# tconst é um id unico de cada filme presente na base de dados
# titleType: classificação em relação ao tipo de midia do video
# primaryTitle: Nome do título em inglês
# originalTitle: Nome do titulo na língua nativa de produção
# isAdult: classificação binomial para filme adulto (0 - Não, 1 - Sim)
# startYear: ano de inicio das produções
# endYear: ano de término das produções
# runtimeMinutes: Duração total do título (min)
# genres: gêneros e subgêneros de classificação do filme
# averageRating: média de pontuação que o filme recebeu
# numVotes: numero de avaliações

# Inicialmente é necessário limpar os dados. 
# Como trata-se de filmes, não faz muito sentido ter dados para inicio e finalização, como é o caso de 
# séries, portanto, podemos remover a coluna endYear e renomear a coluna startYear para releaseYear.
# Além disso, a ausência de alguns dados no dataset é represetado pelo o caractere "\\N". 
# Para continuar a análise, é necessário converte-los para NA.
# Além disso, de acordo com o American Film Institute, um filme de longa metragem deve ter 
# duração superior a 40 minutos. 
# Assim, podemos filtrar também aqueles que se encaixam nessa categoria.

transformar_na <- function(coluna) {
  na_if(coluna, "\\N")
}

data <- data_rating_director |> 
  select(-endYear) |> 
  rename(releaseYear = startYear) |> 
  mutate(across(.fns = transformar_na)) |> 
  mutate(runtimeMinutes = as.numeric(runtimeMinutes)) |>
  mutate(releaseYear = as.numeric(releaseYear)) |> 
  filter(runtimeMinutes > 40)


# mutate(runtimeMinutes = na_if(runtimeMinutes, "\\N")) (outra possibilidade)

glimpse(data)

# Fazendo uma exploração inicial, podemos identificar os filmes mais longos e com melhores pontuações

data |> 
  arrange(desc(runtimeMinutes)) |> 
  View()

# O filme com maior duração é a obra "24h Pscycho". Essa instalação artística foi criada
# por Janet Leigh, em 1993, e exibiu o filme "Psicose" a 2 frames por segundo, resultando em um filme
# de exatamente 24 horas. 
# Como esse tipo de obra não é relevante para a analise, podemos remove-la.
# Para tentar filtrar filmes amadores ou experimentais, podemos remover aqueles que apresentam
# poucas ou nenhuma avaliação.

data_filtrada <- data |> 
  drop_na(averageRating, numVotes) |> 
  filter(numVotes > 100)


# Com os dados filtrados, o filme mais longo é o "The Cremaster Cycle", com quase 7 horas, seguido de
# "DAU. Degeneratsiya" (6 horas) e "Shadow of Chinatown" (4 horas e 41 minutos).

# Agora podemos visualizar os filmes de horror que foram mais bem avaliados.

data_filtrada |> 
  arrange(desc(averageRating)) |> 
  View()
  
# O filme com melhor avaliação é o filme indiano "Fantasy", com nota 9,7. Esse dado é bastante
# curioso, uma vez que, ao consultar sua pagina do imdb, não possui nenhuma review.
# Verificando os 10 filmes mais bem avaliados, isso parece ser uma constante, apenas 3 filmes
# possuem reviews e 2 deles são documentarios sobre direitos dos animais. Para analisar esse dado
# com mais profundidade, seria interessante possuir também os dados referentes a quantidade de
# reviews e projetar algum tipo de dado de enganjamento. 
# Podemos tentar mitigar esse problema aumentando o numero minimo de votos para 10000.


data_filtrada |> 
  filter(numVotes > 10000) |> 
  select(primaryTitle, releaseYear, averageRating, numVotes, ) |> 
  slice_max(order_by = averageRating, n = 10) |>
  View()

# Com os dados filtrados, temos que o top 5 é composto pelos filmes Manichithrathazhu (1993),
# Psycho (1960), Alien (1979), The Shining (1980) e The Thing (1982).
# Por curiosidade, podemos descobrir também quais são os fimes mais bem avaliados do século 21.

data_filtrada |> 
  filter(numVotes > 10000, releaseYear >= 2000) |> 
  select(primaryTitle, releaseYear, averageRating, numVotes, ) |> 
  slice_max(order_by = averageRating, n = 5) |>
  View()

# Agora que identificamos os filmes mais bem votados de todos os tempos, podemos tentar 
# visualizar se a nota média do filmes tende a variar com o passar do tempo.

data_filtrada |> 
  group_by(releaseYear) |> 
  filter(releaseYear <= 2021) |> 
  summarise(numMovies = n(),
            numVotes = sum(numVotes, na.rm = TRUE), 
            avgRating = mean(averageRating, na.rm = TRUE)) |> 
  ggplot() +
  aes(x = releaseYear, y = avgRating) +
  geom_point()


# Se olharmos apenas essa análise, podemos chegar a conclusão de que a 
# nota média dos filmes têm diminuido com o passar do tempo. Essa conclusão parece
# ser um pouco precipitada, uma vez que, com a facilitação do acesso a tecnologia
# uma qunatidade maior de filmes foram produzidas. Uma forma mais representativa
# de visualizar isso seria identificar a quantidade de filmes que obtiveram uma nota mínima
# com o passar do tempo.

data_filtrada |> 
  filter(averageRating >= 6 & releaseYear <=2021) |> 
  group_by(releaseYear) |> 
  summarise(numMovies = n(),
            numVotes = sum(numVotes, na.rm = TRUE), 
            avgRating = mean(averageRating, na.rm = TRUE)) |> 
  ggplot() +
  aes(x = releaseYear, y = numMovies) +
  geom_point() +
  geom_smooth()

# Considerando apenas os filmes com notas maiores que 6, notamos que o gráfico é bastante
# diferente do anterior. 


# Por fim, podemos verificar se há uma preferência por determinado subgênero com o 
# passar do tempo. 

data_decade <- data |> 
  # separate_rows(genres, sep = ",") |> 
  mutate(decade = floor(as.numeric(releaseYear)/10)*10) |> 
  select(tconst, decade, genres, numVotes, averageRating)


top_5_genres <- data |> 
  separate_rows(genres, sep = ",") |>
  filter(genres != "Horror") |> 
  group_by(genres) |> 
  summarise(numMovies = n()) |> 
  slice_max(order_by = numMovies, n = 5) |>
  pull(genres)



data_decade |> 
  separate_rows(genres, sep = ",") |> 
  filter(genres %in% top_5_genres) |> 
  group_by(decade, genres) |> 
  summarise(avgRating = mean(averageRating, na.rm = TRUE)) |> 
  drop_na() |> 
  ggplot(aes(x = decade, y = avgRating, color = genres)) +
  geom_point() +
  geom_line()


















# ---------- Avaliando gênero em relação a avaliação ------------



# Visualizando o numero de filmes em relação ao ano:

data |>
  group_by(releaseYear) |>
  drop_na() |> 
  filter(releaseYear <= 2021) |> 
  summarise(avgRating = mean(averageRating, na.rm = TRUE)) |>  
  ggplot(mapping = aes(x = releaseYear, y = avgRating)) + 
  geom_point(color = "darkgreen") + 
  geom_smooth(color = "darkred", method = "lm") +
  geom_hline(yintercept = 5.5, color = "black")

glimpse(data)

# Agrupando de acordo com a década:

data_decade |> 
  drop_na(numVotes) |> 
  group_by(decade) |> 
  ggplot(mapping = aes(x = decade)) + 
  geom_bar()


# Plotando boxplot

data_decade |>  
  ggplot(mapping = aes(factor(decade), averageRating)) +
  geom_boxplot() 

# Point:

data_decade |> 
  group_by(decade) |> 
  summarise(avgRating = mean(averageRating, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = factor(decade), y = avgRating)) +
  geom_point()

View(data_decade)



# -------- Verificando os gêneros -------------

data_genres <- data_decade |> 
  separate_rows(genres, sep = ",") 

View(data_genres)

# Verificando quais são os 5 gêneros que mais aparecem


# ------ SETUP: 
# Realidade criada para a histório
# ------ CONFLICT:
# Mudança da histório
# ------ RESOLUTION:
# Nova realidade que o conflito criou


# 1. Nunca apresentar um número único
# Comparar com a performance histórica
# 2. Somenta 1 ponto focal nos gráficos
# 3. Usar cores para identificar contrastes
# 4. Seja consistente com o estilo e design por todo o textxo
# 5. Criar gráficos de coparação
# 6. Usar tipos apropriados de gráficos
# 7. Use stickers 




#Diretor com mais filmes:

data |>
  group_by(directorName) |> 
  summarise(n = n(), media = mean(averageRating, na.rm = TRUE)) |> 
  filter(n >=3) |> 
  View()

write_rds(data, "dados/data_rating_director")












