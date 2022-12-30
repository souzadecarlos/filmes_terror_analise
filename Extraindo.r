# Verificando o dataset com o Vroom
# Dataset baixado no dia 19/09

library(tidyverse)

# Selecionando apenas as obras que são filme e do gênero "Horror"

data <- vroom::vroom("dados/data.tsv")

glimpse(data)

unique(data$titleType)

data_movies <- data |> 
  filter(titleType == "movie" | titleType == "tvMovie")

head(data_movies)

data_horror <- data_movies |> 
  filter(str_detect(genres, regex("horror", ignore_case = TRUE)))

# Exportando o dataset com fimes de terror:

write_rds(data_horror, "dados/data_horror.rds")
