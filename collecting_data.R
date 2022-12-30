
# Coletando os dados necessários:

# Informações básicas sobre o filme:
# linK: https://datasets.imdbws.com/title.basics.tsv.gz

library(tidyverse)

data_basic_raw <- vroom::vroom("https://datasets.imdbws.com/title.basics.tsv.gz")

# Como queremos apenas filmes de terror, podemos filtra-la:

colnames(data_basic_raw)

data_basic <- data_basic_raw |> 
  filter(titleType == "movie" | titleType == "tvMovie") |> 
  filter(str_detect(genres, regex("horror", ignore_case = TRUE)))

write_rds(data_basic, "dados/data_basic_horror")

# Limpando cache para liberar memória: 

rm(data_basic_raw, data_basic)

# Informações referentes a produção do filme:
# link: https://datasets.imdbws.com/title.akas.tsv.gz

data_basic <- read_rds("dados/data_basic_horror")

ids_horror <- unique(data_basic$tconst)

data_akas_raw <- vroom::vroom("https://datasets.imdbws.com/title.akas.tsv.gz")

data_akas_horror <- filter(data_akas_raw, titleId %in% ids_horror)

write_rds(data_akas_horror, "dados/data_akas_horror")

rm(data_akas_horror, data_akas_raw)

# Informações referentes a nota dos filmes:
# link: https://datasets.imdbws.com/title.ratings.tsv.gz

data_rating <- vroom::vroom("https://datasets.imdbws.com/title.ratings.tsv.gz")

data_rating_horror <- data_rating |> 
  filter(tconst %in% ids_horror)

write_rds(data_rating_horror, "dados/data_rating_horror")
