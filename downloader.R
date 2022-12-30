library(tidyverse)

data_crew <- vroom::vroom("dados/title.crew.tsv.gz")

data_movie <- read_rds("dados/data_basic_horror")

vetor_movies <- data_movie$tconst

data_movie_principal <- data_crew |> 
  filter(tconst %in% vetor_movies)

data_name_basic <- vroom::vroom("dados/name.basics.tsv.gz")

data_movie_principal <- data_movie_principal |> 
  select(tconst, directors) |> 
  separate_rows(directors, sep = ",") 

glimpse(data_name_basic)

data_name_basic <- data_name_basic |> 
  select(nconst, primaryName, birthYear, deathYear)

data_movie_principal <- data_movie_principal |> 
  rename(nconst = directors)

head(data_movie_principal)


movie_director <- left_join(x= data_movie_principal, y = data_name_basic) 

write_rds(movie_director, "dados/data_director_horror")
