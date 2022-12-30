# Testando Vroom compressed

library(tidyverse)

data_horror <- read_rds("dados/data_horror.rds")

id_horror <- unique(data_horror$tconst)

data <- vroom::vroom("https://datasets.imdbws.com/title.ratings.tsv.gz")


data_id <- data |> 
  filter(tconst %in% id_horror)


data_merged <- left_join(data_horror, data_id, by = "tconst", all = TRUE)

write_rds(data_merged, "dados/data_horror_rating")


# data <- vroom::vroom("https://datasets.imdbws.com/title.akas.tsv.gz")
# 
# data_title <- data |> 
#   filter(titleId %in% id_horror)
# 
# data_title <- data |> 
#   filter()
# 
# for (link in links) {
# 
#   data <- vroom::vroom(link)
# 
#   data_id <- data |>
#     filter(tconst %in% id_horror)
# 
#   data_horror <- merge(data_horror, data_id, by = "tconst", all = TRUE)
# 
# }
