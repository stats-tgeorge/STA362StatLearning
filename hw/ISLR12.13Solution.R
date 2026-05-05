library(readr)
library(tidymodels)
library(tidyclust)
gene_df <- read_csv("hw/data/Ch12Ex13.csv", 
                     col_names = FALSE)
#View(Ch12Ex13)


correlation_dissimilarity <- as.matrix(as.dist(1 - cor(gene_df)))

hc_spec <- hier_clust(
  num_clusters = 2,
  linkage_method = "average"
)


hc_fit <- hc_spec |>
  fit(~ .,
      data = as.data.frame(t(correlation_dissimilarity))
)

library(factoextra)

hc_fit |> extract_fit_engine() |>
  fviz_dend(main = "complete", k = 2)
  
hc_fit |> extract_fit_engine() |>plot()
