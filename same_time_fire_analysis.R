library(dplyr)
library(ggplot2)
library(igraph)
library(lubridate)

# Load data
data_raw <- read.csv("cleaned_data.csv") %>% distinct(MASTERINCIDENTNUMBER, .keep_all = TRUE)

data <- data_raw %>%
  mutate(
    date = date %>% 
      as.POSIXct(format = "%m/%d/%Y") %>%
      format("%m-%d-%Y"),
    time = time %>% 
      as.POSIXct(format = "%H:%M:%S") %>%
      format("%H:%M"),
    index = as.numeric(rownames(data_raw)),
    ADDRESS = location_cat)

timerange <- 60*30 # 30 min
datesrange <- data$date %>% unique()
edge_list <- data.frame()
for (each_day_index in 1:length(datesrange)){
  each_day <- datesrange[each_day_index]
  day_data <- data %>%
    filter(date == each_day)
  unique_nodes <- day_data$index 
  for (each_node in min(unique_nodes) : max(unique_nodes)){
    node_time_df <- day_data %>% 
      filter(index == each_node) %>%
      select(time) 
    node_time <- node_time_df[1,1]
    edges <- day_data %>%
      filter(index > each_node) %>%
      mutate(time_diff = abs(as.numeric(hm(time) - hm(node_time))),
             within_time = time_diff <= timerange,
             to = data[each_node, "ADDRESS"],
             from = data[index, "ADDRESS"]) %>%
      select(-time_diff) %>%
      filter(within_time == T) %>%
      select(to, from)
    edge_list <- bind_rows(edge_list, edges)
  }
}
write.csv(edge_list, "edge_list_new.csv")

# Graphical Analysis
G <- graph_from_data_frame(edge_list,directed = F)
Vx <- V(G)
print(Vx %>% length())
Ex <- E(G)
print(Ex %>% length())

# Graph level stats
density <- edge_density(G, loops = FALSE)
diameter <- diameter(G, directed = FALSE)
component <- components(G)

# node level stats
degree <- degree(G)
closeness <- closeness(G)
clus_coef <- transitivity(G, type = "local")
pagerank <- page_rank(G)$vector
eccentricity <- eccentricity(G)
metrics <- data.frame(degree, closeness, clus_coef, 
                           pagerank, eccentricity)
cor(metrics, method = "pearson", use = "pairwise.complete.obs") %>% round(3)
metrics_mean <- apply(metrics, 2, mean)
metrics_mean
metrics_sd <- apply(metrics, 2, sd)
metrics_sd
