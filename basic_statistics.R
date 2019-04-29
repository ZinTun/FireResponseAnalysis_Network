library(dplyr)
library(ggplot2)
library(igraph)
library(lubridate)

# Load data
data_raw <- read.csv("cleaned_data.csv") %>% distinct(MASTERINCIDENTNUMBER, .keep_all = TRUE)

data <- data_raw %>%
  mutate(
    date = date %>% 
      as.POSIXlt(format = "%m/%d/%Y") %>%
      format("%d-%m-%Y"),
    time = time %>% 
      as.POSIXlt(format = "%H:%M:%S") %>%
      format("%H:%M"))

# Plot overall statistics
density_station <- ggplot(data, aes(x = ResponseTime_Sec, color = LOCATIONATASSIGNTIME)) + geom_density()
density_station  

boxplot_station <- ggplot(data, aes(x = LOCATIONATASSIGNTIME, y = ResponseTime_Sec, color = LOCATIONATASSIGNTIME)) + geom_boxplot()
boxplot_station

# Seasonal and peak hour analysis
data_seasonal <- data %>% 
  mutate(season = ifelse(month(date) == 12 | month(date) == 1 | month(date) == 2, 
         "winter", "other")) %>%
  group_by(season, LOCATIONATASSIGNTIME)

boxplot_season <- ggplot(data_seasonal, aes(x = LOCATIONATASSIGNTIME, y = ResponseTime_Sec, color = season)) + geom_boxplot()
boxplot_season

# w <- data_seasonal %>% filter(season == "winter")
# nw <- data_seasonal %>% filter(season == "other")
# t.test(w$ResponseTime_Sec, nw$ResponseTime_Sec)

data_peak <- data %>% 
  mutate(weekday = ifelse(!(weekdays(as.Date(date)) == "Sunday" | weekdays(as.Date(date)) == "Saturday"), 1, 0),
         peak = ifelse(weekday == 1 & ((time >"06:00" & time <"09:30") | (time >"18:00" & time <"21:30")), 1, 0) %>%
           as.factor()
         )

boxplot_peak <- ggplot(data_peak, aes(x = LOCATIONATASSIGNTIME, y = ResponseTime_Sec, color = peak)) + geom_boxplot()
boxplot_peak

p <- data_peak %>% filter(peak == 1)
np <- data_peak %>% filter(peak == 0)
t.test(p$ResponseTime_Sec, np$ResponseTime_Sec)  

# Network graph analysis
edge_list <- data.frame(from = data$LOCATIONATASSIGNTIME, to = data$location_cat)
G <- graph_from_data_frame(edge_list,directed = T)
Vx <- V(G)
print(Vx %>% length())
Ex <- E(G)
print(Ex %>% length())

# Graph level stats
density <- edge_density(G, loops = FALSE)
diameter <- diameter(G, directed = FALSE)
component <- components(G)
clustering_coefficient <- transitivity(G)

# node level stats
degree <- degree(G)
closeness <- closeness(G)
clus_coef <- transitivity(G, type = "local")
pagerank <- page_rank(G)$vector
eccentricity <- eccentricity(G)
metrics <- data.frame(degree, closeness, clus_coef, 
                      pagerank, eccentricity) 

# metrics_station <- metrics %>%
#   group_by(name) %>%
#   summarise(degree_mean = mean(degree),
#             closeness_mean = mean(closeness),
#             pagerank_mean = mean(pagerank),
#             eccentricity = mean(eccentricity))

metrics_mean <- apply(metrics, 2, mean)
metrics_mean
metrics_sd <- apply(metrics, 2, sd)
metrics_sd
