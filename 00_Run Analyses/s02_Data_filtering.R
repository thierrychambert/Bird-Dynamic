
## Data filtering
min_year = 2009      # Remove years before... 
last_year = 2021     # Remove years after... 
min_Lat = 0          # Minimum Latitude (remove colonies south of XX°)
h_km = 5           # Define the bandwidth (in km) used by the Clustering Algorithm
dist_regr_isol <- 20 # h_km*4    # Define the max distance to regroup an isolated colony to closest cluster

## Stat / infos
#effec01$EFF_Moy %>% unique %>% sort %>% head
#effec01$espece %>% unique %>% sort

## Graphes
#plot(table(effec01$an), lwd = 3)
#points(as.table(table(effec01$an)[paste(2009)]), col = 2, lwd = 5)
