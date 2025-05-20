## Données de comptages agrégés : colonie x an
counts00 <- aggregate(EFF_Moy ~ an + code_colonie + Colonie + sous_unite + rgp, 
                      data = spp, FUN = mean, na.action = NULL) %>%
  arrange(., an, code_colonie)

# Nombre de colonies (avec all NA's (diff) et le reste)
colonies_L93$code_colonie %>% unique %>% length
spp$code_colonie %>% unique %>% length
counts00$code_colonie %>% unique %>% length

## Reshape : colonie x année
molten.data <- reshape2::melt(counts00, id = c("an","code_colonie", "Colonie", "sous_unite"))

#counts01 <- reshape::cast(molten.data, code_colonie + Colonie + sous_unite ~ an, mean)
counts01 <- reshape::cast(molten.data, code_colonie ~ an, mean, subset = variable=="EFF_Moy")
rgp01 <- reshape::cast(molten.data, code_colonie ~ an, subset = variable=="rgp")

## Define missing year
u_yr <- (spp$an %>% unique %>% sort)
all_yr <- (min_year:last_year)
miss_yr <- all_yr[!(all_yr %in% u_yr)]

## Replace NaN by NA
counts01[,paste(u_yr)] <- sapply(counts01[,paste(u_yr)], FUN = function(X) ifelse(is.nan(X), NA, X))

## Add missing years (all NA's)
if(length(miss_yr) > 0){
  for(k in 1:length(miss_yr)){
    counts01 <- counts01 %>% add_column(new = NA, .after = paste(miss_yr[k]-1))
    colnames(counts01)[colnames(counts01) == "new"] <- paste(miss_yr[k])
    
    rgp01 <- rgp01 %>% add_column(new = NA, .after = paste(miss_yr[k]-1))
    colnames(rgp01)[colnames(rgp01) == "new"] <- paste(miss_yr[k])
  } #k
  rm(k)
} # end if
head(counts01)
head(rgp01)


## Total count et Moyenne (sur les années) par colonie
counts02 <- as.matrix(counts01[,-1])
rgp02 <- as.matrix(rgp01[,-1])

## Function "get_last survey"
get_last <- function(x) x[max(which(!is.na(x)))]


for(j in 1:nrow(counts01)){
  sel <- which(rgp02[j,] == 0) # use only data that are not part of a "regroup" thing
  if(length(sel) > 0){
    counts01$tot[j] <- sum(counts02[j, sel])
    counts01$avg[j] <- mean(counts02[j, sel])
    counts01$last[j] <- get_last(counts02[j, sel])
  }else{
    counts01$tot[j] <- sum(counts02[j,], na.rm = TRUE)
    counts01$avg[j] <- mean(counts02[j,], na.rm = TRUE)
    counts01$last[j] <- get_last(counts02[j,])
  } # if
} # j
rm(sel)

head(counts01)

## Rename years
names(counts01)[names(counts01) %in% min_year:last_year] <- paste0("X",min_year:last_year)

## Merge #####
# Counts and colonies (group)
#colonie_counts <- merge(colonies, counts01, by = c("code_colonie", "Colonie", "sous_unite"), all.y = FALSE)    
colonie_counts <- merge(colonies, counts01, by = c("code_colonie"), all.y = FALSE)    
colonie_counts <- colonie_counts %>% arrange(., group, desc(Lat), Lon, code_colonie)
rownames(colonie_counts) <- colonie_counts$code_colonie

## Aggregate colonie_counts by groups
group_counts <- data.frame(group = as.numeric(levels(colonie_counts$group))) # as.numeric needed for proper ordering in next step (loops)

## Loop over years
for(yr in min_year:last_year){
  if(yr %in% u_yr){
    group_counts <- group_counts %>% 
      merge(., 
            aggregate(as.formula(paste0("X", yr, "~ group")), 
                      data = colonie_counts, FUN = sum, na.action = na.omit), 
            by = "group", all.x = TRUE)
  } # if 
}  # yr
rm(yr)


## Add missing years (all NA's)
if(length(miss_yr) > 0){
  for(k in 1:length(miss_yr)){
    group_counts <- group_counts %>% add_column(new = NA, .after = paste0("X",miss_yr[k]-1))
    colnames(group_counts)[colnames(group_counts) == "new"] <- paste0("X",miss_yr[k])
  } #k
  rm(k)
} # end if

## Make "group" as.factor
group_counts$group <- as.factor(group_counts$group)

## Ensure proper ordering, give rownames and remove column "group"
group_counts <- arrange(group_counts, group)
rownames(group_counts) <- group_counts$group # paste("group", group_counts$group)
group_counts <- dplyr::select(group_counts, -group)

head(group_counts)

## Proportion of colonies (from a group) surveyed each year
# as a measure of proportional "count effort"
ncs_yr <- ppc_yr <- ppa_yr <- group_counts
ncs_yr[] <- ppc_yr[] <- ppa_yr[] <- 0
ppa_yr

## Loop over each group
for(i in 1:n_group){
  gp_i <- levels(colonies$group)[i]
  
  tbl00 <- as.data.frame(rbind(
    xtabs(as.formula(paste0("X", min_year, "~ code_colonie")), data = colonie_counts, addNA = TRUE, subset = group == gp_i)
  ))
  
  ## Loop over years
  for(yr in (min_year+1):last_year){
    tbl00 <- rbind(tbl00,
                   xtabs(as.formula(paste0("X", yr, "~ code_colonie")), data = colonie_counts, addNA = TRUE, subset = group == gp_i)
    )
  } # yr
  rm(yr)
  
  ## Proportion of abundance of each colony
  tot_avg <- xtabs(avg ~ code_colonie, data = colonie_counts, addNA = TRUE, subset = group == gp_i)
  ppa_col <- tot_avg/sum(tot_avg, na.rm = TRUE)
  
  ## Year x Colony : surveyed or not ?
  ppa_yr[i,] <- (ifelse(is.na(tbl00), 0, 1) %*% t(t(ppa_col)))
  
  ## Number and proportion of colonies surveyed each year
  ncs_yr[i,] <- (ifelse(is.na(tbl00), 0, 1) %>% rowSums)
  ppc_yr[i,] <- (ncs_yr[i,]/ncol(tbl00))
  
  rm(ppa_col) ; rm(tbl00) ; rm(tot_avg)
} # i

rownames(ncs_yr) <- rownames(ppc_yr) <- rownames(ppa_yr) <- paste("group", levels(colonies$group))
head(ppa_yr)
#ncs_yr ; ppc_yr

## Corrected colonie_counts
corrected_counts <- round(group_counts / ppa_yr)

## Compare
apply(corrected_counts, 2, sum, na.rm = TRUE)
apply(group_counts, 2, sum, na.rm = TRUE)

## Clean workspace
rm(i)
rm(j)
rm(gp_i)


