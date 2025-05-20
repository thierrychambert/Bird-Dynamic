## LOAD DATA ######
## Load species list OF Bird DYNAMIC
BD_list <- read.csv2("../000_Inputs/liste_especes_BD.csv", na.strings = c("NA"))

## Load foraging range data
foraging_range_tbl <- read.csv2("../000_Inputs/foraging_range_species.csv", dec = ".")

## Load vital rates data
vital_rates <- read.csv2("../000_Inputs/Vital_rates.csv", dec = ".")

## Load proportion of breeders per age class
prop_repro <- read.csv2("../000_Inputs/Table_age_prop_repro.csv", dec = ".")

## Load Count data 
effec00 <- read.csv2("../000_Inputs/BD_Effectifs_1997-2021.csv", 
                     na.strings = c("-1", "-999", "---", "----")
)
## Codes utilisés dans les données (champ "Eff)
# -1 : pas de recensement effectués
# -999 : donnée existe peut-être qqpart, mais non dispo ici
# -888 donnée slt dispo à une échelle de regroupement plus large


## CORRECTIONS ######
sel <- which(effec00$Espece == "Goéland leucophée" & effec00$localite_colonie == "BORDS DE LA GIRONDE (33)")
effec00[sel, "Colonie_latitude"] <- 45.20452194
effec00[sel, "Colonie_longitude"] <- -0.649251938

## Suppression de qqs -888 (car pas d'info regroupés pour ces données)
effec00 <- effec00[-which(effec00$sous_unite == "YEU_TOTAL_SANS_DETAILS" & effec00$Espece == "Goéland leucophée" & effec00$EFF_Moy == -888), ]
which(effec00$sous_unite == "YEU_TOTAL_SANS_DETAILS" & effec00$Espece == "Goéland leucophée" & effec00$EFF_Moy == -888)

# Suppression de « localite_colonie == REGION DES ABERS » // info non fiable
effec00 <- effec00[-which(effec00$localite_colonie == "REGION DES ABERS"),]

# Suppression donnée non fiable : Goéland marin, 2002, YEU_TOTAL_SANS_DETAILS / aucun lien avec autres colonies
effec00 <- effec00[-which(effec00$sous_unite == "YEU_TOTAL_SANS_DETAILS" & effec00$Espece == "Goéland marin" & effec00$an == 2002),]


## Remove erroneous spaces
effec00$sous_unite[effec00$sous_unite == " "] <- "" 


## Retirer les colonies urbaines
sites_urbains <- read.csv2("../000_Inputs/Sites urbains Goélands.csv", na.strings = c("NA"))
goel_esp <- c("Goéland argenté", "Goéland brun", "Goéland marin", "Goéland leucophée")
rem <- c()
for(i in 1:nrow(sites_urbains)){
  rem <- c(rem,
           which(
             effec00$secteur == sites_urbains$secteur[i] &
               effec00$localite_colonie == sites_urbains$localite_colonie[i] &
               effec00$sous_unite == sites_urbains$sous_unite[i] &
               effec00$Espece %in% goel_esp
           )
  )
}
effec00 <- effec00[-rem, ]
rm(rem) ; rm(i)

## DATA CLEANING ######
## Check for hidden NA's
lapply(effec00[, c("EFF_Moy", "EFF_Min", "EFF_Max")], unique) %>% unlist %>% unique %>% sort %>% head

## Some cleaning
effec00$Colonie_latitude <- as.numeric(effec00$Colonie_latitude)
effec00$Colonie_longitude <- as.numeric(effec00$Colonie_longitude)

## Rename some columns
effec00 <- dplyr::rename(effec00, 
                         Colonie = localite_colonie, 
                         Lat = Colonie_latitude, 
                         Lon = Colonie_longitude,
                         espece = Espece)


## Treat sous_unite NA's
effec00$sous_unite[is.na(effec00$sous_unite)] <- "--"
dim(effec00)
rm(sel)

## Create code_colonie
source("s01b_Create_Code_Colonie.R")

## FUNCTIONS ######
## Define function clean_names
clean_names <- function(x) enc2utf8(x) %>% iconv(., from = "UTF-8", to="ASCII//TRANSLIT") %>% tolower

## Define function to check if the ordering colonies is the same
check_order <- function(A, B){
  cbind(A, B) %>%
    apply(., 1, FUN = function(x) x[1] != x[2]) %>% 
    which()
}

## Retirer les données NA de effec01 (mais on conserve les -888)
effec01 <- effec00[!is.na(effec00$EFF_Moy), ]
#effec01 <- effec00[!is.na(effec00$EFF_Moy) & effec00$EFF_Moy >= 0, ]
dim(effec01)

which(effec01$an >= 2009) %>% length


##### Remove fake colonies
source("s01c_remove fake colonies.R")

