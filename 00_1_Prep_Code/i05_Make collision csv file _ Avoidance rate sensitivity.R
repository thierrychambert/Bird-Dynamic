rm(list = ls(all.names = TRUE))
source("s00_Load_packages.R")
#DUN, DLT, FEC, COU, SBR, SNA, YEU

## Build Parc dataset ##### 
source("s06_Buid_Parc_Dataset.R")

## Load the full "avoidance rate sensitivity" collision00 dataset (includes all species and all parcs)
# collision00 <- read.csv(paste0("../000_Inputs/data mortality/Avoidance-rate-sensitivity-testing_collision-distributions_MA2.csv")) 
dim(collision00)
unique(collision00$Site)


## Add full nam eof wind farms
collision00$Parc <- NA
collision00[collision00$Site == "DUN", "Parc"] <- "DUNKERQUE"
collision00[collision00$Site == "DLT", "Parc"] <- "DIEPPE - LE TREPORT"
collision00[collision00$Site == "FEC", "Parc"] <- "FECAMP"
collision00[collision00$Site == "COU", "Parc"] <- "COURSEULLES"
collision00[collision00$Site == "SBR", "Parc"] <- "SAINT BRIEUC"
collision00[collision00$Site == "SNA", "Parc"] <- "SAINT NAZAIRE"
collision00[collision00$Site == "YEU", "Parc"] <- "YEU - NOIRMOUTIER"

unique(collision00$Parc)


## Load full species list
BD_list <- read.csv2("../000_Inputs/liste_especes_BD.csv", na.strings = c("NA"))

## Unique species
u_sp <- unique(collision00$Species) ; u_sp
sel00 <- which(BD_list$Nom_english %in% u_sp) ; sel00

## Select species (1,5,6)
sel_esp = 6
esp <- BD_list$espece_BD[sel_esp] ; esp

## Start DF for the chosen species
collision_esp <- collision00[collision00$Species == BD_list$Nom_english[sel_esp], ]
collision_esp$Espece <- esp
head(collision_esp)
dim(collision_esp)


## Save as csv
# collision <- collision_esp
write.csv(collision_esp, 
          file = paste0("../000_Inputs/data mortality/AR95_", sprintf("%02d", sel_esp), "_",  "CollisionRisk_", esp,".csv")
         , row.names = F, quote = F)

