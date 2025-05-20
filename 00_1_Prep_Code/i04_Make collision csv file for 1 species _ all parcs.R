rm(list = ls(all.names = TRUE))
source("s00_Load_packages.R")
#DUN, DLT, FEC, COU, SBR, SNA, YEU

## Build Parc dataset ##### 
source("s06_Buid_Parc_Dataset.R")

jj=11
# for(jj in (5:17)[-7]){

  ## Choose species # espèces inclus dans Bird Risk : 1 à 17
  BD_list <- read.csv2("../000_Inputs/liste_especes_BD.csv", na.strings = c("NA"))
  sel_esp = jj
  esp <- BD_list$espece_BD[sel_esp]
  
  print(sprintf("%02d", sel_esp))
  print(esp)
  print("###############")
  
  ## Start DF for the chosen species (use SNA because it has all species)
  parc <- parcs_L93$NAME[6]
  collision <- read.csv(paste0("../000_Inputs/data mortality/CollisionRisk_", parc,".csv")) 
  collision_esp <- collision[collision$Species == BD_list$Nom_english[sel_esp], ]
  collision_esp$Parc <- parc
  collision_esp$Espece <- esp
  dim(collision_esp)

  ## Loop over other wind farms
  for(kk in (1:7)[-6]){
    parc <- parcs_L93$NAME[kk]
    print(parc)
    collision <- read.csv(paste0("../000_Inputs/data mortality/CollisionRisk_", parc,".csv")) 
    
    if(BD_list$Nom_english[sel_esp] %in% unique(collision$Species)){
      keep <- collision[collision$Species == BD_list$Nom_english[sel_esp], ]
      keep$Parc <- parc
      keep$Espece <- esp
      
      # merge 
      collision_esp <- add_row(.data = collision_esp, keep)
      rm(keep)
    } # close if
    
    
  } # kk
  
  head(collision_esp)
  dim(collision_esp)
  
  ## Save as csv
  write.csv(collision_esp, 
            file = paste0("../000_Inputs/data mortality/", sprintf("%02d", sel_esp), "_",  "CollisionRisk_", esp,".csv")
            , row.names = F, quote = F)
  
  print("###############")
  print("###############")
  
  #} # jj
  
  
  
  