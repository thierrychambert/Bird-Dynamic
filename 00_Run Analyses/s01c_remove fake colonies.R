### Unique Regroupe
u_regr <- effec01$regroupe[effec01$regroupe != "" & effec01$regroupe != "fake_colo"] %>% unique
u_regr
length(u_regr)

## Check : should only be -888
effec01[(effec01$regroupe != "" & effec01$regroupe != "fake_colo"), "EFF_Moy"] %>% unique %>% sort %>% head
# effec01[(effec01$regroupe != "" & effec01$regroupe != "fake_colo" & effec01$EFF_Moy != -888), ]

### Re-affect regrouped info included in fake colonies to real colonies
for(j in 1:length(u_regr)){
  ## Temp data
  tmp_colo <- effec01[ effec01$regroupe == u_regr[j], ]
  tmp_regr <- effec01[ effec01$regroupe == "fake_colo" & (effec01$Colonie == u_regr[j] | effec01$sous_unite == u_regr[j]), ] 
  
  # Apply the following only if it's fake colony 
  if(nrow(tmp_regr) > 0){
    for(k in 1:nrow(tmp_regr)){
      
      first_colo <- unique(tmp_colo$code_colonie[tmp_colo$espece == tmp_regr$espece[k] & tmp_colo$an == tmp_regr$an[k]])[1]
      
      ## Move the count data to the first real colony
      effec01[(effec01$code_colonie == first_colo & effec01$espece == tmp_regr$espece[k] & effec01$an == tmp_regr$an[k]), 
              "EFF_Moy"] <- tmp_regr$EFF_Moy[k]
    } # k
  } # end if
} # j

## Remove all fake colony's data
effec01 <- effec01[-which(effec01$regroupe == "fake_colo"),]
dim(effec01)


## Some checks
sum(effec01$regroupe != "")
sum(effec01$regroupe == "")
unique(effec01$regroupe)

## More checks
effec01[(effec01$regroupe != ""), "EFF_Moy"]
effec01[(effec01$regroupe != ""), "EFF_Moy"] %>% unique %>% sort %>% head
effec01[(effec01$regroupe == "" & effec01$EFF_Moy == -888), ]


## Créer une variable indicatrice pour les données regroupées (sera utile pour définir ppa)
effec01$rgp <- FALSE
effec01$rgp[(effec01$regroupe != "")] <- TRUE
unique(effec01$rgp)

## Check
lapply(effec01[, c("EFF_Moy", "EFF_Min", "EFF_Max")], unique) %>% unlist %>% unique %>% sort %>% head

## Remplacer les "-888" par 0
effec01$EFF_Moy[!is.na(effec01$EFF_Moy) & effec01$EFF_Moy == -888] <- 0
# car l'info de comptage est inclus dans une autre localité proche)
# Il faut utiliser "0" et pas "NA" pour ne pas artificiellement gonflé le comptage à l'échelle des clusters (via PI) 

## Check : no more -888
lapply(effec01[, c("EFF_Moy", "EFF_Min", "EFF_Max")], unique) %>% unlist %>% unique %>% sort %>% head
# effec01[effec01$rgp == TRUE, "EFF_Moy"]


## Clean workspace
rm(first_colo)
rm(u_regr)
rm(j)
rm(k)
rm(tmp_colo)
rm(tmp_regr)
