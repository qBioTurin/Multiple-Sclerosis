Names <- read_table2("Net/Rete_SM_newAM_SR_Laura.PlaceTransition")

transStart<- which(Names[,1] == "#TRANSITION")

NAMES = unlist(Names[1:(transStart-1),1])
NAMES = unname(NAMES)

saveRDS(NAMES,file="./input/NAMES.RDS")
