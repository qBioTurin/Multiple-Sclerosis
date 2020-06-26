Names <- read_table2("Net/MS_Model.PlaceTransition")

transStart<- which(Names[,1] == "#TRANSITION")

NAMES = unlist(Names[1:(transStart-1),1])
NAMES = unname(NAMES)

saveRDS(NAMES,file="./input/NAMES.RDS")
