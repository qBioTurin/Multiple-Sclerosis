
making_the_reference<- function(scenario)
{
  reference<-c()
  
  if(scenario=="healthy")
    {
    # percetnages of TEFF and Treg passed: 0.027	0.08 (healthy)
    reference[1] = 0.027
    reference[2] = 0.08
    # Out: number of INFg 42 IL17	8 IL10 13	(healthy)
    reference[3:5] = c(42,8,13) * c(5.7,0.07,)
    # In: number of INFg 0.59 IL17	0.76 IL10 1.03 (healthy) 
    reference[6:8] = c(.59,0.76,1.03)
    # Number of Teff and Treg out:
    reference[9:10] = c(50,13)
    write.table(reference,"./input/referenceHealthy.csv",row.names = F, col.names= F, sep =" ")
    
  } else if(scenario=="sick")
  {
    # percetnages of TEFF and Treg passed: 0.103	0.03 (sick) 
    reference[1] = 0.103
    reference[2] = 0.03
    # Out: number of INFg 117 IL17	25 IL10 3	(sick)
    reference[3:5] = c(117,25,3)
    # In: number of INFg 12.77 IL17	1.85 IL10 0.09 (sick) 
    reference[6:8] = c(12.77,1.85,.09)
    # Number of Teff and Treg out:
    reference[9:10] = c(142,3)
    write.table(reference,"./input/referenceMS.csv",row.names = F, col.names= F, sep =" ")
  }
  
}
