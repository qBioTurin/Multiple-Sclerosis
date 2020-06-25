
InjectionSetting.generation<-function(InjATime, InjDACTime=NULL, numberA, numberDAC=NULL,nfile="./input/NAMES.RDS" )
{
  y.names <- readRDS(nfile)

  events<-list()
  
  times <- sort(unique(c(InjATime,InjDACTime)))

  for(i in 1:length(times) )
  {
    y <- rep(0,length(y.names))
    names(y)<-y.names
    
    if(times[i] %in% InjATime )  {
      if( length(numberA)==1 )
        {
          y["Antigen"] = numberA
      }else{
        y["Antigen"] = numberA[i]
      }
    }
      
    if(times[i] %in% InjDACTime )  {
      if( length(numberDAC)==1 )
      {
        y["DAC"] = numberDAC
      }else{
        y["DAC"] = numberDAC[i]
      }
    }
    
    events[[i]]<-list( times[i], y )
    
  }
  
  return(events)
}
