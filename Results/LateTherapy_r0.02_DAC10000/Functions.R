init_m <- function(n_file, x=NULL, patient= "HEALTHY")
{
  yini.names <- readRDS(n_file)

  yini <- rep(0,length(yini.names))
  dim(yini)<- c(1,length(yini.names))
  yini <- as.data.frame(yini)
  names(yini)<-yini.names

    yini["ODC_le5"] = 500

    # From literature:
    yini["NK_out"] = 30
    yini["Resting_Teff_out"] = 1687
    yini["Resting_Treg_out"] =  63

    # From Simona/Alessandro 's file:
    # Healthy patient:
      yini["IL10_in"] = 1
      yini["IL10_out"] = 13
      yini["IL17_in"] = 1
      yini["IL17_out"] = 8
      yini["IFNg_in"] = 1
      yini["IFNg_out"] = 42
      yini["Teff_out"] = 0 #50
      yini["Treg_out"] = 0 #13

  return(matrix(yini, ncol = 1))
}

TeE<- function( patient , x=NULL)
{
  
  if(patient == "HEALTHY"){
    if(!is.null(x)) p=x[1]
   
  }else{
    if(!is.null(x)) x[1]
  }

  return(matrix(p, ncol = 1))
}

TrE<- function( patient , x=NULL)
{
  if(patient == "HEALTHY"){
    if(!is.null(x)) p=x[2]
  }else{
    p<-.2
  }

  return(matrix(p, ncol = 1))
}

Tr2<- function( patient , x=NULL)
{
  if(patient == "HEALTHY"){
   # p<-0.09
    if(!is.null(x)) p=x[3]
  }else{
    p<-0.09
  }

  return(matrix(p, ncol = 1))
}

Te2<- function( patient , x=NULL)
{
  if(patient == "HEALTHY"){
    #p<-0.5
    if(!is.null(x)) p=x[4]
  }else{
    p<-0.5
  }

  return(matrix(p, ncol = 1))
}

TekODC <- function( patient , x=NULL)
{
  if(patient == "HEALTHY"){
    #p<-0.15
    if(!is.null(x)) p=x[5]
    
  }else{
    p<-0.1
  }

  return(matrix(p, ncol = 1))
}

TrkTe <- function( patient , x=NULL)
{
  if(patient == "HEALTHY"){
    if(!is.null(x)) p=x[6]
  }else{
    p<- 3
  }

  return(matrix(p, ncol = 1))
}

TekA <- function( patient , x=NULL)
{
  if(patient == "HEALTHY"){
    if(!is.null(x)) p=x[7]
    
  }else{
    p<-0.15
  }

  return(matrix(p, ncol = 1))
}

NKkT<- function( patient , x=NULL)
{
  if(patient == "HEALTHY"){
    if(!is.null(x)) p=x[13]
  }else{
    p<-0.1
  }

  return(matrix(p, ncol = 1))
}

Pass_BBB<- function( x=NULL, type)
{
p=.1
  if(type=="treg")
  {
    if(!is.null(x)) p=x[8]
    
  }else{
    if(!is.null(x)) p=x[9]
  }
  return(matrix(p, ncol = 1))
}

IL10_production<-function(x=NULL, patient)
{
	p = 0.055555555555556
	
  return(matrix(p, ncol = 1))
}

IFNg_production_NK<-function(x=NULL, analysis )
{
  if(is.null(x))
  {
    if(analysis == "sensitivity"){
      center<-0.046604938271605
      p<-runif(1, center*.5,center*1.1)
    }
  }else{
    p=x[14]
  }
  
  return(matrix(p, ncol = 1))
}
IL10_production_NK<-function(x=NULL, analysis )
{
  if(is.null(x))
  {
    if(analysis == "sensitivity"){
    	center<-0.055555555555556
      p<-runif(1, center*.5,center*1.1)
    }
  }else{
    p=x[15]
  }

  return(matrix(p, ncol = 1))
}

IL17_production<-function(x=NULL, patient)
{
	p = 0.008950617283951

  return(matrix(p, ncol = 1))
}

IFNg_production<-function(x=NULL, patient )
{
  p = 0.046604938271605

  return(matrix(p, ncol = 1))
}

IL10Consuption <- function(analysis,x=NULL)
{
	if(is.null(x))
	{
		if(analysis == "sensitivity"){
			p<-runif(1,0,1)
		}
	}else{
	  p=x[19]
	}

	return(matrix(p, ncol = 1))
}

IL17Consuption <- function(analysis,x=NULL)
{
	if(is.null(x))
	{
		if(analysis == "sensitivity"){
			p<-runif(1,0,1)
		}
	}else{
	  p=x[20]
	}

	return(matrix(p, ncol = 1))
}

IFNgConsuption <- function(analysis,x=NULL)
{
	if(is.null(x))
	{
		if(analysis == "sensitivity"){
			p<-runif(1,0.1,1)
		}
	}else{
	  p=x[21]
	}

	return(matrix(p, ncol = 1))
}

Remyelinization<-function(x=NULL, analysis )
{
  if(is.null(x))
  {
    if(analysis == "sensitivity"){
      p<-runif(1,0,1)
    }
  }else{
    p=x[18]
  }

  return(matrix(p, ncol = 1))
}

Cifn<- function(x,patient = "HEALTHY")
{
  p=x[12]
  return(matrix(p, ncol = 1))
}

CIL10<- function(x,patient = "HEALTHY")
{
  p=x[11]
  return(matrix(p, ncol = 1))
}

IL17_BBB<- function(x, patient = "HEALTHY")
{
  p=x[16]
  return(matrix(p, ncol = 1))
}

IL10_BBB<- function(x, patient = "HEALTHY")
{
  p=x[17]
  return(matrix(p, ncol = 1))
}

DACkill<- function(x, therapy, r = NULL)
{
  if(therapy){
    if( is.null(r)) p = x[10]
    else p = r
  }  
  else p = 1e-7
  
  return(matrix(p, ncol = 1))
}
