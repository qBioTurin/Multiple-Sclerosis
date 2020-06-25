msqd<-function(reference, output)
{
	Error=0

	reference<-unlist(reference)
	Teff_out<-output[,"Teff_out"]
	Teff_in<-output[,"Teff_in"]
	Treg_out<-output[,"Treg_out"]
	Treg_in<-output[,"Treg_in"]
	A<-output[,"Antigen"]

	ntime<-length(Teff_out)
	
  # day of infection:
    day.inf<-which.max(output[,"Antigen"])
    day.inf1<-day.inf+18/24

 # percetnages of TEFF and Treg passed:   0.103	0.03 (sick) 0.027	0.08 (healthy)
	perc_Teff <- (Teff_in[day.inf1+2*24] - 1 )^2
	perc_Treg <- (Treg_in[day.inf1+2*24] - 1 )^2

  #Interl_out <- apply(output[,c("INFg_out","IL17_out","IL10_out")],2,"mean")
	Interl_out <- output[day.inf1,c("INFg_out","IL17_out","IL10_out")]
  Interl_in <- output[day.inf1+2*24,c("INFg_in","IL17_in","IL10_in")]

 # Out: number of INFg 42 IL17	8 IL10 13	0.59	0.76	1.03 (healthy)
   Error_Interl_out <- sum((Interl_out - reference[3:5] )^2 )
 # In: number of INFg 0.59 IL17	076 IL10 1.03 (healthy)
  Error_Interl_in <- sum((Interl_in - reference[6:8] )^2)

  # Number of Teff and Treg out: (I put the duoble weight!!)
	ErrorTcell <- (c(Teff_out[day.inf1],Treg_out[day.inf1] ) - reference[9:10])^2
	if(Teff_out[ntime]== 0) ErrorTcell = 10^6
	if(Treg_out[ntime]== 0) ErrorTcell = 10^6
	
	# and Teff should be twice the treg
	ErrorTcell2 = mean( (0.5* Teff_out[(day.inf + (0:5)*24)  ] - Treg_out[(day.inf + (0:5)*24)  ] )^2)
	
 # max value of Teff_out is at the 5th day
	if(!is.na(Teff_out[day.inf+5*24]))
	{
		Error = Error+ 2*(max(Teff_out) - Teff_out[day.inf+5*24])^2
	}
	if(!is.na(A[day.inf+7*24]))
	{
	  Antigene_left_middle<-A[day.inf+(3)*24]
	  Antigene_left<-mean(A[day.inf+(7:14)*24])
		Error = Error+ (max(A)*.95 - Antigene_left )^2 + (max(A)*.5 -Antigene_left_middle)^2
		# After 7 days I want that the antigen is 90% reduced
		
	}
  # I want to minimize the ODC damage
  # ODC_error= mean(output[,"ODC_le1"])^2 

  # Error = Error +  perc_Teff + perc_Treg +  sum(Error_Interl_out) + sum(Error_Interl_in)
	#Error = Error +  perc_Teff + perc_Treg + sum(ErrorTcell) + ODC_error + ErrorTcell2 + Error_Interl_out
	Error = Error +  perc_Teff + perc_Treg + sum(ErrorTcell) + ErrorTcell2 + Error_Interl_out + Error_Interl_in
	
	return(Error)
}
#
# output<-read.table("results_sensitivity_analysis/Rete_SM_newAM_SR_Laura-sensitivity-1793.trace",header = TRUE)
# reference<-read.table("input/reference.csv")
