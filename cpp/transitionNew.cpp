#include <cstdio>
#include <cstring>

// Rate
static double Tr2;
static double TeE;
static double Te2;
static double TrE;
static double TeffkillsA;
static double TregKillsTeff;
static double TeffKillsODC;
static double DACkill;
static double SecondInjectionTime;
static map <string, double> RATES_killingTransitions;
static vector<int> index_places_out;
static vector<int> index_places_in;

static double NKentry;
static double Cinf;
static double cIL10;
//static double Cteff;
// static double nk2 =  0.04166667; //....-> 1/24
static double probDup= 0.6666667;//....-> 2/3

static bool populate_data_structures = true;

// Definition of new structures:

void read_double(string fname, double& d)
{
  cout << "#### " << fname << "####" << endl;
  ifstream f (fname);
  string line;
  if(f.is_open())
  {
    getline(f,line);
    d = stod(line);
    f.close();
    cout << d << endl;
  }
  else
  {
    std::cerr<<"\nUnable to open " << fname << ": file do not exists\": file do not exists\n";
    exit(EXIT_FAILURE);
  }
}

void read_int(string fname, int& d)
{
  cout << "#### " << fname << "####" << endl;
  ifstream f (fname);
  string line;
  if(f.is_open())
  {
    getline(f,line);
    d = stod(line);
    f.close();
    cout << d << endl;
  }
  else
  {
    std::cerr<<"\nUnable to open " << fname << ": file do not exists\": file do not exists\n";
    exit(EXIT_FAILURE);
  }
}


void init_data_structures( map <string,int>& NumPlaces)
{

  // Costants reading:
  read_double("./NKentry", NKentry);
  read_double("./Cinf",Cinf);
  read_double("./CIL10",cIL10);
  //read_double("./Cteff",Cteff);
  read_double("./SecondInjectionTime",SecondInjectionTime);

  // rates reading
  read_double("./TeE",TeE);
  read_double("./TrE",TrE);
  read_double("./Tr2",Tr2);
  read_double("./Te2",Te2);
  read_double("./TekODC",TeffKillsODC);
  read_double("./TrkTe",TregKillsTeff);
  read_double("./TekA",TeffkillsA);
  read_double("./DACkill",DACkill);

  RATES_killingTransitions={{"TeffkillsA", TeffkillsA },
                            {"TeffKillsODC", TeffKillsODC },
                            {"TregKillsTeff_in", TregKillsTeff},
                            {"TregKillsTeff_out", TregKillsTeff}};

  populate_data_structures = false;
}


class TransPL {

  vector<vector<int>> v;

public:

  void insert(vector<int>& indPlacesOrdered, const int T ){
    vector<int>& vat = v.at(T);
    if(vat.size()==0)
    {
      vat.assign( indPlacesOrdered.begin(), indPlacesOrdered.end() );
    }
  }

  vector<int>& capture(const int T){
    return( v.at(T) );
  }

  void initialize(map <string,int>& NumTrans){
    if(v.size() == 0){
      v.resize(NumTrans.size());
    }
  }

  vector<int>& at(int T){
    return v.at(T) ;
  }
};



static TransPL trans_InputPlaces_ordered;



// this function returns the indexes of the input places ordered alphabetically of the firing transition!
vector<int>& InputPlacesOrderedAlphabetically(TransPL& trans_InputPlaces_ordered,  map <string,int>& NumPlaces, const struct InfTr* Trans, const int T, map <string,int>& NumTrans )
{
  trans_InputPlaces_ordered.initialize(NumTrans);


  if(trans_InputPlaces_ordered.at(T).size()==0) // If it is the first time that T fires than the class is updated with the ordered input places
  {
    int size = Trans[T].InPlaces.size();

    vector<string> orderedPlaces(size,"");

    for (int k=0; k<size; k++)
    {
      for (auto it=NumPlaces.begin(); it != NumPlaces.end() ; ++it)
      {
        if( it-> second == Trans[T].InPlaces[k].Id )
        {
          orderedPlaces.at(k) = it -> first;
          break;
        }
      }
    }

    sort(orderedPlaces.begin(),orderedPlaces.end());

    vector<int> indPlacesOrdered (size,0);
    int i=0;
    for( int k=0; k!=size;  k++)
    {

      indPlacesOrdered.at(i) = NumPlaces.find(orderedPlaces.at(k)) -> second;
      //cout << "transition: " << T << "\n place: " << orderedPlaces.at(k) << "\n" << endl;
      i++;
    }

    trans_InputPlaces_ordered.insert(indPlacesOrdered, T);
  }

  return trans_InputPlaces_ordered.capture(T);
}

// general transitions:

double TregDup(double *Value,  map <string,int>& NumTrans,  map <string,int>& NumPlaces, const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{

  if( populate_data_structures )
    init_data_structures(NumPlaces);

  double rate=0.0;
  vector<int> idx; // idx will store the input places indexes ordered alphabetically!! It is necessary to check if the transition is characterized by the right input places!!
  // For istance in this case I need to have in input the following places: DAC and Treg. If I am in the grid position xyz, than the input/output arcs must have the ight instances, this means that I can not have a wrong color instance otherwise the input places considered in the general functions could be wrong!!

  idx = InputPlacesOrderedAlphabetically(trans_InputPlaces_ordered, NumPlaces, Trans, T, NumTrans );
  double Treg=0.0 ;
  int idxTREG = idx.at(0);
  Treg = Value[idxTREG];

  rate =  Tr2 * Treg ;

  if(rate == 0.0 ) rate = 0.0000001;
  //cout << "Time: " << time << " ; rate: " << rate << "; transition: " << NameTrans[T] <<"\n"<< endl;
  return rate;

}

double TeffDup(double *Value,  map <string,int>& NumTrans,  map <string,int>& NumPlaces, const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{

  if( populate_data_structures )
    init_data_structures(NumPlaces);

  double rate = 0.0;
  double p ;
  vector<int> idx;
  double Teff = 0.0 ;

  idx = InputPlacesOrderedAlphabetically(trans_InputPlaces_ordered, NumPlaces, Trans, T, NumTrans );
  int idxTEFF = idx.at(0);
  Teff = Value[idxTEFF];
      
  if(NameTrans[T] == "TeffDup_Asym_out")    p = 1-probDup;
  else p = probDup;

 rate = Te2 * Teff;

 if( p*rate == 0.0 ) rate = 0.0000001;
 return p*rate;

}

double NKBorn(double *Value,  map <string,int>& NumTrans,  map <string,int>& NumPlaces, const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{
	double rate = 0.0;
	double NK = 0.0 ;
	NK = Value[NumPlaces.find("NK_out") -> second];

	if( NK < 30) rate = NKentry * (30 - NK);
	
    if(rate == 0.0 ) rate = 0.0000001;
	//cout << "Time: " << time << " ; rate: " << rate << "; transition: " << NameTrans[T] <<"\n"<< endl;
	return rate;
}

double TeffActivation(double *Value,  map <string,int>& NumTrans,  map <string,int>& NumPlaces, const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{

	if( populate_data_structures )
  init_data_structures(NumPlaces);

  double rate = 0.0;
  double A = 0.0;
  double INFg = 0.0 ;
  double RestingTeff = 0.0 ;

  if ( NameTrans[T] == "TeffActivation_out" )
  {
    //0 "Antigen", 1 "INFg" 2 "RestingTeff out "

    int idxA = NumPlaces.find("Antigen") -> second ;
    int idxINFg = NumPlaces.find("INFg_out") -> second ;
    int idxRestingTeff = NumPlaces.find("Resting_Teff_out") -> second ;

    A = Value[idxA];
    RestingTeff = Value[idxRestingTeff];
    INFg =  Value[idxINFg];

  }
  else
  {
    vector<int> idx; 
    idx = InputPlacesOrderedAlphabetically(trans_InputPlaces_ordered, NumPlaces, Trans, T, NumTrans );
    
    //0 "INF_g ", 1 ODC = Antigene ,  2 "TEFF"
    int idxA = idx.at(1);
    int idxINFg = idx.at(0);
    int idxRestingTeff = idx.at(2);

    A = Value[idxA];
    RestingTeff = Value[idxRestingTeff];
    INFg =  Value[idxINFg];
  }
  
  if ( A < 1.0) A = 0.0;
  else A = 1.0;
 //   rate = TeE * ( 1 - exp(-A/cA))* (0.5 + exp(-INFg/Cinf) ) * RestingTeff ;
 rate = TeE * RestingTeff * A * (0.5 + exp(-INFg/Cinf) ) ;
 
 if(rate == 0.0 ) rate = 0.0000001;
//cout<< "T: " <<  NameTrans[T] << "; RestingTeff: " << RestingTeff<<"; A: " << A << "; infg: "<< (0.5 + exp(-INFg/Cinf) ) << "; TeE: "<<TeE<<"\n"<<endl;
//cout << "Time: " << time << " ; rate: " << rate << "; transition: " << NameTrans[T] <<"\n"<< endl;
  return rate;
}

double TregActivation(double *Value,  map <string,int>& NumTrans,  map <string,int>& NumPlaces, const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{
  if( populate_data_structures )
    init_data_structures(NumPlaces);
  
  double rate = 0.0;
  double RestingTreg = 0.0 ;
  double Teff = 0.0;

  if ( NameTrans[T] == "TregActivation_out" )
  {
    int idxRestingTreg = NumPlaces.find("Resting_Treg_out") -> second ;
    int idxTeff = NumPlaces.find("Teff_out") -> second ;

    RestingTreg = Value[idxRestingTreg];
    Teff = Value[idxTeff];
  } 
  else
  {
    int idxRestingTreg = NumPlaces.find("Resting_Treg_in") -> second ;
    int idxTeff = NumPlaces.find("Teff_in") -> second ;

    RestingTreg = Value[idxRestingTreg];
    Teff = Value[idxTeff];
}
  //rate = TrE * ( (Teff )/(Teff + A+ 1) ) * RestingTreg;
  //rate =  Teff * RestingTreg * exp(-A/cA)  ;
 // rate = TrE *RestingTreg * Teff * exp(-A/cA)  ;
  rate = TrE *RestingTreg * Teff;
  
  if(rate == 0.0 ) rate = 0.0000001;
  //cout <<  NameTrans[T] << ": RestingTreg " << RestingTreg <<"; Teff " << Teff <<"; exp(-A/cA) " << exp(-A/cA) <<"\n"<< endl;
  return rate;
}

double MemActivation(double *Value,  map <string,int>& NumTrans,  map <string,int>& NumPlaces, const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{

  if( populate_data_structures )
    init_data_structures(NumPlaces);

  double MemE = 0.0;
  double rate = 0.0;

  int idxA = NumPlaces.find("Antigen") -> second ;
  int idxINFg = NumPlaces.find("INFg_out") -> second ;
  int idxMem = NumPlaces.find("EffectorMemory") -> second ;

  double A = 0.0;
  double INFg = 0.0 ;
  double Mem = 0.0;

  A = Value[idxA];
  Mem = Value[idxMem];
  INFg =  Value[idxINFg];

  // The memory activation starts after the second Atigene occurence!

  if( time > SecondInjectionTime)
    MemE = 2.0 * TeE;

  rate = MemE * Mem  * A * (0.5 + exp(-INFg/Cinf) ) ;

  if(rate == 0.0 ) rate = 0.0000001;
  
  return rate;
}

double Killing(double *Value, map <string,int>& NumTrans, map <string,int>& NumPlaces,const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{

  if( populate_data_structures )
    init_data_structures(NumPlaces);

  double rate=0.0;
  double intensity = 1.0;
  vector<int> indexesSum;

  rate = RATES_killingTransitions.find(NameTrans[T]) -> second ;

  if ( NameTrans[T] == "TeffkillsA" )
  {
  	  int idxA = NumPlaces.find("Antigen") -> second ;
  	  int idxINFg = NumPlaces.find("INFg_out") -> second ;
      int idxIL10 = NumPlaces.find("IL10_out") -> second ;
      int idxIL17 = NumPlaces.find("IL17_out") -> second ;
      int idxTeff = NumPlaces.find("Teff_out") -> second ;

      double dem = Value[idxIL17]  + Value[idxINFg] + Value[idxIL10];
  
      double p = 0.0;
      if( dem > 1 ) p = 0.5 * (Value[idxIL17]  + Value[idxINFg] - Value[idxIL10]) /( dem ) ;

      intensity = Value[idxA] * Value[idxTeff] * (1+p);
  }
  else if(NameTrans[T] == "TregKillsTeff_in" || NameTrans[T] == "TregKillsTeff_out" )
  	{
  	vector<int> idx; //  IL10 Teff Treg

  	idx = InputPlacesOrderedAlphabetically(trans_InputPlaces_ordered, NumPlaces, Trans, T, NumTrans );

  	int idxIL10 = idx.at(0);
  	int idxTeff = idx.at(1);
  	int idxTreg = idx.at(2);

  	intensity = Value[idxTreg] * Value[idxTeff] * (1 - exp(-Value[idxIL10]/cIL10) );

  	}else{
  		std::cerr<<"\n no killling transition selected \n";
  		exit(EXIT_FAILURE);
  }  
  
  double frate = rate * intensity;
  
  if(frate < 0.0000001 ) frate = 0.0000001;

  //cout << "Time: " << time << " ; rate: " << frate << "; transition: " << NameTrans[T] <<"\n"<< endl;
  return frate;
}

double KillingODC(double *Value, map <string,int>& NumTrans, map <string,int>& NumPlaces,const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{

  if( populate_data_structures )
    init_data_structures(NumPlaces);

  double rate=0.0;

  rate = RATES_killingTransitions.find("TeffKillsODC") -> second ;

  double intensity = 1.0;

  vector<int> idx; //  Env ODC Teff

  idx = InputPlacesOrderedAlphabetically(trans_InputPlaces_ordered, NumPlaces, Trans, T, NumTrans );

  int idxIL10 = idx.at(0);
  int idxIL17 = idx.at(1);
  int idxINFg = idx.at(2);
  int idxODC = idx.at(3);
  int idxTeff = idx.at(4);

  double dem = Value[idxIL17]  + Value[idxINFg] + Value[idxIL10];
  
  double p = 0.0;
  
  if( dem > 1 ) p = 0.5 * (Value[idxIL17]  + Value[idxINFg] - Value[idxIL10]) /( dem ) ;

  intensity = Value[idxODC] * Value[idxTeff] * (1+p);
  
  double frate = rate * intensity;
  
  if(frate < 0.0000001 ) frate = 0.0000001;

  //cout << "Time: " << time << " ; rate: " << frate  << "; transition: " << NameTrans[T] <<"\n"<< endl;
  return frate;
}


double DACKill(double *Value, map <string,int>& NumTrans, map <string,int>& NumPlaces,const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{

  if( populate_data_structures )
    init_data_structures(NumPlaces);

  double intensity = 1.0;
  double rate = 0.0;
  double Teff = Value[NumPlaces.find("Teff_out") -> second];
  double Treg = Value[NumPlaces.find("Treg_out") -> second];

  for (unsigned int k=0; k<Trans[T].InPlaces.size(); k++)
  {
    intensity *= pow(Value[Trans[T].InPlaces[k].Id],Trans[T].InPlaces[k].Card);
  }
  
  double Tcell = (Teff+Treg);
  if( Tcell < 1 ) Tcell = 1;
  
  rate = DACkill / Tcell ;
  
  double frate = rate * intensity;
  
  if(frate < 0.0000001 ) frate = 0.0000001;

  //cout << "Time: " << time << " ; rate: " << frate  << "; transition: " << NameTrans[T] <<"\n"<< endl;
  return frate;
}



