#include <iostream>
#include <vector>

/***********PARSER CODE COPY*****************/
#define MAXBUF 1024

using namespace std;

class PolySet {
	public:
	vector<double> x,y;
	vector<int> pid, pos, sid;
	void print();
};

class EventData {
	public:
		vector<double> x,y;
		vector<int> eid;
		void print();
};

class XYData {
	public:
	vector<double> x,y;
	vector<char*> org_str;
	char *header;
	~XYData();
	void print();
};

int parsePolySet(PolySet *polys, FILE *inFile);
int parseEventData(EventData *events, FILE *inFile);
int parseXYData(XYData *data, FILE *inFile);

//end of parser.h included stuff

void PolySet::print()
{
	int i;
	printf("\nPolySet\n");
	printf("PID \t SID \t POS \t X \t\t Y\n");
	for(i=0;i<pid.size();i++) {
		printf("%i \t %i \t %i \t %f \t %f\n", pid[i], sid[i], pos[i], x[i], y[i]);
	}	
}

void EventData::print()
{
	int i;
	printf("\nEvent Data\n");
	printf("EID \t X \t Y\n");
	for(i=0;i<eid.size();i++) {
		printf("%i \t %f \t %f\n", eid[i], x[i], y[i]);
	}
}

void XYData::print()
{
	int i;
	printf("\nXY Data\n");
	printf("X \t Y\n");
	for(i=0;i<x.size();i++) {
		printf("%s -=- %f \t %f\n", org_str[i], x[i], y[i]);
	}
}

XYData::~XYData()
{
	for(int i=0;i<org_str.size();i++) {
		if (org_str[i])
			delete org_str[i];
	}
	if (header)
		delete header;
}



int parsePolySet(PolySet *polys, FILE *inFile)
{
	int indexPID=-1, indexSID=-1, indexPOS=-1, indexX=-1, indexY=-1;
	char buf[MAXBUF+1];
	char *p;
	int i, lineNum;
	bool gotPID, gotSID, gotPOS, gotX, gotY;
	
    //get first line
	if (!fgets(buf, MAXBUF, inFile)) {
		if (feof(inFile))
			fprintf(stderr, "Error: unexpected end of file.\n");
		else
			perror("Error opening file");
	}
	
	//get the column index where each row is defined
	p=strtok(buf, " \t\n");
	i=0;
	while(p!=NULL) {
		if (strcmp(p,"PID")==0)
			indexPID=i;
		else if (strcmp(p,"SID")==0)
			indexSID=i;
		else if (strcmp(p,"POS")==0)
			indexPOS=i;
		else if (strcmp(p,"X")==0)
			indexX=i;
		else if (strcmp(p,"Y")==0)
			indexY=i;
		i++;
		p=strtok(NULL, " \t\n"); //get next token
	}
	if (indexPID==-1 || indexPOS==-1 || indexX==-1 || indexY==-1) {
		fprintf(stderr, "atleast one required header (PID, POS, X, Y) is missing\n");
		exit(1);
	}

	//read following lines (data)
	lineNum=1;
	while(fgets(buf, MAXBUF, inFile)) {
		lineNum++;
		//get the column index where each row is defined
		p=strtok(buf, " \t\n");
		i=0;
		gotPID=gotSID=gotPOS=gotX=gotY=false;
		while(p!=NULL) {
			if (i==indexPID) {
				polys->pid.push_back(atoi(p));
				gotPID=true;
			} else if (i==indexSID) {
				polys->sid.push_back(atoi(p));
				gotSID=true;
			} else if (i==indexPOS) {
				polys->pos.push_back(atoi(p));
				gotPOS=true;
			} else if (i==indexX) {
				polys->x.push_back(strtod(p,NULL));
				gotX=true;
			} else if (i==indexY) {
				polys->y.push_back(strtod(p,NULL));
				gotY=true;
			}
			i++;
			p=strtok(NULL, " \t\n"); //get next token
		}
		if (!gotPID || (!gotSID && indexSID>=0) || !gotPOS || !gotX || !gotY) {
			fprintf(stderr, "Incomplete polyset data on line %i\n", lineNum);
			exit(1);
		}
	}
	//display error if something other than EOF caused the loop to stop
	if (!feof(inFile))
		perror("Error opening file");	

	return 0;
}


int parseEventData(EventData *events, FILE *inFile)
{
	int indexEID=-1, indexX=-1, indexY=-1;
	char buf[MAXBUF+1];
	char *p;
	int i;
	bool gotEID, gotX, gotY;
	
    //get first line
	if (!fgets(buf, MAXBUF, inFile)) {
		if (feof(inFile))
			fprintf(stderr, "Error: unexpected end of file.\n");
		else
			perror("Error opening file");
	}
	
	//get the column index where each row is defined
	p=strtok(buf, " \t\n");
	i=0;
	while(p!=NULL) {
		if (strcmp(p,"EID")==0)
			indexEID=i;
		else if (strcmp(p,"X")==0)
			indexX=i;
		else if (strcmp(p,"Y")==0)
			indexY=i;
		i++;
		p=strtok(NULL, " \t\n"); //get next token
	}

	int lineNum=1;
	//read following lines (data)
	while(fgets(buf, MAXBUF, inFile)) {
		lineNum++;
		//get the column index where each row is defined
		p=strtok(buf, " \t\n");
		i=0;
		gotEID=gotX=gotY=false;
		while(p!=NULL) {
			if (i==indexEID) {
				events->eid.push_back(atoi(p));
				gotEID=true;
			} else if (i==indexX) {
				events->x.push_back(strtod(p,NULL));
				gotX=true;
			} else if (i==indexY) {
				events->y.push_back(strtod(p,NULL));
				gotY=true;
			}
			i++;
			p=strtok(NULL, " \t\n"); //get next token
		}
		if (!gotEID || !gotX || !gotY) {
			fprintf(stderr, "Incomplete event data on line %i\n", lineNum);
			exit(1);
		}
	}
	//display error if something other than EOF caused the loop to stop
	if (!feof(inFile))
		perror("Error opening file");	

	return 0;
}

int parseXYData(XYData *data, FILE *inFile)
{
	int indexX=-1, indexY=-1;
	char buf[MAXBUF+1];
	char *p;
	int i, len;
	bool gotX, gotY;
	
    //get first line
	if (!fgets(buf, MAXBUF, inFile)) {
		if (feof(inFile))
			fprintf(stderr, "Error: unexpected end of file.\n");
		else
			perror("Error opening file");
	}
	
	//save header for later output
	len=strlen(buf);
	data->header = new char[len+1];
	if (!data->header) {
			fprintf(stderr, "ERROR: out of memory\n");
			exit(1);
	}
	strcpy(data->header, buf);
	if (data->header[len-1]=='\n')
		data->header[len-1]='\0';
	
	//get the column index where each row is defined
	p=strtok(buf, " \t\n");
	i=0;
	while(p!=NULL) {
		if (strcmp(p,"X")==0)
			indexX=i;
		else if (strcmp(p,"Y")==0)
			indexY=i;
		i++;
		p=strtok(NULL, " \t\n"); //get next token
	}

	int lineNum=1;
	//read following lines (data)
	while(fgets(buf, MAXBUF, inFile)) {
		lineNum++;
		//save line for later output
		len=strlen(buf);
		p=new char[len+1];
		if (!p) {
			fprintf(stderr, "ERROR: out of memory\n");
			exit(1);
		}
		strcpy(p, buf);
		if (p[len-1]=='\n')
			p[len-1]='\0';
		data->org_str.push_back(p);
		//get the column index where each row is defined
		p=strtok(buf, " \t\n");
		i=0;
		gotX=gotY=false;
		while(p!=NULL) {
			if (i==indexX) {
				data->x.push_back(strtod(p,NULL));
				gotX=true;
			} else if (i==indexY) {
				data->y.push_back(strtod(p,NULL));
				gotY=true;
			}
			i++;
			p=strtok(NULL, " \t\n"); //get next token
		}
		if (!gotX || !gotY) {
			fprintf(stderr, "Incomplete event data on line %i\n", lineNum);
			exit(1);
		}
	}
	//display error if something other than EOF caused the loop to stop
	if (!feof(inFile))
		perror("Error opening file");	

	return 0;	
}


/***********END OF PARSER CODE***************/



/***************floating code start*********/
#include <math.h>
#include <float.h>

#define EPSILON         (DBL_EPSILON)

/* for checking the equality of floating point numbers */
#define FABS(n)                                                          \
                (((n) < 0) ? -(n) : (n))

#define DBL_EQ(n1, n2)                                                   \
                (((n1) == 0 && (n2) == 0)                                \
                 || (((n1) != 0)                                         \
                     && FABS((n1) - (n2))/FABS((n1)) <= DBL_EPSILON)     \
                 || FABS((n1) - (n2)) <= DBL_EPSILON)
#define DBL_LTEQ(n1, n2)                                                 \
                (((n1) < (n2)) || DBL_EQ((n1), (n2)))
#define DBL_GTEQ(n1, n2)                                                 \
                (((n1) > (n2)) || DBL_EQ((n1), (n2)))
#define DBL_LT(n1, n2)                                                   \
                (((n1) < (n2)) && !DBL_EQ((n1), (n2)))
#define DBL_GT(n1, n2)                                                   \
                (((n1) > (n2)) && !DBL_EQ((n1), (n2)))
/***************floating code end  *********/

#define DEG_TO_RAD      (M_PI / 180.0)      /* degrees to radians */
#define RAD_TO_DEG      (180.0 / M_PI)      /* radians to degrees */

/* the return types from UTM<-->LL conversion routines */
struct pair
{
  double x;
  double y;
};


/*-----------------------------------------------------------------------------
  lonlat_to_utm:
    Convert a point from Lat/Lon to UTM. All angles are in radians.
    Memory should be allocated for the structure before the function
    is called. 

  Author:
    Chris Grandin

  Algorithm Source:
    National Mapping Agency of Great Britain Ordnance Survey 
    <http://www.ordsvy.gov.uk>
  ---------------------------------------------------------------------------*/
void lonlat_to_utm(double lon, double lat, double *eastingNorthing_x, 
                   double *eastingNorthing_y, int utmZone);

/*-----------------------------------------------------------------------------
  utm_to_lonlat:
    Convert a point from UTM to Lat/Lon. All angles are in radians.
    Memory should be allocated for the structure before the function
    is called.

  Author:
    Chris Grandin

  Algorithm Source:
    National Mapping Agency of Great Britain Ordnance Survey 
    <http://www.ordsvy.gov.uk>
  ---------------------------------------------------------------------------*/
void utm_to_lonlat(double easting, double northing, double *lonlat_x,
                   double *lonlat_y, int utmZone);


/* constants of the earth (Ellipsoidal) and conversion factors */
#define A_PRE   6378137.0                 /* major axis of ellipsoid */
#define B_PRE   6356752.3141              /* minor axis of ellipsoid */
#define SF      0.9996                    /* scale factor */
#define A       (A_PRE * SF)              /* major after scaled */
#define B       (B_PRE * SF)              /* minor after scaled */
#define N       ((A - B) / (A + B))       /* ratio used in calculations */
#define E       sqrt(((A*A)-(B*B))/(A*A)) /* eccentricity */
#define E0      500000.0                  /* grid eastings of true origin */

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/*-----------------------------------------------------------------------------
  get_nu:
   
  Description:
  - get a radius of curvature at latitude 'phi' perpendicular to a meridian
  ---------------------------------------------------------------------------*/
static double get_nu(double phi)
{
  double sin1, sin2;

  sin1 = sin(phi);
  sin2 = sin1 * sin1;

  return (A / (sqrt(1.0 - (E * E * sin2))));
}

/*-----------------------------------------------------------------------------
  get_rho:
   
  Description:
  - get a radius of curvature of a meridian at latitude 'phi'
  ---------------------------------------------------------------------------*/
static double get_rho(double nu, double phi)
{
  double sin1, sin2;

  sin1 = sin(phi);
  sin2 = sin1 * sin1;

  return ((nu * (1.0 - E * E)) / (1.0 - (E * E * sin2)));
}

/*-----------------------------------------------------------------------------
  get_eta:
  ---------------------------------------------------------------------------*/
static double get_eta(double nu, double rho)
{
  return (sqrt((nu / rho) - 1.0));
}

/*-----------------------------------------------------------------------------
  get_M:

  Description:
  - get developed arc of a meridian from 'phi' to zero
  ---------------------------------------------------------------------------*/
static double get_M(double phi)
{
  double c1, c2, c3, c4, n2, n3, sin1, cos1, M;

  c1 = 5.0/4.0;
  c2 = 21.0/8.0;
  c3 = 15.0/8.0;
  c4 = 35.0/24.0;
  n2 = N*N;
  n3 = n2*N;
  sin1 = sin(phi);
  cos1 = cos(phi);
  M =     ((1.0 + N + (c1*n2) + (c1*n3)) * phi);
  M = M - (((3.0*N) + (3.0*n2) + (c2*n3)) * sin(phi) * cos(phi));
  M = M + (((c3*n2) + (c3*n3)) * sin(2.0*phi) * cos(2.0*phi));
  M = M - ((c4*n3) * sin(3.0*phi) * cos(3.0*phi));

  return (B*M);
}

/*-----------------------------------------------------------------------------
  lonlat_to_utm:
  ---------------------------------------------------------------------------*/
void lonlat_to_utm(double lon, double lat, double *eastingNorthing_x, 
                   double *eastingNorthing_y, int utmZone)
{
  double sin1, cos1, cos3, cos5, tan1, tan2, tan4, nu, rho, eta, eta2;
  double P, P2, P3, P4, P5, P6, I, II, III, IIIA, IV, V, VI;
  int lonOrig = -177 + ((utmZone - 1) * 6);

  sin1 = sin(lat);
  cos1 = cos(lat);
  cos3 = cos1*cos1*cos1;
  cos5 = cos3*cos1*cos1;
  tan1 = tan(lat);
  tan2 = tan1*tan1;
  tan4 = tan2*tan2;
  nu = get_nu(lat);
  rho = get_rho(nu,lat);
  eta = get_eta(nu,rho);
  eta2 = eta*eta;
  P = lon - (lonOrig * DEG_TO_RAD);
  P2 = P*P;
  P3 = P2*P;
  P4 = P3*P;
  P5 = P4*P;
  P6 = P5*P;
  I = get_M(lat);
  II = (nu/2.0)*sin1*cos1;
  III = (nu/24.0)*sin1*cos3*(5.0-tan2+9.0*eta2);
  IIIA = (nu/720.0)*sin1*cos5*(61.0-58.0*tan2+tan4);
  IV = nu*cos1;
  V = (nu/6.0)*cos3*((nu/rho)-tan2);
  VI = (nu/120.0)*cos5*(5.0-18.0*tan2+tan4+14.0*eta2-58.0*tan2*eta2);

  *eastingNorthing_y = I+P2*II+P4*III+P6*IIIA;
  *eastingNorthing_x = E0+P*IV+P3*V+P5*VI;
}

/*-----------------------------------------------------------------------------
  utm_to_lonlat:
  ---------------------------------------------------------------------------*/
void utm_to_lonlat(double easting, double northing, double *lonlat_x,
                   double *lonlat_y, int utmZone)
{
  double E1, E2, E3, E4, E5, E6, E7, lat1, M, nu, rho, eta, eta2;
  double tan1, tan2, tan4, tan6, sec1, nu3, nu5, nu7, VII, VIII;
  double IX, X, XI, XII, XIIA;
  int lonOrig = -177 + ((utmZone - 1) * 6);
  int i;

  E1 = easting - E0;
  E2 = E1*E1;
  E3 = E2*E1;
  E4 = E3*E1;
  E5 = E4*E1;
  E6 = E5*E1;
  E7 = E6*E1;
  lat1 = northing/A;
  M = get_M(lat1);

  for(i=0;i<10;i++){
    lat1 += (northing-M)/A;
    M = get_M(lat1);
  }

  nu = get_nu(lat1);
  rho = get_rho(nu,lat1);
  eta = get_eta(nu,rho);
  eta2 = eta*eta;
  tan1 = tan(lat1);
  tan2 = tan1*tan1;
  tan4 = tan2*tan2;
  tan6 = tan4*tan4;
  sec1 = 1.0/(cos(lat1));
  nu3 = nu*nu*nu;
  nu5 = nu3*nu*nu;
  nu7 = nu5*nu*nu;
  VII = tan1/(2.0*rho*nu);
  VIII = tan1/(24.0*rho*nu3)*(5.0+3.0*tan2+eta2-9.0*tan2*eta2);
  IX = tan1/(720.0*rho*nu5)*(61.0+90.0*tan2+45.0*tan4);
  X = sec1/nu;
  XI = sec1/(6.0*nu3)*(nu/rho+2.0*tan2);
  XII = sec1/(120.0*nu5)*(5.0+28.0*tan2+24.0*tan4);
  XIIA = sec1/(5040.0*nu7)*(61.0+662.0*tan2+1320.0*tan4+720.0*tan6);

  /* 11 Jul 2003 [Nicholas Boers]
     Swapped the x and y, because values being returned were backwards. */
  *lonlat_x = E1*X-E3*XI+E5*XII-E7*XIIA + (lonOrig * DEG_TO_RAD);
  *lonlat_y = lat1 - E2*VII+E4*VIII-E6*IX;
}

/********************End of conversions************************/


using namespace std;

#define PROG_VERSION "1.40"

char *programName, *inFileName=NULL, *outFileName=NULL;

FILE *outFile, *inFile;

typedef enum {
  LL_TO_UTM,
  UTM_TO_LL,
  UNKNOWN
} Conv;

static Conv conversion = UNKNOWN;
static long zone = -1;
static bool useMeters = false;

/*===========================================================================*/
static void printUsage()
{
  fprintf (stderr, "\
convUL (Version %s)\n\
\n\
Reads an ASCII file containing two fields named X and Y. The program\n\
converts each (X, Y) pair to a new pair (X2, Y2). The output file matches\n\
the input file, with the fields (X2, Y2) appended to the end of each line.\n\
\n",
           PROG_VERSION);
  fprintf (stderr, "\
Usage: %s /i IFILE [/o OFILE] (/u | /l) [/m] /z ZONE\n\
\n\
  /i IFILE    ASCII input file containing the X and Y data (required)\n\
  /o OFILE    ASCII output file (defaults to standard output)\n\
  /u (or /l)  convert to UTM (longitude-latitude) coordinates (required)\n\
  /m          use meters instead of kilometers as UTM measurement\n\
  /z ZONE     source or destination zone for the UTM coordinates (required)\n",
           programName);
}


/*===========================================================================*/
void processArguments (int argc, char *argv[])
{
  char flag;

  /* advance past the name of the program */
  argv++;

  /* walk through the remaining arguments */
  while (*argv != NULL)
    {
      /* get the flag */
      if ((*argv)[0] == '/')
        flag = (*argv)[1];
      else
        goto argument_parse_error;

      /* advance to the value for the flag, unless this one doesn't
         have a value to follow */
      if ((flag != 'u') && (flag != 'l') && (flag != 'm'))
        {
          argv++;
          if (*argv == NULL)
            goto argument_parse_error;
        }

      /* process the value, based on the flag */
      switch (flag)
        {
        case 'i':
          inFileName = *argv;
          break;
        case 'o':
          outFileName = *argv;
          break;
        case 'u':
          conversion = LL_TO_UTM;
          break;
        case 'l':
          conversion = UTM_TO_LL;
          break;
        case 'm':
          useMeters = true;
          break;
        case 'z':
          zone = strtol (*argv, NULL, 10);
          break;
        default:
          goto argument_parse_error;
        }
      argv++;
    }

  return;

 argument_parse_error:
  fprintf (stderr, "\
Error: could not parse arguments.\n\
Try `%s' (without arguments) for help.\n",
           programName);
  exit (1);
}


void convertFormat(XYData *data, FILE *outFile)
{
	double x, y;
	
	fprintf(outFile, "%s\tX2\tY2\n", data->header);
	
	/* duplicated loop inside switch for efficiency */
	switch (conversion) {
		case LL_TO_UTM:
			for(int i=0;i<data->x.size();i++) {
				data->x[i] *= DEG_TO_RAD;
				data->y[i] *= DEG_TO_RAD;
				lonlat_to_utm(data->x[i], data->y[i], &x, &y, zone);
				if (!useMeters) {
					x /= 1000;
					y /= 1000;
				}
				fprintf(outFile, "%s\t%f\t%f\n", data->org_str[i], x, y);
			}
			break;
		case UTM_TO_LL:
			for(int i=0;i<data->x.size();i++) {
				if (!useMeters) {
					data->x[i] *= 1000;
					data->y[i] *= 1000;
				}
				utm_to_lonlat(data->x[i], data->y[i], &x, &y, zone);
				x*=RAD_TO_DEG;
				y*=RAD_TO_DEG;
				fprintf(outFile, "%s\t%f\t%f\n", data->org_str[i], x, y);
			}
			break;
		default:
			fprintf (stderr, "Error: unhandled conversion type.\n");
      exit (EXIT_FAILURE);
	}
}


int main(int argc, char *argv[])
{
	programName = argv[0];
	processArguments(argc, argv);
	XYData xyData;

	if (inFileName==NULL || conversion==UNKNOWN || zone < 0) {
		printUsage();
		exit(1);
	}

	/* open input files */
	inFile = fopen(inFileName, "r");
	if (inFile == NULL) {
		perror(inFileName);
		return(EXIT_FAILURE);
	}
	
	/* open outFile */
	if (outFileName==NULL)
		outFile=stdout;
	else {
		outFile = fopen(outFileName, "w");
		if (outFile == NULL) {
			perror(outFileName);
			return(EXIT_FAILURE);
		}
	}

	parseXYData(&xyData, inFile);
	convertFormat(&xyData, outFile);
	
	fclose(inFile);
	fclose(outFile);
	
	return(EXIT_SUCCESS);
}
