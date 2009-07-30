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
		printf("%f \t %f\n", x[i], y[i]);
	}
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
	int i;
	bool gotX, gotY;
	
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



/**********#include "clip_funcs.h"**********/
#include <string.h>
#include <stdlib.h>
#include <math.h>

#ifndef TRUE
#define TRUE            1
#endif /* TRUE */

#ifndef FALSE
#define FALSE           0
#endif /* FALSE */


/* stuff from polygons.c/h */
typedef enum {
  EDGE_W = 0,
  EDGE_E,
  EDGE_S,
  EDGE_N,
  NUM_EDGES,
  NO_EDGE
} edge;


/* value of oldPOS when clipPolygon generates a point */
#define NO_OLD_POS       -1

/* calculate the X-value for the intersection of the line
   (x1, y1) -> (x2, y2) with the horizontal line at boundary */
#define X_INTERCEPT(x1, x2, y1, y2, boundary)                            \
                (((boundary - y1) * (x1 - x2) / (y1 - y2)) + x1)

/* calculate the Y-value for the intersection of the line
   (x1, y1) -> (x2, y2) with the vertical line at boundary */
#define Y_INTERCEPT(x1, x2, y1, y2, boundary)                            \
                (((boundary - x1) * (y1 - y2) / (x1 - x2)) + y1)

/* calculate intersection with an edge */
#define EDGE_INTERSECTION(x1, y1, x2, y2, xOut, yOut, limits, e)         \
        {                                                                \
          /* calculate the intersection */                               \
          switch(e) {                                                    \
          case EDGE_N:                                                   \
          case EDGE_S:                                                   \
            xOut = X_INTERCEPT(x1, x2, y1, y2, limits[e]);               \
            yOut = limits[e];                                            \
            break;                                                       \
          case EDGE_E:                                                   \
          case EDGE_W:                                                   \
            xOut = limits[e];                                            \
            yOut = Y_INTERCEPT(x1, x2, y1, y2, limits[e]);               \
            break;                                                       \
          case NO_EDGE:                                                  \
          default:                                                       \
            break;                                                       \
          }                                                              \
        }

/*-----------------------------------------------------------------------------
   Functions are below...
  ---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
  inside:
    Returns whether or not a point is 'within' a given edge.
    Used in sutherlandHodgmanPolygonClip().
  ---------------------------------------------------------------------------*/
static short 
inside(double x, double y, double *limits, edge e)
{
  switch(e) 
    {
    case EDGE_N:
      return (DBL_LTEQ(y, limits[EDGE_N]));
    case EDGE_E:
      return (DBL_LTEQ(x, limits[EDGE_E]));
    case EDGE_S:
      return (DBL_GTEQ(y, limits[EDGE_S]));
    case EDGE_W:
      return (DBL_GTEQ(x, limits[EDGE_W]));
    case NO_EDGE:
    default:
      return FALSE;
    }
}


/*-----------------------------------------------------------------------------
  sutherlandHodgmanPolygonClip: 
    Clip a single polygon against one edge.
    Used in clipPolygon().
   
  Author:
    Nicholas Boers (June 18, 2003)

  Algorithm source:
    Computer Graphics: Principles and Practice
  
  Notes:
    - recommended allocated space for out*: 2 x *inVerts

  Returns:
    *outVerts >= 0 on success
    *outVerts = -1 on insufficient memory allocated in out*
  ---------------------------------------------------------------------------*/
static void
sutherlandHodgmanPolygonClip(double *inX, double *inY, int *inPOS,
                             int *inVerts,
                             double *outX, double *outY, int *outOLD,
                             int *outVerts,
                             double *limits, edge e, short polygons)
{
  double sX, sY, pX, pY;   /* for walking through polygon */
  double iX = 0, iY = 0;   /* for intersections */
  int allocatedMemory = *outVerts;
  int i;
 
  *outVerts = 0;

  sX = inX[(*inVerts) - 1];
  sY = inY[(*inVerts) - 1];
  
  /* Idea:
     - add p and any intersections on each iteration of the loop

     The points:
       - p is the 'current' point
       - s in the 'previous' point */

  for (i = 0; i < *inVerts; i++) {
    pX = inX[i];
    pY = inY[i];

    /* if clipping lines, don't test the edge connecting the end -> start */
    if (i == 0 && !polygons) {
      /* because we always add the segment's end point (rather than start
         point, simply add the end point here if necessary */
      if (inside(pX, pY, limits, e)) {
        if (*outVerts == allocatedMemory) {
          *outVerts = -1;
          return;
        }
        outX[*outVerts] = pX;
        outY[*outVerts] = pY;
        outOLD[*outVerts] = inPOS[i];
        (*outVerts)++;
      }
      sX = pX;
      sY = pY;
      continue;
    }

    /* 4 cases:
       #1: s inside     p inside
       #2: s inside     p outside
       #3: s outside    p outside
       #4: s outside    p inside */

    if (inside(pX, pY, limits, e)) {            /* case #1 & #4 */
      if (inside(sX, sY, limits, e)) {          /* case #1 */
        if (*outVerts == allocatedMemory) {
          *outVerts = -1;
          return;
        }
        outX[*outVerts] = pX;
        outY[*outVerts] = pY;
        outOLD[*outVerts] = inPOS[i];
        (*outVerts)++;
      }
      else {                                    /* case #4 */
        if (*outVerts == allocatedMemory) {
          *outVerts = -1;
          return;
        }
        EDGE_INTERSECTION(sX, sY, pX, pY, iX, iY, limits, e);
        outX[*outVerts] = iX;
        outY[*outVerts] = iY;
        outOLD[*outVerts] = NO_OLD_POS;
        (*outVerts)++;

        if (*outVerts == allocatedMemory) {
          *outVerts = -1;
          return;
        }
        outX[*outVerts] = pX;
        outY[*outVerts] = pY;
        outOLD[*outVerts] = inPOS[i];
        (*outVerts)++;
      }
    }
    else {                                      /* case #2 & #3 */
      if (inside(sX, sY, limits, e)) {          /* case #2 */
        if (*outVerts == allocatedMemory) {
          *outVerts = -1;
          return;
        }
        EDGE_INTERSECTION(sX, sY, pX, pY, iX, iY, limits, e);
        outX[*outVerts] = iX;
        outY[*outVerts] = iY;
        outOLD[*outVerts] = NO_OLD_POS;
        (*outVerts)++;
      }                                         /* no action for case #3 */
    }
    
    sX = pX;
    sY = pY;
  }
}


/*-----------------------------------------------------------------------------
  clipPolygon:
  ---------------------------------------------------------------------------*/

void
clipPolygon(double *inX, double *inY, int *inPOS, int inVerts,
            double *outX, double *outY, int *outOLD, int *outVerts,
            double *limits, short polygons)
{
  /* create a second copy of the output variables, for temporary work */
  double *tempX = (double *) malloc(sizeof(double) * (*outVerts));
  double *tempY = (double *) malloc(sizeof(double) * (*outVerts));
  int *tempOLD = (int *) malloc(sizeof(int) * (*outVerts));
  int tempVerts;

  int allocatedMemory = (*outVerts);
  int e; //in C this is "edge e;" but wont work for c++

  const short ERROR_NO_MEM = -1;
  const short ERROR_NO_OUT = -2;

  if (!tempX || !tempY || !tempOLD) {
    if (!tempX) free(tempX);
    if (!tempY) free(tempY);
    if (!tempOLD) free(tempOLD);

    (*outVerts) = ERROR_NO_MEM;
    return;
  }

  if (inVerts > (*outVerts)) {
    free(tempX);
    free(tempY);
    free(tempOLD);

    (*outVerts) = ERROR_NO_OUT;
    return;
  }
  memcpy(tempX, inX, sizeof(double) * inVerts);
  memcpy(tempY, inY, sizeof(double) * inVerts);
  memcpy(tempOLD, inPOS, sizeof(int) * inVerts);
  tempVerts = inVerts;

  /* clip against each edge */
  
  for (e = 0; e < NUM_EDGES; e++) {
    (*outVerts) = allocatedMemory;

    sutherlandHodgmanPolygonClip(tempX, tempY, tempOLD, &tempVerts,
                                 outX, outY, outOLD, outVerts,
                                 limits, (edge) e, polygons);
    /* sutherlandHodgmanPolygonClip() returns whether or not it was
       successful using tempOutVerts:
       -1 on insufficient memory allocated to tempOut* */

    if ((*outVerts) == -1) {
      free(tempX);
      free(tempY);
      free(tempOLD);

      *outVerts = ERROR_NO_OUT; /* out* were too small */
      return;
    }

    /* only copy it over if we'll need it again */
    if ((e + 1) < NUM_EDGES) {
      memcpy(tempX, outX, sizeof(double) * (*outVerts));
      memcpy(tempY, outY, sizeof(double) * (*outVerts));
      memcpy(tempOLD, outOLD, sizeof(int) * (*outVerts));
      tempVerts = (*outVerts);
    }
  }

  free(tempX);
  free(tempY);
  free(tempOLD);

} 
/**********end of "clip_funcs.h"**********/

using namespace std;

#define PROG_VERSION    "1.04"


char *inFileName=NULL, *outFileName=NULL, *programName;
/* limits are {minX, maxX, minY, maxY} */
double locLimits[4];
bool locLimitExists[4];







/*-----------------------------------------------------------------------------
  printUsage:
   
  Description:
  Prints usage information to the specified stream.
  ---------------------------------------------------------------------------*/
void printUsage(FILE *stream)
{
	fprintf (stream, "\
clipPolys (Version %s)\n\
\n\
Reads an ASCII file containing a PolySet and then clips it.  The first\n\
line of the PolySet input file must contain the field names (PID, SID,\n\
POS, X, Y), where SID is optional.  Subsequent lines must contain the\n\
data, with four or five fields per row.  All fields must be\n\
white-space delimited. The program generates a properly formatted\n\
PolySet.\n\
\n", 
           PROG_VERSION);

	fprintf (stream, "\
Usage: %s /i IFILE [/o OFILE] [/x MIN_X] [/X MAX_X] [/y MIN_Y] [/Y MAX_Y]\n\
\n\
  /i IN_FILE   ASCII input file containing a PolySet (required)\n\
  /o OUT_FILE  ASCII output file (defaults to standard output)\n\
  /x MIN_X     lower X limit (defaults to minimum X in the PolySet)\n\
  /X MAX_X     upper X limit (defaults to maximum X in the PolySet)\n\
  /y MIN_Y     lower Y limit (defaults to minimum Y in the PolySet)\n\
  /Y MAX_Y     upper Y limit (defaults to maximum Y in the PolySet)\n",
           programName);
}


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

      /* advance to the value for the flag */
      argv++;
      if (*argv == NULL)
        goto argument_parse_error;

      /* process the value, based on the flag */
      switch (flag)
        {
        case 'i':
          inFileName = *argv;
          break;
        case 'o':
          outFileName = *argv;
          break;
        case 'x':
          locLimits[0] = strtod (*argv, NULL);
          locLimitExists[0] = true;
          break;
        case 'X':
          locLimits[1] = strtod (*argv, NULL);
          locLimitExists[1] = true;
          break;
        case 'y':
          locLimits[2] = strtod (*argv, NULL);
          locLimitExists[2] = true;
          break;
        case 'Y':
          locLimits[3] = strtod (*argv, NULL);
          locLimitExists[3] = true;
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

void printPoly(FILE *outFile, int nVerts, double *x, double *y, int *oldPOS, int pid, int *sid=NULL, int isHole=0)
{
	int i;
	if (sid) {
		if (isHole)
			for (i=0;i<nVerts;i++) {
				fprintf(outFile, "%i\t%i\t%i\t%f\t%f\n", pid, *sid, nVerts-i, x[i], y[i]);
			}
		else
			for (i=0;i<nVerts;i++) {
				fprintf(outFile, "%i\t%i\t%i\t%f\t%f\n", pid, *sid, i+1, x[i], y[i]);
			}
	} else {
		if (isHole)
			for (i=0;i<nVerts;i++) {
				fprintf(outFile, "%i\t%i\t%f\t%f\n", pid, nVerts-i, x[i], y[i]);
			}
		else
			for (i=0;i<nVerts;i++) {
				fprintf(outFile, "%i\t%i\t%f\t%f\n", pid, i+1, x[i], y[i]);
			}
	}
}

/* setDefaultLimits:
   sets default values for the locLimits[4] array.
*/
void setDefaultLimits(PolySet *polys)
{
	int i;
	if (!polys->x.size() || !polys->y.size()) {
		fprintf(stderr, "empty poly set supplied.\n");
		return;
	}
	//store first x or y as limit, which will be used to find max
	if (!locLimitExists[0])
		locLimits[0]=polys->x[0];
	if (!locLimitExists[1])
		locLimits[1]=polys->x[0];
	if (!locLimitExists[2])
		locLimits[2]=polys->y[0];
	if (!locLimitExists[3])
		locLimits[3]=polys->y[0];
	
	//calculate the min/max of x or y
	for(i=0;i<polys->x.size();i++) {
		if (!locLimitExists[0] && locLimits[0] > polys->x[i])
			locLimits[0] = polys->x[i];
		if (!locLimitExists[1] && locLimits[1] < polys->x[i])
			locLimits[1] = polys->x[i];
		if (!locLimitExists[2] && locLimits[2] > polys->y[i])
			locLimits[2] = polys->y[i];
		if (!locLimitExists[3] && locLimits[3] < polys->y[i])
			locLimits[3] = polys->y[i];
	}
}

int main(int argc, char *argv[])
{
	char buf[MAXBUF+1], buf2[MAXBUF+1], *p;
	FILE *inFile, *outFile;
	int i,row;
	
	PolySet polys;
	
	programName = argv[0];
	if (argc < 3) {
		printUsage (stderr);
		exit (EXIT_FAILURE);
	}
	/* initialize locLimits and locLimitExists to 0 / false;
	  these will be copied over to the external limits/limitExists
	  after initialize() is called */
	memset (&locLimits, 0, sizeof(double) * 4);
	memset (&locLimitExists, 0, sizeof(bool) * 4);
	processArguments(argc, argv);
	
	if (inFileName == NULL) {
		fprintf (stderr, "\
Error: must specify an input file.\n");
		exit (EXIT_FAILURE);
    }
    //open input file
	inFile = fopen(inFileName, "r");
	if (inFile == NULL) {
		perror(inFileName);
		return(EXIT_FAILURE);
	}

	//open output file
	if (outFileName == NULL) {
		outFile = stdout;
	} else {
		outFile = fopen(outFileName, "w");
		if (outFile == NULL) {
			perror(outFileName);
			return(EXIT_FAILURE);
		}
    }
    //load data into the polyset from inFile
    parsePolySet(&polys, inFile);
    
    //calculate default values for missing area arguments
    setDefaultLimits(&polys);

	//print headerline
	if (polys.sid.size())
		fprintf(outFile, "PID\tSID\tPOS\tX\tY\n");
	else
		fprintf(outFile, "PID\tPOS\tX\tY\n");

	//iterate over each polygon and call the clipPolygon function (from PBSMapping)
	double *outX, *outY;
	int *outOLD;
	int outVerts, outVertsSpace;
	int lastPid=-1;
	int start=0;
	
	if (polys.x.size()<=1)
	{
		fprintf(stderr, "polygon size is too small (<= 1 points)\n");
		exit(1);
	}
	
	outVerts=outVertsSpace=2*polys.x.size();
	outX = new double[outVerts];
	outY = new double[outVerts];
	outOLD = new int[outVerts];

	for(i=0;i<polys.pid.size();i++) {
		if (lastPid!=polys.pid[i] && lastPid!=-1) {
			//clip polygon (where i goes from start...i)
			
			clipPolygon(&(polys.x[start]),
			            &(polys.y[start]),
			            &(polys.pos[start]),
			            i-start,
			            outX,
			            outY,
			            outOLD,
			            &outVerts,
			            locLimits,
			            1);
			if (outVerts<0)
				fprintf(stderr, "out of memory - bad\n");
			
			int *sid = (polys.sid.size()) ? &(polys.sid[start]) : NULL;
			int isHole = (polys.pos[start] > polys.pos[start+1]);
			printPoly(outFile, outVerts, outX, outY, outOLD, polys.pid[start], sid, isHole);

			//reset vars for next polygon
			outVerts=outVertsSpace;
			start=i;
			lastPid=polys.pid[i];
		}
		else if (lastPid==-1)
			lastPid=polys.pid[i];
	}
	clipPolygon(&(polys.x[start]),
	            &(polys.y[start]),
	            &(polys.pos[start]),
	            i-start,
	            outX,
	            outY,
	            outOLD,
	            &outVerts,
	            locLimits,
	            1);
	if (outVerts<0)
		fprintf(stderr, "out of memory - bad\n");
	
	int *sid = (polys.sid.size()) ? &(polys.sid.back()) : NULL;
	int isHole = (polys.pos[start] > polys.pos[start+1]);
	printPoly(outFile, outVerts, outX, outY, outOLD, polys.pid.back(), sid, isHole);


	//close files and then exit
	if (fclose(inFile) != 0) {
		fprintf(stderr, "Error: could not close input file.\n");
		return (EXIT_FAILURE);
	}
	if (fclose(outFile) != 0) {
		fprintf(stderr, "Error: could not close output file.\n");
		return (EXIT_FAILURE);
	}
	return(EXIT_SUCCESS);
}
