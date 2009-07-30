#include <iostream>
#include <vector>


/***********PARSER CODE COPY*****************/
#include <iostream>
#include <vector>

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



using namespace std;

#define PROG_VERSION    "1.02"
#define MAXROWS         100

char *programName, *polyFileName=NULL, *eventFileName=NULL, *outFileName=NULL;

FILE *outFile, *inPolyFile, *inEventFile;

/* status options to pass back to R/S-PLUS */
#define PBS_SUCCESS     0       /* success */
#define PBS_ERR_MEM     1       /* insufficient memory */
#define PBS_ERR_OUT     2       /* output full */
#define PBS_ERR_OT1     3       /* other error */
#define PBS_ERR_OT2     4       /* other error */
#define PBS_ERR_OT3     5       /* other error */
#define PBS_ERR_OT4     6       /* other error */
#define PBS_ERR_OT5     7       /* other error */
#define PBS_ERR_OT6     8       /* other error */

#define FREE(ptr)       { if (ptr) { free(ptr); (ptr) = NULL; }}
	
/* possible return values from pointInPolygon */
#define UNKNOWN          -2
#define OUTSIDE          -1
#define BOUNDARY          0
#define INSIDE            1

#ifndef TRUE
#define TRUE            1
#endif /* TRUE */

#ifndef FALSE
#define FALSE           0
#endif /* FALSE */

typedef enum {
  EDGE_W = 0,
  EDGE_E,
  EDGE_S,
  EDGE_N,
  NUM_EDGES,
  NO_EDGE
} edge;


/*-----------------------------------------------------------------------------
  pointInPolygon:
  ---------------------------------------------------------------------------*/
short
pointInPolygon(double *inX, double *inY, int inVerts, double x, 
               double y)
{
  short result, inside;
  int i, j;
  double yCalc;

  /* each time we start a new point, reset */
  result = UNKNOWN;
  inside = FALSE;
    
  /* walk through the polygon */
  for (i = 0, j = inVerts-1; i < inVerts; j = i++) {
    /* in X-range */
    if ((DBL_LTEQ(inX[i], x) && DBL_LT(x, inX[j])) ||
        (DBL_LTEQ(inX[j], x) && DBL_LT(x, inX[i]))) {

      yCalc = (inY[i] - inY[j]) / (inX[i] - inX[j]) * (x - inX[j]) + inY[j];

      if (DBL_LT(y, yCalc))
        inside = !inside;
      else if (DBL_EQ(y, yCalc))
        result = BOUNDARY;

    }      
    else if ((DBL_LTEQ(inX[i], x) &&  DBL_LTEQ(x, inX[j])) ||
             (DBL_LTEQ(inX[j], x) &&  DBL_LTEQ(x, inX[i]))) {
      
      if ((DBL_EQ(x, inX[i]) && DBL_EQ(y, inY[i])) ||
          (DBL_EQ(x, inX[j]) && DBL_EQ(y, inY[j])))
        result = BOUNDARY;
      else if (DBL_EQ(inX[i], inX[j]) &&
               ((DBL_LTEQ(inY[i], y) && DBL_LTEQ(y, inY[j])) ||
                (DBL_LTEQ(inY[j], y) && DBL_LTEQ(y, inY[i]))))
        result = BOUNDARY;

    }
  }

  if (result == UNKNOWN)
    result = (inside) ? INSIDE : OUTSIDE;

  return(result);
}


/*-----------------------------------------------------------------------------
  pnpoly:
    This function computes whether or not an array of points lie
    outside, on the boundary of, or inside a polygon.  The results are
    returned in `results' (an array).
  
    The polygon can optionally be closed: (x_0, y_0) == (x_n, y_n).
  
  Author:
    Nicholas Boers (June 13, 2003)
  ---------------------------------------------------------------------------*/
void
pnpoly(int *polyPts, double *polyX, double *polyY,
       int *pts, double *x, double *y,
       int *results)
{
  int i;
  double limits[NUM_EDGES];

  /* calculate the limits of the polygon */
  if (*polyPts > 0) {
    limits[EDGE_W] = limits[EDGE_E] = polyX[0];
    limits[EDGE_S] = limits[EDGE_N] = polyY[0];
  }
  for (i = 1; i < *polyPts; i++) {
    if (DBL_LT(polyX[i], limits[EDGE_W])) limits[EDGE_W] = polyX[i];
    if (DBL_GT(polyX[i], limits[EDGE_E])) limits[EDGE_E] = polyX[i];
    if (DBL_LT(polyY[i], limits[EDGE_S])) limits[EDGE_S] = polyY[i];
    if (DBL_GT(polyY[i], limits[EDGE_N])) limits[EDGE_N] = polyY[i];
  }

  /* walk through the points */
  for (i = 0; i < *pts; i++) {
    /* do a quick test to see if the event is outside the limits */
    if (DBL_LT(x[i], limits[EDGE_W]) || DBL_GT(x[i], limits[EDGE_E]) ||
        DBL_LT(y[i], limits[EDGE_S]) || DBL_GT(y[i], limits[EDGE_N]))
      results[i] = OUTSIDE;
    else
      results[i] = pointInPolygon(polyX, polyY, *polyPts, x[i], y[i]);
  }
}


/*-----------------------------------------------------------------------------
  polyStartsEnds:
    This function determines the indices (0 .. n-1) where polygons
    start and end.
  
  Author:
    Nicholas Boers (June 11, 2003)
  
  Returns: 
    the number of polygons (start/end pairs).
  ---------------------------------------------------------------------------*/
static int
polyStartsEnds(int *polyStarts, int *polyEnds,
               int *inPID, int *inSID, int *inVerts)
{
  int curPID, curSID;
  int count = 0;
  int i;

  /* ensure at least one polygon must exist */
  if (*inVerts == 0)
    return 0;

  /* set up for the first one */
  curPID = inPID[0];
  curSID = inSID[0];

  /* add the start of the first one */
  *polyStarts++ = 0;
  count++;
  
  /* walk through all the vertices (less the first one)... */
  for (i = 1; i < *inVerts; i++) {
    if (inPID[i] != curPID ||
        inSID[i] != curSID) {
      curPID = inPID[i];
      curSID = inSID[i];

      *polyEnds++ = i - 1;
      *polyStarts++ = i;
      count++;
    }
  }

  /* add the end of the last one */
  *polyEnds = i - 1;

  return count;
}


/*-----------------------------------------------------------------------------
  findPolys:
    Locate events within a polyset.
  
  Author:
    Nicholas Boers
  
  Notes:
    - recommended allocated space for out*: 
  ---------------------------------------------------------------------------*/
void
findPolys(int *inEventsID, double *inEventsXY, int *inEvents,
          int *inPolysID, double *inPolysXY, int *inPolys,
          int *outID, int *outIDs,
          int *status)
{
  /* declarations to make accessing values user-friendly */
  double *inEventsX = inEventsXY;
  double *inEventsY = inEventsXY + (1 * (*inEvents));

  int *inPolysPID = inPolysID;
  int *inPolysSID = inPolysID + (1 * (*inPolys));
  int *inPolysPOS = inPolysID + (2 * (*inPolys));
  double *inPolysX = inPolysXY;
  double *inPolysY = inPolysXY + (1 * (*inPolys));

  int *outEID = outID;
  int *outPID = outID + (1 * (*outIDs));
  int *outSID = outID + (2 * (*outIDs));
  int *outBdry = outID + (3 * (*outIDs));

  int i, j;

  int nVerts;
  int nPolys;
  int *polyStarts = (int *) malloc(sizeof(int) * (*inPolys));
  int *polyEnds = (int *) malloc(sizeof(int) * (*inPolys));

  int *results = (int *) malloc(sizeof(int) * (*inEvents));
  int *resultsTemp = (int *) malloc(sizeof(int) * (*inEvents));
  short *boundary = (short *) malloc(sizeof(short) * (*inEvents));

  int parentPID = -1, parentSID = -1;
  short isHole, isNext;

  int allocatedMemory = (*outIDs);
  *outIDs = 0;

  if (results == NULL || polyStarts == NULL || polyEnds == NULL ||
      resultsTemp == NULL || boundary == NULL) {
    (*status) = PBS_ERR_MEM;
    goto FINDPOLYS_FREE_MEM;
  }

  nPolys = polyStartsEnds(polyStarts, polyEnds, inPolysPID, 
                          inPolysSID, inPolys);

  /* walk through all the polygons */
  for (i = 0; i < nPolys; i++) {
    /* run points-in-polygons */
    nVerts = polyEnds[i] - polyStarts[i] + 1;
    /* does not dynamically allocate memory */
    pnpoly(&nVerts, &inPolysX[polyStarts[i]], &inPolysY[polyStarts[i]],
           inEvents, inEventsX, inEventsY,
           results);

    /* if !hole, then set "parent" variables */
    if (!(isHole = (inPolysPOS[polyStarts[i]] > 
                    inPolysPOS[polyStarts[i] + 1]))) {
      parentPID = inPolysPID[polyStarts[i]];
      parentSID = inPolysSID[polyStarts[i]];
    }

    /* process the results from points-in-polygons */
    if (!isHole) {
      for (j = 0; j < *inEvents; j++) {
        resultsTemp[j] = (results[j] == INSIDE || results[j] == BOUNDARY);
        boundary[j] = (results[j] == BOUNDARY);
      }
    }
    else {
      for (j = 0; j < *inEvents; j++) {
        resultsTemp[j] = (resultsTemp[j] && !(results[j] == INSIDE));
        boundary[j] = boundary[j] || (results[j] == BOUNDARY);
      }
    }

    /* determine if the next one is a hole */
    if ((isNext = ((i + 1) < nPolys)))
      /* if the polySet is valid, the PID check is useless... */
      isHole = ((inPolysPID[polyStarts[i]] == 
                 inPolysPID[polyStarts[i + 1]]) &&
                (inPolysPOS[polyStarts[i + 1]] > 
                 inPolysPOS[polyStarts[i + 1] + 1]));

    /* output when necessary */
    if ((isNext && !isHole) || (!isNext)) {
      for (j = 0; j < *inEvents; j++) {
        if (resultsTemp[j]) {
          if ((*outIDs) >= allocatedMemory) {
            (*status) = PBS_ERR_OUT;
            goto FINDPOLYS_FREE_MEM;
          }
          outEID[*outIDs] = inEventsID[j];
          outPID[*outIDs] = parentPID;
          outSID[*outIDs] = parentSID;
          outBdry[*outIDs] = boundary[j];
          (*outIDs)++;
        }
      }
    }
  }

  (*status) = PBS_SUCCESS;

 FINDPOLYS_FREE_MEM:
  FREE(polyStarts);
  FREE(polyEnds);
  FREE(results);
  FREE(resultsTemp);
  FREE(boundary);

#ifdef _MCD_CHECK
  showMemStats();
#endif /* _MCD_CHECK */
}


/*-----------------------------------------------------------------------------
  printUsage:
   
  Description:
  Prints usage information to the specified stream.
  ---------------------------------------------------------------------------*/
void printUsage()
{
      fprintf (stderr, "\
findPolys (Version %s)\n\
\n\
Reads two ASCII files: one containing a PolySet and the other containing\n\
EventData.  The program then determines which events fall inside the\n\
polygons.\n\
\n",
               PROG_VERSION);
      fprintf (stderr, "\
Usage: %s /p POLY_FILE /e EVENT_FILE [/o OFILE]\n\
\n\
  /p POLY_FILE input file containing the PolySet (required)\n\
  /e EVENT_FILE input file containing EventData (required)\n\
  /o OFILE output file (defaults to standard output)\n",
               programName);
      exit(EXIT_FAILURE);
}


void
processArguments (int argc, char *argv[])
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
      if (*(++argv) == NULL)
        goto argument_parse_error;

      /* process the value, based on the flag */
      switch (flag)
        {
        case 'p':
          polyFileName = *argv;
          break;
        case 'e':
          eventFileName = *argv;
          break;
        case 'o':
          outFileName = *argv;
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

void findPolyWrap(PolySet *polys, EventData *events, FILE *outFile)
{
	int *inEventsID, inEvents;
	double *inEventsXY;
	
	inEvents=events->eid.size();
	inEventsID=new int[inEvents];
	for(int i=0;i<inEvents;i++)
		inEventsID[i]=events->eid[i];
	
	inEventsXY = new double[2*inEvents];
	for(int i=0;i<inEvents;i++)
		inEventsXY[i]=events->x[i];
	for(int i=0;i<inEvents;i++)
		inEventsXY[inEvents+i]=events->y[i];

    int *inPolysID, inPolys;
    double *inPolysXY;
    
    inPolys=polys->pid.size();
    inPolysID=new int[3*inPolys];
	for(int i=0;i<inPolys;i++)
		inPolysID[i]=polys->pid[i];
	if (polys->sid.size())
		for(int i=0;i<inPolys;i++)
			inPolysID[inPolys+i]=polys->sid[i];
	else
		for(int i=0;i<inPolys;i++)
			inPolysID[inPolys+i]=-1;
	for(int i=0;i<inPolys;i++)
		inPolysID[2*inPolys+i]=polys->pos[i];
	
	inPolysXY = new double[2*inPolys];
	for(int i=0;i<inPolys;i++)
		inPolysXY[i]=polys->x[i];
	for(int i=0;i<inPolys;i++)
		inPolysXY[inPolys+i]=polys->y[i];
    
    int outID[4*25], outIDs=25;
    int status;

	findPolys(inEventsID, inEventsXY, &inEvents,
          inPolysID, inPolysXY, &inPolys,
          outID, &outIDs,
          &status);
	
	if (polys->sid.size()) {
		fprintf(outFile, "EID\tPID\tSID\tBdry\n");
		for(int i=0;i<outIDs;i++) {
			fprintf(outFile, "%i\t", outID[i]);
			fprintf(outFile, "%i\t", outID[i+1*25]);
			fprintf(outFile, "%i\t", outID[i+2*25]);
			fprintf(outFile, "%i\n", outID[i+3*25]);
		}
	} else { //no SID
		fprintf(outFile, "EID\tPID\tBdry\n");
		for(int i=0;i<outIDs;i++) {
			fprintf(outFile, "%i\t", outID[i]);
			fprintf(outFile, "%i\t", outID[i+1*25]);
			fprintf(outFile, "%i\n", outID[i+3*25]);
		}
	}
	
}

int main(int argc, char *argv[])
{
	programName = argv[0];
	processArguments(argc, argv);
	PolySet polys;
	EventData events;
	if (polyFileName==NULL || eventFileName==NULL) {
		printUsage();
		exit(1);
	}
	/* open input files */
	inPolyFile = fopen(polyFileName, "r");
	if (inPolyFile == NULL) {
		perror(polyFileName);
		return(EXIT_FAILURE);
	}
	inEventFile = fopen(eventFileName, "r");
	if (inEventFile == NULL) {
		perror(eventFileName);
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
	
	parsePolySet(&polys, inPolyFile);
	parseEventData(&events, inEventFile);
	
	//polys.print();
	//events.print();
	
	findPolyWrap(&polys, &events, outFile);
	
	fclose(inEventFile);
	fclose(inPolyFile);
	
	return(EXIT_SUCCESS);
}
