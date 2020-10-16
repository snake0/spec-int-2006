
/*!
 ***********************************************************************
 * \file
 *    configfile.c
 * \brief
 *    Configuration handling.
 * \author
 *  Main contributors (see contributors.h for copyright, address and affiliation details)
 *    - Stephan Wenger           <stewe@cs.tu-berlin.de>
 * \note
 *    In the future this module should hide the Parameters and offer only
 *    Functions for their access.  Modules which make frequent use of some parameters
 *    (e.g. picture size in macroblocks) are free to buffer them on local variables.
 *    This will not only avoid global variable and make the code more readable, but also
 *    speed it up.  It will also greatly facilitate future enhancements such as the
 *    handling of different picture sizes in the same sequence.                         \n
 *                                                                                      \n
 *    For now, everything is just copied to the inp_par structure (gulp)
 *
 **************************************************************************************
 * \par Configuration File Format
 **************************************************************************************
 * Format is line oriented, maximum of one parameter per line                           \n
 *                                                                                      \n
 * Lines have the following format:                                                     \n
 * \<ParameterName\> = \<ParameterValue\> # Comments \\n                                    \n
 * Whitespace is space and \\t
 * \par
 * \<ParameterName\> are the predefined names for Parameters and are case sensitive.
 *   See configfile.h for the definition of those names and their mapping to
 *   configinput->values.
 * \par
 * \<ParameterValue\> are either integers [0..9]* or strings.
 *   Integers must fit into the wordlengths, signed values are generally assumed.
 *   Strings containing no whitespace characters can be used directly.  Strings containing
 *   whitespace characters are to be inclosed in double quotes ("string with whitespace")
 *   The double quote character is forbidden (may want to implement something smarter here).
 * \par
 * Any Parameters whose ParameterName is undefined lead to the termination of the program
 * with an error message.
 *
 * \par Known bug/Shortcoming:
 *    zero-length strings (i.e. to signal an non-existing file
 *    have to be coded as "".
 *
 * \par Rules for using command files
 *                                                                                      \n
 * All Parameters are initially taken from DEFAULTCONFIGFILENAME, defined in configfile.h.
 * If an -f \<config\> parameter is present in the command line then this file is used to
 * update the defaults of DEFAULTCONFIGFILENAME.  There can be more than one -f parameters
 * present.  If -p <ParameterName = ParameterValue> parameters are present then these
 * override the default and the additional config file's settings, and are themselves
 * overridden by future -p parameters.  There must be whitespace between -f and -p commands
 * and their respective parameters
 ***********************************************************************
 */

#define INCLUDED_BY_CONFIGFILE_C

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#if defined WIN32
  #include <io.h>
#else
  #include <unistd.h>
#endif
#include <fcntl.h>
#include <sys/stat.h>

#include "global.h"
#include "configfile.h"

#include "fmo.h"

#if defined(SPEC_CPU)
InputParameters configinput;
#endif 

       char *GetConfigFileContent (char *Filename);
static void ParseContent (char *buf, int bufsize);
static int ParameterNameToMapIndex (char *s);
static int InitEncoderParams();
static int TestEncoderParams(int bitdepth_qp_scale);
static int DisplayEncoderParams();
static void PatchInp ();
static void ProfileCheck();
static void LevelCheck();


#define MAX_ITEMS_TO_PARSE  10000


/*!
 ***********************************************************************
 * \brief
 *   print help message and exit
 ***********************************************************************
 */
void JMHelpExit ()
{
  fprintf( stderr, "\n   lencod [-h] [-p defenc.cfg] {[-f curenc1.cfg]...[-f curencN.cfg]}"
    " {[-p EncParam1=EncValue1]..[-p EncParamM=EncValueM]}\n\n"    
    "## Parameters\n\n"

    "## Options\n"
    "   -h :  prints function usage\n"
    "   -d :  use <defenc.cfg> as default file for parameter initializations.\n"
    "         If not used then file defaults to encoder.cfg in local directory.\n"
    "   -f :  read <curencM.cfg> for reseting selected encoder parameters.\n"
    "         Multiple files could be used that set different parameters\n"
    "   -p :  Set parameter <EncParamM> to <EncValueM>.\n"
    "         See default encoder.cfg file for description of all parameters.\n\n"
    
    "## Supported video file formats\n"
    "   RAW:  .yuv -> YUV 4:2:0\n\n"
    
    "## Examples of usage:\n"
    "   lencod\n"
    "   lencod  -h\n"
    "   lencod  -d default.cfg\n"
    "   lencod  -f curenc1.cfg\n"
    "   lencod  -f curenc1.cfg -p InputFile=\"e:\\data\\container_qcif_30.yuv\" -p SourceWidth=176 -p SourceHeight=144\n"  
    "   lencod  -f curenc1.cfg -p FramesToBeEncoded=30 -p QPISlice=28 -p QPPSlice=28 -p QPBSlice=30\n");

  exit(-1);
}

/*!
 ***********************************************************************
 * \brief
 *    Parse the command line parameters and read the config files.
 * \param ac
 *    number of command line parameters
 * \param av
 *    command line parameters
 ***********************************************************************
 */
void Configure (int ac, char *av[])
{
  char *content;
  int CLcount, ContentLen, NumberParams;
  char *filename=DEFAULTCONFIGFILENAME;

  memset (&configinput, 0, sizeof (InputParameters));
  //Set default parameters.
  printf ("Setting Default Parameters...\n");
  InitEncoderParams();

  // Process default config file
  CLcount = 1;

  if (ac==2)
  {
    if (0 == strncmp (av[1], "-h", 2))
    {
      JMHelpExit();
    }
  }

  if (ac>=3)
  {
    if (0 == strncmp (av[1], "-d", 2))
    {
      filename=av[2];
      CLcount = 3;
    }
    if (0 == strncmp (av[1], "-h", 2))
    {
      JMHelpExit();
    }
  }
  printf ("Parsing Configfile %s", filename);
  content = GetConfigFileContent (filename);
  if (NULL==content)
    error (errortext, 300);
  ParseContent (content, strlen(content));
  printf ("\n");
  free (content);

  // Parse the command line

  while (CLcount < ac)
  {
    if (0 == strncmp (av[CLcount], "-h", 2))
    {
      JMHelpExit();
    }
    
    if (0 == strncmp (av[CLcount], "-f", 2))  // A file parameter?
    {
      content = GetConfigFileContent (av[CLcount+1]);
      if (NULL==content)
        error (errortext, 300);
      printf ("Parsing Configfile %s", av[CLcount+1]);
      ParseContent (content, strlen (content));
      printf ("\n");
      free (content);
      CLcount += 2;
    } else
    {
      if (0 == strncmp (av[CLcount], "-p", 2))  // A config change?
      {
        // Collect all data until next parameter (starting with -<x> (x is any character)),
        // put it into content, and parse content.

        CLcount++;
        ContentLen = 0;
        NumberParams = CLcount;

        // determine the necessary size for content
        while (NumberParams < ac && av[NumberParams][0] != '-')
          ContentLen += strlen (av[NumberParams++]);        // Space for all the strings
        ContentLen += 1000;                     // Additional 1000 bytes for spaces and \0s


        if ((content = malloc (ContentLen))==NULL) no_mem_exit("Configure: content");;
        content[0] = '\0';

        // concatenate all parameters identified before

        while (CLcount < NumberParams)
        {
          char *source = &av[CLcount][0];
          char *destin = &content[strlen (content)];

          while (*source != '\0')
          {
            if (*source == '=')  // The Parser expects whitespace before and after '='
            {
              *destin++=' '; *destin++='='; *destin++=' ';  // Hence make sure we add it
            } else
              *destin++=*source;
            source++;
          }
          *destin = '\0';
          CLcount++;
        }
        printf ("Parsing command line string '%s'", content);
        ParseContent (content, strlen(content));
        free (content);
        printf ("\n");
      }
      else
      {
        snprintf (errortext, ET_SIZE, "Error in command line, ac %d, around string '%s', missing -f or -p parameters?", CLcount, av[CLcount]);
        error (errortext, 300);
      }
    }
  }
  printf ("\n");
  PatchInp();
  if (input->DisplayEncParams)
    DisplayEncoderParams();
}

/*!
 ***********************************************************************
 * \brief
 *    allocates memory buf, opens file Filename in f, reads contents into
 *    buf and returns buf
 * \param Filename
 *    name of config file
 * \return
 *    if successfull, content of config file
 *    NULL in case of error. Error message will be set in errortext
 ***********************************************************************
 */
char *GetConfigFileContent (char *Filename)
{
  long FileSize;
  FILE *f;
  char *buf;

  if (NULL == (f = fopen (Filename, "r")))
  {
      snprintf (errortext, ET_SIZE, "Cannot open configuration file %s.", Filename);
      return NULL;
  }

  if (0 != fseek (f, 0, SEEK_END))
  {
    snprintf (errortext, ET_SIZE, "Cannot fseek in configuration file %s.", Filename);
    return NULL;
  }

  FileSize = ftell (f);
  if (FileSize < 0 || FileSize > 60000)
  {
    snprintf (errortext, ET_SIZE, "Unreasonable Filesize %ld reported by ftell for configuration file %s.", FileSize, Filename);
    return NULL;
  }
  if (0 != fseek (f, 0, SEEK_SET))
  {
    snprintf (errortext, ET_SIZE, "Cannot fseek in configuration file %s.", Filename);
    return NULL;
  }

  if ((buf = malloc (FileSize + 1))==NULL) no_mem_exit("GetConfigFileContent: buf");

  // Note that ftell() gives us the file size as the file system sees it.  The actual file size,
  // as reported by fread() below will be often smaller due to CR/LF to CR conversion and/or
  // control characters after the dos EOF marker in the file.

  FileSize = fread (buf, 1, FileSize, f);
  buf[FileSize] = '\0';


  fclose (f);
  return buf;
}


/*!
 ***********************************************************************
 * \brief
 *    Parses the character array buf and writes global variable input, which is defined in
 *    configfile.h.  This hack will continue to be necessary to facilitate the addition of
 *    new parameters through the Map[] mechanism (Need compiler-generated addresses in map[]).
 * \param buf
 *    buffer to be parsed
 * \param bufsize
 *    buffer size of buffer
 ***********************************************************************
 */
void ParseContent (char *buf, int bufsize)
{

  char *items[MAX_ITEMS_TO_PARSE];
  int MapIdx;
  int item = 0;
  int InString = 0, InItem = 0;
  char *p = buf;
  char *bufend = &buf[bufsize];
  int IntContent;
  double DoubleContent;
  int i;

// Stage one: Generate an argc/argv-type list in items[], without comments and whitespace.
// This is context insensitive and could be done most easily with lex(1).

  while (p < bufend)
  {
    switch (*p)
    {
      case 13:
        p++;
        break;
      case '#':                 // Found comment
        *p = '\0';              // Replace '#' with '\0' in case of comment immediately following integer or string
        while (*p != '\n' && p < bufend)  // Skip till EOL or EOF, whichever comes first
          p++;
        InString = 0;
        InItem = 0;
        break;
      case '\n':
        InItem = 0;
        InString = 0;
        *p++='\0';
        break;
      case ' ':
      case '\t':              // Skip whitespace, leave state unchanged
        if (InString)
          p++;
        else
        {                     // Terminate non-strings once whitespace is found
          *p++ = '\0';
          InItem = 0;
        }
        break;

      case '"':               // Begin/End of String
        *p++ = '\0';
        if (!InString)
        {
          items[item++] = p;
          InItem = ~InItem;
        }
        else
          InItem = 0;
        InString = ~InString; // Toggle
        break;

      default:
        if (!InItem)
        {
          items[item++] = p;
          InItem = ~InItem;
        }
        p++;
    }
  }

  item--;

  for (i=0; i<item; i+= 3)
  {
    if (0 > (MapIdx = ParameterNameToMapIndex (items[i])))
    {
      snprintf (errortext, ET_SIZE, " Parsing error in config file: Parameter Name '%s' not recognized.", items[i]);
      error (errortext, 300);
    }
    if (strcmp ("=", items[i+1]))
    {
      snprintf (errortext, ET_SIZE, " Parsing error in config file: '=' expected as the second token in each line.");
      error (errortext, 300);
    }

    // Now interpret the Value, context sensitive...

    switch (Map[MapIdx].Type)
    {
      case 0:           // Numerical
        if (1 != sscanf (items[i+2], "%d", &IntContent))
        {
          snprintf (errortext, ET_SIZE, " Parsing error: Expected numerical value for Parameter of %s, found '%s'.", items[i], items[i+2]);
          error (errortext, 300);
        }
        * (int *) (Map[MapIdx].Place) = IntContent;
        printf (".");
        break;
      case 1:
        strncpy ((char *) Map[MapIdx].Place, items [i+2], FILE_NAME_SIZE);
        printf (".");
        break;
      case 2:           // Numerical double
        if (1 != sscanf (items[i+2], "%lf", &DoubleContent))
        {
          snprintf (errortext, ET_SIZE, " Parsing error: Expected numerical value for Parameter of %s, found '%s'.", items[i], items[i+2]);
          error (errortext, 300);
        }
        * (double *) (Map[MapIdx].Place) = DoubleContent;
        printf (".");
        break;
      default:
        assert ("Unknown value type in the map definition of configfile.h");
    }
  }
  memcpy (input, &configinput, sizeof (InputParameters));
}

/*!
 ***********************************************************************
 * \brief
 *    Returns the index number from Map[] for a given parameter name.
 * \param s
 *    parameter name string
 * \return
 *    the index number if the string is a valid parameter name,         \n
 *    -1 for error
 ***********************************************************************
 */
static int ParameterNameToMapIndex (char *s)
{
  int i = 0;

  while (Map[i].TokenName != NULL)
    if (0==strcmp (Map[i].TokenName, s))
      return i;
    else
      i++;
  return -1;
};

/*!
 ***********************************************************************
 * \brief
 *    Sets initial values for encoding parameters.
 * \return
 *    -1 for error
 ***********************************************************************
 */
static int InitEncoderParams()
{
  int i = 0;

  while (Map[i].TokenName != NULL)
  {
    if (Map[i].Type == 0)
        * (int *) (Map[i].Place) = (int) Map[i].Default;
    else if (Map[i].Type == 2)
    * (double *) (Map[i].Place) = Map[i].Default;
      i++;
  }
  return -1;
};

/*!
 ***********************************************************************
 * \brief
 *    Validates encoding parameters.
 * \return
 *    -1 for error
 ***********************************************************************
 */
static int TestEncoderParams(int bitdepth_qp_scale)
{
  int i = 0;

  while (Map[i].TokenName != NULL)
  {
    if (Map[i].param_limits == 1)
    {
      if (Map[i].Type == 0)
      {
        if ( * (int *) (Map[i].Place) < (int) Map[i].min_limit || * (int *) (Map[i].Place) > (int) Map[i].max_limit )
        {
          snprintf(errortext, ET_SIZE, "Error in input parameter %s. Check configuration file. Value should be in [%d, %d] range.", Map[i].TokenName, (int) Map[i].min_limit,(int)Map[i].max_limit );
          error (errortext, 400);
        }
        
      }
      else if (Map[i].Type == 2)
      {
        if ( * (double *) (Map[i].Place) < Map[i].min_limit || * (double *) (Map[i].Place) > Map[i].max_limit )
        {
          snprintf(errortext, ET_SIZE, "Error in input parameter %s. Check configuration file. Value should be in [%.2f, %.2f] range.", Map[i].TokenName,Map[i].min_limit ,Map[i].max_limit );
          error (errortext, 400);
        }        
      }            
    }
    else if (Map[i].param_limits == 2)
    {
      if (Map[i].Type == 0)
      {
        if ( * (int *) (Map[i].Place) < (int) Map[i].min_limit )
        {
          snprintf(errortext, ET_SIZE, "Error in input parameter %s. Check configuration file. Value should not be smaller than %d.", Map[i].TokenName, (int) Map[i].min_limit);
          error (errortext, 400);
        }
        
      }
      else if (Map[i].Type == 2)
      {
        if ( * (double *) (Map[i].Place) < Map[i].min_limit )
        {
          snprintf(errortext, ET_SIZE, "Error in input parameter %s. Check configuration file. Value should not be smaller than %2.f.", Map[i].TokenName,Map[i].min_limit);
          error (errortext, 400);
        }        
      }
    }
    else if (Map[i].param_limits == 3) // Only used for QPs
    {
      if (Map[i].Type == 0)
      {
        if ( * (int *) (Map[i].Place) < (int) (Map[i].min_limit - bitdepth_qp_scale) || * (int *) (Map[i].Place) > (int) Map[i].max_limit )
        {
          snprintf(errortext, ET_SIZE, "Error in input parameter %s. Check configuration file. Value should be in [%d, %d] range.", Map[i].TokenName, (int) (Map[i].min_limit - bitdepth_qp_scale),(int)Map[i].max_limit );
          error (errortext, 400);
        }
        
      }
    }
 
    i++;
  }
  return -1;
};



/*!
 ***********************************************************************
 * \brief
 *    Outputs encoding parameters.
 * \return
 *    -1 for error
 ***********************************************************************
 */
static int DisplayEncoderParams()
{
  int i = 0;

  printf("******************************************************\n");
  printf("*               Encoder Parameters                   *\n");
  printf("******************************************************\n");
  while (Map[i].TokenName != NULL)
  {
    if (Map[i].Type == 0)
      printf("Parameter %s = %d\n",Map[i].TokenName,* (int *) (Map[i].Place));
    else if (Map[i].Type == 1)
      printf("Parameter %s = ""%s""\n",Map[i].TokenName,(char *)  (Map[i].Place));
    else if (Map[i].Type == 2)
      printf("Parameter %s = %.2f\n",Map[i].TokenName,* (double *) (Map[i].Place));
      i++;
  }
  printf("******************************************************\n");
  return -1;
};

/*!
 ************************************************************************
 * \brief
 *    calculate Ceil(Log2(uiVal))
 ************************************************************************
 */
unsigned CeilLog2( unsigned uiVal)
{
  unsigned uiTmp = uiVal-1;
  unsigned uiRet = 0;

  while( uiTmp != 0 )
  {
    uiTmp >>= 1;
    uiRet++;
  }
  return uiRet;
}


/*!
 ***********************************************************************
 * \brief
 *    Checks the input parameters for consistency.
 ***********************************************************************
 */
static void PatchInp ()
{
  int bitdepth_qp_scale = 6*(input->BitDepthLuma - 8);
  
  // These variables are added for FMO
  FILE * sgfile=NULL;
  int i;
  int frame_mb_only;
  int mb_width, mb_height, mapunit_height;

//  input->BitDepthChroma = input->BitDepthLuma;
  input->LowPassForIntra8x8 = 1;                    //low pass is always used

  TestEncoderParams(bitdepth_qp_scale);

  if (input->FrameRate == 0.0)
    input->FrameRate = INIT_FRAME_RATE;

  // Set block sizes

    input->blc_size[0][0]=16;
    input->blc_size[0][1]=16;

    input->blc_size[1][0]=16;
    input->blc_size[1][1]=16;

    input->blc_size[2][0]=16;
    input->blc_size[2][1]= 8;

    input->blc_size[3][0]= 8;
    input->blc_size[3][1]=16;

    input->blc_size[4][0]= 8;
    input->blc_size[4][1]= 8;

    input->blc_size[5][0]= 8;
    input->blc_size[5][1]= 4;

    input->blc_size[6][0]= 4;
    input->blc_size[6][1]= 8;

    input->blc_size[7][0]= 4;
    input->blc_size[7][1]= 4;


    input->part_size[0][0] = 4;
    input->part_size[0][1] = 4;
    input->part_size[1][0] = 4;
    input->part_size[1][1] = 4;
    input->part_size[2][0] = 4;
    input->part_size[2][1] = 2;
    input->part_size[3][0] = 2;
    input->part_size[3][1] = 4;
    input->part_size[4][0] = 2;
    input->part_size[4][1] = 2;
    input->part_size[5][0] = 2;
    input->part_size[5][1] = 1;
    input->part_size[6][0] = 1;
    input->part_size[6][1] = 2;
    input->part_size[7][0] = 1;
    input->part_size[7][1] = 1;


  // set proper log2_max_frame_num_minus4.
  {
    int storedBplus1 = (input->BRefPictures ) ? input->successive_Bframe + 1: 1;

//    log2_max_frame_num_minus4 = max( (int)(CeilLog2(input->no_frames * storedBplus1))-4, 0);
    if (input->Log2MaxFrameNum < 4)
      log2_max_frame_num_minus4 = max( (int)(CeilLog2(input->no_frames *storedBplus1 ))-4, 0);
    else 
      log2_max_frame_num_minus4 = input->Log2MaxFrameNum - 4;
  
  }

  log2_max_pic_order_cnt_lsb_minus4 = max( (int)(CeilLog2( 2*input->no_frames * (input->jumpd + 1))) -4, 0);

  // B picture consistency check
  if(input->successive_Bframe > input->jumpd)
  {
    snprintf(errortext, ET_SIZE, "Number of B-frames %d can not exceed the number of frames skipped", input->successive_Bframe);
    error (errortext, 400);
  }

  // Direct Mode consistency check
  if(input->successive_Bframe && input->direct_spatial_mv_pred_flag != DIR_SPATIAL && input->direct_spatial_mv_pred_flag != DIR_TEMPORAL)
  {
    snprintf(errortext, ET_SIZE, "Unsupported direct mode=%d, use TEMPORAL=0 or SPATIAL=1", input->direct_spatial_mv_pred_flag);
    error (errortext, 400);
  }

  if (input->PicInterlace>0 || input->MbInterlace>0)
  {
    if (input->directInferenceFlag==0)
      printf("\nDirectInferenceFlag set to 1 due to interlace coding.");
    input->directInferenceFlag=1;
  }

  if (input->PicInterlace>0)
  {
    if (input->IntraBottom!=0 && input->IntraBottom!=1)
    {
      snprintf(errortext, ET_SIZE, "Incorrect value %d for IntraBottom. Use 0 (disable) or 1 (enable).", input->IntraBottom);
      error (errortext, 400);
    }
  } 
  // Cabac/UVLC consistency check
  if (input->symbol_mode != UVLC && input->symbol_mode != CABAC)
  {
    snprintf (errortext, ET_SIZE, "Unsupported symbol mode=%d, use UVLC=0 or CABAC=1",input->symbol_mode);
    error (errortext, 400);
  }

  // Open Files
  if ((p_in=open(input->infile, OPENFLAGS_READ))==-1)
  {
    snprintf(errortext, ET_SIZE, "Input file %s does not exist",input->infile);
    error (errortext, 500);
  }

  if (strlen (input->ReconFile) > 0 && (p_dec=open(input->ReconFile, OPENFLAGS_WRITE, OPEN_PERMISSIONS))==-1)
  {
    snprintf(errortext, ET_SIZE, "Error open file %s", input->ReconFile);
    error (errortext, 500);
  }

  if (strlen (input->TraceFile) > 0 && (p_trace=fopen(input->TraceFile,"w"))==NULL)
  {
    snprintf(errortext, ET_SIZE, "Error open file %s", input->TraceFile);
    error (errortext, 500);
  }


  // consistency check size information
  /*
  if (input->img_height % 16 != 0 || input->img_width % 16 != 0)
  {
    snprintf(errortext, ET_SIZE, "Unsupported image format x=%d,y=%d, must be a multiple of 16",input->img_width,input->img_height);
    error (errortext, 400);
  }

  if (input->PicInterlace || input->MbInterlace)
  {
    if (input->img_height % 32 != 0 )
    { 
      snprintf(errortext, ET_SIZE, "Unsupported image format y=%d, must be a multiple of 32 for adaptive frame/field",input->img_height);
      error (errortext, 400);
    }
  }
 */
  if (input->img_width % 16 != 0)
  {
    img->auto_crop_right = 16-(input->img_width % 16);
  }
  else
  {
    img->auto_crop_right=0;
  }
  if (input->PicInterlace || input->MbInterlace)
  {
    if (input->img_height % 2 != 0)
    {
      error ("even number of lines required for interlaced coding", 500);
    }
    if (input->img_height % 32 != 0)
    {
      img->auto_crop_bottom = 32-(input->img_height % 32);
    }
    else
    {
      img->auto_crop_bottom=0;
    }
  }
  else
  {
    if (input->img_height % 16 != 0)
    {
      img->auto_crop_bottom = 16-(input->img_height % 16);
    }
    else
    {
      img->auto_crop_bottom=0;
    }
  }
  if (img->auto_crop_bottom || img->auto_crop_right)
  {
    printf ("Warning: Automatical cropping activated: Coded frame Size: %dx%d\n", input->img_width+img->auto_crop_right, input->img_height+img->auto_crop_bottom);
  }

  /*
  // add check for MAXSLICEGROUPIDS
  if(input->num_slice_groups_minus1>=MAXSLICEGROUPIDS)
  {
    snprintf(errortext, ET_SIZE, "num_slice_groups_minus1 exceeds MAXSLICEGROUPIDS");
    error (errortext, 500);
  }
  */

  // Following codes are to read slice group configuration from SliceGroupConfigFileName for slice group type 0,2 or 6
  if( (input->num_slice_groups_minus1!=0)&&
    ((input->slice_group_map_type == 0) || (input->slice_group_map_type == 2) || (input->slice_group_map_type == 6)) )
  { 
    if (strlen (input->SliceGroupConfigFileName) > 0 && (sgfile=fopen(input->SliceGroupConfigFileName,"r"))==NULL)
    {
      snprintf(errortext, ET_SIZE, "Error open file %s", input->SliceGroupConfigFileName);
      error (errortext, 500);
    }
    else
    {
      if (input->slice_group_map_type == 0) 
      {
        input->run_length_minus1=(int *)malloc(sizeof(int)*(input->num_slice_groups_minus1+1));
        if (NULL==input->run_length_minus1) 
          no_mem_exit("PatchInp: input->run_length_minus1");
        
        // each line contains one 'run_length_minus1' value
        for(i=0;i<=input->num_slice_groups_minus1;i++)
        {
          fscanf(sgfile,"%d",(input->run_length_minus1+i));
          fscanf(sgfile,"%*[^\n]");
          
        }
      }
      else if (input->slice_group_map_type == 2)
      {
        input->top_left=(int *)malloc(sizeof(int)*input->num_slice_groups_minus1);
        input->bottom_right=(int *)malloc(sizeof(int)*input->num_slice_groups_minus1);
        if (NULL==input->top_left) 
          no_mem_exit("PatchInp: input->top_left");
        if (NULL==input->bottom_right) 
          no_mem_exit("PatchInp: input->bottom_right");
        
        // every two lines contain 'top_left' and 'bottom_right' value
        for(i=0;i<input->num_slice_groups_minus1;i++)
        {
          fscanf(sgfile,"%d",(input->top_left+i));
          fscanf(sgfile,"%*[^\n]");
          fscanf(sgfile,"%d",(input->bottom_right+i));
          fscanf(sgfile,"%*[^\n]");
        }
        
      }
      else if (input->slice_group_map_type == 6)
      {
        int tmp;

        frame_mb_only = !(input->PicInterlace || input->MbInterlace);
        mb_width= (input->img_width+img->auto_crop_right)/16;
        mb_height= (input->img_height+img->auto_crop_bottom)/16;
        mapunit_height=mb_height/(2-frame_mb_only);
        
        input->slice_group_id=(byte * ) malloc(sizeof(byte)*mapunit_height*mb_width);
        if (NULL==input->slice_group_id) 
          no_mem_exit("PatchInp: input->slice_group_id");
        
        // each line contains slice_group_id for one Macroblock
        for (i=0;i<mapunit_height*mb_width;i++)
        {
          fscanf(sgfile,"%d", &tmp);
          input->slice_group_id[i]= (byte) tmp;
          if ( *(input->slice_group_id+i) > input->num_slice_groups_minus1 )
          {
            snprintf(errortext, ET_SIZE, "Error read slice group information from file %s", input->SliceGroupConfigFileName);
            error (errortext, 500);
          }
          fscanf(sgfile,"%*[^\n]");
        }
      }
      fclose(sgfile);
      
    }
  }
  
  
  if (input->PyramidRefReorder && input->PyramidCoding && (input->PicInterlace || input->MbInterlace))
  {
    snprintf(errortext, ET_SIZE, "PyramidRefReorder Not supported with Interlace encoding methods\n");
    error (errortext, 400);
  }

  if (input->PocMemoryManagement && input->PyramidCoding && (input->PicInterlace || input->MbInterlace))
  {
    snprintf(errortext, ET_SIZE, "PocMemoryManagement not supported with Interlace encoding methods\n");
    error (errortext, 400);
  }


  // frame/field consistency check
  if (input->PicInterlace != FRAME_CODING && input->PicInterlace != ADAPTIVE_CODING && input->PicInterlace != FIELD_CODING)
  {
    snprintf (errortext, ET_SIZE, "Unsupported PicInterlace=%d, use frame based coding=0 or field based coding=1 or adaptive=2",input->PicInterlace);
    error (errortext, 400);
  }

  // frame/field consistency check
  if (input->MbInterlace != FRAME_CODING && input->MbInterlace != ADAPTIVE_CODING && input->MbInterlace != FIELD_CODING)
  {
    snprintf (errortext, ET_SIZE, "Unsupported MbInterlace=%d, use frame based coding=0 or field based coding=1 or adaptive=2",input->MbInterlace);
    error (errortext, 400);
  }
   
 
  if ((!input->rdopt)&&(input->MbInterlace))
  {
    snprintf(errortext, ET_SIZE, "MB AFF is not compatible with non-rd-optimized coding.");
    error (errortext, 500);
  }

  if (input->rdopt>1)
  {
    snprintf(errortext, ET_SIZE, "RDOptimization=2 mode has been deactivated do to diverging of real and simulated decoders.");
    error (errortext, 500);
  }

  // Tian Dong: May 31, 2002
  // The number of frames in one sub-seq in enhanced layer should not exceed
  // the number of reference frame number.
  if ( input->NumFramesInELSubSeq >= input->num_ref_frames || input->NumFramesInELSubSeq < 0 )
  {
    snprintf(errortext, ET_SIZE, "NumFramesInELSubSeq (%d) is out of range [0,%d).", input->NumFramesInELSubSeq, input->num_ref_frames);
    error (errortext, 500);
  }
  // Tian Dong: Enhanced GOP is not supported in bitstream mode. September, 2002
  if ( input->NumFramesInELSubSeq > 0 && input->of_mode == PAR_OF_ANNEXB )
  {
    snprintf(errortext, ET_SIZE, "Enhanced GOP is not supported in bitstream mode and RTP mode yet.");
    error (errortext, 500);
  }
  // Tian Dong (Sept 2002)
  // The AFF is not compatible with spare picture for the time being.
  if ((input->PicInterlace || input->MbInterlace) && input->SparePictureOption == TRUE)
  {
    snprintf(errortext, ET_SIZE, "AFF is not compatible with spare picture.");
    error (errortext, 500);
  }

  // Only the RTP mode is compatible with spare picture for the time being.
  if (input->of_mode != PAR_OF_RTP && input->SparePictureOption == TRUE)
  {
    snprintf(errortext, ET_SIZE, "Only RTP output mode is compatible with spare picture features.");
    error (errortext, 500);
  }

  if( (input->WeightedPrediction > 0 || input->WeightedBiprediction > 0) && (input->MbInterlace))
  {
    printf("Weighted prediction coding is not supported for MB AFF currently.");
    error (errortext, 500);
  }
  if ( input->NumFramesInELSubSeq > 0 && input->WeightedPrediction > 0)
  {
    snprintf(errortext, ET_SIZE, "Enhanced GOP is not supported in weighted prediction coding mode yet.");
    error (errortext, 500);
  }

  //! the number of slice groups is forced to be 1 for slice group type 3-5
  if(input->num_slice_groups_minus1 > 0)
  {
    if( (input->slice_group_map_type >= 3) && (input->slice_group_map_type<=5) ) 
      input->num_slice_groups_minus1 = 1;
  }
  
  // Rate control
  if(input->RCEnable)
  {
    if ( ((input->img_height+img->auto_crop_bottom)*(input->img_width+img->auto_crop_right)/256)%input->basicunit!=0)
    {
      snprintf(errortext, ET_SIZE, "Basic unit is not defined correctly.");
      error (errortext, 500);
    }
  }

  if ((input->successive_Bframe)&&(input->BRefPictures)&&(input->idr_enable)&&(input->intra_period)&&(input->pic_order_cnt_type!=0))
  {
    error("Stored B pictures combined with IDR pictures only supported in Picture Order Count type 0\n",-1000);
  }
  
  if( !input->direct_spatial_mv_pred_flag && input->num_ref_frames<2 && input->successive_Bframe >0)
    error("temporal direct needs at least 2 ref frames\n",-1000);

  // frext
  if(input->AllowTransform8x8 && input->sp_periodicity /*SP-frames*/)
  {
    snprintf(errortext, ET_SIZE, "\nThe new 8x8 mode is not implemented for sp-frames.");
    error (errortext, 500);
  }

  if(input->AllowTransform8x8 && (input->ProfileIDC<FREXT_HP || input->ProfileIDC>FREXT_Hi444))
  {
    snprintf(errortext, ET_SIZE, "\nAllowTransform8x8 may be used only with ProfileIDC %d to %d.", FREXT_HP, FREXT_Hi444);
    error (errortext, 500);
  }
  if(input->ScalingMatrixPresentFlag && (input->ProfileIDC<FREXT_HP || input->ProfileIDC>FREXT_Hi444))
  {
    snprintf(errortext, ET_SIZE, "\nScalingMatrixPresentFlag may be used only with ProfileIDC %d to %d.", FREXT_HP, FREXT_Hi444);
    error (errortext, 500);
  }

  if(input->yuv_format==YUV422 && input->ProfileIDC < FREXT_Hi422)
  {
    snprintf(errortext, ET_SIZE, "\nFRExt Profile(YUV Format) Error!\nYUV422 can be used only with ProfileIDC %d or %d\n",FREXT_Hi422, FREXT_Hi444);
    error (errortext, 500);
  }
  if(input->yuv_format==YUV444 && input->ProfileIDC < FREXT_Hi444)
  {
    snprintf(errortext, ET_SIZE, "\nFRExt Profile(YUV Format) Error!\nYUV444 can be used only with ProfileIDC %d.\n",FREXT_Hi444);
    error (errortext, 500);
  }
  
  // Residue Color Transform
  if(input->yuv_format!=YUV444 && input->residue_transform_flag)
  {
    snprintf(errortext, ET_SIZE, "\nResidue color transform is supported only in YUV444.");
    error (errortext, 500);
  }

  ProfileCheck();
  LevelCheck();
}

void PatchInputNoFrames()
{
  // Tian Dong: May 31, 2002
  // If the frames are grouped into two layers, "FramesToBeEncoded" in the config file
  // will give the number of frames which are in the base layer. Here we let input->no_frames
  // be the total frame numbers.
  input->no_frames = 1+ (input->no_frames-1) * (input->NumFramesInELSubSeq+1);
  if ( input->NumFrameIn2ndIGOP )
    input->NumFrameIn2ndIGOP = 1+(input->NumFrameIn2ndIGOP-1) * (input->NumFramesInELSubSeq+1);
  FirstFrameIn2ndIGOP = input->no_frames;
}

static void ProfileCheck()
{
  if((input->ProfileIDC != 66 ) &&
     (input->ProfileIDC != 77 ) && 
     (input->ProfileIDC != 88 ) && 
     (input->ProfileIDC != FREXT_HP    ) && 
     (input->ProfileIDC != FREXT_Hi10P ) && 
     (input->ProfileIDC != FREXT_Hi422 ) && 
     (input->ProfileIDC != FREXT_Hi444 ))
  {
    snprintf(errortext, ET_SIZE, "Profile must be baseline(66)/main(77)/extended(88) or FRExt (%d to %d).", FREXT_HP,FREXT_Hi444);
    error (errortext, 500);
  }
  // baseline
  if (input->ProfileIDC == 66 )
  {
    if (input->successive_Bframe)
    {
      snprintf(errortext, ET_SIZE, "B pictures are not allowed in baseline.");
      error (errortext, 500);
    }
    if (input->sp_periodicity)
    {
      snprintf(errortext, ET_SIZE, "SP pictures are not allowed in baseline.");
      error (errortext, 500);
    }
    if (input->partition_mode)
    {
      snprintf(errortext, ET_SIZE, "Data partitioning is not allowed in baseline.");
      error (errortext, 500);
    }
    if (input->WeightedPrediction)
    {
      snprintf(errortext, ET_SIZE, "Weighted prediction is not allowed in baseline.");
      error (errortext, 500);
    }
    if (input->WeightedBiprediction)
    {
      snprintf(errortext, ET_SIZE, "Weighted prediction is not allowed in baseline.");
      error (errortext, 500);
    }
    if (input->symbol_mode == CABAC)
    {
      snprintf(errortext, ET_SIZE, "CABAC is not allowed in baseline.");
      error (errortext, 500);
    }
  }

  // main
  if (input->ProfileIDC == 77 )
  {
    if (input->sp_periodicity)
    {
      snprintf(errortext, ET_SIZE, "SP pictures are not allowed in main.");
      error (errortext, 500);
    }
    if (input->partition_mode)
    {
      snprintf(errortext, ET_SIZE, "Data partitioning is not allowed in main.");
      error (errortext, 500);
    }
    if (input->num_slice_groups_minus1)
    {
      snprintf(errortext, ET_SIZE, "num_slice_groups_minus1>0 (FMO) is not allowed in main.");
      error (errortext, 500);
    }
    if (input->redundant_slice_flag)
    {
      snprintf(errortext, ET_SIZE, "Redundant pictures are not allowed in main.");
      error (errortext, 500);
    }
  }

  // extended
  if (input->ProfileIDC == 88 )
  {
    if (!input->directInferenceFlag)
    {
      snprintf(errortext, ET_SIZE, "direct_8x8_inference flag must be equal to 1 in extended.");
      error (errortext, 500);
    }

    if (input->symbol_mode == CABAC)
    {
      snprintf(errortext, ET_SIZE, "CABAC is not allowed in extended.");
      error (errortext, 500);
    }
  }
}

static void LevelCheck()
{
  return;
}

