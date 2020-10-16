
/*!
 *************************************************************************************
 * \file q_offsets.c
 *
 * \brief
 *    read Quantization Offset matrix parameters from input file: q_OffsetMatrix.cfg
 *
 *************************************************************************************
 */
#include <stdlib.h>
#include <string.h>

#include "global.h"

extern char *GetConfigFileContent (char *Filename, int error_type);

#define MAX_ITEMS_TO_PARSE  1000

int offset4x4_check[6] = {0, 0, 0, 0, 0, 0};
int offset8x8_check[2] = {0, 0};

static const char OffsetType4x4[9][24] =
{
  "INTRA4X4_LUMA_INTRA",
  "INTRA4X4_CHROMAU_INTRA",
  "INTRA4X4_CHROMAV_INTRA",
  "INTRA4X4_LUMA_INTER",
  "INTRA4X4_CHROMAU_INTER",
  "INTRA4X4_CHROMAV_INTER",
  "INTER4X4_LUMA",
  "INTER4X4_CHROMAU",
  "INTER4X4_CHROMAV"
};

static const char OffsetType8x8[3][24] =
{
  "INTRA8X8_LUMA_INTRA",
  "INTRA8X8_LUMA_INTER",
  "INTER8X8_LUMA",
};


int LevelOffset4x4Luma_Intra[13][4][4];
int LevelOffset4x4Chroma_Intra[2][13][4][4];

int LevelOffset4x4Luma_Inter[13][4][4];
int LevelOffset4x4Chroma_Inter[2][13][4][4];

int LevelOffset8x8Luma_Intra[13][8][8];
int LevelOffset8x8Luma_Inter[13][8][8];

short OffsetList4x4input[9][16];
short OffsetList8x8input[3][64];
short OffsetList4x4[9][16];
short OffsetList8x8[3][64];

//short UseDefaultOffsetMatrix4x4Flag[6];
//short UseDefaultOffsetMatrix8x8Flag[2];

static const short Offset_intra_default_intra[16] =
{
  341,341,341,341,
  341,341,341,341,
  341,341,341,341,
  341,341,341,341
};

static const short Offset_intra_default_inter[16] =
{
  171,171,171,171,
  171,171,171,171,
  171,171,171,171,
  171,171,171,171,
};

static const short Offset_inter_default[16] =
{
  171,171,171,171,
  171,171,171,171,
  171,171,171,171,
  171,171,171,171,
};

static const short Offset8_intra_default_intra[64] =
{
  341,341,341,341,341,341,341,341,
  341,341,341,341,341,341,341,341,
  341,341,341,341,341,341,341,341,
  341,341,341,341,341,341,341,341,
  341,341,341,341,341,341,341,341,
  341,341,341,341,341,341,341,341,
  341,341,341,341,341,341,341,341,
  341,341,341,341,341,341,341,341
};

static const short Offset8_intra_default_inter[64] =
{
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171
};

static const short Offset8_inter_default[64] =
{
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171,
  171,171,171,171,171,171,171,171
};


/*!
 ***********************************************************************
 * \brief
 *    Check the parameter name.
 * \param s
 *    parameter name string
 * \param type
 *    4x4 or 8x8 offset matrix type
 * \return
 *    the index number if the string is a valid parameter name,         \n
 *    -1 for error
 ***********************************************************************
 */

int CheckOffsetParameterName (char *s, int *type)
{
  int i = 0;

  *type = 0;
  while ((OffsetType4x4[i] != NULL) && (i<9))
  {
    if (0==strcmp (OffsetType4x4[i], s))
      return i;
    else
      i++;
  }

  i = 0;
  *type = 1;
  while ((OffsetType8x8[i] != NULL) && (i<3))
  {
    if (0==strcmp (OffsetType8x8[i], s))
      return i;
    else
      i++;
  }

  return -1;
};

/*!
 ***********************************************************************
 * \brief
 *    Parse the Q Offset Matrix values read from cfg file.
 * \param buf
 *    buffer to be parsed
 * \param bufsize
 *    buffer size of buffer
 ***********************************************************************
 */
void ParseQOffsetMatrix (char *buf, int bufsize)
{
  char *items[MAX_ITEMS_TO_PARSE];
  int MapIdx;
  int item = 0;
  int InString = 0, InItem = 0;
  char *p = buf;
  char *bufend = &buf[bufsize];
  int IntContent;
  int i, j, range, type, cnt;
  short *OffsetList;

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

      case ',':
        p++;
        InItem = 0;
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

  for (i=0; i<item; i+=cnt)
  {
    cnt=0;
    if (0 > (MapIdx = CheckOffsetParameterName (items[i+cnt], &type)))
    {
      snprintf (errortext, ET_SIZE, " Parsing error in config file: Parameter Name '%s' not recognized.", items[i+cnt]);
      error (errortext, 300);
    }
    cnt++;
    if (strcmp ("=", items[i+cnt]))
    {
      snprintf (errortext, ET_SIZE, " Parsing error in config file: '=' expected as the second token in each item.");
      error (errortext, 300);
    }
    cnt++;

    if (!type) //4x4 Matrix
    {
      range = 16;
      OffsetList = OffsetList4x4input[MapIdx];
      offset4x4_check[MapIdx] = 1; //to indicate matrix found in cfg file
    }
    else //8x8 matrix
    {
      range = 64;
      OffsetList = OffsetList8x8input[MapIdx];
      offset8x8_check[MapIdx] = 1; //to indicate matrix found in cfg file
    }

    for(j=0; j<range; j++)
    {
      if (1 != sscanf (items[i+cnt+j], "%d", &IntContent))
      {
        snprintf (errortext, ET_SIZE, " Parsing error: Expected numerical value for Parameter of %s, found '%s'.", items[i], items[i+cnt+j]);
        error (errortext, 300);
      }

      OffsetList[j] = (short) IntContent; //save value in matrix
    }
    cnt+=j;
    printf (".");
  }
}


/*!
 ***********************************************************************
 * \brief
 *    Initialise Q offset matrix values.
 ***********************************************************************
 */
void Init_QOffsetMatrix ()
{
  char *content;

  if(input->OffsetMatrixPresentFlag)
  {
    printf ("Parsing Quantization Offset Matrix file %s ", input->QOffsetMatrixFile);
    content = GetConfigFileContent(input->QOffsetMatrixFile, 0);
    if(content!='\0')
      ParseQOffsetMatrix(content, strlen (content));
    else
    {
      printf("\nError: %s\nProceeding with default values for all matrices.", errortext);
      input->OffsetMatrixPresentFlag = 0;
    }

    printf("\n");

  
    free(content);
  }
}


/*!
 ************************************************************************
 * \brief
 *    Calculation of the quantization offset params at the frame level
 *
 * \par Input:
 *    none
 *
 * \par Output:
 *    none
 ************************************************************************
 */
 void CalculateOffsetParam()
 {
   int i, j, k, temp;
   int qp_per;
   
   if(input->OffsetMatrixPresentFlag)
   {
     for(k=0; k<13; k++)
     {
       qp_per = Q_BITS + k - 10;
       for(j=0; j<4; j++)
       {
         for(i=0; i<4; i++)
         {
           temp = (i<<2)+j;
           if (img->type == I_SLICE)
           {
             LevelOffset4x4Luma_Intra[k][j][i]         = (int)OffsetList4x4input[0][temp]<<qp_per;
             LevelOffset4x4Chroma_Intra[0][k][j][i]    = (int)OffsetList4x4input[1][temp]<<qp_per;
             LevelOffset4x4Chroma_Intra[1][k][j][i]    = (int)OffsetList4x4input[2][temp]<<qp_per;
           }
           else
           {
             LevelOffset4x4Luma_Intra[k][j][i]         = (int)OffsetList4x4input[3][temp]<<qp_per;
             LevelOffset4x4Chroma_Intra[0][k][j][i]    = (int)OffsetList4x4input[4][temp]<<qp_per;
             LevelOffset4x4Chroma_Intra[1][k][j][i]    = (int)OffsetList4x4input[5][temp]<<qp_per;
           }
           
           LevelOffset4x4Luma_Inter[k][j][i]         = (int)OffsetList4x4input[6][temp]<<qp_per;
           LevelOffset4x4Chroma_Inter[0][k][j][i]    = (int)OffsetList4x4input[7][temp]<<qp_per;
           LevelOffset4x4Chroma_Inter[1][k][j][i]    = (int)OffsetList4x4input[8][temp]<<qp_per;
         }
       }
     }
   }
   else
   {
     for(k=0; k<13; k++)
     {
       qp_per = Q_BITS + k - 10;
       for(j=0; j<4; j++)
       {
         for(i=0; i<4; i++)
         {
           temp = (i<<2)+j;
           if (img->type == I_SLICE)
           {
             LevelOffset4x4Luma_Intra[k][j][i]         = (int)Offset_intra_default_intra[temp]<<qp_per;
             LevelOffset4x4Chroma_Intra[0][k][j][i]    = (int)Offset_intra_default_intra[temp]<<qp_per;
             LevelOffset4x4Chroma_Intra[1][k][j][i]    = (int)Offset_intra_default_intra[temp]<<qp_per;
           }
           else
           {
             LevelOffset4x4Luma_Intra[k][j][i]         = (int)Offset_intra_default_inter[temp]<<qp_per;
             LevelOffset4x4Chroma_Intra[0][k][j][i]    = (int)Offset_intra_default_inter[temp]<<qp_per;
             LevelOffset4x4Chroma_Intra[1][k][j][i]    = (int)Offset_intra_default_inter[temp]<<qp_per;
           }
           LevelOffset4x4Luma_Inter[k][j][i]         = (int)Offset_inter_default[temp]<<qp_per;
           LevelOffset4x4Chroma_Inter[0][k][j][i]    = (int)Offset_inter_default[temp]<<qp_per;
           LevelOffset4x4Chroma_Inter[1][k][j][i]    = (int)Offset_inter_default[temp]<<qp_per;
         }
       }
     }
   }
 }
 
 /*!
 ************************************************************************
 * \brief
 *    Calculate the quantisation offset parameters
 *
 ************************************************************************
 */
void CalculateOffset8Param()
{

  int i, j, k, temp;
   int q_bits;
   
   if(input->OffsetMatrixPresentFlag)
   {
     for(k=0; k<13; k++)
     {
       q_bits = Q_BITS_8 + k - 10;
       for(j=0; j<8; j++)
       {
         for(i=0; i<8; i++)
         {           
           temp = (i<<3)+j;
           if (img->type == I_SLICE)
             LevelOffset8x8Luma_Intra[k][j][i] = (int)OffsetList8x8input[0][temp] << q_bits;
           else
             LevelOffset8x8Luma_Intra[k][j][i] = (int)OffsetList8x8input[1][temp] << q_bits;

           LevelOffset8x8Luma_Inter[k][j][i] = (int)OffsetList8x8input[2][temp] << q_bits;
         }
       }
     }
   }
   else
   {
     for(k=0; k<13; k++)
     {
       q_bits = Q_BITS_8 + k - 10;
       for(j=0; j<8; j++)
       {
         for(i=0; i<8; i++)
         {
           temp = (i<<3)+j;
           if (img->type == I_SLICE)
             LevelOffset8x8Luma_Intra[k][j][i] = (int)Offset8_intra_default_intra[temp] << q_bits;
           else
             LevelOffset8x8Luma_Intra[k][j][i] = (int)Offset8_intra_default_inter[temp] << q_bits;
           LevelOffset8x8Luma_Inter[k][j][i] = (int)Offset8_inter_default[temp] << q_bits;
         }
       }
     }
   }
}

