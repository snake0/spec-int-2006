
/*!
 **************************************************************************************
 * \file
 *    parset.c
 * \brief
 *    Picture and Sequence Parameter set generation and handling
 *  \date 25 November 2002
 * \author
 *    Main contributors (see contributors.h for copyright, address and affiliation details) 
 *      - Stephan Wenger        <stewe@cs.tu-berlin.de>
 *
 **************************************************************************************
 */

#include <stdlib.h>
#include <assert.h>
#include <string.h>
 
#include "global.h"

#include "contributors.h"
#include "mbuffer.h"
#include "parset.h"
#include "vlc.h"

// Local helpers
static int IdentifyProfile();
static int IdentifyLevel();
static int GenerateVUISequenceParameters();

extern ColocatedParams *Co_located;

seq_parameter_set_rbsp_t SeqParSet[MAXSPS];
pic_parameter_set_rbsp_t PicParSet[MAXPPS];

static const byte ZZ_SCAN[16]  =
{  0,  1,  4,  8,  5,  2,  3,  6,  9, 12, 13, 10,  7, 11, 14, 15
};

static const byte ZZ_SCAN8[64] =
{  0,  1,  8, 16,  9,  2,  3, 10, 17, 24, 32, 25, 18, 11,  4,  5,
   12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13,  6,  7, 14, 21, 28,
   35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
   58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63
};


/*! 
 *************************************************************************************
 * \brief
 *    generates a sequence and picture parameter set and stores these in global
 *    active_sps and active_pps
 *
 * \return
 *    A NALU containing the Sequence ParameterSet
 *
 *************************************************************************************
*/
void GenerateParameterSets ()
{
  seq_parameter_set_rbsp_t *sps = NULL; 
  pic_parameter_set_rbsp_t *pps = NULL;

  sps = AllocSPS();
  pps = AllocPPS();

  GenerateSequenceParameterSet(sps, 0);

  if (input->GenerateMultiplePPS)
  {
    if (sps->profile_idc >= FREXT_HP)
    {
      GeneratePictureParameterSet( pps, sps, 0, 0, 0, input->cb_qp_index_offset, input->cr_qp_index_offset);
      memcpy (&PicParSet[0], pps, sizeof (pic_parameter_set_rbsp_t));
      GeneratePictureParameterSet( pps, sps, 1, 1, 1, input->cb_qp_index_offset, input->cr_qp_index_offset);
      memcpy (&PicParSet[1], pps, sizeof (pic_parameter_set_rbsp_t));
      GeneratePictureParameterSet( pps, sps, 2, 1, 2, input->cb_qp_index_offset, input->cr_qp_index_offset);
      memcpy (&PicParSet[2], pps, sizeof (pic_parameter_set_rbsp_t));

    }
    else
    {
      GeneratePictureParameterSet( pps, sps, 0, 0, 0, input->chroma_qp_index_offset, 0);
      memcpy (&PicParSet[0], pps, sizeof (pic_parameter_set_rbsp_t));
      GeneratePictureParameterSet( pps, sps, 1, 1, 1, input->chroma_qp_index_offset, 0);
      memcpy (&PicParSet[1], pps, sizeof (pic_parameter_set_rbsp_t));
      GeneratePictureParameterSet( pps, sps, 2, 1, 2, input->chroma_qp_index_offset, 0);
      memcpy (&PicParSet[2], pps, sizeof (pic_parameter_set_rbsp_t));
    }
  }
  else
  {
    if (sps->profile_idc >= FREXT_HP)
      GeneratePictureParameterSet( pps, sps, 0, input->WeightedPrediction, input->WeightedBiprediction, 
                                   input->cb_qp_index_offset, input->cr_qp_index_offset);
    else
      GeneratePictureParameterSet( pps, sps, 0, input->WeightedPrediction, input->WeightedBiprediction,
                                   input->chroma_qp_index_offset, 0);
    
    memcpy (&PicParSet[0], pps, sizeof (pic_parameter_set_rbsp_t));

  }

  active_sps = sps;
  active_pps = &PicParSet[0];
}

/*! 
*************************************************************************************
* \brief
*    frees global parameter sets active_sps and active_pps
*
* \return
*    A NALU containing the Sequence ParameterSet
*
*************************************************************************************
*/
void FreeParameterSets ()
{

  FreeSPS (active_sps);
  //FreePPS (active_pps);
}

/*! 
*************************************************************************************
* \brief
*    int GenerateSeq_parameter_set_NALU ();
*
* \note
*    Uses the global variables through GenerateSequenceParameterSet()
*    and GeneratePictureParameterSet
*
* \return
*    A NALU containing the Sequence ParameterSet
*
*************************************************************************************
*/

NALU_t *GenerateSeq_parameter_set_NALU ()
{
  NALU_t *n = AllocNALU(64000);
  int RBSPlen = 0;
  int NALUlen;
  byte rbsp[MAXRBSPSIZE];

  RBSPlen = GenerateSeq_parameter_set_rbsp (active_sps, rbsp);
  NALUlen = RBSPtoNALU (rbsp, n, RBSPlen, NALU_TYPE_SPS, NALU_PRIORITY_HIGHEST, 0, 1);
  n->startcodeprefix_len = 4;

  return n;
}


/*! 
*************************************************************************************
* \brief
*    NALU_t *GeneratePic_parameter_set_NALU (int PPS_id);
*
* \note
*    Uses the global variables through GenerateSequenceParameterSet()
*    and GeneratePictureParameterSet
*
* \return
*    A NALU containing the Picture Parameter Set
*
*************************************************************************************
*/

NALU_t *GeneratePic_parameter_set_NALU(int PPS_id)
{
  NALU_t *n = AllocNALU(64000);
  int RBSPlen = 0;
  int NALUlen;
  byte rbsp[MAXRBSPSIZE];

  RBSPlen = GeneratePic_parameter_set_rbsp (&PicParSet[PPS_id], rbsp);
  NALUlen = RBSPtoNALU (rbsp, n, RBSPlen, NALU_TYPE_PPS, NALU_PRIORITY_HIGHEST, 0, 1);
  n->startcodeprefix_len = 4;

  return n;
}


/*!
 ************************************************************************
 * \brief
 *    GenerateSequenceParameterSet: extracts info from global variables and
 *    generates sequence parameter set structure
 *
 * \param sps
 *    Sequence parameter set to be filled
 *
 * \par
 *    Function reads all kinds of values from several global variables,
 *    including input-> and image-> and fills in the sps.  Many
 *    values are current hard-coded to defaults.
 *
 ************************************************************************
 */

void GenerateSequenceParameterSet(seq_parameter_set_rbsp_t *sps, int SPS_id)
{
  unsigned i;
  int SubWidthC  [4]= { 1, 2, 2, 1};
  int SubHeightC [4]= { 1, 2, 1, 1};

  int frext_profile = ((IdentifyProfile()==FREXT_HP) || 
                      (IdentifyProfile()==FREXT_Hi10P) ||
                      (IdentifyProfile()==FREXT_Hi422) ||
                      (IdentifyProfile()==FREXT_Hi444));

  // *************************************************************************
  // Sequence Parameter Set
  // *************************************************************************
  assert (sps != NULL);
  // Profile and Level should be calculated using the info from the config
  // file.  Calculation is hidden in IndetifyProfile() and IdentifyLevel()
  sps->profile_idc = IdentifyProfile();
  sps->level_idc = IdentifyLevel();

  // needs to be set according to profile
  sps->constrained_set0_flag = 0;
  sps->constrained_set1_flag = 0;
  sps->constrained_set2_flag = 0;
  sps->constrained_set3_flag = 0;

  // Parameter Set ID hard coded to zero
  sps->seq_parameter_set_id = 0;

  // Fidelity Range Extensions stuff
  sps->bit_depth_luma_minus8   = input->BitDepthLuma - 8;
  sps->bit_depth_chroma_minus8 = input->BitDepthChroma - 8;
  img->lossless_qpprime_flag = input->lossless_qpprime_y_zero_flag & (sps->profile_idc==FREXT_Hi444);
  img->residue_transform_flag = input->residue_transform_flag;
  
  //! POC stuff:
  //! The following values are hard-coded in init_poc().  Apparently,
  //! the poc implementation covers only a subset of the poc functionality.
  //! Here, the same subset is implemented.  Changes in the POC stuff have
  //! also to be reflected here
  sps->log2_max_frame_num_minus4 = log2_max_frame_num_minus4;
  sps->log2_max_pic_order_cnt_lsb_minus4 = log2_max_pic_order_cnt_lsb_minus4;
  
  sps->pic_order_cnt_type = input->pic_order_cnt_type;
  sps->num_ref_frames_in_pic_order_cnt_cycle = img->num_ref_frames_in_pic_order_cnt_cycle;
  sps->delta_pic_order_always_zero_flag = img->delta_pic_order_always_zero_flag;
  sps->offset_for_non_ref_pic = img->offset_for_non_ref_pic;
  sps->offset_for_top_to_bottom_field = img->offset_for_top_to_bottom_field;

  for (i=0; i<img->num_ref_frames_in_pic_order_cnt_cycle; i++)
  {
    sps->offset_for_ref_frame[i] = img->offset_for_ref_frame[i];
  }
  // End of POC stuff

  // Number of Reference Frames
  sps->num_ref_frames = input->num_ref_frames;

  //required_frame_num_update_behaviour_flag hardcoded to zero
  sps->gaps_in_frame_num_value_allowed_flag = FALSE;    // double check

  sps->frame_mbs_only_flag = !(input->PicInterlace || input->MbInterlace);

  // Picture size, finally a simple one :-)
  sps->pic_width_in_mbs_minus1 = ((input->img_width+img->auto_crop_right)/16) -1;
  sps->pic_height_in_map_units_minus1 = (((input->img_height+img->auto_crop_bottom)/16)/ (2 - sps->frame_mbs_only_flag)) - 1;

  // a couple of flags, simple
  sps->mb_adaptive_frame_field_flag = (FRAME_CODING != input->MbInterlace);
  sps->direct_8x8_inference_flag = input->directInferenceFlag;
  
  // Sequence VUI not implemented, signalled as not present
  sps->vui_parameters_present_flag = (input->rgb_input_flag && input->yuv_format==3);

  sps->chroma_format_idc = input->yuv_format;

  // This should be moved somewhere else.
  {
    int PicWidthInMbs, PicHeightInMapUnits, FrameHeightInMbs;
    int width, height;
    PicWidthInMbs = (sps->pic_width_in_mbs_minus1 +1);
    PicHeightInMapUnits = (sps->pic_height_in_map_units_minus1 +1);
    FrameHeightInMbs = ( 2 - sps->frame_mbs_only_flag ) * PicHeightInMapUnits;
    
    width = PicWidthInMbs * MB_BLOCK_SIZE;
    height = FrameHeightInMbs * MB_BLOCK_SIZE;
    
    Co_located = alloc_colocated (width, height,sps->mb_adaptive_frame_field_flag);
    
  }

  // Fidelity Range Extensions stuff
  if(frext_profile)
  {

    sps->seq_scaling_matrix_present_flag = (input->ScalingMatrixPresentFlag&1);
    for(i=0; i<8; i++)
    {
      if(i<6)
        sps->seq_scaling_list_present_flag[i] = (input->ScalingListPresentFlag[i]&1);
      else
      {
        if(input->AllowTransform8x8)
          sps->seq_scaling_list_present_flag[i] = (input->ScalingListPresentFlag[i]&1);
        else
          sps->seq_scaling_list_present_flag[i] = 0;
      }
    }
  }
  else
  {
    sps->seq_scaling_matrix_present_flag = 0;
    for(i=0; i<8; i++)
      sps->seq_scaling_list_present_flag[i] = 0;

  }


  if (img->auto_crop_right || img->auto_crop_bottom)
  {
    sps->frame_cropping_flag = TRUE;
    sps->frame_cropping_rect_left_offset=0;
    sps->frame_cropping_rect_top_offset=0;
    sps->frame_cropping_rect_right_offset=  (img->auto_crop_right / SubWidthC[sps->chroma_format_idc]);
    sps->frame_cropping_rect_bottom_offset= (img->auto_crop_bottom / (SubHeightC[sps->chroma_format_idc] * (2 - sps->frame_mbs_only_flag)));
    if (img->auto_crop_right % SubWidthC[sps->chroma_format_idc])
    {
      error("automatic frame cropping (width) not possible",500);
    }
    if (img->auto_crop_bottom % (SubHeightC[sps->chroma_format_idc] * (2 - sps->frame_mbs_only_flag)))
    {
      error("automatic frame cropping (height) not possible",500);
    }
  }
  else
  {
    sps->frame_cropping_flag = FALSE;
  }
};

/*!
 ************************************************************************
 * \brief
 *    GeneratePictureParameterSet: 
 *    Generates a Picture Parameter Set structure
 *
 * \param pps
 *    Picture parameter set to be filled
 *
 * \par
 *    Regarding the QP
 *    The previous software versions coded the absolute QP only in the 
 *    slice header.  This is kept, and the offset in the PPS is coded 
 *    even if we could save bits by intelligently using this field.
 *
 ************************************************************************
 */

void GeneratePictureParameterSet( pic_parameter_set_rbsp_t *pps, seq_parameter_set_rbsp_t *sps, int PPS_id, 
                                 int WeightedPrediction, int WeightedBiprediction, 
                                 int cb_qp_index_offset, int cr_qp_index_offset)
{
  unsigned i;

  int frext_profile = ((IdentifyProfile()==FREXT_HP) || 
                      (IdentifyProfile()==FREXT_Hi10P) ||
                      (IdentifyProfile()==FREXT_Hi422) ||
                      (IdentifyProfile()==FREXT_Hi444));

  // *************************************************************************
  // Picture Parameter Set 
  // *************************************************************************

  pps->seq_parameter_set_id = sps->seq_parameter_set_id;
  pps->pic_parameter_set_id = PPS_id;
  pps->entropy_coding_mode_flag = (input->symbol_mode==UVLC?0:1);

  // Fidelity Range Extensions stuff
  if(frext_profile)
  {
    pps->transform_8x8_mode_flag = input->AllowTransform8x8 ? 1:0;
    pps->pic_scaling_matrix_present_flag = (input->ScalingMatrixPresentFlag&2)>>1;
    for(i=0; i<8; i++)
    {
      if(i<6)
        pps->pic_scaling_list_present_flag[i] = (input->ScalingListPresentFlag[i]&2)>>1;
      else
      {
        if(pps->transform_8x8_mode_flag)
          pps->pic_scaling_list_present_flag[i] = (input->ScalingListPresentFlag[i]&2)>>1;
        else
          pps->pic_scaling_list_present_flag[i] = 0;
      }
    }
  }
  else
  {
    pps->pic_scaling_matrix_present_flag = 0;
    for(i=0; i<8; i++)
      pps->pic_scaling_list_present_flag[i] = 0;

    pps->transform_8x8_mode_flag = input->AllowTransform8x8 = 0;
  }

  // JVT-Fxxx (by Stephan Wenger, make this flag unconditional
  pps->pic_order_present_flag = img->pic_order_present_flag;


  // Begin FMO stuff
  pps->num_slice_groups_minus1 = input->num_slice_groups_minus1;

	
  //! Following set the parameter for different slice group types
  if (pps->num_slice_groups_minus1 > 0)
    switch (input->slice_group_map_type)
    {
    case 0:
			
      pps->slice_group_map_type = 0;
      for(i=0; i<=pps->num_slice_groups_minus1; i++)
      {
        pps->run_length_minus1[i]=input->run_length_minus1[i];
      }
			
      break;
    case 1:
      pps->slice_group_map_type = 1;
      break;
    case 2:
      // i loops from 0 to num_slice_groups_minus1-1, because no info for background needed
      pps->slice_group_map_type = 2;
      for(i=0; i<pps->num_slice_groups_minus1; i++)
      {
        pps->top_left[i] = input->top_left[i];
        pps->bottom_right[i] = input->bottom_right[i];      
      }
     break;
    case 3:
    case 4:
    case 5:
      pps->slice_group_map_type = input->slice_group_map_type;
			
      pps->slice_group_change_direction_flag = input->slice_group_change_direction_flag;
      pps->slice_group_change_rate_minus1 = input->slice_group_change_rate_minus1;
      break;
    case 6:
      pps->slice_group_map_type = 6;   
      pps->pic_size_in_map_units_minus1 = 
				(((input->img_height+img->auto_crop_bottom)/MB_BLOCK_SIZE)/(2-sps->frame_mbs_only_flag))
				*((input->img_width+img->auto_crop_right)/MB_BLOCK_SIZE) -1;
			
      for (i=0;i<=pps->pic_size_in_map_units_minus1; i++)
        pps->slice_group_id[i] = input->slice_group_id[i];
			
      break;
    default:
      printf ("Parset.c: slice_group_map_type invalid, default\n");
      assert (0==1);
    }
// End FMO stuff

  pps->num_ref_idx_l0_active_minus1 = sps->frame_mbs_only_flag ? (sps->num_ref_frames-1) : (2 * sps->num_ref_frames - 1) ;   // set defaults
  pps->num_ref_idx_l1_active_minus1 = sps->frame_mbs_only_flag ? (sps->num_ref_frames-1) : (2 * sps->num_ref_frames - 1) ;   // set defaults
  
  pps->weighted_pred_flag = WeightedPrediction;
  pps->weighted_bipred_idc = WeightedBiprediction;

  pps->pic_init_qp_minus26 = 0;         // hard coded to zero, QP lives in the slice header
  pps->pic_init_qs_minus26 = 0;

  pps->chroma_qp_index_offset = cb_qp_index_offset; 
  if (frext_profile)
  {
    pps->cb_qp_index_offset     = cb_qp_index_offset;
    pps->cr_qp_index_offset     = cr_qp_index_offset;
  }
  else
    pps->cb_qp_index_offset = pps->cr_qp_index_offset = pps->chroma_qp_index_offset;

  pps->deblocking_filter_control_present_flag = input->LFSendParameters;
  pps->constrained_intra_pred_flag = input->UseConstrainedIntraPred;
  
  pps->redundant_pic_cnt_present_flag = 0;
};

/*! 
 *************************************************************************************
 * \brief
 *    syntax for scaling list matrix values
 *
 * \param scalingListinput
 *    input scaling list
 * \param scalingList
 *    scaling list to be used
 * \param sizeOfScalingList
 *    size of the scaling list
 * \param UseDefaultScalingMatrix
 *    usage of default Scaling Matrix
 * \param partition
 *    partition info for writing syntax
 *
 * \return
 *    size of the RBSP in bytes
 *
 *************************************************************************************
 */
int Scaling_List(short *scalingListinput, short *scalingList, int sizeOfScalingList, short *UseDefaultScalingMatrix, DataPartition *partition)
{
  int j, scanj;
  int len=0;
  int delta_scale, lastScale, nextScale;

  lastScale = 8;
  nextScale = 8;

  for(j=0; j<sizeOfScalingList; j++)
  {
    scanj = (sizeOfScalingList==16) ? ZZ_SCAN[j]:ZZ_SCAN8[j];

    if(nextScale!=0)
    {
      delta_scale = scalingListinput[scanj]-lastScale; // Calculate delta from the scalingList data from the input file
      if(delta_scale>127)
        delta_scale=delta_scale-256;
      else if(delta_scale<-128)
        delta_scale=delta_scale+256;

      len+=se_v ("   : delta_sl   ",                      delta_scale,                       partition);
      nextScale = scalingListinput[scanj];
      *UseDefaultScalingMatrix|=(scanj==0 && nextScale==0); // Check first matrix value for zero
    }

    scalingList[scanj] = (nextScale==0) ? lastScale:nextScale; // Update the actual scalingList matrix with the correct values
    lastScale = scalingList[scanj];
  }

  return len;
}


/*! 
 *************************************************************************************
 * \brief
 *    int GenerateSeq_parameter_set_rbsp (seq_parameter_set_rbsp_t *sps, char *rbsp);
 *
 * \param sps
 *    sequence parameter structure
 * \param rbsp
 *    buffer to be filled with the rbsp, size should be at least MAXIMUMPARSETRBSPSIZE
 *
 * \return
 *    size of the RBSP in bytes
 *
 * \note
 *    Sequence Parameter VUI function is called, but the function implements
 *    an exit (-1)
 *************************************************************************************
 */
int GenerateSeq_parameter_set_rbsp (seq_parameter_set_rbsp_t *sps, char *rbsp)
{
  DataPartition *partition;
  int len = 0, LenInBytes;
  unsigned i;

  assert (rbsp != NULL);
  // In order to use the entropy coding functions from golomb.c we need 
  // to allocate a partition structure.  It will be freed later in this
  // function
  if ((partition=calloc(1,sizeof(DataPartition)))==NULL) no_mem_exit("SeqParameterSet:partition");
  if ((partition->bitstream=calloc(1, sizeof(Bitstream)))==NULL) no_mem_exit("SeqParameterSet:bitstream");
  // .. and use the rbsp provided (or allocated above) for the data
  partition->bitstream->streamBuffer = rbsp;
  partition->bitstream->bits_to_go = 8;

  len+=u_v  (8, "SPS: profile_idc",                             sps->profile_idc,                               partition);

  len+=u_1  ("SPS: constrained_set0_flag",                      sps->constrained_set0_flag,    partition);
  len+=u_1  ("SPS: constrained_set1_flag",                      sps->constrained_set1_flag,    partition);
  len+=u_1  ("SPS: constrained_set2_flag",                      sps->constrained_set2_flag,    partition);
  len+=u_1  ("SPS: constrained_set3_flag",                      sps->constrained_set3_flag,    partition);
  len+=u_v  (4, "SPS: reserved_zero_4bits",                     0,                             partition);

  len+=u_v  (8, "SPS: level_idc",                               sps->level_idc,                                 partition);

  len+=ue_v ("SPS: seq_parameter_set_id",                    sps->seq_parameter_set_id,                      partition);

  // Fidelity Range Extensions stuff
  if((sps->profile_idc==FREXT_HP) || 
     (sps->profile_idc==FREXT_Hi10P) ||
     (sps->profile_idc==FREXT_Hi422) ||
     (sps->profile_idc==FREXT_Hi444))
  {
    len+=ue_v ("SPS: chroma_format_idc",                        sps->chroma_format_idc,                          partition);
    if(img->yuv_format == 3)
      len+=u_1  ("SPS: residue_transform_flag",                 img->residue_transform_flag,                     partition);
    len+=ue_v ("SPS: bit_depth_luma_minus8",                    sps->bit_depth_luma_minus8,                      partition);
    len+=ue_v ("SPS: bit_depth_chroma_minus8",                  sps->bit_depth_chroma_minus8,                    partition);
    len+=u_1  ("SPS: lossless_qpprime_y_zero_flag",             img->lossless_qpprime_flag,                      partition);
    //other chroma info to be added in the future

    len+=u_1 ("SPS: seq_scaling_matrix_present_flag",           sps->seq_scaling_matrix_present_flag,            partition);

    if(sps->seq_scaling_matrix_present_flag)
    {
      for(i=0; i<8; i++)
      {
        len+=u_1 ("SPS: seq_scaling_list_present_flag",         sps->seq_scaling_list_present_flag[i],           partition);
        if(sps->seq_scaling_list_present_flag[i])
        {
          if(i<6)
            len+=Scaling_List(ScalingList4x4input[i], ScalingList4x4[i], 16, &UseDefaultScalingMatrix4x4Flag[i], partition);
          else
            len+=Scaling_List(ScalingList8x8input[i-6], ScalingList8x8[i-6], 64, &UseDefaultScalingMatrix8x8Flag[i-6], partition);
        }
      }
    }
  }

  len+=ue_v ("SPS: log2_max_frame_num_minus4",               sps->log2_max_frame_num_minus4,                 partition);
  len+=ue_v ("SPS: pic_order_cnt_type",                      sps->pic_order_cnt_type,                        partition);

  if (sps->pic_order_cnt_type == 0)
    len+=ue_v ("SPS: log2_max_pic_order_cnt_lsb_minus4",     sps->log2_max_pic_order_cnt_lsb_minus4,         partition);
  else if (sps->pic_order_cnt_type == 1)
  {
    len+=u_1  ("SPS: delta_pic_order_always_zero_flag",        sps->delta_pic_order_always_zero_flag,          partition);
    len+=se_v ("SPS: offset_for_non_ref_pic",                  sps->offset_for_non_ref_pic,                    partition);
    len+=se_v ("SPS: offset_for_top_to_bottom_field",          sps->offset_for_top_to_bottom_field,            partition);
    len+=ue_v ("SPS: num_ref_frames_in_pic_order_cnt_cycle",   sps->num_ref_frames_in_pic_order_cnt_cycle,     partition);
    for (i=0; i<sps->num_ref_frames_in_pic_order_cnt_cycle; i++)
      len+=se_v ("SPS: offset_for_ref_frame",                  sps->offset_for_ref_frame[i],                      partition);
  }
  len+=ue_v ("SPS: num_ref_frames",                          sps->num_ref_frames,                            partition);
  len+=u_1  ("SPS: gaps_in_frame_num_value_allowed_flag",    sps->gaps_in_frame_num_value_allowed_flag,      partition);
  len+=ue_v ("SPS: pic_width_in_mbs_minus1",                 sps->pic_width_in_mbs_minus1,                   partition);
  len+=ue_v ("SPS: pic_height_in_map_units_minus1",          sps->pic_height_in_map_units_minus1,            partition);
  len+=u_1  ("SPS: frame_mbs_only_flag",                     sps->frame_mbs_only_flag,                       partition);
  if (!sps->frame_mbs_only_flag)
  {
    len+=u_1  ("SPS: mb_adaptive_frame_field_flag",            sps->mb_adaptive_frame_field_flag,              partition);
  }
  len+=u_1  ("SPS: direct_8x8_inference_flag",               sps->direct_8x8_inference_flag,                 partition);

  len+=u_1  ("SPS: frame_cropping_flag",                      sps->frame_cropping_flag,                       partition);
  if (sps->frame_cropping_flag)
  {
    len+=ue_v ("SPS: frame_cropping_rect_left_offset",          sps->frame_cropping_rect_left_offset,           partition);
    len+=ue_v ("SPS: frame_cropping_rect_right_offset",         sps->frame_cropping_rect_right_offset,          partition);
    len+=ue_v ("SPS: frame_cropping_rect_top_offset",           sps->frame_cropping_rect_top_offset,            partition);
    len+=ue_v ("SPS: frame_cropping_rect_bottom_offset",        sps->frame_cropping_rect_bottom_offset,         partition);
  }

  len+=u_1  ("SPS: vui_parameters_present_flag",             sps->vui_parameters_present_flag,               partition);
  if (sps->vui_parameters_present_flag)
    len+=GenerateVUISequenceParameters(partition);    // currently a dummy, asserting

  SODBtoRBSP(partition->bitstream);     // copies the last couple of bits into the byte buffer
  
  LenInBytes=partition->bitstream->byte_pos;

  free (partition->bitstream);
  free (partition);
  
  return LenInBytes;
}


/*! 
 ***********************************************************************************************
 * \brief
 *    int GeneratePic_parameter_set_rbsp (pic_parameter_set_rbsp_t *sps, char *rbsp);
 *
 * \param pps
 *    picture parameter structure
 * \param rbsp
 *    buffer to be filled with the rbsp, size should be at least MAXIMUMPARSETRBSPSIZE
 *
 * \return
 *    size of the RBSP in bytes, negative in case of an error
 *
 * \note
 *    Picture Parameter VUI function is called, but the function implements
 *    an exit (-1)
 ************************************************************************************************
 */
 
int GeneratePic_parameter_set_rbsp (pic_parameter_set_rbsp_t *pps, char *rbsp)
{
  DataPartition *partition;
  int len = 0, LenInBytes;
  unsigned i;
  unsigned NumberBitsPerSliceGroupId;
  int profile_idc;

  assert (rbsp != NULL);

  // In order to use the entropy coding functions from golomb.c we need 
  // to allocate a partition structure.  It will be freed later in this
  // function
  if ((partition=calloc(1,sizeof(DataPartition)))==NULL) no_mem_exit("PicParameterSet:partition");
  if ((partition->bitstream=calloc(1, sizeof(Bitstream)))==NULL) no_mem_exit("PicParameterSet:bitstream");
  // .. and use the rbsp provided (or allocated above) for the data
  partition->bitstream->streamBuffer = rbsp;
  partition->bitstream->bits_to_go = 8;
  //sw paff
  pps->pic_order_present_flag = img->pic_order_present_flag;

  len+=ue_v ("PPS: pic_parameter_set_id",                    pps->pic_parameter_set_id,                      partition);
  len+=ue_v ("PPS: seq_parameter_set_id",                    pps->seq_parameter_set_id,                      partition);
  len+=u_1  ("PPS: entropy_coding_mode_flag",                pps->entropy_coding_mode_flag,                  partition);
  len+=u_1  ("PPS: pic_order_present_flag",                  pps->pic_order_present_flag,                    partition);
  len+=ue_v ("PPS: num_slice_groups_minus1",                 pps->num_slice_groups_minus1,                   partition);

  // FMO stuff
  if(pps->num_slice_groups_minus1 > 0 )
  {
    len+=ue_v ("PPS: slice_group_map_type",                 pps->slice_group_map_type,                   partition);
    if (pps->slice_group_map_type == 0)
      for (i=0; i<=pps->num_slice_groups_minus1; i++)
        len+=ue_v ("PPS: run_length_minus1[i]",                           pps->run_length_minus1[i],                             partition);
    else if (pps->slice_group_map_type==2)
      for (i=0; i<pps->num_slice_groups_minus1; i++)
      {

        len+=ue_v ("PPS: top_left[i]",                          pps->top_left[i],                           partition);
        len+=ue_v ("PPS: bottom_right[i]",                      pps->bottom_right[i],                       partition);
      }
    else if (pps->slice_group_map_type == 3 ||
             pps->slice_group_map_type == 4 ||
             pps->slice_group_map_type == 5) 
    {
      len+=u_1  ("PPS: slice_group_change_direction_flag",         pps->slice_group_change_direction_flag,         partition);
      len+=ue_v ("PPS: slice_group_change_rate_minus1",            pps->slice_group_change_rate_minus1,            partition);
    } 
    else if (pps->slice_group_map_type == 6)
    {
      if (pps->num_slice_groups_minus1>=4)
        NumberBitsPerSliceGroupId=3;
      else if (pps->num_slice_groups_minus1>=2)
        NumberBitsPerSliceGroupId=2;
      else if (pps->num_slice_groups_minus1>=1)
        NumberBitsPerSliceGroupId=1;
      else
        NumberBitsPerSliceGroupId=0;
        
      len+=ue_v ("PPS: pic_size_in_map_units_minus1",          pps->pic_size_in_map_units_minus1,             partition);
      for(i=0; i<=pps->pic_size_in_map_units_minus1; i++)
        len+= u_v  (NumberBitsPerSliceGroupId, "PPS: >slice_group_id[i]",                            pps->slice_group_id[i],                         partition);
    }
  }
  // End of FMO stuff

  len+=ue_v ("PPS: num_ref_idx_l0_active_minus1",             pps->num_ref_idx_l0_active_minus1,              partition);
  len+=ue_v ("PPS: num_ref_idx_l1_active_minus1",             pps->num_ref_idx_l1_active_minus1,              partition);
  len+=u_1  ("PPS: weighted_pred_flag",                       pps->weighted_pred_flag,                        partition);
  len+=u_v  (2, "PPS: weighted_bipred_idc",                   pps->weighted_bipred_idc,                       partition);
  len+=se_v ("PPS: pic_init_qp_minus26",                      pps->pic_init_qp_minus26,                       partition);
  len+=se_v ("PPS: pic_init_qs_minus26",                      pps->pic_init_qs_minus26,                       partition);

  profile_idc = IdentifyProfile();
  if((profile_idc==FREXT_HP) || 
     (profile_idc==FREXT_Hi10P) ||
     (profile_idc==FREXT_Hi422) ||
     (profile_idc==FREXT_Hi444))
    len+=se_v ("PPS: chroma_qp_index_offset",                 pps->cb_qp_index_offset,                        partition);
  else
    len+=se_v ("PPS: chroma_qp_index_offset",                 pps->chroma_qp_index_offset,                    partition);

  len+=u_1  ("PPS: deblocking_filter_control_present_flag",   pps->deblocking_filter_control_present_flag,    partition);
  len+=u_1  ("PPS: constrained_intra_pred_flag",              pps->constrained_intra_pred_flag,               partition);
  len+=u_1  ("PPS: redundant_pic_cnt_present_flag",           pps->redundant_pic_cnt_present_flag,            partition);

  // Fidelity Range Extensions stuff
  if((profile_idc==FREXT_HP) || 
     (profile_idc==FREXT_Hi10P) ||
     (profile_idc==FREXT_Hi422) ||
     (profile_idc==FREXT_Hi444))
  {
    len+=u_1  ("PPS: transform_8x8_mode_flag",                pps->transform_8x8_mode_flag,                   partition);
    
    len+=u_1  ("PPS: pic_scaling_matrix_present_flag",        pps->pic_scaling_matrix_present_flag,           partition);

    if(pps->pic_scaling_matrix_present_flag)
    {
      for(i=0; i<(6+((unsigned)pps->transform_8x8_mode_flag<<1)); i++)
      {
        len+=u_1  ("PPS: pic_scaling_list_present_flag",      pps->pic_scaling_list_present_flag[i],          partition);

        if(pps->pic_scaling_list_present_flag[i])
        {
          if(i<6)
            len+=Scaling_List(ScalingList4x4input[i], ScalingList4x4[i], 16, &UseDefaultScalingMatrix4x4Flag[i], partition);
          else
            len+=Scaling_List(ScalingList8x8input[i-6], ScalingList8x8[i-6], 64, &UseDefaultScalingMatrix8x8Flag[i-6], partition);
        }
      }
    }
    len+=se_v ("PPS: second_chroma_qp_index_offset",          pps->cr_qp_index_offset,                        partition);
  }

  SODBtoRBSP(partition->bitstream);     // copies the last couple of bits into the byte buffer
  
  LenInBytes=partition->bitstream->byte_pos;

  // Get rid of the helper structures
  free (partition->bitstream);
  free (partition);

  return LenInBytes;
}



/*! 
 *************************************************************************************
 * \brief
 *    Returns the Profile
 *
 * \return
 *    Profile according to Annex A
 *
 * \note
 *    Function is currently a dummy.  Should "calculate" the profile from those
 *    config file parameters.  E.g.
 *
 *    Profile = Baseline;
 *    if (CABAC Used || Interlace used) Profile=Main;
 *    if (!Cabac Used) && (Bframes | SPframes) Profile = Streaming;
 *
 *************************************************************************************
 */
int IdentifyProfile()
{
  return input->ProfileIDC;
};

/*! 
 *************************************************************************************
 * \brief
 *    Returns the Level
 *
 * \return
 *    Level according to Annex A
 *
 * \note
 *    This function is currently a dummy, but should calculate the level out of 
 *    the config file parameters (primarily the picture size)
 *************************************************************************************
 */
int IdentifyLevel()
{
  return input->LevelIDC;
};


/*! 
 *************************************************************************************
 * \brief
 *    Function body for VUI Parameter generation (to be done)
 *
 * \return
 *    exits with error message
 *************************************************************************************
 */
static int GenerateVUISequenceParameters(DataPartition *partition)
{
  int len=0;

  // special case to signal the RGB format
  if(input->rgb_input_flag && input->yuv_format==3)
  { 
    //still pretty much a dummy VUI
    printf ("test: writing Sequence Parameter VUI to signal RGB format\n");
    len+=u_1 ("VUI: aspect_ratio_info_present_flag", 0, partition);
    len+=u_1 ("VUI: overscan_info_present_flag", 0, partition);
    len+=u_1 ("VUI: video_signal_type_present_flag", 1, partition);
    len+=u_v (3, "VUI: video format", 2, partition);
    len+=u_1 ("VUI: video_full_range_flag", 1, partition);
    len+=u_1 ("VUI: color_description_present_flag", 1, partition);
    len+=u_v (8, "VUI: colour primaries", 2, partition);
    len+=u_v (8, "VUI: transfer characteristics", 2, partition);
    len+=u_v (8, "VUI: matrix coefficients", 0, partition);
    len+=u_1 ("VUI: chroma_loc_info_present_flag", 0, partition);
    len+=u_1 ("VUI: timing_info_present_flag", 0, partition);
    len+=u_1 ("VUI: nal_hrd_parameters_present_flag", 0, partition);
    len+=u_1 ("VUI: vcl_hrd_parameters_present_flag", 0, partition);
    len+=u_1 ("VUI: pic_struc_present_flag", 0, partition);
    len+=u_1 ("VUI: bitstream_restriction_flag", 0, partition);

    return len;
  }
  else 
  {
    printf ("Sequence Parameter VUI not yet implemented, this should never happen, exit\n");
    exit (-1);
  }
}

