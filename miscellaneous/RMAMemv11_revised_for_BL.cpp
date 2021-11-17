//========================================================================
// RMA Memory project Main Script Code
// This is a main script code for Reward-modulated Motor Adaptation task back with visual shift.
// version: 1
//
// Author: Taisei Sugiyama
// Date created: 12/28/2017
// Last Update: 1/16/2018
//
//
// ================ Edit Log =============================================
// 
// 2018/1/10
// Modification on reading data from tgt file (additional column is added).
// It is used in analysis but not in experiment
// 2018/1/11
// Reward zone is reduced from 1.5 to 1.0. The param is added to output log file (para.dat)
//
// 2018/1/11 (2)
// Now the score criteria is different. read Training type (go or nogo) from a sequence file and decide evaluation according to that. 1 - go, 2 - nogo 
//
// 2018/1/15
// Now tsize is used in such a way that the arc is drawn instead of the target when tsize = 0
// scoring is modified so that the max shift size (15) is used regardless of the actual shift size
//
// 2018/1/16
// Target is now drawn usign GL_POINTS (the size might have slightly changed, though it should be negligible degree)
// Start dot is red in Probes to warn different target direction from 90. Also, point size is increased from 2 to 3 (again negligible difference)
//
// 2018/01/29
// Adjust the interval of scoring (from every 1.0 deg to 0.5 deg)
//
// 2018/02/01
// Adjust the interval of scoring (from every 0.5 to 0.7 degree (10% of adaptation))
//
// 2018/10/17
// Do some modifications for version 13 (large perturbation)
//========================================================================

// compile command
// g++ RMAMemv11_revised_for_BL.cpp libportaudio.a -rdynamic /usr/local/lib/libglfw3.a -I/usr/local/lib/ -lOGLFT -lGLU -lGLEW -lglut -lX11 -lpthread -lX11 -lXmu -lXrandr -lXinerama -lXi -lXxf86vm -lrt -lm -lXcursor -lGL -ldl -lasound -lsndfile -I/usr/include/QtCore/ -I/usr/include/qt5/QtGui/ -o TaskMemv11_revised


#include <iostream>
//#include <string>
#include <GL/glew.h>
#include <GL/glut.h>
#include <GLFW/glfw3.h>
#include <sys/time.h>
#include <time.h>
#include <stdlib.h>
//#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <dirent.h>

#include <alsa/asoundlib.h> // sound-related libary 1
#include <portaudio.h> // sound-related libary 2
#include <sndfile.h> // sound-related libary 3

#include <termios.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include <oglft/OGLFT.h>

// #include "GLMetaseq.h"	// モデルローダ

#include "/home/izawa/Robot/Develop2/control/transdata.h"

#define SHMSZ 4096

#define BUF_SIZE 4096

/* PCMデフォルト設定 */
#define DEF_CHANNEL         2
#define DEF_FS              48000
#define DEF_BITPERSAMPLE    16
#define WAVE_FORMAT_PCM     1
#define BUF_SAMPLES         1024


static int state;
static int goodprobe = 0; // used to check if subject did okay in a probe
static int yinput; // tracking keyboard input for yes
static int ninput; // tracking keyboard input for no


// define key experiment parameters
static  float cx = 0; // the center (visual) coordinate. This corresponds to the center of the center circle.
static  float cy = -80; // the center (visual) coordinate. This corresponds to the center of the center circle.
float xint = 0; // x interval between the circles in mm 
float Trad=0.10; // Radius of the outer circle (and also distance between target and the screen center)
float MaxMT=0.200; // max movement time
float MinMT=0.300; // minimum movement time 
float MaxPV= 0.700; // max peak velocity
float MinPV= 0.400; // max peak velocity
float xoffset = 5; // offset from robot coord to visual coord 
float yoffset = -81; // offset from robot coord to visual coord 
double RwdZone = 1.4; // a unit (interval) size of score calculation
double L2Maxpay = 1.000; // "border" learning rate to maximize payoff in go training (e.g., If this is 0.5, then subject needs to learn at least a half of error to get max payoff)

// These vectors are from calibration data. 
//float Cal1[] = {0.9389, 0.0247, 0.0555, -0.0390, -0.2176, -0.0172}; // data from 12/28/16
//float Cal2[] = {-0.0356, 1.0314, -0.2907, -0.1311, -0.0141, -0.0388}; // data from 12/28/16
//float Cal1[] = {0.91696, 0.011768, 0.019187, -0.00072728, -0.19514, -0.0099475}; // data from 2/10/17
//float Cal2[] = {-0.019129, 1.031235, -0.12701, -0.232543, -0.070955, -0.026539}; // data from 2/10/17

float Cal1[] = {0.92653, -0.0008, 0.0801, -0.0133, -0.1116, -0.0259}; // data from 4/25/17
float Cal2[] = {-0.0684, 1.0362, 0.0712, -0.1992, -0.0671, -0.0397}; // data from 4/25/17

// As there is no way to get a projected size of display, measure it physically and input it here.
int widthMM = 990;
int heightMM = 560; 


// define minor experiment parameters
int changeh=1; // control the rate of change in the height of coin presentation
int randpic=1; // randomly present picture (slot symbol) at the beginning
GLfloat linewidth = 2; // the line width of the outer circles



// Initialize MQO 3D object variables
// MQO_MODEL CoinModel, CStackModel, TenCStackModel, GrapeModel, CherryModel, BananaModel, OrangeModel;

// These functions are from Kougaku lab sample code
// プロトタイプ宣言
void mySetLight(void);
void mySetLightStack(void);

// 光源の設定を行う関数
void mySetLight(void)
{
  GLfloat light_diffuse[]  = { 0.9, 0.9, 0.9, 1.0 };	// 拡散反射光
  GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };	// 鏡面反射光
  GLfloat light_ambient[]  = { 0.3, 0.3, 0.3, 0.1 };	// 環境光
  //GLfloat light_ambient[]  = { 1.0, 1.0, 1.0, 1.0 };	// 環境光
  GLfloat light_position[] = { 0.0, 0.0, -1000, 1.0 };	// 位置と種類
  GLfloat light_spotdir[] = {0.0, 0.0, -1.0};             // direction of light
  
  // 光源の設定
  glLightfv( GL_LIGHT0, GL_DIFFUSE,  light_diffuse );	 // 拡散反射光の設定
  glLightfv( GL_LIGHT0, GL_SPECULAR, light_specular ); // 鏡面反射光の設定
  glLightfv( GL_LIGHT0, GL_AMBIENT,  light_ambient );	 // 環境光の設定
  glLightfv( GL_LIGHT0, GL_POSITION, light_position ); // 位置と種類の設定
  glLightfv( GL_LIGHT0, GL_SPOT_DIRECTION, light_spotdir);
  
  
  glShadeModel( GL_SMOOTH );	// シェーディングの種類の設定
  glEnable( GL_LIGHT0 );		// 光源の有効化
}

void mySetLightStack(void)
{
  GLfloat light_diffuse[]  = { 0.9, 0.9, 0.9, 1.0 };	// 拡散反射光
  GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };	// 鏡面反射光
  GLfloat light_ambient[]  = { 0.3, 0.3, 0.3, 0.1 };	// 環境光
  //GLfloat light_ambient[]  = { 1.0, 1.0, 1.0, 1.0 };	// 環境光
  GLfloat light_position[] = { -100.0, 0.0, -100, 1.0 };	// 位置と種類
  GLfloat light_spotdir[] = {0.0, 0.0, -1.0};             // direction of light
  
  // 光源の設定
  glLightfv( GL_LIGHT0, GL_DIFFUSE,  light_diffuse );	 // 拡散反射光の設定
  glLightfv( GL_LIGHT0, GL_SPECULAR, light_specular ); // 鏡面反射光の設定
  glLightfv( GL_LIGHT0, GL_AMBIENT,  light_ambient );	 // 環境光の設定
  glLightfv( GL_LIGHT0, GL_POSITION, light_position ); // 位置と種類の設定
  glLightfv( GL_LIGHT0, GL_SPOT_DIRECTION, light_spotdir);
  
  
  glShadeModel( GL_SMOOTH );	// シェーディングの種類の設定
  glEnable( GL_LIGHT1 );		// 光源の有効化
}

void resize(GLFWwindow *const rwindow, int rwidth, int rheight){
  glViewport(0,0, rwidth, rheight);
  
  
}

// callback functions
static void error_callback(int error, const char* description)
{
  fputs(description, stderr);
}



struct command *com_mem;
struct data *dat_mem;

// Handle keyborad inputs.
// For this task, ESC to close the task, y/n for yes/no response after movement in a probe trial 
static void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
  if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
    glfwSetWindowShouldClose(window, GL_TRUE);
  
  if (key == GLFW_KEY_Y && action == GLFW_PRESS && state == 8){ // && state == 5
    yinput = 1;
  }
  
  if (key == GLFW_KEY_N && action == GLFW_PRESS && state == 8){
    ninput = 1;
    
  }
  
}


// Get time stamp
double gettimeofday_sec() {
  struct timespec t;
  clock_gettime(CLOCK_REALTIME , &t);
  return t.tv_sec + (double)t.tv_nsec*1e-9;
}


/******* sound-related functions and variables ***********/

struct OurData
{
  SNDFILE *sndFile;
  SF_INFO sfInfo;
  int position;
};


int Callback(const void *input,
	     void *output,
	     unsigned long frameCount,
	     const PaStreamCallbackTimeInfo* paTimeInfo,
	     PaStreamCallbackFlags statusFlags,
	     void *userData)
{
  OurData *data = (OurData *)userData; /* we passed a data structure
  into the callback so we have something to work with */
  int *cursor; /* current pointer into the output  */
  int *out = (int *)output;
  int thisSize = frameCount;
  int thisRead;
  
  cursor = out; /* set the output cursor to the beginning */
  while (thisSize > 0)
  {
    /* seek to our current file position */
    sf_seek(data->sndFile, data->position, SEEK_SET);
    
    /* are we going to read past the end of the file?*/
    if (thisSize > (data->sfInfo.frames - data->position))
    {
      /*if we are, only read to the end of the file*/
      thisRead = data->sfInfo.frames - data->position;
      /* and then loop to the beginning of the file */
      data->position = 0;
    }
    else
    {
      /* otherwise, we'll just fill up the rest of the output buffer */
      thisRead = thisSize;
      /* and increment the file position */
      data->position += thisRead;
    }
    
    /* since our output format and channel interleaving is the same as
     *sf_readf_int's requirements */
    /* we'll just read straight into the output buffer */
    sf_readf_int(data->sndFile, cursor, thisRead);
    /* increment the output cursor*/
    cursor += thisRead;
    /* decrement the number of samples left to process */
    thisSize -= thisRead;
  }
  
  return paContinue;
}


static void error_openfile(int sdataindex)
{
  printf("Error: cannot open sound file %i\n",sdataindex);
}

static void error_openstream(int sdataindex, int errorcode)
{
  printf("Error: cannot open sound stream %i\n error code = %i\n", sdataindex, errorcode);
  const char* errmsg = Pa_GetErrorText(errorcode);
  printf(errmsg); printf("\n");
  Pa_Terminate();
}


/*********** functions for displaying texts ******************/

OGLFT::Filled* filled = new OGLFT::Filled("/usr/share/fonts/truetype/OpenSans-Bold.ttf", 12); //("/usr/share/fonts/truetype/OpenSans-Regular.ttf", 36);

static void displaytxt2( double textx, double texty, const char* text, double c1, double c2, double c3)
{
  // First clear the window ...
  //glClear( GL_COLOR_BUFFER_BIT );
  // ... then draw the string
  filled->setForegroundColor( c1, c2, c3 );
  filled->draw( textx, texty, text);
}


// for backup. delete this later
static void displaytxt ( double textx, double texty, const char* text)
{
  // First clear the window ...
  //glClear( GL_COLOR_BUFFER_BIT );
  // ... then draw the string
  filled->setForegroundColor( 1., 1., 1. );
  filled->draw( textx, texty, text);
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////// Main Function //////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main(void)
{
  
  
  // initialize variables
  GLFWwindow* window;
  char no[10];
  
  
  int shmid;
  void *shared_mem;
  key_t keyt;
  
  int shmid2;
  void *shared_mem2;
  key_t keyt2;
  
  
  double t; 
  double t1=0;
  double t2=0;
  double DT = 0; // Decision time 
  double t_st=0;
  int rewarded = 0; // whether or not rewarded.
  int missed = 0; // whether or not missed. 
  int playsnd=0;
  int playprbsnd=0;
  int coinrot= 0;
  double coinh=0;
  double coinsh=0;
  double feedx, feedy, feedangle, handangle, feedangledeg; // feedback (score just earned) location
  double diststart, distcenter, distrigth, distleft; // distance between the cursor and start position.
  double peakv = 0;
  double peakyvel = 0; // peak y-velocity
  double thisv = 0;
  double compens_yen = 0; // compensation in yen.
  double yenxpos, yenypos; // position of yen symbol.
  distcenter = 0.0;
  
  
  int count;
  int stackh = 0;
  float stackx = 0;
  int stackleftx;
  char filename[20];
  char tarfilename[30];
  char Dtarfilename[25];
  char tgtfilename[30]; 
  
  
  FILE *fp;
  FILE *fp1;
  
  
  
  
  float Btmp;
  float ScScale=1.0;
  
  
  GLFWmonitor **monitors;
  
  int dcount;
  float ratio;
  int width, height;
  float xpos, ypos; // current handle position
  float tmpx, tmpy, rotxpos, rotypos; // temporary xy values when you do cursor transformation. 
  float xcurrent, ycurrent; // track current cursor position when the robot brings the handle to the center
  float xvel, yvel; // xy components of velocity 
  float hand_sp; // velocity
  float fx, fy; // related to force?
  float Txpos, Typos; // target coordinate
  int gain = 0; // how much point is gained/lost in a current trial
  char thispayoff [15]; // the payoff of current trial (score)
  char payforhit[15]; // potential payoff for hit  
  char payformiss[15]; // potential payoff for miss
  char cumpayoff [15]; // cumulative payoff (yen)
  char peakyvstr[15]; // peak velocity for displaying
  char peakyvelfeed[15];
  int exRate = 20; // score divided by this number will be a payoff in yen
  float scorexpos = cx-3; // x position of cumulative score (which changes according to the number of digits) 
  float Sxpos=0;
  float Sypos=0;
  float tmpSxpos, tmpSypos;
  float VSxpos, VSypos; // start position in visual coordinate (numbers are not calculated based on conversion matrix).
  float Chanxpos, Chanypos;
  float Cursize; // cursor size
  float xpass, ypass, calxpass, calypass, Vxpass, Vypass; // where the handle/cursor crossed the circle.
  float epx, epy; // endponint xy.
  float Verror; // visual error (distance)
  float Aerror; // visual error (angular)
  
  double c1, c2, c3; // handle text color
  double MVtime; // movement time
  double RCtime; // reaction time  
  int ptime; // presentation time for random symbols 
  int selectleft = 0; 
  int selectright = 0; // track subject decision on right/left circle.
  int countstart = 0; // start counting a time when waiting at the start position.
  float calxpos,calypos, calTxpos,calTypos, calSxpos,calSypos;
  
  float VTx, VTy;
  GLfloat col1[] = {1.0f, 1.0f, 1.0f};
  
  int trial=0;
  int PosCursor=1;
  int i;
  int score=0;
  int showstack = 1;
  int prevc = 0; // choice in a previous trial
  int nstack, nsingle;
  
  float Gsize=5;//mm
  
  double base_time=0;
  double currenttime=0;
  
  sprintf(cumpayoff,"%d",score);
  
  
  
  
  
  /////////////////////// read sound data ////////////////////////////////////////// 
  
  OurData *sdata1 = (OurData *)malloc(sizeof(OurData));
  OurData *sdata2 = (OurData *)malloc(sizeof(OurData));
  OurData *sdata3 = (OurData *)malloc(sizeof(OurData));
  
  PaStreamParameters out_param1, out_param2, out_param3;
  PaStream * stream1; PaStream * stream2; PaStream * stream3;
  PaError err; // track error for Portaudio functions
  
  /* initialize our data structure */
  sdata1->position = 0; sdata1->sfInfo.format = 0;
  sdata2->position = 0; sdata2->sfInfo.format = 0;
  sdata3->position = 0; sdata3->sfInfo.format = 0;
  
  // Open sound files
  sdata1->sndFile = sf_open("button01a_800ms.wav", SFM_READ, &sdata1->sfInfo); // neutral tone 
  sdata2->sndFile = sf_open("coin05_800ms.wav", SFM_READ, &sdata2->sfInfo); // coin (reward) sound
  sdata3->sndFile = sf_open("incorrect2_800ms.wav", SFM_READ, &sdata3->sfInfo);  // low beep (punishment) sound
  
  // check if successfully opened the sound files
  if (!sdata1->sndFile)  {error_openfile(1); return 1;}
  else if (!sdata2->sndFile){error_openfile(2); return 1;}
  else if (!sdata3->sndFile){error_openfile(3); return 1;}
  
  /* init portaudio */
  err = Pa_Initialize();
  if (err) // failed to initialize
  {
    printf("error initialization, error code = %i\n", err);
    Pa_Terminate();
    return 1;
  }
  
  
  /* we are using the default device */
  out_param1.device = Pa_GetDefaultOutputDevice(); // use the default device
  
  if (out_param1.device == paNoDevice) // couldn't find any audio device
  {
    fprintf(stderr, "Haven't found an audio device!\n");
    return -1;
  }
  
  /* stero or mono */
  out_param1.channelCount = sdata1->sfInfo.channels; // use the same number of channels as our sound file 
  out_param1.sampleFormat = paInt32; // 32bit int format
  out_param1.suggestedLatency = Pa_GetDeviceInfo(out_param1.device)->defaultLowOutputLatency; // set acceptable latency 
  out_param1.hostApiSpecificStreamInfo = 0; // no api specific data
  
  
  /* we are using the default device */
  out_param2.device = Pa_GetDefaultOutputDevice(); // use the default device
  out_param2.channelCount = sdata2->sfInfo.channels; // use the same number of channels as our sound file 
  out_param2.sampleFormat = paInt32; // 32bit int format
  out_param2.suggestedLatency = Pa_GetDeviceInfo(out_param2.device)->defaultLowOutputLatency; // set acceptable latency 
  out_param2.hostApiSpecificStreamInfo = 0; // no api specific data
  
  
  /* we are using the default device */
  out_param3.device = Pa_GetDefaultOutputDevice(); // use the default device
  out_param3.channelCount = sdata3->sfInfo.channels; // use the same number of channels as our sound file 
  out_param3.sampleFormat = paInt32; // 32bit int format
  out_param3.suggestedLatency = Pa_GetDeviceInfo(out_param3.device)->defaultLowOutputLatency; // set acceptable latency 
  out_param3.hostApiSpecificStreamInfo = 0; // no api specific data
  
  
  // Check if you can open a stream data without error
  err = Pa_OpenStream(&stream1, 0, &out_param1, sdata1->sfInfo.samplerate, paFramesPerBufferUnspecified, paNoFlag,Callback, sdata1);
  /* if we can't open it, then bail out */
  if (err){ error_openstream(1,err); return 1;}
  
  err = Pa_OpenStream(&stream2, 0, &out_param2, sdata2->sfInfo.samplerate, paFramesPerBufferUnspecified, paNoFlag, Callback, sdata2);  
  /* if we can't open it, then bail out */
  if (err){ error_openstream(2,err); return 1;}
  
  err = Pa_OpenStream(&stream3, 0, &out_param3, sdata3->sfInfo.samplerate, paFramesPerBufferUnspecified, paNoFlag, Callback, sdata3); 
  /* if we can't open it, then bail out */
  if (err){ error_openstream(3,err); return 1;}
  
  // successfully opened them, so close it for now.
  Pa_CloseStream(stream1);
  Pa_CloseStream(stream2);
  Pa_CloseStream(stream3);
  
  /////////////////////// Finished reading sound data ////////////////////////////////////////
  
  
  
  
  
  ////////////// Precompute the xy for an approximate circles /////////////////////////////////////////////////////
  int num_segments = 100; // Number of vertices for the outer circle 
  //float ccircx[num_segments], ccircy[num_segments]; // center (probe) circle coordinate
  float ccircx[num_segments], ccircy[num_segments], rcircx[num_segments]; float rcircy[num_segments], lcircx[num_segments], lcircy[num_segments]; // store circle coordinates
  float theta = 2 * 3.1415926 /num_segments; 
  float c = cosf(theta);//precalculate the sine and cosine
  float s = sinf(theta);
  float temp, newx, newy;
  
  
  float x = Trad*1000; // we start at angle = 0 
  float y = 0; // we start at angle = 0 
  int ii;
  
  x = Trad*1000; // temp
  
  // Calculate center circle coordinate
  for(ii = 0; ii < num_segments; ii++) 
  { 
    
    // calculate based on robot coordinate
    //newx = x + cx/1000; 
    //newy = y + cy/1000;//output vertex 
    //ccircx[ii] = (Cal1[0]*newx+Cal1[1]*newy+Cal1[2]*newx*newx+Cal1[3]*newy*newy+Cal1[4]*newx*newy+Cal1[5])*1000; // convert robot coord to visual coord
    //ccircy[ii] = (Cal2[0]*newx+Cal2[1]*newy+Cal2[2]*newx*newx+Cal2[3]*newy*newy+Cal2[4]*newx*newy+Cal2[5])*1000; 
    
    newx = x + cx; 
    newy = y + cy;//output vertex 
    
    ccircx[ii] = newx;
    ccircy[ii] = newy;
    
    //apply the shift matrix
    temp = x;
    x = c * x - s * y;
    y = s * temp + c * y;
  } 
  
  
  // Calculate right circle coordinate 
  float rcx=  cx+xint;// the center x-coord  
  float rcy = cy;// the center y-coord
  
  x = Trad*1000; // we start at angle = 0 
  y = 0; // we start at angle = 0 
  
  for(ii = 0; ii < num_segments; ii++) 
  { 
    newx = x + rcx; 
    newy = y + rcy;//output vertex 
    
    rcircx[ii] = newx;
    rcircy[ii] = newy;
    
    //apply the shift matrix
    temp = x;
    x = c * x - s * y;
    y = s * temp + c * y;
  } 
  
  // Calculate left circle coordinate 
  float lcx = cx-xint;// center of x. Adjust the number to control the degree of shift
  float lcy = cy;// center of y
  
  x = Trad*1000; // we start at angle = 0 
  y = 0; // we start at angle = 0 
  
  for(ii = 0; ii < num_segments; ii++) 
  { 
    newx = x + lcx; 
    newy = y + lcy;//output vertex 
    
    lcircx[ii] = newx;
    lcircy[ii] = newy;
    
    //apply the shift matrix
    temp = x;
    x = c * x - s * y;
    y = s * temp + c * y;
  } 
  
  
  // initialize some global varialbes
  printf("id=%d",1);
  
  // mem=(struct transfer_data *)malloc(sizeof(struct transfer_data));
  com_mem=(struct command *)malloc(sizeof(struct command));
  dat_mem=(struct data *)malloc(10000*sizeof(struct data));
  
  com_mem->K11=0.0;
  com_mem->K12=0;
  com_mem->K21=0;
  com_mem->K22=0;
  com_mem->B11=0;
  com_mem->B12=0;
  com_mem->B21=0;
  com_mem->B22=0;
  com_mem->VTx=0;
  com_mem->VTy=0;
  com_mem->state=0;
  com_mem->start_x=0;//meter
  com_mem->start_y=0;
  com_mem->target_x=0;
  com_mem->target_y=0;
  // for direct mode
  com_mem->Dforce_x=0;
  com_mem->Dforce_y=0;
  com_mem->apply_field=0;
  
  
  // initialization
  if((keyt=ftok("../task/key3.dat",'R'))==-1){
    printf("ftok");
    exit(1);
  }
  
  printf("keyt=%d",keyt);
  
  //shmid=shmget(keyt, 4096, 0777|IPC_CREAT);
  if((shmid=shmget(keyt, sizeof(struct command),IPC_CREAT |0666))<0){
    printf("shmget");
  } else{
    printf("id=%lx",shmid);
  }
  
  shared_mem=shmat(shmid,(void*)0,0);
  //shared_mem=shmat(keyt,0,0);
  
  
  com_mem=(struct command *) shared_mem; 
  
  
  printf("keyt=%d",keyt);
  
  
  
  
  
  // initialization
  if((keyt2=ftok("../task/key2.dat",'R'))==-1){
    printf("ftok");
    exit(1);
  }
  
  printf("keyt2=%d",keyt2);
  
  //shmid=shmget(keyt, 4096, 0777|IPC_CREAT);
  if((shmid2=shmget(keyt2, 10000*sizeof(struct data),IPC_CREAT |0666))<0){
    printf("shmget");
  } else{
    printf("id2=%lx",shmid2);
  }
  
  shared_mem2=shmat(shmid2,(void*)0,0);
  //shared_mem=shmat(keyt,0,0);
  
  
  dat_mem=(struct data *) shared_mem2; 
  
  
  printf("key2t=%d",keyt);
  
  
  glfwSetErrorCallback(error_callback);
  
  if (!glfwInit())
    exit(EXIT_FAILURE);
  
  
  monitors = glfwGetMonitors(&count);
  const GLFWvidmode *modes = glfwGetVideoModes(monitors[0], &count);
  // GLFWvidmode *mode=modes;//modes+10;
  window = glfwCreateWindow(1600,900, "RMA experiment",NULL, NULL);
  
  printf("count %d\n",count);
  
  // glfwGetMonitorPhysicalSize(monitors[0], &widthMM, &heightMM);
  
  
  if (!window)
  {
    glfwTerminate();
    exit(EXIT_FAILURE);
  }
  
  glfwMakeContextCurrent(window);
  glfwSwapInterval(1);
  
  glfwSetKeyCallback(window, key_callback);
  glfwSetWindowSizeCallback(window, resize);
  
  resize(window, width, height);
  
  state=0;
  
  
  
  glfwGetFramebufferSize(window, &width, &height); 
  
  ratio = width / (float) height; // not used. maybe remove this.
  
  
  
  glClear(GL_COLOR_BUFFER_BIT); 
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity(); 
  glOrtho(-0.5*widthMM, 0.5*widthMM, -0.5*heightMM, 0.5*heightMM, 1000, -1000); 
  
  double	znear = 1;
  double	fovy = 50;
  double	zfar = 10000;
  
  //gluPerspective(fovy, (double)width/height, znear, zfar);
  //glOrtho(0, 0, -1.f, 1.f, 1.f, -1.f); */
  
  glMatrixMode(GL_MODELVIEW); 
  
  glLoadIdentity(); 
  
  t=gettimeofday_sec();
  
  glClearColor(0.0f,0.0f,0.0f,1.0);
  
  glfwSwapBuffers(window);
  
  
  int Block=1;
  
  char Sbj[20];
  
  //************************************ Keyboard Inputs *****************************************
  printf("Enter Subject's Initial (e.g. JI) \n");
  scanf("%s",Sbj);
  printf("Enter Block number (e.g. 1) \n");
  scanf("%d",&Block);	
  //fprintf(stderr,"filename=%s\n",tarfilename);
  printf("Enter Start Trial Number (Note: Enter 0 to start from the beginning) \n");
  scanf("%d",&trial);
  //printf("Enter Target Filename(e.g. TarMrrPRE.tgt) \n");
  //scanf("%s",tarfilename);
  //fprintf(stderr,"filename=%s\n",tarfilename);
  // strcpy(tgtfilename,tarfilename); // copy the sequence file name for saving
  // sprintf(Dtarfilename,"./Target/%s",tarfilename);
  //sprintf(tgtfilename,"%s",tarfilename);
  
  // char Sbj[20] = {'P','0','1'};
  
    DIR *TarDir;
  struct dirent *dent;
  TarDir = opendir("./Target/");
  int filecount = 0;
  printf("Enter Target Filename(e.g. TarMrrPRE.tgt) \n");
  printf("possible names: \n");
  while ((dent = readdir(TarDir)) != NULL){
    if ((dent->d_type) == DT_REG){
      printf("'%s'  \n", dent->d_name);
      filecount++;
      if (filecount>5){
	filecount = 0;
	printf("\n");
      }
    }
  }
  if (filecount>0) {printf("\n");}
  closedir(TarDir);
  scanf("%s",tarfilename);
  
  strcpy(tgtfilename,tarfilename); // copy the sequence file name for saving
  sprintf(Dtarfilename,"./Target/%s",tarfilename);
  
  
  //tarfilename[20] = "1_RMAfamv3.tgt";
  //fprintf(stderr,"filename=%s\n",tarfilename);
  //sprintf(Dtarfilename,"./Target/%s",tarfilename);
  //fp1=fopen("./Target/testtrial1.tgt", "r");
  //fprintf(stderr, "ファイルのオープンに失敗しました．\n");
  if ((fp1 = fopen(Dtarfilename, "r")) == NULL) {
    //fprintf(stderr,"filename=%s\n",Dtarfilename);
    fprintf(stderr, "ファイルのオープンに失敗しました．\n");
    
    return EXIT_FAILURE;
  }
  
  
  
  int tcount=0;
  int Maxtrial=0;
  int tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11,tmp12,tmp13;
  
  // These variables takes values from a sequence file
  int  tdir[1000],field[1000],hit[1000], maxpayoff[1000], probe[1000], fchoice[1000], tsize[1000], minpayoff[1000],bet[1000],ShowCur[1000],basepenalty[1000],bphase[1000], trtype[1000];
  float iti[1000], bvalue[1000], shift[1000];
  
  double tmpiti = 0;
  
  int Trset = 1; 
  char Trnum[10];
  
  
  printf("tdir, iti, bval, field, shift, hit, maxpay, basepenalty, ShowCur, MF, tsize, phase, Train Type, minpay\n");
  
  // Read sequence file info.
  while( fscanf(fp1, "%d %d %d %d %d %d %d %d %d %d %d %d %d\n",&tmp1,&tmp2,&tmp3,&tmp4,&tmp5,&tmp6,&tmp7,&tmp8,&tmp9,&tmp10,&tmp11,&tmp12,&tmp13)!=EOF){
    tdir[tcount]= tmp1; // target shift
    iti[tcount]=(float)tmp2/1000;// waiting time (msec to sec) 
    bvalue[tcount]=(float)tmp3; // force strength
    field[tcount]=tmp4; // type of trial 
    shift[tcount]=tmp5/1000.0; // m degree to degree
    maxpayoff[tcount]=tmp6; // max payoff
    basepenalty[tcount]=tmp7; // 1 penalty size unit 
    ShowCur[tcount] = tmp8; // whether or not to show a cursor
    probe[tcount]=tmp9; // probe (with reward/punishment)
    tsize[tcount]=tmp10; // target size
    bphase[tcount]=tmp11; // block phase 
    trtype[tcount]=tmp12; // training type (1-go, 2-nogo)
    minpayoff[tcount]=tmp13; // min payoff
    printf("%d, %3.3f, %3.3f, %d, %3.3f, %d, %d, %d, %d, %d, %d, %d, %d \n",
	   tdir[tcount], //1
	   iti[tcount], //2 
	   bvalue[tcount], //3
	   field[tcount], //4 
	   shift[tcount], //5
	   maxpayoff[tcount], //6
	   basepenalty[tcount], //7
	   ShowCur[tcount], //8
	   probe[tcount], //9
	   tsize[tcount], //10
	   bphase[tcount], //11
	   trtype[tcount], //12
	   minpayoff[tcount]); //13
    tcount=tcount+1;
    
  }
  
  
  
  
  
  
  
  
  // Output a parameter file
  sprintf(filename,"./Data/%s/B%dPara.dat",Sbj,Block);
  /* ファイルオープン */
  if ((fp = fopen(filename, "w")) == NULL) {
    fprintf(stderr,"filename=%s\n",filename);
    fprintf(stderr, "ファイルのオープンに失敗しました．\n");
    
    return EXIT_FAILURE;
  }
  
  
  /* 書き込み */
  
  fprintf(fp, "cx, cy, xint, Trad, MaxMT, MinMT, xoffset, yoffset, Sxpos, Sypos, Tgt Filename, RwdZone, L2Maxpay\n"
  //      1      2     3     4     5     6     7     8     9     10    11   
  "%3.3f,%3.3f,%3.3f,%3.3f,%3.3f,%3.3f,%3.3f,%3.3f,%3.3f,%3.3f,%s,%3.3f,%3.3f\n",
  cx,    //1
  cy,    //2
  xint,   //3
  Trad,   //4
  MaxMT,   //5
  MinMT,   //6
  xoffset,   //7
  yoffset, //8
  Sxpos,   //9
  Sypos,   //10
  tgtfilename,// tarfilename //11
  RwdZone, // 12
  L2Maxpay //13
  );	      
  
  
  /* ファイルクローズ */
  fclose(fp);
  //printf("end\n");
  
  Maxtrial=tcount-1;   
  
  glfwSwapInterval(0); 
  
  yenxpos = cx-5;
  yenypos = 93;
  glfwSwapBuffers(window);
  glfwPollEvents();
  base_time=gettimeofday_sec();
  currenttime=gettimeofday_sec();
  
  // ******************** Main Experiment Loop *********************************************************************
  while (!glfwWindowShouldClose(window))
  {
    
    // Get the cursor position
    xpos=com_mem->Hx;// mm
    ypos=com_mem->Hy;
    
    // Convert the robot cursor coord to visual coord
    calxpos = xpos*1000 + xoffset;
    calypos = ypos*1000 + yoffset; 
    
    // Get velocity (not used in this task)
    xvel=com_mem->Vx;
    yvel=com_mem->Vy;
    
    // Get Force (not used in this task)
    fx=com_mem->Fx;//projectcoeff*xt
    fy=com_mem->Fy;
    
    hand_sp=sqrt(xvel*xvel+yvel*yvel); // magnitude of hand velocity (speed) in 2D
    
    
    // calculate visual start coordinate
    calSxpos = Sxpos*1000 + xoffset;
    calSypos = Sypos*1000 + yoffset;
    
    // for debugging
    // printf("state: %d\n",state);
    
    t=gettimeofday_sec(); // get current time stamp in each loop
    
    com_mem->state=state;
    
    
    // Do different things in different state (stage) of a trial
    switch(state){
      
      case 0:// first initialization. Run only once at the beginning of task
	
	com_mem->K11=0.0;
	com_mem->K12=0;
	com_mem->K21=0;
	com_mem->K22=0;
	com_mem->B11=0;
	com_mem->B12=0;
	com_mem->B21=0;
	com_mem->B22=0;
	com_mem->VTx=0;
	com_mem->VTy=0;
	com_mem->RecON=0;
	com_mem->start_x=0;//meter
	com_mem->start_y=0;
	com_mem->target_x=0;
	com_mem->target_y=0;
	// for direct mode
	com_mem->Dforce_x=0;
	com_mem->Dforce_y=0;
	com_mem->apply_field=0;
	// com_mem->B11=6;//(float)Btmp;
	
	score=0;
	
	
	state=1;// go to the next state
	t1=gettimeofday_sec();
	
	break;
	
	
	
      case 1: // second initialization. Run only once in each trial.
	
	xcurrent=com_mem->Hx;
	ycurrent=com_mem->Hy;
	
	// Compute the target position in xy-coordinate
	Txpos=Trad*cos((float)tdir[trial]/360*2*M_PI)+Sxpos;
	Typos=Trad*sin((float)tdir[trial]/360*2*M_PI)+Sypos;t1=gettimeofday_sec(); // get time stamp
	t2=gettimeofday_sec(); // get time stamp
	linewidth = 2; // set line width
	state=2; // go to the next 
	t_st=0.0;
	peakv = 0;
	
	if (bphase[trial]==4 & ShowCur[trial]==1){
	Trset = Trset + 1;
	sprintf(Trnum,"Set %d",Trset);
	}
	yinput = 0; ninput = 0; // initalize keyboard inputs for probe trial.
	countstart = 0; // initialize count timing
	selectleft = 0; selectright = 0;
	rewarded = 0; missed = 0;
	peakyvel = 0;
	
	VSxpos = cx; VSypos = cy; // Set visual coordinate.
	Sxpos = (VSxpos-xoffset)/1000; Sypos = (VSypos-yoffset)/1000; // Get robot coordinate
	
	// calculate the target position in visual coord
	calTxpos = Txpos*1000 + xoffset;
	calTypos = Typos*1000 + yoffset;
	
	
	// initialize tracking variables for sound feedback
	playsnd =1; // reward/punishment sound feedback
	playprbsnd=1; // probe sound feedback
	// 	sprintf(payforhit, "%d", maxpayoff[trial]);
	// 	sprintf(payformiss, "%d", misspayoff[trial]);
	
	// initialize recording variable
	for(dcount=0;dcount<3000;dcount++)
	{
	  
	  dat_mem[dcount].Dx=0.0;
	  dat_mem[dcount].Dy=0.0;
	  dat_mem[dcount].Ddx=0.0;
	  dat_mem[dcount].Ddy=0.0;
	  dat_mem[dcount].Fsx=0.0;
	  dat_mem[dcount].Fsy=0.0;
	  dat_mem[dcount].state=0;
	  
	}
	
	// Reset the variable in nth trial
	/*
	 *	if (trial % 3 == 0){
	 *	  prevc = 0;
    }
    */
	
	break;
	
	case 2: // bring the hand to the center (regardless of probe/non-probe)
	  
	  
	  Txpos=Trad*cos((float)tdir[trial]/360*2*M_PI)+Sxpos;
	  Typos=Trad*sin((float)tdir[trial]/360*2*M_PI)+Sypos;
	  
	  // calculate the target position in visual coord
	  calTxpos = Txpos*1000 + xoffset;
	  calTypos = Typos*1000 + yoffset;
	  
	  // temporary set start position to the center
	  tmpSxpos = (cx-xoffset)/1000;
	  tmpSypos = (cy-yoffset)/1000;
	  
	  
	  
	  // bring the hand to the center in 1 second
	  if(t-t1<0.7){ 
	    com_mem->VTx= xcurrent+(tmpSxpos-xcurrent)/0.7*(t-t1);
	    com_mem->VTy= ycurrent+(tmpSypos-ycurrent)/0.7*(t-t1);
	  }
	  
	  com_mem->K11=-400;
	  com_mem->K12=0;
	  com_mem->K21=0;
	  com_mem->K22=-400;
	  com_mem->B11=-20;
	  com_mem->B12=0;
	  com_mem->B21=0;
	  com_mem->B22=-20;
	  //com_mem->VTx=VTx;
	  //com_mem->VTy=VTy;
	  com_mem->RecON=0;
	  com_mem->start_x=0;//meter
	  com_mem->start_y=0;
	  com_mem->target_x=0;
	  com_mem->target_y=0;
	  // for direct mode
	  com_mem->Dforce_x=0;
	  com_mem->Dforce_y=0;
	  com_mem->apply_field=1;
	  
	  
	  
	  
	  // get the distance to the center
	  distcenter = sqrt((cx-calxpos)*(cx-calxpos)+(cy-calypos)*(cy-calypos)); 
	  
	  // Too far away from the center yet. not start timing yet.
	  if(distcenter>=3){countstart = 0;}
	  
	  // Get to the center close enough. Start timing
	  if(distcenter < 3 && countstart == 0){
	    countstart = 1;
	    t2=gettimeofday_sec();
	    
	  }
	  
	  
	  if(distcenter<3 && t-t2>0.200) // waiting at the center for 200ms before moving to the next state
	  {
	    
	    t1=gettimeofday_sec(); // get a time stamp before moving onto the next.
	    // printf("trial=%d\n,Maxtrial=%d\n",trial,Maxtrial);
	    state=3; // go to the next state
	    
	  }
	  // }
	  
	  break;
	  
	  
	  case 3: // Turn off the force before moving to the next phase 
	    
	    // Turn off the force 
	    com_mem->K11=0;
	    com_mem->K12=0;
	    com_mem->K21=0;
	    com_mem->K22=0;
	    com_mem->B11=0;
	    com_mem->B12=0;
	    com_mem->B21=0;
	    com_mem->B22=0;
	    //com_mem->VTx=VTx;
	    //com_mem->VTy=VTy;
	    com_mem->RecON=0;
	    com_mem->start_x=0;//meter
	    com_mem->start_y=0;
	    com_mem->target_x=0;
	    com_mem->target_y=0;
	    // for direct mode
	    com_mem->Dforce_x=0;
	    com_mem->Dforce_y=0;
	    com_mem->apply_field=1;
	    
	    
	    DT = 0.0;
	    
	    
	    state = 4;
	    
	    break; 
	    
	  case 4: // Skip this phase for this version
	    
	    state = 5;
	    
	    break; 
	    
	  case 5: // Waiting at the start point
	    
	    
	    // Not move the handle, but hold it at the start point
	    com_mem->K11=-400;
	    com_mem->K12=0;
	    com_mem->K21=0;
	    com_mem->K22=-400;
	    com_mem->B11=-20;
	    com_mem->B12=0;
	    com_mem->B21=0;
	    com_mem->B22=-20;
	    //com_mem->VTx=VTx;
	    //com_mem->VTy=VTy;
	    com_mem->RecON=0;
	    com_mem->start_x=0;//meter
	    com_mem->start_y=0;
	    com_mem->target_x=0;
	    com_mem->target_y=0;
	    // for direct mode
	    com_mem->Dforce_x=0;
	    com_mem->Dforce_y=0;
	    com_mem->apply_field=1;
	    
	    // Turn off the force 
	    // 	  com_mem->K11=0;
	    // 	  com_mem->K12=0;
	    // 	  com_mem->K21=0;
	    // 	  com_mem->K22=0;
	    // 	  com_mem->B11=0;
	    // 	  com_mem->B12=0;
	    // 	  com_mem->B21=0;
	    // 	  com_mem->B22=0;
	    // 	  //com_mem->VTx=VTx;
	    // 	  //com_mem->VTy=VTy;
	    // 	  com_mem->RecON=0;
	    // 	  com_mem->start_x=0;//meter
	    // 	  com_mem->start_y=0;
	    // 	  com_mem->target_x=0;
	    // 	  com_mem->target_y=0;
	    // 	  // for direct mode
	    // 	  com_mem->Dforce_x=0;
	    // 	  com_mem->Dforce_y=0;
	    // 	  com_mem->apply_field=1;
	    // 	      
	    
	    
	    // get the distance between the cursor and start point
	    diststart = sqrt((VSxpos-calxpos)*(VSxpos-calxpos)+(VSypos-calypos)*(VSypos-calypos));
	    
	    // close enough to the start. Show the cursor and start timing.
	    if(diststart <3 && countstart == 0){
	      PosCursor=1;
	      countstart = 1;
	      t2=gettimeofday_sec();
	      
	    }
	    // Too far away, bring back the handle
	    else if (diststart > 40 ) { 	  
	      PosCursor = 0;
	      state = 1;
	      
	      
	    }
	    
	    // Not too far away but not close enough either.
	    else { 	  
	      PosCursor = 0;	  
	      
	    }
	    
	    // The first trial. Just show the cursor
	    if (trial == 0){
	      PosCursor = 1;
	    }
	    
	    // Staying within the start. start timing 
	    if(diststart<3) 
	    {
	      PosCursor=1;
	      
	      tmpiti = iti[trial];
	      
	      
	      // Waiting time has passed.
	      if(t-t2>tmpiti) 
	      {
		
		
		Chanxpos=xpos; Chanypos=ypos;
		t1=gettimeofday_sec(); // get a time stamp for reaction time.
		state=6; // go to the next state
		
	      }
	    }
	    else // going out of the start center. Reset and re-count a waiting time
	    {
	      countstart = 0;
	      t2=gettimeofday_sec();	  
	    }
	    
	    break;
	    
	    
	    
	    
	    case 6: // Between go signal and initiation of movement (Reaction stage) 
	      
	      if(ShowCur[trial]==1){
		PosCursor=1;
	      }
	      else{
		PosCursor=0; 
	      }
	      
	      
	      
	      // Now no force from the motor
	      com_mem->K11=0;
	      com_mem->K12=0;
	      com_mem->K21=0;
	      com_mem->K22=0;
	      com_mem->B11=0;
	      com_mem->B12=bvalue[trial];
	      com_mem->B21=-1.0*bvalue[trial];
	      com_mem->B22=0;
	      com_mem->VTx=0;
	      com_mem->VTy=0;
	      com_mem->RecON=1; // start recording
	      
	      com_mem->start_x=Chanxpos;//meter
	      com_mem->start_y=Chanypos;
	      com_mem->target_x=Txpos;
	      com_mem->target_y=Typos;
	      // for direct mode
	      com_mem->Dforce_x=0;
	      com_mem->Dforce_y=0;
	      com_mem->apply_field=field[trial];//1 FF, 2, Dir, 3, Chan
	      
	      // get the distance between the cursor and start point
	      diststart = sqrt((VSxpos-calxpos)*(VSxpos-calxpos)+(VSypos-calypos)*(VSypos-calypos));
	      
	      
	      // Hand speed exceeded a threshold or they traveled half of the radius. Assume that subject has reacted and initiated a movement.  
	      if(hand_sp>0.03 || diststart > Trad*500) 
	      {
		RCtime=t-t1; // reaction time
		t1=gettimeofday_sec(); // get a time stamp for MT
		
		Chanxpos=com_mem->Hx;//xpos;
		Chanypos=com_mem->Hy;//ypos;
		
		
		state=7; // go to next state
	      }
	      
	      break;
	      
	      
	      case 7: // shooting
		
		/* com_mem->VTy=0; */
		/* com_mem->RecON=1; */
		com_mem->K11=0;
		com_mem->K12=0;
		com_mem->K21=0;
		com_mem->K22=0;
		com_mem->B11=0;
		com_mem->B12=bvalue[trial];
		com_mem->B21=-1.0*bvalue[trial];
		com_mem->B22=0;
		com_mem->VTx=0;
		com_mem->VTy=0;
		com_mem->RecON=1;
		
		
		
		com_mem->start_x=Chanxpos;//meter
		com_mem->start_y=Chanypos;
		com_mem->target_x=(float)Txpos;
		com_mem->target_y=(float)Typos;
		// for direct mode
		com_mem->Dforce_x=0;
		com_mem->Dforce_y=0;
		com_mem->apply_field=field[trial];//1 FF, 2, Dir, 3, Chan
		
		
		
		if(ShowCur[trial]==1){
		  PosCursor=1;
		}
		else{
		  PosCursor=0; 
		}
		
		if(peakyvel <= yvel*1000){
		  peakyvel = yvel*1000;
		}
		
		
		//if (trial % 20 == 2 || trial % 20 == 6 || trial % 20 == 14 || trial % 20 == 16){
		//  PosCursor = 1;
		//}
		
		// ||t-t1>2.5
		
		
		if(sqrt((xpos-Sxpos)*(xpos-Sxpos)+(ypos-Sypos)*(ypos-Sypos))>Trad)// Passed the outer circle (or taking too long). Get some values and go to the next phase. 
		{
		  xpass=xpos; ypass=ypos; // get the cross point of hand in robot coord.
		  calxpass=calxpos; calypass=calypos; // get the cross point of hand in visual coord (assuming no shift).
		  Vxpass=tmpx; Vypass=tmpy; // get where the cursor crossed 
		  Verror=sqrt((Vxpass-calTxpos)*(Vxpass-calTxpos)+(Vypass-calTypos)*(Vypass-calTypos)); // error (no direction)
		  MVtime=t-t1; // get MT.
		  t1=gettimeofday_sec(); // get a time stamp before moving to the next state.
		  sprintf(peakyvstr, "%3.0f", peakyvel);
		  strcpy(peakyvelfeed,peakyvstr);
		  strcat(peakyvelfeed,"ms");
		  
		  // cursor angle (originally used to show feedback) 
		  handangle = atan2(calypass-VSypos, calxpass - VSxpos)* 180 / 3.14159265358;;
		  feedangle = atan2(Vypass-VSypos, Vxpass - VSxpos)* 180 / 3.14159265358;;
		  
		  Aerror = tdir[trial] - feedangle;
		  
		  // Decide the gain of this trial based on performance 
		  if(probe[trial]==0){gain = 0;} // practice. no gain or loss
		  
		  else{
		    
		    // Go version
		    if (trtype[trial]==1){
		      
		      // positive shift
		      if (shift[trial]>0){
			if (handangle <= tdir[trial] - shift[trial]*L2Maxpay){// (handangle <= tdir[trial] - shift[trial]*L2Maxpay){
			  gain = maxpayoff[trial];
			}
			else
			  gain = maxpayoff[trial]-ceil(abs(tdir[trial]-shift[trial]*L2Maxpay-handangle)/RwdZone)*basepenalty[trial];
			  // gain = maxpayoff[trial]-floor(abs(tdir[trial]-shift[trial]*L2Maxpay-handangle)/RwdZone)*basepenalty[trial];
		      }
		      
		      // negative shift
		      if (shift[trial]<0){
			if  (handangle >=tdir[trial] - shift[trial]*L2Maxpay){//(handangle >=tdir[trial] - shift[trial]*L2Maxpay){
			  gain = maxpayoff[trial];
			}
			else{
			  gain = maxpayoff[trial]-ceil(abs(tdir[trial]-shift[trial]*L2Maxpay-handangle)/RwdZone)*basepenalty[trial];
			  //gain = maxpayoff[trial]-floor(abs(tdir[trial]-shift[trial]*L2Maxpay-handangle)/RwdZone)*basepenalty[trial];
			}
		  
		      }
		    }
		    
		
		    else if (trtype[trial]==2){
		  
		  
		  gain = maxpayoff[trial] - floor(abs(Aerror)/RwdZone)*basepenalty[trial];
		  
		  
		}
		}
		  
		// check if gain is below the "minimum" number 
		if(gain < minpayoff[trial]){ gain = minpayoff[trial];}
		
		score=score+gain; // update score
		
		// update stringgs for displaying	
		sprintf(thispayoff, "%d", gain); // gain for this trial
		sprintf(cumpayoff, "%d", score); // cumulative score
		
		state=8; // go to the next state
    }
    
    
    break;
    
    
    case 8:// Feedback
      
      /* com_mem->VTy=0; */
      /* com_mem->RecON=1; */
      com_mem->K11=0;
      com_mem->K12=0;
      com_mem->K21=0;
      com_mem->K22=0;
      com_mem->B11=0;
      com_mem->B12=bvalue[trial];
      com_mem->B21=-1.0*bvalue[trial];
      com_mem->B22=0;
      com_mem->VTx=0;
      com_mem->VTy=0;
      com_mem->RecON=1;
      
      com_mem->start_x=Chanxpos;//meter
      com_mem->start_y=Chanypos;
      com_mem->target_x=Txpos;
      com_mem->target_y=Typos;
      // for direct mode
      com_mem->Dforce_x=0;
      com_mem->Dforce_y=0;
      com_mem->apply_field=field[trial];//1 FF, 2, Dir, 3, Chan
      
      
      
      
      feedx = Trad*1100*cosf(feedangle)+VSxpos;
      feedy = Trad*1100*sinf(feedangle)+VSypos;
      
      // calculate endpoint xy
      epx = Trad*1000*cosf(feedangle)+VSxpos; 
      epy = Trad*1000*sinf(feedangle)+VSypos;
      
      
      if(t-t1<0.12 && ShowCur[trial]==1){
	PosCursor=1;
      }
      else {
	PosCursor=0;	       
      }
      
      
      
      // Play sound (and process score) once. This sould be done after a screen is refreshed, so an arbitrary tiny delay (~50~150ms) is inserted just so that 
      // this doesn't run before refreshing the display.
      // Also, because this runs only once per trial, some process (e.g., updating a score) is done inside this conditional statement
      
      
      if(playsnd == 1 && t-t1>0.15)
      {
	
	
	// practice (no reward) or zero score. neutral tone.
	if(probe[trial] == 0 || gain == 0){ 
	  sdata1->position = 0; // play from the beginning
	  err = Pa_OpenStream(&stream1, 0, &out_param1, sdata1->sfInfo.samplerate, paFramesPerBufferUnspecified, paNoFlag, Callback, sdata1);  // open a stream
	  Pa_StartStream(stream1); // start playing
	  Pa_Sleep(300); // play for this duration (ms)
	  Pa_StopStream(stream1); // stop playing
	  Pa_CloseStream(stream1); // close the stream
	  
	  
	}
	
	// positive gain. bell tone
	else if (gain > 0){
	  sdata2->position = 0; // play from the beginning
	  err = Pa_OpenStream(&stream2, 0, &out_param2, sdata2->sfInfo.samplerate, paFramesPerBufferUnspecified, paNoFlag, Callback, sdata2); // open a stream 
	  Pa_StartStream(stream2); // start playing
	  Pa_Sleep(800); // play for this duration (ms)
	  Pa_StopStream(stream2); // stop playing
	  Pa_CloseStream(stream2); // close the stream
	  
	}
	
	// negative gain. beep
	else if (gain < 0){
	  sdata3->position = 0; // play from the beginning
	  err = Pa_OpenStream(&stream3, 0, &out_param3, sdata3->sfInfo.samplerate, paFramesPerBufferUnspecified, paNoFlag, Callback, sdata3);  // open a stream
	  Pa_StartStream(stream3); // start playing
	  Pa_Sleep(800); // play for this duration (ms)
	  Pa_StopStream(stream3); // stop playing
	  Pa_CloseStream(stream3); // close the stream
	}
	
	playsnd = 0; // done playing sound for this trial.
      }
      
      
      // Showed feedback for 1200ms, and then proceed to data saving phase
      
//       if (bphase[trial+1] != 4){ // not in intervention
	if (t-t1>1.2){ state = 9;}
//       }
//       else{
//       if(ShowCur[trial+1] == 0 && t-t1>1.2){ state=9;} // Trdy
//       
//       else if(ShowCur[trial+1] == 1 && t-t1>1.2 && t-t1<=2.4){PosCursor = 0;}
//       
//       else if(ShowCur[trial+1] == 1 && t-t1>2.4){state=9;}
//       }
      
      break;
      
      
      
      case 9: //data save
	com_mem->K11=0.0;
	com_mem->K12=0;
	com_mem->K21=0;
	com_mem->K22=0;
	com_mem->B11=0;
	com_mem->B12=0;
	com_mem->B21=0;
	com_mem->B22=0;
	com_mem->VTx=0;
	com_mem->VTy=0;
	com_mem->RecON=2;
	
	com_mem->start_x=0;//meter
	com_mem->start_y=0;
	com_mem->target_x=Txpos;
	com_mem->target_y=Typos;
	// for direct mode
	com_mem->Dforce_x=0;
	com_mem->Dforce_y=0;
	com_mem->apply_field=1;
	
	
	sprintf(filename,"./Data/%s/B%d_T%d.dat",Sbj,Block,trial);
	/* ファイルオープン */
	if ((fp = fopen(filename, "w")) == NULL) {
	  fprintf(stderr,"filename=%s\n",filename);
	  fprintf(stderr, "ファイルのオープンに失敗しました．\n");
	  
	  return EXIT_FAILURE;
	}
	
	// feedangledeg = feedangle * 180 / 3.14159265358;
	
	/* 書き込み */
	
	for(dcount=0;dcount<3000;dcount++){
	  //           1     2     3     4     5     6     7  8     9     10    11    12    13    14     
	  fprintf(fp, "%3.6f,%3.6f,%3.6f,%3.6f,%3.6f,%3.6f,%d,%3.6f,%3.6f,%3.6f,%3.6f,%3.6f,%3.6f,%d\n",
		  dat_mem[dcount].Dx,    //1
	   dat_mem[dcount].Dy,    //2
	   dat_mem[dcount].Ddx,   //3
	   dat_mem[dcount].Ddy,   //4
	   dat_mem[dcount].Fsx,   //5
	   dat_mem[dcount].Fsy,   //6
	   dat_mem[dcount].state, //7
	   MVtime,   //8
	   RCtime,   //9
	   calxpass, //10
	   calypass, //11
	   xpass,    //12
	   ypass,    //13
	   gain);    //14	      
	  
	  thisv = sqrt(dat_mem[dcount].Ddx*dat_mem[dcount].Ddx+dat_mem[dcount].Ddy*dat_mem[dcount].Ddy); // robot speed 
	  
	  if (thisv > peakv){
	    peakv = thisv;
	  }
	  
	  
	}
	
	
	/* ファイルクローズ */
	fclose(fp);
	
	compens_yen = score/exRate;
	
	
	// Display a bunch of values on the monitoring screen 
	
	//            1         2         3            4          5          6            7      8            9             10           11 
	printf("trial %d: probe=%d, score=%d, PeakVel= %3.3lf, MT=%3.3lf, RT=%3.3lf, Tdir=%d Cur=%3.2lf, hand=%3.2lf, shift=%3.3lf, gain=%d \n",
	       trial, //1
	probe[trial], //2
	score, //3
	peakv, //4
	MVtime, //5
	RCtime,  //6 
	tdir[trial], //7
	feedangle, //8
	handangle, //9
	shift[trial], //10 
	gain); //11
	
	
	
	for(dcount=0;dcount<3000;dcount++)
	{
	  
	  dat_mem[dcount].Dx=0.0;
	  dat_mem[dcount].Dy=0.0;
	  dat_mem[dcount].Ddx=0.0;
	  dat_mem[dcount].Ddy=0.0;
	  /* dat_mem[dcount].As=0.0; */
	  /* dat_mem[dcount].Ae=0.0; */
	  dat_mem[dcount].Fsx=0.0;
	  dat_mem[dcount].Fsy=0.0;
	  /* dat_mem[dcount].Fax=0.0; */
	  /* dat_mem[dcount].Fay=0.0; */
	  /* dat_mem[dcount].handf0=0.0; */
	  /* dat_mem[dcount].handf1=0.0; */
	  /* dat_mem[dcount].handf2=0.0; */
	  /* dat_mem[dcount].handf3=0.0; */
	  /* dat_mem[dcount].handf4=0.0; */
	  /* dat_mem[dcount].handf5=0.0; */
	  dat_mem[dcount].state=0;
	  
	  
	  
	  
	}
	
	
	if(trial>=Maxtrial)
	{  
	  com_mem->K11=0;
	  com_mem->K12=0;
	  com_mem->K21=0;
	  com_mem->K22=0;
	  com_mem->B11=0;
	  com_mem->B12=0;
	  com_mem->B21=0;
	  com_mem->B22=0;
	  com_mem->VTx=0;
	  com_mem->VTy=0;
	  com_mem->RecON=0;
	  
	  com_mem->start_x=0;//meter
	  com_mem->start_y=0;
	  com_mem->target_x=0;
	  com_mem->target_y=0;
	  // for direct mode
	  com_mem->Dforce_x=0;
	  com_mem->Dforce_y=0;
	  com_mem->apply_field=0;
	  
	  
	  
	  
	  
	  exit(0);
	}
	
	
	
	
	
	
	
	trial=trial+1; 
	state=1;
	goodprobe = 0;
	t1=gettimeofday_sec();
	
	
	break; 
	
	default:
	  state=0;
	  com_mem->K11=0.0;
	  com_mem->K12=0;
	  com_mem->K21=0;
	  com_mem->K22=0;
	  com_mem->B11=0;
	  com_mem->B12=0;
	  com_mem->B21=0;
	  com_mem->B22=0;
	  com_mem->VTx=0;
	  com_mem->VTy=0;
	  com_mem->RecON=0;
	  com_mem->start_x=0;//meter
	  com_mem->start_y=0;
	  com_mem->target_x=0;
	  com_mem->target_y=0;
	  // for direct mode
	  com_mem->Dforce_x=0;
	  com_mem->Dforce_y=0;
	  com_mem->apply_field=0;
	  
	  
	  break;
	  
  }
  
  
  
  currenttime = gettimeofday_sec();
  
  //printf("%3.3lf\n",currenttime - base_time);
  //printf("current time: %3.3lf\n",currenttime);
  //printf("base time: %3.3lf\n",base_time);
  
  
  // Here we control the refresh rate. If elapsed time exceeds the time defined here,
  // We draw a new frame and swap it.	
  // 0.005 - 200Hz
  // 0.010 - 100Hz
  // 0.016 -  60Hz	    
  //if(currenttime-base_time>0.010)
  if(currenttime-base_time>0.005)
  {
    
    
    /*********************** Drawing section *********************************/
    
    
    // clear the window
    glClear(GL_COLOR_BUFFER_BIT);
    
    
    
    /////////////// DRAW TEXTS ////////////////////////
    
    
    //// total score ////
    
    if (trtype[trial]!= 0){
      
      if (score > 0){ c1 = 0.3; c2=1.0; c3=0.3;}
      else if (score == 0){ c1 = 0.5; c2=0.5; c3=0.5;}
      else if (score < 0){ c1 = 1.0; c2=0.0; c3=0.0;}
      displaytxt2( cx-35, 100, "Total: ",c1,c2,c3); 
      displaytxt2( cx+18, 100, cumpayoff,c1,c2,c3);
      
      
      //// payoff for this trial ////
      if (state == 8 && probe[trial] == 1 && t-t1< 1.2){
	
	if (gain > 0){ c1 = 0.3; c2=1.0; c3=0.3;}
	else if (gain == 0){ c1 = 0.5; c2=0.5; c3=0.5;}
	else if (gain < 0){ c1 = 1.0; c2=0.0; c3=0.0;}
	
	//c1 = 0.3; c2=1.0; c3=0.3;
	displaytxt2(calTxpos-5, calTypos+15, thispayoff, c1, c2, c3);
	
      }
    }
    
    //// message ////
    
//     if(state == 2 && probe[trial+1] == 1){
//       
//       c1 = 0.7; c2=0.7; c3=0.7;
//       displaytxt2(cx-5.,VSypos+20,"1",c1,c2,c3);
//       
//     }
//     
//     if(state == 2 && probe[trial] == 1){
//       
//       c1 = 0.7; c2=0.7; c3=0.7;
//       displaytxt2(cx-5.,VSypos+20,"2",c1,c2,c3);
//       
//     }
//     
//     if(state == 8 && probe[trial+2] == 1 && t-t1>=1.2){
//       
//       c1 = 0.7; c2=0.7; c3=0.7;
//       displaytxt2(cx-15,VSypos+20,Trnum,c1,c2,c3);
//       
//     }
//     


//     if(state == 2 && bphase[trial] == 4 && ShowCur[trial] == 1 ){
//       
//       c1 = 0.7; c2=0.7; c3=0.7;
//       displaytxt2(cx-5.,VSypos+20,"1",c1,c2,c3);
//       
//     }
//     
//     if(state == 2 && bphase[trial] == 4 && ShowCur[trial] == 0){
//       
//       c1 = 0.7; c2=0.7; c3=0.7;
//       displaytxt2(cx-5.,VSypos+20,"2",c1,c2,c3);
//       
//     }
//     
//     if(state == 8 && bphase[trial+1] == 4 && ShowCur[trial+1] == 1 && t-t1>=1.2){
//       
//       c1 = 0.7; c2=0.7; c3=0.7;
//       displaytxt2(cx-15,VSypos+20,Trnum,c1,c2,c3);
//       
//     }
    
    
    if(state == 8 && MVtime > MinMT && t-t1<1.2){ 
      displaytxt2(cx-35.,-10,"Too Slow",0.7,0.1,0.1);}
      
      if(state == 8 && MVtime < MaxMT && t-t1<1.2){ 
	displaytxt2(cx-35.,-10,"Too Fast",0.7,0.1,0.1);}

//     if(state == 8 && peakv < MinPV && t-t1<1.2){ 
//       displaytxt2(cx-35.,-10,"Too Slow",0.7,0.1,0.1);}
//       
//       if(state == 8 && peakv > MaxPV && t-t1<1.2){ 
// 	displaytxt2(cx-35.,-10,"Too Fast",0.7,0.1,0.1);}
// 	
	
	///////////// DRAW START POINT (CENTER DOT) /////////////////////////////
	
	// dot color
// 	if(bphase[trial]==2 || bphase[trial]==6) {glColor3f(0.7f, 0.0f, 0.0f);}// make it red in probes where target may not be at 90-deg
// 	else{glColor3f(0.7f, 0.7f, 0.7f);}
	
	glColor3f(0.7f, 0.7f, 0.7f);
	
	glPointSize(3); // dot size
	// Draw dot
	if (state<=5){
	  glPointSize(4); // dot size
	  glBegin(GL_POINTS);
	  glVertex2f(cx,cy);
	  glEnd();
	}
	
	/////////////// DRAW TARGET AND HORIZONTAL LINE //////////////////////////////////////
	

	
	if(state >= 6){ 
	  
	  // 	glColor3f(1.0f, 1.0f, 1.0f);
	  // 	Cursize = 6;
	  
	  //// Target ////
	  
// 	  if(bphase[trial] == 4 && tsize[trial] > 0){
// 	    glColor3f(0.0f, 0.7f, 0.7f);
// 	  }
// 	  else{
	    glColor3f(0.7f, 0.7f, 0.7f);
// 	  }
	  Cursize = 6;
	  
	  // erase target when tsize is zero
	  if (tsize[trial] == 0){
	    glBegin(GL_LINES);
	    
	    for(ii = 12; ii < 38; ii++) 
	    { 
	      glVertex2f(ccircx[ii],ccircy[ii]); glVertex2f(ccircx[ii+1],ccircy[ii+1]);	  
	    }
	    glEnd();
	    
	    
	  }
	  
	  else if (state == 8 && t-t1> 1.2){}
	  else{
// 	    glBegin(GL_QUADS);
// 	    glVertex2f(-0.5*Cursize +calTxpos, 0.5*Cursize+calTypos); glVertex2f(-0.5*Cursize+calTxpos, -0.5*Cursize+calTypos); // vertex 1-2
// 	    glVertex2f(0.5*Cursize+calTxpos, -0.5*Cursize+calTypos); glVertex2f(0.5*Cursize+calTxpos, 0.5*Cursize+calTypos); // vertex 3-4
// 	    glEnd();
	    
	  glPointSize(10); // cursor size
	  // Now draw the cursor
	  glBegin(GL_POINTS);
	  
	  glVertex2f(calTxpos,calTypos);
	  
	  glEnd();
	    
	    
	  }
	  
	  glColor3f(1.0f, 1.0f, 1.0f); // reset color.
	}
	
	
	
	
	/////////////// DRAW CURSOR //////////////////////////////////////
	
	
	// Apply shift/rotation during movement (if any)
	if(state>=6){
	  
	  // Since shift is with respect to the visual coord origin, first shift the cursor if shooting from the center of right/left circle.
	  tmpx = calxpos - VSxpos; 
	  tmpy = calypos - VSypos;
	  
	  // Rotate.
	  rotxpos = tmpx*cos(shift[trial]*2*M_PI/360) - tmpy*sin(shift[trial]*2*M_PI/360);
	  rotypos = tmpx*sin(shift[trial]*2*M_PI/360) + tmpy*cos(shift[trial]*2*M_PI/360);
	  
	  
	  // Shift back the cursor
	  tmpx=  rotxpos + VSxpos;
	  tmpy= rotypos + VSypos; 
	}
	// initial stages of a trial, so no shift. 
	else{
	  tmpx = calxpos;
	  tmpy = calypos;
	  
	}
	
	
	// Draw cursor
	if(PosCursor==1){
	  //glColor3f(1.0f, 1.0f, 0.0f); // cursor color
	  glColor3f(0.7f, 0.7f, 0.0f); // cursor color
	  glPointSize(6); // cursor size
	  // Now draw the cursor
	  glBegin(GL_POINTS);
	  
	  if (probe[trial]==0){
	    glVertex2f(tmpx,tmpy);
	  }
	  else{
	    glVertex2f(calxpos,calypos);
	    
	  }
	  glEnd();
	  
	} 	  
	
	
	
	// Draw feedback (endpoint) cursor
	//       if(state>=8 && ShowCur[trial] == 1 && t-t1<=1.2){
	// 	
	// 	// green
	// 	glColor3f(0.0f, 0.7f, 0.0f); // cursor color
	// 
	// 	  
	// 	glPointSize(6); // cursor size
	// 	// Now draw the cursor
	// 	glBegin(GL_POINTS);	
	// 	  glVertex2f(epx,epy); // from the hand	
	// 	glEnd();
	// 	
	//       } 	
	//       
	
	
	/****************************** End of Drawing Section ************************************/
	
	// Show what you have drawn 
	glfwSwapBuffers(window);
	//glfwPollEvents();
	base_time=gettimeofday_sec();
  }
  
} // End of Main Experiment "while" loop 


// Set these robot parameters to zero before quit.
com_mem->K11=0.0;
com_mem->K12=0;
com_mem->K21=0;
com_mem->K22=0;
com_mem->B11=0;
com_mem->B12=0;
com_mem->B21=0;
com_mem->B22=0;
com_mem->VTx=0;
com_mem->VTy=0;

//Quit(); // Delete the mqo objects 
glfwDestroyWindow(window);

glfwTerminate();
exit(EXIT_SUCCESS);
shmdt(shared_mem);

// Close sound files
Pa_CloseStream(stream1);
Pa_CloseStream(stream2);
Pa_CloseStream(stream3);
Pa_Terminate(); // Terminate PortAudio.	

} // All Done!

