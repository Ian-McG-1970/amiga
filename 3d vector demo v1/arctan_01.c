#define WIN32_LEAN_AND_MEAN
#include <windows.h>   	// required for all Windows applications
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define IDM_EXIT           106

#define INITIAL_DIB_WIDTH   512   // initial dimensions of DIB
#define INITIAL_DIB_HEIGHT  400   //  into which we'll draw

struct pBITMAPINFO
{
  BITMAPINFOHEADER bmiHeader;
  RGBQUAD bmiColors[256];
} BMInfo;

struct pLOGPALETTE
{
  WORD palVersion;
  WORD palNumEntries;
  PALETTEENTRY palPalEntry[256];
} PalInfo;

char *pDIB, *pDIBBase; // pointers to DIB section we'll draw into
HBITMAP hDIBSection;   // handle of DIB section
HWND hwndOutput;
int DIBWidth=INITIAL_DIB_WIDTH, DIBHeight=INITIAL_DIB_HEIGHT, DIBPitch;

FILE *file;

char string[256];

typedef struct
{
  long x;
  long z;
} xz;

typedef struct
{
  unsigned long x;
  unsigned long z;
} uxz;

const int scale=1024;

const int width=INITIAL_DIB_WIDTH/2;
const int height=INITIAL_DIB_HEIGHT/2;


xz start={150, 30};
xz end={20,100};

const void debugstring() { file=fopen("log.txt","a"); fprintf(file,string); fclose(file); }

const void debug(const char* str) { sprintf(string,"%s\n",str); debugstring(); }

const xz XZ(const long x, const long z)
{
  xz rc;
  rc.x=x;
  rc.z=z;
  return rc;
}

const uxz UXZ(const unsigned long x, const unsigned long z)
{
  uxz rc;
  rc.x=x;
  rc.z=z;
  return rc;
}

const void Plot(const xz pos, const unsigned char colour)
{
	unsigned char *screen_line=(char *)pDIB + (pos.z*DIBPitch);
	*(screen_line + (pos.x))=colour;
}

const void BigPlot(const xz pos, const unsigned char colour)
{
	Plot(XZ(pos.x+0,pos.z+0),colour);	
	Plot(XZ(pos.x+1,pos.z+0),colour);	
	Plot(XZ(pos.x+2,pos.z+0),colour);	

	Plot(XZ(pos.x+0,pos.z+1),colour);	
	Plot(XZ(pos.x+1,pos.z+1),colour);	
	Plot(XZ(pos.x+2,pos.z+1),colour);	

	Plot(XZ(pos.x+0,pos.z+2),colour);	
	Plot(XZ(pos.x+1,pos.z+2),colour);	
	Plot(XZ(pos.x+2,pos.z+2),colour);	
}

const void Set_DIB()
{
  if (BMInfo.bmiHeader.biHeight > 0)
  {
    pDIB = (pDIBBase + (DIBHeight - 1) * DIBWidth);
    DIBPitch = -DIBWidth;
  }
  else
  {
    pDIB = pDIBBase;
    DIBPitch = DIBWidth;
  }
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM uParam, LPARAM lParam)
{
    switch (message) 
    {
    case WM_COMMAND:  // message: command from application menu
        switch (LOWORD(uParam)) 
        {
        case IDM_EXIT:
            DestroyWindow(hwnd);
            break;
        default:
            return (DefWindowProc(hwnd, message, uParam, lParam));
        }
        break;

    case WM_SIZE:   // window size changed
        if (uParam != SIZE_MINIMIZED) 
        {
          if (hDIBSection != 0) // Skip when this is called before the first DIB section is created
          {
            const HBITMAP holdDIBSection = hDIBSection; // Resize the DIB section to the new size
            BMInfo.bmiHeader.biWidth = (LOWORD(lParam) +3) & ~3;
            BMInfo.bmiHeader.biHeight = HIWORD(lParam);;

            hDIBSection = CreateDIBSection(GetDC(hwnd), (BITMAPINFO*)&BMInfo, DIB_RGB_COLORS, &pDIBBase, NULL, 0);
            if (hDIBSection) 
            {
              DIBWidth = BMInfo.bmiHeader.biWidth;
              DIBHeight = BMInfo.bmiHeader.biHeight;
              DeleteObject(holdDIBSection);
              Set_DIB();
            }
          } 
        }
        break;

    case WM_DESTROY:  // message: window being destroyed
        DeleteObject(hDIBSection);                     
        PostQuitMessage(0);
        break;

    default:
        return (DefWindowProc(hwnd, message, uParam, lParam)); // Passes it on if unproccessed
    }
    return 0;
}

const BOOL InitApp(const HINSTANCE hInstance)
{
  WNDCLASS  wc;
  wc.style         = CS_HREDRAW | CS_VREDRAW; // Fill in window class structure with parameters that describe the main window.
  wc.lpfnWndProc   = (WNDPROC)WndProc;
  wc.cbClsExtra    = wc.cbWndExtra = 0;
  wc.hInstance     = hInstance;
  wc.hIcon         = LoadIcon (hInstance, "Clip");
  wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
  wc.lpszMenuName  = wc.lpszClassName = "Clip";
  return RegisterClass(&wc); // Register the window class and return success/failure code.
}

const BOOL InitInst(const HINSTANCE hInstance, const int nCmdShow)
{
  RECT rctmp={0, 0, DIBWidth, DIBHeight};
  AdjustWindowRect(&rctmp, WS_OVERLAPPEDWINDOW, FALSE);

  const HWND hwnd = CreateWindow("Clip", "Clip", WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX|WS_DLGFRAME, GetSystemMetrics(SM_CXSCREEN) - (rctmp.right - rctmp.left), GetSystemMetrics(SM_CYSCREEN) - (rctmp.bottom - rctmp.top), rctmp.right - rctmp.left, rctmp.bottom - rctmp.top, NULL, NULL, hInstance, NULL);
  if (!hwnd)
  {
    return FALSE;
  }

  BMInfo.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
  BMInfo.bmiHeader.biWidth = DIBWidth;
  BMInfo.bmiHeader.biHeight = DIBHeight;
  BMInfo.bmiHeader.biPlanes = 1;
  BMInfo.bmiHeader.biBitCount = 8;
  BMInfo.bmiHeader.biCompression = BI_RGB;
  BMInfo.bmiHeader.biSizeImage = BMInfo.bmiHeader.biXPelsPerMeter = BMInfo.bmiHeader.biYPelsPerMeter = 0;
  BMInfo.bmiHeader.biClrUsed = BMInfo.bmiHeader.biClrImportant = 256;

  RGBQUAD palette[256];
  for (int i=0; i!=256; ++i)
  {
    palette[i].rgbRed = rand(); palette[i].rgbGreen = rand(); palette[i].rgbBlue = rand(); palette[i].rgbReserved = 0;
  }
  memcpy(&BMInfo.bmiColors[0], &palette[0], sizeof(palette[0])*256);

  PalInfo.palVersion = 0x300;
  PalInfo.palNumEntries = 256;
  for (int i=0; i!=256; ++i)
  {
    PalInfo.palPalEntry[i].peRed = palette[i].rgbRed;
    PalInfo.palPalEntry[i].peGreen = palette[i].rgbGreen;
    PalInfo.palPalEntry[i].peBlue = palette[i].rgbBlue;
    PalInfo.palPalEntry[i].peFlags = PC_NOCOLLAPSE;
  }

  const HPALETTE log_palette = CreatePalette((LOGPALETTE*)&PalInfo); // create the palette
  const HDC hdc = GetDC(hwnd);
  SelectPalette(hdc, log_palette, FALSE); // select it for that DC
  RealizePalette(hdc); // realize a palette on that DC
  DeleteObject(log_palette); // delete palette handler

  hDIBSection = CreateDIBSection(hdc, (BITMAPINFO*)&BMInfo, DIB_RGB_COLORS, &pDIBBase, NULL, 0);
  if (!hDIBSection) 
  {
    return FALSE;
  }

  Set_DIB();

  ShowWindow(hwnd, nCmdShow); // Show the window

  ReleaseDC(hwnd, hdc);
  hwndOutput = hwnd;

  file=fopen("log.txt","w");
  sprintf(string,"bb %f\n",123.45); debugstring();
 
  return TRUE;
}

const void Input()
{
  if (GetAsyncKeyState(VK_UP)) --start.z;
  if (GetAsyncKeyState(VK_DOWN)) ++start.z;
  if (GetAsyncKeyState(VK_LEFT)) --start.x;
  if (GetAsyncKeyState(VK_RIGHT)) ++start.x;

  if (GetAsyncKeyState(VK_NUMPAD8)) --end.z;
  if (GetAsyncKeyState(VK_NUMPAD2)) ++end.z;
  if (GetAsyncKeyState(VK_NUMPAD4)) --end.x;
  if (GetAsyncKeyState(VK_NUMPAD6)) ++end.x;
}

const xz Clip3DLeft(const xz Start, const xz End)
{
	const float Dx = End.x - Start.x;
	const float Dz = End.z - Start.z;
	const float Scale = (Start.x + Start.z) / (-Dz - Dx);

	xz rc;
	rc.x = Start.x + (End.x - Start.x) *Scale;
	rc.z = Start.z + (End.z - Start.z) *Scale;
	return rc;
}


const xz Clip3DRightV1(const xz Start, const xz End)
{
	const float Dx = End.x - Start.x;
	const float Dz = End.z - Start.z;
	const float Scale = (Start.x - Start.z) / (-Dx + Dz);

	xz rc;
	rc.x = Start.x + (End.x - Start.x) *Scale;
	rc.z = Start.z + (End.z - Start.z) *Scale;
	return rc;
}

const xz Clip3DRight(const xz Start, const xz End)
{
	const int D_x = End.x - Start.x;
	const int D_z = End.z - Start.z;
	const int D_Diff = D_z - D_x ; //-D_x + D_z;
	const int S_Diff = Start.x - Start.z;
	const int Scale = S_Diff *32768 / D_Diff;

	xz rc;
	rc.x = Start.x + (D_x *Scale /32768);
	rc.z = Start.z + (D_z *Scale /32768);
	return rc;
}

const xz Clip3DRightShift(const xz Start, const xz End)
{
	sprintf(string,"c3drs sx %4i sz %4i ex %4i ez %4i\n",Start.x,Start.z,End.x,End.z); debugstring();
	const int Dx = End.x - Start.x;
	const int Dz = End.z - Start.z;
	sprintf(string," dx %4i dz %4i\n",Dx,Dz); debugstring();

	int Sx=0;
	int Sz=0;
	int Ex=Dx;
	int Ez=-Dz;
	int Mx;
	int Mz;
	sprintf(string," sx %4i sz %4i ex %4i ez %4i\n",Sx,Sz,Ex,Ez); debugstring();

	while (1)
	{
		
		Mx=(Sx+Ex)/2;
		Mz=(Sz+Ez)/2;
	sprintf(string,"  Mx %4i Mz %4i\n",Mx,Mz); debugstring();
		if (Mx==Mz) break;
		if (Mx>Mz)
		{
			Ex=Mx;
			Ez=Mz;
		}
		else
		{
			Sx=Mx;
			Sz=Mz;
		}
	}
	xz rc;
	rc.x=Mx;
	rc.z=Mz;
	return rc;
}

const void UpdateWorld() // Render the current state of the world to the screen.
{
  	Input();

  	memset(pDIBBase, 0, DIBWidth*DIBHeight);    // clear frame
  	
	BigPlot(XZ(start.x+width, start.z+height), 30);
  	BigPlot(XZ(end.x+width, end.z+height), 20);

	const xz clip=Clip3DRight(start, end);
	const xz clipshift=Clip3DRightShift(end, start);
  	BigPlot(XZ(clip.x+width, clip.z+height), 40);

  const HDC hdcScreen = GetDC(hwndOutput); // We've drawn the frame; copy it to the screen
  const HDC hdcDIBSection = CreateCompatibleDC(hdcScreen);
  const HBITMAP holdbitmap = SelectObject(hdcDIBSection, hDIBSection);
  BitBlt(hdcScreen, 0, 0, DIBWidth, DIBHeight, hdcDIBSection, 0, 0, SRCCOPY);
  
  const HFONT hf=CreateFont(1, 0, 0, 0, 0, TRUE, 0, 0, 0, 0, 0, 0, 0, "Times New Roman");
  sprintf(string,"s %4i %4i e %4i %4i c %4i %4i cs %4i %4i\n",start.x,start.z,end.x,end.z,clip.x,clip.z,clipshift.x,clipshift.z); TextOutA(hdcScreen,1,1,string, strlen(string));

  SelectObject(hdcDIBSection, holdbitmap);
  DeleteDC(hdcDIBSection);
  DeleteObject(holdbitmap);
  ReleaseDC(hwndOutput,hdcScreen);

  Sleep(1); // 20ms = 1/50s
}

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
  if (!InitApp(hInstance)) // Initialize shared things
  { 
    return FALSE;     // Exits if unable to initialize
  }
  if (!InitInst(hInstance, nCmdShow)) // Perform initializations that apply to a specific instance
  {
    return FALSE;
  }

  MSG msg;
  for (;;) // Acquire and dispatch messages until a WM_QUIT message is received
  {
    while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) 
    {
      if (msg.message == WM_QUIT) 
      {
        return (msg.wParam);
      }
      TranslateMessage(&msg);// xlates virt keycodes
      DispatchMessage(&msg); // Dispatches msg to window
    }
    UpdateWorld(); // Update the world
  }
  return (msg.wParam); // Returns the value from PostQuitMessage
}

/*So, our clipping function for the left plane is:
 dx = x2 - x1
 dz = z2 - z1
 scalar = (x1 + z1) / (-dz - dx)
So, to calculate the intersection point for the left plane we have:


 dx = Bx - Ax
 dz = Bz - Az
 Scaler = (Ax + Az) / (-dz - dx)

 Ix = Ax + (Bx - Ax)*Scaler
 Iy = Ay + (By - Ay)*Scaler
 Iz = Az + (Bz - Az)*Scaler
Tada!! Simple or what. Now, do the same thing for right, top and bottom and you have these formulas:


 dx = x2 - x1
 dy = y2 - y1
 dz = z2 - z1

   Plane ³ Scaler
 --------Å----------------------------
    left ³ (Ax + Az) / (-dz - dx)
   right ³ (Ax - Az) / (-dx + dz)
     top ³ (Ay - Az) / (-dy + dz)
  bottom ³ (Ay + Az) / (-dz - dy)*/
