program Project1;

uses
  BaseUnix,
  unixtype,
  ctypes,
  xlib,
  xutil,
  keysym,
  x,
  v4l2,
  sdl2
  ;

type
  TxbmMask = record
    Width, Height: cuint;
    bits: array of byte;
  end;

  // https://stackoverflow.com/questions/64521652/create-xlib-window-with-a-frame-buffer-i-can-draw-directly-and-use-xputimage

type
  TBitMapData = record
    Width, Height: integer;
    Data: packed array of record
      b, g, r, a: byte;
      end;
  end;

var
  dis: PDisplay;
  win, rootWin: TWindow;
  Event: TXEvent;
  scr: cint;
  gc: TGC;

  BitmapData: TBitMapData;

  image: PXImage;
  My_v4l2: Tv4l2;

const
  EventMask = KeyPressMask or ExposureMask or PointerMotionMask or ButtonPressMask;

  procedure wait;
  var
    rem, Req: timespec;
  begin
    Req.tv_nsec := 30000000;
    Req.tv_sec := 0;
    fpNanoSleep(@Req, @rem);
  end;

  procedure Create_V4L2;
  const
    device = '/dev/video0';
  begin
    My_v4l2 := Tv4l2.Create(device, 640, 480);

    My_v4l2.QueryCap;
    My_v4l2.GetFormat;
    My_v4l2.StreamOn;
  end;


  procedure Create_MainWin;
  var
    visual: PVisual;
    x, y: integer;
  begin

    // Erstellt die Verbindung zum Server
    dis := XOpenDisplay(nil);
    if dis = nil then begin
      WriteLn('Kann nicht das Display öffnen');
      Halt(1);
    end;
    scr := DefaultScreen(dis);
    gc := DefaultGC(dis, scr);
    win := XCreateSimpleWindow(dis, RootWindow(dis, scr), 10, 10, 640, 480, 1, BlackPixel(dis, scr), WhitePixel(dis, scr));

    XSelectInput(dis, win, KeyPressMask or ExposureMask);
    XMapWindow(dis, win);


    with BitmapData do begin
      Width := 640;
      Height := 480;

      SetLength(Data, Width * Height);
      for y := 0 to Height - 1 do begin
        for x := 0 to Width - 1 do begin
          Data[y * Width + x].b := x * y;
          Data[y * Width + x].g := y;
          Data[y * Width + x].r := x;
          Data[y * Width + x].a := $00;
        end;
      end;

      visual := DefaultVisual(dis, scr);

  //  image := XCreateImage(dis, visual, DefaultDepth(dis, scr), ZPixmap, 0, pansichar(BitmapData.Data), 640, 480, 32, 0);
//  image := XCreateImage(dis, visual, DefaultDepth(dis, scr), ZPixmap, 0,PAnsiChar( My_v4l2.GetRGB32Buffer), Width, Height, 32, 0);
  image := XCreateImage(dis, visual, DefaultDepth(dis, scr), ZPixmap, 0,PAnsiChar( My_v4l2.GetRGB24Buffer), Width, Height, 32, 0);
    end;

  end;

function NewHandle(para1: PDisplay; para2: PXErrorEvent): cint; cdecl;
begin

end;

begin
  dis := XOpenDisplay(nil);
  if dis = nil then begin
    WriteLn('Kann nicht das Display öffnen');
    Halt(1);
  end;
  scr := DefaultScreen(dis);
  rootWin := RootWindow(dis, scr);

 Create_V4L2;
  Create_MainWin;

  XSetErrorHandler(@NewHandle);

  gc := XCreateGC(dis, win, 0, nil);

  XStoreName(dis, win, 'Transparentes-Fenster');

  while (True) do begin

    if XPending(dis) > 0 then begin

      XNextEvent(dis, @Event);
      case Event._type of
        Expose: begin
          BitmapData.Data[random(1000)].r := $FF;
          BitmapData.Data[random(1000)].g := $FF;
          BitmapData.Data[random(1000)].b := $FF;
        end;
        ButtonPress: begin
          XRaiseWindow(dis, Event.xbutton.window);
        end;

        KeyPress: begin
          if XLookupKeysym(@Event.xkey, 0) = XK_Escape then begin
            Break;
          end;
        end;
      end;
    end else begin
      //  wait;

      //      XClearWindow(dis, win);


      My_v4l2.GetRGB24Buffer;
      //      XPutImage(dis, win, gc, image, 0, 0, 10, 10, BitmapData.Width, BitmapData.Height);
            XPutImage(dis, win, gc, image, 0, 0, 10, 10, BitmapData.Width, BitmapData.Height);
    end;

  end;

  XDestroyImage(image);

  XDestroyWindow(dis, win);


  XCloseDisplay(dis);
end.
