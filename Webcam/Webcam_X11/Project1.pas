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
  sdl2;

var
  dis: PDisplay;
  win, rootWin: TWindow;
  Event: TXEvent;
  scr: cint;
  gc: TGC;
  Width, Height: cint;

  image: PXImage;
  My_v4l2: Tv4l2;

const
  EventMask = KeyPressMask or ExposureMask or PointerMotionMask or ButtonPressMask;

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
  begin
    Width := 640;
    Height := 480;

    dis := XOpenDisplay(nil);
    if dis = nil then begin
      WriteLn('Kann nicht das Display öffnen');
      Halt(1);
    end;
    scr := DefaultScreen(dis);
    gc := DefaultGC(dis, scr);
    rootWin := RootWindow(dis, scr);
    win := XCreateSimpleWindow(dis, rootWin, 10, 10, 640, 480, 1, BlackPixel(dis, scr), WhitePixel(dis, scr));
    XStoreName(dis, win, 'Webcam-Fenster');
    XSelectInput(dis, win, EventMask);
    XMapWindow(dis, win);

    gc := XCreateGC(dis, win, 0, nil);

    visual := DefaultVisual(dis, scr);
    image := XCreateImage(dis, visual, 24, ZPixmap, 0, nil, Width, Height, 32, 0);
  end;

begin

  Create_V4L2;
  Create_MainWin;

  while (True) do begin
    if XPending(dis) > 0 then begin
      XNextEvent(dis, @Event);
      case Event._type of
        Expose: begin
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
      image^.Data := PChar(My_v4l2.Get_BGRA_Buffer);
      XPutImage(dis, win, gc, image, 0, 0, 0, 0, Width, Height);
    end;
  end;

  XDestroyImage(image);
  XDestroyWindow(dis, win);
  XCloseDisplay(dis);
end.
