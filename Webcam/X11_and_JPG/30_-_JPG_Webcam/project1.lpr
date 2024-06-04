program project1;

uses
  heaptrc,
  BaseUnix,
  jpeglib,
  jmorecfg,
  xlib,
  xutil,
  keysym,
  v4l2,
  x;

  // https://stackoverflow.com/questions/59632172/how-to-decode-an-mjpeg-to-raw-rgb-or-yuv-data
  // https://gist.github.com/PhirePhly/3080633

const
  EventMask = KeyPressMask or ExposureMask or PointerMotionMask or ButtonPressMask;

var
  dis: PDisplay;
  win, rootWin: TWindow;
  Event: TXEvent;
  scr: cint;
  gc: TGC;
  Width: DWord = 1920;
  Height: DWord = 1080;
//  Width: DWord = 640;
//  Height: DWord = 480;

  image: PXImage;

  My_v4l2: Tv4l2;

  procedure Create_V4L2;
  const
    device = '/dev/video0';
  begin
    My_v4l2 := Tv4l2.Create(device, Width, Height);
    My_v4l2.QueryCap;
    My_v4l2.GetFormat;
    My_v4l2.StreamOn;
  end;

  function JPG_To_RGB(jpg_buf: pbyte; jpg_size: dword; var rgb_Buf: PChar; var w, h: DWord): cint;
  var
    cinfo: Tjpeg_decompress_struct;
    rc, row_stride: longint;
    jerr: Tjpeg_error_mgr;
    buffer_array: PChar;

  begin
    cinfo.err := jpeg_std_error(@jerr);

    jpeg_create_decompress(@cinfo);
    jpeg_mem_src(@cinfo, jpg_buf, jpg_size);

    rc := jpeg_read_header(@cinfo, 1);
    if rc <> 1 then begin
      WriteLn('File does not seem to be a normal JPEG');
    end;

    jpeg_start_decompress(@cinfo);
    w := cinfo.output_width;
    h := cinfo.output_height;

    row_stride := w * 4;

    cinfo.out_color_space := JCS_EXT_BGRA;
    while cinfo.output_scanline < cinfo.output_height do begin
      buffer_array := @rgb_Buf[cinfo.output_scanline * row_stride];
      jpeg_read_scanlines(@cinfo, @buffer_array, 1);
    end;

    jpeg_finish_decompress(@cinfo);
    jpeg_destroy_decompress(@cinfo);
    Result := 0;
  end;

  procedure Create_MainWin;
  var
    visual: PVisual;
  begin
    dis := XOpenDisplay(nil);
    if dis = nil then begin
      WriteLn('Kann nicht das Display öffnen');
      Halt(1);
    end;
    scr := DefaultScreen(dis);
    gc := DefaultGC(dis, scr);
    rootWin := RootWindow(dis, scr);
    win := XCreateSimpleWindow(dis, rootWin, 10, 10, Width, Height, 1, BlackPixel(dis, scr), WhitePixel(dis, scr));
    XStoreName(dis, win, 'Webcam-Fenster');
    XSelectInput(dis, win, EventMask);
    XMapWindow(dis, win);

    gc := XCreateGC(dis, win, 0, nil);

    visual := DefaultVisual(dis, scr);
    image := XCreateImage(dis, visual, 24, ZPixmap, 0, nil, Width, Height, 32, 0);
    Getmem(image^.Data, Width * Height * 4);
  end;

  function main: cint;
  var
    jpg_size: int64;
    jpg_buffer: pbyte;
  begin
    jpg_size := Width * Height * 4;
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
        jpg_buffer := My_v4l2.GetYUYVBuffer;
        JPG_To_RGB(jpg_buffer, jpg_size, image^.Data, Width, Height);
        XPutImage(dis, win, gc, image, 0, 0, 0, 0, Width, Height);
      end;
    end;

    Freemem(image^.Data);

    Result := 0;
  end;

begin
  main;
end.
