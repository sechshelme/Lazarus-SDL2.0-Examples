program project1;

uses
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

const
  path = 'bild.jpg';

var
  dis: PDisplay;
  win, rootWin: TWindow;
  Event: TXEvent;
  scr: cint;
  gc: TGC;
  Width, Height: DWord;

  image: PXImage;

  My_v4l2: Tv4l2;

  procedure Create_V4L2;
  const
    device = '/dev/video0';
  begin
    My_v4l2 := Tv4l2.Create(device, 640, 480);
    //    My_v4l2.se;

    My_v4l2.QueryCap;
    My_v4l2.GetFormat;
    My_v4l2.StreamOn;
  end;



  function ReadJPG(path: PChar; var jpg_buf: pbyte; var jpg_size: int64): cint;
  var
    file_info: stat;
    i, fd, rc: cint;
  begin
    Result := 0;
    rc := FpStat(path, file_info);
    if rc <> 0 then begin
      WriteLn('Kann JPG nicht öffnen');
      Exit(1);
    end;

    jpg_size := file_info.st_size;
    Getmem(jpg_buf, jpg_size + 100);

    fd := FpOpen(path, O_RDONLY);
    i := 0;
    while i < jpg_size do begin
      rc := FpRead(fd, PChar(jpg_buf) + i, jpg_size - 1);
      WriteLn('Input: Read ', rc, jpg_size - i, ' bytes');
      Inc(i, rc);
    end;
    FpClose(fd);
  end;

  function Write_PPM(w, h, ps: cint; Data: PChar): cint;
  var
    fd: cint;
    s: string;
  begin
    Result := 0;
    fd := FpOpen('test.ppm', O_CREAT or O_WRONLY, &666);

    WriteStr(s, 'P6 ', w, ' ', h, ' 255'#10);
    WriteLn(s);

    FpWrite(fd, PChar(s), Length(s));
    FpWrite(fd, Data, w * h * ps);
    FpClose(fd);

    WriteLn('End of decompression');
  end;

  function JPG_To_RGB(jpg_buf: pbyte; jpg_size: dword; var rgb_Buf: PChar; var w, h: DWord): cint;
  var
    cinfo: Tjpeg_decompress_struct;
    rc, pixel_size, row_stride: longint;
    jerr: Tjpeg_error_mgr;
    buffer_array: PChar;

  begin
    WriteLn('Proc: Create Decompress struct');
    cinfo.err := jpeg_std_error(@jerr);

    jpeg_create_decompress(@cinfo);

    WriteLn('Proc: Set memory buffer as source');
    jpeg_mem_src(@cinfo, jpg_buf, jpg_size);

    WriteLn('Proc: Read the JPEG header');
    rc := jpeg_read_header(@cinfo, 1);
    if rc <> 1 then begin
      WriteLn('File does not seem to be a normal JPEG');
    end;

    WriteLn('Proc: Initiate JPEG decompression');
    jpeg_start_decompress(@cinfo);
    w := cinfo.output_width;
    h := cinfo.output_height;
    pixel_size := cinfo.output_components;

    WriteLn('Proc: Image is ', w, ' by ', h, ' with ', pixel_size, ' components');

    Getmem(rgb_Buf, w * h * 4);
    row_stride := w * 4;

    cinfo.out_color_space := JCS_EXT_BGRA;
    WriteLn('Proc: Start reading scanlines');
    while cinfo.output_scanline < cinfo.output_height do begin
      buffer_array := @rgb_Buf[cinfo.output_scanline * row_stride];
      jpeg_read_scanlines(@cinfo, @buffer_array, 1);
    end;

    WriteLn('Proc: Done reading scanlines');
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
    win := XCreateSimpleWindow(dis, rootWin, 10, 10, 640, 480, 1, BlackPixel(dis, scr), WhitePixel(dis, scr));
    XStoreName(dis, win, 'Webcam-Fenster');
    XSelectInput(dis, win, EventMask);
    XMapWindow(dis, win);

    gc := XCreateGC(dis, win, 0, nil);

    visual := DefaultVisual(dis, scr);
    image := XCreateImage(dis, visual, 24, ZPixmap, 0, nil, Width, Height, 32, 0);
  end;

  function main: cint;
  var
    jpg_size: int64 = 0;
    jpg_buffer: pbyte = nil;
    bmp_buffer: PChar = nil;

  begin
    Create_V4L2;
    ReadJPG(path, jpg_buffer, jpg_size);

    jpg_buffer := My_v4l2.GetYUYVBuffer;
    JPG_To_RGB(jpg_buffer, jpg_size, bmp_buffer, Width, Height);

    jpg_size := 2000000000;
    WriteLn('----------', jpg_size);


    Width := 640;
    Height := 480;
    //JPG_To_RGB(My_v4l2.GetYUYVBuffer, jpg_size, bmp_buffer, Width, Height);
    //  Write_PPM(Width, Height, 3, PChar(bmp_buffer));

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
        //        image^.Data := bmp_buffer;
        XPutImage(dis, win, gc, image, 0, 0, 0, 0, Width, Height);
      end;
    end;

    Freemem(jpg_buffer);
    Freemem(bmp_buffer);
    Result := 0;
  end;

begin
  main;
end.
