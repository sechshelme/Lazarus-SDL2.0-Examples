program project1;

uses
  BaseUnix,
  jpeglib,
  jmorecfg;

const
  path = 'bild.jpg';

  function sprintf(restrict:PChar; fornat:PChar): cint;varargs cdecl; external 'c';


  function main: cint;
  var
    file_info: stat;
    i, rc, fd: cint;
    jpg_size: int64;
    jpg_buffer: pbyte;
    cinfo: Tjpeg_decompress_struct;
    jerr: Tjpeg_error_mgr;
    Width, Height: TJDIMENSION;
    pixel_size, bmp_size, row_stride: longint;
    bmp_buffer:PChar;
    buf:array[0..1023]of Char;

    buffer_array:array[0..0]of PByte;
  begin

    WriteLn('------  ', SizeOf(TJ_COLOR_SPACE));
    WriteLn('------  ', SizeOf(Tjpeg_decompress_struct));

    rc := FpStat(path, file_info);
    if rc <> 0 then begin
      WriteLn('Kann JPG nicht öffnen');
      Exit(1);
    end;

    jpg_size := file_info.st_size;
    Getmem(jpg_buffer, jpg_size + 100);

    fd := FpOpen(path, O_RDONLY);
    i := 0;
    while i < jpg_size do begin
      rc := FpRead(fd, PChar(jpg_buffer) + i, jpg_size - 1);
      WriteLn('Input: Read ', rc, jpg_size - i, ' bytes');
      Inc(i, rc);
    end;
    FpClose(fd);

    WriteLn('Proc: Create Decompress struct');
    cinfo.err := jpeg_std_error(@jerr);
    jpeg_create_decompress(@cinfo);


    WriteLn('Proc: Set memory buffer as source');
    jpeg_mem_src(@cinfo, jpg_buffer, jpg_size);


    WriteLn('Proc: Read the JPEG header');
    rc := jpeg_read_header(@cinfo, 1);
    if rc <> 1 then begin
      WriteLn('File does not seem to be a normal JPEG');
    end;

    WriteLn('Proc: Initiate JPEG decompression');
    jpeg_start_decompress(@cinfo);
    Width := cinfo.output_width;
    Height := cinfo.output_height;
    pixel_size := cinfo.output_components;

    WriteLn('Proc: Image is ', Width, ' by ', Height, ' with ', pixel_size, ' components');

    bmp_size:=Width*Height*pixel_size;
  Getmem( bmp_buffer,bmp_size);

  row_stride:=Width*pixel_size;

  WriteLn('Proc: Start reading scanlines');

  while cinfo.output_scanline<cinfo.output_height do begin
    buffer_array[0]:=PByte( bmp_buffer)+cinfo.output_scanline*row_stride;
    jpeg_read_scanlines(@cinfo,buffer_array,1);
    end;

  WriteLn('Proc: Done reading scanlines');
  jpeg_finish_decompress(@cinfo);
  jpeg_destroy_decompress(@cinfo);
  Freemem(jpg_buffer);

  fd:=FpOpen('test.ppm',O_CREAT or O_WRONLY, &666);

  rc:=  sprintf(buf, 'P6 %d %d 255'#10, width, height);

  WriteLn(buf);
  WriteLn(rc);
    FpWrite(fd,buf,rc);
    FpWrite(fd,bmp_buffer,bmp_size);
    FpClose(fd);
    Freemem(bmp_buffer);

    WriteLn('End of decompression');
  end;


begin
  main;
end.
