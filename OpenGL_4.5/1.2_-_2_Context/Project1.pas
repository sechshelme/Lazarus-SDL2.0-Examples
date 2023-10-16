program Project1;

uses
  dglOpenGL,
  oglContext,
  oglVector,
  oglMatrix,
  oglShader,
  oglDebug,
  SDL2;

const
  Screen_Widht = 640;
  Screen_Height = 480;

var
  // SDL
  gWindow0, gWindow1: PSDL_Window;
  glcontext: TSDL_GLContext;

  quit: boolean = False;
  e: TSDL_Event;

  // OpenGL
  MyShader: TShader;

  VAO: GLuint;
  VBO: GLuint;
  Color_ID: GLint;


const
  vertices: array of TVector2f = (
    (-0.90, -0.90), (0.85, -0.90), (-0.90, 0.85),
    (0.90, -0.85), (0.90, 0.90), (-0.85, 0.90));


  procedure Init_SDL_and_OpenGL;
  begin
    // --- OpenGL inizialisieren
    if not InitOpenGL then begin
      WriteLn('OpenGL-Fehler');
      Halt(1);
    end;
    ReadExtensions;
    ReadImplementationProperties;
    InitOpenGLDebug;

    // --- SDL inizialisieren
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      Halt(1);
    end;

    // --- Context für OpenGL erzeugen
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 5);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

    gWindow0 := SDL_CreateWindow('SDL Tuorial (0)', 100, 50, Screen_Widht, Screen_Height, SDL_WINDOW_RESIZABLE or SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN);
    gWindow1 := SDL_CreateWindow('SDL Tuorial (1)', 100+Screen_Widht, 50, Screen_Widht, Screen_Height, SDL_WINDOW_RESIZABLE or SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN);

    glcontext := SDL_GL_CreateContext(gWindow1);
    //    glcontext1 := SDL_GL_CreateContext(gWindow1);

    if glcontext = nil then begin
      Writeln('OpenGL context could not be created! SDL Error (0): ', SDL_GetError);
      Halt(1);
    end;

    //if glcontext1 = nil then begin
    //  Writeln('OpenGL context could not be created! SDL Error (1): ', SDL_GetError);
    //  Halt(1);
    //end;

    if SDL_GL_SetSwapInterval(1) < 0 then begin
      WriteLn('Warning: Unable to set VSync! SDL Error: ', SDL_GetError);
    end;
  end;

  procedure CreateScene;
  begin
    glCreateBuffers(1, @VBO);
    glNamedBufferStorage(VBO, Length(vertices) * SizeOf(TVector3f), PVector3f(vertices), 0);

    MyShader := TShader.Create;
    MyShader.LoadShaderObjectFromFile(GL_VERTEX_SHADER, 'Vertexshader.glsl');
    MyShader.LoadShaderObjectFromFile(GL_FRAGMENT_SHADER, 'Fragmentshader.glsl');
    MyShader.LinkProgramm;
    MyShader.UseProgram;
    Color_ID := MyShader.UniformLocation('uCol');


    glGenVertexArrays(1, @VAO);
    glBindVertexArray(VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, nil);
    glEnableVertexAttribArray(0);
  end;


  procedure DrawScene;
  const
    BkRed: TVector4f = (1.0, 0.5, 0.5, 1.0);
    BKGreen: TVector4f = (0.5, 1.0, 0.5, 1.0);
    ColRed: TVector3f = (1, 0, 0);
    ColGreen: TVector3f = (0, 1, 0);
  begin
    // Window 0
    SDL_GL_MakeCurrent(gWindow0, glcontext);
    glClearBufferfv(GL_COLOR, 0, BkRed);

    MyShader.UseProgram;
    ColRed.Uniform(Color_ID);

    glBindVertexArray(VAO);
    glDrawArrays(GL_TRIANGLES, 0, Length(vertices));

    SDL_GL_SwapWindow(gWindow0);

    // Window 1
    SDL_GL_MakeCurrent(gWindow1, glcontext);
    glClearBufferfv(GL_COLOR, 0, BKGreen);

    MyShader.UseProgram;
    ColGreen.Uniform(Color_ID);

    glBindVertexArray(VAO);
    glDrawArrays(GL_TRIANGLES, 0, Length(vertices));

    SDL_GL_SwapWindow(gWindow1);
  end;

  procedure Destroy_SDL_and_OpenGL;
  begin
    glDeleteVertexArrays(1, @VAO);
    glDeleteBuffers(1, @VBO);

    MyShader.Free;

    SDL_GL_DeleteContext(glcontext);
    SDL_DestroyWindow(gWindow0);
    SDL_DestroyWindow(gWindow1);
    SDL_Quit();
  end;

  procedure RunScene;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) <> 0 do begin
        case e.type_ of
          SDL_KEYDOWN: begin
            if e.key.repeat_ = 0 then begin
              case e.key.keysym.sym of
                SDLK_ESCAPE: begin
                  quit := True;
                end;
              end;
            end;
          end;
          SDL_QUITEV: begin
            quit := True;
          end;
        end;
      end;
      //      MeshPos := MeshPos + MeshPosStep;
      DrawScene;
    end;
  end;

begin
  Init_SDL_and_OpenGL;
  CreateScene;
  RunScene;
  Destroy_SDL_and_OpenGL;
end.
