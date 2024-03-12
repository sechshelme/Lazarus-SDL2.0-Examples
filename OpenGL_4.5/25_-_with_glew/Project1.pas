program Project1;

uses
  dglOpenGL,
  oglContext,
  oglVector,
  oglMatrix,
  oglShader,
  oglDebug,
  SDL2;


function glewInit:TGLenum; cdecl;external 'GLEW';

const
  Screen_Widht = 640;
  Screen_Height = 480;

var
  // SDL
  glcontext: TSDL_GLContext;
  gWindow: PSDL_Window;

  quit: boolean = False;
  e: TSDL_Event;

  // OpenGL
  MyShader: TShader;

  VAO: GLuint;
  VBO: GLuint;


const
  vertices: array of TVector2f = (
    (-0.90, -0.90), (0.85, -0.90), (-0.90, 0.85),
    (0.90, -0.85), (0.90, 0.90), (-0.85, 0.90));


  procedure Init_SDL_and_OpenGL;
  var
    glew_status: TGLenum;
  begin

    //// --- OpenGL inizialisieren
    //if not InitOpenGL then begin
    //  WriteLn('OpenGL-Fehler');
    //  Halt(1);
    //end;
    //ReadExtensions;
    //ReadImplementationProperties;
//    InitOpenGLDebug;

    // --- SDL inizialisieren
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      Halt(1);
    end;

    // --- Context für OpenGL erzeugen
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 5);

    gWindow := SDL_CreateWindow('SDL Tuorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN);
    glcontext := SDL_GL_CreateContext(gWindow);
    if glcontext = nil then begin
      Writeln('OpenGL context could not be created! SDL Error: ', SDL_GetError);
      Halt(1);
    end;

       glew_status:= glewInit;
   glViewport(0, 0, Screen_Widht, Screen_Height);
   WriteLn('glew_status: ', glew_status);


    if SDL_GL_SetSwapInterval(1) < 0 then begin
      WriteLn('Warning: Unable to set VSync! SDL Error: ', SDL_GetError);
    end;
  end;

  procedure CreateScene;
  begin

    MyShader := TShader.Create;
    MyShader.LoadShaderObjectFromFile(GL_VERTEX_SHADER, 'Vertexshader.glsl');
    MyShader.LoadShaderObjectFromFile(GL_FRAGMENT_SHADER, 'Fragmentshader.glsl');
    MyShader.LinkProgram;
    MyShader.UseProgram;


    WriteLn('dfdsaf');
    glCreateBuffers(1, @VBO);
    WriteLn('dfdsaf');
    glNamedBufferStorage(VBO, Length(vertices) * SizeOf(TVector3f), PVector3f(vertices), 0);
            WriteLn('dfdsaf');



    glGenVertexArrays(1, @VAO);
    glBindVertexArray(VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, nil);
    glEnableVertexAttribArray(0);
  end;


  procedure DrawScene;
  const
    black: TVector4f = (0.3, 0.0, 0.2, 1.0);
  begin
    glClearBufferfv(GL_COLOR, 0, black);

    glBindVertexArray(VAO);
    glDrawArrays(GL_TRIANGLES, 0, Length(vertices));

    SDL_GL_SwapWindow(gWindow);
  end;

  procedure Destroy_SDL_and_OpenGL;
  begin
    glDeleteVertexArrays(1, @VAO);
    glDeleteBuffers(1, @VBO);

    MyShader.Free;

    SDL_GL_DeleteContext(glcontext);
    SDL_DestroyWindow(gWindow);
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