program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  dglOpenGL,
  oglVector,
  oglMatrix,
  Shader,
  SDL2;

const
  Screen_Widht = 640;
  Screen_Height = 480;

var
  gWindow: PSDL_Window;

  quit: boolean = False;
  e: TSDL_Event;

var
  MyShader: TShader;

  // Seite 53


const
  VAO_IDs_Trinagles = 0;
  VAO_IDs_NUMVAOs = 1;

  Buffer_IDs_ArrayBuffer = 0;
  Buffer_IDs_NumBuffers = 1;

  Attrib_IDs_vPosition = 0;

var
  VAOs: array[0..VAO_IDs_NUMVAOs - 1] of GLuint;
  Buffers: array[0..Buffer_IDs_NumBuffers - 1] of GLuint;
const
  NumVertices = 6;


const
  vertices: array [0..NumVertices - 1]of TVector2f = (
    (-0.90, -0.90), // Triangle 1
    (0.85, -0.90),
    (-0.90, 0.85),
    (0.90, -0.85), // Triangle 2
    (0.90, 0.90),
    (-0.85, 0.90));



  procedure Init_SDL_and_OpenGL;
  begin
    // --- OpenGL inizialisieren
    if not InitOpenGL then begin
      WriteLn('OpenGL-Fehler');
      Halt(1);
    end;
    ReadExtensions;
    ReadImplementationProperties;

    // --- SDL inizialisieren
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      Halt(1);
    end;

    // --- Context für OpenGL erzeugen
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

    gWindow := SDL_CreateWindow('SDL Tuorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN);
    if SDL_GL_CreateContext(gWindow) = nil then begin
      Writeln('OpenGL context could not be created! SDL Error: ', SDL_GetError);
      Halt(1);
    end;

    if SDL_GL_SetSwapInterval(1) < 0 then begin
      WriteLn('Warning: Unable to set VSync! SDL Error: ', SDL_GetError);
    end;
  end;

  procedure CreateScene;
  begin
    glCreateBuffers(Buffer_IDs_NumBuffers, Buffers);
    glNamedBufferStorage(Buffers[Buffer_IDs_ArrayBuffer], SizeOf(vertices), @vertices, 0);

    MyShader := TShader.Create([FileToStr('Vertexshader.glsl'), FileToStr('Fragmentshader.glsl')]);
    MyShader.UseProgram;

    glGenVertexArrays(VAO_IDs_NUMVAOs, VAOs);
    glBindVertexArray(VAOs[VAO_IDs_Trinagles]);
    glBindBuffer(GL_ARRAY_BUFFER, Buffers[Buffer_IDs_ArrayBuffer]);
    glVertexAttribPointer(Attrib_IDs_vPosition, 2, GL_FLOAT, GL_FALSE, 0, nil);
    glEnableVertexAttribArray(Attrib_IDs_vPosition);
  end;


  procedure DrawScene;
  const
    black: TVector4f = (0, 0, 0, 0);
  begin
    glClearBufferfv(GL_COLOR, 0, black);

    glBindVertexArray(VAOs[VAO_IDs_Trinagles]);
    glDrawArrays(GL_TRIANGLES, 0, NumVertices);


    SDL_GL_SwapWindow(gWindow);
  end;

  procedure Destroy_SDL_and_OpenGL;
  begin
    //glDeleteVertexArrays(1, @VBTriangle.VAO);
    //glDeleteVertexArrays(1, @VBQuad.VAO);
    //
    //glDeleteBuffers(1, @VBTriangle.VBO);
    //glDeleteBuffers(1, @VBQuad.VBO);

    MyShader.Free;

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
