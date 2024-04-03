program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  oglglad_gl,
  oglVector,
  oglMatrix,
  oglShader,
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

const
  Triangle: array of Tmat3x3 =
    (((-0.4, 0.1, 0.0), (0.4, 0.1, 0.0), (0.0, 0.7, 0.0)));
  Quad: array of Tmat3x3 =
    (((-0.2, -0.6, 0.0), (-0.2, -0.1, 0.0), (0.2, -0.1, 0.0)),
    ((-0.2, -0.6, 0.0), (0.2, -0.1, 0.0), (0.2, -0.6, 0.0)));

type
  TVB = record
    VAO,
    VBO: GLuint;
  end;

var
  VBTriangle, VBQuad: TVB;

  MeshPos: TVector2f = (0, 0);
  MeshPos_ID: GLint;

  procedure Init_SDL_and_OpenGL;
  begin
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

    // --- OpenGL inizialisieren
    Load_GLADE;
  end;

  procedure CreateScene;
  begin
    MyShader := TShader.Create([FileToStr('Vertexshader.glsl'), FileToStr('Fragmentshader.glsl')]);
    MyShader.UseProgram;

    MeshPos_ID := MyShader.UniformLocation('MeshPos');

    glGenVertexArrays(1, @VBTriangle.VAO);
    glGenVertexArrays(1, @VBQuad.VAO);

    glGenBuffers(1, @VBTriangle.VBO);
    glGenBuffers(1, @VBQuad.VBO);

    glClearColor(0.6, 0.6, 0.4, 1.0); // Hintergrundfarbe

    // Daten für Dreieck
    glBindVertexArray(VBTriangle.VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBTriangle.VBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(Tmat3x3) * Length(Triangle), Pmat3x3(Triangle), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nil);

    // Daten für Quadrat
    glBindVertexArray(VBQuad.VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBQuad.VBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(Tmat3x3) * Length(Quad), Pmat3x3(Quad), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nil);
  end;


  procedure DrawScene;
  begin
    glClear(GL_COLOR_BUFFER_BIT);

    MyShader.UseProgram;

    MeshPos.Uniform(MeshPos_ID);

    // Zeichne Dreieck
    glBindVertexArray(VBTriangle.VAO);
    glDrawArrays(GL_TRIANGLES, 0, Length(Triangle) * 3);

    // Zeichne Quadrat
    glBindVertexArray(VBQuad.VAO);
    glDrawArrays(GL_TRIANGLES, 0, Length(Quad) * 3);

    SDL_GL_SwapWindow(gWindow);
  end;

  procedure Destroy_SDL_and_OpenGL;
  begin
    glDeleteVertexArrays(1, @VBTriangle.VAO);
    glDeleteVertexArrays(1, @VBQuad.VAO);

    glDeleteBuffers(1, @VBTriangle.VBO);
    glDeleteBuffers(1, @VBQuad.VBO);

    MyShader.Free;

    SDL_DestroyWindow(gWindow);
    SDL_Quit();
  end;

  procedure RunScene;
  const
    step = 0.01;
    MeshPosStep: TVector2f = (0, 0);
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
                SDLK_UP: begin
                  MeshPosStep.y := MeshPosStep.y + step;
                end;
                SDLK_DOWN: begin
                  MeshPosStep.y := MeshPosStep.y - step;
                end;
                SDLK_LEFT: begin
                  MeshPosStep.x := MeshPosStep.x - step;
                end;
                SDLK_RIGHT: begin
                  MeshPosStep.x := MeshPosStep.x + step;
                end;
              end;
            end;
          end;
          SDL_KEYUP: begin
            if e.key.repeat_ = 0 then begin
              case e.key.keysym.sym of
                SDLK_UP: begin
                  MeshPosStep.y := MeshPosStep.y - step;
                end;
                SDLK_DOWN: begin
                  MeshPosStep.y := MeshPosStep.y + step;
                end;
                SDLK_LEFT: begin
                  MeshPosStep.x := MeshPosStep.x + step;
                end;
                SDLK_RIGHT: begin
                  MeshPosStep.x := MeshPosStep.x - step;
                end;
              end;
            end;
          end;
          SDL_QUITEV: begin
            quit := True;
          end;
        end;
      end;
      MeshPos := MeshPos + MeshPosStep;
      DrawScene;
    end;
  end;

begin
  Init_SDL_and_OpenGL;
  CreateScene;
  RunScene;
  Destroy_SDL_and_OpenGL;
end.
