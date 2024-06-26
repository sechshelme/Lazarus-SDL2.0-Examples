program Project1;

uses
  oglglad_gl,
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

  vertex_shader_text: string =
    '#version 450 core' + #10 +
    '' + #10 +
    'layout (location = 0) in vec4 vPosition;' + #10 +
    '' + #10 +
    'void main()' + #10 +
    '{' + #10 +
    '  gl_Position = vPosition;' + #10 +
    '}';

  fragment_shader_text =
    '#version 450 core' + #10 +
    '' + #10 +
    'layout (location = 0) out vec4 fColor;' + #10 +
    '' + #10 +
    'void main()' + #10 +
    '{' + #10 +
    '  fColor = vec4(0.5, 0.4, 0.8, 1.0);' + #10 +
    '}';

  procedure Init_SDL_and_OpenGL;
  begin
    // --- SDL inizialisieren
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      Halt(1);
    end;

    // --- Context für OpenGL erzeugen
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 5);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

    gWindow := SDL_CreateWindow('SDL Tuorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN);
    glcontext := SDL_GL_CreateContext(gWindow);
    if glcontext = nil then begin
      Writeln('OpenGL context could not be created! SDL Error: ', SDL_GetError);
      Halt(1);
    end;

    if SDL_GL_SetSwapInterval(1) < 0 then begin
      WriteLn('Warning: Unable to set VSync! SDL Error: ', SDL_GetError);
    end;

    // --- OpenGL inizialisieren
    Load_GLADE;
    InitOpenGLDebug;
  end;

  procedure CreateScene;
  begin
    glCreateBuffers(1, @VBO);
    glNamedBufferStorage(VBO, Length(vertices) * SizeOf(TVector2f), PVector2f(vertices), 0);

    MyShader := TShader.Create;
    MyShader.LoadShaderObject(GL_VERTEX_SHADER, vertex_shader_text);
    MyShader.LoadShaderObject(GL_FRAGMENT_SHADER, fragment_shader_text);
    MyShader.LinkProgram;
    MyShader.UseProgram;

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
