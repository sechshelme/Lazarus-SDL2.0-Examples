program Project1;

uses
  oglglad_gl,
  oglContext,
  oglVector,
  oglMatrix,
  oglShader,
  oglTextur,
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
  Shader: TShader; // Shader Klasse
  Textur: TTexturBuffer;

  VAO, VBO: GLuint;
  ViewMatrix, ProdMatrix, RotateMatrix: TMatrix;

  Color_ID,
  ModelMatrix_ID,
  ViewMatrix_ID,
  ProMatrix_ID: GLint;

  // https://learnopengl.com/code_viewer_gh.php?code=src/4.advanced_opengl/2.stencil_testing/stencil_testing.cpp
  // https://learnopengl.com/Advanced-OpenGL/Stencil-testing

  // https://open.gl/depthstencils
  // https://open.gl/content/code/c5_reflection.txt

  // https://learnopengl.com/Advanced-OpenGL/Stencil-testing
  // https://en.wikibooks.org/wiki/OpenGL_Programming/Stencil_buffer
  // https://gist.github.com/sealfin/d22f4ba4d1022e1b89dd
  // https://lazyfoo.net/tutorials/OpenGL/26_the_stencil_buffer/index.php

const
  vertices: array of GLfloat = (
    -0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 0.0,
    0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 1.0, 0.0,
    0.5, 0.5, -0.5, 1.0, 1.0, 1.0, 1.0, 1.0,
    0.5, 0.5, -0.5, 1.0, 1.0, 1.0, 1.0, 1.0,
    -0.5, 0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,
    -0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 0.0,

    -0.5, -0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0,
    0.5, -0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,
    0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0,
    0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0,
    -0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 1.0,
    -0.5, -0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0,

    -0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,
    -0.5, 0.5, -0.5, 1.0, 1.0, 1.0, 1.0, 1.0,
    -0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,
    -0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,
    -0.5, -0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0,
    -0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,

    0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,
    0.5, 0.5, -0.5, 1.0, 1.0, 1.0, 1.0, 1.0,
    0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,
    0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,
    0.5, -0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0,
    0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,

    -0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,
    0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 1.0, 1.0,
    0.5, -0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,
    0.5, -0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,
    -0.5, -0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0,
    -0.5, -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,

    -0.5, 0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,
    0.5, 0.5, -0.5, 1.0, 1.0, 1.0, 1.0, 1.0,
    0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,
    0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0,
    -0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0,
    -0.5, 0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0,

    -1.0, -1.0, -0.5, 0.0, 0.0, 0.0, 0.0, 0.0,
    1.0, -1.0, -0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
    1.0, 1.0, -0.5, 0.0, 0.0, 0.0, 1.0, 1.0,
    1.0, 1.0, -0.5, 0.0, 0.0, 0.0, 1.0, 1.0,
    -1.0, 1.0, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
    -1.0, -1.0, -0.5, 0.0, 0.0, 0.0, 0.0, 0.0);

const
  Vertex_Shader =
    '#version 330 core' + #10 +
    'layout (location = 0) in vec3 position;' + #10 +
    'layout (location = 1) in vec3 color;' + #10 +
    'layout (location = 2) in vec2 texcoord;' + #10 +
    'out vec3 Color;' + #10 +
    'out vec2 Texcoord;' + #10 +
    'uniform mat4 model;' + #10 +
    'uniform mat4 view;' + #10 +
    'uniform mat4 proj;' + #10 +
    'uniform vec3 overrideColor;' + #10 +
    'void main()' + #10 +
    '{' + #10 +
    '    Color = overrideColor * color;' + #10 +
    '    Texcoord = texcoord;' + #10 +
    '    gl_Position = proj * view * model * vec4(position, 1.0);' + #10 +
    '}';

  Fragment_Shader =
    '#version 330 core' + #10 +
    'in vec3 Color;' + #10 +
    'in vec2 Texcoord;' + #10 +
    'out vec4 outColor;' + #10 +
    'uniform sampler2D texKitten;' + #10 +
    'uniform sampler2D texPuppy;' + #10 +
    'void main()' + #10 +
    '{' + #10 +
    '    outColor = vec4(Color, 1.0) * texture(texKitten, Texcoord);' + #10 +
    '}';

  procedure Init_SDL_and_OpenGL;
  begin
    // --- SDL inizialisieren
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      Halt(1);
    end;

    gWindow := SDL_CreateWindow('SDL Tuorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height,
      SDL_WINDOW_RESIZABLE or SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN);
    // --- Context für OpenGL erzeugen
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);

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
    Shader := TShader.Create;
    Shader.LoadShaderObject(GL_VERTEX_SHADER, Vertex_Shader);
    Shader.LoadShaderObject(GL_FRAGMENT_SHADER, Fragment_Shader);
    Shader.LinkProgram;
    Shader.UseProgram;
    with Shader do begin
      ModelMatrix_ID := UniformLocation('model');
      ViewMatrix_ID := UniformLocation('view');
      ProMatrix_ID := UniformLocation('proj');
      Color_ID := UniformLocation('overrideColor');
      glUniform1i(UniformLocation('texKitten'), 0);
    end;
    glUniform3f(Color_ID, 2.0, 2.0, 2.0);

    RotateMatrix.Identity;
    ViewMatrix.Identity;
    ViewMatrix.TranslateZ(-4);
    ViewMatrix.RotateA(2.0 + pi);
    ProdMatrix.Identity;
    ProdMatrix.Perspective(45, Screen_Widht / Screen_Height, 0.1, 100.0);

    glEnable(GL_DEPTH_TEST);

    glGenVertexArrays(1, @VAO);
    glBindVertexArray(VAO);
    glGenBuffers(1, @VBO);

    // Vertex
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, Length(vertices) * sizeof(GLfloat), PGLvoid(vertices), GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * SizeOf(GLfloat), nil);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * SizeOf(GLfloat), PGLvoid(3 * SizeOf(GLfloat)));

    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * SizeOf(GLfloat), PGLvoid(6 * SizeOf(GLfloat)));

    ViewMatrix.Uniform(ViewMatrix_ID);
    ProdMatrix.Uniform(ProMatrix_ID);

    Textur := TTexturBuffer.Create;
    //    Textur.LoadTextures('mauer.bmp');
    Textur.LoadTextures(2, 2, [$FF, $00, $00, $FF, $00, $FF, $00, $FF, $00, $00, $FF, $FF, $FF, $00, $00, $FF]);
    Textur.ActiveAndBind;
  end;

  procedure DrawScene;
  var
    mat: TMatrix;
  begin
    glBindVertexArray(VAO);
    Shader.UseProgram;

    glClearColor(0.25, 0.1, 0.2, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // Draw cube
    mat.Identity;
    mat := mat * RotateMatrix;
    mat.Uniform(ModelMatrix_ID);
    glDrawArrays(GL_TRIANGLES, 0, 36);

    // Draw floor
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_ALWAYS, 1, $FF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
    glStencilMask($FF);
    glDepthMask(GL_FALSE);
    glClear(GL_STENCIL_BUFFER_BIT);
    glDrawArrays(GL_TRIANGLES, 36, 6);

    // Draw cube reflect
    // glStencilFunc(GL_NOTEQUAL, 1, $FF);
    glStencilFunc(GL_EQUAL, 1, $FF);
    glStencilMask($00);
    glDepthMask(GL_TRUE);

    mat.TranslateZ(-1.0);
    mat.Scale(1, 1, -1);
    mat.Uniform(ModelMatrix_ID);
    glUniform3f(Color_ID, 0.5, 0.5, 0.5);
    glDrawArrays(GL_TRIANGLES, 0, 36);
    glUniform3f(Color_ID, 1.0, 1.0, 1.0);

    glDisable(GL_STENCIL_TEST);

    SDL_GL_SwapWindow(gWindow);
  end;

  procedure Destroy_SDL_and_OpenGL;
  begin
    glDeleteVertexArrays(1, @VAO);
    glDeleteBuffers(1, @VBO);

    Shader.Free;

    SDL_GL_DeleteContext(glcontext);
    SDL_DestroyWindow(gWindow);
    SDL_Quit();
  end;

  procedure RunScene;
  const
    step: GLfloat = 0.02;
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
      RotateMatrix.RotateC(step);
      DrawScene;
    end;
  end;

begin
  Init_SDL_and_OpenGL;
  CreateScene;
  RunScene;
  Destroy_SDL_and_OpenGL;
end.
