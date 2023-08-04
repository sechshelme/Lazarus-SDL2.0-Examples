#include "v4l2_driver.h"
#include <SDL2/SDL.h>
#include <linux/videodev2.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <time.h>
#include <unistd.h>

#define SAVE_EVERY_FRAME 0

pthread_t thread_stream;
SDL_Window *sdlScreen;
SDL_Renderer *sdlRenderer;
SDL_Texture *sdlTexture;
SDL_Rect sdlRect;


struct streamHandler {
  int fd;
  void (*framehandler)(void *pframe, int length);
};

static void frame_handler(void *pframe, int length) {
  SDL_UpdateTexture(sdlTexture, &sdlRect, pframe, IMAGE_WIDTH * 2);
  //  SDL_UpdateYUVTexture
  SDL_RenderClear(sdlRenderer);
  SDL_RenderCopy(sdlRenderer, sdlTexture, NULL, &sdlRect);
  SDL_RenderPresent(sdlRenderer);

}

static void *v4l2_streaming(void *arg) {
  // SDL2 begins
  memset(&sdlRect, 0, sizeof(sdlRect));
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER)) {
    printf("Could not initialize SDL - %s\n", SDL_GetError());
    return NULL;
  }

  sdlScreen = SDL_CreateWindow("Simple YUV Window", SDL_WINDOWPOS_UNDEFINED,
                               SDL_WINDOWPOS_UNDEFINED, IMAGE_WIDTH,
                               IMAGE_HEIGHT, SDL_WINDOW_SHOWN);

  if (!sdlScreen) {
    fprintf(stderr, "SDL: could not create window - exiting:%s\n",
            SDL_GetError());
    return NULL;
  }

  sdlRenderer = SDL_CreateRenderer(
      sdlScreen, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (sdlRenderer == NULL) {
    fprintf(stderr, "SDL_CreateRenderer Error\n");
    return NULL;
  }
  sdlTexture =
      SDL_CreateTexture(sdlRenderer, SDL_PIXELFORMAT_YUY2,
                        SDL_TEXTUREACCESS_STREAMING, IMAGE_WIDTH, IMAGE_HEIGHT);
  sdlRect.w = IMAGE_WIDTH;
  sdlRect.h = IMAGE_HEIGHT;

  int fd = ((struct streamHandler *)(arg))->fd;

  void (*handlerXXX)(void *pframe, int length) = ((struct streamHandler *)(arg))->framehandler;

  fd_set fds;
  struct v4l2_buffer buf;

  while (1) {
    int ret;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    struct timeval tv = {.tv_sec = 1, .tv_usec = 0};
    ret = select(fd + 1, &fds, NULL, NULL, &tv);

    if (FD_ISSET(fd, &fds)) {
      memset(&buf, 0, sizeof(buf));
      buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      buf.memory = V4L2_MEMORY_MMAP;
      ioctl(fd, VIDIOC_DQBUF, &buf);

      if (handlerXXX)
{
        (*handlerXXX)(v4l2_ubuffers[buf.index].start,
                   v4l2_ubuffers[buf.index].length);
}

      buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      buf.memory = V4L2_MEMORY_MMAP;
      ioctl(fd, VIDIOC_QBUF, &buf);
    }
  }
  return NULL;
}

int main(int argc, char const *argv[]) {
  const char *device = "/dev/video0";

  int video_fildes = v4l2_open(device);
  if (video_fildes == -1) {
    fprintf(stderr, "can't open %s\n", device);
    exit(-1);
  }

  if (v4l2_querycap(video_fildes, device) == -1) {
    perror("v4l2_querycap");
    goto exit_;
  }

printf("----- %d\n", V4L2_PIX_FMT_YUYV);

  // most of devices support YUYV422 packed.
  if (v4l2_sfmt(video_fildes, V4L2_PIX_FMT_YUYV) == -1) {
    perror("v4l2_sfmt");
    goto exit_;
  }

  if (v4l2_gfmt(video_fildes) == -1) {
    perror("v4l2_gfmt");
    goto exit_;
  }

  if (v4l2_sfps(video_fildes, 30) == -1) { // no fatal error
    perror("v4l2_sfps");
  }

  if (v4l2_mmap(video_fildes) == -1) {
    perror("v4l2_mmap");
    goto exit_;
  }

  if (v4l2_streamon(video_fildes) == -1) {
    perror("v4l2_streamon");
    goto exit_;
  }

  // create a thread that will update frame int the buffer
  struct streamHandler sH = {video_fildes, frame_handler};

 v4l2_streaming( (void *)(&sH));

  int quit = 0;
  SDL_Event e;
  while (!quit) {
    usleep(250);
  }


  if (v4l2_streamoff(video_fildes) == -1) {
    perror("v4l2_streamoff");
    goto exit_;
  }

  if (v4l2_munmap() == -1) {
    perror("v4l2_munmap");
    goto exit_;
  }

exit_:
  if (v4l2_close(video_fildes) == -1) {
    perror("v4l2_close");
  };
  SDL_Quit();
  return 0;
}
