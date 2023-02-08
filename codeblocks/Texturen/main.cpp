#include <exception>
#include <string>
#include <iostream>
#include <SDL.h>

int main(int argc, char* argv[]) {

    SDL_Init(SDL_INIT_EVERYTHING);

    SDL_Window *MainWindow = SDL_CreateWindow("My Game Window",
        SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED,
        1024, 768,
        SDL_WINDOW_SHOWN
        );

    SDL_Renderer *renderer = SDL_CreateRenderer(MainWindow, -1, 0);

    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);

    SDL_RenderClear(renderer);

    SDL_Texture *Tile = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888,
                        SDL_TEXTUREACCESS_STREAMING, 64, 64);

    // Initialize texture pixels to a red opaque RGBA value
    unsigned char* bytes = nullptr;
    int pitch = 0;
    SDL_LockTexture(Tile, nullptr, reinterpret_cast<void**>(&bytes), &pitch);
    unsigned char rgba[4] = { 255, 0, 0, 255 };
    for(int y = 0; y < 64; ++y) {
        for (int x = 0; x < 64; ++x) {
            memcpy(&bytes[(y * 64 + x)*sizeof(rgba)], rgba, sizeof(rgba));
        }
    }
    SDL_UnlockTexture(Tile);

    SDL_Rect destination = { 320, 240, 64, 64 };
    SDL_RenderCopy(renderer, Tile, NULL, &destination);
    SDL_RenderPresent(renderer);
    SDL_Delay(3000);

    //Clean up
    SDL_DestroyTexture(Tile);
    SDL_DestroyWindow(MainWindow);
    SDL_Quit();

    return 0;
}
