https://liballeg.org/

#Allegro 5
Download:
[https://github.com/liballeg/allegro5]

```bash
cmake -S . -B build && cmake --build build
su
cmake --install build --prefix=/usr
```
oder
```bash
    mkdir build
    cd build
    cmake ..
```
## Kompilieren

```bash
gcc allegro.c -lallegro
gcc allegro.c `pkg-config allegro-5 --libs --cflags`
`

```

#Allegro 4.x
```bash
apt install liballegro4-dev
```

## Kompilieren
```bash
gcc allegro.c -I/usr/local/include -L/usr/local/lib -lallegro
```




