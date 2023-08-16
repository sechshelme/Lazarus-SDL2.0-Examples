https://wiki.ubuntuusers.de/Videograbbing/

Testprogramm:

```bash
qv4l2 
```

Tools:
```bash
sudo apt-get install qv4l2 

v4l2-ctl --list-formats-ext
v4l2-ctl --list-device

ffmpeg -f v4l2 -list_formats all -i /dev/video0
ffmpeg -f video4linux2 -list_formats all -i /dev/video0
ffmpeg -f v4l2 -s 200x150 -i /dev/video0 -c:v libx264 -b:v 300k out.mp4
ffmpeg -f v4l2 -i /dev/video0 -map 0 -c:v libx264 -f tee "output.mkv|[f=nut]pipe:" | ffplay-Pfeife:

```

