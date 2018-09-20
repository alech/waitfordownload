# WaitForDownload

A quick tool that just waits using INotify until a .part file (created by
Firefox during a download) is gone and then quits. Allows you to do something
along the lines of

```
watchfordownload foo.mp4; mplayer foo.mp4
```
