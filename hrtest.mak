BASE=hrtest
CFLAGS=/Q+ /Ss /W3 /Kb+p+ /Gm- /Gd+ /Ti+ /O- /C
LFLAGS=/NOI /MAP /DE /NOL /A:16 /EXEPACK /BASE:65536

.SUFFIXES: .c

.c.obj:
    icc $(CFLAGS) $*.c

$(BASE).exe: $*.obj $*.def
    link386 $(LFLAGS) $*,,, os2386, $*
