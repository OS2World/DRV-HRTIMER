BASE=hrtimer

.SUFFIXES: .asm

$(BASE).sys: $*.obj $*.def
    link /MAP $*, $*.sys,, os2286, $*
    mapsym $*
