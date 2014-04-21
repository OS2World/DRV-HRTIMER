HRTIMER.SYS is a high resolution timer implemented as a device driver. The
source code for this device driver was obtained from the Fall 1991 issue of
IBM Personal Systems Developer magazine. For insightful information on the
way this device driver works, please read that article. It's very interesting...

HRTIMER.SYS runs under OS/2 1.x and 2.0. It has a resolution of 840 nanoseconds.

HRTEST.EXE is a sample C Set/2 program that shows how to use the device driver
to calculate elapsed times. It demonstrates how to open the device driver, read
timestamps from it and close it. It factors in the overhead of the read and
has a function that is used to calculate elapsed time from a start and stop
timestamp.

To install the device driver, put the following statement in your config.sys:

DEVICE=HRTIMER.SYS


To run the test program, use the following command-line:

HRTEST [ milliseconds ]


HRTEST.EXE will issue a DosSleep for the amount of milliseconds specified or
will use a default if no command-line parameter is given. It will get a
timestamp from the device driver before and after the DosSleep and will
calculate the elapsed time of that sleep and display the results. It will do
this continuously until Ctrl-C or Ctrl-Break is pressed.

Keep in mind that DosSleep has a granularity of 32 milliseconds. Any
discrepency between the number of milliseconds used for the DosSleep and the
elapsed time results from the timer are the fault of this granularity, not a
problem with the timer. DosSleep is used solely as a convenient method of
displaying the capabilities of the driver.

*******************************************************************************
IMPORTANT NOTE: There is a known bug with running HRTIMER.SYS while running
a full-screen Windows application as WinOS2 hooks the same timer that is used
by the device driver. I have not seen the bug myself but it was pointed out
to me. It is recommended therefore that if you use full-screen Windows apps
that you rem out the device statement in your config.sys when not using the
timer. The bug does not exist with seamless Windows apps.
*******************************************************************************

This package is distributed on an as-is basis solely for the benefit of other
developers either in need of a high-resolution timer or some sample device
driver code. The author makes no promises to provide support for the contents.

Any comments can be sent to my CIS id 72251,750.

Hope this proves useful...

Rick Fishman
Code Blazers, Inc.
4113 Apricot
Irvine, CA 92720
