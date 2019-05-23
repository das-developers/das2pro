# das2pro
Das2 servers typically provide data relavent to space plasma and magnetospheric
physics research.  To retrieve data, an HTTP GET request is posted to a das2 
server by a client program and a self-describing stream of data values covering
the requested time range, at the requested time resolution, is provided in the
response body.

This software, **das2pro**, provides a client library for das2 servers written in
pure [IDL](https://www.harrisgeospatial.com/docs/using_idl_home.html).  To find
out more about das2 visit https://das2.org.

To download this package and run the included examples directly from your git
working copy area issue:
```
$ git clone https://github.com/das-developers/das2pro.git
$ cd das2pro
$ env IDL_PATH="<IDL_DEFAULT>:$(pwd)/src" idl87
IDL> das2pro_ut              ;running unit tests
IDL> ex02_mex_marsis_ais     ;for example
```
Here '$' indicates a shell command, and 'IDL>' indicates an IDL command.

**das2pro** is an IDL package and may be installed using the 
[IPM](https://www.harrisgeospatial.com/docs/ipm.html)  command.  The IPM command
only installs github.com releases (does not use clone) and as of May 2019, the
current github releases are  *not yet ready for public use*.  If you would like 
to try them anyway and are running idl version 8.7.1 or above, das2pro can be
installed by issuing the single IDL command:

`IDL> ipm, /install, 'https://github.com/das-developers/das2pro'`

After installation it's best to test the package by running the unit test main
program as so:

`IDL> das2pro_ut`

and use the following to run one or more of the included example programs:

```
IDL> ex01_cassini_rpws_wfrm
IDL> ex02_mex_marsis_ais
```
which generate output PNG images into your current directory.

Other examples may be included as well.  If das2pro has been installed as a
package, See your 

`$HOME/.idl/idl/packages/das2pro/examples`

directory for other example programs that may have been added.  If das2pro 
was aquired via git clone, just look in the examples sub-directory.

To remove this package use the IPM command:

  `IDL> IPM, /remove, 'das2pro'`

