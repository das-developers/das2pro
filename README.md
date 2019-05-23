# das2pro
das2 servers provide data streams typically contaning data relavent to Space Plasma
and Magnetospheric research.  After a GET request is posted, a stream of values 
covering a requested time range at a requested time resolution is provided by the
server.  This software, **das2pro**, provides client for these servers written in
pure [IDL](https://www.harrisgeospatial.com/docs/using_idl_home.html).  To find
out more about das2 visit https://das2.org.

To run examples directly from the git working copy area:
```
$ git clone https://github.com/das-developers/das2pro.git
$ cd das2pro
$ env IDL_PATH="<IDL_DEFAULT>:$(pwd)/src" idl87
IDL> das2pro_ut              ;running unit tests
IDL> ex02_mex_marsis_ais     ;for example
```
Here '$' indicates a shell command, and 'IDL>' indicates an IDL command.

**das2pro** is an IDL package and may be installed using the (
[IPM](https://www.harrisgeospatial.com/docs/ipm.html)  command.  The IPM command
only installs github.com releases (does not use clone) and as of May 2019, the
current github releases are  *not yet ready for public use*.  If you would like 
to try them anyway and are running idl version 8.7.1 or above das2pro can be
installed by issuing:

`IDL> ipm, /install, 'https://github.com/das-developers/das2pro'`

in the interpreter.  After installation it's best to test the package by running
the unit test main program as so.:

   `IDL> das2pro_ut`

The following commands will run included example programs:
```
IDL> ex01_cassini_rpws_wfrm
IDL> ex02_mex_marsis_ais
```
Other example may be included as well included as well see your 

`$HOME/.idl/idl/packages/das2pro/examples`
  
directory (or the examples sub directory if cloned) for other programs
that may have been added.

To remove this package is the following:

  `IDL> IPM, /remove, 'das2pro'`

