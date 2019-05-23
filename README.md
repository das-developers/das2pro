# das2pro
Pure IDL das2 client package

Under Development, *DOES NOT WORK YET*, *DON'T USE THIS YET*!

Instructions that will work later are given below...


To install this package using IDL 8.7.1 or above:

   `IDL> ipm, /install, 'https://github.com/das-developers/das2pro'`

To run unit tests after installation:

   `IDL> das2pro_ut`

To run example programs:
```
   IDL> ex01_cassini_rpws_wfrm
   IDL> ex02_mex_marsis_ais
```

Other example are included as well see your 

  `$HOME/.idl/idl/packages/das2pro/examples`
  
directory for other $MAIN$ programs.

To remove this package:

  `IDL> IPM, /remove, 'das2pro'`

To run examples directly from the git working copy area:

```
   $ git clone
   $ cd das2pro
   $ env IDL_PATH="<IDL_DEFAULT>:$(pwd)/src" idl87
   IDL> das2pro_ut              ;running unit tests
   IDL> ex02_mex_marsis_ais     ;for example
```

