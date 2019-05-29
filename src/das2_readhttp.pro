; The MIT License
;
; Copyright 2018-2019 David Pisa (IAP CAS Prague) dp@ufa.cas.cz
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
; copies of the Software, and to permit persons to whom the Software is 
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in 
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.


; docstrings formatted according to the guidlines at:
; https://www.harrisgeospatial.com/docs/IDLdoc_Comment_Tags.html 

;+
; Provide feedback that the stream is downloading
;
; :Private:
;-
function _das2_urlCallback, status, progress, data
   compile_opt idl2, hidden
   
   print, status    ; print the info msgs from the url object
   return, 1        ; return 1 to continue, return 0 to cancel
end


;+
; Request data from a specific das2 server using native HTTP GET parameters
; 
; :Params:
;    sServer:  in, required, type=string
;    sDataset: in, required, type=string
;    stime: in, required, type=string
;    ftime: in, required, type=string
;
; :Keywords:
;    params: in, optional, type=list
;    ascii: in, optional, type=boolean
;    extras: in, optional, type=list
;    verbose: in, optional, type=boolean
;
; :Returns:
;    A list of dataset objects.  Each dataset object corresponds to a single
;    packet type in the input.
;
; :Requires:
;    xml_parse: IDL 8.6.1
;    IDLnetURL: IDL 6.4
;
; :History:
;    Jul. 2018, D. Pisa : original
;    May  2019, C. Piker: refactored
;-
function das2_httpget, sServer, sDataset, stime, ftime, $
   interval=interval, resolution=resolution, params=params, ascii=ascii, $
   extras=extras, verbose=verbose, debug=debug, messages=messages
   
   compile_opt idl2

   ; catch exceptions
   ;CATCH, errorStatus
   errorStatus = 0

;  if float(!version.release) LT 8.6 then begin
;     print, 'Das2reader does not support earlier IDL version than 8.6!'
;     return, !null
;  endif

   ; Test a number of input parameters, if < 3 printout help
   if n_params() lt 3 then begin
      printf, -2, 'ASSERT: No parameters'
      return, !NULL
   endif

   if keyword_set(VERBOSE) then VERBOSE = 1 else VERBOSE = 0

   url_host = sServer
   url_path = '?server=dataset&'
   url_path += 'dataset='+sDataset
   if keyword_set(interval) then url_path += '&interval='+string(interval)
   if keyword_set(resolution) then begin 
      if (resolution > 0.0) then url_path += '&resolution='+strtrim(string(resolution), 2)
   endif
        
   if keyword_set(params) then url_path += '&params=' + IDLnetURL.URLEncode(STRJOIN('--'+params+' ', /SINGLE))
   if keyword_set(ascii) then url_path += '&ascii=1'
   url_path += '&start_time=' + stime
   url_path += '&end_time=' + ftime

   ; url_path = 'http://planet.physics.uiowa.edu/das/das2Server?server=dataset&params=10khz&dataset=Cassini/RPWS/HiRes_MidFreq_Waveform&start_time=2008-08-10T09:06:00.000Z&end_time=2008-08-10T09:13:00.000Z'
   ; url_path = 'http://planet.physics.uiowa.edu/das/das2Server?server=dataset&dataset=Cassini/RPWS/HiRes_HiFreq_Spectra&start_time=2008-08-10T09:00:00.000Z&end_time=2008-08-10T10:00:00.000Z'
   if (errorStatus NE 0) then begin
      CATCH, /CANCEL
      ; Display the error msg in a dialog and in the IDL output log
      r = DIALOG_MESSAGE(!ERROR_STATE.msg, TITLE='URL Error', /ERROR)
      if VERBOSE then print, !ERROR_STATE.msg
      ; Get the properties that will tell us more about the error.
      oUrl.GetProperty, RESPONSE_CODE=rspCode, $
      RESPONSE_HEADER=rspHdr, RESPONSE_FILENAME=rspFn
      if VERBOSE then begin
         print, 'rspCode = ', rspCode
         print, 'rspHdr= ', rspHdr
         print, 'rspFn= ', rspFn
      endif
      ; Destroy the url object
      OBJ_DESTROY, oUrl

      return, !null
   endif

   oUrl = IDLnetURL()  ; object creation

   if VERBOSE then begin
     oUrl.SetProperty, CALLBACK_FUNCTION='_das2_urlCallback'
     oUrl.SetProperty, VERBOSE=1
   endif

   if keyword_set(extras) then begin
       oUrl.SetProperty, URL_USERNAME=extras.USERNAME
       oUrl.SetProperty, URL_PASSWORD=extras.PASSWORD
       oUrl.SetProperty, URL_PORT=extras.port
   endif

   sUrl = url_host + url_path
   printf, -2, "INFO: Requesting "+sUrl
   
   ; Should maybe change this so to callback processing so the whole stream is
   ; not buffered in memory twice but is put in data arrays as read. -cwp
   buffer = oUrl.get(URL=sUrl, /buffer)
   
   ;save, buffer, file='buffer_wbr.sav'
   ;restore, 'buffer_wbr.sav', /v
   obj_destroy, oUrl
	
   return das2_parsestream(buffer, /messages=messages)
	
end
