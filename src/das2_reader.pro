;+
; Acquiring and parsing a das2 stream into a dataset list
;
; :Author:
;    David Pisa (IAP CAS Prague) dp@ufa.cas.cz
;
; :Copyright:
;    2018 - 2019 David Pisa
;
; :License:
;    MIT
;-

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


; Check if a tag name exists in a stucture
;
; :Private:
;
; :Returns:
;    byte 1 - true, 0 - false
;
; :History:
;    Jul. 2018, D. Pisa : original
;-
function _das2_tagExist, struct, tag
  compile_opt idl2, hidden

  ind = where(strcmp(tag_names(struct),tag, /fold_case))
  if ind ne -1 then return, 1b else return, 0b
end


;+
; Turn strings such as 'little_endian_real4' into the number of bytes required
; to store a single data value for a stream variable.
;
; :Private:
;
; :Params:
;    encoding : in, required, type=string
;
; :Returns:
;    unit : The number of bytes for a given type
;
; :Example:
;    sz = _das2_typeSize('little_endian_real4')
;
; :History:
;    Jul. 2018 D. Pisa : original
;-
function _das2_typeSize, encoding
  compile_opt idl2, hidden

  b = stregex(encoding, '[0-9]{1,2}$', /extract)
  if strcmp(b, '') then return, 0u else return, uint(b)
end

;+
; Convert a byte array to an array of given type
;
; :Private:
;
; :Params:
;    data : in, required, type=byte array
;       The binary data to decode
;    sType : in, required, type=string
;       The encoding type for the data.  These are: sun_real4, sun_real8,
;       little_endian4, little_endian8, timeN and asciiN, where N is the
;       number of bytes in the value.
;
; :Keywords:
;    dim : in, optional, type=uint
;        Number if values to decode, defaults to 1 if not specified
;
;
; :Returns:
;    an array of given type
; 
; :History:
;    Jul. 2018, D. Pisa : original
;    May  2019, C. Piker : updates to handle multiple time values per row
;-
function _das2_decodeValues, data, sType, dim=dim, debug=debug
   compile_opt idl2, hidden

   if not keyword_set(dim) then dim = 1
   data_size = n_elements(data)
   type_size = _das2_typeSize(sType)
   ;; !! big endian
   if stregex(sType, '^big_', /boolean) or stregex(sType, '^sun_', /boolean) then bige = 1 else bige = 0
   
   ; if number of bytes do not match type and dimension then return []
   if data_size / type_size NE dim then begin
     message, 'data bytes are not an integer number of ' + sType + ' item widths'
   endif
   
   ; Get array of properly sized byte strings
   aVals = reform(data, type_size, dim)

   if stregex(sType, 'real4$', /boolean) then begin
      if bige then return, swap_endian(reform(float(aVals, 0, 1, dim))) $
      else return, reform(float(aVals, 0, 1, dim))
   endif

   if stregex(sType, 'real8$', /boolean) then begin
      if bige then return, swap_endian(reform(double(aVals, 0, 1, dim))) $
      else return, reform(double(aVals, 0, 1, dim))
   endif

   if strcmp(sType, 'ascii', 5) then begin
     temp = float(string(aVals))
     if n_elements(temp) ne dim then stop
     return, temp
   endif

   if strcmp(sType, 'time', 4) then begin ; no case fold on purpose
     temp = strtrim( string(aVals), 2)    ; remove spaces to clean times
     return, temp
   endif
   
   ; never should reach this
   message, 'can not decode values of type ' + sType
   return, []
end

;+
; Create a dataset structure from a list of packets
;
; :Private:
;
; :Params:
;    pkts : in, required, type=byte array
;       A byte vector containing all the packets in a das2 stream, the
;       header packet (id [00]) should not be included.
;
; :Returns:
;    list of dataset structures
;
; :History:
;     Jul. 2018, D. Pisa : original 
;     Nov. 2018, D. Pisa : fixed object.struct conversion for ypackets
;-

function _das2_parsePackets, pks, renderer_waveform, debug=debug
   compile_opt idl2, hidden
   
   ptr_stream = 0l ; byte offset in the stream
   ptr_packet = 0l ; number of packets
   stream_length = n_elements(pks) ; number of bytes in the stream
   
   while ptr_stream lt stream_length do begin ; loop across the stream
      
      sPktId = string(pks[ptr_stream+1:ptr_stream+2])
      
      ; if packet does not start with [(0-9){2}] then stop
      if strmatch(string(pks[ptr_stream:ptr_stream+3]), '\[??\]') then begin
         ptr_stream += 4
      endif else begin
         printf, -2, 'ERROR: In packet stream. Expecting packet id '+ $
                    '[01] - [99], or [XX]. Got: '+ string(pks[ptr_stream:ptr_stream+3])
         stop
         if keyword_set(debug) then begin
            printf, -2, 'WARNING: Partial results returned'
            return, debug
         endif else return, !null
      endelse
      
      ; number of bytes for a packet header
      packetHeaderSize = long(string(pks[ptr_stream:ptr_stream+5]))
      ptr_stream += 6   ; shift a stream pointer

      ; parse a packet header form xml to struct
      packetHeader = (xml_parse(string(pks[ptr_stream:ptr_stream+packetHeaderSize-1]))).ToStruct(/recursive)
      
      ptr_stream += packetHeaderSize ; shift a stream pointer

      ptr_data = 0L  ; reset data pointer
      
      ; A comment packet, some of these contain messages that should go to the
      ; standard error channel, but just ignore them for now
      if strcmp('xx', sPktId, /fold_case) then continue
      
      ; loop across a stream
      xdata = list()
      ydata = list()
      zdata = list()
      
      while ptr_stream lt stream_length do begin
         ; expecting a binary data starting with a string :??:
         ; if a semicolom is not found possibly a new packet header occurred
         ; or en error in a stream parsing
         if strcmp(string(pks[ptr_stream]), ':') NE 1 then break
         
         ; shift a stream pointer
         ptr_stream += 4
         ; parse a type of x variable, typically time
         xTypeSize = _das2_typeSize(packetHeader.packet.x._type)
         if keyword_set(debug) then printf, -2, xTypeSize, format='DEBUG: xTypeSize = %s'
         
         ; !!! this is a tricky part, need to set variable type properly
         xdata.add, _das2_decodeValues(pks[ptr_stream:ptr_stream+xTypeSize-1], packetHeader.packet.x._type)
 
         ptr_stream += xTypeSize ; shift a stream pointer
         
         ; set a number of items stored in a packet, yscan
         if _das2_tagExist(packetHeader.packet, 'yscan') then begin
            yscan = packetHeader.packet.yscan
         endif else begin
            yscan = packetHeader.packet.y
         endelse
         
         ; set yscan to LIST, this is a hook because the packet scheme
         if size(yscan, /type) ne 11 then yscan = LIST(yscan)
         
         for j=0, yscan.Count()-1 do begin
            yscan_struct = yscan[j];.ToStruct()
            
            if SIZE(yscan_struct, /type) EQ 11 then yscan_struct = yscan_struct.ToStruct()
            if _das2_tagExist(yscan_struct, '_nitems') then begin
                nitems = yscan_struct._nitems
            endif else begin
                nitems = 1
            endelse
            yTypeSize = _das2_typeSize(yscan_struct._type)
            
            ; !! NEEDS to be updated
            ; read a whole chunk of data as a byte vector
            if ptr_packet eq 2 and ptr_data eq 28 then stop
            
            z = _das2_decodeValues( $
               pks[ptr_stream:ptr_stream+(yTypeSize*nitems)-1], yscan_struct._type, $
               dim=nitems $
            )
            
            if not keyword_set(z) then begin
               return, !NULL
            endif
            
           ; shift a stream pointer
           if ptr_data EQ 0 then begin
              zdata.add, LIST(z)
           endif else begin
              zdata[j].add, z
           endelse
           ptr_stream += yTypeSize * nitems
           
           ;if j EQ 0 then tempz else list_tempz.add, tempz
           
         endfor
         ptr_data += 1
      
      endwhile
      
      if n_elements(xdata) NE 0 then begin
         if keyword_set(debug) then print, xdata
         
         x = reform(xdata.ToArray())
         yscan = yscan[0]
         
         if renderer_waveform EQ 1 then begin
            y = findgen(yscan._nitems) * yscan._YTAGINTERVAL
         endif else begin
            if  _das2_tagExist(packetHeader.packet, 'yscan') then begin
               if _das2_tagExist(yscan, '_ytags') then begin
                  y = float(strsplit(yscan[0]._ytags, ',', /extract))
               endif else begin
                  y = yscan._ytagmin + findgen(yscan._nitems) * yscan._YTAGINTERVAL
               endelse
            endif
         endelse

         ;if not keyword_set(x) then x = -1 ;else x = reform(xdata.ToArray())
         if not keyword_set(y) then y = -1 ;else y = reform(ydata.ToArray())
         if not keyword_set(z) then z = -1 else z = reform(zdata.ToArray())

         if ptr_packet eq 0 then begin
            d = list(create_struct('xdata', x, 'ydata', y, 'zdata', z, packetHeader)) 
         endif else begin
            d.add, create_struct('xdata', x, 'ydata', y, 'zdata', z, packetHeader)
         endelse
      
      endif else begin
         if ptr_packet eq 0 then begin
            d = list(packetHeader) 
         endif else begin
            d.add, packetHeader
         endelse
         
      endelse
      
      ptr_packet +=1
   endwhile
   return, d
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
;    A list of structures.  Each structure corresponds to one dataset in the 
;    stream
;
; :Requires:
;    xml_parse: IDL 8.6.1
;    IDLnetURL: IDL 6.4
;
; :History:
;    Jul. 2018, D. Pisa : original
;    May  2019, C. Piker: added server selection parameter
;-
function das2_reader, sServer, sDataset, stime, ftime, $
   interval=interval, resolution=resolution, params=params, ascii=ascii, $
   extras=extras, verbose=verbose, debug=debug
   
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
   
   nStreamSz = n_elements(buffer)
   if strcmp(string(buffer[0:3]), '[00]') NE 1 AND $
      strcmp(string(buffer[0:3]), '[xx]') NE 1 then begin
       printf, -2, 'ERROR: Invalid Das2 stream! Does not start with [00]. Got: '+string(buffer[0:3])
       return, !null
   endif
   nStreamHdrSz = long(string(buffer[4:9])) ; fixed length for stream header size
   streamHeader = (xml_parse(string(buffer[10:10+nStreamHdrSz-1]))).ToStruct(/recursive)
   
   if _das2_tagExist(streamHeader, 'stream') then begin
      lStream = list(streamHeader)
      ptrStream = 10 + nStreamHdrSz
      
      ; No packets in the stream, just a header.  This is format error
      ; as there should at least be a [XX] packet (comment) that says 
      ; "NoDataInInterval" for a properly behaved stream.
      if ptrStream eq n_elements(buffer) then return, lStream
      
      ; Bad hack for treating yTags as xOffset values.  Should be denoted
      ; with an explicit attribute name, should update this for das2.4.
      if _das2_tagExist(streamHeader.stream.properties, '_string_renderer') then begin
         if strcmp(streamHeader.stream.properties._string_renderer, 'waveform') then waveform = !true $
         else waveform = !false
      endif else waveform = !false
      
      if keyword_set(debug) then begin
         lDataSet = _das2_parsePackets(buffer[ptrStream:*], waveform, debug)
      endif else begin
         lDataSet = _das2_parsePackets(buffer[ptrStream:*], waveform)
      endelse
      
      if keyword_set(dataSet) then return, lStream + lDataSet else return, lStream
      
    endif else begin
       ; This looks like an error return case, should probably go in an output
       ; status message variable instead of being the return value. -cwp
       return, string(buffer)
       
    endelse

end
