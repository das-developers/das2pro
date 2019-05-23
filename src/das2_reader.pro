FUNCTION _das2_urlCallback, status, progress, data
;+
; NAME:
;         Url_Callback
; PURPOSE:
;
; INPUTS:
;
; OUTPUTS:
;
; MODIFICATION HISTORY:
;
;
;-
	compile_opt, idl2, HIDDEN

   ; print the info msgs from the url object
   PRINT, status
   ; return 1 to continue, return 0 to cancel
   RETURN, 1
END

FUNCTION _das2_tagExist, struct, tag
  ;+
  ; NAME:
  ;         _das2_tagExist
  ; PURPOSE:
  ;         Check if a tag name exists in a stucture
  ; OUTPUTS:
  ;           byte 1 - true, 0 - false
  ; EXAMPLE:
  ;
  ; MODIFICATION HISTORY:
  ;     Jul. 2018 Written by D. Pisa (IAP CAS Prague) dp@ufa.cas.cz
  ;
  ;-
  compile_opt, idl2, HIDDEN

  ind = where(strcmp(TAG_NAMES(struct),tag, /fold_case))
  if ind NE -1 THEN return, 1b ELSE return, 0b
END

FUNCTION _das2_parseVarSize, type
  ;+
  ; NAME:
  ;       _das2_parseVarSize
  ; PURPOSE:
  ;       check a size of variable in bytes for a given type
  ; INPUTS:
  ;       (STRING) type
  ; OUTPUTS:
  ;       (uint) number of bytes for a given Var type
  ;
  ; EXAMPLE:
  ;       sz = _das2_parseVarSize('little_endian_real4')
  ;
  ; MODIFICATION HISTORY:
  ;     Jul. 2018 Written by D. Pisa (IAP CAS Prague) dp@ufa.cas.cz
  ;-
  compile_opt, idl2, HIDDEN

  b = stregex(type, '[0-9]{1,2}$', /extract)
  if strcmp(b, '') THEN return, 0u ELSE return, uint(b)
END

FUNCTION _das2_setVarType, data, type, dim=dim
  ;+
  ; NAME:
  ;         _das2_setVarType
  ; PURPOSE:
  ;       resort and convert a byte stream to an array of given type and dimension
  ; INPUTS:
  ;       data (optional)
  ;       type (string)
  ; OPTIONAL INPUTS:
  ;       dim (uint)
  ;
  ; OUTPUTS:
  ;       an array of given type
  ; EXAMPLE:
  ;
  ; MODIFICATION HISTORY:
  ;     Jul. 2018 Written by D. Pisa (IAP CAS Prague) dp@ufa.cas.cz
  ;-
  compile_opt, idl2, HIDDEN

      IF NOT KEYWORD_SET(dim) then dim = 1
      data_size = n_elements(data)
      type_size = _das2_parseVarSize(type)
      ;; !! big endian
      IF stregex(type, '^big_', /boolean) THEN bige = 1 ELSE bige = 0
      ; if number of bytes do not match type and dimension then return []
      IF data_size / type_size NE dim THEN begin
        print, 'Bad data conversion while reading data...'
        return, []
      ENDIF

      IF stregex(type, '^ascii', /boolean) THEN begin
           return, string(reform(data, type_size, dim))
      ENDIF

      IF stregex(type, 'real4$', /boolean) THEN BEGIN
          temp = reform(data, type_size, dim)
          IF bige THEN return, SWAP_ENDIAN(reform(float(temp, 0, 1, dim))) $
          ELSE return, reform(float(temp, 0, 1, dim))
      ENDIF

      IF strcmp(type, 'ascii12') THEN begin
        temp = strsplit(string(data), /extract)
        if n_elements(temp) ne dim then stop
        return, float(temp)
      ENDIF

      IF strcmp(type, 'time22') THEN begin
        temp = string(data)
        return, temp
      ENDIF

      IF stregex(type, 'real8$', /boolean) THEN BEGIN
          temp = reform(data, type_size, dim)
          IF bige THEN return, SWAP_ENDIAN(reform(double(temp, 0, 1, dim))) $
          ELSE return, reform(double(temp, 0, 1, dim))
      ENDIF
      ; never should reach this
      return, []
END

FUNCTION _das2_parsePackets, pks, renderer_waveform
  ;+
  ; NAME:
  ;         _das2_parsePackets
  ; PURPOSE:
  ;
  ; INPUTS:
  ;
  ; OUTPUTS:
  ;
  ; RESTRICTIONS:
  ;
  ; EXAMPLE:
  ;
  ; MODIFICATION HISTORY:
  ;     Jul. 2018 Written by D. Pisa (IAP CAS Prague) dp@ufa.cas.cz
  ;     Nov. 2018 DP, fixed object->struct conversion for ypackets
  ;-
  compile_opt, idl2, HIDDEN
        ptr_stream = 0l ; byte pointer in the stream
        ptr_packet = 0l ; number of packets
        stream_lenght = n_elements(pks) ; number of bytes in the stream
        while ptr_stream lt stream_lenght DO BEGIN ; loop across the stream
              ; if packet does not start with [(0-9){2}] then stop
              IF strmatch(string(pks[ptr_stream:ptr_stream+3]), '\[??\]') THEN ptr_stream += 4 ELSE BEGIN
                print, 'Error encountered while parsing data packtes. Expecting packet id [01] - [99]. Got: '+ string(pks[ptr_stream:ptr_stream+3])
                stop
                if KEYWORD_SET(d) THEN BEGIN
                  print, 'Partial results returned'
                  return, d
                ENDIF ELSE return, !null
              ENDELSE
              ; number of byte for a packet header
              packetHeaderSize = long(string(pks[ptr_stream:ptr_stream+5]))
              ; shift a stream pointer
              ptr_stream += 6
              ; parse a packet header form xml to struct
              packetHeader = (xml_parse(string(pks[ptr_stream:ptr_stream+packetHeaderSize-1])))->ToStruct(/recursive)
              ; shift a stream pointer
              ptr_stream += packetHeaderSize
              ; reset data pointer
              ptr_data = 0L
              ; loop across a stream
              xdata = LIST()
              ydata = LIST()
              zdata = LIST()
              WHILE ptr_stream lt stream_lenght DO BEGIN
                ; expecting a binary data starting with a string :??:
                ; if a semicolom is not found possibly a new packet header occurred
                ; or en error in a stream parsing
                IF strcmp(string(pks[ptr_stream]), ':') NE 1 THEN BREAK
                ; shift a stream pointer
                ptr_stream += 4
                ; parse a type of x variable, typically time
                xTypeSize = _das2_parseVarSize(packetHeader.packet.x._type)
                ; !!! this is a tricky part, need to set variable type properly
                xdata.add, _das2_setVarType(pks[ptr_stream:ptr_stream+xTypeSize-1], packetHeader.packet.x._type)
                ; shift a stream pointer
                ptr_stream += xTypeSize
                ; set a number of items stored in a packet, yscan
                IF _das2_tagExist(packetHeader.packet, 'yscan') THEN begin
                  yscan = packetHeader.packet.yscan
                ENDIF ELSE begin
                  yscan = packetHeader.packet.y
                ENDELSE
                ; set yscan to LIST, this is a hook because the packet scheme
                IF SIZE(yscan, /type) NE 11 THEN yscan = LIST(yscan)
                FOR j=0, yscan.Count()-1 DO BEGIN
                  yscan_struct = yscan[j];.ToStruct()
                  IF SIZE(yscan_struct, /type) EQ 11 THEN yscan_struct = yscan_struct->ToStruct()
                  IF _das2_tagExist(yscan_struct, '_nitems') THEN BEGIN
                    nitems = yscan_struct._nitems
                  ENDIF ELSE begin
                    nitems = 1
                  ENDELSE
                  yTypeSize = _das2_parseVarSize(yscan_struct._type)
                ; !! NEED to be updated
                ; read a whole chunk of data as a byte vector
                if ptr_packet eq 2 and ptr_data eq 28 then stop
                  z = _das2_setVarType(pks[ptr_stream:ptr_stream+(yTypeSize*nitems)-1], yscan_struct._type, $
                    dim=nitems)
                    IF not KEYWORD_SET(z) THEN begin
                        return, !NULL
                      ENDIF
                ; shift a stream pointer
                  IF ptr_data EQ 0 THEN BEGIN
                        zdata.add, LIST(z)
                    ENDIF ELSE begin
                        zdata[j].add, z
                    ENDELSE
                  ptr_stream += yTypeSize * nitems
              ;    IF j EQ 0 THEN tempz ELSE list_tempz.add, tempz
                ENDFOR
                ptr_data += 1
            ENDWHILE
            IF n_elements(xdata) NE 0 THEN BEGIN
              x = reform(xdata->ToArray())
              yscan = yscan[0]
              IF renderer_waveform EQ 1 THEN begin
                 y = findgen(yscan._nitems) * yscan._YTAGINTERVAL
              ENDIF ELSE BEGIN
                IF  _das2_tagExist(packetHeader.packet, 'yscan') THEN BEGIN
                  IF _das2_tagExist(yscan, '_ytags') THEN begin
                    y = float(strsplit(yscan[0]._ytags, ',', /extract))
                  ENDIF ELSE BEGIN
                    y = yscan._ytagmin + findgen(yscan._nitems) * yscan._YTAGINTERVAL
                  ENDELSE
                ENDIF
              ENDELSE

                ;IF not KEYWORD_SET(x) THEN x = -1 ;ELSE x = reform(xdata->ToArray())
                IF not KEYWORD_SET(y) THEN y = -1 ;ELSE y = reform(ydata->ToArray())
                IF not KEYWORD_SET(z) THEN z = -1 ELSE z = reform(zdata->ToArray())

              IF ptr_packet EQ 0 THEN d = LIST(CREATE_STRUCT('xdata', x, 'ydata', y, 'zdata', z, packetHeader)) ELSE BEGIN
                d.ADD, CREATE_STRUCT('xdata', x, 'ydata', y, 'zdata', z, packetHeader)
              ENDELSE
            ENDIF ELSE begin
              IF ptr_packet EQ 0 THEN d = LIST(packetHeader) ELSE BEGIN
                d.ADD, packetHeader
              ENDELSE
            ENDELSE
            ptr_packet +=1
        endwhile
    return, d
END

FUNCTION das2_reader, dataSet, stime, ftime, interval=interval, resolution=resolution, params=params, ascii=ascii, extras=extras, verbose=verbose
  ;+
  ; NAME:
  ;           das2reader
  ; PURPOSE:
  ;
  ; INPUTS:
  ;
  ; OPTIONAL INPUTS:
  ;
  ; KEYWORD PARAMETERS:
  ;
  ; OUTPUTS:
  ;
  ; RESTRICTIONS:
  ;           xml_parse: introduced in version 8.6.1
  ;           IDLnetURL: introduced in version 6.4
  ; PROCEDURE:
  ;
  ; EXAMPLE:
  ;
  ; MODIFICATION HISTORY:
  ;     Jul. 2018 Written by D. Pisa (IAP CAS Prague) dp@ufa.cas.cz
  ;
  ;-
  compile_opt IDL2

    ; catch exceptions
    ;CATCH, errorStatus
    errorStatus = 0

;    IF float(!version.release) LT 8.6 THEN begin
;        print, 'Das2reader does not support earlier IDL version than 8.6!'
;        return, !null
;    ENDIF

    ; Test a number of input parameters, if < 3 printout help
    IF N_PARAMS() LT 3 THEN begin
        PRINT, 'No parameters'
        RETURN, !NULL
    ENDIF

    IF KEYWORD_SET(VERBOSE) THEN VERBOSE = 1 ELSE VERBOSE = 0


    url_host = 'http://planet.physics.uiowa.edu'
    url_path = '/das/das2Server'
    url_path += '?server=dataset&'
    url_path += 'dataset='+dataSet
    IF KEYWORD_SET(interval) THEN url_path += '&interval='+string(interval)
    IF KEYWORD_SET(resolution) THEN url_path += '&resolution='+strtrim(string(resolution), 2) ELSE url_path += '&resolution=0'
    IF KEYWORD_SET(params) THEN url_path += '&params=' + IDLnetURL.URLEncode(STRJOIN('--'+params+' ', /SINGLE))
    IF KEYWORD_SET(ascii) THEN url_path += '&ascii=1'
    url_path += '&start_time=' + stime
    url_path += '&end_time=' + ftime

  ;  url_path = 'http://planet.physics.uiowa.edu/das/das2Server?server=dataset&params=10khz&dataset=Cassini/RPWS/HiRes_MidFreq_Waveform&start_time=2008-08-10T09:06:00.000Z&end_time=2008-08-10T09:13:00.000Z'
  ;  url_path = 'http://planet.physics.uiowa.edu/das/das2Server?server=dataset&dataset=Cassini/RPWS/HiRes_HiFreq_Spectra&start_time=2008-08-10T09:00:00.000Z&end_time=2008-08-10T10:00:00.000Z'
    IF (errorStatus NE 0) THEN BEGIN
      CATCH, /CANCEL
    ; Display the error msg in a dialog and in the IDL output log
    r = DIALOG_MESSAGE(!ERROR_STATE.msg, TITLE='URL Error', /ERROR)
    IF VERBOSE THEN PRINT, !ERROR_STATE.msg
    ; Get the properties that will tell us more about the error.
    oUrl->GetProperty, RESPONSE_CODE=rspCode, $
       RESPONSE_HEADER=rspHdr, RESPONSE_FILENAME=rspFn
    IF VERBOSE THEN BEGIN
      PRINT, 'rspCode = ', rspCode
      PRINT, 'rspHdr= ', rspHdr
      PRINT, 'rspFn= ', rspFn
    ENDIF
    ; Destroy the url object
    OBJ_DESTROY, oUrl

    RETURN, !null
    ENDIF

    oUrl = OBJ_NEW('IDLnetURL')

    IF VERBOSE THEN BEGIN
      oUrl->SetProperty, CALLBACK_FUNCTION='_das2_urlCallback'
      oUrl->SetProperty, VERBOSE=1
    ENDIF

    IF KEYWORD_SET(extras) THEN begin
        oUrl->SetProperty, URL_USERNAME=extras.USERNAME
        oUrl->SetProperty, URL_PASSWORD=extras.PASSWORD
        oUrl->setProperty, URL_PORT=extras.port
    ENDIF

    buffer = oUrl->get(URL=url_host + url_path, /buffer)
    ;save, buffer, file='buffer_wbr.sav'
    ;restore, 'buffer_wbr.sav', /v
    OBJ_DESTROY, oUrl
    streamSize = n_elements(buffer)
    IF strcmp(string(buffer[0:3]), '[00]') NE 1 AND strcmp(string(buffer[0:3]), '[xx]') NE 1 THEN begin
        print, 'Invalid Das2 stream! Expected [00]. Got: '+string(buffer[0:3])
        return, !null
    ENDIF
    streamHeaderSize = long(string(buffer[4:9])) ; fixed lenght for stream header size
    streamHeader = (xml_parse(string(buffer[10:10+streamHeaderSize-1])))->ToStruct(/recursive)
    IF _das2_tagExist(streamHeader, 'stream') THEN BEGIN
      stream = LIST(streamHeader)
      ptrStream = 10 + streamHeaderSize
      IF ptrStream eq n_elements(buffer) THEN return, stream
      IF _das2_tagExist(streamHeader.stream.properties, '_string_renderer') THEN BEGIN
         IF strcmp(streamHeader.stream.properties._string_renderer, 'waveform') THEN waveform = 1 $
         ELSE waveform = 0
       ENDIF ELSE waveform = 0
      dataSet = _das2_parsePackets(buffer[ptrStream:*], waveform)
      IF KEYWORD_SET(dataSet) THEN RETURN, stream + dataSet ELSE RETURN, stream
    ENDIF ELSE begin
      RETURN, string(buffer)
    ENDELSE
  END
