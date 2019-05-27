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
; Create an approprate dataset object by inspecting stream and packet
; headers structures
;
; :Private:
;
; :Returns:
;    A das2ds object
;+
function das2_ds_from_hdrs, dStreamHdr, dPktHdr
	ds = das2ds()
	
	dPkt = pPktHdr['packet']  ; only element
	
	; Setting up dimensions
	;
	; <x>  -> X centers
	;
	; When: <stream><properties>renderer=waveform  (Just...wow)
	; <yscan> yTags, yInterval, yMin -> X offsets
	
	; Handling the Y axis...
	; <y> -> Y center
	; <yscan> yTags, yInterval, yMin -> Y offsets

	; Handling the Z axis...
	; <z> -> Z center
	; <yscan> -> Z center
	
	
	; X
	bXOffset = !false
	if dStreamHdr.haskey('properties') then begin
		d = dStreamHdr['properties']
		if d.haskey('renderer') then bXOffset = (d['renderer'] eq 'waveform')
	endif
	
	; Watch out. If stream contains <x><x> then the 'x' body is a list not a hash
	if dPkt.haskey('x'):
		
		
		
		
	
	
	if dStreamHdr.haskey('properties') then begin
	
		; there's probably a better hash key iteration idiom than this in IDL
		d = dStreamHdr['properties']
		k = d.keys()
		for i= 0,n_elements(k) - 1 do begin
			sType = 'string'
	   	sKey = k[i]
			sVal = d[k[i]]
			
			; this is dumb, das2 streams need to follow proper XML rules -cwp
			lTmp = strsplit(sKey, ':')
			if n_elements(lTmp) > 1 then begin
				sType = lTmp[0]
				sKey = lTmp[1]
			endif
			
			; For stuff that's not tagged as part of an axis, just add it's
			; properties to the top level dataset
			sAx = sKey.charat(0) 
			if (sAx ne 'x') and (sAx ne 'y') and (sAx ne 'z') then &
				ds.props[sKey] = das2_makeprop(sType,  sVal)
		endfor
	endif

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
; Create a dataset structure from a list of packets.
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

function _das2_parsePackets, hStreamHdr, buffer, debug=debug, messages=messages
                             
   compile_opt idl2, hidden
   
	; A hash map of packet IDs to dataset objects.  Holds the current dataset
	; map.  If a packet ID is redefined a new dataset is placed here.
	dDatasets = hash()
	
	; All the datasets read from the stream, not just the curent definitions.
	lAllDs = list()  
	
	messages = ''
	
   iBuf = 0l       ; byte offset in the stream
   ptr_packet = 0l ; number of packets
   nBufSz = n_elements(buffer) ; number of bytes in the stream
   
   while iBuf lt nBufSz do begin ; loop across the stream
      
		sTag = string(buffer[iBuf:iBuf+3])
		
		if sTag.charat(0) eq '[' then begin
			; New packet header, maybe dataset or comment
			
			
			if (sTag.substring(1,2)).tolower() eq 'xx' then begin
			
				; it's a comment packet
				if not _das2_handleComment(buffer, iBuf, messages) then return !null
				
			endif else begin
			
				; it's a dataset
				nPktId = fix(sTag.substring(1,2))
				
				if not _das2_handleDsDef(buffer, iBuf, messages, dataset) then return !null
				
				; If they have re-defined a dataset that has already been defined
				; which this ID then just skip it.
				
				if nPktId not in dDatasets.keys() then begin
					dDatsets[nPktId] = dataset
					lAllDs.add(dataset)
				else endif 
				
			endelse
		endif else begin
		
			if sTag.charat(0) eq ':' then begin
				
				; A data packet	
				nPktId = fix(sTag.substring(1,2))
				dataset = dDatasets[nPktId]
				
				if not _das2_handleData(buffer, iBuf, messages, dataset) then return !null
				
			endif else begin
				; Illegal packet start character
				messages = "Illegal character "+sTag.charat(0)+"in stream at position"+string(iBuf)
				return !null
			endelse
		endelse
	endwhile
	
	return lAllDs
end

	
		
      sPktId = string(pks[iBuf+1:iBuf+2])
      
      ; if packet does not start with [(0-9){2}] then stop
      if strmatch(string(pks[iBuf:iBuf+3]), '\[??\]') then begin
         iBuf += 4
      endif else begin
         printf, -2, 'ERROR: In packet stream. Expecting packet id '+ $
                    '[01] - [99], or [XX]. Got: '+ string(pks[iBuf:iBuf+3])
         stop
         if keyword_set(debug) then begin
            printf, -2, 'WARNING: Partial results returned'
            return, debug
         endif else return, !null
      endelse
      
      ; number of bytes for a packet header
      pktHdrSz = long(string(pks[iBuf:iBuf+5]))
      iBuf += 6   ; shift a stream pointer

      ; parse a packet header form xml to struct
      hPktHdr = xml_parse(string(pks[iBuf:iBuf+pktHdrSz-1]))
      hPktHdr = hPktHdr.ToStruct(/recursive)
      
      iBuf += pktHdrSz ; shift a stream pointer

      ptr_data = 0L  ; reset data pointer
      
      ; A comment packet send to the message variable.  Need to implement
		; throwing away progress messages so they don't obsure important error
		; messages.
      if strcmp('xx', sPktId, /fold_case) then begin
			messages += string(tPktHdr)
			continue
		endif 
      
		; Looks like we have a data packet HDR, create a dateset object to store
		; it's values, save off any needed header info
		ds = das2_ds_from_hdrs(hStreamHdr, hPkthdr)
		
      ; loop across a stream
      xdata = list()
      ydata = list()
      zdata = list()
      
      while iBuf lt stream_length do begin
         ; expecting a binary data starting with a string :??:
         ; if a semicolom is not found possibly a new packet header occurred
         ; or en error in a stream parsing
         if strcmp(string(pks[iBuf]), ':') NE 1 then break
         
         ; shift a stream pointer
         iBuf += 4
			
			
			
			
			; END of test ideas
			
         ; parse a type of x variable, typically time
         xTypeSize = _das2_typeSize(pktHdr.packet.x._type)
         if keyword_set(debug) then printf, -2, xTypeSize, format='DEBUG: xTypeSize = %s'
         
         ; !!! this is a tricky part, need to set variable type properly
         xdata.add, _das2_decodeValues(pks[iBuf:iBuf+xTypeSize-1], pktHdr.packet.x._type)
 
         iBuf += xTypeSize ; shift a stream pointer
         
         ; set a number of items stored in a packet, yscan
         if _das2_tagExist(pktHdr.packet, 'yscan') then begin
            yscan = pktHdr.packet.yscan
         endif else begin
            yscan = pktHdr.packet.y
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
               pks[iBuf:iBuf+(yTypeSize*nitems)-1], yscan_struct._type, $
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
           iBuf += yTypeSize * nitems
           
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
            if  _das2_tagExist(pktHdr.packet, 'yscan') then begin
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
            d = list(create_struct('xdata', x, 'ydata', y, 'zdata', z, tPktHdr)) 
         endif else begin
            d.add, create_struct('xdata', x, 'ydata', y, 'zdata', z, tPktHdr)
         endelse
      
      endif else begin
         if ptr_packet eq 0 then begin
            d = list(tPktHdr) 
         endif else begin
            d.add, tPktHdr
         endelse
         
      endelse
      
      ptr_packet +=1
   endwhile
   return, d
end


;+
; Parse the contents of a byte buffer into a list of das2 dataset (das2ds) 
; objects.  This is an all-at-once parser that can't handle datasets larger
; than around 1/2 to 1/3 of the host machine's ram. 
;
; :Returns:
;    list - a list of das2 dataset (das2ds) objects.
;-
function das2_parsestream, buffer, MESSAGES=messages

	messages = ""
	
   nStreamSz = n_elements(buffer)
   if strcmp(string(buffer[0:3]), '[00]') NE 1 AND $
      strcmp(string(buffer[0:3]), '[xx]') NE 1 then begin
       printf, -2, 'ERROR: Invalid das2 stream! Does not start with [00]. Got: '+string(buffer[0:3])
       return, !null
   endif
   nStreamHdrSz = long(string(buffer[4:9])) ; fixed length for stream header size
	hStreamHdr = xml_parse(string(buffer[10:10+nStreamHdrSz-1]))
   tStreamHdr = hStreamHdr.ToStruct(/recursive)
   
   if _das2_tagExist(tStreamHdr, 'stream') then begin
      ptrStream = 10 + nStreamHdrSz
      
      ; No packets in the stream, just a header.  This is format error
      ; as there should at least be a [XX] packet (comment) that says 
      ; "NoDataInInterval" for a properly behaved stream.
      if ptrStream eq n_elements(buffer) then begin
			messages="Stream contains no packets, not even an exception message"
			return, !null
		endif
      
      ; Bad hack for treating yOffset (yTags) as xOffset values.  Should be
		; denoted with an explicit attribute name, should update this for das2.4
      if _das2_tagExist(tStreamHdr.stream.properties, '_string_renderer') then begin
         if strcmp(tStreamHdr.stream.properties._string_renderer, 'waveform') $
			then waveform = !true else waveform = !false
      endif else waveform = !false
      
      if keyword_set(debug) then begin
         lDataSet = _das2_parsePackets(
				hStreamHdr, buffer[ptrStream:*], waveform, debug=debug, messages=messages
			)
      endif else begin
         lDataSet = _das2_parsePackets(
				hStreamHdr, buffer[ptrStream:*], waveform, messages=messages
			)
      endelse
      
      return, lDataSet 
      
   endif else begin
		messages = string(buffer)
	   return, !null
   endelse
	 
end

