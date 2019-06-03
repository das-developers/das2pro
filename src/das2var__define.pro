; The MIT License
;
; Copyright 2019 Chris Piker
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

function das2var::init, _EXTRA=ex
	compile_opt idl2
	void = self.IDL_Object::init()
	
	; default values
	self.units = ''
	self.values = ptr_new()
	self.parser = obj_new()
	self.idxmap = ptr_new()
	
	if(isa(ex)) then self.setproperty, _EXTRA=ex
	return, !TRUE
end

pro das2var::getproperty, $
	UNITS=units, VALUES=values, IDXMAP=idxmap, PARSER=parser
	compile_opt idl2
	if arg_present(units) then units = self.units
	if arg_present(values) then values = self.values
	if arg_present(idxmap) then idxmap = *(self.idxmap)
	if arg_present(parser) then parser = self.parser
end

pro das2var::setproperty, $
	UNITS=units, VALUES=values, IDXMAP=idxmap, PARSER=parser
	compile_opt idl2
	
	if isa(units) then self.units = units 
	if isa(values) then self.values = ptr_new(values, /no_copy) 
	if isa(parser) then self.parser = parser 
	
	; Maybe add checking code here to make sure that the index map 
	; has no more non-zero entries than the values array?  I don't
	; want this function to be annoying to code that sets the index
	; before the array is set, so let it go for now.
	if isa(idxmap) then begin
		self.idxmap = ptr_new(idxmap) ; copies internally by default
	endif
end

;+
; Provide index ranges for this variable in terms of the top level dataset
; index space.
;
; Cordinate variables often have lower dimensional arrays than the data
; variables.  For example, think of an insturment that collects energetic
; partical hit-counts in 45 energy bands once per minute.  The coordinates of 
; this dataset would be energy and time, with the data being the count rate.
; An hour's worth of these measurements could be stored in the following
; arrays:
; 
; ```
;    aTime    = long64arr(60)
;    aEnergy  = fltarr(45)
;    aCounts  = intarr(45, 60)
; ```
;
; Looking at the index ranges for this simple dataeset it's apparent that the
; first index of array aTime must correspond to the second index of array 
; aCounts.  To help visualize this mapping, especially when datasets become
; more complex, we could "line-up" all the index ranges to make the mapping
; more explicit:
;
; ```
; Values   Extents
; ------   -------
; Time     [ - , 60]
; Energy   [ 45, - ]
; Counts   [ 45, 60]
; ```
;
; This is what the idxmap function outputs, the mapping of index space of a
; single array to the overall dataset index space.  Assume now that these
; arrays are actually the `.values` member of three different das2var objects.
; Calling `.dshape()` would yield the following (without comments of course):
;
; ```
; print, vTime.dshape() 
;   -1      0      ; var is degenerate in first dataset index (0 values)
;    0     60      ; second dataset index maps to first var index (60 values)
;
; print, vEnergy.dshape()
;    0     45      ; first dataset index maps to first var index (45 values)
;   -1      0      ; var is degenerate in the second dataset index (0 values)
;
; print, vCounts.dshape()
;    0     45      ; first dataset index maps to first var index (45 values)
;    1     60      ; second dataset index maps to second var index (60 values)
; ```
;
; :Returns:
;   A 2 by N array, where N is the number of independent indexes required to
;   correlate all values in a dataset.
;
; :Author: Chris Piker
;-
function das2var::dshape
	
	idxmap = *(self.idxmap)
	
	nIdxDims = n_elements(idxmap)
	aShape = intarr(2, nIdxDims)
	for i =0, nIdxDims - 1 do begin
		aShape[0, i] = idxmap[i]
		if idxmap[i] gt -1 then begin
			arydims = size(*(self.values), /DIMENSIONS)
			;printf, -2, 'val', *(self.values), 'array dims', arydims
			aShape[1, i] =  arydims[ idxmap[i] ]
		endif else aShape[1,i] = 0.0
	endfor
	
	return, aShape
end

; Overloading this properly will involve array broadcasting, skip for now.
;function das2var::_overloadBracketsRightSide, $
;	isrange, sub1, sub2, sub3, sub4, sub5, sub6, sub7, sub8 
;	
;	if n_elements(isrange) gt n_elements(self.idxmap) then begin
;		;todo: improve error message
;		message, 'Number of index dimensions, exceeds dataset dimensions in array index operation'
;	endif
;	
;	if n_elements(isrange) eq 0 then return, !null 
;	
;	case n_elements(isrange) of
;		1: aDsIdx = [sub1]
;		2: aDsIdx = [sub1, sub2]
;		3: aDsIdx = [sub1, sub2, sub3]
;		4: aDsIdx = [sub1, sub2, sub3, sub4]
;		5: aDsIdx = [sub1, sub2, sub3, sub4, sub5]
;		6: aDsIdx = [sub1, sub2, sub3, sub4, sub5, sub6]
;		7: aDsIdx = [sub1, sub2, sub3, sub4, sub5, sub6, sub7]
;		8: aDsIdx = [sub1, sub2, sub3, sub4, sub5, sub6, sub7, sub8]
;	else: message, 'Syntax error: NULL array index'
;	endcase
;	
;	lVarIdx = make
;	
;	for i = 0, n_elements(aDsIdx) - 1 do begin
;	
;	
;	endfor
;	
;	return, !null
;end


;+
; Das2 Variable, an array, it's units and it's index map.
;-
pro das2var__define
	compile_opt idl2, hidden
	void = { $
		das2var, inherits IDL_Object, units:'', values:ptr_new(), $
		parser:obj_new(), idxmap: ptr_new() $
	}
end
