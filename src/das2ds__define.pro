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

function das2ds::init, _extra=ex
	compile_opt idl2
	
	void = self.IDL_Object::init()
	self.props = hash()
	self.dims = hash()
	return, !true
end

pro das2ds::getproperty, PROPS=props, DIMS=dims
	compile_opt idl2
	if arg_present(props) then props = self.props
	if arg_present(dims) then dims = self.dims
end

;+
; Answers the question, how big are data packets for this dataset.
;
; Datasets can be parsed from das2 streams.  A stream consists of 
; header and data packets.  Each data packet must have a sufficent 
; number of data values to increment the highest index of the internal
; data arrays by 1.
;
; :Returns:
;    The number of total bytes in each data packet for this dataset.
;     
;-
function das2ds::recsize
	
	nRecBytes = 0
	
	aDims = self.dims.keys()
	
	;printf, -2, "DEBUG: Dims in dataset: ", n_elements(nDims)
	
	for d = 0, n_elements(aDims) - 1 do begin
		dim = self.dims[ aDims[d] ]
		
		aVar = dim.vars.keys()
		for v = 0, n_elements(aVar) - 1 do begin
			var = dim.vars[ aVar[v] ]
			
			if var.parser ne obj_new() then begin
				
				; Using IDL convention of end including the last index instead of
				; of being an upper bound.
				nEnd = var.parser.offset() + var.parser.chunksize()
				
				if nEnd gt nRecBytes then nRecBytes = nEnd
			endif 
		endfor 
	endfor
	return, nRecBytes
end

;+
; Inspect all owned variables and get the index range of the
; overall dataset.  Map sizes to array dimensions using the 
; index maps.
;
; :Private:
;-

function das2ds::_idxRangeStr
	compile_opt idl2, hidden

	return, 'todo,das2ds__idxRangeStr'
end

function das2ds::_overloadPrint
	compile_opt idl2, hidden

	nl = string([10B])

	s = self._idxRangeStr()
	n = self.recsize()
	s = string(n, format='Record Size: %d bytes')

	sOut = string(self.name, self.group, s, format='Dataset: ''%s'' from group ''%s'' | %s')
	aKeys = self.props.keys()
	for i = 0, n_elements(aKeys) - 1 do begin
		prop = self.props[aKeys[i]]
		sOut += nl + string(aKeys[i], prop.strval, format='   Property: %s | %s')
	endfor
	
	; TODO: Refactor this.  Dims and Variables should be responsible for
	;       printing themselves instead of doing it all here
	
	aKeys = self.dims.keys()
	
	; TODO: Sort all data dimensions before all coordinate dimensions
	
	for i = 0, n_elements(aKeys) - 1 do begin
		dim = self.dims[aKeys[i]]
		
		sDimName = aKeys[i]
		
		sOut += nl + nl + string(dim.kind, sDimName, format="   %s Dimension: %s")
		
		aSubKeys = dim.props.keys()
		for j = 0, n_elements(aSubKeys) - 1 do begin
			prop = dim.props[aSubKeys[j]]
			sOut += nl + string(aSubKeys[j], prop.strval, $
			                    format='      Property: %s | %s')
		endfor
		
		aSubKeys = dim.vars.keys()
		for j = 0, n_elements(aSubKeys) - 1 do begin
			sRole = aSubKeys[j]
			var = dim.vars[sRole]
			
			if var.values eq ptr_new() then begin
				sType = 'NO_DATA_YET' 
			endif else begin
				sType = typename(*(var.values))
				printf, -2, n_elements(*(var.values))
			endelse
						
			sOut += nl + string(sDimName, sRole, sType, var.units, $
			                    format='      Variable: %s[''%s''] (%s) %s')
		endfor
		
	endfor
	
	return, sOut + nl
end

;+
; Das2 dataset object, holds overall dataset properties and 1-N physical
; dimension objects.  All sub-arrays of a dataset are correlated in index
; space.
;-
pro das2ds__define
	compile_opt idl2, hidden
	
	; Have to use obj_new() to declare storage space for any compound types
	; (i.e. classes) that are part of a structure.  See:
	;
	;  https://www.harrisgeospatial.com/Support/Maintenance-Detail/ArtMID/13350/ArticleID/15715/Problems-Assigning-LIST-HASH-etc-to-Class-or-Structure-Tags
	;
	; for details
	void = { $
		das2Ds, inherits IDL_Object, name:'', group:'', $
		props:obj_new(), dims:obj_new() $
	}
end
