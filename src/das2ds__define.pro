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


pro das2ds::getproperty, PROPS=props, DIMS=dims
	compile_opt idl2
	if arg_present(props) then props = self.props
	if arg_present(dims) then dims = self.dims
end

function das2ds::recsize
	
	iMaxEnd = 0
	
	aDims = self.dims.keys()
	
	printf, -2, "Dims in dataset: ", n_elements(nDims)
	
	for d = 0, n_elements(aDims) - 1 do begin
		dim = self.dims[ aDims[d] ]
		
		aVar = dim.vars.keys()
		for v = 0, n_elements(aVar) do begin
			var = dim.vars[ aVar[v] ]
			
			if var.decoder ne !null then begin
				
				; Using IDL convention of end including the last index instead of
				; of being an upper bound.
				iEnd = var.decoder.offset() + var.decoder.chunksize() - 1
				
				if iEnd gt iMaxEnd then iMaxEnd = iEnd
				
			endif
		endfor 
	endfor
	return, iMaxEnd + 1
end

function das2ds::init, _extra=ex
	compile_opt idl2
	
	void = self.IDL_Object::init()
	self.props = hash()
	self.dims = hash()
	return, !true
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
	void = { das2Ds, inherits IDL_Object, props:obj_new(), dims:obj_new()}
end
