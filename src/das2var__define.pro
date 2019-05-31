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
	self.idxmap = [-1, -1, -1, -1]
	
	if(isa(ex)) then self.setproperty, _EXTRA=ex
	return, !TRUE
end

pro das2var::getproperty, $
	UNITS=units, VALUES=values, IDXMAP=idxmap, PARSER=parser
	compile_opt idl2
	if arg_present(units) then units = self.units
	if arg_present(values) then values = self.values
	if arg_present(idxmap) then idxmap = self.idxmap
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
		iStop = n_elements(idxmap) - 1
		if iStop gt 3 then iStop = 3
		for i=0,iStop do self.idxmap[i] = idxmap[i]
	endif
end

;+
; Das2 Variable, an array, it's units and it's index map.
;-
pro das2var__define
	compile_opt idl2, hidden
	void = { $
		das2var, inherits IDL_Object, units:'', values:ptr_new(), $
		parser:obj_new(), idxmap: [-1, -1, -1, 0] $
	}
end
