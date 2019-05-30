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

pro das2dim::getproperty, PROPS=props, VARS=vars
	compile_opt idl2
	if arg_present(props) then props = self.props
	if arg_present(vars) then vars = self.vars
end

function das2dim::init, _extra=ex
	compile_opt idl2
	
	void = self.IDL_Object::init()
	self.props = hash()
	self.vars = hash()
	return, !TRUE
end

;+
; Das2 dimension object, provides arrays and properties for a single
; physical dimension, such as Time or Spectral Density
;-
pro das2dim__define
	compile_opt idl2, hidden
	void = { das2dim, inherits IDL_Object, props:obj_new(), vars:obj_new()}
end
