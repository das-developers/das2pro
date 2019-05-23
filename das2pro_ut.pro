; Unit testing for the das2pro package


pro das2pro_ut
	compile_opt IDL2

	sTest = 'Test 1: '
	t = das2_double_to_tt2000('t1970', 10)
	if t eq 0 then begin
		print, sTest + 'passed'
	endif else begin
		print, sTest + 'failed'
	endelse

	sTest = 'Test 2: '
	t = das2_text_to_tt2000('2018-245T14:13:36.123456789')
	if t eq 0 then begin
		print, sTest + 'passed'
	endif else begin
		print, sTest + 'failed'
	endelse

end
