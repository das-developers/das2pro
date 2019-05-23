; Unit testing for the das2pro package


pro das2pro_ut
	compile_opt IDL2

	sTest = 'Test 1: '
	t = das2_double_to_tt2000('t1970', 10)
	if t eq -946727949814622001LL then begin
		print, sTest + 'passed'
	endif else begin
		print, sTest + 'failed'
	endelse

	sTest = 'Test 2: '
	t = das2_text_to_tt2000('2000-01-01T11:58:55.816')
	if t eq 0LL then begin
		print, sTest + 'passed'
	endif else begin
		print, sTest + 'failed'
	endelse

end
