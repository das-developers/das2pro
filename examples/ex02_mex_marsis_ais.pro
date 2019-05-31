; program to get and print a few Mars Express Ionograms


pro ex02_mex_marsis_ais
	compile_opt idl2

	; Generate the URL for the desired subset
	sServer = 'http://planet.physics.uiowa.edu/das/das2Server'
	sDataset = 'Mars_Express/MARSIS/Spectrogram'
	sMin = '2005-08-06T00:52:09'
	sMax = '2005-08-06T00:52:17'
	sFmt = "%s?server=dataset&dataset=%s&start_time=%s&end_time=%s"
	sUrl = string(sServer, sDataset, sMin, sMax, format=sFmt)
	
	; Get datasets from a web server.  The sMsg varible will hold any
	; error or message data returned from the server.  Typically this
	; is empty unless an error occurs.
	sMsg = !null
	lDs = das2_readhttp(sUrl, messages=sMsg)
	
	if lDs eq !null then begin
		printf, -2, sMsg
		stop
	endif

	print, n_elements(lDs), format="%d datesets read"

	; There is typically only one dataset for homogeneous streams
	ds = lDs[0]
	
	; Let's see what it contains
	print, ds
	
	
	
end

