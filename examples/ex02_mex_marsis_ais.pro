; program to get and print a few Mars Express Ionograms


pro ex02_mex_marsis_ais
	compile_opt idl2

	; Generate the URL for the desired subset
	sServer = 'http://planet.physics.uiowa.edu/das/das2Server'
	sDataset = 'Mars_Express/MARSIS/Spectrogram'
	sMin = '2005-08-06T00:47:40'
	sMax = '2005-08-06T01:32:40'
	sFmt = "%s?server=dataset&dataset=%s&start_time=%s&end_time=%s"
	sQuery = string(sServer, sDataset, sMin, sMax, format=sFmt)
	
	; Get datasets from a web server.  The sMsg varible will hold any
	; error or message data returned from the server.  Typically this
	; is empty unless an error occurs.
	lDs = das2_readhttp(sQuery, /messages=sMsg)

	print, n_elements(l), /format="%d datesets read"

	; There is typically only one dataset for homogeneous streams
	ds = lDs[0]
	
	; Let's see what it contains
	print, ds
	
	
	
end

