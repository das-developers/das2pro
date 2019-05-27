; program to get and print a few Mars Express Ionograms


pro ex02_mex_marsis_ais
	compile_opt IDL2

	sServer = 'http://planet.physics.uiowa.edu/das/das2Server'
	sDataset = 'Mars_Express/MARSIS/Spectrogram'
	sMin = '2005-08-06T00:47:40'
	sMax = '2005-08-06T01:32:40'

	lDs = das2_read_server(sServer, sDataSet, sMin, sMax)

	print, n_elements(l), /format="%d datesets downloaded"

	; There is typically only one dataset for homogeneous streams
	ds = lDs[0]
	
	; Let's see what it contains
	print, ds
	
	
	
end

