; include das2 modules
@das2

server = 'http://planet.physics.uiowa.edu/das/das2Server'
dataSet = 'Mars_Express/MARSIS/Spectrogram'
sTime = '2005-08-06T00:47:40'
fTime = '2005-08-06T01:32:40'
params = ['10khz','Ew=false']

d = das2reader(dataSet, sTime, fTime, params=params, /verbose)

