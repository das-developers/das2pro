; include das2 modules
@das2

; -- $MAIN$ ------------------------------------
;http://planet.physics.uiowa.edu/das/das2Server?server=dataset&dataset=&start_time=&end_time=
dataSet = 'Cassini/RPWS/HiRes_MidFreq_Waveform'
sTime = '2004-11-15T00:40:50.400'
fTime = '2004-11-15T00:40:50.500'
params = ['10khz','Ew=false']
d = das2reader(dataSet, sTime, fTime, params=params, /verbose)

; help, d[0] ; print stream header
prop = d[1].packet.yscan

tt2000 = das2_double_to_tt2000(d[1].packet.x._units, d[1].xdata)
timestamp = cdf_encode_tt2000(tt2000, epoch=3)
p = plot(d[1].ydata * 1e3, d[1].zdata, xtit='Time [ms] from ' + timestamp, ytit=prop.properties._string_zlabel, $
         tit=d[0].stream.properties._string_title, dimensions=[1800, 600], /buffer, xst=1)
p.xthick = 4.
p.ythick = 4.
p.thick = 3.
p.font_size = 24
p.tit.font_size = 24
p.save, 'run_example.png', width=3600, height=1200, resolution=300

end
