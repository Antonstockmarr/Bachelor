source('data.r')

wdtime <- weather$WindDirection[as.Date(weather$ObsTime) == '2018-12-31']
wstime <- weather$WindSpeed[as.Date(weather$ObsTime) == '2018-12-31']

wdday <- day.weather$WindDirection[day.weather$Date == '2018-12-31']
wsday <- day.weather$WindSpeed[day.weather$Date == '2018-12-31']

polar.plot(wstime,wdtime)


polar.points(wsday,wdday, col=2)
