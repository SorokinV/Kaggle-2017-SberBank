require(plyr)
#require(tidyr)
#require(caTools)
require(forecast)

mm$timestamp[is.na(mm$oil_urals)]



pp=pacf(mm$usdrub[mm$timestamp>"2010-01-04"],lag.max=366*5)
str(pp)
pp=pacf(mm$oil_urals[mm$timestamp>"2010-01-04"],lag.max=180)
str(pp)



zz              = ts(log(1-zz.df$amount), frequency = 2)
#print(zz)

## rmse = 1.36 (don't best) 04.11.2016 (opt.crit=likehood)
## zz.stl          = stlm(zz)#,s.window=NULL)#,method = 'arima')

#  rmse = 1.3168 (best with mse 04:00 04/11/2016)
#zz.stl          = ets (zz,opt.crit = 'mse')

