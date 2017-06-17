require(data.table)
require(plyr)

### read main datas

mm = read.csv("./Data/macro.csv",stringsAsFactors = FALSE)
str(mm)
mm = as.data.table(mm)
mm$timestamp=as.Date(mm$timestamp)

tr = read.csv("./Data/train.csv",stringsAsFactors = FALSE)
str(tr)
tr$timestamp=as.Date(tr$timestamp)
tr = as.data.table(tr)

trmm = merge(tr,mm,by=c('timestamp'))
trmm = as.data.table(trmm)

trmm$usd.price = trmm$price_doc/trmm$usdrub;
trmm$eur.price = trmm$price_doc/trmm$eurrub;

trmm$usd.full_1m2 = trmm$usd.price/(trmm$full_sq+0.01)
trmm$usd.live_1m2 = trmm$usd.price/(trmm$life_sq+0.01) 

trmm$month = as.numeric(strftime(trmm$timestamp,'%m'))



hist(trmm$usd.full_1m2[trmm$usd.full_1m2<=10000],breaks=100)
hist(trmm$usd.live_1m2[trmm$usd.live_1m2<=20000],breaks=100)

#-----------------------------------------------------------

trmm$OK = TRUE

## 30471-29368 = 1103 flats
trmm$OK[!(trmm$build_year<=2018&trmm$build_year>1940)] = FALSE
length(trmm$OK[trmm$OK])

## 30471-16866 = 13605 flats without build year
trmm$OK[is.na(trmm$build_year)] = FALSE
length(trmm$OK[trmm$OK])



plot(trmm$build_year[trmm$OK&trmm$usd.live_1m2<=20000],trmm$usd.live_1m2[trmm$OK&trmm$usd.live_1m2<=20000])
hist(trmm$build_year[trmm$build_year<=2018&trmm$build_year>1940&!is.na(trmm$build_year)],breaks=100)

plot(trmm$month[trmm$OK&trmm$usd.live_1m2<=20000],trmm$usd.live_1m2[trmm$OK&trmm$usd.live_1m2<=20000])

## == 57 points (small) first and last floor
hist(trmm$usd.live_1m2[trmm$usd.live_1m2<=15000&trmm$floor==1&trmm$floor==trmm$max_floor],breaks=100)
length(trmm$usd.live_1m2[trmm$usd.live_1m2<=15000&trmm$floor==1&trmm$floor==trmm$max_floor&!is.na(trmm$floor)&!is.na(trmm$max_floor)])


trmmx  = trmm
trname = names(trmm)
for (i in 1:length(trname)) {
  if (!(typeof(trmm[,i]) %in% c("double","integer"))) {
    print(paste(trname[i],"-->",typeof(trmm[,i])))
    print(sort(unique(trmm[,i])))
    tmp.value = sort(unique(trmm[,i]))
    tmp.list  = trmm[,i]
    jj = 0
    for (j in tmp.value) {
      tmp.list[tmp.list==j] = jj
      jj = jj+1
    }
    #trmmx[,i] = as.numeric(tmp.list)
    trmmx[,i] = as.integer(tmp.list)
  }
}
str(trmmx)
rm(i,j,jj,tmp.list,tmp.value,trname)

trmmx$floor[is.na(trmmx$floor)]=1
trmmx$max_floor[is.na(trmmx$max_floor)]=1
trmmx$build_year[is.na(trmmx$build_year)]=1940
trmmx$state[is.na(trmmx$state)]=-1
trmmx$usd.live_1m2[is.na(trmmx$usd.live_1m2)]=trmmx$usd.full_1m2[is.na(trmmx$usd.live_1m2)]



plot(trmmx$sub_area[trmm$OK&trmm$usd.live_1m2<=20000],trmm$usd.live_1m2[trmm$OK&trmm$usd.live_1m2<=20000])

agg.sub_area = 
  ddply(trmmx,.(sub_area,month),summarise,
        ssum.full=sum(usd.full_1m2),
        ssum.live=sum(usd.live_1m2),
        medi.full=median(usd.full_1m2),
        medi.live=median(usd.live_1m2),
        mean.full=mean(usd.full_1m2),
        mean.live=mean(usd.live_1m2),
        std.full=sd(usd.full_1m2),
        std.live=sd(usd.live_1m2))

agg = subset(agg.sub_area,agg.sub_area$mean.live<10000&sub_area==3); plot(agg$month,agg$medi.live)

str(agg.sub_area)



##cor(trmmx$usd.full_1m2,trmmx$full_sq)

trname = names(trmmx); r1=c(); rr=c()
for (i in 2:length(trname)) if (i!=382){
  if ((typeof(trmmx[,i]) %in% c("double","integer"))) {
    j = cor(trmmx$usd.full_1m2[!is.na(trmmx[,i])],trmmx[!is.na(trmmx[,i]),i])
    k = cor(trmmx$usd.live_1m2[!is.na(trmmx[,i])],trmmx[!is.na(trmmx[,i]),i])
    if ((abs(j)>0.01)) {
      print(paste(i,"(",length(trmmx$usd.full_1m2[is.na(trmmx[,i])]),")",trname[i],"-->",j))
      ######## ??????????? trmmx[is.na(trmmx[,i]),i]=-1
      r1 = c(r1,i)
      rr = c(rr,j)
    }
  }
}

#str(trmmx)
rm(i,j,jj,tmp.list,tmp.value,trname)


trmmxx = trmmx[,c(r1,13)]
xx.lm=lm(usd.full_1m2~.-usd.live_1m2-sub_area,subset(trmmxx,sub_area==51))
summary(xx.lm)
