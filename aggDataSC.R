
### Construct dataset ####


# get index of all users

# query-------------------
# SELECT ev.user_id, sum(real_usd_amount), max(real_usd_amount), 
# min(real_usd_amount), count(real_usd_amount), sum(event_duration_seconds), 
# max(event_duration_seconds), min(event_duration_seconds), 
# count(event_duration_seconds), sum(message_accept_delay_seconds), 
# max(message_accept_delay_seconds), min(message_accept_delay_seconds), 
# count(message_accept_delay_seconds), sum(product_extended_amount), 
# max(product_extended_amount), min(product_extended_amount), 
# count(product_extended_amount) FROM [Dataset229.Table85] AS ev 
# JOIN (SELECT user_id FROM [Dataset229.Table85] 
# WHERE unix_timestamp<1317772800 AND action_type=1) AS users 
# ON ev.user_id=users.user_id WHERE unix_timestamp<1336176000 group by 
# ev.user_id;

# The counts include the zeros and are therefore no good for product, 
# and real usd.



quants<-read.csv("quantities.csv")
colnames(quants)<-c("user", "real_sum", "real_max", "real_min", "realC", "sessSum","sessmax","sessmin","sessC", "mess_sum", "mess_max", "mess_min", "messC","virt_sum", "virt_max", "virt_min","virtC")

u<-unique(quants[,1])
idMap<-data.frame(seq(1,length(u)), u)
idMap<-idMap[order(idMap[,2]),]
userID<-idMap[,1]

# map users with counts data to all users, map 0 to the remaining users

# query-------------------------
# SELECT ev.user_id, count(real_usd_amount) FROM [Dataset229.Table85] 
# AS ev JOIN (SELECT user_id FROM [Dataset229.Table85] 
# WHERE unix_timestamp<1317772800 AND action_type=1) AS users 
# ON ev.user_id=users.user_id WHERE unix_timestamp<1336176000 
# AND real_usd_amount>0 group by ev.user_id;

realCount<-read.csv("realCount.csv")
messCount<-read.csv("messCount.csv")
virtCount<-read.csv("virtCount.csv")

realC<-realCount[match(u, realCount[,1]),2]
realC[is.na(realC)]<-0

messC<-messCount[match(u, messCount[,1]),2]
messC[is.na(messC)]<-0

virtC<-virtCount[match(u, virtCount[,1]),2]
virtC[is.na(virtC)]<-0

# SG 1mo
#user1<-cbind(quants[,1:4], realC)
#colnames(user1)<-c("user", "real_sum", "real_max", "real_min", "realC")
#

# SG 3mo
#user1<-cbind(quants[,1:4], realC, quants[,10:12], messC)
#colnames(user1)<-c("user", "real_sum", "real_max", "real_min", "realC", "mess_sum", "mess_max", "mess_min", "messC")

# SC 1mo

user1<-cbind(quants[,1],userID, quants[,2:4], realC)
colnames(user1)<-c("oldID","user", "real_sum", "real_max", "real_min", "realC")

# legacy
#user1<-cbind(quants[,1:4], realC, quants[,10:12], messC, quants[,14:16], virtC)
#colnames(user1)<-c("user", "real_sum", "real_max", "real_min", "realC", "mess_sum", "mess_max", "mess_min", "messC","virt_sum", "virt_max", "virt_min","virtC")


## compile additional summary information

# generate averages

getRealMean<-function(row){
  if(row["realC"]>0){
    return(row["real_sum"]/row["realC"])
  }
  return(0)
}

# iterate version
realMean<-rep(0, length(user1[,1]))

for(i in 1:length(user1[,1])){
  print(i)
  if(user1[i,"real_sum"]==0){
    realMean[i]<-0
  }else{
    realMean[i]<-user1[i,"real_sum"]/user1[i,"realC"]
  }
  
}

getMessMean<-function(row){
  if(row["messC"]>0){
    return(row["mess_sum"]/row["messC"])
  }
  return(0)
}

getVirtMean<-function(row){
  if(row["virtC"]>0){
    return(row["virt_sum"]/row["virtC"])
  }
  return(0)
}

#realMean<-apply(user1,1,getRealMean)
messMean<-apply(user1,1, getMessMean)
#virtMean<-apply(user1,1,getVirtMean)


# SG 1mo
#user2<-cbind(user1[,1:5], realMean, user1[,6:length(user1[1,])],messMean)
#colnames(user2)<-c("user", "real_sum", "real_max", "real_min", "realC", "realmean", "mess_sum", "mess_max", "mess_min", "messC", "messMean")

# SC 1mo
user2<-cbind(user1[,1:5], realMean)
colnames(user2)<-c("user", "real_sum", "real_max", "real_min", "realC", "realmean")

user2<-user2[order(user2[,1]),]

## apply static demographics

# query-------------------
# SELECT ev.user_id, gender FROM [Dataset229.Table85] AS ev JOIN 
# (SELECT user_id FROM [Dataset229.Table85] WHERE unix_timestamp<1317772800 AND
# action_type=1) AS users ON ev.user_id=users.user_id 
# WHERE unix_timestamp<1336176000 group by ev.user_id, gender;


# genders: 0=none, 1=male, 2=female

gender<-read.csv("genders.csv")

# SG1mo


#genNum<-rep(0, length(unique(gender[,1])))
#format(as.numeric(x), scientific = F)
#dupGen<-gender[which(duplicated(gender[,1])==TRUE),]

#genSet<-rep(FALSE, length(gender[,1]))

# find duplicates
#for(i in 1:length(gender[,1])){
#  print(i)
#  for(j in 1:length(gender[,1])){    
#    if(gender[i,1]==gender[j,1]&& i!=j){
#      genSet[i]<-TRUE
#    }
#  }
#}

#dupGen<-gender[which(genSet==TRUE),]

#dupGenOrder<-dupGen[order(dupGen[,1]),]



dupGenOrder<-gender[order(gender[,1]),]

if(length(unique(gender[,1]))!=length(u)){
  m<-match(dupGenOrder[,1], u)
  bad<-which(is.na(m))
  dupGenOrder<-dupGenOrder[-bad,]
}

genNum<-rep(0, length(unique(dupGenOrder[,1])))

encodeGen<-function(charGen){
  
  if(is.na(charGen)){return(-1)}
  if(as.character(charGen)==as.character("F")) {return(2)}
  if(as.character(charGen)=="M") {return(1)}
  if(as.character(charGen)=='') {return(0)}
  return(-1)
  
}

currentID=0
currentPos=0
bestGen=-1
num=0

for(i in 1:length(dupGenOrder[,1])){  
  
  if(i%%100==0){
    
    print(i)
    print(paste("current# :", currentPos))
    print(paste("last num: ",num))
    print(paste("prev last num: ",genNum[currentPos]))        
  }
  
  if(dupGenOrder[i,1]==currentID){    
    
    num<-encodeGen(dupGenOrder[i,2])
    
    if(num>genNum[currentPos]){
      genNum[currentPos]<-num
    }
    
    #currentPos<-currentPos+1
    
  }else{
    currentPos<-currentPos+1
    currentID<-dupGenOrder[i,1]
    genNum[currentPos]<-encodeGen(dupGenOrder[i,2])
  }
}

fixedGen<-cbind(unique(dupGenOrder[,1]),genNum )

#SG1mo

s

# ages - not available in this set, they are all zero



age<-read.csv("age.csv")

if(max(age[,2])>0){
  if(length(unique(age[,1]))!=length(u)){
    m<-match(age[,1], u)
    bad<-which(is.na(m))
    shrunkAge<-age[-bad,]
    orderAge<-shrunkAge[order(shrunkAge[,1]),]
    ageNum<-rep(0,length(unique(shrunkAge[,1])))
  }
  else{
    orderAge<-age[order(age[,1]),]
    ageNum<-rep(0,length(unique(age[,1])))
  }
} 
  currentID=0
  currentPos=0
  
  for(i in 1:length(orderAge[,1])){  
    print(i)
    if(orderAge[i,1]==currentID){    
      
      num<-orderAge[,2]
      
      if(num>ageNum[currentPos]){
        ageNum[currentPos]<-num
      }
      
      #currentPos<-currentPos+1
      
    }else{
      currentPos<-currentPos+1
      currentID<-orderAge[i,1]
      ageNum[currentPos]<-orderAge[i,2]
    }
  }



ageNum<-age[order(age[,1]),2]
# query-------
# modify gender


# countries: as factors

# query-------
# modify gender

country<-read.csv("country.csv")
if(length(unique(country[,1]))!=length(u)){
  m<-match(country[,1], u)
  bad<-which(is.na(m))
  country<-country[-bad,]
}
countryIndex<-seq(1, length(unique(country[,2])))
uCountry<-unique(country[,2])
uCountryOrder<-uCountry[order(uCountry)]
countryDict<-data.frame(uCountryOrder, countryIndex, stringsAsFactors=F )
dupCountryOrder<-country[order(country[,1]),]

encodeCountry<-function(country){
  return(countryDict[match(country,countryDict[,1]),2])
}

currentID=0
currentPos=0
bestCountry=-1

countryNum<-rep(0, length(unique(dupCountryOrder[,1])))

for(i in 1:length(dupCountryOrder[,1])){  
  
  if(dupCountryOrder[i,1]==currentID){    
    
    num<-encodeCountry(dupCountryOrder[i,2])
    
    if(i%%100==0){
      print(paste("current# :", currentPos))
      print(paste("new: ",num))
      print(paste("old: ",countryNum[currentPos]))        
    }
    
    if(num>countryNum[currentPos]){
      countryNum[currentPos]<-num
    }
    
    #currentPos<-currentPos+1
    
  }else{
    currentPos<-currentPos+1
    currentID<-dupCountryOrder[i,1]
    countryNum[currentPos]<-encodeCountry(dupCountryOrder[i,2])
  }
}

#user2<-cbind(user2,genNum,countryNum)
user2<-cbind(user2,genNum)

# get referrer code

# query-------
# modify gender

referrer<-read.csv("referrer.csv")
if(length(unique(referrer[,1]))!=length(u)){
  m<-match(referrer[,1], u)
  bad<-which(is.na(m))
  referrer<-referrer[-bad,]
}
dupRefOrder<-referrer[order(referrer[,1]),]

refIndex<-seq(1, length(unique(referrer[,2])))
uRef<-unique(referrer[,2])
uRefOrder<-uRef[order(uRef)]
refDict<-data.frame(uRefOrder, refIndex, stringsAsFactors=F )

encodeRef<-function(ref){
  return(refDict[match(ref,refDict[,1]),2])
}

currentID=0
currentPos=0
bestRef=-1

refNum<-rep(0, length(unique(dupRefOrder[,1])))

for(i in 1:length(dupRefOrder[,1])){  
  
  if(dupRefOrder[i,1]==currentID){    
    
    num<-encodeRef(dupRefOrder[i,2])
    
    if(i%%100==0){
      print(paste("current# :", currentPos))
      print(paste("new: ",num))
      print(paste("old: ",refNum[currentPos]))        
    }
    
    if(num>refNum[currentPos]){
      refNum[currentPos]<-num
    }
    
    #currentPos<-currentPos+1
    
  }else{
    currentPos<-currentPos+1
    currentID<-dupRefOrder[i,1]
    refNum[currentPos]<-encodeRef(dupRefOrder[i,2])
  }
}

if(max(age[,2])>0){
  user3<-cbind(user2, refNum, ageNum)
}else{
  user3<-cbind(user2, refNum)
  }
# action types

# query--------------
# SELECT ev.user_id, count(action_type) FROM [Dataset229.Table85] 
# AS ev JOIN (SELECT user_id FROM [Dataset229.Table85] 
# WHERE unix_timestamp<1317772800 AND action_type=1) AS users 
# ON ev.user_id=users.user_id WHERE unix_timestamp<1336176000 
# and action_type=2 group by ev.user_id;

act2<-read.csv("act2.csv")
act3<-read.csv("act3.csv")
act4<-read.csv("act4.csv")
act5<-read.csv("act5.csv")
act6<-read.csv("act6.csv")
act7<-read.csv("act7.csv")
act23<-read.csv("act23.csv")
act26<-read.csv("act26.csv")
act27<-read.csv("act27.csv")

act2Match<-match(u,act2[order(act2[,1]),1])
act3Match<-match(u,act3[order(act3[,1]),1])
act4Match<-match(u,act4[order(act4[,1]),1])
act5Match<-match(u,act5[order(act5[,1]),1])
act6Match<-match(u,act6[order(act6[,1]),1])
act7Match<-match(u,act7[order(act7[,1]),1])
act23Match<-match(u,act23[order(act23[,1]),1])
act26Match<-match(u,act26[order(act26[,1]),1])
act27Match<-match(u,act27[order(act27[,1]),1])

act2Match[is.na(act2Match)]<-0
act3Match[is.na(act3Match)]<-0
act4Match[is.na(act4Match)]<-0
act5Match[is.na(act5Match)]<-0
act6Match[is.na(act6Match)]<-0
act7Match[is.na(act7Match)]<-0
act23Match[is.na(act23Match)]<-0
act26Match[is.na(act26Match)]<-0
act27Match[is.na(act27Match)]<-0

getCount<-function(index, input, output){
  for(i in 1:length(output)){
    if(index[i]>0){
      output[i]<-input[index[i],2]
    }
  }
  return(output)
}

act2Count<-getCount(act2Match, act2, rep(0, length(act2Match)))
act3Count<-getCount(act3Match, act3, rep(0, length(act3Match)))
act4Count<-getCount(act4Match, act4, rep(0, length(act4Match)))
act5Count<-getCount(act5Match, act5, rep(0, length(act5Match)))
act6Count<-getCount(act6Match, act6, rep(0, length(act6Match)))
act7Count<-getCount(act7Match, act7, rep(0, length(act7Match)))
act23Count<-getCount(act23Match, act23, rep(0, length(act23Match)))
act26Count<-getCount(act26Match, act26, rep(0, length(act26Match)))
act27Count<-getCount(act27Match, act27, rep(0, length(act27Match)))

#SG
user4<-cbind(user3, act2Count,act3Count,act4Count,act5Count,act6Count,act7Count,act27Count)

#SC
#user4<-cbind(user3, act2Count,act3Count,act4Count,act5Count,act6Count,act7Count,act27Count)


genTotalAct<-function(row){
  #print(paste(c(row["act2Count"],row["act3Count"],row["act4Count"],row["act6Count"],row["act7Count"],row["act27Count"])))
  #SG
  return(as.numeric(row["act2Count"])+as.numeric(row["act3Count"])+as.numeric(row["act4Count"])+as.numeric(row["act6Count"])+as.numeric(row["act7Count"])+as.numeric(row["act27Count"]))
  #SC
  #return(row["act2Count"]+row["act3Count"]+row["act4Count"]+row["act5Count"]+row["act6Count"]+row["act7Count"]+row["act27Count"])
}

activity<-apply(user4,1,genTotalAct)

act34<-read.csv("act34.csv")
act34Match<-match(u,act34[order(act34[,1]),1])
act34Match[is.na(act34Match)]<-0
sessions<-getCount(act34Match, act34, rep(0, length(act34Match)))


#SG
#user5<-cbind(user4,activity,sessions)

#SC
user5<-cbind(user4,activity)

##get level

## query --------



levels<-read.csv("gamelevel.csv")
maxLevel<-levels[order(levels[,1]),]
if(length(unique(maxLevel[,1]))!=length(u)){
  m<-match(maxLevel[,1], u)
  bad<-which(is.na(m))
  maxLevel<-maxLevel[-bad,]
}
users<-cbind(user5, maxLevel[,2])
colnames(users)[length(colnames(users))]<-"maxlevel"

maxTime<-read.csv("maxTime.csv")
current<-1330775999
if(length(unique(maxTime[,1]))!=length(u)){
  m<-match(maxTime[,1], u)
  bad<-which(is.na(m))
  maxTime<-maxTime[-bad,]
}
time<-current-maxTime[,2]

users<-cbind(users, time)
colnames(users)[length(colnames(users))]<-"last"

minTime<-read.csv("minTime.csv")
if(length(unique(minTime[,1]))!=length(u)){
  m<-match(minTime[,1], u)
  bad<-which(is.na(m))
  minTime<-minTime[-bad,]
}
time<-current-minTime[,2]

users<-cbind(users, time)
colnames(users)[length(colnames(users))]<-"length"

write.csv(users,file="users.csv")

# make test and train next

# test integrity of data



