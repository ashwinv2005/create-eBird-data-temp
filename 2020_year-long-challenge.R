require(lubridate)
require(tidyverse)

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","LAST.EDITED.DATE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","HAS.MEDIA",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

rawpath = "ebd_IN_202001_202012_relDec-2020.txt"

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

days = c(31,29,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,60,91,121,152,182,213,244,274,305,335)

data = data %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month],
         daym = day(OBSERVATION.DATE))

data0 = data
datas = data %>% filter(CATEGORY %in% c("species","issf"))

totobs = length(data0$COMMON.NAME)
totlists = length(unique(data0$SAMPLING.EVENT.IDENTIFIER))
totbir = length(unique(data0$OBSERVER.ID))
totspecs = length(unique(datas$COMMON.NAME))
media = data0 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(any(HAS.MEDIA == 1)) %>%
  ungroup
media = length(unique(media$SAMPLING.EVENT.IDENTIFIER))


nol = data0 %>% filter(ALL.SPECIES.REPORTED == 1)
n = length(unique(nol$SAMPLING.EVENT.IDENTIFIER))



############# prolific eBirder - 480 complete lists for the year

data1 = data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14.5) 

data2 = data1 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>%
  ungroup

data4 = data2 %>%
  group_by(OBSERVER.ID) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER))

data5 = data4 %>% filter(count >= 480)
temp = data5

#############

temp$obs.id.num <- gsub("[[:alpha:]]", "", temp$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relNov-2020.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST_NAME","LAST_NAME")
eBird.users = eBird.users %>% mutate(FULL.NAME = paste(FIRST_NAME, LAST_NAME, sep = " "))
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

temp = left_join(temp, eBird.users)

write.csv(temp, "prolific-eBirder-2020.csv", row.names = F)

a = read.csv("prolific-eBirder-2020.csv")
a = a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok")
set.seed(n)
sample(a$FULL.NAME, 1)




############# consistent eBirder - 4 lists every week

data1 = data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14.5) 

data2 = data1 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>%
  ungroup

for (i in 1:52)
{
  low = (i-1)*7 + 1
  high = i*7
  if ((366 - high) <= 5)
    high = 366
  
  data3 = data2 %>% filter(day %in% c(low:high))
  
  data4 = data3 %>%
    group_by(OBSERVER.ID) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER))
  
  data5 = data4 %>% filter(count >= 4)
  
  data2 = data2 %>% filter(OBSERVER.ID %in% data5$OBSERVER.ID)
}

temp = data5

#############

temp$obs.id.num <- gsub("[[:alpha:]]", "", temp$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relNov-2020.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST_NAME","LAST_NAME")
eBird.users = eBird.users %>% mutate(FULL.NAME = paste(FIRST_NAME, LAST_NAME, sep = " "))
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

temp = left_join(temp, eBird.users)

write.csv(temp, "consistent-eBirder-2020.csv", row.names = F)

a = read.csv("consistent-eBirder-2020.csv")
a = a %>% filter(FULL.NAME != "MetalClicks Ajay Ashok")
set.seed(n)
sample(a$FULL.NAME, 1)



############# adventurous eBirder - 4 lists from 15 districts

data1 = data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14.5) 

data2 = data1 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>%
  ungroup

data3 = data2 %>%
  group_by(OBSERVER.ID,COUNTY) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER))

data4 = data3 %>% filter(count >= 4) %>% group_by(OBSERVER.ID) %>% summarize(dist = n_distinct(COUNTY))

data5 = data4 %>% filter(dist >= 15)
temp = data5


#############

temp$obs.id.num <- gsub("[[:alpha:]]", "", temp$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relNov-2020.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST_NAME","LAST_NAME")
eBird.users = eBird.users %>% mutate(FULL.NAME = paste(FIRST_NAME, LAST_NAME, sep = " "))
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

temp = left_join(temp, eBird.users)

write.csv(temp, "adventurous-eBirder-2020.csv", row.names = F)

a = read.csv("adventurous-eBirder-2020.csv")
a = a %>% filter(!FULL.NAME %in% c("MetalClicks Ajay Ashok","AWC India"))
set.seed(n+1)
sample(a$FULL.NAME, 1)



############# faithful eBirder - 160 lists from a single location

data1 = data0 %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14.5) 

data2 = data1 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% filter(!any(OBSERVATION.COUNT == "X")) %>%
  ungroup

data3 = data2 %>%
  group_by(OBSERVER.ID,LOCALITY) %>% summarize(count = n_distinct(SAMPLING.EVENT.IDENTIFIER))

data4 = data3 %>% filter(count >= 160)

data5 = data4
temp = data5


#############

temp$obs.id.num <- gsub("[[:alpha:]]", "", temp$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relNov-2020.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST_NAME","LAST_NAME")
eBird.users = eBird.users %>% mutate(FULL.NAME = paste(FIRST_NAME, LAST_NAME, sep = " "))
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

temp = left_join(temp, eBird.users)

write.csv(temp, "faithful-eBirder-2020.csv", row.names = F)

a = read.csv("faithful-eBirder-2020.csv")
a = a %>% filter(!FULL.NAME %in% c("MetalClicks Ajay Ashok","AWC India"))
set.seed(n)
sample(a$FULL.NAME, 1)



############# dedicated eBirder - 500 hours of birding

data1 = data0 %>% filter(ALL.SPECIES.REPORTED == 1, !is.na(DURATION.MINUTES)) 

data2 = data1 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>%
  group_by(OBSERVER.ID) %>% summarize(count1 = sum(DURATION.MINUTES))

data3 = data2 %>% mutate(count = count1/60)

data4 = data3 %>% filter(count >= 500)

data5 = data4
temp = data5


#############

temp$obs.id.num <- gsub("[[:alpha:]]", "", temp$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relNov-2020.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST_NAME","LAST_NAME")
eBird.users = eBird.users %>% mutate(FULL.NAME = paste(FIRST_NAME, LAST_NAME, sep = " "))
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

temp = left_join(temp, eBird.users)

write.csv(temp, "dedicated-eBirder-2020.csv", row.names = F)

a = read.csv("dedicated-eBirder-2020.csv")
a = a %>% filter(!FULL.NAME %in% c("MetalClicks Ajay Ashok","AWC India"))
set.seed(n)
sample(a$FULL.NAME, 1)

