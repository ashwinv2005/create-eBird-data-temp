require(lubridate)
library(tidyverse)
library(ggthemes)
theme_set(theme_tufte())

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

require(rgdal)
require(sp)
require(sf)
require(mapview)
require(leaflet)

indiamap = readOGR("India","India_2011")
statemap = readOGR("in_states_2019","in_states_2019")
proj4string(statemap) = "+proj=longlat +datum=WGS84"
proj4string(indiamap) = "+proj=longlat +datum=WGS84"

state = "WEST BENGAL"
wbmap = statemap[statemap@data$stname %in% state,]

bgdmap = readOGR("BGD_adm","BGD_adm0")



preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","COUNTRY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

rawpath1 = "ebd_IN-WB_202005_202005_relAug-2020.txt"

nms = read.delim(rawpath1, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data1 = read.delim(rawpath1, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

rawpath2 = "ebd_BD_202005_202005_relAug-2020.txt"

nms = read.delim(rawpath1, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data2 = read.delim(rawpath2, colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ",NA))

data = rbind(data1,data2)

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data = data %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         daym = day(OBSERVATION.DATE))

pelagics = read.csv("cyclone_amphan_birds.csv")
pelagics = pelagics$pelagics

nonpels = c("River Tern","Whiskered Tern") 
pels = setdiff(pelagics,nonpels)
allbirds = pelagics



### subset to regions with pelagics

data_pel = data1 %>% filter(COMMON.NAME %in% pels)
counties_pel = unique(data_pel$COUNTY)

data_pel = data2 %>% filter(COMMON.NAME %in% pels)
states_pel = unique(data_pel$STATE)

data1 = data1 %>% filter(COUNTY %in% counties_pel)
data2 = data2 %>% filter(STATE %in% states_pel)


data = rbind(data1,data2)

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data = data %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         daym = day(OBSERVATION.DATE))




########### comparison with 2019

require(lubridate)
require(tidyverse)

preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","COUNTRY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

rawpath1 = "ebd_IN-WB_201905_201905_relAug-2020.txt"

nms = read.delim(rawpath1, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data1 = read.delim(rawpath1, colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ",NA))

rawpath2 = "ebd_BD_201905_201905_relAug-2020.txt"

nms = read.delim(rawpath1, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data2 = read.delim(rawpath2, colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ",NA))

data1 = data1 %>% filter(COUNTY %in% counties_pel)
data2 = data2 %>% filter(STATE %in% states_pel)


data2019 = rbind(data1,data2)

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data2019 = data2019 %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data2019 = data2019 %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         daym = day(OBSERVATION.DATE))

data.1 = data2019
data.1$year = 2019
data.2 = data
data.2$year = 2020

extra = read.csv("extra.csv")
extra$OBSERVATION.DATE = as.Date(extra$OBSERVATION.DATE)
data.2 = rbind(data.2,extra)

data.3 = rbind(data.1,data.2)

data.4 = data.3 %>% group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup() %>%
  filter(COMMON.NAME %in% pels)

plotmap = ggplot(data.4, aes(x=LONGITUDE,y=LATITUDE)) +
  facet_wrap(. ~ year, ncol = 2)+
  geom_polygon(data = wbmap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = bgdmap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_jitter(col = cols[3], size = 3, width = 0.05, height = 0.05, alpha = 0.3) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20, colour = cols[3])) +
  coord_map()

print(plotmap)
ggsave(file="amphan_locations.jpeg", units="in", width=10, height=7)
dev.off()



##### plot comparison of species, frequencies, high counts, etc.
## 1) species

temp = data.frame(daym = 1:31)

total.lists = data.2 %>%
  group_by(daym) %>% summarize(stat = n_distinct(group.id))
total.lists$type = "Total Checklists Uploaded"

data.species = data.2 %>% group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup() %>%
  filter(CATEGORY %in% c("species","issf")) %>% filter(COMMON.NAME %in% pels) %>% group_by(daym) %>%
  summarize(stat = n_distinct(COMMON.NAME))

data.species = left_join(temp,data.species)
data.species$stat[is.na(data.species$stat)] = 0

data.freq = data.2 %>% group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup() %>% 
  filter(ALL.SPECIES.REPORTED == 1)
data.freq$pel = 0
data.freq$pel[data.freq$COMMON.NAME %in% pels] = 1
data.freq$pel[data.freq$COMMON.NAME %in% nonpels] = 2

data.freq1 = data.freq %>% group_by(daym) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(pel == 1) %>% summarize(stat = 100*n_distinct(group.id)/max(lists))
data.freq1 = left_join(temp,data.freq1)
data.freq1$stat[is.na(data.freq1$stat)] = 0

data.freq2 = data.freq %>% group_by(daym) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(pel == 2) %>% summarize(stat = 100*n_distinct(group.id)/max(lists))
data.freq2 = left_join(temp,data.freq2)
data.freq2$stat[is.na(data.freq2$stat)] = 0


data.count = data.2 %>% filter(OBSERVATION.COUNT != "X")
data.count$OBSERVATION.COUNT = as.numeric(data.count$OBSERVATION.COUNT)
data.count = data.count %>% group_by(daym,COMMON.NAME) %>% arrange(desc(OBSERVATION.COUNT)) %>%
  group_by(daym,COMMON.NAME) %>% slice(1)

data.count1 = data.count %>% filter(COMMON.NAME %in% pels) %>%
  group_by(daym) %>% summarize(stat = sum(OBSERVATION.COUNT))
data.count1 = left_join(temp,data.count1)
data.count1$stat[is.na(data.count1$stat)] = 0

data.count2 = data.count %>% filter(COMMON.NAME %in% nonpels) %>%
  group_by(daym) %>% summarize(stat = sum(OBSERVATION.COUNT))
data.count2 = left_join(temp,data.count2)
data.count2$stat[is.na(data.count2$stat)] = 0


data.species$type = "Number of Pelagic Species"
data.freq1$type = "Checklists with Pelagics (%)"
data.count1$type = "Total Pelagic Individuals"

data.final = rbind(total.lists, data.species,data.freq1,data.count1)
data.final$type = factor(data.final$type, levels = c("Total Checklists Uploaded","Checklists with Pelagics (%)",
                                                     "Number of Pelagic Species",
                                                     "Total Pelagic Individuals"))



ggp = ggplot(data = data.final, aes(x = daym, y = stat)) +
  facet_wrap(. ~ type, ncol = 1, scales = "free") +
  geom_bar(stat = "identity", width=.5, position = "dodge") +
  xlab("Day of Month (May)") +
  ylab("") +
  geom_vline(xintercept = 20.5, linetype = "dotted", size = 1, col = "black") +
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(breaks = integer_breaks()) +
  scale_x_continuous(breaks = 1:31) +
  theme(strip.text.x = element_text(size = 15))


png('pelagics_over_month.jpeg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()
