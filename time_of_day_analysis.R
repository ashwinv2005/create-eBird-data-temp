require(tidyverse)
require(stringr)
require(ggthemes)
require(extrafont)

theme_set(theme_tufte())

load("allGBBCdata.RData")
GBBCdata = unique(data$group.id)

load("data.RData")

GBBCdata1 = data %>% filter(group.id %in% GBBCdata)
GBBCdata1 = GBBCdata1 %>% group_by(group.id) %>% slice(1)

data1 = data %>% group_by(group.id) %>% slice(1)
data1 = setdiff(data1,GBBCdata1)

GBBCdata1$pyear = "GBBC"
 
data1 = data1 %>% filter(cyear > 2014,cyear < 2020)
data1$pyear = as.character(data1$cyear)
data1 = rbind(data1,GBBCdata1)

sp1 = str_split_fixed(data1$TIME.OBSERVATIONS.STARTED,":",3)
data1$hr = as.numeric(sp1[,1])
data1$min = as.numeric(sp1[,2])
data1$time = data1$hr*60+data1$min


ggp = ggplot(data1, aes(x=time)) + 
  facet_wrap(.~pyear, nrow = 3, ncol = 2) +
  geom_histogram(aes(y = ..density..), binwidth = 15, color="black", fill="white") +
  xlab("hrs") +
  ylab("proportion of checklists")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(60,120,180,240,300,360,420,480,540,600,660,720,780,840,900,960,1020,1080,1140,
                                1200,1260,1320,1380,1440),
                     labels = seq(1,24,1))
)

