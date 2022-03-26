library(tidyverse)


## ANES CDF
cdf <- rio::import("~/Dropbox/anes_timeseries_cdf_spss_20211118.sav")


table(cdf$VCF0004)
cdf <- cdf %>% mutate(pid3= case_when(VCF0301 %in% c(1,2,3) ~ 'Democrat',
                               VCF0301 %in% c(5,6,7) ~ 'Republican',
                               TRUE~'Independent'))

library(ggridges)
library(mousetrap)
VCF0809
table(cdf$VCF0838)
table(cdf$VCF0839)
cdf$VCF0839[cdf$VCF0839>7]=NA
cdf %>% group_by(VCF0004) %>% summarise(bimodality_coefficient(VCF0839)) -> out

table(sjlabelled::la(cdf$VCF0839))
table(cdf$VCF0809)
library(ggridges)
library(scales)
ggplot(cdf %>% filter(VCF0004>1980),aes(x=VCF0839))  + ylab("Year") + theme_bw(base_size = 15) + scale_x_continuous(breaks=1:7,labels=c("Decreased",2:6,"Increased")) + xlab("Should government services be increased or decreased?") + facet_wrap(~VCF0004) + geom_density()+scale_y_continuous(labels = percent )+theme(axis.text.x = element_text(angle = 90,hjust = 1))

ggsave("Figures/governmentservices.png",width=12,height=12)

table(cdf$VCF0830)
ggplot(cdf %>% filter(VCF0004>1980),aes(x=VCF0830))  + ylab("Year") + theme_bw(base_size = 15) + scale_x_continuous(breaks=1:7,labels=c("Govt help",2:6,"Groups help themselves"))  + facet_wrap(~VCF0004) + geom_density()+scale_y_continuous(labels = percent )+theme(axis.text.x = element_text(angle = 90,hjust = 1)) +xlab("")


ggsave("Figures/minorities.png",width=12,height=12)

out <- cdf %>% group_by(pid3,VCF0004) %>% summarise(ideo_dem=mean(VCF0503,na.rm=T),ideo_rep=mean(VCF0504,na.rm=T),ideo_self=mean(VCF0803,na.rm=T))

m1 <- na.omit(out) %>% filter(pid3=='Republican'|pid3=='Democrat') %>% select(ideo_self,VCF0004)

all <- m1 %>% pivot_wider(id_cols = VCF0004,names_from = pid3,values_from = ideo_self) %>% mutate(diff=abs(Democrat-Republican)) %>% select(VCF0004,diff) %>% mutate(pid3='Actual')

m1 <- na.omit(out) %>% filter(pid3=='Republican') %>% select(ideo_rep,ideo_dem,VCF0004) %>% mutate(diff=abs(ideo_rep-ideo_dem)) %>% select(VCF0004,diff)
m2 <- na.omit(out) %>% filter(pid3=='Democrat') %>% select(ideo_rep,ideo_dem,VCF0004) %>% mutate(diff=abs(ideo_rep-ideo_dem)) %>% select(VCF0004,diff)

forplot <- rbind(m1,m2,all)


m1 %>% pivot_wider(id_cols = VCF0004,names_from = pid3,values_from = ideo_self) -> actual_polarization

ggplot(forplot,aes(x=VCF0004,y=diff,lty=pid3)) + geom_line() 

#############
out <- cdf %>% group_by(pid3,VCF0004) %>% summarise(ideo_dem=mean(VCF0508,na.rm=T),ideo_rep=mean(VCF0509,na.rm=T),ideo_self=mean(VCF0806,na.rm=T))

m1 <- na.omit(out) %>% filter(pid3=='Republican'|pid3=='Democrat') %>% select(ideo_self,VCF0004)

all <- m1 %>% pivot_wider(id_cols = VCF0004,names_from = pid3,values_from = ideo_self) %>% mutate(diff=abs(Democrat-Republican)) %>% select(VCF0004,diff) %>% mutate(pid3='Actual')

m1 <- na.omit(out) %>% filter(pid3=='Republican') %>% select(ideo_rep,ideo_dem,VCF0004) %>% mutate(diff=abs(ideo_rep-ideo_dem)) %>% select(VCF0004,diff)
m2 <- na.omit(out) %>% filter(pid3=='Democrat') %>% select(ideo_rep,ideo_dem,VCF0004) %>% mutate(diff=abs(ideo_rep-ideo_dem)) %>% select(VCF0004,diff)

forplot <- rbind(m1,m2,all)


m1 %>% pivot_wider(id_cols = VCF0004,names_from = pid3,values_from = ideo_self) -> actual_polarization

ggplot(forplot,aes(x=VCF0004,y=diff,lty=pid3)) + geom_line() + geom_point()

#####
#############
out <- cdf %>% group_by(pid3,VCF0004) %>% summarise(ideo_dem=mean(VCF0513,na.rm=T),ideo_rep=mean(VCF0514,na.rm=T),ideo_self=mean(VCF0809,na.rm=T))

m1 <- na.omit(out) %>% filter(pid3=='Republican'|pid3=='Democrat') %>% select(ideo_self,VCF0004)

all <- m1 %>% pivot_wider(id_cols = VCF0004,names_from = pid3,values_from = ideo_self) %>% mutate(diff=abs(Democrat-Republican)) %>% select(VCF0004,diff) %>% mutate(pid3='Actual')

m1 <- na.omit(out) %>% filter(pid3=='Republican') %>% select(ideo_rep,ideo_dem,VCF0004) %>% mutate(diff=abs(ideo_rep-ideo_dem)) %>% select(VCF0004,diff)
m2 <- na.omit(out) %>% filter(pid3=='Democrat') %>% select(ideo_rep,ideo_dem,VCF0004) %>% mutate(diff=abs(ideo_rep-ideo_dem)) %>% select(VCF0004,diff)

forplot <- rbind(m1,m2,all)



ggplot(forplot,aes(x=VCF0004,y=diff,lty=pid3)) + geom_line() + geom_point() + theme_bw()
