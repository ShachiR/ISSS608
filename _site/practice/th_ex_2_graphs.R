packages = c('tidyverse', 'ggdist', 'ggridges',
             'patchwork', 'ggthemes', 'hrbrthemes',
             'ggrepel', 'ggforce',"HH","vcd",'scales', 'grid', 'gridExtra',
             'formattable','readr')
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

participants <- read_csv("data/Participants.csv")
participants$agegroup <- cut(participants$age, breaks = c(17,20,25,30,35,40,45,50,55,60), 
                             labels = c("18-20","20-25","26-30","31-35","36-40","41-45","45-50","50-55","55-60"))
# graph 1
ggplot(data = participants,
       aes(x = agegroup)) +
  geom_bar() +
  xlab("Age Group (years)") +
  ylab("Count") +
  geom_text(stat="count", 
            aes(label = paste0(..count..), vjust=-1)) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 200))+
  ggtitle("Distribution of Age among participants")

participants <- read_csv("data/Participants.csv")
participants$agegroupmod <- cut(participants$age, breaks = c(17,25,35,45,55,60), 
                             labels = c("18-25","26-35","36-45","46-55","56+"))
library(dplyr)


#graph 1 modification
ggplot(data = participants,
       aes(x = agegroupmod)) +
  geom_bar(alpha=0.5, fill = 4) +
  xlab("Age Group (years)") +
  ylab("Count of participants")  +
  geom_line(aes(group=1),stat="count",colour="black")+
  geom_point(aes(),stat="count",colour="red")+
  geom_text(stat="count", 
            aes(label = paste0(round(..count../sum(..count..)*100,1), "%"),hjust = 0.5,vjust=-1)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "light grey")) +
  scale_y_continuous(breaks = seq(0, 280, by = 30), limits = c(0, 280))+
  ggtitle("Distribution of Age among participants")

##
#participants_count<- participants %>% group_by(agegroupmod) %>%tally()

#Graph 2

ggplot(data = participants,
       aes(x = agegroup, fill = haveKids)) +
  geom_bar(alpha= 0.6)+
  geom_line(aes(group=1),stat="count",colour="black",size=0.7)+
  geom_point(aes(),stat="count",colour="red")+
  geom_text(stat="count", 
            aes(label = paste0(round(..count..,1)),hjust = 0.5,vjust=-1)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey")) +
  scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 140))+
  ggtitle("Distribution of Age among participants")+
  facet_grid(~ haveKids)

#Frequency count by age group and sort
freq_agegroup <- participants %>%
  group_by(`agegroup`) %>%
  summarise('participants' = n()) %>%
  ungroup()%>%
  arrange(desc(participants))
paged_table(freq_agegroup)

freq_cum_ag <- freq_agegroup %>%
  mutate(cumfreq = cumsum(participants)/sum(participants)*100)

paged_table(freq_cum_ag)
### graph 2 pareto chart
coeff <- 0.4
ggplot(data=freq_cum_ag, 
             aes(x = reorder(`agegroupmod`, -participants), y = participants)) +
  geom_col(fill = "light blue") +
  labs(x = "Age Group", title = "Pareto Chart of participants by age group") +
  geom_point(aes(y = `cumfreq`/coeff), colour = 'orange', size = 2) +
  geom_line(aes(y = `cumfreq`/coeff), colour = 'orange', group = 1) +
  geom_hline(yintercept = 80/coeff, colour = 'dark grey', linetype = 'dashed') +
  scale_y_continuous(name =  "No. of participants", breaks = seq(0, 1000, 50), 
                     sec.axis = sec_axis(~.*coeff, name = "Percentage of cumulative sum of participants (%)")) +
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 0.5)) + 
  #annotate("text", x='56 and over', y = 85/coeff, label = "80%", colour = "dark grey")

  ## Graph 3

participants <- read_csv("data/Participants.csv")
participants$agegroupmod <- cut(participants$age, breaks = c(17,25,35,45,55,60), 
                                labels = c("18-25","26-35","36-45","46-55","56+"))
library(dplyr)
df_noKids = participants %>% group_by(agegroupmod,haveKids) %>%
    filter(`haveKids` ==  FALSE)%>%
    summarise(joviality = mean(joviality),
              .groups = 'drop')

df_Kids = participants %>% group_by(agegroupmod,haveKids) %>%
  filter(`haveKids` ==  TRUE)%>%
  summarise(joviality = mean(joviality),
            .groups = 'drop')

ggplot(data = df_noKids,aes(x = agegroupmod, y=joviality)) +
  geom_point(aes(x=agegroupmod, y=joviality), 
             colour=alpha('red', 1), size=5) + 
  geom_line(aes(x=agegroupmod, y=joviality, group=1), 
            colour=alpha('red', 1), size=2) + 
  geom_text(data = df_noKids, aes(y = joviality, label = paste(round(joviality,2)),
                                  hjust = 0.5,vjust=-1))+
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1), limits = c(0, 0.8))+
  ggplot(data = df_Kids,aes(x = agegroupmod, y=joviality)) +
    geom_point(aes(x=agegroupmod, y=joviality), 
               colour=alpha('blue', 1), size=5) + 
    geom_line(aes(x=agegroupmod, y=joviality, group=1), 
              colour=alpha('blue', 1), size=2) + 
    geom_text(data = df_Kids, aes(y = joviality, label = paste(round(joviality,2)),
                                    hjust = 0.5,vjust=-1))+
    scale_y_continuous(breaks = seq(0, 0.8, by = 0.1), limits = c(0, 0.8))
  
# modified graph 3

df = participants %>% group_by(agegroupmod,haveKids) %>%
  summarise(joviality = mean(joviality), 
            .groups = 'drop')

ggplot(data= df, 
       aes(x= agegroupmod, y= joviality, 
           group= haveKids, 
           color= factor(haveKids))) +
  geom_line(size=1.5) +
  geom_point(aes(x=agegroupmod, y=joviality), 
             colour=alpha('red', 1), size=5) + 
  scale_color_discrete(name= 'Have Kids') +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1), 
                     limits = c(0, 0.8))+
  geom_text(aes(y = joviality, label = paste(round(joviality,2)),
                                hjust = 0.5,vjust=-1))
  
# modified graph 4

df = participants %>% group_by(agegroupmod,haveKids) %>%
  summarise(joviality = mean(joviality), 
            .groups = 'drop')

ggplot(data= df, 
       aes(x= agegroupmod, y= joviality, 
           group= haveKids, 
           color= factor(haveKids))) +
  geom_line(size=1.5) +
  scale_color_discrete(name= 'Have Kids') +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1), 
                     limits = c(0, 0.8))+
  theme_bw()

##graph 2 additional 

participants_data_ag_haveKids <- participants %>%
  filter(`haveKids` ==  TRUE) %>%
  mutate (householdSize = -householdSize)


participants_data_ag_noKids <-participants %>%
  filter(`haveKids` ==  FALSE)

participants_data_ag_byKids <- rbind(participants_data_ag_haveKids, participants_data_ag_noKids)

ggplot(participants_data_ag_byKids, aes (x = agegroupmod, y = householdSize , fill = haveKids)) +
  geom_bar(stat = "identity", alpha= 0.7) +
  coord_flip()+
  scale_y_continuous(breaks = seq(-250, 250, 50), 
                     labels = paste0(as.character(c(seq(250, 0, -50), seq(50, 250, 50)))), 
                     name = "Household Size")+
  labs(x = "Age Group", title = "Household size by age groups and whether have kids")+
  theme_bw() 


# Graph 5 original

df = participants %>% group_by(agegroupmod,educationLevel) %>%
  summarise(joviality = mean(joviality), 
            .groups = 'drop')
ggplot(data= participants, 
       aes(x= agegroupmod, y= joviality, 
           group= educationLevel, 
           fill=educationLevel)) +
  geom_col() +
  scale_color_discrete(name= 'Education Level') +
  scale_y_continuous(labels = percent_format())+
  theme_bw()

#graph 5 modified




participants$educationLevel <-factor(participants_data$educationLevel,ordered=TRUE,levels=c('Low','HighSchoolOrCollege',"Bachelors","Graduate"))
agg_happy <- participants %>% 
  select(c("educationLevel","joviality")) %>%
  group_by(educationLevel) %>% 
  summarise(joviality=mean(joviality))

happy_sorted <- agg_happy %>%
  arrange(desc(educationLevel))

ggplot(participants, aes(x = joviality, y = educationLevel, fill = educationLevel)) +
  geom_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE,
                      quantiles = 4, 
                      quantile_lines = TRUE,
                      alpha = .2) +
  theme_ridges() + 
  scale_fill_viridis_d(name = "Quartiles")+
  ggtitle("Distribution of joviality in different Education Level")+
  theme(plot.title = element_text(size = 12))