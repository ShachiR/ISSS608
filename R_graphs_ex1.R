packages = c('tidyverse', 'ggdist', 'ggridges',
             'patchwork', 'ggthemes', 'hrbrthemes',
             'ggrepel', 'ggforce',"HH","vcd",'scales', 'grid', 'gridExtra',
             'formattable')
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


#'tidyverse','ggplot', 'ggdist', 'gghalves', 'ggridges','knitr', 'ggpubr', 'patchwork', 'readr'
participants <- read_csv("data/Participants.csv")

# DATA WRANGLING
#student 1
participants <- participants %>%
  mutate(age_category=cut(age, 
                          breaks=c(17, 30, 40, 50, 60),
                          labels=c('30 and below','31-40','41-50','51 and above')))
ggplot(data=participants, 
       aes(x = joviality, colour = haveKids)) +
  geom_density() +
  ggtitle("Figure 1: What is the distribution of Joviality?")

ggplot(data=participants, aes(y = joviality, 
                              x= reorder(interestGroup, joviality, FUN = mean))) +
  xlab("interestGroup") +
  geom_boxplot() +
  stat_summary(geom = "point",
               fun.y="mean",
               colour ="red",
               size=2) +
  stat_summary(geom = "text",
               aes(label=paste("mean=",round(..y..,2))),
               fun.y="mean",
               colour="red",
               size=2,
               vjust = -2) +
  ggtitle("Figure 6: Which interest group is the most jovial?")

# student 2
ggplot(data = participants,
       aes(x = haveKids)) +
  geom_bar() +
  geom_text(aes(label=..count..),
            stat="count",
            vjust=1.5,
            color="white") +
  xlab("Residents Have Kids") +
  ylab("No. of\nResidents") +
  ggtitle("Only 30% of Engagement Residents Have Kids",
          subtitle = "A survey of 1011 Residents Taken in 2022") +
  theme_ipsum(plot_title_size = 14,
              axis_title_face = 24,
              base_size=10,
              grid="Y")


ggplot(data = demographics,
       aes(x = joviality*100))+
  geom_histogram(bins=20) +
  facet_grid(~ age_category) +
  xlab("Happiness Level Percentage") +
  ylab("No. of\nResidents") +
  scale_x_continuous(breaks = c(0,50,100)) +
  ggtitle("Elderlies are Getting Unhappy",
          subtitle="Happiness Level Across Different Age Groups") +
  theme_ipsum(plot_title_size = 14,
              axis_title_face = 24,
              strip_text_size = 10,
              base_size=10,
              plot_margin = margin(5,5,5,5))

# student 3

ggplot(data = participants,
       aes(x=reorder(householdSize, householdSize, function(x)-length(x)))) +
  geom_bar() +
  ylim(0,550) +
  geom_text(stat="count", 
            aes(label=paste0(..count.., " (", 
                             round(..count../sum(..count..)*100,
                                   1), "%)")),
            vjust=-1) +
  xlab("Household Size") +
  ylab("No. of\nParticipants") +
  ggtitle("Household Size of Participants")

# student 5
participants$educationLevel <- factor(participants$educationLevel,
                                      levels = c("Low", "HighSchoolOrCollege",
                                                 "Bachelors", "Graduate"))
ggplot(data=participants, 
       aes(y = joviality, x = educationLevel,fill = educationLevel)) +
  geom_boxplot() + stat_summary(geom = "point",fun="mean")

#cut
participants$agegroup <- cut(participants$age, breaks = c(17,30,40,50,60), 
                             labels = c("18-30","30-40","40-50","50-60"))

ggplot(data=participants, 
       aes(x =joviality,colour = haveKids)) +
  geom_density()+
  facet_wrap(~ agegroup,nrow = 2)

participants$educationLevel <- factor(participants$educationLevel,
                                      levels =  c("Low", "HighSchoolOrCollege", 
                                                  "Bachelors", "Graduate"))

participants %>%
  group_by(householdSize, haveKids) %>%
  summarise(n = n()) %>%
  ggplot(aes(fill=haveKids, x=householdSize, y=n)) + 
  geom_col() +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x="Household Size", y="Count",
       title = "Household Size versus Kids", fill = "Have Kids?") +
  theme_minimal()

participants %>%
  group_by(educationLevel, haveKids) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n),3)) %>%
  ggplot(aes(fill=haveKids, x=educationLevel, y=freq)) + 
  geom_col() +
  geom_text(aes(label = freq), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x="Education Level", y="Frequency",
       title = "Education Level versus Kids", fill = "Have Kids?") +
  geom_errorbar(data=df.mean, aes(x, ymax = ymean, ymin = ymean),
                size=0.5, linetype = "longdash", inherit.aes = F, width = 1)
  theme_minimal()


participants %>% count(interestGroup) %>% 
  ggplot(aes(x=reorder(interestGroup, n, fun=sum), y=n)) + 
  geom_col(fill="#00BFC4") +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5)) +
  geom_hline(aes(yintercept = mean(n), color = "mean")) +
  labs(x="Interest Group", y="Count", title="Interest Group Count Plot", 
       color = "") +
  theme_minimal()

#Anne

participants_data$educationLevel <-factor(participants_data$educationLevel,ordered=TRUE,levels=c('Low','HighSchoolOrCollege',"Bachelors","Graduate"))
agg_happy <- participants_data %>% 
  select(c("educationLevel","joviality")) %>%
  group_by(educationLevel) %>% 
  summarise(joviality=mean(joviality))

happy_sorted <- agg_happy %>%
  arrange(desc(educationLevel))

ggplot(participants_data, aes(x = joviality, y = educationLevel, fill = educationLevel)) +
  geom_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE,
                      quantiles = 4, 
                      quantile_lines = TRUE,
                      alpha = .2) +
  theme_ridges() + 
  scale_fill_viridis_d(name = "Quartiles")+
  ggtitle("Distribution of joviality in different Education Level")+
  theme(plot.title = element_text(size = 12))

ggplot(data=happy_sorted,
       aes(y = joviality,
           x= educationLevel,
           group=1)) +
  geom_line(linetype = "dashed",color='black',arrow = arrow(type = "closed"))+
  geom_point(stat = "identity",
             position = "identity")+
  ggtitle("Change of average joviality when Education Level increase")+
  annotate("text", 
           x = 4, 
           y = 0.5, 
           label = "People tend \n to be happier\n at the start of study with\n higher education level",size=3,color='#4682B4') + 
  theme(plot.title = element_text(size = 12))

ggplot(data=participants_data,
       aes(x= joviality,
           fill = haveKids)) +
  geom_histogram(alpha=0.2) +
  annotate("text", x = 0.7, y = 20, label = "People with kids tend\n to be happier at the start of the study",size=3,color='red') + 
  ggtitle("Distribution of joviality with and without kids")+
  theme(plot.title = element_text(size = 10))

##Sulfizan
participants_data$householdSize <- as.factor(participants_data$householdSize)
ggplot(data = participants_data,
       aes(x = age)) +
  geom_bar(fill = "navy") +
  xlab("Age") +
  ylab("Count") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey")) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  ggtitle("Distribution of Ohio participants by Age") +
  geom_vline(aes(xintercept=mean(age,na.rm=T)),
             color="red", linetype="dashed", size=1) +
  geom_text(aes(x=40, label="mean = 39.07", y=30), colour="red", angle=90, text=element_text(size=9))

#juniqu
dsbc=likert(Type ~ . | Subtable, data = participants, as.percent = TRUE, positive.order = TRUE, 
            scales = list(y = list(relation = "free")), layout = c(1, 6))
dsbc

ggplot(participants,aes(x=factor(age,levels=c("Below 30","30-39","40-49","50 and above"))))+
  geom_bar(fill="pink")+
  geom_line(aes(group=1),stat="count",colour="darkblue")+
  geom_point(aes(),stat="count",colour="black")+
  geom_text(stat="count",aes(label=paste0(round(..count../sum(..count..)*100,2),"%")),vjust=-1)+
  ylim(0,350)+
  xlab("Age Group")+
  ylab("Frequency")+
  ggtitle("How old is the crowd?") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

packages = c('tidyverse', 'knitr', 'ggdist', 'scales', 'grid', 'gridExtra',
             'patchwork', 'formattable')
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

participants <- read_csv('data/Participants.csv')
financial <- read_csv('data/FinancialJournal.csv')


summary(participants)
summary(financial)


income <- financial %>% 
  filter(category == 'Wage') %>% # extract only wage data
  select(participantId, amount) %>% # extract participant ID and amount columns
  group_by(participantId) %>% # group by participant ID
  summarise(Monthly_Income = sum(amount)/15) 
# calculate average monthly income for each participant

# check the derived file income 
summary(income)

participants <- inner_join(x= participants, y= income, by= 'participantId')

# confirm tables are joined correctly
head(participants)

participants <- participants %>%
  rename('Participant_ID' = 'participantId', 
         'Household_Size' = 'householdSize', 
         'Have_Kids' = 'haveKids', 
         'Age' = 'age', 
         'Education_Level' = 'educationLevel', 
         'Interest_Group' = 'interestGroup', 
         'Joviality' = 'joviality')

# verify if the columns have been renamed correctly 
colnames(participants)

#rename value 
participants$Education_Level <- sub('HighSchoolOrCollege', 
                                    'High School or College',
                                    participants$Education_Level)

# check min and max ages 
summary(participants$Age)

# binning

brks <- c(17, 20, 25, 30, 35, 40, 45, 50, 55, 60)
grps <- c('20 & Below', '21-25', '26-30', '31-35', '36-40', '41-45', 
          '46-50', '51-55', '56-60')

participants$Age_Group <- cut(participants$Age, breaks=brks, labels = grps)

ggplot(data= participants, 
       aes(x= Age_Group)) +
  geom_bar(fill= '#468499') +
  ylim(0, 150) +
  geom_text(stat = 'count',
            aes(label= paste0(stat(count), ', ', 
                              round(stat(count)/sum(stat(count))*100, 
                                    1), '%')), vjust= -0.5, size= 2.5) +
  labs(y= 'No. of\nResidents', title = "Distribution of Residents' Age") +
  theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
        panel.background= element_blank(), axis.line= element_line(color= 'grey'))

ggplot(data= participants,
       aes(x= Household_Size,
           fill = Have_Kids)) +
  geom_bar()+
  ylim(0, 400) +
  geom_text(stat = 'count',
            aes(label= stat(count)), 
            vjust= -0.5, 
            size= 3) +
  labs(title = 'Household Size of the Residents', y= 'No of\nResidents') +
  theme(axis.title.y= element_text(angle=0), 
        axis.ticks.x= element_blank(),
        panel.background= element_blank(), 
        axis.line= element_line(color= 'grey'))

ggplot(data= participants,
       aes(x= Household_Size,
           fill = Have_Kids)) +
  geom_bar()+
  ylim(0, 400) +
  geom_text(stat = 'count',
            aes(label= stat(count)), 
            vjust= -0.5, 
            size= 3) +
  labs(title = 'Household Size of the Residents', y= 'No of\nResidents') +
  theme(axis.title.y= element_text(angle=0), 
        axis.ticks.x= element_blank(),
        panel.background= element_blank(), 
        axis.line= element_line(color= 'grey'))

participants %>%
  mutate(Education= fct_infreq(Education_Level)) %>%
  ggplot(aes(x= Education)) +
  geom_bar(fill= '#6897bb') +
  geom_text(stat = 'count',
            aes(label= paste0(stat(count), ', ', 
                              round(stat(count)/sum(stat(count))*100, 
                                    1), '%')), vjust= -0.5, size= 3) +
  labs(y= 'No. of\nResidents', title = "Distribution of Residents' Education Level") +
  theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
        panel.background= element_blank(), axis.line= element_line(color= 'grey'))

EduLevels <- c('Low', 'High School or College', 'Bachelors', 'Graduate')

ggplot(data=participants,
       aes(x= as.factor(Household_Size), y= Age)) +
  geom_violin(fill= '#66cdaa',
              scale = 'count',
              color= NA,
              bw= 0.4) +
  geom_boxplot(width= 0.2,
               color = '#065535',
               alpha= 0.3) +
  stat_summary(geom= 'point',
               fun= 'mean',
               color= '#ff7373',
               size= 2) + 
  facet_grid(~factor(Education_Level, levels = EduLevels)) +
  labs(title= 'Age Distribution for Different Household Sizes', 
       x= 'Household Size') +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line= element_line(color= 'grey'),
        panel.grid.major.y = element_line(color= 'grey', size = 0.1))

# plot p1: bar chart of interest group distribution in descending order
number <- 
  participants %>%
  mutate(Interest= fct_infreq(Interest_Group)) 

p1 <-  
  ggplot(data= number, aes(x= Interest)) +
  geom_bar(fill= '#468499') +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  labs(y= 'No. of\nResidents', 
       title = "Total Number of Residents in Interest Groups") +
  theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
        panel.background= element_blank(), axis.line= element_line(color= 'grey'),
        panel.grid.major.y = element_line(size= 0.2, color = "grey"),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust=0.5))

# plot p2: composition of corresponding interest group in terms of education levels
int_lvl <- c('J', 'H', 'G', 'F', 'A', 'C', 'D', 'I', 'B', 'E')
percent <- 
  participants %>% 
  group_by(Interest_Group, Education_Level) %>%
  summarise(edu_size= n()) %>%
  mutate(edu_pct= percent(edu_size/sum(edu_size))) 

p2<- 
  ggplot(data= percent, 
         aes(x= factor(Interest_Group, levels = int_lvl), y= edu_pct, 
             group= Education_Level, 
             color= factor(Education_Level, levels = EduLevels))) +
  geom_line() +
  scale_color_discrete(name= 'Education Level') +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0.2, 0.2)) +
  labs(y= 'Percent', x= 'Interest Group', 
       title = "Composition of Residents' Education Level in Interest Groups")+
  theme(legend.position = 'top', legend.direction = 'horizontal',
        axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
        panel.background= element_blank(), axis.line= element_line(color= 'grey'),
        panel.grid.major = element_line(size= 0.2, color = "grey"),
        legend.key = element_rect(fill= NA), legend.title = element_text(size = 8.5),
        plot.title = element_text(hjust=0.5))

# use patchwork to stack 2 graphs 
p1/p2

ggplot(participants, 
       aes(x= fct_rev(Interest_Group), y= Joviality)) +
  stat_halfeye(adjust = .35,
               width = .6,
               color = '#20b2aa',
               justification = -.15,
               position = position_nudge(x = .12)) +
  scale_x_discrete(expand= c(0.1, 0.1)) +
  geom_hline(aes(yintercept = 0.5),
             linetype= 'dashed',
             color= '#f08080',
             size= .6) +
  coord_flip() +
  labs(x = 'Interest Group',
       title = 'Joviality Distribution in Different Interest Groups') +
  theme(panel.background= element_blank(), axis.line= element_line(color= 'grey'),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_line(size= 0.2, color = "grey"))

ggplot(data= participants,
       aes(x= Monthly_Income, y= Joviality, color= Joviality)) +
  scale_color_gradient(low= '#133337', high = '#a0db8e') +
  geom_point() +
  scale_x_continuous(breaks= c(0,5000,10000,15000),
                     labels = c('0', '5k', '10k', '15k')) +
  geom_vline(aes(xintercept = median(Monthly_Income,
                                     na.rm = T)),
             color= 'red',
             linetype= 'dashed',
             size= .6) +
  geom_text(aes(median(Monthly_Income), 1, 
                label= 'Median Monthly Income', hjust= -0.1)) +
  geom_hline(aes(yintercept = 0.5),
             color= 'red',
             linetype= 'dashed',
             size= .6)+
  labs(title = "Joviality versus Monthly Income")+
  theme(panel.background= element_blank(), axis.line= element_line(color= 'grey'),
        panel.grid.major = element_line(size= 0.2, color = "grey"))


###BEST
packages = c('tidyverse', 'knitr', 'ggdist', 'scales', 'grid', 'gridExtra',
             'patchwork', 'formattable')
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

participants <- read_csv('data/Participants.csv')
financial <- read_csv('data/FinancialJournal.csv')


summary(participants)
summary(financial)


income <- financial %>% 
  filter(category == 'Wage') %>% # extract only wage data
  select(participantId, amount) %>% # extract participant ID and amount columns
  group_by(participantId) %>% # group by participant ID
  summarise(Monthly_Income = sum(amount)/15) 
# calculate average monthly income for each participant

# check the derived file income 
summary(income)

participants <- inner_join(x= participants, y= income, by= 'participantId')

# confirm tables are joined correctly
head(participants)

participants <- participants %>%
  rename('Participant_ID' = 'participantId', 
         'Household_Size' = 'householdSize', 
         'Have_Kids' = 'haveKids', 
         'Age' = 'age', 
         'Education_Level' = 'educationLevel', 
         'Interest_Group' = 'interestGroup', 
         'Joviality' = 'joviality')

# verify if the columns have been renamed correctly 
colnames(participants)

#rename value 
participants$Education_Level <- sub('HighSchoolOrCollege', 
                                    'High School or College',
                                    participants$Education_Level)

# check min and max ages 
summary(participants$Age)

# binning

brks <- c(17, 20, 25, 30, 35, 40, 45, 50, 55, 60)
grps <- c('20 & Below', '21-25', '26-30', '31-35', '36-40', '41-45', 
          '46-50', '51-55', '56-60')

participants$Age_Group <- cut(participants$Age, breaks=brks, labels = grps)

ggplot(data= participants, 
       aes(x= Age_Group)) +
  geom_bar(fill= '#468499') +
  ylim(0, 150) +
  geom_text(stat = 'count',
            aes(label= paste0(stat(count), ', ', 
                              round(stat(count)/sum(stat(count))*100, 
                                    1), '%')), vjust= -0.5, size= 2.5) +
  labs(y= 'No. of\nResidents', title = "Distribution of Residents' Age") +
  theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
        panel.background= element_blank(), axis.line= element_line(color= 'grey'))

ggplot(data= participants,
       aes(x= Household_Size,
           fill = Have_Kids)) +
  geom_bar()+
  ylim(0, 400) +
  geom_text(stat = 'count',
            aes(label= stat(count)), 
            vjust= -0.5, 
            size= 3) +
  labs(title = 'Household Size of the Residents', y= 'No of\nResidents') +
  theme(axis.title.y= element_text(angle=0), 
        axis.ticks.x= element_blank(),
        panel.background= element_blank(), 
        axis.line= element_line(color= 'grey'))

ggplot(data= participants,
       aes(x= Household_Size,
           fill = Have_Kids)) +
  geom_bar()+
  ylim(0, 400) +
  geom_text(stat = 'count',
            aes(label= stat(count)), 
            vjust= -0.5, 
            size= 3) +
  labs(title = 'Household Size of the Residents', y= 'No of\nResidents') +
  theme(axis.title.y= element_text(angle=0), 
        axis.ticks.x= element_blank(),
        panel.background= element_blank(), 
        axis.line= element_line(color= 'grey'))

participants %>%
  mutate(Education= fct_infreq(Education_Level)) %>%
  ggplot(aes(x= Education)) +
  geom_bar(fill= '#6897bb') +
  geom_text(stat = 'count',
            aes(label= paste0(stat(count), ', ', 
                              round(stat(count)/sum(stat(count))*100, 
                                    1), '%')), vjust= -0.5, size= 3) +
  labs(y= 'No. of\nResidents', title = "Distribution of Residents' Education Level") +
  theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
        panel.background= element_blank(), axis.line= element_line(color= 'grey'))

EduLevels <- c('Low', 'High School or College', 'Bachelors', 'Graduate')

ggplot(data=participants,
       aes(x= as.factor(Household_Size), y= Age)) +
  geom_violin(fill= '#66cdaa',
              scale = 'count',
              color= NA,
              bw= 0.4) +
  geom_boxplot(width= 0.2,
               color = '#065535',
               alpha= 0.3) +
  stat_summary(geom= 'point',
               fun= 'mean',
               color= '#ff7373',
               size= 2) + 
  facet_grid(~factor(Education_Level, levels = EduLevels)) +
  labs(title= 'Age Distribution for Different Household Sizes', 
       x= 'Household Size') +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line= element_line(color= 'grey'),
        panel.grid.major.y = element_line(color= 'grey', size = 0.1))

# plot p1: bar chart of interest group distribution in descending order
number <- 
  participants %>%
  mutate(Interest= fct_infreq(Interest_Group)) 

p1 <-  
  ggplot(data= number, aes(x= Interest)) +
  geom_bar(fill= '#468499') +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  labs(y= 'No. of\nResidents', 
       title = "Total Number of Residents in Interest Groups") +
  theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
        panel.background= element_blank(), axis.line= element_line(color= 'grey'),
        panel.grid.major.y = element_line(size= 0.2, color = "grey"),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust=0.5))

# plot p2: composition of corresponding interest group in terms of education levels
int_lvl <- c('J', 'H', 'G', 'F', 'A', 'C', 'D', 'I', 'B', 'E')
percent <- 
  participants %>% 
  group_by(Interest_Group, Education_Level) %>%
  summarise(edu_size= n()) %>%
  mutate(edu_pct= percent(edu_size/sum(edu_size))) 

p2<- 
  ggplot(data= percent, 
         aes(x= factor(Interest_Group, levels = int_lvl), y= edu_pct, 
             group= Education_Level, 
             color= factor(Education_Level, levels = EduLevels))) +
  geom_line() +
  scale_color_discrete(name= 'Education Level') +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0.2, 0.2)) +
  labs(y= 'Percent', x= 'Interest Group', 
       title = "Composition of Residents' Education Level in Interest Groups")+
  theme(legend.position = 'top', legend.direction = 'horizontal',
        axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
        panel.background= element_blank(), axis.line= element_line(color= 'grey'),
        panel.grid.major = element_line(size= 0.2, color = "grey"),
        legend.key = element_rect(fill= NA), legend.title = element_text(size = 8.5),
        plot.title = element_text(hjust=0.5))

# use patchwork to stack 2 graphs 
p1/p2

ggplot(participants, 
       aes(x= fct_rev(Interest_Group), y= Joviality)) +
  stat_halfeye(adjust = .35,
               width = .6,
               color = '#20b2aa',
               justification = -.15,
               position = position_nudge(x = .12)) +
  scale_x_discrete(expand= c(0.1, 0.1)) +
  geom_hline(aes(yintercept = 0.5),
             linetype= 'dashed',
             color= '#f08080',
             size= .6) +
  coord_flip() +
  labs(x = 'Interest Group',
       title = 'Joviality Distribution in Different Interest Groups') +
  theme(panel.background= element_blank(), axis.line= element_line(color= 'grey'),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_line(size= 0.2, color = "grey"))

ggplot(data= participants,
       aes(x= Monthly_Income, y= Joviality, color= Joviality)) +
  scale_color_gradient(low= '#133337', high = '#a0db8e') +
  geom_point() +
  scale_x_continuous(breaks= c(0,5000,10000,15000),
                     labels = c('0', '5k', '10k', '15k')) +
  geom_vline(aes(xintercept = median(Monthly_Income,
                                     na.rm = T)),
             color= 'red',
             linetype= 'dashed',
             size= .6) +
  geom_text(aes(median(Monthly_Income), 1, 
                label= 'Median Monthly Income', hjust= -0.1)) +
  geom_hline(aes(yintercept = 0.5),
             color= 'red',
             linetype= 'dashed',
             size= .6)+
  labs(title = "Joviality versus Monthly Income")+
  theme(panel.background= element_blank(), axis.line= element_line(color= 'grey'),
        panel.grid.major = element_line(size= 0.2, color = "grey"))