#Plotting PLEX categories
datViolinFormat <- data.frame(
  Condition=character(),
  Duration=numeric(),
  Experience=numeric(),
  Group=character(),
  stringsAsFactors=FALSE
) 
datViolinFormat <- lapply(colnames(dat)[7:18],
                          function(col){ 
                            return(
                              dat %>% 
                                mutate(
                                  Condition = cond,
                                  Duration = cond_dur,
                                  Group = col,
                                  Experience= get(col)
                                ) %>% 
                                select(Condition, Duration, Experience, Group) 
                            )}
) %>% bind_rows()

ggplot(
  datViolinFormat %>% filter(Condition=='Exp'), 
  aes(
    fill=as.factor(Experience), 
    color=as.factor(Experience),
    x=Group, 
    y=Duration) 
) + 
  geom_boxplot(width=0.5, alpha=0.5, outlier.shape=NA, lwd=0.1, fatten=15) +
  scale_color_manual("Participants experienced", values=c("#969696", "#252525"), labels=c('No', 'Yes')) + 
  scale_fill_manual("Participants experienced", values=c("#969696", "#252525"), labels=c('No', 'Yes')) + 
  theme_bw() + ylab('Duration (mins)') + xlab('Playful experiences') +
  theme(
    axis.text.x = element_text(angle = 70, vjust = 1, hjust=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")
  ) 

ggplot(
  datViolinFormat %>% filter(Condition=='Ctr'), 
  aes(
    fill=as.factor(Experience), 
    color=as.factor(Experience),
    x=Group, 
    y=Duration) 
) + 
  geom_boxplot(width=0.5, alpha=0.5, outlier.shape=NA, lwd=0.1, fatten=15) +
  scale_color_manual("Participants experienced", values=c("#969696", "#252525"), labels=c('No', 'Yes')) + 
  scale_fill_manual("Participants experienced", values=c("#969696", "#252525"), labels=c('No', 'Yes')) + 
  theme_bw() + ylab('Duration (mins)') + xlab('Playful experiences') +
  theme(
    axis.text.x = element_text(angle = 70, vjust = 1, hjust=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")
  ) 

# Scores on each research problem

randomSolutions <- allInterventions %>%
  filter(Group=='Random guessing') %>%
  group_by(Goal) %>%
  summarise(
    mean=mean(Score, na.rm=TRUE),
    sd=ifelse( is.na(sd(Score)), 0, sd(Score)),
    conf_upper=min(100, mean(Score)+( ifelse( is.na(sd(Score)), 0, sd(Score)) /2) ),
    conf_lower=max(0, mean(Score)-( ifelse( is.na(sd(Score)), 0, sd(Score)) /2) ),
    min=min(Score),
    max=max(Score)
  )
adjustedScores <- allInterventions %>%
  filter(Group=='Players solutions') %>%
  group_by(Goal, Group) %>%
  summarise(
    mean=mean(Score, na.rm=TRUE),
    sd=ifelse( is.na(sd(Score)), 0, sd(Score)),
    serr = sd(Score)/sqrt(n()),
    conf_upper=min(100, mean(Score)+ (sd(Score)/sqrt(n())) ),
    conf_lower=max(0, mean(Score)- (sd(Score)/sqrt(n())) ),
    min=min(Score),
    max=max(Score)
  ) %>%
  left_join(
    randomSolutions %>% select(Goal, randomMean = mean)
  ) %>%
  mutate(adjustedScore = mean - randomMean)

ggplot( adjustedScores %>% arrange(adjustedScore) ) +
  geom_errorbar( aes(x=Goal, ymax=adjustedScore+serr, ymin=adjustedScore-serr) ) +
  geom_point( aes(y=adjustedScore, x=Goal) ) +
  theme_bw() + ylab('Score above baseline (%)') + xlab('Goals') +
  ylim(-10,50) +
  theme(
    axis.text.x = element_text(angle = 70, vjust = 1, hjust=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")
  ) 

# Duration x plex gamma relation
ggplot(dat, aes(x=PLEX_count,y=cond_dur, group=PLEX_count) ) +
  geom_boxplot()

#Usability

ggplot( Q %>% filter(Usability<55) ) + 
  geom_density( aes(x = Usability, y = ..density.., fill = cond, colour = cond), alpha=.6) +
  scale_fill_manual(values=c("#d8d8d8", "#6cafb4")) +
  scale_colour_manual(values=c("#d8d8d8", "#6cafb4")) +
  theme_bw() + xlab('Usability (%)') + ylab('Participants (proportion)') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggplot( Q %>% filter(Usability>60) ) + 
  geom_density( aes(x = Usability, y = ..density.., fill = cond, colour = cond), alpha=.6) +
  scale_fill_manual(values=c("#d8d8d8", "#6cafb4")) +
  scale_colour_manual(values=c("#d8d8d8", "#6cafb4")) +
  theme_bw() + xlab('Usability (%)') + ylab('Participants (proportion)') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#Player scores
ggplot( rbind( playerMeans, randomMeans) ) + 
  geom_density( aes(x = score, y = ..density.., color=group, fill=group), alpha=.6) +
  scale_fill_manual(values=c("#6cafb4", "#d8d8d8")) +
  scale_colour_manual(values=c("#6cafb4", "#d8d8d8")) +
  theme_bw() + xlab('Mean score (%)') + ylab('Frequency') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Mean scores per minute in game

ggplot( learningInGame_dat ) +
  geom_density( aes(x = mean, y = ..density..), alpha=.6) +
  scale_fill_manual(values=c("#d8d8d8", "#6cafb4")) +
  scale_colour_manual(values=c("#d8d8d8", "#6cafb4")) +
  theme_bw() + xlab('Mean score per minute (%)') + ylab('Frequency') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#Old Method
# lapply(colnames(dat)[7:18],
#        function(col){
#          ggsave(
#            paste('outputs/plots/PLEX/',col,'.png', sep=''),
#            plot=ggplot(dat %>% replace(, w=="a" & x==2, 9), aes(x=get(col), y=cond_dur)) + 
#              geom_point() + 
#              geom_jitter(position=position_jitter(0.2)) + 
#              geom_smooth(method = "glm", se = TRUE) +
#              theme_bw() + xlab(col) + ylab('Duration (mins)') + 
#              scale_x_continuous(breaks= c(0,1), labels=c('No', 'Yes')) +
#              theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#          )
#        }
# )