# Sensitivity - comparing time across measures
#Check timing calculations
timings <- gameplay %>% 
  group_by(user, tPct, condition) %>%
  summarise(
    condition=first(condition),
    user=first(user),
    tPct=first(tPct),
    t=first(t),
    n=n()
  )

#Calculate breaks between inputs
timejump <- gameplay %>%
  arrange(user, t) %>%
  mutate(
    t1 = ifelse(
      user==lag(user),
      t - lag(t),
      0
    )
  )

# Numbers of individuals taking breaks
plot(table(timejump %>% filter(condition=='game') %>% group_by(user) %>% summarise(t1_max = max(t1)) %>% select(t1_max) ))
plot(table(timejump %>% filter(condition=='iv') %>%  group_by(user) %>% summarise(t1_max = max(t1)) %>% select(t1_max) ))

# Summarise playtime excluding individuals with excessive breaks
timejump %>% 
  group_by(user, condition) %>% 
  summarise( 
    t1_max = max(t1, na.rm=TRUE),
    t_diff = first(t_diff)
  ) %>%
  group_by(condition) %>%
  summarise( 
    t_diff_all = sum(t_diff, na.rm=TRUE),
    t_diff_cont10 = sum(t_diff[t1_max<10], na.rm=TRUE),
    t_diff_cont5 = sum(t_diff[t1_max<5], na.rm=TRUE),
    t_diff_cont3 = sum(t_diff[t1_max<3], na.rm=TRUE)
  )

# Summary statistics for durations
summ_durations <- rbind(
  dat %>% group_by(cond) %>% summarise(
    total = sum(cond_dur),
    mean = mean(cond_dur),
    sd = sd(cond_dur),
    min = min(cond_dur),
    max = max(cond_dur)
  ),
  gameplay %>% 
    group_by(user, condition) %>% summarise(t_diff = first(t_diff) ) %>% 
    group_by(condition) %>% summarise(
      total = sum(t_diff),
      mean = mean(t_diff),
      sd = sd(t_diff),
      min = min(t_diff),
      max = max(t_diff)
    ) %>% 
    rename(cond=condition)
)

# Select appropriate test for distributions of data
ggplot( dat ) +
  geom_density( aes(x = cond_dur, y = ..density.., fill = cond, colour = cond), alpha=.6) +
  scale_fill_manual(values=c("#6cafb4", "#d8d8d8")) +
  scale_colour_manual(values=c("#6cafb4", "#d8d8d8")) +
  theme_bw() + xlab('Timestamp (% total duration)') + ylab('Participants (proportion)') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggplot( gameplay %>% group_by(user, condition) %>% summarise(t_diff = first(t_diff)) ) +
  geom_density( aes(x = t_diff, y = ..density.., fill = condition, colour = condition), alpha=.6) +
  scale_fill_manual(values=c("#6cafb4", "#d8d8d8")) +
  scale_colour_manual(values=c("#6cafb4", "#d8d8d8")) +
  theme_bw() + xlab('Timestamp (% total duration)') + ylab('Participants (proportion)') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Test whether duration measures are different across SW and Q measures
differentMeasures_game <- t.test(
  dat %>% filter(cond=='Exp') %>% select(cond_dur), 
  gameplay %>% filter(condition=='game') %>% group_by(user) %>% summarise(t_diff = first(t_diff)) %>% select(t_diff),
  equal.vars=FALSE
)
differentMeasures_control <- t.test(
  dat %>% filter(cond=='Ctr') %>% select(cond_dur), 
  gameplay %>% filter(condition=='iv') %>% group_by(user) %>% summarise(t_diff = first(t_diff)) %>% select(t_diff),
  equal.vars=FALSE
)
summ_durations_tests <- cbind(
  rbind(
    'Game',
    differentMeasures_game$statistic,
    differentMeasures_game$parameter,
    differentMeasures_game$p.value
  ),
  rbind(
    'Control',
    differentMeasures_control$statistic,
    differentMeasures_control$parameter,
    differentMeasures_control$p.value
  )
)

#Visualise individuals making these breaks
plot(dat %>% filter(cond=='Ctr') %>% select(cond_dur) )
plot(dat %>% filter(cond=='Exp') %>% select(cond_dur) )
plot(gameplay %>% filter(condition=='iv') %>% group_by(user) %>% summarise(t = as.numeric( max(t) )) %>% select(t) )
plot(gameplay %>% filter(condition=='game') %>% group_by(user) %>% summarise(t = as.numeric( max(t) )) %>% select(t) )

plot(table(
  gameplay %>% filter(condition=='iv') %>% mutate(times=as.numeric(t)) %>% select(times)
))
plot(table(
  gameplay %>% filter(condition=='game') %>% mutate(times=as.numeric(t)) %>% select(times)
))

ggplot( gameplay ) +
  geom_density( aes(x = tPct, y = ..density.., fill = condition, colour = condition), alpha=.6) +
  scale_fill_manual(values=c("#6cafb4", "#d8d8d8")) +
  scale_colour_manual(values=c("#6cafb4", "#d8d8d8")) +
  theme_bw() + xlab('Timestamp (% total duration)') + ylab('Participants (proportion)') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot( timings ) +
  geom_density( aes(x = t, y = ..density.., fill = condition, colour = condition), alpha=.6) +
  scale_fill_manual(values=c("#6cafb4", "#d8d8d8")) +
  scale_colour_manual(values=c("#6cafb4", "#d8d8d8")) +
  theme_bw() + xlab('Timestamp (mins)') + ylab('Participants (proportion)') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot( timejump %>% group_by(user, condition) %>% summarise(t1 = max(t1)) ) +
  geom_density( aes(x = t1, y = ..density.., fill = condition, colour = condition), alpha=.6) +
  scale_fill_manual(values=c("#6cafb4", "#d8d8d8")) +
  scale_colour_manual(values=c("#6cafb4", "#d8d8d8")) +
  xlim(c(0,10)) +
  theme_bw() + xlab('Breaks between interventions (mins)') + ylab('Participants (proportion)') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

gameplay %>% group_by(user) %>% summarise(user=first(user),condition=first(condition),t_diff = max(t_diff)) %>% group_by(condition) %>% summarise(totalTime = sum(t_diff))

ggplot( timejump ) +
  geom_boxplot(aes(x = t1_max, y =)
               
               gameplay %>%
                 group_by(user) %>%
                 summarise(
                   condition = first(condition),
                   t_diff = first(t_diff),
                   t_l25 = sum( case_when(tPct <.25 ~ 1, TRUE ~ 0) ) / n(),
                   t_t25 = sum( case_when(tPct >.75 ~ 1, TRUE ~ 0) ) / n()
                 ) %>%
                 group_by(condition) %>%
                 summarise(
                   t_l25 = mean(t_l25),
                   t_t25 = mean(t_t25)
                 )
               
               # Timelines
               ggplot(
                 timings %>% 
                   filter(
                     user %in% (
                       timejump %>% filter(
                         condition=='iv',
                         t1>=3
                       )
                     )$user
                   ), 
                 aes(x = as.numeric(t), y = n, color=user)
               ) +
                 geom_line() +
                 theme_bw() + xlab('Timestamp (mins)') + ylab('Interventions (n)') +
                 xlim(0,45) +
                 theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
                 theme(legend.position='none')
               
               
  # Usability
   ggplot( Q ) +
     geom_density( aes(x = Usability, y = ..density.., fill = cond, colour = cond), alpha=.6) +
     scale_fill_manual(values=c("#6cafb4", "#d8d8d8")) +
     scale_colour_manual(values=c("#6cafb4", "#d8d8d8")) +
     theme_bw() + xlab('Usability (%)') + ylab('Participants (proportion)') +
     theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
               
               