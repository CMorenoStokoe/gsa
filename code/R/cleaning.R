library('dplyr')
library('readr')

# read in data
Q <- read_csv("./data/raw/Q_halfClean.csv")

# combine condition info
Q <- Q %>% 
  mutate(cond = case_when(
    !is.na(G) ~ 'Exp',
    !is.na(IV) ~ 'Ctr'
  ))
Q <- Q %>% 
  mutate(cond_dur = case_when(
    !is.na(G) ~ G_dur,
    !is.na(IV) ~ IV_dur
))
Q <- Q %>% 
  mutate(cond_clicks = case_when(
    !is.na(G) ~ G_clicks,
    !is.na(IV) ~ IV_clicks
))

# combine total time on assessment
Q <- Q %>% 
  rowwise() %>% 
  mutate(TestDur = sum(c(s1dur, s2dur), na.rm=TRUE))

# drop unused cols
Q <- Q %>% select(11:25, 33:37)

# Save
write_csv(Q, "./data/cleaned/Q.csv")