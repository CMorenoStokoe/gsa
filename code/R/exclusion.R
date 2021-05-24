## initialisation
library('dplyr')
library('readr')
library('psych')
library('broom')
setwd("C:/py/gsa") # Set WD

## read in data
Q <- read_csv("data/cleaned/Q.csv")
AI <- read_csv("data/cleaned/allPossibleInterventions.csv")

## start analysis

# exclude participants

# n participants at start
print(nrow( Q %>% filter(!is.na(cond)) ))
print(nrow( Q %>% filter(!is.na(test_score)) ))

# Exclude participants

# Pilots, duplicates, DNFs
Q <- Q %>% filter(!is.na(test_score)) # 27 pilot or duplicates, 5 did not finish (DNF)

# Plot supporting max exclusions
par(mfrow=c(1,1))
boxplot(Q$cond_dur)
boxplot(Q$test_dur)
# Stats supporting exclusions
mean(Q$cond_dur)
mean(Q$test_dur)

# Ppts who spent too long/little using the learning software
Q <- Q[-which.max(Q$cond_dur),] # 1 ppt in boxplot who spent too long
Q <- Q %>% filter(cond_dur >= 10) # 2 ppts who used software for <10s

# Ppts who spent too long/little on the assessment
Q <- Q[-which.max(Q$test_dur),] # 2 outlying ppts in boxplot who spent too long
Q <- Q[-which.max(Q$test_dur),]
Q <- Q %>% filter(test_dur >= 420) # Exclude 9 who did test in <1 min per section

# Plot supporting min exclusion
plot(
  Q$test_dur[Q$test_dur<1000], 
  Q$test_score[Q$test_dur<1000],
  main = 'Time spent on assessment against score',
  xlab = 'Time spent on assessment (s)',
  ylab = 'Raw assessment score'
)

# n participants after exclusion
print(nrow( Q %>% filter(!is.na(cond)) ))

# save
write_csv(Q, "data/cleaned/Q_excludedPpts.csv")