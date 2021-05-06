library('dplyr')
library('readr')

Q <- read_csv("data/cleaned/Q.csv", col_types = cols(PLEX11 = col_double(), 
                                                     PLEX12 = col_double()))

# plot factors
plot_cond_dur <- boxplot(Q$cond_dur)
plot_test_dur <- boxplot(Q$TestDur)

# exclude participants

  # define functions
  quart <- function(col, mode){
    if(mode=='min'){
      return(
        quantile(col, na.rm=TRUE)['25%'] - (IQR(col, na.rm=TRUE)*1.5) 
      )
    }else{
      return(
        quantile(col, na.rm=TRUE)['75%'] + (IQR(col, na.rm=TRUE)*1.5)
      )
    }
  }
  
  # use functions to count exclusions
  # nrow(Q %>% filter(cond_dur < quart(Q$cond_dur, 'min') ))
  #nrow(Q %>% filter(TestDur > quart(Q$TestDur, 'max') ))
  
  
nrow(Q)
# Exclude outliers
  
Q <- Q %>% filter(
    cond_dur <= quart(Q$cond_dur, 'max') & #0
    cond_dur >= quart(Q$cond_dur, 'min') #7
  ) # 7 outliers on condition duration

Q <- Q %>% filter(
  TestDur <= quart(Q$TestDur, 'max') & #0
    TestDur >= quart(Q$TestDur, 'min') #9
) # 9 outliers on test duration

# Exclude on pre-agreed criteria
Q <- Q %>% filter(!is.na(Total)) # 13 did not finish study
Q <- Q %>% filter(cond_dur >= 10) # 2 did not use learning tool
Q <- Q %>% filter(TestDur >= 60) #2 did not engage with assessment

nrow(Q)

# descriptive analysis
#summary(Q)