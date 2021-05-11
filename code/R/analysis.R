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
  
    # define functions
    countOutliers <- function(df, cell, col){
      max <- quantile(col, na.rm=TRUE)['75%'] + (IQR(col, na.rm=TRUE)*1.5)
      min <- quantile(col, na.rm=TRUE)['25%'] - (IQR(col, na.rm=TRUE)*1.5)
      return (rbind(
        c('max', max),
        c('min', min),
        c('exceed max', nrow( df %>% filter( !!sym(cell) > max))),
        c('exceed min', nrow( df %>% filter( !!sym(cell) < min)))
      ))
    }
    excludeOutliers <- function(df, cell, col){
      return ( df %>% filter(
        !!sym(cell) <= quantile(col, na.rm=TRUE)['75%'] + (IQR(col, na.rm=TRUE)*1.5) 
        &
        !!sym(cell) >= quantile(col, na.rm=TRUE)['25%'] - (IQR(col, na.rm=TRUE)*1.5) 
      ))
    }
    
    # Exclude participants
    
      # Pilots, duplicates, DNFs
      Q <- Q %>% filter(!is.na(test_score)) # 27 pilot or duplicates, 4 did not finish (DNF)
      
      # Explore data
      par(mfrow=c(1,3))
      boxplot(Q$cond_dur)
      boxplot(Q$test_dur)
      plot(Q$test_dur[Q$test_dur<1000], Q$test_score[Q$test_dur<1000])
      
      # Ppts who spent too long/little using the learning software
      Q <- Q[-which.max(Q$cond_dur),] # 1 ppt in boxplot who spent too long
      Q <- Q %>% filter(cond_dur >= 10) # 2 ppts who used software for <10s
    
      # Ppts who spent too long/little on the assessment
      Q <- Q[-which.max(Q$test_dur),] # 2 outlying ppts in boxplot who spent too long
      Q <- Q[-which.max(Q$test_dur),]
      Q <- Q %>% filter(test_dur >= 420) # Exclude 9 who did test in <1 min per section

## scoring
      
    # Convert seconds to minutes
      Q <- Q %>% 
        mutate(cond_dur = cond_dur/60)
      Q <- Q %>% 
        mutate(test_dur = test_dur/60)
    
    # Positively score likerts
    Q <- Q %>% 
      mutate(Motivation = 11 - Motivation)
    Q <- Q %>% 
      mutate(Direction = 11 - Direction)
    
    # Count PLEXs
    Q <- Q %>% 
      rowwise() %>% 
      mutate(PLEX_count = sum(c(Captivation,Challenge,Competition,Completion,Discovery,Progression,Exploration,Fantasy,Humor,Nurture,Relaxation,Sensation),na.rm=TRUE))
    
    # Adjust mcq for guessing and convert to % out of 100
    Q <- Q %>% 
      mutate(test_score_adjusted = (test_score - 8.56) / 16.64 * 100 ) # 16.64 highest possible score
    
    # Score free form and convert to % out of 100
    
      # Define function
      primaryScore <- function(trait, objective){
        i <- AI %>% filter(ORIGIN == trait)
        return (as.double(i[objective]))
      }
      sideScore <- function(trait){
        i <- AI %>% filter(ORIGIN == trait)
        return (as.double(i$SCORE))
      }
      scoreFfe <- function(df, test, objective){
        addPrimaryScore <- df %>% 
          rowwise() %>% 
          mutate(!!paste('test_ffe',test,'_primary',sep='') := sum(c(
            primaryScore(!!sym(paste('test_ffe',test,'_1',sep='')), objective),
            primaryScore(!!sym(paste('test_ffe',test,'_2',sep='')), objective),
            primaryScore(!!sym(paste('test_ffe',test,'_3',sep='')), objective) 
          ), na.rm=TRUE)
        )
        addSideScore <- addPrimaryScore %>% 
          rowwise() %>% 
          mutate(!!paste('test_ffe',test,'_side',sep='') := sum(c(
            sideScore(!!sym(paste('test_ffe',test,'_1',sep=''))),
            sideScore(!!sym(paste('test_ffe',test,'_2',sep=''))),
            sideScore(!!sym(paste('test_ffe',test,'_3',sep=''))) 
          ), na.rm=TRUE)
        )
        return(addSideScore)
      }
      
      # Use function
      Q <- scoreFfe(Q, 1, 'Wellbeing')
      Q <- scoreFfe(Q, 2, 'Not socialising') # Note: Reducing not socialisation is the same as increasing socialisation
      Q <- scoreFfe(Q, 3, 'Smoking')
      Q <- scoreFfe(Q, 4, 'CHD')
        
      # Total score
        
        # Calculate totals out of best answers
        Q <- Q %>% 
          rowwise() %>% 
          mutate(test_ffe_primaryTotal = mean(c(
            test_ffe1_primary / 1.063948313 * 100,
            test_ffe2_primary / -0.590338799 * 100,
            test_ffe3_primary / -6.93150945 * 100,
            test_ffe4_primary / -3.263130341 * 100
          ), na.rm=TRUE))
        Q <- Q %>% 
          rowwise() %>% 
          mutate(test_ffe_SideTotal = mean(c(
            test_ffe1_side / 21.82497 * 100,
            test_ffe2_side / 21.82497 * 100,
            test_ffe3_side / 21.82497 * 100,
            test_ffe4_side / 21.82497 * 100
          ), na.rm=TRUE))
        Q <- Q %>% 
          rowwise() %>% 
          mutate(test_ffe_total = 
                   mean(c(test_ffe_primaryTotal,test_ffe_SideTotal), na.rm=TRUE))
        
        #optimal_wellbeing <- AI %>% filter(ORIGIN != 'Wellbeing') %>% arrange(desc(Wellbeing))
        #optimal_socialising <- AI %>% filter(ORIGIN != 'Not socialising') %>% arrange(`Not socialising`)
        #optimal_smoking <- AI %>% filter(ORIGIN != 'Smoking') %>% arrange(Smoking)
        #optimal_chd <- AI %>% filter(ORIGIN != 'CHD') %>% arrange(CHD)
        #mostGood <- AI %>% arrange(desc(SCORE))
        
      
      # Explore data
      par(mfrow=c(2,2))
      hist(Q$test_ffe1_primary)
      hist(Q$test_ffe2_primary)
      hist(Q$test_ffe3_primary)
      hist(Q$test_ffe4_primary)
      
      hist(Q$test_ffe1_side)
      hist(Q$test_ffe2_side)
      hist(Q$test_ffe3_side)
      hist(Q$test_ffe4_side)
      
      par(mfrow=c(1,3))
      hist(dat$test_score_adjusted)
      hist(Q$test_ffe_primaryTotal)
      hist(Q$test_ffe_SideTotal)
      
      # Loadings
      testLoadings <- as.data.frame(
        cor(cbind(
          Q$test_score_adjusted,
          Q$test_ffe_primaryTotal, 
          Q$test_ffe_SideTotal
        )
      ))
      colnames(testLoadings) <- c('MCQ score', 'FFE primary score', 'FFE side score')
      rownames(testLoadings) <- c('MCQ score', 'FFE primary score', 'FFE side score')
      
   
# select used variables 
dat <- Q %>% select(
    cond, cond_dur,
    Motivation,
    Direction, 
    FreeText,
    PLEX_count,
    Captivation,Challenge,Competition,Completion,Discovery,Progression,Exploration,Fantasy,Humor,Nurture,Relaxation,Sensation,
    test_score_adjusted, test_dur,
    test_ffe_total
)
      
## analysis

  # descriptives
    
    # totals
    descriptives <- describe( dat %>% select(-cond, -FreeText) ) %>% select(n, mean, sd, min, max)
    
    descr_exp <- describe(dat %>% filter(cond == 'Exp')) %>% select(n, mean, sd, min, max)
    descr_ctr <- describe(dat %>% filter(cond == 'Ctr')) %>% select(n, mean, sd, min, max)
    
    par(mfrow=c(1,3))
    hist(dat$Motivation)
    hist(dat$Direction)
    hist(dat$PLEX_count)
  
  # usability
    # data not extracted
  
  #h1: Structure learners
  h1_srq <- t.test(dat$Direction ~ dat$cond)
  
  #h2: Motivate learners
  h2_srq <- t.test(dat$Motivation ~ dat$cond)
  h2_dur <- t.test(dat$cond_dur ~ dat$cond)
  
  #h3: Better teach learners
  h3_mcq <- t.test(dat$test_score_adjusted ~ dat$cond)
  h3_ffe <- t.test(dat$test_ffe_total ~ dat$cond)
  h3_dur <- t.test(dat$test_dur  ~ dat$cond)
  
# Save
  write_csv(testLoadings, "./outputs/loadings.csv")
  write_csv(dat, "./outputs/table.csv")
  write_csv(
    rbind(
      broom::tidy(h1_srq)[0,],
      broom::tidy(h1_srq)[1,],
      broom::tidy(h2_srq)[1,],
      broom::tidy(h2_dur)[1,],
      broom::tidy(h3_mcq)[1,],
      broom::tidy(h3_ffe)[1,],
      broom::tidy(h3_dur)[1,]
    ),
    "./outputs/hypothesisTests.csv"
  )
