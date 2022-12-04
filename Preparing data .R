library(pacman)
pacman::p_load(tidyverse, tidytext, install = T) #load all your packages (and install if needed) at once

#load in our raw data, this will take a few minutes 
rawdata <- read.csv(file = 
                      'https://bhciaaablob.blob.core.windows.net/cmotionsnlpblogs/RestoReviewRawdata.csv',
                    header=TRUE,stringsAsFactors = FALSE,row.names = NULL)

str(rawdata)#check out our data 

#look at some reviewer texts
rawdata %>% 
  select(reviewText) %>% 
  sample_n(5,seed=1234) %>% 
  pull()

#view frequent review texts
rawdata %>% 
  group_by(reviewText) %>% 
  summarize(n_reviews=n()) %>% 
  mutate(pct=n_reviews/sum(n_reviews)) %>%
  arrange(-n_reviews) %>% 
  top_n(10,n_reviews) 
#we can see that ~3% of the reviews have no text, another 0.03% are being processed 

data <- rawdata %>% 
  #remove metatext ('b:'), replace linebreaks and some punctuation with space 
  #  and remove other punctuation and set to lower case.
  mutate(reviewTextClean=gsub('[[:punct:]]+', '',
                              gsub('\\\\n|\\.|\\,|\\;',' ',tolower(substr(reviewText,3,nchar(reviewText)-1))))) %>%
  # create indicator validReview that is 0 for reviews to delete 
  mutate(validReview=case_when(# unpublished review texts 
    grepl('recensie is momenteel in behandeling',reviewTextClean)~0, 
    # review texts less than 2 characters in length
    nchar(reviewTextClean) <2 ~ 0,  
    # review texts of length 2, not being 'ok'
    nchar(reviewTextClean)==2 & 
      grepl('ok',reviewTextClean)==FALSE ~ 0, 
    # review texts of length 3, not being 'top','wow','oke'
    nchar(reviewTextClean) ==3 & 
      grepl('top|wow|oke',reviewTextClean)==FALSE ~ 0, 
    TRUE ~ 1))

# check most frequent reviews (and indicator to drop or not)
data %>% 
  group_by(reviewTextClean,validReview) %>% 
  summarize(n_reviews=n()) %>% 
  group_by(validReview) %>% 
  arrange(validReview,desc(n_reviews)) %>% 
  top_n(5,n_reviews) 

data <- data %>%
  # remove all reviews that are not valid accoring to our rules defined earnier
  filter(validReview==1) %>%
  mutate(#create a unique identifier by combining the restaurant-id 
    # and the review-id that identifies unique reviews within a restaurant
    restoReviewId = paste0(restoId,'_',review_id),
    #some extra preparation on other features we have available: 
    reviewDate = lubridate::dmy(reviewDate),
    yearmonth=format(reviewDate,'%Y%m'),
    waitingTimeScore = recode(waitingTimeScore,"Kort"=1, "Redelijk"=2, 
                              "Kan beter"=3, "Hoog tempo"=4, "Lang"=5, .default=0, .missing = 0),
    valueForPriceScore = recode(valueForPriceScore,"Erg gunstig"=1, "Gunstig"=2, 
                                "Kan beter"=3, "Precies goed"=4, "Redelijk"=5, .default=0, .missing = 0),
    noiseLevelScore = recode(noiseLevelScore,"Erg rustig"=1, "Precies goed"=2, 
                             "Rumoerig"=3, "Rustig"=4, .default=0, .missing = 0),
    scoreFood = as.numeric(gsub(",", ".", scoreFood)),
    scoreService = as.numeric(gsub(",", ".", scoreService)),
    scoreDecor = as.numeric(gsub(",", ".", scoreDecor)),
    reviewScoreOverall = as.numeric(gsub(",", ".", reviewScoreOverall)),
    scoreTotal = as.numeric(gsub(",", ".", scoreTotal)),
    reviewTextLength = nchar(reviewTextClean)
  )
