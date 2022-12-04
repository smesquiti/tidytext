#####The tidytext format 

#tidy text format as being a table with one-token-per-row. 
#A token is a meaningful unit of text, such as a word, that we are interested in using for analysis, 
#and tokenization is the process of splitting text into tokens.

###Different data structures in tidytext 

#String: Text can, of course, be stored as strings, i.e., character vectors, within R, and often text data is first read into memory in this form.
#Corpus: These types of objects typically contain raw strings annotated with additional metadata and details.
#Document-term matrix: This is a sparse matrix describing a collection (i.e., a corpus) of documents with one row for each document and one column for each term. 

#sample text 
text <- c("Because I could not stop for Death -",
                  "He kindly stopped for me -",
                  "The Carriage held but just Ourselves -",
                  "and Immortality")
text#check out the object we created 

#This is a typical character vector that we might want to analyze. 
#In order to turn it into a tidy text dataset, we first need to put it into a data frame.

library(tidyverse)
text_df <- tibble(line = 1:4, text = text) 
text_df

text_df %>%
  unnest_tokens(word, text) #first part of argument is the name of the tokenized column
#note that tokenized texts come out lower-cased 

#load packages to work with jane austen data 
library(janeaustenr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(line_number = row_number(), #keep track of lines in the original format
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]", #keep track of where chapters are 
                                           ignore_case = TRUE)))) %>%
  ungroup()

#To work with this as a tidy dataset, we need to restructure it in the one-token-per-row format, 
#which as we saw earlier is done with the unnest_tokens() function.

library(tidytext)
tidy_books <- original_books %>% 
  unnest_tokens(word, text) #doing this one line at a time for each token

#This function uses the tokenizers package to separate each line of text in the original data frame into tokens. 
#The default tokenizing is for words, but other options include characters, n-grams, sentences, lines, paragraphs, 
#or separation around a regex pattern.

#Now that the data is in one-word-per-row format, we can manipulate it with tidy tools like dplyr. 
#Often in text analysis, we will want to remove stop words; 
#stop words are words that are not useful for an analysis, 
#typically extremely common words such as “the”, “of”, “to”, and so forth in English. 
#We can remove stop words (kept in the tidytext dataset stop_words) with an anti_join().

data("stop_words") #load in stop words 

tidy_books <- tidy_books %>% 
  anti_join(stop_words) #removing the stop words in a tidy way 

#look at the most common words in the books
tidy_books %>% 
  count(word, sort = T) 

#starting to plot the most common words 

tidy_books %>% 
  count(word, sort = T) %>% 
  filter(n > 600) %>% #filtering out lower frequency
  mutate(word = reorder(word, n)) %>% #going in descending order 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#extending this to project gutenberg 
devtools::install_github("ropensci/gutenbergr") #need to do this to install bc it is no longer on cran :(
library(gutenbergr) #library it 

hgwells <- gutenberg_download(c(35, 36, 5230, 159)) #load in the HG Wells books we are interested 

#now let's tokenize them 
tidy_wells <- hgwells %>% 
  unnest_tokens(word, text) %>% #unnest words  
  anti_join(stop_words) #get rid of the stop words

#now let's look and graph  the most common 

tidy_wells %>% 
  count(word, sort = T) %>% 
  filter(n > 200) %>% #let's prune the words we wanna include
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n,word)) +
  geom_col( color = 'dodgerblue') +
  labs(y = NULL)

#now let's look at the bronte sisters 

# We will again use the Project Gutenberg ID numbers for each novel and access the texts using gutenberg_download().

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

#tokenize the text 
tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#look at the most common, filtering for n > 300
tidy_bronte %>% 
  count(word, sort = T) %>% 
  filter(n > 300)  %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n,word)) + 
  geom_col() +
  labs(y = NULL)

#Now, let’s calculate the frequency for each word for the works of Jane Austen, 
#the Brontë sisters, and H.G. Wells by binding the data frames together. 
#We can use pivot_wider() and pivot_longer() from tidyr to reshape our 
#dataframe so that it is just what we need for plotting and comparing the three sets of novels.

#We use str_extract() here because the UTF-8 encoded texts from Project Gutenberg 
#have some examples of words with underscores around them to indicate emphasis (like italics). 
#The tokenizer treated these as words, but we don’t want to count “_any_” separately from “any”
#as we saw in our initial data exploration before choosing to use str_extract().

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_wells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Brontë Sisters`:`H.G. Wells`,
               names_to = "author", values_to = "proportion")
  
#Now let's plot the words that are in all three authors domains 

library(scales)

# expect a warning about rows with missing values being removed
#words closer to our line of best fit have similar frequencies in both sets of texts
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

#Let’s quantify how similar and different these sets of word frequencies are using a correlation test.
#How correlated are the word frequencies between Austen and the Brontë sisters, and between Austen and Wells?
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)
#Just as we saw in the plots, the word frequencies are more correlated between 
#the Austen and Brontë novels than between Austen and H.G. Wells.


#let's practice what we learned with some of our own data now 

#load in the Tech CEO earning call data 
df <- read.csv("Tech_CEOS copy.csv") 

#get rid of the LIWC output :(
df <- df[, c(1:6)] #we trimmed it down 
  
str(df) #check out our data

#try and turn it into tokenized format and save it as a dataframe and drop the stop words 

#data(stop_words) make sure to load in the stop words again if needed 

tidy_df <- df %>% 
  unnest_tokens(word, Text) %>% 
  anti_join(stop_words)

tidy_df
#now get the top words in our dataset 
tidy_df %>% 
  count(word, sort = T)

#ok, now let's visualize it 
tidy_df %>% 
  count(word, sort = T) %>% 
  filter(n>1000) %>% 
  mutate(word = reorder(word,n)) %>% #get them in descending order 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#ok now let's try and build some of the graphs, we will need to restructure our data to get some of the variables 
tidy_df$Speaker <- as.factor(tidy_df$Speaker) 

frequency <- tidy_df %>%
  filter(Speaker == "Shantanu Narayen" |Speaker == "Tim Cook" | Speaker =="Elon Musk") %>%  #let's just get a few CEOS
  count(Speaker, word) %>%
  group_by(Speaker) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Speaker, values_from = proportion) %>%
  pivot_longer(`Shantanu Narayen`:`Elon Musk`,
               names_to = "Speaker", values_to = "proportion")


ggplot(frequency, aes(x = proportion, y = `Tim Cook`, 
                      color = abs(`Tim Cook` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Speaker, ncol = 2) + #change this to our grouping variable 
  theme(legend.position="none") +
  labs(y = "Tim Cook", x = NULL)


#let's see how correlated they are now 
cor.test(data = frequency[frequency$Speaker == "Shantanu Narayen",],
         ~ proportion + `Tim Cook`)

cor.test(data = frequency[frequency$Speaker == "Elon Musk",],
         ~ proportion + `Tim Cook`)

#we can see Shantanu Narayen's tokenized words are more similar to tim cook than elon 
  