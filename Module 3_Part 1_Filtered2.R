library(httr)
library(tidyverse)
library(data.table)
library(feather)


setwd("C:/Users/leebb/Documents/Module 3 Filtered")

patent <- fread('data/patent/g_patent_2012_2021.csv')
assignee <- fread('data/assignee/g_assignee_disambiguated_2012_2021.csv')
term <- fread('data/term/g_us_term_of_grant_2012_2021.csv')
application <- fread('data/application/g_application_2012_2021.csv')
cpc <- fread('data/cpc/g_cpc_current_2012_2021.csv')
location <- fread('data/location/g_location_disambiguated_2012_2021.csv')

patent_text <- patent[,c('patent_id','patent_title','patent_abstract')]
write.csv(patent_text,'data/patent/patent_text.csv',row.names = F)


# Rdata
save(patent,file = 'data/patent/g_patent_2012_2021.Rdata')
save(assignee,file = 'data/assignee/_assignee_disambiguated_2012_2021.Rdata')
save(term,file = 'data/term/g_us_term_of_grant_2012_2021.Rdata')
save(application,file = 'data/application/g_application_2012_2021.Rdata')
save(cpc,file = 'data/cpc/g_cpc_current_2012_2021.Rdata')
save(location,file = 'data/location/g_location_disambiguated_2012_2021.Rdata')

# Feather (https://posit.co/blog/feather/)
write_feather(patent,'data/patent/g_patent_2012_2021.feather')
write_feather(assignee,'data/assignee/_assignee_disambiguated_2012_2021.feather')
write_feather(term,'data/term/g_us_term_of_grant_2012_2021.feather')
write_feather(application,'data/application/g_application_2012_2021.feather')
write_feather(cpc,'data/cpc/g_cpc_current_2012_2021.feather')
write_feather(location,'data/location/g_location_disambiguated_2012_2021.feather')


# Run load time comparison
benchmark <- c()
# read.table 
start_time <- Sys.time()
cpc <- read.table(file = 'data/cpc/g_cpc_current_2012_2021.csv',sep = ',')
end_time <- difftime(Sys.time(),start_time,units = 'mins' )
benchmark <- c(benchmark,'read.table' = end_time)
# read.csv 
start_time <- Sys.time()
cpc <- read.delim(file = 'data/cpc/g_cpc_current_2012_2021.csv',sep = ',')
end_time <- difftime(Sys.time(),start_time,units = 'mins' )
benchmark <- c(benchmark,'read.csv' = end_time)
# fread 
start_time <- Sys.time()
cpc <- fread(file = 'data/cpc/g_cpc_current_2012_2021.csv')
end_time <- difftime(Sys.time(),start_time,units = 'mins' )
benchmark <- c(benchmark,'fread' = end_time)
# feather 
start_time <- Sys.time()
cpc <- read_feather(path = 'data/cpc/g_cpc_current_2012_2021.feather')
end_time <- difftime(Sys.time(),start_time,units = 'mins' )
benchmark <- c(benchmark,'feather' = end_time)
# Rdata
start_time <- Sys.time()
load('data/cpc/g_cpc_current_2012_2021.Rdata')
end_time <- difftime(Sys.time(),start_time,units = 'mins' )
benchmark <- c(benchmark,'Rdata' = end_time)

benchmark


###############################################################################
# R Shiny preview while we wait: https://davidpeterhall.shinyapps.io/gcstudy/ #
###############################################################################


# Some thoughts:
# feather is nice because it can be read in R or python and is close to the speed of fread
# Rdata files will take up less storage space
# fread is part of the data.table package, which is great for working with large files once they are loaded




###############################################################################
# PART 2
###############################################################################

library(tidyverse)
library(data.table)

###########################
# Competitive positioning #
###########################

setwd("C:/Module 3 Filtered")
# I want to enter the market. Where are the biggest barriers to innovation? 


################################################################################
# Read data 

starttime <- Sys.time()
# patents file (grant date)
patent <- fread('data/patent/g_patent_2012_2021.csv')
# disambiguated assignee (company name)
assignee <- fread('data/assignee/g_assignee_disambiguated_2012_2021.csv')
# term grant (expiration date)
term <- fread('data/term/g_us_term_of_grant_2012_2021.csv')
# cpc codes (for heatmap)
cpc <- fread('data/cpc/g_cpc_current_2012_2021.csv')
Sys.time() - starttime


################################################################################  
# Define the scope of the analysis    

# Ideas
# Telescopes, periscopes, viewfinders, etc. G02B 23/14
# Wheelchairs A61G 5
# Guitars G10D 1/08
# Stoves, Ranges, Grills F24B
# Skis or snowboards A63C 5
# find more.... (ones they can easily create submarkets for)


# Assemble the dataframe 
# Tip: Always be aware of what a "row" is when merging, watch the row count of the resulting data frame to make sure it does what you expect
cpc$patent_id <- as.character(cpc$patent_id)
# Filter the cpc codes
dt <- cpc %>% filter(grepl(pattern = 'A61K38/',x = cpc$cpc_group,ignore.case = T))
# merge with patents
dt <- merge(dt,patent,by = 'patent_id')
# merge with assignee
dt <- merge(dt,assignee,by = 'patent_id') # why is this dropping? no assignee?


################################################################################
# Total patents, 5 year CAGR, avg claims as quality proxy 

# Get top 10 companies  
totals <- dt %>% filter(disambig_assignee_organization!='') %>% group_by(disambig_assignee_organization) %>% summarize(total=uniqueN(patent_id)) 
totals <- totals[order(totals$total,decreasing = T),] %>% slice(1:10)  

# Calculate 5 year CAGR for top 10 companies
cagr <- data.frame(expand.grid(year=2017:2021,disambig_assignee_organization=totals$disambig_assignee_organization))

temp <- dt %>% 
  filter(disambig_assignee_organization %in% totals$disambig_assignee_organization) %>% 
  group_by(year=year(patent_date),disambig_assignee_organization) %>% 
  summarise(n=uniqueN(patent_id))
cagr <- merge(cagr,temp,by = c('year','disambig_assignee_organization'),all.x = T)
rm(temp)
cagr[is.na(cagr)] <- 0
cagr <- cagr %>%
  group_by(disambig_assignee_organization) %>%
  mutate(cum_cnt = cumsum(n)) %>%  # make sure your date are sorted correctly before calculating the cumulative :)
  filter(year %in% c(2017,2021)) %>%
  pivot_wider(id_cols = disambig_assignee_organization,names_from = year,values_from = cum_cnt)
cagr$cagr_2017_2021 <- round(((cagr$`2021`/cagr$`2017`)^(1/5))-1,3)


# Calculate avg claim count for top 10 companies
claims <- dt %>% 
  filter(disambig_assignee_organization %in% totals$disambig_assignee_organization) %>%
  select(disambig_assignee_organization,patent_id,num_claims) %>%
  unique() %>%
  group_by(disambig_assignee_organization) %>%
  summarise(avg_claims=round(mean(num_claims)))

# Combine and save file
totals <- merge(totals,cagr,by = 'disambig_assignee_organization')
totals <- merge(totals,claims,by = 'disambig_assignee_organization')
totals <- totals %>% select(-`2017`,-`2021`)


################################################################################
# Positioning heatmap (reshaping data)

# Identify patents in each segment (benefit of doing it this way? scale)

segments_names <- c('Enzyme Inhibitors', 'Hydrolyzed Proteins', 'Peptides of Undefined Length', 'Fully Defined Peptides of up to Twenty Amino Acids', 'Peptides Containing Over Twenty Amino Acids', 'Not Fully Defined Peptides that Contain up to Twenty Amino Acids')
segments_codes <- c('A61K38/005','A61K38/01','A61K38/02','A61K38/04','A61K38/16','A61K38/03')
segments_dtlist <- list()
for (segment in segments_codes) {
  segments_dtlist[[length(segments_dtlist)+1]] <- dt[grepl(pattern = segment,x = dt$cpc_group,ignore.case = T),c('patent_id','disambig_assignee_organization')] %>% unique()
}
names(segments_dtlist) <- segments_names

# Assemble data and pivot
for (segment in segments_names) {
  temp <- segments_dtlist[[segment]] %>% group_by(disambig_assignee_organization) %>% summarise(n=uniqueN(patent_id))
  colnames(temp)[colnames(temp)=='n'] <- segment
  totals <- merge(totals,temp,by = 'disambig_assignee_organization',all.x = T)
}  
totals[is.na(totals)] <- 0

totals <- totals %>% arrange(desc(total))

totals$total_sum <- totals['Enzyme Inhibitors'] + totals["Hydrolyzed Proteins"] + totals["Peptides of Undefined Length"] + totals['Fully Defined Peptides of up to Twenty Amino Acids'] + totals['Peptides Containing Over Twenty Amino Acids'] + totals['Not Fully Defined Peptides that Contain up to Twenty Amino Acids']

# Save file
write.csv(totals,'output/competitive positioning.csv',row.names = F)

###GRAPH MAKING
# Assuming your "totals" table has a column called "total" with the values you want to represent as bar lengths


###COMPETITION ANALYSIS#####
# Create a vector of heights (total column of totals table)
heights <- totals$cagr_2017_2021

# Create a bar chart
barplot(heights, 
        main = "Competition Analysis (2017 - 2021)",  # Specify the title of the chart
        ylab = "CAGR",               # Specify the label for the y-axis
        col = "steelblue",           # Specify the color of the bars
        border = "black",            # Specify the color of the bar borders
        names.arg = totals$disambig_assignee_organization,
        las = 3) # Specify the labels for the x-axis (assuming "category" is the column name in your "totals" table)

# Note: You can customize other aspects of the bar chart, such as font size, axis limits, etc., using additional parameters in the barplot() function.

###TREND ANALYSIS
barplot(as.matrix(totals$total_sum),  # Use the "count" column as the heights for the bars
        main = "Segmented Bar Chart of Patents",  # Specify the title of the chart  # Specify the label for the x-axis
        ylab = "Count",  # Specify the label for the y-axis
        col = rainbow(length(totals$disambig_assignee_organization)),  # Specify colors for the segments
        beside = TRUE)  # Create segmented bars side by side

# Add legend to the chart
legend("topright",  # Specify the location of the legend
       legend = totals$disambig_assignee_organization,  # Use the "category" column as the legend labels
       fill = rainbow(length(totals$disambig_assignee_organization)))  # Specify colors for the legend labels

###Try 2
# library
library(ggplot2)

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Stacked
ggplot(totals, aes(fill = totals['Enzyme Inhibitors'] + totals["Hydrolyzed Proteins"] + totals["Peptides of Undefined Length"] + totals['Fully Defined Peptides of up to Twenty Amino Acids'] + totals['Peptides Containing Over Twenty Amino Acids'] + totals['Not Fully Defined Peptides that Contain up to Twenty Amino Acids'] ,y = total_sum, x = disambig_assignee_organization)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Bar Chart of Total Sum by Organization",  # Specify the title of the chart
       x = "Organization",  # Specify the label for the x-axis
       y = "Total Sum")


###PART 3 Location Mapping
install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages("text2vec")
install.packages("rjson")
install.packages("plotly")
install.packages("stm")
install.packages("udpipe")

library(tidyverse)
library(data.table)
library(quanteda)
library(quanteda.textstats)
library(text2vec)
library(rjson)
library(plotly)
library(stm)
library(udpipe)



######################
# Technology trends #
######################

# 1) Plot the geographic activity and find regions of highest growth (table)
# 2) Generate S-Curve of ML adoption break out by sub-markets
# 3) Text analytics


################################################################################
# Read data
starttime <- Sys.time()
# patents file (grant date)
patent <- fread('data/patent/g_patent_2012_2021.csv')
# disambiguated assignee (company name)
assignee <- fread('data/assignee/g_assignee_disambiguated_2012_2021.csv')
# term grant (expiration date)
location <- fread('data/location/g_location_disambiguated_2012_2021.csv')
# cpc codes (for heatmap)
cpc <- fread('data/cpc/g_cpc_current_2012_2021.csv')
Sys.time() - starttime


################################################################################
# Filter to technology of interest
# Pharma (A61P,A61J)
# Medical devices (A61B,A61C,A61F,A61L,A61M,A61N)
# Audio (H04R,H04S)
# Automotive (B60)
# Cyber security (G06F21, H04L63, H04L9, H04W12, H04K1)

cpc$patent_id <- as.character(cpc$patent_id)
# Filter the cpc codes
keep <- cpc %>% 
  filter(grepl(pattern = 'A61K38',x = cpc$cpc_group,ignore.case = T)) %>%
  select(patent_id) %>%
  unique()

dt <- cpc %>% filter(patent_id %in% keep$patent_id)

# merge with patents
dt <- merge(dt,patent,by = 'patent_id')
# merge with assignee
dt <- merge(dt,assignee,by = 'patent_id')
# merge location 
dt <- merge(dt,location,by = 'location_id')


# tidy up the location data
dt$state_fips <- str_pad(string = dt$state_fips,width = 2,side = 'left',pad = '0')
dt$county_fips <- str_pad(string = dt$county_fips,width = 3,side = 'left',pad = '0')
dt$fips <- paste(dt$state_fips,dt$county_fips,sep = '')



################################################################################
################################################################################
# Mapping using plotly
# Plotly basics: https://plotly.com/r/
# Maps: https://plotly.com/r/choropleth-maps/

# County level
# Summarize data by county
dt_county <- dt %>% 
  filter(!is.na(county_fips)) %>% 
  group_by(disambig_state,county,fips) %>% 
  summarise(n=uniqueN(patent_id))

# get geojson files for mapping
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file = url)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_ly()
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=counties,
  locations= dt_county$fips,
  z=dt_county$n,
  colorscale="Viridis",
  zmin=0,
  zmax=3000,
  marker=list(line=list(
    width=0)
  )
)
fig <- fig %>% colorbar(title = "Number of patents")
fig <- fig %>% layout(
  title = "Medicinal peptide patents granted by county",
  geo = g
)

fig


# State level
# Summarize data by state
dt_state <- dt %>% group_by(disambig_state,state_fips) %>% summarise(n=uniqueN(patent_id))

l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig <- plot_geo(dt_state, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~n, 
  text = ~disambig_state, 
  locations = ~disambig_state,
  color = ~n, 
  colors = 'Blues'
)
fig <- fig %>% colorbar(title = "Count of patents")
fig <- fig %>% layout(
  title = 'Medicinal peptide patents granted by State',
  geo = g
)

fig



################################################################################
################################################################################
# S-Curves

segments_names <- c('segment 1','segment 2','segment 3','segment 4','segment 5')
segments_codes <- c('G06F21', 'H04L63', 'H04L9', 'H04W12', 'H04K1')

# First we need to identify patents that contain machine learning methods
# This can be done by looking at the CPC codes and/or the text

# cpc codes to flag
ml_cpcs <- c('G06N3/02','G06N3/08','G06N3/12','G06N5','G06N20','G10L15/02', 'G10L15/075', 'G10L15/08', 'G10L15/014','G10L15/16',
             'G10L15/18', 'G10L17/02', 'G10L17/04', 'G10L17/16', 'G10L17/18', 'G10L25/30', 'G10L25/33', 'G10L25/36', 'G10L25/39') 

# keywords to flag
ml_keywords <- c('machine learning', 'deep learning', 'statistical learning','neural network')
# keywords <- c('machine learning', 'deep learning', 'statistical learning', 'support vector machine', 'supervised learning' 
#             , 'unsupervised learning', 'nearest neighbor', 'neural network', 'convolutional network','reinforcement learning',
#             'feature selection','feature engineering','adversarial learning','adversarial network','transfer learning',
#             'lstm','long short term memory','knn','k-nn','k-nearest','logit','logistic','markov','belief network',
#             'latent dirichlet','genetic algorithm','xgboost','random forest','mixture model','maximum likelihood',
#             'semi-supervised learning','regression','adaboost','gradient descent','recurrent network','recurrent neural',
#             'convolutional neural','posteriori model','naive bayes','intelligent','predict','classifier')

dt$ml <- ifelse(grepl(pattern = paste(ml_cpcs,collapse = '|',sep = ''),x = dt$cpc_group,ignore.case = T),1,0)
table(dt$ml)
head(dt$cpc_group)
dt$ml <- ifelse(grepl(pattern = paste(ml_keywords,collapse = '|',sep = ''),x = dt$patent_title,ignore.case = T),1,dt$ml)
dt$ml <- ifelse(grepl(pattern = paste(ml_keywords,collapse = '|',sep = ''),x = dt$patent_abstract,ignore.case = T),1,dt$ml)
table(dt$ml) 

# add sub categories
dt$segment <- NA
for (i in 1:length(segments_names)) {
  dt$segment <- ifelse(grepl(pattern = segments_codes[i],x = dt$cpc_group,ignore.case = T),segments_names[i],dt$segment)
}
dt$segment[is.na(dt$segment)] <- 'Other'
table(dt$segment,useNA = 'ifany')

# Questions
# what has adoption of machine learning looked like overall?  
temp <- dt %>% filter(ml==1) %>% group_by(year=year(patent_date)) %>% summarise(n=uniqueN(patent_id)) 
temp <- temp[order(temp$year,decreasing = F),]
fig <- plot_ly(temp, x = ~year, y = ~n, type = 'bar')
fig

# what share of the technology incorporates machine learning?  
temp <- dt %>% group_by(year=year(patent_date),ml) %>% summarise(n=uniqueN(patent_id))
temp <- temp %>% group_by(year) %>% mutate(total=sum(n))
temp <- temp %>% filter(ml==1)
temp$share_ml <- round(temp$n/temp$total,3)
fig <- plot_ly(temp, x = ~year, y = ~share_ml, type = 'bar')
fig

# which segments in the industry have seen higher rates of adoption? 
temp <- dt %>% group_by(year=year(patent_date),ml,segment) %>% summarise(n=uniqueN(patent_id))
temp <- temp %>% group_by(year) %>% mutate(total=sum(n))
temp <- temp %>% filter(ml==1)
temp$share_ml <- round(temp$n/temp$total,3)
temp <- pivot_wider(data = temp,id_cols = year,names_from = segment,values_from = share_ml,values_fill = 0)

fig <- plot_ly(temp, x = ~year, y = ~temp[[2]], type = 'bar',name = segments_names[1]) # change y-axis label
for (i in 2:length(segments_names)) {
  fig <- fig %>% add_trace(x = ~year, y = ~temp[[i+1]], name = segments_names[i])
}
fig

# Grouped bar chart
fig <- fig %>% layout(title = 'Share ML by segment', yaxis = list(title = ''), barmode = 'group')
fig
# Stacked bar chart
fig <- fig %>% layout(title = 'Share ML by segment', yaxis = list(title = ''), barmode = 'stack') 
fig

# Which chart is more accurate?





################################################################################
################################################################################
# Text analysis 

# Agenda:
# Regular expressions (Regex)
# Pre-processing: cleaning, tokens, and dtm's
# Keyword extraction
# Themes/Topics using LDA
# UDPipe and POS tagging


################################################################################
# Regular expressions

# Syntax
# There are thousands of regex references out there
# I bookmarked this one forever ago and still use it: http://www.endmemo.com/r/grep.php

# Functions
# https://www.datacamp.com/tutorial/regex-r-regular-expressions-guide

strings <- c('I love BYU','I LOVE my ice cream','Go cougars','I Love the Jazz','Do you like the jazz?','the12345and','2001','3001')
string <- 'I am a student at BYU and will one day make lots of money'

str_subset(string = strings,pattern = 'love')
str_detect(string = strings,pattern = 'love')
str_extract(string = strings,pattern = 'Jazz')
str_match(string = strings,pattern = '[J-j]azz')
str_locate_all(string = strings,pattern = '[[:digit:]]')
str_locate_all(string = strings,pattern = '[[:digit:]]+')

# What if we want to find patents that contain two words near each other, but not necessarily right next to each other?
# For example, we might want to find patents where the words "autonomous" and "vehicle" appear near each other, even if the exact phrase "autonomous vehicle" isn't found (show an example to make the point)
# Creating a "near" function with regular expressions

near <-   '\\bstudent\\W+(?:\\w+\\W+){1,6}?BYU\\b'
grepl(pattern = near, string)
str_detect(string = string,pattern = near)

# breaking it down
# word boundary   \\b
# word 1          student
# Not a word at least once (i.e. space, punctuation, etc.)  \\W+
# Match but do not capture/return result  ?:
# Word at least once, followed by not a word at least once  \\w+\\W+
# Quantifier, lower boundary and upper boundary {1,6}?
# word 2  BYU
# word boundary \\b

# Task: Identify patents with the words "autonomous" and "vehicle" within 5 words of each other (either direction)


################################################################################
# Pre-processing: cleaning, tokens, and dtm's

# The Quanteda package
# https://tutorials.quanteda.io/basic-operations/workflow/  

# creating a corpus
abstracts <- unique(dt[,c('patent_id','patent_abstract','patent_date')]) # get unique documents from cpc-level data

# dealing with accents and special characters:
# encoding - for when you want to keep the letters and remove the accent
abstracts$patent_abstract <- iconv(abstracts$patent_abstract,to="ASCII//TRANSLIT")
# typing out the list and using str_replace_all - for when you want to remove the entire character
abstracts$patent_abstract <- str_replace_all(string = abstracts$patent_abstract,pattern = 'Â|®|¢|â',replacement = '')

abstract_corpus <- corpus(abstracts$patent_abstract,docnames = abstracts$patent_id)
docvars(abstract_corpus,field = 'year') <- year(abstracts$patent_date) # add document variables/attributes
summary(abstract_corpus)

# creating tokens objects
abstract_tokens <- quanteda::tokens(x = abstract_corpus,what = 'word',remove_punct = T,remove_symbols = T,remove_numbers = T,include_docvars = T)
head(abstract_tokens)

# remove stopwords
my_stopwords <- fread('data/stopwords.csv') # my own list of words I want to remove
stopwords() # a standard list of stopwords from quanteda
abstract_tokens <- tokens_select(x = abstract_tokens,selection = 'remove',pattern = c(stopwords(),my_stopwords$token))
head(abstract_tokens) # compare

# lower case
abstract_tokens <- tokens_tolower(x = abstract_tokens)

### Stemming (the "root" of the word) vs Lemmatization (standardizing the word form)
# stemming 
abstract_tokens_stems <- tokens_wordstem(x = abstract_tokens)

# lemmatization
abstract_tokens_lemmas <- tokens_replace(abstract_tokens, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
head(abstract_tokens_lemmas)

# Document feature matrix 
abstract_dfm <- dfm(x = abstract_tokens_lemmas)
head(abstract_dfm) 

# Now let's do it for ngrams where n=3
abstract_tokens_ngrams <- tokens_ngrams(x = abstract_tokens_lemmas,n = 3,concatenator = ' ')
head(abstract_tokens_ngrams)

# Finally, let's weight the ngrams based on tf-idf to get a sense for how uniquely important they are to the document
# tf-idf: term frequency inverse document frequency. 
# This weights words/ngrams higher when they occur frequently within a single document AND less frequently across all documents
# i.e. words/ngrams that are more unique to and prevalent within a single document are weighted highly
abstract_dfm_ngrams <- dfm(abstract_tokens_ngrams) # 26 billion elements...
abstract_dfm_ngrams_tfidf <- dfm_tfidf(abstract_dfm_ngrams) # 26 billion elements...

head(abstract_dfm_ngrams)
head(abstract_dfm_ngrams_tfidf)

################################################################################
# Keyword extraction

# words and ngrams  
top_words <- textstat_frequency(x = abstract_dfm,n = 100)  
top_ngrams <- textstat_frequency(x = dfm(abstract_dfm_ngrams),n = 100)  

# by group, e.g. grouping by year to create a trendline of the topics  
top_ngrams_year <- textstat_frequency(x = dfm(abstract_dfm_ngrams),n = 10,groups = year) 
top_ngrams_year <- topfeatures(x = abstract_dfm_ngrams,n = 100,decreasing = T,groups = year)


# top keywords each year
fig <- plot_ly(x = top_ngrams_year %>% filter(rank==1) %>% pull(docfreq), 
               y = top_ngrams_year %>% filter(rank==1) %>% pull(group), 
               type = 'bar', 
               orientation = 'h',
               text = top_ngrams_year %>% filter(rank==1) %>% pull(feature),
               textposition = 'inside')
fig 

# trendline of keywords

# kwic
wearables <- kwic(x = abstract_tokens_lemmas,pattern = 'wearable',window = 10,case_insensitive = F)
wearables <- merge(wearables,patent[,c('patent_id','patent_date')],by.x = 'docname',by.y='patent_id')
wearables_trend <- wearables %>%
  group_by(year=year(patent_date)) %>%
  summarise(n=uniqueN(docname))
fig <- plot_ly(data = wearables_trend, x = ~year, y = ~n, type = 'bar')
fig



################################################################################  
# Themes/Topics using LDA  
# Quanteda does not have an implementation of LDA
# We will use the text2vec library - going to be slightly different functions but some familiar steps

# create tokens
lda_tokens <- itoken(unique(dt[1:5000,]$patent_abstract),
                     preprocessor = tolower,
                     tokenizer = word_tokenizer,
                     ids = unique(dt$patent_id))
lda_vocab <- create_vocabulary(lda_tokens,stopwords = stopwords(),ngram = 2:3)
lda_vector <- vocab_vectorizer(lda_vocab)
lda_dtm <- create_dtm(it = lda_tokens,vectorizer = lda_vector)

# choose model parameters
topic_cnt <- 5
doc_prior <- 0.1
word_prior <- 0.01
lda_model <- LDA$new(n_topics = topic_cnt,doc_topic_prior = doc_prior,topic_word_prior = word_prior)
lda_model_distribution <- lda_model$fit_transform(x = lda_dtm,n_iter = 1000,convergence_tol = 0.001,n_check_convergence = 25)
lda_model_top_words <- lda_model$get_top_words(n = 10)

# pick top topic
lda_model_distribution <- as.data.frame(lda_model_distribution)
lda_model_top_words <- as.data.frame(lda_model_top_words)
topic_names <- c('noise_reduction','acoustic_transducer','not_sure','hearing_aid','wearables')
colnames(lda_model_distribution) <- topic_names
colnames(lda_model_top_words) <- topic_names

lda_model_distribution$main_topic <- colnames(lda_model_distribution)[apply(X = lda_model_distribution, MARGIN=1, FUN=which.max)]

lda_model_distribution$patent_id <- row.names(lda_model_distribution)
lda_model_distribution <- merge(lda_model_distribution,patent[,c('patent_id','patent_date')], by='patent_id')

topic_trends <- lda_model_distribution %>%
  group_by(year=year(patent_date),main_topic) %>%
  summarise(n=uniqueN(patent_id)) %>% 
  pivot_wider(id_cols = year,names_from = main_topic,values_from = n)

fig <- plot_ly(topic_trends, x = ~year, y = ~topic_trends[[2]], type = 'scatter', mode = 'line', name = colnames(topic_trends)[2]) # change y-axis label
for (i in 3:length(topic_trends)) {
  print(colnames(topic_trends)[i])
  print(topic_trends[[i]])
  
  fig <- fig %>% add_trace( data = topic_trends, x = ~year, y = ~topic_trends[[i]], type = 'scatter', mode = 'line', name = colnames(topic_trends)[i])
}
fig


################################################################################
# POS tagging

# download and load the model 
udpipe_download_model(language = 'english')
udpipe_model <- udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')

# note that this library allows you to do a lot of the same text analytics

# annotate
wearable_abstracts <- abstracts %>% filter(grepl(pattern = 'wearable',x = abstracts$patent_abstract,ignore.case = T))
abstracts_pos <- udpipe_annotate(object = udpipe_model,x = wearable_abstracts$patent_abstract,doc_id = wearable_abstracts$patent_id) %>%
  as.data.frame()

top_tokens <- abstracts_pos %>%
  group_by(lemma,upos) %>%
  summarise(n=n()) %>%
  arrange(upos,desc(n)) %>%
  group_by(upos) %>%
  mutate(seq=row_number()) %>%
  filter(seq<=10)
