library(httr)
library(tidyverse)
library(data.table)
library(feather)

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

theme_set(theme_gray())
###COMPETITION ANALYSIS#####
# Create a vector of heights (total column of totals table)
heights <- totals$cagr_2017_2021

# Create a bar chart
barplot(heights, 
        main = "Competition Analysis (2017 - 2021)",  # Specify the title of the chart
        ylab = "CAGR",               # Specify the label for the y-axis
        names.arg = totals$disambig_assignee_organization,
        las = 3) # Specify the labels for the x-axis (assuming "category" is the column name in your "totals" table)
# Note: You can customize other aspects of the bar chart, such as font size, axis limits, etc., using additional parameters in the barplot() function.

###TREND ANALYSIS

# library

library(tidyverse)
library(RColorBrewer)

totals %>%
  pivot_longer(`Enzyme Inhibitors`:`Not Fully Defined Peptides that Contain up to Twenty Amino Acids`,
               names_to = "enzyme type") %>%
  ggplot(mapping = aes(y=disambig_assignee_organization,
                       x=value,
                       fill=`enzyme type`)) +
  geom_col()+
  labs(y="") +
  scale_fill_brewer(type = "qual", palette = "Dark2")
  
