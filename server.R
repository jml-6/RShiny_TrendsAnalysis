

################################################################################
# define the server function

server <- function(input,output,session) {

   
   competition <- reactiveValues(plot = plotly_empty())
   trend <- reactiveValues(plot = plotly_empty())
   

   ##Here we create our output and stuff
   observeEvent(input$generate_competitive_position, {
     dt <- cpc %>% filter(grepl(pattern = paste(input$comp_cpc_codes,sep ='',collapse='|'),x = cpc$cpc_group,ignore.case = T))
     dt <- merge(dt,patent,by = 'patent_id')
     # merge with assignee
     dt <- merge(dt,assignee,by = 'patent_id') # why is this dropping? no assignee?
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
     #Add in other inputs###########################################################################################################
     segments_names <- strsplit(input$comp_label, ",\\s*")[[1]]

     segments_codes <- strsplit(input$comp_subcode, ",\\s*")[[1]]

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
     
     #totals$total_sum <- totals['Enzyme Inhibitors'] + totals["Hydrolyzed Proteins"] + totals["Peptides of Undefined Length"] + totals['Fully Defined Peptides of up to Twenty Amino Acids'] + totals['Peptides Containing Over Twenty Amino Acids'] + totals['Not Fully Defined Peptides that Contain up to Twenty Amino Acids']
     
     # Save file
     #write.csv(totals,'output/competitive positioning.csv',row.names = F)
     
     ###GRAPH MAKING
     # Assuming your "totals" table has a column called "total" with the values you want to represent as bar lengths
     
     theme_set(theme_gray())
     ###COMPETITION ANALYSIS#####
     # Create a vector of heights (total column of totals table)
     heights <- totals$cagr_2017_2021
     
     df <- data.frame(
       heights = heights,
       names = totals$disambig_assignee_organization
     )
     
     competition$plot <- ggplot(data = df) +
       geom_bar(aes(x = names, y = heights, fill = names), 
                stat = "identity", position = "dodge") +
       labs(title = "Competition Analysis (2017 - 2021)", y = "CAGR") +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = 'none')
     

     
     output$competition_plot <- renderPlot({competition$plot})
     
     

     
   })
   
   
   observeEvent(input$generate_trend, {
     dt <- cpc %>% filter(grepl(pattern = paste(input$trends_input1,sep ='',collapse='|'),x = cpc$cpc_group,ignore.case = T))
     dt <- merge(dt,patent,by = 'patent_id')
     # merge with assignee
     dt <- merge(dt,assignee,by = 'patent_id') # why is this dropping? no assignee?
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
     #Add in other inputs###########################################################################################################
     segments_names <- strsplit(input$trends_input3, ",\\s*")[[1]]
     segments_codes <- strsplit(input$trends_input2, ",\\s*")[[1]]

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
     
    # totals$total_sum <- totals['Enzyme Inhibitors'] + totals["Hydrolyzed Proteins"] + totals["Peptides of Undefined Length"] + totals['Fully Defined Peptides of up to Twenty Amino Acids'] + totals['Peptides Containing Over Twenty Amino Acids'] + totals['Not Fully Defined Peptides that Contain up to Twenty Amino Acids']
     
     # Save file
     #write.csv(totals,'output/competitive positioning.csv',row.names = F)
     
     ###GRAPH MAKING
     # Assuming your "totals" table has a column called "total" with the values you want to represent as bar lengths
     
     theme_set(theme_gray())
     ###COMPETITION ANALYSIS#####
     # Create a vector of heights (total column of totals table)
     heights <- totals$cagr_2017_2021
     
     # Create a bar chart
      
     browser
     trend$plot <- totals %>%
       pivot_longer(cols = 5:last_col(),
                    names_to = "subcode type") %>%
       ggplot(mapping = aes(y=disambig_assignee_organization,
                            x=value,
                            fill=`enzyme type`)) +
       geom_col()+
       labs(y="") +
       scale_fill_brewer(type = "qual", palette = "Dark2")
     
     
     output$trend_plot <- renderPlot({trend$plot})
     
     
     
     
   })
   

  
  
}

