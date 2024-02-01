library(rvest)
library(jsonlite)
library(httr)
library(clipr)
library(glue)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rcrossref)

# Results from 6 databases (Library and Information Science Source, Library
# Information Science and Technology Abstracts, Web of Science, Global
# Health, ACM Digital Library, IEEE Xplore) were downloaded from the database
# websites and input into Zotero, then uploaded to R.

# Engineering Village (which contains Compendex and Inspec databases) results
# were too large to download from the website, so we accessed via the API.
# See Elsevier's Developer Portal at
# https://dev.elsevier.com/interactive.html

################################################
################################################
# Script parameters

# Elsevier API

ev_base_url <- "https://api.elsevier.com/content/ev"
ev_api_key <- "XXXX"
ev_api_inst_token <- "XXXX"

# Files

base_dir <- "."

# Crossref params

email_address <- "XXX" # for adding "mailto" parameter to request so they can contact me if needed

################################################
################################################

raw_dir <- file.path(base_dir, "raw")
dir.create(raw_dir, showWarnings = FALSE)

data_dir <- file.path(base_dir, "data")
dir.create(data_dir, showWarnings = FALSE)


# Retrieving the article list from Engineering Village API
# (Compendex Database):

max_page <- 21870
# 21870 is the number of results found in our Compendex database website search. Will change with future searches.

EV_running_cpx <- list()
api_query <- URLencode("(((((twitter AND data) OR (twitter AND api) OR (twitter AND dataset) NOT survey) WN ALL)) NOT (({ch} OR {ip} OR {bk} OR {er} OR {tb} OR {ed}) WN DT))", reserved = TRUE)
for (cur_page in 0:max_page) {
  res1 <- GET(glue("{ev_base_url}/results?apiKey={ev_api_key}&query={api_query}&database=c&sortField=relevance&offset={cur_page}&pageSize=100&insttoken={ev_api_inst_token}"))
  res1.content <- content(res1, as = "text")
  EV_running_cpx <- c(res1.content, EV_running_cpx)
}

# Remove blank items from lists:
# We noticed that EV_running_cpx contained some blank rows (1-162), so we needed to remove them in order to move forward. 
# Row numbers will probably be different when code is run in the future.
EV_running_cpx <- EV_running_cpx [- c(1:162)]

saveRDS(EV_running_cpx, file = file.path(raw_dir, "EV_running_cpx.RDS"))

# Parse the output 
EV_running_cpx <- readRDS(file.path(raw_dir, "EV_running_cpx.RDS"))
EV_cpx2023 <- data.frame()
for(i in EV_running_cpx){
  res.df <- fromJSON(i,  simplifyVector = FALSE)
  page_results <- res.df$PAGE$`PAGE-RESULTS`
  temp = as_tibble(page_results) %>% 
    unnest_wider(`PAGE-ENTRY`) %>% 
    unnest_wider(`EI-DOCUMENT`)
  
  doc_properties <- temp %>% 
    select(DOCUMENTPROPERTIES, DOC) %>% 
    unnest_wider(col = DOCUMENTPROPERTIES) %>% 
    unnest_wider(col = DOC)
  
  EV_cpx2023 <- bind_rows(doc_properties, EV_cpx2023)
}

write_csv(EV_cpx2023, file.path(data_dir, "EV_cpx2023.csv"))

# GET abstract for every article
EV_cpx2023 <- read_csv(file.path(data_dir, "EV_cpx2023.csv"))
vec_docid_cpx <- EV_cpx2023$`DOC-ID` %>% unique()
abs_cpx2023 <- list()
for (cur_doc_id in vec_docid_cpx) {
  resp <- GET(glue("{ev_base_url}/records?apiKey={ev_api_key}&docId={cur_doc_id}&insttoken={ev_api_inst_token}"))
  resp.content <- content(resp, as = "text")
  abs_cpx2023 <- c(abs_cpx2023, resp.content)
}

# Remove errors from list:
# We noticed that abs_cpx2023 contained an error in row 986, so we need to remove them to move forward. 
# Row numbers will probably be different when code is run in the future.
abs_cpx2023 <- abs_cpx2023 [- c(986)]

saveRDS(abs_cpx2023, file.path(raw_dir, "abs_cpx2023.RDS"))

# PARSING THE ABSTRACT API OUTPUT 
abs_cpx2023 <- readRDS(file.path(raw_dir, "abs_cpx2023.RDS"))
EV_running_abstracts_cpx <- data.frame()
for (i in abs_cpx2023){
  vec.df <- fromJSON(i,  simplifyVector = FALSE)
  page_results_vec <- vec.df$PAGE$`PAGE-RESULTS`
  temp_vec <- as_tibble(page_results_vec) %>% 
    unnest_wider(`PAGE-ENTRY`) %>% 
    unnest_wider(`EI-DOCUMENT`)
  
  doc_properties_inspec <- temp_vec %>% 
    select(DOCUMENTPROPERTIES, DOC) %>% 
    unnest_wider(col = DOCUMENTPROPERTIES) %>% 
    unnest_wider(col = DOC) 
  
  EV_running_abstracts_cpx <- bind_rows(doc_properties_inspec, EV_running_abstracts_cpx)
}
write_csv(EV_running_abstracts_cpx, file.path(data_dir, "EV_running_abstracts_cpx.csv"))

# FINAL CLEANING
# Select and rename columns 
EV_cpx_finalDec2023 <- EV_running_abstracts_cpx %>% 
  select(doc_id = `DOC-ID`,
         DOI = DO,
         title = TI,
         abstract = AB,
         year = YR,
         manuscript_date = MD,
         doc_type = DT, 
         publishing_company = PN,
         publisher = RIL,
         publisher2 = CF,
         h_index = HITINDEX)

write_csv(EV_cpx_finalDec2023, file.path(data_dir, "EV_cpx_finalDec2023.csv"))

# Use Excel to combine publisher and publisher2 into one column
# called "publisher".
EV_cpx_finalDec2023 <- read.csv(file.path(data_dir, "EV_cpx_finalDec2023.csv"))

# removing duplicates
EV_cpx_finalDec2023 <- read.csv(file.path(data_dir, "EV_cpx_finalDec2023.csv")
EV_cpx_final_duplicates <- EV_cpx_finalDec2023 %>%
  distinct(doc_id, .keep_all = TRUE)

################
# Retrieving the article list from Engineering Village API (Inspec Database):
# 15205 is the number of results found in our Inspec database search. Will change with future searches.
max_page <- 15205
EV_running_inspec <- list()
api_query <- URLencode("(((((twitter AND data) OR (twitter AND api) OR (twitter AND dataset) NOT survey) WN ALL)) NOT (({ch} OR {bk}) WN DT))", reserved = TRUE)
for (cur_page in 0:max_page) {
  res1 <- GET(glue("{ev_base_url}/results?apiKey={ev_api_key}&query={api_query}&database=i&sortField=relevance&offset={cur_page}&pageSize=100&insttoken={ev_api_inst_token}"))
  res1.content <- content(res1, as = "text")
  EV_running_inspec <- c(res1.content, EV_running_inspec)
}

# Remove blank items from lists:
# We noticed that EV_running_inspec contained some blank rows (1-74), so we need to remove them to move forward. 
# Row numbers will probably be different when code is run in the future.
EV_running_inspec <- EV_running_inspec [- c(1:74)]

saveRDS(EV_running_inspec, file = file.path(raw_dir, "EV_running_inspec.RDS"))

# Parse the output 
EV_running_inspec <- readRDS(file.path(raw_dir, "EV_running_inspec.RDS"))
EV_inspec2023 <- data.frame()
for(i in EV_running_inspec){
  res.df <- fromJSON(i,  simplifyVector = FALSE)
  page_results <- res.df$PAGE$`PAGE-RESULTS`
  temp <- as_tibble(page_results) %>% 
    unnest_wider(`PAGE-ENTRY`) %>% 
    unnest_wider(`EI-DOCUMENT`)
  
  doc_properties <- temp %>% 
    select(DOCUMENTPROPERTIES, DOC) %>% 
    unnest_wider(col = DOCUMENTPROPERTIES) %>% 
    unnest_wider(col = DOC)
  
  EV_inspec2023 <- bind_rows(doc_properties, EV_inspec2023)
}

write_csv(EV_inspec2023, file.path(data_dir, "EV_inspec2023.csv"))

# GET abstract for every article
EV_inspec2023 <- read_csv(file.path(data_dir, "EV_inspec2023.csv"))
vec_docid_inspec <- EV_inspec2023$`DOC-ID` %>% unique()
abs_inspec2023 <- list()
for (cur_doc_id in vec_docid_inspec) {
  resp <- GET(glue("{ev_base_url}/records?apiKey={ev_api_key}&docId={cur_doc_id}&insttoken={ev_api_inst_token}"))
  resp.content <- content(resp, as = "text")
  abs_inspec2023 <- c(abs_inspec2023, resp.content)
}
saveRDS(abs_inspec2023, file.path(raw_dir, "abs_inspec2023.RDS"))

# PARSING THE ABSTRACT API OUTPUT 
abs_inspec2023 <- readRDS(file.path(raw_dir, "abs_inspec2023.RDS"))
EV_running_abstracts_inspec <- data.frame()
for (i in abs_inspec2023){
  vec.df <- fromJSON(i,  simplifyVector = FALSE)
  page_results_vec <- vec.df$PAGE$`PAGE-RESULTS`
  temp_vec <- as_tibble(page_results_vec) %>% 
    unnest_wider(`PAGE-ENTRY`) %>% 
    unnest_wider(`EI-DOCUMENT`)
  
  doc_properties_inspec <- temp_vec %>% 
    select(DOCUMENTPROPERTIES, DOC) %>% 
    unnest_wider(col = DOCUMENTPROPERTIES) %>% 
    unnest_wider(col = DOC) 
  
  EV_running_abstracts_inspec <- bind_rows(doc_properties_inspec, EV_running_abstracts_inspec)
}
write_csv(EV_running_abstracts_inspec, file.path(data_dir, "EV_running_abstracts_inspec.csv"))

# FINAL CLEANING
# Select and rename columns 
EV_inspec_finalDec2023 <- EV_running_abstracts_inspec %>% 
  select(doc_id = `DOC-ID`,
         DOI = DO,
         title = TI,
         abstract = AB,
         year = PD_YR,
         manuscript_date = MD,
         doc_type = DT, 
         publishing_company = PN,
         publisher = RIL,
         publisher2 = CF,
         h_index = HITINDEX)
# Combine publisher and publisher2 into one column called "publisher".
EV_inspec_finalDec2023 <- EV_inspec_finalDec2023 %>%
  unite(publisher, publisher, publisher2, sep = " ")

write_csv(EV_inspec_finalDec2023, file.path(data_dir, "EV_inspec_finalDec2023.csv"))

# Use Excel to remove all "NA" from publisher column.
EV_inspec_finalDec2023 <- read.csv(file.path(data_dir, "EV_inspec_finalDec2023.csv"))

# removing duplicates
EV_inspec_finalDec2023 <- read.csv(file.path(data_dir, "EV_inspec_finalDec2023.csv"))
EV_inspec_final_duplicates <- EV_inspec_finalDec2023 %>%
  distinct(doc_id, .keep_all = TRUE)

EV_inspec_final <- read.csv(file.path(data_dir, "EV_inspec_final.csv"))
EV_inspec_final_duplicates2 <- EV_inspec_final %>%
  distinct(doc_id, .keep_all = TRUE)

# Change year to numeric from character
EV_inspec_finalDec2023$year <- as.numeric(as.character(EV_inspec_finalDec2023$year))

write_csv(EV_inspec_finalDec2023, file.path(data_dir, "EV_inspec_finalDec2023.csv"))

# Combining Compex and Inspec datasets:
EV_finalDec2023 <- bind_rows(EV_cpx_finalDec2023, EV_inspec_finalDec2023)
write_csv(EV_finalDec2023, file.path(data_dir, "EV_finalDec2023.csv"))

# Combining 6 other databases:
# Results from 6 databases (Library and Information Science Source, Library
# Information Science and Technology Abstracts, Web of Science, Global
# Health, ACM Digital Library, IEEE Xplore) were downloaded from the database
# websites and input into Zotero, then uploaded to R.
database_files <- c(
  "ebsco_finalDec2023.csv",
  "wos_finalDec2023.csv",
  "acm_finalDec2023.csv",
  "ieee_finalDec2023.csv",
  "global_health_finalDec2023.csv")
Combined_6 <- data.frame()
for (filename in database_files) {
  Combined_6 <- bind_rows(Combined_6, read.csv(file.path(data_dir, filename)))
}
Combined_6 <- Combined_6[c("title", "abstract", "year", "manuscript_date", "doc_type", "publisher", "publisher_stripped", "publishing_company", "DOI", "referenced_by_count", "Discipline", "Notes")]

# Combining EV_finalDec2023 with Combined_df which contains papers from the
# other 6 databases:
Combined_finalDec2023 <- bind_rows(EV_finalDec2023, Combined_6)
Combined_finalDec2023 <- Combined_finalDec2023[c("title", "abstract", "year", "manuscript_date", "doc_type", "publisher", "publisher_stripped", "publishing_company", "DOI", "referenced_by_count", "Discipline", "Notes")]

# Removing any duplicates..use all 3 to catch all of them.
Combined_finalDec2023 <- Combined_finalDec2023 %>% 
                          distinct(DOI, .keep_all = TRUE) %>% 
                          distinct(title, .keep_all = TRUE) %>% 
                          distinct(abstract, .keep_all = TRUE)
write.csv(Combined_finalDec2023, file.path(data_dir, "Combined_finalDec2023.csv"), row.names=FALSE)

# Use Excel to change all values to lowercase, so all caps and lowercase aren't calculated as 2 separate publishers. Also removed more stopwords (the, proceedings of, and all remaining numbers, suffixes, punctuation)
# reload adjusted Combined_final
Combined_final <- read_csv(file.path(data_dir, "Combined_finalDec2023"))

#randomize Combined_final before conducting analysis:
Combined_final_random <- slice(Combined_final, sample(1:n()))
write.csv(Combined_final_random, file.path(data_dir, "Combined_final_random.csv"), row.names=FALSE)


##########################
##Begin data analysis:

Combined_finalDec2023 <- read.csv(file.path(data_dir, "Combined_finalDec2023.csv"))

# Counting the number of articles per year
year_count <- Combined_finalDec2023 %>% group_by(year) %>% 
  summarise(number_of_studies=n()) %>% 
  filter(number_of_studies > 1)

write.csv(year_count, file.path(data_dir, "year_count.csv"), row.names=FALSE)

# Graphing the number of articles per year
# I also did this in Excel for ease of manipulating visualization
ggplot(data = year_count) + 
  geom_col(aes(x = year, y = number_of_studies)) + 
  scale_x_continuous(breaks=seq(0,2023,by=1)) + 
  labs(x = "Year", y = "Number of Studies")

# Counting the number of times a journal or conference comes up and
# putting in order
publisher_count2 <- Combined_final_random %>%
  group_by(publisher) %>% 
  summarise(number_of_studies=n()) %>%
  na.omit(publisher_count2) %>%
  arrange(desc(number_of_studies))

write.csv(publisher_count2, file.path(data_dir, "Publisher_Count2.csv"), row.names=FALSE)
publisher_count2 <- read_csv(file.path(data_dir, "Publisher_Count2.csv"))

# How many unique journals and conferences are represented in the data?
publisher_count2_unique <- unique(publisher_count2$Publisher) %>% na.omit()
# result: 7432

write.csv(publisher_count2_unique, file.path(data_dir, "publisher_count2_unique.csv"), row.names=FALSE)

# export publisher_count2_unique to excel and search for duplicates (happens when
# there are acronyms at the beginning or end of cell), adding cells together
# and then deleting extra cells.
#
# Occasionally several conferences combine into a single conference, or there
# are breakout workshops/series as part of a conference. I tried to select
# the primary conference or the conference with the widest umbrella.
#
# I stopped at row 145 (19 papers). It is possible there are others that if
# combined would make it into the top 100 journal/conferences, but I decided
# to make the call here. My suspicion is that even if there are a few more,
# they would not change the % for the discipline breakout.

Journals_Top_100 <- read_csv(file.path(data_dir, "publisher_count2_unique.csv"))

# Graphing Top 100 journals
ggplot(data = Journals_Top_100) + 
  geom_col(aes(x = Publisher, y = number_of_studies)) + 
  labs(x = "Journal/Conference", y = "Number of Studies") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

####################
# Using Crossref to find citations for each article

library("rjson")

# Create a vector containing all DOIs
Combined_finalDec2023 <- read.csv(file.path(data_dir, "Combined_finalDec2023.csv"), stringsAsFactors = FALSE)
FinalDec2023_vec <- Combined_finalDec2023$DOI

# Create for loop to collect all of the "is-referenced-by-count" numbers (number of citations):
cited_by_list <- numeric(0)  

for (cur_doi in FinalDec2023_vec) {
  resp <- GET(glue("https://api.crossref.org/works/{cur_doi}?mailto={email_address}"))
  
  if (resp[["status_code"]] == 404) {
    # Handle the 404 case
    cited_by_list <- c(cited_by_list, NA)  # Add NA to the list
  } else {
    json_content <- content(resp, as = "text")
    json_data <- fromJSON(json_content)
    count_data <- json_data[["message"]][["is-referenced-by-count"]]
    
    # Append the count_data to cited_by_list
    cited_by_list <- c(cited_by_list, count_data)
  }
  cited_by_df <- data.frame(referenced_by_count = cited_by_list)
}

# Save as csv to desktop (just in case a GET gets messed up)
write.csv(cited_by_df, file.path(data_dir, "cited_by_df.csv"), row.names=FALSE)

# Create and save new DataFrame with only the "DOI" column from final dataset
finalDec2023_DOIs <- data.frame(DOI = Combined_finalDec2023$DOI)
write.csv(finalDec2023_DOIs, file.path(data_dir, "finalDec2023_DOIs.csv"), row.names=FALSE)

# Read in full DOI list: 
finalDec2023_DOIs <- read.csv(file.path(data_dir, "finalDec2023_DOIs.csv"), stringsAsFactors = FALSE)
# Take cited_by_df and combine with DOI list to create one dataframe with 2 columns:
combined_dois_Dec2023 <- data.frame(referenced_by_count = unlist(cited_by_df), DOI = unlist(finalDec2023_DOIs))

# Sort the DataFrame in descending order based on "referenced_by_count"
combined_dois_Dec2023 <- combined_dois_Dec2023[order(-combined_dois_Dec2023$referenced_by_count), ]

# Calculate the sum of the "referenced_by_count" column
total_sum_dois <- sum(combined_dois_Dec2023$referenced_by_count)

# Save as csv to desktop:
write.csv(combined_dois_Dec2023, file.path(data_dir, "combined_dois_Dec2023.csv"), row.names=FALSE)

# Rank the final_dataset by referenced_by_count column:
final_dataset <- read.csv(file.path(data_dir, "final_dataset.csv"), stringsAsFactors = FALSE)
final_dataset_references <- final_dataset[order(-final_dataset$referenced_by_count), ]
write.csv(final_dataset_references, file.path(data_dir, "final_dataset_references.csv"), row.names=FALSE)

