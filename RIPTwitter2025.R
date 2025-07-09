install.packages(c("rvest", "jsonlite", "httr", "clipr", "glue", "tidyverse", "ggplot2", "dplyr", "rcrossref"))


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

#################################################
#################################################
# Script parameters

# Elsevier API

ev_base_url <- "https://api.elsevier.com/content/ev"
ev_api_key <- "7a9e3f84fd5e869e971bd1f8b525c9c7"
ev_api_inst_token <- "9da2a70c0a23dfa1aa5be16a0d41f569"

# Files

#base_dir <- "."
base_dir <- "~/Desktop"

# Crossref params

email_address <- "ryanmurt@uw.edu" 

################################################
################################################

raw_dir <- file.path(base_dir, "raw")
dir.create(raw_dir, showWarnings = FALSE)

data_dir <- file.path(base_dir, "data")
dir.create(data_dir, showWarnings = FALSE)

#Define function to call EV api - Retrieving the article list from Compendex database:
library(httr)
library(glue)

fetch_ev_data <- function(start_year, end_year, total_results = 4900, page_size = 100) {
  EV_running_cpx <- list()
  offsets <- seq(0, total_results, by = page_size)
  
  for (cur_offset in offsets) {
    url <- glue("{ev_base_url}/results?apiKey={ev_api_key}&query={api_query}&database=c&startYear={start_year}&endYear={end_year}&sortField=relevance&offset={cur_offset}&pageSize={page_size}&insttoken={ev_api_inst_token}")
    
    tryCatch({
      res <- GET(url)
      status <- status_code(res)
      
      if (status == 200) {
        res_text <- content(res, as = "text")
        EV_running_cpx <- c(EV_running_cpx, res_text)
        message("Success: offset ", cur_offset)
      } else {
        message("Failed at offset ", cur_offset, " — Status: ", status)
      }
      
      Sys.sleep(1.5)
    }, error = function(e) {
      message("Error at offset ", cur_offset, ": ", e$message)
    })
  }
  return(EV_running_cpx)
}

#This loop calls the fetch_ev_data function for each date range in list:
year_ranges <- list(
  c(2006, 2015),
  c(2016, 2017),
  c(2018, 2019),
  c(2020, 2021),
  c(2022, 2022),
  c(2023, 2023),
  c(2024, 2026)
)

all_results <- list()

for (range in year_ranges) {
  start <- range[1]
  end <- range[2]
  message("Fetching ", start, " to ", end)
  
  result <- tryCatch({
    fetch_ev_data(start, end)
  }, error = function(e) {
    warning(glue("Failed to fetch data for {start}-{end}: {e$message}"))
    NULL
  })
  
  all_results[[glue("{start}_{end}")]] <- result
}


#Parsing script:
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)

EV_cpx2025 <- data.frame()

for (year_key in names(all_results)) {
  result_list <- all_results[[year_key]]
  
  if (is.null(result_list)) {
    message("Skipping year range ", year_key, " (null result)")
    next
  }
  
  for (json_str in result_list) {
    if (!is.character(json_str)) {
      message("Skipping non-character JSON in ", year_key)
      next
    }
    
    res.df <- tryCatch({
      fromJSON(json_str, simplifyVector = FALSE)
    }, error = function(e) {
      message("invalid JSON in ", year_key, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(res.df) || is.null(res.df$PAGE$`PAGE-RESULTS`)) next
    
    page_results <- res.df$PAGE$`PAGE-RESULTS`
    
    if (is.list(page_results) && length(page_results) == 1 && is.null(names(page_results))) {
      page_results <- page_results[[1]]
    }
    
    if (!is.list(page_results) || is.null(page_results$`PAGE-ENTRY`)) {
      message("Skipping entry in ", year_key, " — PAGE-RESULTS is not a list or missing PAGE-ENTRY (type: ", typeof(page_results), ")")
      next
    }
    
    temp <- tryCatch({
      as_tibble(page_results) %>%
        unnest_wider(`PAGE-ENTRY`) %>%
        unnest_wider(`EI-DOCUMENT`)
    }, error = function(e) {
      message("Skipping entry in ", year_key, " due to structure error: ", e$message)
      return(NULL)
    })
    
    if (is.null(temp)) next
    
    doc_properties <- tryCatch({
      temp %>%
        select(DOCUMENTPROPERTIES, DOC) %>%
        unnest_wider(DOCUMENTPROPERTIES) %>%
        unnest_wider(DOC)
    }, error = function(e) {
      message("Skipping doc_properties in ", year_key, " due to unnesting error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(doc_properties)) {
      EV_cpx2025 <- bind_rows(EV_cpx2025, doc_properties)
    }
  }
}

message("Parsing complete! Final row count: ", nrow(EV_cpx2025))

write_csv(EV_cpx2025, file.path(data_dir, "EV_cpx2025.csv"))

#Retrieve abstracts
library(httr)
library(glue)
library(readr)
library(dplyr)
library(jsonlite)

EV_cpx2025 <- read_csv(file.path(data_dir, "EV_cpx2025.csv"))
vec_docid_cpx <- EV_cpx2025$`DOC-ID` %>% unique()

abs_cpx2025 <- list()
failed_ids <- c()

get_ev_record <- function(doc_id, max_attempts = 5) {
  attempt <- 1
  while (attempt <= max_attempts) {
    try({
      url <- glue("{ev_base_url}/records?apiKey={ev_api_key}&docId={doc_id}&insttoken={ev_api_inst_token}")
      resp <- GET(url)
      
      if (status_code(resp) == 200) {
        content_text <- content(resp, as = "text", encoding = "UTF-8")
        return(content_text)
      } else if (status_code(resp) == 429) {
        message(glue("Rate limited on doc_id {doc_id}. Sleeping 60s..."))
        Sys.sleep(60)
      } else if (status_code(resp) == 401) {
        message(glue("Unauthorized (401) on doc_id {doc_id}. Check your API key or token."))
        return(NULL)  # Don't retry unauthorized errors
      } else {
        message(glue("HTTP error {status_code(resp)} on doc_id {doc_id}."))
      }
    }, silent = TRUE)
    
    attempt <- attempt + 1
    Sys.sleep(2)
  }
  return(NULL)
}

# Main loop
for (i in seq_along(vec_docid_cpx)) {
  cur_doc_id <- vec_docid_cpx[i]
  message(glue("[{i}/{length(vec_docid_cpx)}] Fetching doc ID: {cur_doc_id}"))
  
  result <- get_ev_record(cur_doc_id)
  
  if (!is.null(result)) {
    abs_cpx2025 <- c(abs_cpx2025, result)
  } else {
    failed_ids <- c(failed_ids, cur_doc_id)
    write_lines(cur_doc_id, "failed_ids_log.txt", append = TRUE)
  }
  
  if (i %% 100 == 0) {
    saveRDS(abs_cpx2025, file = "abs_cpx2025_progress.rds")
    message("Saved checkpoint.")
  }
  
  Sys.sleep(runif(1, 1, 2))
}

# Parsing abstract api output
raw_dir <- file.path("~/Desktop/data")
data_dir <- raw_dir 
library(jsonlite)
library(tidyverse)

EV_running_abstracts_cpx <- data.frame()

for (i in seq_along(abs_cpx2025)) {
  message("Processing entry ", i, " of ", length(abs_cpx2025))
  
  result <- tryCatch({
    vec.df <- fromJSON(abs_cpx2025[[i]], simplifyVector = FALSE)
    page_results_vec <- vec.df$PAGE$`PAGE-RESULTS`
    
    temp_vec <- as_tibble(page_results_vec) %>%
      unnest_wider(`PAGE-ENTRY`) %>%
      unnest_wider(`EI-DOCUMENT`)
    
    doc_properties_inspec <- temp_vec %>%
      select(DOCUMENTPROPERTIES, DOC) %>%
      unnest_wider(col = DOCUMENTPROPERTIES) %>%
      unnest_wider(col = DOC)
    
    doc_properties_inspec
    
  }, error = function(e) {
    message("  Skipping entry ", i, " due to error: ", e$message)
    NULL
  })
  
  if (!is.null(result)) {
    EV_running_abstracts_cpx <- bind_rows(EV_running_abstracts_cpx, result)
  }
}

# Remove duplicates if any
EV_running_abstracts_cpx <- distinct(EV_running_abstracts_cpx)

# FINAL CLEANING
# Select and rename columns 
library(dplyr)

EV_cpx_finalJune2025 <- EV_running_abstracts_cpx %>% 
  mutate(`Publication Title` = coalesce(CF, RIL)) %>% 
  select(doc_id = `DOC-ID`,
         DOI = DO,
         Title = TI,
         Abstract = AB,
         Year = YR,
         manuscript_date = MD,
         doc_type = DT, 
         publishing_company = PN,
         `Publication Title`,
         h_index = HITINDEX)

# removing duplicates by DOC-ID
EV_cpx_finalJune2025 <- EV_cpx_finalJune2025 %>%
  distinct(doc_id, .keep_all = TRUE)

write_csv(EV_cpx_finalJune2025, file.path(data_dir, "EV_cpx_finalJune2025.csv"))
                                
################
# Retrieving the article list from Engineering Village API (Inspec Database):

#Define function to call EV api - Retrieving the article list from Inspec database:
library(httr)
library(glue)

fetch_ev_inspec_data <- function(start_year, end_year, total_results = 4900, page_size = 100) {
  EV_running_inspec <- list()
  offsets <- seq(0, total_results, by = page_size)
  
  for (cur_offset in offsets) {
    url <- glue("{ev_base_url}/results?apiKey={ev_api_key}&query={api_query}&database=i&startYear={start_year}&endYear={end_year}&sortField=relevance&offset={cur_offset}&pageSize={page_size}&insttoken={ev_api_inst_token}")
    
    tryCatch({
      res <- GET(url)
      status <- status_code(res)
      
      if (status == 200) {
        res_text <- content(res, as = "text")
        EV_running_inspec <- c(EV_running_inspec, res_text)
        message("Success: offset ", cur_offset)
      } else {
        message("Failed at offset ", cur_offset, " — Status: ", status)
      }
      
      Sys.sleep(1.5)  
    }, error = function(e) {
      message("Error at offset ", cur_offset, ": ", e$message)
    })
  }
  return(EV_running_inspec)
}

#This loop calls the fetch_ev_inspec_data function for each date range in list:
year_ranges_inspec <- list(
  c(2006, 2016),
  c(2017, 2019),
  c(2020, 2021),
  c(2022, 2023),
  c(2024, 2026)
)

all_results <- list()

for (range in year_ranges_inspec) {
  start <- range[1]
  end <- range[2]
  message("Fetching ", start, " to ", end)
  
  result <- tryCatch({
    fetch_ev_inspec_data(start, end)
  }, error = function(e) {
    warning(glue("Failed to fetch data for {start}-{end}: {e$message}"))
    NULL
  })
  
  all_results[[glue("{start}_{end}")]] <- result
}

# Parse the output 
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)

EV_inspec2025 <- data.frame()

for (year_key in names(all_results)) {
  result_list <- all_results[[year_key]]
  
  if (is.null(result_list)) {
    message("Skipping year range ", year_key, " (null result)")
    next
  }
  
  for (json_str in result_list) {
    if (!is.character(json_str)) {
      message("Skipping non-character JSON in ", year_key)
      next
    }
    
    res.df <- tryCatch({
      fromJSON(json_str, simplifyVector = FALSE)
    }, error = function(e) {
      message("Invalid JSON in ", year_key, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(res.df) || is.null(res.df$PAGE$`PAGE-RESULTS`)) next
    
    page_results <- res.df$PAGE$`PAGE-RESULTS`
    
    if (is.list(page_results) && length(page_results) == 1 && is.null(names(page_results))) {
      page_results <- page_results[[1]]
    }
    
    # Check if page_results is a list before accessing $PAGE-ENTRY
    if (!is.list(page_results) || is.null(page_results$`PAGE-ENTRY`)) {
      message("Skipping entry in ", year_key, " — PAGE-RESULTS is not a list or missing PAGE-ENTRY (type: ", typeof(page_results), ")")
      next
    }
    
    temp <- tryCatch({
      as_tibble(page_results) %>%
        unnest_wider(`PAGE-ENTRY`) %>%
        unnest_wider(`EI-DOCUMENT`)
    }, error = function(e) {
      message("Skipping entry in ", year_key, " due to structure error: ", e$message)
      return(NULL)
    })
    
    if (is.null(temp)) next
    
    doc_properties <- tryCatch({
      temp %>%
        select(DOCUMENTPROPERTIES, DOC) %>%
        unnest_wider(DOCUMENTPROPERTIES) %>%
        unnest_wider(DOC)
    }, error = function(e) {
      message("Skipping doc_properties in ", year_key, " due to unnesting error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(doc_properties)) {
      EV_inspec2025 <- bind_rows(EV_inspec2025, doc_properties)
    }
  }
}

message("Parsing complete. Final row count: ", nrow(EV_inspec2025))

write_csv(EV_inspec2025, file.path(data_dir, "EV_inspec2025.csv"))

#Abstract api call with built in error safeties:
library(httr)
library(glue)
library(readr)
library(dplyr)
library(jsonlite)

EV_inspec2025 <- read_csv(file.path(data_dir, "EV_inspec2025.csv"))
vec_docid_inspec <- EV_inspec2025$`DOC-ID` %>% unique()

abs_inspec2025 <- list()
failed_ids <- c() 

get_ev_inspec_record <- function(doc_id, max_attempts = 5) {
  attempt <- 1
  while (attempt <= max_attempts) {
    try({
      url <- glue("{ev_base_url}/records?apiKey={ev_api_key}&docId={doc_id}&insttoken={ev_api_inst_token}")
      resp <- GET(url)
      
      if (status_code(resp) == 200) {
        content_text <- content(resp, as = "text", encoding = "UTF-8")
        return(content_text)
      } else if (status_code(resp) == 429) {
        message(glue("Rate limited on doc_id {doc_id}. Sleeping 60s..."))
        Sys.sleep(60)
      } else if (status_code(resp) == 401) {
        message(glue("Unauthorized (401) on doc_id {doc_id}. Check your API key or token."))
        return(NULL)  # Don't retry unauthorized errors
      } else {
        message(glue("HTTP error {status_code(resp)} on doc_id {doc_id}."))
      }
    }, silent = TRUE)
    
    attempt <- attempt + 1
    Sys.sleep(2)
  }
  return(NULL)
}

# Main loop
for (i in seq_along(vec_docid_inspec)) {
  cur_doc_id <- vec_docid_inspec[i]
  message(glue("[{i}/{length(vec_docid_inspec)}] Fetching doc ID: {cur_doc_id}"))
  
  result <- get_ev_inspec_record(cur_doc_id)
  
  if (!is.null(result)) {
    abs_inspec2025 <- c(abs_inspec2025, result)
  } else {
    failed_ids <- c(failed_ids, cur_doc_id)
    write_lines(cur_doc_id, "failed_ids_log.txt", append = TRUE)
  }
  
  if (i %% 100 == 0) {
    saveRDS(abs_inspec2025, file = "abs_inspec2025_progress.rds")
    message("Saved checkpoint.")
  }
  
  Sys.sleep(runif(1, 1, 2))
}

# Parsing abstract API output
raw_dir <- file.path("~/Desktop/data")
data_dir <- raw_dir 
library(jsonlite)
library(tidyverse)

EV_running_abstracts_inspec <- data.frame()

for (i in seq_along(abs_inspec2025)) {
  message("Processing entry ", i, " of ", length(abs_inspec2025))
  
  result <- tryCatch({
    vec.df <- fromJSON(abs_inspec2025[[i]], simplifyVector = FALSE)
    page_results_vec <- vec.df$PAGE$`PAGE-RESULTS`
    
    temp_vec <- as_tibble(page_results_vec) %>%
      unnest_wider(`PAGE-ENTRY`) %>%
      unnest_wider(`EI-DOCUMENT`)
    
    doc_properties_inspec <- temp_vec %>%
      select(DOCUMENTPROPERTIES, DOC) %>%
      unnest_wider(col = DOCUMENTPROPERTIES) %>%
      unnest_wider(col = DOC)
    
    doc_properties_inspec
    
  }, error = function(e) {
    message("  Skipping entry ", i, " due to error: ", e$message)
    NULL
  })
  
  if (!is.null(result)) {
    EV_running_abstracts_inspec <- bind_rows(EV_running_abstracts_inspec, result)
  }
}

# Remove duplicates if any
EV_running_abstracts_inspec <- distinct(EV_running_abstracts_inspec)

# FINAL CLEANING
# Select and rename columns 
library(dplyr)

EV_inspec_finalJune2025 <- EV_running_abstracts_inspec %>% 
  mutate(`Publication Title` = coalesce(CF, RIL)) %>% 
  select(doc_id = `DOC-ID`,
         DOI = DO,
         Title = TI,
         Abstract = AB,
         Year = PD_YR,
         manuscript_date = MD,
         doc_type = DT, 
         `Publication Title`,
         h_index = HITINDEX)
  
# removing duplicates by DOC-ID
EV_inspec_finalJune2025 <- EV_inspec_finalJune2025 %>%
  distinct(doc_id, .keep_all = TRUE)

write_csv(EV_inspec_finalJune2025, file.path(data_dir, "EV_inspec_finalJune2025.csv"))

##################################
# Combining Compex and Inspec datasets:
EV_finalJune2025 <- bind_rows(EV_cpx_finalJune2025, EV_inspec_finalJune2025)
write_csv(EV_finalJune2025, file.path(data_dir, "EV_finalJune2025.csv"))

# Combining 6 other databases:
# Results from 6 databases (Library and Information Science Source, Library
# Information Science and Technology Abstracts, Web of Science, Global
# Health, ACM Digital Library, IEEE Xplore) were downloaded from the database
# websites and input into Zotero, then uploaded to R.
library(dplyr)
library(readr)  

database_files <- c(
  "LISS-LISTA2006-June2025.csv",
  "wos2006-June2025.csv",
  "acm2006-June2025.csv",
  "IEEE2006-June2025.csv",
  "GlobalHealth2006-June2025.csv"
)

# Column names to keep/use
standard_cols <- c("Title", "Abstract", "Authors", "Year", "Publication Title", "DOI")

colname_mapping <- list(
  "Authors" = c("Author", "Authors"),
  "Title" = c("Title", "Document Title", "title"),
  "Abstract" = c("Abstract", "abstract", "Abstract Note"),
  "Year" = c("year", "Publication Year"),
  "Publication Title" = c("Publication Title", "publisher"),
  "DOI" = c("DOI")
)

standardize_columns <- function(df) {
  names(df) <- trimws(names(df))
  for (std_col in names(colname_mapping)) {
    matches <- colname_mapping[[std_col]]
    match_found <- intersect(matches, names(df))
    if (length(match_found) > 0) {
      names(df)[names(df) == match_found[1]] <- std_col
    }
  }
  
  for (col in standard_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  return(df[, standard_cols])
}

# Merge all files
Combined_6 <- data.frame()
for (filename in database_files) {
  df <- read_csv(file.path(data_dir, filename), show_col_types = FALSE)
  df <- standardize_columns(df)
  Combined_6 <- bind_rows(Combined_6, df)
}

# Combining EV_finalJune2025 with Combined_6 which contains papers from the
# other 6 databases:
EV_finalJune2025$Year <- as.character(EV_finalJune2025$Year)
Combined_6$Year <- as.character(Combined_6$Year)

Combined_finalJune2025 <- bind_rows(EV_finalJune2025, Combined_6)
Combined_finalJune2025 <- Combined_finalJune2025[c("Authors", "Title", "Abstract", "Year", "Publication Title", "DOI")]

write.csv(Combined_finalJune2025, file.path(data_dir, "Combined_finalJune2025.csv"), row.names=FALSE)

# Removing any duplicates..use all 3 to catch all of them.
Combined_finalJune2025 <- Combined_finalJune2025 %>%
  distinct(DOI, .keep_all = TRUE)
write.csv(Combined_finalJune2025, file.path(data_dir, "Combined_finalJune2025.csv"), row.names=FALSE)

# Transform Publication Title column in the following ways: make all characters lowercase, remove "the", "proceedings of", and all punctuation. 
# This will allow for clean analysis such as counting how many articles were published in a particular journal or conference.

library(dplyr)
library(stringr)

Combined_finalJune2025 <- Combined_finalJune2025 %>%
  mutate(
    `Publication Title Stripped` = `Publication Title` %>%
      str_to_lower() %>%                                 # Make lowercase
      str_remove_all("\\b\\d+(st|nd|rd|th)?\\b") %>%      # Remove numbers and ordinal suffixes
      str_remove_all("\\bthe\\b") %>%                     # Remove 'the'
      str_remove_all("\\bproceedings of\\b") %>%          # Remove 'proceedings of'
      str_remove_all("[[:punct:]]") %>%                   # Remove punctuation
      str_squish()                                        # Remove extra whitespace
  )

# Move "Publication Title Stripped" before "DOI"
Combined_finalJune2025 <- Combined_finalJune2025 %>%
  select(
    -`Publication Title`, -DOI, -`Publication Title Stripped`,
    `Publication Title`,
    `Publication Title Stripped`,
    DOI
  )

# Keep only 4 digit year in "Year" column and drop things like "2 Sept."
Combined_finalJune2025 <- Combined_finalJune2025 %>%
  mutate(Year = stringr::str_extract(Year, "\\b\\d{4}\\b"))

write.csv(Combined_finalJune2025, file.path(data_dir, "Combined_finalJune2025.csv"), row.names=FALSE)

##########################
##Begin data analysis:

Combined_finalJune2025 <- read.csv(file.path(data_dir, "Combined_finalJune2025.csv"))

# Counting the number of articles per year
year_count <- Combined_finalJune2025 %>% group_by(Year) %>% 
  summarise(number_of_studies=n()) %>% 
  filter(number_of_studies > 1)

view(year_count)

# Graphing the number of articles per year
# I also did this in Excel for ease of manipulating visualization
year_count$Year <- as.numeric(as.character(year_count$Year))

ggplot(data = year_count) + 
  geom_col(aes(x = Year, y = number_of_studies)) + 
  scale_x_continuous(breaks = seq(2000, 2025, by = 1)) +
  labs(x = "Year", y = "Number of Studies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

