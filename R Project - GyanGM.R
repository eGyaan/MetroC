## FUNCITON DEFINITIONS ##

### DATA CLEANING FUNCIOTN
  # Function to clean junk characters from data; used for data cleaning
tidy.data <- function(data)
{
  str_replace_all(data, regex('\r\n|\n|\t|\r|,|/|<|>|\\.'), ' ')
}

### SCRAPE FUNCTION
  # Function to pull URL from web and extract HTML content as R dataset
  # First argument `search_page` is the first page of search results.
  # Second argument `nsPages` is the number of pages of search results to be scraped.

scrape_web = function(search_page, nsPages) {
  
  # Checking and installing required R packages
  
  # if (!require("magrittr")) {
  #   install.packages("magrittr")
  # }
  # 
  # if (!require("rvest")) {
  #   install.packages("rvest")
  # }
  # 
  # if (!require("tidyverse")) {
  #   install.packages("tidyverse")
  # }
  
  require(magrittr)
  require(rvest)
  require(tidyverse)
  
  # current_url is the page that loop works upon in each and every iteration.
  # Initializing it as search_page as that's the 1st page to be scraped our of other search results.
  current_url = search_page
  
  # Creating empty lists to store the scraped data.
  titles = list()
  
  companies = list()
  
  summaries = list()
  
  locations = list()
  
  joblinks = list()
  
  posteddate = list()
  
  
  # Setting index of the first element edited in each iteration of the loop to zero.
  k = 0
  
  # Looping through each search result.
  for (i in 1:nsPages) {
    
    # 1. Navigating to current_url & loading it into R as XML.
    current_page = read_html(current_url)
    
    # 2. Extracting job postings from current page, finding their HTML nodes containing job details, & storing them in empty lists created above.
    ads = current_page %>%
      html_nodes('[class="  row  result"]')
    
    # 3. Loop through each job posting.
    for (j in 1:length(ads)) {
      
      # 3a. Get the job title and add it to a list.
      titles[[j + k]] = ads[[j]] %>%
        html_nodes('[class=jobtitle]') %>%
        html_text()  # Outputted as character.
      
      # 3b. Get the company and add it to a list.
      companies[[j + k]] = ads[[j]] %>%
        html_nodes('[class=company]') %>%
        html_text()
      
      # 3c. Get the job summary and add it to a list.
      summaries[[j + k]] = ads[[j]] %>%
        html_nodes('[class=summary]') %>%
        html_text() 
      
      # 4d. Get the company location and add it to a list.
      locations[[j + k]] = ads[[j]] %>%
        html_nodes('[class=location]') %>%
        html_text() 
      
      # 5e. Get the job URLs and add it to a list.
      joblinks[[j + k]] = ads[[j]] %>%
        html_nodes("h2 a") %>%
        html_attr('href')
      
      # 6f. Get the job posted time and add it to a list.
      posteddate[[j + k]] = ads[[j]] %>%
        html_nodes('[class=date]') %>%
        html_text() 
    }
    
    # 4. Each list has k + j elements. Increase k so that we don't overwrite list contents in the next loop iteration.
    k = k + j
    
    # Note: We've scraped the data we want from every job ad on the current i.e. first search result page.
    
    # 5. Change the current URL to be the URL of the next page of search results.
    # Note: Indeed's search result page url's follow a simple pattern. 
    # Page 1 is 'https://www.indeed.ca/jobs?q=data+scientist&l=new+york%2C+ny'.
    # Page 2 is 'https://www.indeed.ca/jobs?q=data+scientist&l=new+york%2C+ny&start=10'.
    # Page 3 is ''https://www.indeed.ca/jobs?q=data+scientist&l=new+york%2C+ny&start=20'.
    
    current_url = paste0(search_page, '&start=', as.character(i * 10))
    
    # 6. Wait 1-5 seconds, to look more human.
    Sys.sleep(runif(n = 1, min = 1, max = 5))
 
    # 7. Repeat steps 1-6 for every page of search results.
  }
  
  # Combinining above lists into matrices that is also appending data in rows in new matrices.
  
  titles_final = do.call(rbind, titles)
  
  companies_final = do.call(rbind, companies)
  
  summaries_final = do.call(rbind, summaries)
  
  locations_final = do.call(rbind, locations)
  
  joblinks_final = do.call(rbind, joblinks)
  
  posteddate_final = do.call(rbind, posteddate)
  
  ## Some Data Cleaning using tidy.data() function created above and built-in function trimws.
  ## Basically removing junk characters and leading and trailing white spaces
  titles_final = trimws(tidy.data(titles_final))
  companies_final = trimws(tidy.data(companies_final))
  summaries_final = trimws(tidy.data(summaries_final))
  joblinks_final = trimws(joblinks_final)
  locations_final = trimws(tidy.data(locations_final))
  posteddate_final = trimws(tidy.data(posteddate_final))
  
  # Combining above matrices into one tibble {let's same data.frames with StringsAsFactors = F by default} and giving variable names.
  scraped = cbind(titles_final, companies_final, locations_final, summaries_final, paste0("https://www.indeed.ca",joblinks_final), posteddate_final) %>%
    as.tibble() %>%
    set_colnames(c('title', 'company', 'location', 'summary', 'link', 'postedD'))
  return(scraped)
}

# Scrapping 'data jobs' near by Ontario, Canada from www.indeed.ca.
data_jobs_scrapped = scrape_web(
  search_page = 'https://www.indeed.ca/jobs?as_and=&as_phr=&as_any=%22analyst%22,+%22scientist%22&as_not=&as_ttl=&as_cmp=&jt=all&st=&salary=&radius=50&l=Toronto,+ON&fromage=any&limit=50&sort=&psf=advsrch',
  nsPages = 20
)

## Filtering is not required for this project I don't need it as while searching itself I have supplied 'Analyst or Scientist' job titles.
## But can be useful if let's say we want to pull all job posting from ON and then filter only some using below filter conditions. OR can be used to refine the current results into specific job title or locaiton

## TRANSFORMING OR APPLYING FILTERS
# data_jobs_scrapped_2 = data_jobs_scrapped %>%
#   filter(
#     # Keep only ads w/ "Data Scientist" in the title.
#     grepl(pattern = 'analyst | scientist', x = title, ignore.case = TRUE)
#     #,
#     # Remove cases where job title contains "analyst" or seniority level.
#     #!grepl(pattern = 'analyst|sr.|jr.|senior|junior|lead|intern', 
#     #       x = title, ignore.case = TRUE)
#   ) 

#data_jobs_scrapped
View(data_jobs_scrapped)
# data_jobs_scrapped[order(data_jobs_scrapped$title),]

## REMOVING DUPLICATES
View(unique(data_jobs_scrapped))
dsJobsFinal <-unique(data_jobs_scrapped)

###### MOST POPULAR SKILLSETS for DS job
# Load packages
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)

# Indeed Search Words
job_title <- "\"analyst%2C+scientist\""
location <- "Toronto%2C+ON"

# use advanced search to get 50 results in a page
BASE_URL <- 'https://www.indeed.ca/'
ADV_URL <- paste0('https://www.indeed.ca/jobs?as_and=&as_phr=&as_any=%22analyst%22%2C+%22scientist%22&as_not=&as_ttl=&as_cmp=&jt=all&st=&salary=&radius=50&l=Toronto%2C+ON&fromage=any&limit=50&sort=&psf=advsrch', job_title, '&l=', location)
cat(ADV_URL)
cat(BASE_URL)

# get the html file from search url
start_page <- read_html(ADV_URL)

# get the total job count 
job_count <- unlist(strsplit(start_page %>% 
                               html_node("#searchCount") %>%
                               html_text(), split = ' ')) 
job_count <- as.numeric(str_replace_all(job_count[length(job_count)],',',''))
cat('Total job count: ', job_count)

# Get start page job URLs
links <- start_page %>%
  html_nodes("h2 a") %>%
  # html_text()
  html_attr('href')

# Get result page links
page.links <- start_page %>%
  html_nodes(xpath = '//div[contains(@class,"pagination")]//a') %>%
  html_attr('href')

KEYWORDS <- c('Hadoop','Python','\\bSQL', 'NoSQL','\\bR\\b', 'Spark', 'SAS', 'Excel', 'Java', 'Tableau', 'CSS', 'Oracle')

# Clean the raw html - removing commas, tabs, line changers, etc  
clean.text <- function(text)
{
  str_replace_all(text, regex('\r\n|\n|\t|\r|,|/|<|>|\\.'), ' ')
}

# Given running total dataframe and links to scrape skills and compute running total
ScrapeJobLinks <- function(res, job.links){
  for(i in 1:length(job.links)){
    job.url <- paste0(BASE_URL,job.links[i])
    
    #Sys.sleep(0.001)
    cat(paste0('Reading job ', i, '\n'))
    
    tryCatch({
      html <- read_html(job.url)
      company <- unlist(strsplit(html %>%
                                   html_node(".company") %>%
                                   html_text(), split = ' '))
      text <- html_text(html)
      text <- clean.text(text)
      df <- data.frame(skill = KEYWORDS, count = ifelse(str_detect(text, KEYWORDS), 1, 0))
      res$running$count <- res$running$count + df$count
      res$num_jobs <- res$num_jobs + 1
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  return(res)
}

# For display purpose, we also need the \\b removed from the keyword set
KEYWORDS_DISPLAY <- c('Hadoop','Python','SQL', 'NoSQL','R', 'Spark', 'SAS', 'Excel', 'Java', 'Tableau', 'CSS', 'Oracle')

# Create running total dataframe
running <- data.frame(skill = KEYWORDS_DISPLAY, count = rep(0, length(KEYWORDS_DISPLAY)))

# # Since the indeed only display max of 20 pages from search result, we cannot use job_count but need to track by creating a num_jobs
num_jobs <- 0
# 
# # Here is our results object that contains the two stats
results <- list("running" = running, "num_jobs" = num_jobs)

if(job_count != 0){
  cat('Scraping jobs in Start Page\n')
  results <- ScrapeJobLinks(results, links)
}

# for(p in 1:length(page.links)-1){
#   
#   cat('Moving to Next 50 jobs\n')
#   
#   # Navigate to next page
#   new.page <- read_html(paste0(BASE_URL, page.links[p]))
#   
#   # Get new page job URLs
#   links <- new.page %>%
#     html_nodes("h2 a") %>%
#     html_attr('href')
#   
#   # Scrap job links
#   results <- ScrapeJobLinks(results, links)
# }


###*********** ANALYSIS PART ***********### 
# Q1. Location wise job listings ?
require(plyr)
ljobs<- print(arrange(count(dsJobsFinal, 'location'), -freq))
ljobs

install.packages("plotrix")
library(plotrix)
pie3D(ljobs$freq, labels = ljobs$location, explode = 0.1, labelcol = "blue")

# Q2. What are top six companies with most jobs in data science ?
require(plyr)
cjobs <- head(print(arrange(count(dsJobsFinal, 'company'), -freq)))
barplot(cjobs$freq, main="Top 6 DS job providers", xlab = "Companies", ylab = "No of jobs"
        , names.arg = cjobs$company, col="brown")

# Q3. Analyst vs Scientist jobs
install.packages("sqldf")
require(sqldf)
aVSs <- sqldf("SELECT case when upper(title) like '%ANALYST%' then 'Analyst' else 'Scientist' end
      , count(*) from dsJobsFinal group by case when upper(title) like '%ANALYST%' then 'Analyst' else 'Scientist' end")
aVSs
names(aVSs) <- c("title", "count")
barplot(aVSs$count, main="Analyst vs Scientist Jobs", ylab = "Titles", xlab = "No of jobs"
        , names.arg = aVSs$title, col="darkmagenta", horiz = T)

# Q4. What is the most commonly used word in job postings ?
# install.packages("tm")
require("tm")
myCorpus <- Corpus(VectorSource(data_jobs_scrapped$summary))
my_tdm <- TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE, stopwords=TRUE))
m <- as.matrix(my_tdm)
words <- sort(rowSums(m), decreasing = T)
my_data <-data.frame(word = names(words), freq = words)
View(my_data)
# install.packages("wordcloud")
require(wordcloud)
wordcloud(words = my_data$word, freq = my_data$freq, min.freq = 2, max.words = 145, random.order = F
          , rot.per = 0.35, colors = brewer.pal(8, "Paired"))

hist(my_data$freq, main="Histogram for Air Passengers", xlab="Passengers"
     , ylab = "Score", col="darkmagenta")

# Q5. Most popular skillsets required for DS job ?
print(arrange(results$running, desc(count)))
require(ggplot2)
ggplot(results$running, aes(reorder(skill,-count), count)) + geom_bar(stat="identity") +
  labs(x = 'Skill', y = 'Occurrences (%)', title = "Skills required")

