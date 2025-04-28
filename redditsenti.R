#######################################################################
##### SETUP ########################################################
###################################################################

#install.packages(c('httr', 'jsonlite', 'dplyr'))

library(httr)
library(dplyr)
library(jsonlite)

### reddit api credentials

client_id <- "YOUR CLIENT ID"
client_secret <- "YOUR CLIENT SECRET"
username <- "YOUR USERNAME"
password <- "YOUR PASSWORD"
user_agent <- "sixerssentiment"

auth <- POST("https://www.reddit.com/api/v1/access_token",
             authenticate(client_id, client_secret),
             body = list(grant_type = "password", username = username, password = password),
             encode = "form",
             add_headers(`User-Agent` = user_agent))
token <- content(auth)$access_token

get_reddit_posts <- function(subreddit, query, after, before, limit = 100) {
  url <- paste0("https://oauth.reddit.com/r/", subreddit, "/search")
  
  res <- GET(url,
             add_headers(Authorization = paste("bearer", token),
                         `User-Agent` = user_agent),
             query = list(q = query,
                          restrict_sr = "on",
                          sort = "new",
                          limit = limit,
                          after = after,
                          before = before))
  
  posts_data <- content(res, as = "parsed")
  posts <- posts_data$data$children
  
  if (length(posts) == 0) {
    return(data.frame())
  }
  
  # Safely extract only needed fields
  posts_list <- lapply(posts, function(post) {
    post_data <- post$data
    
    if (is.null(post_data)) return(NULL)
    
    tibble(
      id = ifelse(!is.null(post_data$id), post_data$id, NA),
      title = ifelse(!is.null(post_data$title), post_data$title, NA),
      selftext = ifelse(!is.null(post_data$selftext), post_data$selftext, NA),
      author = ifelse(!is.null(post_data$author), post_data$author, NA),
      created_utc = ifelse(!is.null(post_data$created_utc), post_data$created_utc, NA),
      url = ifelse(!is.null(post_data$url), post_data$url, NA)
    )
  })
  
  # Remove NULLs
  posts_list <- Filter(Negate(is.null), posts_list)
  
  if (length(posts_list) == 0) {
    return(data.frame())
  }
  
  posts_df <- bind_rows(posts_list)
  
  return(posts_df)
}

#######################################################################
##### GET DATA ########################################################
###################################################################

subreddit <- "philadelphia"
query <- "arena"


### july 2022 to now
after_time <- 1656633600 
before_time <- 1745443200

posts_df <- get_reddit_posts(subreddit, query, after = after_time, before = before_time, limit = 100)

head(posts_df[c("title", "selftext", "created_utc")])

# Save posts to CSV
#write.csv(posts_df, "reddit_posts_feb2025.csv", row.names = FALSE)

# changing date to be readable
posts_df$created_date <- as.POSIXct(posts_df$created_utc, origin = "1970-01-01", tz = "UTC")

remove_indexes <- c(3, 7, 12)

# Remove irrelevant titles
#remove_indexes = c(8,30,39,46,53,56,59,62,71,74,84,85,93,94,95,96,97,98,99,100)
posts_df <- posts_df %>%
  slice(-remove_indexes)

## can i get comments for these posts too
get_reddit_comments <- function(post_id) {
  url <- paste0("https://oauth.reddit.com/comments/", post_id)
  
  res <- GET(url,
             add_headers(Authorization = paste("bearer", token),
                         `User-Agent` = user_agent))
  
  comments_data <- content(res, as = "parsed")
  
  # if no comments
  if (length(comments_data) < 2) {
    return(data.frame())
  }
  
  comments_list <- lapply(comments_data[[2]]$data$children, function(x) {
    comment_data <- x$data
    
    if (is.null(comment_data)) return(NULL)
    
    tibble(
      comment_id = ifelse(!is.null(comment_data$id), comment_data$id, NA),
      body = ifelse(!is.null(comment_data$body), comment_data$body, NA),
      author = ifelse(!is.null(comment_data$author), comment_data$author, NA),
      created_utc = ifelse(!is.null(comment_data$created_utc), comment_data$created_utc, NA),
      parent_id = ifelse(!is.null(comment_data$parent_id), comment_data$parent_id, NA)
    )
  })
  
  comments_list <- Filter(Negate(is.null), comments_list)
  
  if (length(comments_list) == 0) {
    return(data.frame())
  }
  
  comments_df <- bind_rows(comments_list)
  
  return(comments_df)
}

# lets see
# Create an empty list to hold comments
all_comments <- list()

# Loop over clean post IDs
for (i in seq_along(posts_df$id)) {
  post_id <- posts_df$id[i]
  cat("Getting comments for post:", post_id, "\n")
  
  # Pull comments
  comments <- get_reddit_comments(post_id)
  
  if (nrow(comments) > 0) {
    comments$post_id <- post_id  # Track which post the comments belong to
    all_comments[[length(all_comments) + 1]] <- comments
  }
  
  Sys.sleep(1)
}

# Combine into one big dataframe
comments_df_all <- bind_rows(all_comments)

# View the comments
head(comments_df_all)

## sort comments by date
comments_df_all <- comments_df_all %>%
  mutate(created_date = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"))

# Now sort by date
comments_df_all <- comments_df_all %>%
  arrange(created_date)  # ascending order (earliest first)
head(comments_df_all)


### making datasets for time periods
date1_start <- as.POSIXct("2022-07-01", tz = "UTC")
date1_end   <- as.POSIXct("2024-08-01", tz = "UTC")

date2_start <- as.POSIXct("2024-08-01", tz = "UTC")
date2_end   <- as.POSIXct("2025-01-01", tz = "UTC")

date3_start <- as.POSIXct("2025-01-01", tz = "UTC")
date3_end   <- Sys.time()  # Current time

# split
comments_period1 <- comments_df_all %>%
  filter(created_date >= date1_start & created_date < date1_end)

comments_period2 <- comments_df_all %>%
  filter(created_date >= date2_start & created_date < date2_end)

comments_period3 <- comments_df_all %>%
  filter(created_date >= date3_start & created_date <= date3_end)

library(dplyr)
library(stringr)

# cleaning
clean1 <- comments_period1 %>%
  mutate(text_clean = body %>%
           str_to_lower() %>%                         # lowercase
           str_replace_all("http\\S+", "") %>%         # remove links
           str_replace_all("u/\\w+", "") %>%           # remove Reddit usernames
           str_replace_all("[^a-z67\\s]", " ") %>%       # keep only letters and spaces
           str_replace_all("\\s+", " ") %>%            # collapse multiple spaces
           str_trim())                                # trim leading/trailing spaces

clean2 <- comments_period2 %>%
  mutate(text_clean = body %>%
           str_to_lower() %>%                         # lowercase
           str_replace_all("http\\S+", "") %>%         # remove links
           str_replace_all("u/\\w+", "") %>%           # remove Reddit usernames
           str_replace_all("[^a-z67\\s]", " ") %>%       # keep only letters and spaces
           str_replace_all("\\s+", " ") %>%            # collapse multiple spaces
           str_trim())   

clean3 <- comments_period3 %>%
  mutate(text_clean = body %>%
           str_to_lower() %>%                         # lowercase
           str_replace_all("http\\S+", "") %>%         # remove links
           str_replace_all("u/\\w+", "") %>%           # remove Reddit usernames
           str_replace_all("[^a-z67\\s]", " ") %>%       # keep only letters and spaces
           str_replace_all("\\s+", " ") %>%            # collapse multiple spaces
           str_trim())   

library(tm)
library(wordcloud)
library(RColorBrewer)

# stopwords and extras plus words from contractions that lost their apostrophe
stop_words <- stopwords('en')
extra_words <- c("don", "thing", "isn", "will", "said", 
                 "doesn", "just", "get")
stop_words <- c(stop_words, extra_words)

### first period
words_list1 <- str_split(clean1$text_clean, pattern = "\\s+")
words_flat1 <- unlist(words_list1)
words_flat1 <- words_flat1[!words_flat1 %in% stop_words]
wordcloud(
  words_flat1, 
  min.freq = 25,
  scale = c(3,1),
  max.words = 40,
  random.order = FALSE,
  colors = "#fe4f3a")


### period 2
words_list2 <- str_split(clean2$text_clean, pattern = "\\s+")
words_flat2 <- unlist(words_list2)
words_flat2 <- words_flat2[!words_flat2 %in% stop_words]
wordcloud(
  words_flat2, 
  min.freq = 25,
  scale = c(3,1),
  max.words = 40,
  random.order = FALSE,
  colors = "#fe4f3a")
### period 3
words_list3 <- str_split(clean3$text_clean, pattern = "\\s+")
words_flat3 <- unlist(words_list3)
words_flat3 <- words_flat3[!words_flat3 %in% stop_words]
wordcloud(
  words_flat3, 
  min.freq = 7,
  scale = c(3,1),
  max.words = 40,
  random.order = FALSE,
  colors = "#fe4f3a")

#######################################################################
##### NRC ########################################################
###################################################################

## sentiment analysis
#install.packages("syuzhet")
library(syuzhet)

# period 1 sentiment
nrc_period1 <- get_nrc_sentiment(words_flat1)
mean_emotions_period1 <- nrc_period1 %>%
  summarise(across(anger:positive, mean, na.rm = TRUE))

# period 2 sentiment
nrc_period2 <- get_nrc_sentiment(words_flat2)
mean_emotions_period2 <- nrc_period2 %>%
  summarise(across(anger:positive, mean, na.rm = TRUE))

# period 3 sentiment
nrc_period3 <- get_nrc_sentiment(words_flat3)
mean_emotions_period3 <- nrc_period3 %>%
  summarise(across(anger:positive, mean, na.rm = TRUE))

## visualize differences

## plot 1
emotion_summary <- bind_rows(
  mean_emotions_period1,
  mean_emotions_period2,
  mean_emotions_period3
) %>%
  mutate(period = c("July 2022–Aug 2024", "Aug 2024–Jan 2025", "Jan 2025–Now"))

emotion_summary$period <- factor(emotion_summary$period,
                                 levels = c("July 2022–Aug 2024", 
                                            "Aug 2024–Jan 2025", 
                                            "Jan 2025–Now"))
library(tidyr)
library(ggplot2)

# Reshape for ggplot
emotion_summary_long <- emotion_summary %>%
  pivot_longer(cols = anger:positive, names_to = "emotion", values_to = "mean_score")

# Plot 2
ggplot(emotion_summary_long, aes(x = period, y = mean_score, color = emotion, group = emotion)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Reddit Comment Emotion Scores Over Time",
       x = "Period",
       y = "Average Emotion Score") +
  theme_minimal()

#######################################################################
##### LLMs ########################################################
###################################################################

#### LLMs with openai

openai_api_key <- "YOUR API KEY HERE"

library(httr)
library(jsonlite)

# Function to classify stance using GPT
get_llm_stance <- function(comment_text) {
  prompt <- paste0(
    "You are analyzing Reddit comments about a proposed basketball arena in Philadelphia.\n",
    "Classify the following comment as either 'support', 'oppose', or 'neutral'. Only return one word: support, oppose, or neutral.\n\n",
    "Comment:\n", comment_text, "\n\nResponse:"
  )
  
  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization = paste("Bearer", openai_api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(role = "user", content = prompt)
      ),
      max_tokens = 10,
      temperature = 0
    ), auto_unbox = TRUE)
  )
  
  response_content <- httr::content(res, as = "parsed")  # <<< fixed here
  
  if (!is.null(response_content$choices) && length(response_content$choices) > 0) {
    return(response_content$choices[[1]]$message$content)
  } else {
    warning("No choices returned by the API.")
    return(NA)
  }
}

#testing should probably test first to not use too many requests

# Test on first 5 comments (small batch)
#test_comments <- clean1$text_clean[1:5]
# Run stance detection
#test_stances <- sapply(test_comments, get_llm_stance)
#print(test_stances)

##### PERIOD 1 COMMENTS ##############################
set.seed(42)  # So it's reproducible
sampled_comments <- clean1 %>% sample_n(50)

stances <- vector("character", length = length(sampled_comments$text_clean))

for (i in seq_along(sampled_comments$text_clean)) {
  stances[i] <- get_llm_stance(sampled_comments$text_clean[i])
  
  cat("Classified comment", i, "/", length(sampled_comments$text_clean), "\n")
  
  Sys.sleep(21)  # Wait 21 seconds per comment to stay within OpenAI limit
}

# Add stance back
sampled_comments$stance <- stances


##vis
write.csv(sampled_comments, "sampled_comments_with_stance1.csv", row.names = FALSE)

sampled_comments %>%
  group_by(stance) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

################ PERIOD 2 COMMENTS ##########################################
set.seed(42)  # So it's reproducible
sampled_comments2 <- clean2 %>% sample_n(50)

stances2 <- vector("character", length = length(sampled_comments2$text_clean))

for (i in seq_along(sampled_comments2$text_clean)) {
  stances[i] <- get_llm_stance(sampled_comments2$text_clean[i])
  
  cat("Classified comment", i, "/", length(sampled_comments2$text_clean), "\n")
  
  Sys.sleep(21)  # Wait 21 seconds per comment to stay within OpenAI limit
}
# Add stance back
sampled_comments2$stance <- stances

##vis
write.csv(sampled_comments2, "sampled_comments_with_stance2.csv", row.names = FALSE)

sampled_comments2 %>%
  group_by(stance) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


### PERIOD 3 COMMENTS ##########################################
## (comments are really all about one event)

set.seed(42)  # for reproducibility
sampled_comments3 <- clean3 %>% sample_n(50)

stances3 <- vector("character", length = length(sampled_comments3$text_clean))

for (i in seq_along(sampled_comments3$text_clean)) {
  stances3[i] <- get_llm_stance(sampled_comments3$text_clean[i])
  
  cat("Classified comment", i, "/", length(sampled_comments3$text_clean), "\n")
  
  Sys.sleep(21)  # Wait 21 seconds per comment to stay within OpenAI limit
}
sampled_comments3$stance <- stances3

write.csv(sampled_comments3, "sampled_comments_with_stance3.csv", row.names = FALSE)

### SENTIMENT FOR PERIOD 3

combined_text_period3 <- paste0("Comment ", seq_along(sampled_comments3$text_clean), ": ", 
                                sampled_comments3$text_clean, collapse = "\n\n")

## function for summary of topics in these comments
library(httr)
library(jsonlite)

get_period3_topics_sample <- function(text_blob) {
  prompt <- paste0(
    "You are analyzing 50 Reddit comments discussing the decision by the Sixers to stay in South Philadelphia instead of building a new arena in Center City after city council approval.\n",
    "Each comment is labeled 'Comment 1', 'Comment 2', etc.\n\n",
    "Please extract 5 to 10 common topics or concerns that appear across these comments. List the topics as simple keywords or short phrases, separated by commas.\n\n",
    "Comments:\n", text_blob, "\n\n",
    "Topics:"
  )
  
  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization = paste("Bearer", openai_api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(role = "user", content = prompt)
      ),
      max_tokens = 500,
      temperature = 0
    ), auto_unbox = TRUE)
  )
  
  response_content <- httr::content(res, as = "parsed")
  
  if (!is.null(response_content$choices) && length(response_content$choices) > 0) {
    return(response_content$choices[[1]]$message$content)
  } else {
    warning("No choices returned by the API.")
    return(NA)
  }
}


# get summary topics of comments for period 3
period3_sample_topics <- get_period3_topics_sample(combined_text_period3)

print(period3_sample_topics)
writeLines(period3_sample_topics, "period3_topics_output.txt")


## more specific -- are people angry and at who are they angry if so
## function for summary of topics in these comments
get_period3_sentiment_sample <- function(text_blob) {
  prompt <- paste0(
    "You are analyzing 50 Reddit comments discussing the decision by the Sixers to stay in South Philadelphia instead of building a new arena in Center City after city council approval.\n",
    "Each comment is labeled 'Comment 1', 'Comment 2', etc.\n\n",
    "Specifically, please:\n",
    "- Identify any major targets of anger, frustration, or blame expressed by Reddit users (e.g., Sixers ownership, city council, Mayor Cherelle Parker, Chinatown activists, developers, etc.)\n",
    "- Describe the overall sentiment of the comments (e.g., angry, relieved, betrayed, skeptical, disappointed)\n",
    "- List 5 to 10 common topics, concerns, or emotions appearing across the comments, separated by commas.\n\n",
    "Comments:\n", text_blob, "\n\n",
    "Analysis:"
  )
  
  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization = paste("Bearer", openai_api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(role = "user", content = prompt)
      ),
      max_tokens = 500,
      temperature = 0
    ), auto_unbox = TRUE)
  )
  
  response_content <- httr::content(res, as = "parsed")
  
  if (!is.null(response_content$choices) && length(response_content$choices) > 0) {
    return(response_content$choices[[1]]$message$content)
  } else {
    return(NA)
  }
}

### get broad sentiment and summary for period 3 comments
period3_sample_sentiment <- get_period3_sentiment_sample(combined_text_period3)

print(period3_sample_sentiment)
writeLines(period3_sample_sentiment, "period3_sentiment_output.txt")

###################################################################
################ VISUALIZE ###################################

library(dplyr)
library(ggplot2)
library(readr)

# Step 1: Read in each CSV
period1 <- read_csv("graphics/sampled_comments_with_stance1.csv") 
period2 <- read_csv("graphics/sampled_comments_with_stance2.csv")
period3 <- read_csv("sampled_comments_with_stance3.csv")

period1 <- period1 %>% mutate(created_date = as.character(created_date))
period2 <- period2 %>% mutate(created_date = as.character(created_date))
period3 <- period3 %>% mutate(created_date = as.character(created_date))

# Step 2: Add a column to label the period
period1 <- period1 %>% mutate(period = "July 2022 - Aug 2024")
period2 <- period2 %>% mutate(period = "Aug 2024 - Jan 2025")
period3 <- period3 %>% mutate(period = "Jan 2025 - Now")


# Step 3: Bind all together
all_comments <- bind_rows(period1, period2, period3)

# Step 4: Create a summary matrix
stance_summary <- all_comments %>%
  group_by(period, stance) %>%
  summarise(count = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = stance, values_from = count, values_fill = 0)

# View the summary matrix
print(stance_summary)

stance_long <- all_comments %>%
  group_by(period, stance) %>%
  summarise(count = n(), .groups = 'drop')

stance_long <- stance_long %>%
  group_by(period) %>%
  mutate(
    total = sum(count),         # total number of comments per period
    percent = (count / total) * 100  # percent of comments in each stance per period
  )

stance_long$period <- factor(stance_long$period, levels = c("July 2022 - Aug 2024", "Aug 2024 - Jan 2025", "Jan 2025 - Now"))

ggplot(stance_long, aes(x = period, y = count, fill = stance)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),  # Label text: rounded percentage with %
    position = position_fill(vjust = 0.5),        # Position inside each fill area, vertically centered
    color = "white",                              # White text for good contrast
    size = 4                                      # Adjust size as needed
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "support" = "#61210F",   
    "oppose" = "#EA2B1F",   
    "neutral" = "#D3D3D3"
  )) +
  labs(
    title = "Stance Toward Arena Over Time",
    x = "Period",
    y = "Percentage of Comments",
    fill = "Stance"
  ) +
  theme_minimal(base_size = 14)+
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

## tables for presentation
library(dplyr)
library(knitr)
library(kableExtra)

# random selection
set.seed(123)  # for reproducibility
sample_comments <- clean3 %>%
  select(created_date, body) %>%   # select only date and raw comment text
  sample_n(8)                      # pick how many you want to display (e.g., 8)


sample_comments <- sample_comments %>%
  mutate(
    created_date = as.Date(created_date),  # format date nicely
    body = stringr::str_trunc(body, width = 120)  # truncate long comments nicely
  )
sample_comments <- sample_comments[-5, ]


sample_comments %>%
  kable("html", escape = TRUE, col.names = c("Date", "Comment")) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = F,
    position = "center",
    font_size = 14
  )

## doing same thing for assigning comments a sentiment
set.seed(123)  # for reproducibility
sample_sentis <- period1 %>%
  select(created_date, body, stance) %>%   # select only date and raw comment text
  sample_n(8)                      # pick how many you want to display (e.g., 8)


sample_sentis <- sample_sentis %>%
  mutate(
    created_date = as.Date(created_date), 
    body = stringr::str_trunc(body, width = 250)
    # format date nicely
  )
sample_sentis <- sample_sentis[-8, ]


sample_sentis %>%
  kable("html", escape = TRUE, col.names = c("Date", "Comment", "Stance")) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = F,
    position = "center",
    font_size = 14
  )

## node mapping for topics
install.packages('igraph')
install.packages('ggraph')
library(igraph)
library(ggraph)

nodes <- data.frame(
  name = c(
    "Sixers ownership", "City Council", "Mayor Parker", "Chinatown activists", "developers",    # Blame
    "skeptical", "disappointed", "relieved", "angry",                                           # Sentiment
    "chinatown impact", "lack of need for arena", "financial motivations", "Market East development", "frustration with city planning process", "tax dollars concern", "activist criticism", "project collapse confusion", "desire for revitalization"
  , "criticism of city government"),
  group = c(
    rep("Blame", 5),
    rep("Sentiment", 4),
    rep("Topic", 10)
  )
)

nodes <- nodes %>%
  mutate(
    x = case_when(
      group == "Blame" ~ -1,
      group == "Sentiment" ~ 0,
      group == "Topic" ~ 1
    ),
    y = runif(n(), -2, 2))  # Random Y position for some vertical variation
 
#connections
edges <- data.frame(
  from = c(
    "Sixers ownership", "City Council", "Mayor Parker", "Developers", "Chinatown activists", 
    "Market East development", "City planning process", "City Council", "Mayor Parker"
  ),
  to = c(
    "Financial motivations", "City planning process", "Tax dollars concern", "Market East development", 
    "Activist criticism", "Desire for revitalization", "Chinatown impact", "lack of need for arena", "financial motivations"
  )
)

g <- graph_from_data_frame(d = edges, vertices = nodes)

ggraph(g, layout = "manual", x = nodes$x, y = nodes$y) +
  geom_edge_link(color = "grey70", alpha = 0.7) +
  geom_node_point(aes(color = group), size = 6) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_color_manual(values = c(
    "Blame" = "#8B0000",
    "Sentiment" = "#808080",
    "Topic" = "#DC143C"
  )) +
  theme_void() +
  labs(title = "Period 3: Public Sentiment, Blame, and Topics")