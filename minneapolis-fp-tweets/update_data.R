
library(lubridate)

source("search.R")

update.df <- function(df){
  max.id <- df$status.id[df$epoch==max(df$epoch)]
  new.df <- twitter_search("food poisoning", count=100,
                       geocode="44.96,-93.2117,11mi", since_id=max.id)
  if(nrow(new.df) > 0){
    new.df <- add.cols(new.df)
  }else{
    new.df <- df
  }
  return(new.df)
}

add.cols <- function(df){
  df$text <- iconv(df$text, "WINDOWS-1252", "UTF-8")
  df$created.at2 <- gsub("\\+0000 ", "", df$created.at)
  df$created.at2 <- parse_date_time(substring(df$created.at2, 5,
                      nchar(df$created.at2)), "%b %d %H:%M:%S %Y")
  df$epoch <- as.numeric(seconds(df$created.at2))
  df$is.rt <- grepl("^RT| RT @", df$text)
  fp.url <- "http://174.129.49.183/cgi-bin/R/fp_classifier?text="
  df$classification <- sapply(df$text,
                         function(x)getURI(paste0(fp.url,
                           curlPercentEncode(x))))
  df$classification <- ifelse(df$classification=="food poisoning tweet\n",
                              "Good", "Junk")
  return(df)
}

if(file.exists("minneapolis_fp.Rdata")){
  load("minneapolis_fp.Rdata")
  df <- update.df(df)
}else{
  df <- twitter_search("food poisoning", count=100,
                       geocode="44.96,-93.2117,11mi")
  df <- add.cols(df)

}

save(list=c("df"), file="minneapolis_fp.Rdata")
