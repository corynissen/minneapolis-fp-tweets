
library(lubridate)
library(reshape2)

source("search.R")

update.df <- function(df){
  df <- dcast(df, status.id ~ variable, value.var="value")
  df$epoch <- as.numeric(df$epoch)
  max.id <- df$status.id[df$epoch==max(df$epoch)]
  df <- melt(df, id.vars="status.id")
  new.df <- twitter_search("food poisoning", count=100,
                       geocode="44.96,-93.2117,11mi", since_id=max.id)
  if(nrow(new.df) > 0){
    new.df <- add.cols(new.df)
    new.df <- data.frame(rbind(new.df, df))
  }else{
    new.df <- df
  }
  return(new.df)
}

add.cols <- function(df){
  df <- dcast(df, status.id ~ variable, value.var="value")
  df$text <- as.character(iconv(df$text, "WINDOWS-1252", "UTF-8", ""))
  df$created.at2 <- as.character(gsub("\\+0000 ", "", df$created.at))
  df$created.at2 <- parse_date_time(substring(df$created.at2, 5,
                      nchar(df$created.at2)), "%b %d %H:%M:%S %Y")
  df$epoch <- as.character(as.numeric(seconds(df$created.at2)))
  df$is.rt <- as.character(grepl("^RT| RT @", df$text))
  fp.url <- "http://174.129.49.183/cgi-bin/R/fp_classifier?text="
  df$classification <- sapply(df$text,
                         function(x)getURI(paste0(fp.url,
                           curlPercentEncode(iconv(x, "", "ASCII", "")))))
  df$classification <- as.character(ifelse(df$classification=="food poisoning tweet\n",
                              "Good", "Junk"))
  df$status.link <- paste0('<a href="https://twitter.com/', df$user.screen.name,
                           '/status/', df$status.id, '" target="_blank">View on Twitter</a>')
  df$created.at2 <- as.character(df$created.at2)
  df <- melt(df, id.vars="status.id")
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
