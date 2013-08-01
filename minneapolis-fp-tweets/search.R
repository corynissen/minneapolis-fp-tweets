
library(stringr)
library(RCurl)
library(RJSONIO)
library(digest)
 
# Download the certificate needed for authentication
if (!file.exists('cacert.perm')){
    download.file(url = 'http://curl.haxx.se/ca/cacert.pem',
        destfile='cacert.perm')
}
 
creds <- read.table(".chireply_twitter_creds", stringsAsFactors=F)

twitCred = list(consumerKey = creds[1,2],
                consumerSecret = creds[2,2],
                oauthKey = creds[3,2],
                oauthSecret = creds[4,2])
 
# Set curl options
curl = getCurlHandle()
options(RCurlOptions = list(capath = system.file('CurlSSL', 'cacert.pem',
                                package = 'RCurl'), ssl.verifypeer = FALSE))
curlSetOpt(.opts = list(proxy = 'proxyserver:port'), curl = curl)
 
twitter_search = function(term, count = 100, geocode="41.8607,-87.6408,16mi",
    result_type="recent", since_id = "", max_id = ""){
  # geocode cannot have spaces. Format is 'lat,lng,(num)mi'
  # max_id can be found in output$search_meta$next_results
  term <- curlPercentEncode(term)
  # Search term
  query <- paste0('&q=', term)
  result_type_string <- paste0('&result_type=', result_type)
  if(since_id==""){
    since <- ""
  }else{
    since <- paste0('&since_id=', since_id)
  }
  urlParams <- paste0('count=', count, '&geocode=', curlPercentEncode(geocode),
                      '&lang=en', max_id)
  url <- 'https://api.twitter.com/1.1/search/tweets.json'
  uriExtra <- paste('q=', term, '&', urlParams, result_type_string, since,
                    sep = '') 
  
  # Get the base string
  httpMethod <- 'GET'
  baseString <- paste(curlPercentEncode(c(httpMethod, url, urlParams)),
      collapse = '&')
 
  # Get the param string
  cKey <- twitCred$consumerKey
  nonce <- paste(letters[runif(34,1,27)],collapse = "")
  signatureMethod <- 'HMAC-SHA1'
  timestamp <- as.integer(Sys.time())
  token <- twitCred$oauthKey
   
  paramString <- paste(
      '&oauth_consumer_key=',     cKey,
      '&oauth_nonce=',            nonce,
      '&oauth_signature_method=', signatureMethod,
      '&oauth_timestamp=',        timestamp,
      '&oauth_token=',            token,
      '&oauth_version=1.0',
      query,
      result_type_string,
      since,
      sep = '')             
  signatureBaseString <- paste0(baseString, curlPercentEncode(paramString))
 
  # Get the signing key
  signingKey <- paste(curlPercentEncode(twitCred$consumerSecret), '&',
      curlPercentEncode(twitCred$oauthSecret), sep = '')
  signature <- hmac(signingKey, signatureBaseString, algo = 'sha1',
                    serialize = FALSE, raw = TRUE)
  signature <- curlPercentEncode(base64(signature))
 
  # getURI inputs
  authHeader <- paste0(
      'Authorization: OAuth ', 
      'oauth_consumer_key="',     cKey,            '", ',
      'oauth_nonce="',            nonce,           '", ',
      'oauth_signature="',        signature,       '", ',
      'oauth_signature_method="', signatureMethod, '", ',
      'oauth_timestamp="',        timestamp,       '", ',
      'oauth_token="',            token,           '", ',
      'oauth_version="1.0"')
  .opts <- list(header = TRUE, httpauth = TRUE, verbose = TRUE,
      ssl.verifypeer = FALSE)
  uri <- paste0(url, '?', uriExtra)
 
  result <- getURI(uri, .opts = .opts, httpheader = authHeader)
  result <- fromJSON(str_match_all(result, '\\r\\n\\r\\n(\\{.+)')[[1]][1,2])

   df <- data.frame(user.name=sapply(sapply(result$statuses, "[", "user"),
                        "[[", "name"))
  df$user.id <- sapply(sapply(result$statuses, "[", "user"), "[[", "id_str")
  df$user.screen.name <- sapply(sapply(result$statuses, "[", "user"),
                                "[[", "screen_name")
  df$user.location <- sapply(sapply(result$statuses, "[", "user"),
                             "[[", "location")
  df$user.description <- sapply(sapply(result$statuses, "[", "user"),
                                "[[", "description")
  df$user.followers.count <- sapply(sapply(result$statuses, "[", "user"),
                                    "[[", "followers_count")
  df$user.friends.count <- sapply(sapply(result$statuses, "[", "user"),
                                  "[[", "friends.count")
 
  df$geo <- sapply(result$statuses, "[[", "geo")
  df$coordinates <- sapply(result$statuses, "[[", "coordinates")
  df$place <- sapply(result$statuses, "[[", "place")
  df$status.id <- sapply(result$statuses, "[[", "id_str")
  df$text <- sapply(result$statuses, "[[", "text")
  df$created.at <- sapply(result$statuses, "[[", "created_at")
  
  return(df)
}

#twitter_search("food poisoning")
