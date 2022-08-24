library(httr)
library(stringr)

source(".env.R")

# function to interact with github api
github_api <- function(path, config = list()) {
  url <- httr::modify_url("https://api.github.com", path = path)
  resp <- httr::GET(url, config = config)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )
}

# pretty printing an api response
print.github_api <- function(x, ...) {
  cat("<GitHub ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

# function for easy linebreaks in app
linebreaks <- function(n){HTML(strrep(br(), n))}

# return empty if null
empty_if_null <- function(x){
  if(!is.null(x)){
    return(x)
  } 
  return("")
}

# create link out of character if not NULL nor empty
to_link <- function(x){
  if(is.null(x)) {
    return("")
  }

  if(str_trim(x) == "") {
    return(x)
  }
  
  return(
    sprintf('<a href="%s" target="_blank" class="btn btn-primary">Website</a>',
            x)
  )
  
}

# convert slug to github link
slug_to_link <- function(slug){
  link <- paste0("https://github.com/", slug)
  html <- sprintf('<a href="%s" target="_blank">%s</a>',
                  link, slug)
  return(html)
}

# pretty format stargazers
format_stargazers <- function(stargazers_count){
  sort_string <- formatC(stargazers_count, width = 12,
                         format = "d", flag = "0")
  star_icon <- icon("star", lib = "glyphicon", style="color:#f1c40f")
  stargazers_string <- paste("<!--", sort_string, "-->", 
                             as.character(star_icon),
                             stargazers_count)
  return(stargazers_string)
}

# convert list of responses into dataframe
get_repo_info_df <- function(responses){
  
  df_list <- list()
  for(response in responses){
    repo_record <- response$content
    timediff_latest_push <- Sys.time() - as.POSIXct(repo_record$pushed_at)
    days_since_latest_push <- round(as.numeric(timediff_latest_push, 
                                               units = "days"))
    df <- data.frame(
      Repository = slug_to_link(repo_record$full_name),
      Description = empty_if_null(repo_record$description),
      Website = to_link(repo_record$homepage),
      Stargazers = format_stargazers(repo_record$stargazers_count),
      Days_since_last_push = days_since_latest_push,
      License = empty_if_null(repo_record$license$key)
    )
    df_list[[repo_record$full_name]] <- df
  }
  
  repo_df <- do.call(rbind, df_list)
  repo_df <- repo_df[order(repo_df$Stargazers, decreasing = TRUE),]
  colnames(repo_df) <- gsub("_", " ", colnames(repo_df))
  return(repo_df)
}

# 1:1 mapping of slugs-tags
get_tag_df <- function(slugs_tags_df){
  tag_list <- list()
  for(slug in rownames(slugs_tags_df)){
    tags <- strsplit(slugs_tags_df[slug, "tags"], ",")[[1]]
    tags <- str_trim(tags)
    tag_list[[slug]] <- data.frame(tag = tags, slug = slug)
  }
  
  tag_df <- do.call(rbind, tag_list)
  return(tag_df)
}

main <- function(){
  age <- 0
  
  if(file.exists("repo_info.rds")){
    info <- file.info("repo_info.rds")
    age <- as.numeric(Sys.time() - info$mtime, 
                      units = "hours")
  }
  
  # only recreate if repo_info.rds is older than 1 day
  if(age > 24 | !file.exists("repo_info.rds")){
    
    # read in tab delimited file with slugs and tags
    slugs_tags_df <- read.delim("gh_slugs_tags.txt", header = FALSE)
    colnames(slugs_tags_df) <- c("slug", "tags")
    slugs_tags_df <- slugs_tags_df[!duplicated(slugs_tags_df$slug),]
    rownames(slugs_tags_df) <- slugs_tags_df$slug
    
    # get api resonses of repo info
    responses <- lapply(slugs_tags_df$slug, function(repo_slug){
      github_api(paste0("/repos/", repo_slug), 
                 authenticate(user = GITHUB_USER,
                              password = GITHUB_PAT))
    }) 
    
    # convert responses into dataframe with records of interest
    repo_df <- get_repo_info_df(responses)
    
    # convert tags into 1:1 tag - slug mapping
    tag_df <- get_tag_df(slugs_tags_df)
    
    # return repo info and tag 
    repo_info <- list(repo_df = repo_df, tag_df = tag_df)
    saveRDS(repo_info, "repo_info.rds")
  }
  
  # read file from disk
  repo_info <- readRDS("repo_info.rds")
  return(repo_info)
}

repo_info <- main()

