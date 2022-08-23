library(httr)
library(stringr)
# 
# system(paste0(". ", getwd(), "/.env"))
# 
# write(Sys.getenv("GITHUB_USER"), stderr())

source(".env.R")

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

print.github_api <- function(x, ...) {
  cat("<GitHub ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

linebreaks <- function(n){HTML(strrep(br(), n))}


empty_if_null <- function(x){
  if(!is.null(x)){
    return(x)
  } 
  return("")
}

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

slug_to_link <- function(slug){
  link <- paste0("https://github.com/", slug)
  html <- sprintf('<a href="%s" target="_blank">%s</a>',
                  link, slug)
  return(html)
}

format_stargazers <- function(stargazers_count){
  sort_string <- formatC(stargazers_count, width = 12,
                         format = "d", flag = "0")
  star_icon <- icon("star", lib = "glyphicon", style="color:#f1c40f")
  stargazers_string <- paste("<!--", sort_string, "-->", as.character(star_icon),
                             stargazers_count)
  return(stargazers_string)
}

main <- function(){
  age <- 0
  
  if(file.exists("repo_info.rds")){
    info <- file.info("repo_info.rds")
    age <- as.numeric(Sys.time() - info$mtime, 
                      units = "hours")
  }
  
  if(age > 1 | !file.exists("repo_info.rds")){
    
    repo_slugs <- read.delim("gh_slugs_tags.txt", header = FALSE)
    colnames(repo_slugs) <- c("slug", "tags")
    repo_slugs <- repo_slugs[!duplicated(repo_slugs$slug),]
    rownames(repo_slugs) <- repo_slugs$slug
    
    tag_list <- list()
    for(slug in rownames(repo_slugs)){
      tags <- strsplit(repo_slugs[slug, "tags"], ",")[[1]]
      tags <- str_trim(tags)
      tag_list[[slug]] <- data.frame(tag = tags, slug = slug)
    }
    
    tag_df <- do.call(rbind, tag_list)
    
    responses <- lapply(repo_slugs$slug, function(repo_slug){
      github_api(paste0("/repos/", repo_slug), 
                 authenticate(user = GITHUB_USER,
                              password = GITHUB_PAT))
    }) 
    
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
    
    
    repo_info <- list(repo_df = repo_df, tag_df = tag_df)
    saveRDS(repo_info, "repo_info.rds")
  }
  
  repo_info <- readRDS("repo_info.rds")
  return(repo_info)
}

repo_info <- main()

