library(shinyjqui)
library(DT)
library(shiny)

source("github_api.R")

repo_df <- repo_info$repo_df
tag_df <- repo_info$tag_df

all_topics <- unique(tag_df$tag)
all_topics <- all_topics[order(all_topics)]

column_explanation <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th('Repository', title = 'Repository name'),
      th('Description', title = 'Description by the repo author'),
      th('Website', title = 'Link to the website (if applicable)'),
      th('Stargazers', title = 'Number of GitHub stars'),
      th('Days since last push', title = 'Measure for recent activity'),
      th('License', title = 'Type of license')
    )
  )
))

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Bioinformatics training material on GitHub"),
  fluidRow(
    column(12, 
           p(
             strong("Author:"),
             "Geert van Geest",
             a(href="mailto:geert.vangeest@sib.swiss",
               "geert.vangeest@sib.swiss")
           ),
           p(
             icon("console", lib = "glyphicon"),
             strong("Source code:"),
             a(href="https://github.com/sib-swiss/training-collection-app", 
               target="_blank",
               "sib-swiss/training-collection-app")
             ),
           linebreaks(1),
           orderInput('source_topics', 
                      'Choose from the following topics 
                      (or use the search functionality below):',
                      items = all_topics,
                      as_source = TRUE, connect = 'selected_topics',
                      width = '900px'),
           linebreaks(2),
           orderInput('selected_topics', 'Drag topics here:', 
                      items = NULL,
                      placeholder = 'Drag items here...')
    )
  ),
  hr(),
  
  fluidRow(
    column(12, 
           DTOutput('tbl')
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  repo_df_sel <- reactive({
    if(length(input$selected_topics) == 0){
      return(repo_df)
    }
    selected_slugs <- tag_df$slug[tag_df$tag %in% input$selected_topics]
    out_df <- repo_df[unique(selected_slugs),]
    out_df <- out_df[order(out_df$Stargazers, decreasing = TRUE),]
    return(out_df)
  })
  
  output$tbl = renderDT(
    datatable(repo_df_sel(), escape = FALSE, selection = "none", filter = "top",
              rownames = FALSE, options = list(pageLength = 25), 
              container = column_explanation
    ) %>% formatStyle(
      "Days since last push",
      color = styleInterval(c(30,90,365), c('green', 'lightgreen', 'orange', 'red')),
      fontWeight = 'bold'
    ) %>% formatStyle(
      "Repository",
      fontWeight = "bold")
  )
  
}

shinyApp(ui, server)
