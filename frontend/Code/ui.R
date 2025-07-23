####################################################################
# UI ---------------------------------------------------------------
####################################################################

## Set generic dashboard UI settings
options(spinner.color="#2c3e50", spinner.color.background="#ffffff", spinner.size=0.5)

##--- UI object
ui <- fluidPage(theme = shinytheme(choosetheme("Code/theme.txt")),
                rclipboardSetup(),
navbarPage("Liaison Dashboard",
           
#-------------------------------------------------------------------------------------------------------------
#---------------------------------------------------- Home ---------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
## Home page contains info on using the dashboard, this is generated in a bunch of Rmd files saved in the markdown folder
tabPanel("Home",
    fluidRow(column(width = 12, style = "background: linear-gradient(to right, #2c3e50 50%, #c6cfd7);padding: 20px 0px;margin-top:-2em",
  HTML('<p style="font-size:40pt; color: white;margin-left: 30px">Welcome to the Liaison Dashboard</p>'),
  HTML('<em style="color: white;margin-left: 50px">Note: This is an example version of the dashboard and only contains artifical liaison text data, generated using AI.</em>'),
  br(),br())),
  br(),
  fluidRow(column(width=9,offset=1,navlistPanel(
    id = "tabset",
    "Sections",
    tabPanel("Home", fluidRow(column(width=10,offset=1,includeMarkdown("Code/markdown/titlepage.Rmd")))),
    tabPanel("Search", fluidRow(column(width=10,offset=1,includeMarkdown("Code/markdown/Search.Rmd")))),
    tabPanel("Topic Builder", fluidRow(column(width=10,offset=1,includeMarkdown("Code/markdown/Topic_builder.Rmd")))),
    tabPanel("Understanding the Filters", fluidRow(column(width=10,offset=1,includeMarkdown("Code/markdown/Understanding_filters.Rmd")))),
  ,widths = c(2, 10))))
), ## tabPanel: Home



#-------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------- Simple Filtering ---------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------
## Main page with primary search sidepanel and datatable/plot outputs based on applied filters
tabPanel("Search",
    sidebarLayout(
      #### Sidepanel for filtering options --------
      sidebarPanel(
        br(),
        ##---- Press "Update Data" button to initiate SQL database search and table/plot generation
      actionButton("suppdate", "Update Data",class = "btn-primary btn-lg",width = "100%"),
      hr(),
      h3("Basic Filters"),
      ##---- Filters on ContactDate 
      dateRangeInput(
        inputId = "sup_daterange",
        label = "DateRange",
        start="2001-01-01",
        min="2001-01-01",
        startview = "decade"
      ),
      tags$style(HTML(".datepicker {z-index:99999 !important;}")),
      ##---- Filters State
      selectInput(
        inputId="sup_state",
        label = "State",
        multiple = TRUE,
        selected = "",
        choices = c(STATES)
      ),
      bsTooltip("sup_state", "Based on the Contacts State identified from REGI. This is at the liaison level."
                ,placement = "right",options = list(container="body")),
      ##---- Filters IndustryGroupName
      selectInput(
        inputId = 'sup_industrygrp',
        label = 'Company Industry Group',
        multiple = TRUE,
        selected = "",
        choices = c(INDUSTRY_GROUPS)
      ),
      bsTooltip("sup_industrygrp", "Based on the Contacts SIC industry code identified from REGI. This is at the liaison level."
                ,placement = "right",options = list(container="body")),
      ##---- Filters last_heading as a text search
      selectizeInput('heading_text', "Last Heading", choices = NULL, multiple = TRUE,
        options = list(create = TRUE,
                       persist = FALSE,
                       placeholder = 'Type here and press enter, not case sensitive')
      ),
      bsTooltip("heading_text", "Extracts all paragraphs following that heading (and before the next heading)."
                ,placement = "right",options = list(container="body")),
      ##---- TEXT SEARCH: filters "text" column
      selectizeInput("sup_text", "Paragraph Search", choices = NULL, multiple = TRUE,
                    options = list(create = TRUE,
                                   persist = FALSE,
                                   placeholder = 'Type here and press enter, not case sensitive')),
      ## generates similar word output
      textOutput("sup_suggested_phrases"),
      bsTooltip("sup_text", "Type word and press enter to include in word-search of paragraphs. Enter should be pressed between each word typed (unless searching a phrase)."
                  ,placement = "right",options = list(container="body")),
      ### LM FILTERS ###
      h3("Language Model Filters"),
      ##----  Filters selected CATEGORY columns that begin with "cat_" above a numerical threshold 
      selectInput(
        inputId = 'sup_category',
        label = "Category",
        selected = "",
        multiple = TRUE,
        choices = c(CATEGORIES)
      ),
      bsTooltip("sup_category", "Filter for paragraphs that discuss selected categories, detected by Zero-shot learning model."
                ,placement = "right",options = list(container="body")),#Model labels paragraphs based on expected topics of discussion in Notes."),
      ##---- Filters selected INDUSTRY columns that begin with "ind_" above a numerical threshold 
      selectInput(
        inputId = 'sup_industry',
        label = 'Industry',
        selected = "",
        multiple = TRUE,
        choices = c(INDUSTRIES)
      ),
      bsTooltip("sup_industry", "Filter for paragraphs that discuss selected industries, detected by Zero-shot learning model. May be different from Contacts industry."
                ,placement = "right",options = list(container="body")),
      ### Advanced Settings ###
      h3('Advanced Settings'),       
      ##---- Toggle on to only select paragraphs with non-null Price Extractions
      div(checkboxInput('price_extract',
                   label = "Filter for Price Extractions",
                   value = FALSE,
                   width = NULL), style = "font-weight:bold; font-size:16px"),
      bsTooltip("price_extract", "Filter for only paragraphs that contain valid price extractions."
               ,placement = "right",options = list(container="body")),
      ##---- Chooses time segment for any aggregated data 
      radioButtons(
           inputId = "aggr",
           label = "Aggregate Data by",
           choices = c('Year','Quarter',"Month"),
           selected = "Quarter"
         ),
      bsTooltip("aggr", "Change how to aggregate data in 'Outline of Filtered Data' and 'Overall Trend in Tags' graphs as well as the 'Aggregated Data' table."
                  ,placement = "right",options = list(container="body")),
      ##---- Numerical thresholds for any category tags selected, bigger number  = higher model Confidence = less paragraphs extracted
      numericInput(
          inputId = 'sup_cat_threshold',
          value = 0.9,
          label = 'Category Confidence',
          min = 0.1,
          max = 1,
          step = 0.01
        ),
      bsTooltip("sup_cat_threshold", "Set level of confidence the Zero-shot learning model must have that a paragraph is about any category selected above. A higher value indicates more confidence."
                  ,placement = "right",options = list(container="body")),
      ##---- Numerical thresholds for any industry tags selected, bigger number  = higher model Confidence = less paragraphs extracted
        numericInput(
          inputId = 'sup_industry_threshold',
          label = "Industry Confidence",
          value = 0.9,
          min = 0.1,
          max = 1,
          step = 0.01,
        ),
        bsTooltip("sup_industry_threshold", "Set level of confidence the Zero-shot learning model must have that a paragraph discusses any industry selected above. A higher value indicates more confidence."
                  ,placement = "right",options = list(container="body")),
      ##---- Toggle off = any word in "text" search, On = must contain all words in list
        checkboxInput('supinclusive', label= "Keyword Search using 'AND'", value = FALSE, width = NULL),
        bsTooltip("supinclusive", "If selected, paragraphs must contain all words in search list above; if not it only needs to contain at least one."
                  ,placement = "right",options = list(container="body")),
       width = 3
      ),


      ##### Search Output: plots, summary table, and raw tables --------
      mainPanel(width = 9,
                tabsetPanel(type = "tabs",
                            ## Summary Plots tab ("front" page) ---------
                            tabPanel("Summary",h3(strong("Overview of Filtered Data")), ## upper plot
                                     fluidRow(column(10, offset = 1, withSpinner(plotlyOutput("countplot",height="700px"),type=6)),style = "height:700px"
                                     ),
                                     br(),
                                     h3(strong("Individual Trends in Text Filters")), ## lower plot with mulitple tabs 
                                     fluidRow(tabBox(width=NULL,
                                                     tabPanel("Word Usage",fluidRow(column(10,offset = 1, withSpinner(plotlyOutput("wcplot",height="700px"),type=6))),style = "height:700px"),
                                                     tabPanel("Category",fluidRow(column(10,offset = 1,withSpinner(plotlyOutput("catplot",height="700px"),type=6))),style = "height:700px"),
                                                     tabPanel("Industry",fluidRow(column(10,offset = 1,withSpinner(plotlyOutput("indplot",height="700px"),type=6))),style = "height:700px")
                                     )
                                     )
                            ),
                            ## Table of Aggregated data
                            tabPanel("Aggregated Data",
                                     br(),
                                     fluidRow(
                                       column(width = 2,
                                              downloadButton("aggdownload", "Download to Excel", class = "btn-success"))),
                                     withSpinner(DTOutput("aggtable"),type=7)
                            ),
                            ## Table of Raw data at paragraph level (lowest level) ----------
                            tabPanel("Raw Data by Paragraph",
                                     br(),
                                     fluidRow(
                                       column(width = 2,
                                              downloadButton("supdownload", "Download to Excel", class = "btn-success"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(width = 12,withSpinner(DTOutput("suptable"),type=6)
                                       ))
                            ),
                            ## Table of Raw data at liaison level ----------
                            tabPanel("Raw Data by Liaison",
                                     br(),
                                     fluidRow(
                                       column(width = 2,
                                              downloadButton("suplsndownload", "Download to Excel", class = "btn-success"))
                                     ),
                                     br(),
                                     fluidRow(
                                       column(width = 12,withSpinner(DTOutput("suplsntable"),type=6)
                                       ))
                            ),
                            ## Table and Plot of all extracted Paragraphs with valid "PricesExtract" values
                            tabPanel("Price Extraction",
                                     br(),
                                     fluidRow(
                                       column(width = 6, br(),br() , h3(strong("Price Inflation")), withSpinner(plotlyOutput("peplot",height="700px"),type=6)),
                                       column(width = 6, br(), fluidRow(
                                                  column(width = 2,
                                                         downloadButton("price_ext_dl", "Download to Excel", class = "btn-success"))
                                                ),
                                                br(),
                                                fluidRow(
                                                  column(width = 12,withSpinner(DTOutput("price_ext_tbl"),type=6)
                                                  )))
                                       )
                            ),

          ) ## tabsetPanel
        ) ## mainPanel
      ) ## sidebarLayout
), ## tabPanel: Search


#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------ Topic Builder ----------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
tabPanel("Topic Builder",
        ## Upper panel that describes topic builder and has elements to build word list for topics
        fluidRow(column(4, h2("Welcome to Topic Builder"),
"Use the search bar to the right to build a list of words used in Liaison notes which can be developed into a manual topic search based around words selected. Suggested words are based on semantically similar words from Liaison Notes, and if multiple words have been selected, the resultant suggestions will be an 'average' of all words selected. You can remove words from the suggestions by entering it into the adjacent field. This will show the next closest word(s), but will not change the 'average' of the words selected. Once you have built a list of words, the word list output can be copied into the Search or Advanced search text field, or even into R, to search liaison data for your topic (based on the word list)."),
                    column(4, br(), br(), selectizeInput("phrases", "Create Topic Word List Here:", choices = NULL, multiple = TRUE,
                                   options = list(create = TRUE,
                                                  persist = FALSE,
                                                  placeholder = 'Type here and press enter to see similar words'),width="100%"),textOutput("suggested_phrases")),
                    column(2, br(), br(), selectizeInput("phrases_drop", "Remove Words From Suggestions Here:", choices = NULL, multiple = TRUE,
                                     options = list(create = TRUE,
                                                    persist = FALSE,
                                                    placeholder = 'Type here and press enter'),width="100%")),
                    # Word2Vec Recommendations
                    column(2, br(),br(),
                           uiOutput("clip"), br(),
                           uiOutput("clipR")
                           )),
        br(),br(),
        ## Lower panel for checking specific word usage examples in liaisons
        fluidRow(
             column(2, h4("Examples in Liaisons: "),
                        selectizeInput("tb_text", "Enter word below to see uses in liaisons", choices = NULL, multiple = TRUE,
                                      options = list(create = TRUE,
                                                     persist = FALSE,
                                                     placeholder = 'Type word and press enter')),
              actionButton("tbupdate", "See Examples",class = "btn-primary"),
              br(),
              textOutput("tb_warning"),
              br()),
              column(10, withSpinner(DTOutput("tbtable"),type=5)))
), ##tabPanel: Topic Builder 



 ) ## navbarPage
) ## fluidPage
