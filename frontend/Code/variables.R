#############################################################
################# Global Variable Creation ##################
#############################################################

### ----- initialise connection to SQLite database  
mydb <- dbConnect(SQLite(), "../Data/liaison.sqlite")

### ----- Specify Target Table
DSH_tablename <- 'liaison_data'

### ----- Check global variables in table
print("Checking global variables...")
Tabledata <- dbGetQuery(mydb, glue("PRAGMA table_info({DSH_tablename});"))
## collate all available category NLP tags
cat_cols <- Tabledata %>% select(COLUMN_NAME = name) %>% 
  filter(str_detect(COLUMN_NAME,"^cat_")) %>%
  mutate(COLUMN_NAME = str_replace(COLUMN_NAME,"cat_","")) %>%
  mutate(COLUMN_NAME = str_replace_all(COLUMN_NAME,"_"," "))
## collate all available industry NLP tags
ind_cols <- Tabledata %>% select(COLUMN_NAME = name) %>% 
  filter(str_detect(COLUMN_NAME,"^ind_")) %>%
  mutate(COLUMN_NAME = str_replace(COLUMN_NAME,"ind_","")) %>%
  mutate(COLUMN_NAME = str_replace_all(COLUMN_NAME,"_"," "))

### ----- Get summary tables
agg_data <- list()
agg_data$month <- get_summary_data(mydb, DSH_tablename, "month")
agg_data$quarter <- get_summary_data(mydb, DSH_tablename, "quarter")
agg_data$year <- get_summary_data(mydb, DSH_tablename, "year")

### ----- Set global filter lists
INDUSTRY_GROUPS <- c('Agriculture','Business Services','Construction','Household Services',
                     'Manufacturing','Mining','Tourism','Transport and Storage','Utilities','Wholesale and Retail Trade')
STATES <- c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')
CATEGORIES <- cat_cols$COLUMN_NAME
INDUSTRIES <- ind_cols$COLUMN_NAME
ANY <- 'ANY'

### ----- Create jquery filters
filter_list <- list(list(name = 'IndustryGroupName', type = 'string', input = 'selectize', values = INDUSTRY_GROUPS),
                    list(name = 'State', type = 'string', input = 'selectize',values=STATES),
                    list(name = 'ContactDate', type = 'date', mask = 'yyyy-mm-dd'),
                    list(name = "text",type="string"),
                    list(name = "last_heading",type="string"),
                    list(name = "style",type="string"),
                    list(name = "CompanyName",type="string"))

topics <- Tabledata %>% select(COLUMN_NAME = name) %>% filter(str_detect(COLUMN_NAME,"^cat_|^ind_|sentiment_score|PricesExtract"))
for (i in topics$COLUMN_NAME) {
  filter_list <- filter_builder(filter_list,i)
}

### ---- Import word2vec model
print("Importing Word2Vec model...")
w2v_mod <- get_w2v("Code/w2v.bin")

