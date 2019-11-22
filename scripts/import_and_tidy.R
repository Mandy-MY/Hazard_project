library(tidyverse) # load the tidyverse
library(readxl) # load the read Excel library as the data is in xlsx format
library(stringr)

# might need to also run remotes::install_github("sparce/DSreport") to get the RMarkdown template for our Data School project, Resart the R session to access the template and install the packages 'gapminder' and 'kableEXTRA' when prompted

#read in the data but ignore the first 3 'hidden' cells
locations <- read_xlsx("data/All_Hazards_location_details.xlsx", range = cell_cols("D:M"))
categories_new <- read_xlsx("data/All_Hazards_all_new_categories.xlsx", range = cell_cols("D:DC"))
categories_old <- read_xlsx("data/All_Hazards_all_old_categories.xlsx", range = cell_cols("D:BB"))
actions <- read_xlsx("data/Action_from_issue.xlsx", range = cell_cols("D:S"))

# first task is to assess differences/commonality in the Hazard category data between the old and new categories
# Join the Hazard category data using full join

categories_all <- full_join(categories_old, categories_new, by = c("Issue ID"))

#rename columns to get rid of spaces as this will be an problem later on  -and for the record I must have found this script example somewhere and renamed it :-)

colnames(categories_all)[which(names(categories_all) == "Issue ID")] <- "Issue_ID"
colnames(categories_all)[which(names(categories_all) =="BU of Person Involved.x")] <- "Business_Unit"

colnames(locations)[which(names(locations) == "Issue ID")] <- "Issue_ID"
colnames(locations)[which(names(locations) =="BU of Person Involved")] <- "Business_Unit"
#add code to replace spaces and forward slash with underscores in reamining columns, note that forward slash needs to be identified as the character using the \\
colnames(locations) <- str_replace_all(colnames(locations), c(" " = "_", "\\/" = "_"))


colnames(actions)[which(names(actions) == "Issue ID")] <- "Issue_ID"
#add code to replace spaces and forward slash with underscores in reamining columns
colnames(actions) <- str_replace_all(colnames(actions), c(" " = "_", "\\/" = "_"))

# The categories_all data set is now very 'flat'. Ideally I'd like to have this so that the upper level hazard categories are a variable and the subcategories records within those. Challenge? Yes, I think so (ha!)
#gather and spread to the rescue (I hope); here goes...
#remember gather(dataset, where_to_store_column_names, where_to_store_values, what_to_gather, anything_to_ignore)
# I want to end up with columns Issue ID, BU, hazard category, hazard result (yes or no)

categories_long <- gather(categories_all, hazard_category,hazard_result,-Issue_ID,-Business_Unit)

#because in the first instance I am only interested in where a hazard has been identified (ie a 'yes') I'll create a dataframe for hazards present

categories_present <- filter(categories_long, hazard_result == "Yes")

# hidden in the Hazard category is the category and subcategory details, gonna try splitting that out....
#get rid of the Hazard Category string at the start of each category
#get rid of added .x and .y text created at join - categories will be the same and have the same meaning 
#remove the space in the category Work Env as we need spaces to separate the columns in the next step


categories_present_tidy <- categories_present %>%                                    #create a new tidy data frame
  mutate(hazard_category = str_remove(hazard_category, "Hazard Categories ")) %>%    #this is telling R to overwrite the hazard_categories column (ie create a column with the same name = overwrite)and look for and remove the string (ie text) Hazard Categories 
  mutate(hazard_category = str_remove(hazard_category, "\\.x")) %>%                  #this is telling R to overwrite the hazard_categories column and look for and remove the literal text string .x (that was added at the join step)
  mutate(hazard_category = str_remove(hazard_category, "\\.y")) %>%                  #this is telling R to overwrite the hazard_categories column and look for and remove the literal text string .x (that was added at the join step)
  mutate(hazard_category = str_replace(hazard_category, "Work Env\\.?", "Work_Env")) #this is telling R to overwrite the hazard_categories column and look for anything that has Work Env with a literal full stop (the \\ denotes an actual full stop as opposed to any character) and the ? inicates any character either side of that, that will allow for both version or Work Env with and without the full stop to be replaced (from the  old hazard categories and new categories) 


# now with the cleaned hazard category and sub group in a string, time to split the columns into category and subgroup, and write this to a file as we will need it later.
# the code will split the string at the first white space (which is why we replaced Work Env with Work_Env in the previous step) and then everything after that is the next column
Hazards_present_merged <- separate(categories_present_tidy, hazard_category, into = c("hazard_category", "hazard_subgroup"), sep = "\\s", extra = "merge")
write_csv(Hazards_present_merged, path = "data/processed_data/Hazards_present_merged.csv")

# and I probably should have done this earlier but as I intend to join the locations data to the merged hazard data so I'll write that to a csv file as well
write_csv(locations, path = "data/processed_data/locations.csv")

# and while I'm at it I may get around to the actions piece so let's create that file too
write_csv(actions, path = "data/processed_data/actions.csv")

# and so I have all the 'tidy' data I inteded I'm going to join merged hazards with locations and with actions
hazard_locations <- inner_join(Hazards_present_merged, locations, by = c("Issue_ID", "Business_Unit")) # ingnore the repeated BU information in the locations data
write_csv(hazard_locations, path = "data/processed_data/hazard_locations.csv")

hazard_actions <- inner_join(hazard_locations, actions, by = c("Issue_ID")) # incomplete code - but in the actins data the column name is Issue being addressed so need to fix this too

#Phew! That took longer than I thought but I'm getting it now :-)  time to start some example visualisations. I'm going to do some testing in a new script and then copy the ones I want to keep into this file

