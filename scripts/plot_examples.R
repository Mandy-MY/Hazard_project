library(tidyverse) # load the tidyverse
library(readxl) # load the read Excel library as the data is in xlsx format
library(stringr)

# Below are some examples for how the data may be displayed.
# I think I will need to summarise the data before plotting 
# eg using this code as an example to print teh values to the console 
hazard_locations %>% 
  filter(Site == "Clayton") %>% 
  group_by(hazard_category) %>%  # groups by hazrd category
  summarise(num_rows = n())

#the function geom_bar will summarise data without needing to do this in the code itself

ggplot(hazard_locations %>% 
         filter(Site == "Clayton"), aes(hazard_category, fill = Further_Actions)) + 
  geom_bar() +
  labs(title = "Hazards reported at Clayton",
       x = "Hazard Category",
       y = "Count",
       fill = "Follow up actions indicated") # was not intuitive that the fill is where I needed to change the legend title but there you go, something new learned :-)


ggplot(hazard_locations %>% 
         filter(Site == "Black Mountain"), aes(hazard_category, fill = Further_Actions)) + 
  geom_bar()

ggplot(hazard_locations %>% 
         filter(Site == "Clayton", hazard_category == "Work_Env"), aes(hazard_category, fill = Further_Actions)) + 
  geom_bar() +
  facet_wrap(~hazard_subgroup) +
  labs(title = "Work Environment Hazards reported at Clayton",
       x = "Hazard Sub-group",
       y = "Count",
       fill = "Follow up actions indicated") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(hazard_locations %>% 
         filter(Site == "Clayton"), aes(hazard_category, fill = Business_Unit)) + 
  geom_bar()+
  labs(title = "Hazards reported at Clayton",
       x = "Hazard Category",
       y = "Count",
       fill = "Business Unit of reporter")+
  theme(axis.text.x = element_text(angle = 45))

ggplot(hazard_locations %>% 
         filter(Site == "Black Mountain"), aes(hazard_category, fill = Business_Unit)) + 
  geom_bar()

ggplot(hazard_locations, aes(hazard_category, fill = State_Territory)) + 
  geom_bar()
