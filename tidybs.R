# library(tidyr)
# library(readxl)
# library(dplyr)
# library(purrr)
# library(janitor)
# library(readr)
# library(ggplot2)
# library(ggthemes)
# library(scales)


list_of_packages <- c("tidyr", "readxl", "dplyr", "purrr", "janitor", "readr",
                      "ggplot2", "ggthemes", "scales")

new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list_of_packages, require, character.only = TRUE)


# Read in Data ------------------------------------------------------------

Lloyds_path <- "data-raw/lloyds.xlsx"
Natwest_path <- "data-raw/natwest.xlsx"


## Lloyds
## Consolidated Balance sheet is sheet named 37 and consolidated income statement is 35
## Not neccesary to read in all sheets but done for ease of use 
Lloyds <- Lloyds_path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = Lloyds_path)

Lloyds_BS <- Lloyds$`37`
Lloyds_BS2 <- Lloyds$`38`
Lloyds_IS <- Lloyds$`35`



## Natwest
## Natwest is labelled 1.1 and 1.2 is needed.   
Natwest <- Natwest_path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = Natwest_path) 

Natwest_BS <- Natwest$`1.2 - Balance Sheet`
Natwest_IS <- Natwest$`1.1 - Income Statement`



# Tidy tables -------------------------------------------------------------



####################### Lloyds########################

## Balance Sheet
Lloyds_BS_clean <- Lloyds_BS %>%
  janitor::remove_empty(c("cols", "rows")) %>% 
  janitor::row_to_names(2) %>% 
  rename(Item = 1) %>% 
  mutate(Type = case_when(is.na(`2019`) & is.na(`2020`) ~ Item)) %>% 
  fill(Type) %>% 
  slice(3:n()) %>% 
  pivot_longer(cols = `2019`:`2020`, names_to = "Year", values_to = "Value") %>% 
  mutate(Value = as.numeric(Value), 
         Entity = "Lloyds Banking Group")




## Lloyds BS is split over two tabs. Slightly diff format so cleaning again. 
Lloyds_BS2_clean <- Lloyds_BS2 %>% 
  janitor::remove_empty(c("cols", "rows")) %>% 
  janitor::row_to_names(2) %>% 
  select(Item = 1, 2, 4) %>% 
  mutate(Type = case_when(is.na(`2019`) & is.na(`2020`) ~ Item)) %>% 
  fill(Type) %>% 
  slice(4:n()) %>% 
  drop_na() %>% 
  pivot_longer(cols = `2019`:`2020`, names_to = "Year", values_to = "Value") %>% 
  mutate(Value = as.numeric(Value), 
         Entity = "Lloyds Banking Group")




## Combining the two tabs into one df
Lloyds_BS_final <- bind_rows(Lloyds_BS_clean, Lloyds_BS2_clean)

  


## Income Statement

Lloyds_IS_clean <- Lloyds_IS  %>% 
  janitor::remove_empty(c("rows", "cols")) %>% 
  janitor::row_to_names(2) %>% 
  select(Item = 1, `2020`, `2019`) %>% 
  slice(2:n()) %>% 
  mutate(across(`2019`:`2020`, readr::parse_number),
         Type = "Income",
         Entity = "Lloyds Banking Group") %>% 
  pivot_longer(cols = `2019`:`2020`, names_to = "Year", values_to = "Value")
  




####################### NATWEST########################

## Taking the 4th row with the years to duplicate the years where cells were merged in excel and replace
Natwest_BS[4,] <- Natwest_BS[4,] %>% 
  pivot_longer(cols = everything()) %>% fill(value) %>% pivot_wider(names_from = name, values_from = value)


Natwest_BS_clean <- Natwest_BS %>% 
  select(all_of(which(grepl("December", .), arr.ind = TRUE))) %>% 
  janitor::remove_empty(c("rows", "cols")) %>% 
  janitor::row_to_names(3) %>% 
  select(Item = 1, `2019`, `2020`) %>% 
  mutate(Type = case_when(is.na(`2019`) & is.na(`2020`) ~ Item)) %>% 
  fill(Type) %>% 
  filter(Type %in% c("Assets", "Liabilities", "Equity")) %>% 
  drop_na() %>% 
  pivot_longer(cols = `2019`:`2020`, names_to = "Year", values_to = "Value") %>% 
  mutate(Value = as.numeric(Value),
         Entity = "Natwest Group")



## Taking the 4th row with the years to duplicate the years where cells were merged in excel and replace
Natwest_IS[4,] <- Natwest_IS[4,] %>% 
  pivot_longer(cols = everything()) %>% fill(value) %>% pivot_wider(names_from = name, values_from = value)


Natwest_IS_clean <- Natwest_IS %>% 
  select(all_of(c(1, which(grepl("Full year", .), arr.ind = TRUE)))) %>% 
  janitor::remove_empty(c("rows", "cols")) %>%
  janitor::row_to_names(3) %>%
  select(Item = 1, `2019`, `2020`) %>% 
  drop_na() %>% 
  pivot_longer(cols = `2019`:`2020`, names_to = "Year", values_to = "Value") %>% 
  mutate(Type = "Income",
         Value = as.numeric(gsub("[^0-9]", "", Value)), 
         Entity = "Natwest Group")
  



consolidated_IS <- bind_rows(Natwest_IS_clean, Lloyds_IS_clean)
consolidated_BS <- bind_rows(Natwest_BS_clean, Lloyds_BS_final)
# Plots -------------------------------------------------------------------



total_income <- consolidated_IS %>% 
       filter(Item =="Total income") %>% 
       ggplot(aes(x = Year, y = Value, fill = Entity)) +
       geom_bar(stat = "identity",
                position = position_dodge2(preserve = "single"),
                width = 0.6) + 
       ggthemes::theme_clean() +
       facet_wrap(~Entity) + 
  theme(legend.position = "none",
        plot.background = element_blank()) +
  labs(title = "Total income",
       subtitle = "A tough year affecting both Banks",
       y = "£mns") +
  scale_y_continuous(label = comma)



Total_income_breakdown <- consolidated_IS %>% 
  filter(Item %in% c("Net interest income", "Total non-interest income",
                     "Other income")) %>%
  mutate(Item = gsub("Total non-interest income", "Other income", Item)) %>%
  ggplot(aes(x = Year, y = Value, fill = Item)) +
  geom_bar(stat = "identity",
           position = position_dodge2(),
           width = 0.6) + 
  ggthemes::theme_clean() +
  facet_wrap(~Entity) +
  theme(legend.position = "bottom",
        plot.background = element_blank()) +
  labs(title = "Total income breakdown",
       subtitle = "Lloyds suffer from large decrease to other income",
       y = "£mns") +
  scale_y_continuous(label = comma) +
  coord_flip()





impairment <- consolidated_IS %>% 
  filter(grepl("Impairment", Item)) %>% 
  mutate(Value = abs(Value)) %>% 
  ggplot(aes(x = Year, y = Value, fill = Entity)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"),
           width = 0.6) + 
  ggthemes::theme_clean() +
  facet_wrap(~Entity) + 
  theme(legend.position = "none",
        plot.background = element_blank()) +
  labs(title = "Impairment losses",
       subtitle = "Impairment affecting income",
       y = "£mns") +
  scale_y_continuous(label = comma)


shareholderProfit <- consolidated_IS %>% 
  filter(grepl("ordinary shareholders", Item, ignore.case = TRUE)) %>% 
  mutate(Value = abs(Value)) %>% 
  ggplot(aes(x = Year, y = Value, fill = Entity)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"),
           width = 0.6) + 
  ggthemes::theme_clean() +
  facet_wrap(~Entity) + 
  theme(legend.position = "none",
        plot.background = element_blank()) +
  labs(title = "Profit attributable to Ordinary Shareholders",
       subtitle = "Pockets have been hit hard over the year",
       y = "£mns") +
  scale_y_continuous(label = comma)

  

### Balance Sheet plots ###

BalanceSheetOverview <- consolidated_BS %>% 
  filter(Item %in% c("Total assets", "Total equity", "Total liabilities"),
         Type == "Assets") %>% 
  ggplot(aes(x = Year, y = Value, fill = Entity)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"),
           width = 0.6) + 
  ggthemes::theme_clean() +
  #facet_wrap(~Entity) + 
  theme(legend.position = "bottom",
        plot.background = element_blank()) +
  labs(title = "Total Assets",
       subtitle = "Natwest making ground",
       y = "£mns") +
  scale_y_continuous(label = comma)




deposits <- consolidated_BS %>% 
  filter(Item == "Customer deposits") %>%
  pivot_wider(names_from = Type, values_from = Value) %>% 
  group_by(Entity) %>% 
  summarise(change  = Liabilities / lag(Liabilities) -1) %>%  
  ggplot(aes(x = Entity, y = change, fill = Entity)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"),
           width = 0.6) + 
  ggthemes::theme_clean() +
  #facet_wrap(~Entity) + 
  theme(legend.position = "none",
        plot.background = element_blank()) +
  labs(title = "Customer Deposits",
       subtitle = "Natwest making ground",
       y = "% Change",
       x = element_blank()) +
  scale_y_continuous(label = percent) +
  coord_flip()

  
  


loans_customers <- consolidated_BS %>% 
  filter(grepl("loans.*customers*", Item, ignore.case = TRUE)) %>%
  pivot_wider(names_from = Type, values_from = Value) %>% 
  group_by(Entity) %>% 
  summarise(change  = Assets / lag(Assets) -1)  %>%
  ggplot(aes(x = Entity, y = change, fill = Entity)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"),
           width = 0.6) + 
  ggthemes::theme_clean() +
  #facet_wrap(~Entity) + 
  theme(legend.position = "none",
        plot.background = element_blank()) +
  labs(title = "Change in 'Loans to Customers'",
       subtitle = "Natwest significantly increase loans compared to previous year",
       y = "% Change",
       x = element_blank()) +
  scale_y_continuous(label = percent) + 
  coord_flip()
  
  

