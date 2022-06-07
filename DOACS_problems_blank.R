
# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)

# Load Data ---------------------------------------------------------------

doacs_df <- read_csv("DOACS_data.csv")


# Reminder of dplyr verbs -------------------------------------------------

?select # picks columns
?filter # selects rows
?mutate # create new columns
?arrange # sort by column
?group_by # group by column
?summarise # summarise grouped data to fewer rows

# Problems ----------------------------------------------------------------


# 1 - Filter the data to get only prescribing of Rivaroxaban in NHS Manchester CCG (code = 14L) over the last 2 years
    
    # last two years from today
    doacs_df %>% 
        filter(date %within% interval((today() - years(2)), today()),
               ccg_ods == "14L",
               chemical == "Rivaroxaban")
    
    # 2020 and 2021
    doacs_df %>% 
        filter(date %within% interval(ymd("2020-01-01"), ymd("2021-12-31")),
               ccg_ods == "14L",
               chemical == "Rivaroxaban")
    
    doacs_df %>% 
        filter(date %within% interval(ymd("2020-01-01"), ymd("2021-12-31")),
               ccg_ods == "14L",
               chemical == "Rivaroxaban")
    
    doacs_df %>% 
        filter(year(date) %in% c(2020, 2021),
               ccg_ods == "14L",
               chemical == "Rivaroxaban")

# 2 - How many items of Apixaban were presribed by NHS Manchester CCG (code = 14L) in June 2021?
doacs_df %>% 
    filter(year(date) == 2021,
           month(date) == 6,
           chemical == "Apixaban",
           ccg_ods == "14L")

# 3 - How much did NHS Salford CCG (code = 01G) spend on Edoxaban in 2021?

doacs_df %>% 
    filter(date >= "2021-01-01",
           date <= "2021-12-01",
           ccg_ods == "01G",
           chemical == "Edoxaban") %>% 
    group_by(chemical) %>% 
    summarise(total_cost = sum(actual_cost))

doacs_df %>% 
    filter(year(date) == 2021,
           ccg_ods == "01G",
           chemical == "Edoxaban") %>% 
    group_by(chemical) %>% 
    summarise(total_cost = sum(actual_cost))


# 4 how many items per 1000 population of Rivaroxaban did NHS Salford CCG (code = 01G) prescribe in Dec 2021?
doacs_df %>% 
    filter(year(date) == 2021,
           month(date) == 12,
           ccg_ods == "01G",
           chemical == "Rivaroxaban") %>% 
    mutate(items_per1000 = items/total_list_size * 1000) %>% 
    select(date, chemical, ccg_name, items_per1000)

# 5 - Which medicine was most prescribed in England in 2021?
doacs_df %>% 
    filter(year(date) == 2021) %>% 
    group_by(chemical) %>% 
    summarise(total_items = sum(items)) %>% 
    arrange(desc(total_items))

# 6 - Which CCG prescribed the most items of Apixaban per 1000 population in 2020?
doacs_df %>% 
    filter(year(date) == 2020,
           chemical == "Apixaban") %>% 
    mutate(apixa_per1000 = items/total_list_size * 1000) %>% 
    group_by(ccg_name) %>% 
    summarise(apixa_2020 = mean(apixa_per1000)) %>% 
    arrange(desc(apixa_2020)) %>% 
    slice(1)

# 7 - Plot a bar chart showing annual prescribing of all 5 DOACs in 2021
doacs_df %>% 
    filter(year(date) == 2021) %>% 
    group_by(chemical) %>% 
    summarise(annual_items = sum(items)) %>% 
    ggplot(aes(x = chemical, y = annual_items)) +
    geom_col()

# 8 - Plot a line chart of Apixaban prescribing over the last two years
doacs_df %>% 
    filter(year(date) %in% c(2020, 2021),
           chemical == "Apixaban") %>% 
    # Want to calculate total apixaban items for each month so need to group by date
    group_by(date) %>% 
    summarise(total_apixa = sum(items)) %>% 
    ggplot(aes(x = date, y = total_apixa)) +
    geom_line()

doacs_df %>%
    filter(chemical == 'Apixaban') %>% 
    ggplot(aes(x=date, y=items)) +
    geom_point()

# 9 - Plot a line chart showing prescribing of all doacs over the last 2 years
doacs_df %>% 
    filter(year(date) %in% c(2020, 2021)) %>% 
    group_by(chemical, date) %>% 
    summarise(total = sum(items)) %>% 
    ggplot(aes(x = date, y = total, colour = chemical)) +
    geom_line()
