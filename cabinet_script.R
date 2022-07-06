library(tidyverse)
library(data.table)
library(mnis)
library(pdpr)

#Get the list of all government roles from 1950.
all_government_roles <- pdpr::fetch_mps_government_roles(from_date = "1950-06-10", to_date = "2022-07-05", while_mp = TRUE)

cabinet_roles <- all_government_roles %>%
  filter(grepl("Home Secretary",position_name) |
           grepl("Secretary of State",position_name) |
           grepl("Chancellor of the Exchequer", position_name) |
           grepl("Foreign Secretary", position_name) |
           grepl("Attorney General", position_name) |
           grepl("Prime Minister, First Lord of the Treasury", position_name)
  ) %>%
  filter(!grepl("Attorney General's Office",position_name))

#Get list of PMs.
pms <- all_government_roles %>% filter(grepl("Prime Minister, First Lord of the Treasury",position_name)) %>%
  select(display_name,mnis_id,government_incumbency_start_date,government_incumbency_end_date) %>%
  arrange(government_incumbency_start_date) %>%
  add_column(cabinet_date = c("1964-10-22","1970-06-29","1974-05-10","1976-04-14","1979-05-16","1990-12-02","1997-07-22","2007-07-02","2010-06-29","2016-07-18","2019-07-29")) %>%
  mutate(id = row_number(),
         government_incumbency_end_date = fifelse(is.na(government_incumbency_end_date),Sys.Date(),government_incumbency_end_date),
         count = 1) %>%
  group_by(display_name) %>%
  mutate(count = cumsum(count),
         display_name = paste(display_name,count, sep = " "))

final_pm <- data.frame()
for (i in pms$id) {
  
  start_filter <- pms %>% filter(id == i) %>% pull(cabinet_date)
  end_filter <- pms %>% filter(id == i) %>% pull(government_incumbency_end_date)
  pm <- pms %>% filter(id == i) %>% pull(display_name)
  count <- cabinet_roles %>%
    mutate(government_incumbency_end_date = fifelse(is.na(government_incumbency_end_date),Sys.Date(),government_incumbency_end_date)) %>%
    filter(government_incumbency_start_date > start_filter & government_incumbency_end_date <= end_filter) %>%
    mutate(pm = pm)
  
  final_pm <- bind_rows(final_pm,count)
  
}

pm_mini <- pms %>% select(display_name,government_incumbency_start_date,government_incumbency_end_date) %>% rename(pm = display_name, pm_start = government_incumbency_start_date, pm_end = government_incumbency_end_date )
pm_changes <- final_pm %>% select(pm,government_incumbency_start_date) %>% 
  left_join(.,pm_mini, by = c("pm")) %>%
  mutate(time = government_incumbency_start_date - pm_start,
         time = as.numeric(time /365.25,units = "days") * 12)

months <- seq(from = 0, to = 130, by = 1)
pm_changes_final <- data.frame()
distinct_pms <- pms %>% distinct(display_name)

for (i in months) {
  
  default <- data.frame(pm = c(distinct_pms), default = c(0,0,0,0,0,0,0,0,0,0,0)) %>% rename(pm = display_name)
  
  temp <- pm_changes %>% filter(time < i) %>% group_by(pm) %>% tally() 
  
  temp <- left_join(default,temp, by = c("pm")) %>% mutate(month = i,
                                                           n = ifelse(is.na(n),default,n)) %>% select(-c(default))
  
  pm_changes_final <- bind_rows(pm_changes_final,temp)
  
}

pm_mini <- pm_mini %>% mutate(duration = pm_end - pm_start,
                              duration = as.numeric(duration /365.25,units = "days") * 12)

pm_abridged <- pm_changes_final %>% distinct(pm) %>% arrange(pm) %>%
  add_column(short = c("Thatcher","Johnson","","Cameron","Brown","Major","Blair","May","","",""))

pm_changes_final<- left_join(pm_changes_final, pm_mini, by = ("pm")) %>%
  mutate(n = ifelse(month > duration, NA,n)) %>%
  left_join(.,pm_abridged, by = c("pm")) %>%
  filter(short != "") %>%
  filter(month < 35) %>%
  mutate(label = ifelse(month == 34, short, NA),
         label = ifelse(short == "Blair",NA,label),
         n = ifelse(pm == "Boris Johnson 1" & month >= 34,n+2,n)) #CHANGE THIS ONCE DATA HAS UPDATED 

blair_label <- data.frame(short = c("Blair"), n = c(8),label = c("Blair"), month = c(34))

p <- ggplot(data = pm_changes_final, aes(x = month, y = n, colour = short, label = label)) + 
  geom_line() +
  geom_label() +
  geom_label(data = blair_label ,aes(x = month, y = n, label = label, colour = "#F8766D")) +
  labs(title = " Everybody's fault but mine.",
       x = "Months since became Prime Minister",
       subtitle = " Number of new cabinet appointments, since the month they became Prime Minister.",
       caption = " \n  Produced by @DataLobo. Data sourced from UK Parliament's data platform. \n  For broad comparability across premireships, only appointments to  Secretary of State and Attorney General are included.") +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text = element_text(size=12),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="black" ),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.title = element_text(hjust = 0,size=12,face = "bold"),
    plot.caption = element_text(hjust = 0,size=9),
    plot.subtitle = element_text(hjust = 0,size=10),
    legend.position = "none")