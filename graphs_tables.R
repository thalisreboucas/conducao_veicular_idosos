library(tidyverse)
library(forcats)
library(lemon)
library(ggtext)


#data <- read.csv2(file.choose())

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title = element_blank(),
                                  axis.line.x = element_line(color = "#929497"),
                                  axis.ticks.x = element_blank(),
                                  axis.text.x = element_text(size = 12),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_text(color = "#828282", hjust = 1,size = 5,vjust = 0.8),
                                  plot.margin = unit(c(1,3,1,1), "cm"),
                                  plot.subtitle = element_text(hjust = 1, color = "#646369"),
                                  plot.caption = element_text(size = 8, color = "#929497", hjust = .5, margin = margin(15,0,0,0, "pt")),
                                  strip.text.y.left = element_text(size = 14, angle = 0,hjust=1)
))
#####
# 1 #
#####

# Tota of Publications By Year and Country

data <- df_complete %>% 
         select(-total_intervention_type,
         -total_country,-total_elder_population_type,
         -total_validator_occupation)

# Data frame 1

df <- data %>% 
          group_by(country,year) %>%
          summarise(total_publications = length(unique(id_publication))) %>% 
          mutate(total_publications = as.numeric(total_publications),year = as.numeric(year))


# graph of temporal series

#####
# 1 #
#####
  
aux <- df %>% mutate(name = country)


 ggplot(df,aes(x = year,y = total_publications)) + 
  geom_line(color="#929497", size=0.5, alpha=0.3,linetype = "dotdash", aes(group = country)) +
  geom_line(data = aux %>% dplyr::select(-country), aes(group=name), color="#646369", size=1.2) +
  scale_y_continuous(breaks = seq(0,6,2), limits = c(0,7),position = "right") + 
  scale_x_continuous(breaks = c(1991,1995,1999,2003,2007,2011,2015,2019,2023),limits = c(1991,2023),position = "top") +
  facet_grid(rows = vars(name),switch = "y") +
  coord_cartesian(clip = "off") + 
  labs(title = "Number of publications per year and country",
        subtitle = "" ,
         caption = )


 #####
 # 2 #
 #####
 
# Bar graph
 
 theme_set(theme_minimal() + theme(
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   axis.line.x = element_line(color = "#929497"),
   axis.title.y = element_blank(),
   plot.title = element_markdown(lineheight = 1.2),
   plot.subtitle = element_markdown(lineheight = 1.5, size = 10),
   axis.text.x = element_text(size = 9, color = "#929497"),
   axis.text.y = element_text(color = "#929497", size = 10),
   axis.title.x = element_text(color = "#929497", size = 10, hjust = 0.05),  
   axis.line.y = element_blank(),
   axis.ticks.y = element_blank(),
   strip.placement = "outside",
   strip.background = element_rect(fill = NA, color = "#929497")
 ))
 
 # Data frame 2

df1 <- df_author_occupation %>% group_by(author_occupation) %>%  
          summarise(Total = sum(total_author_occupation))

df1$type = "author"

df2<- df_validator_occupation %>% group_by(validator_occupation) %>%  
summarise(Total = sum(total_validator_occupation))

df2$type = "valitador"

df2 <- bind_rows(df1,df2) 

df2 <- df2 %>% mutate(Total = as.numeric(Total),
                      fill = case_when(type == "author" ~ "#111111" ,
                                       type == "valitador" ~"#848884" ))


ggplot(df2,aes(x = reorder(author_occupation ,-Total), y = Total ,fill = type,group = type),width = 10) +
   geom_bar(stat='identity', position = position_dodge2(preserve = "single"),size = 10) +
   scale_fill_manual(values = c("#111111","#848884"),labels = c("Author","Validator")) +
   scale_x_discrete() +
   scale_y_continuous(position = "right") +
   coord_capped_flip(top = waiver()) +
   labs(
     caption = "Data source: ..., Have orther's 57 articles without applications ",
     y = "Number total of profissional",
     title = "**Comparation**",
     subtitle = " of valitador and author of article "
   )
 

##################### Tables 

elder_table <- data %>% 
     dplyr::group_by(elder_population_type,country) %>%  
     dplyr::summarise(total_publications = length(unique(id_publication))) %>%  
     tidyr::pivot_wider(names_from = country , values_from = total_publications) 
  
 
intervation_table <- data %>% 
    dplyr::group_by(intervention_type,country) %>%
    dplyr::summarise(total_publications = length(unique(id_publication))) %>% 
    tidyr::pivot_wider(names_from = country , values_from = total_publications) 


bind_rows(elder_table,intervation_table) %>%
   mutate_if(is.numeric, ~replace_na(., 0)) %>%
   mutate_if(is.character, ~replace_na(., "-")) %>% knitr::kable()
