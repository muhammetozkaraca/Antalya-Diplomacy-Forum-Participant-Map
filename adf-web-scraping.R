library(rvest)
library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(countrycode)
library(xlsx)
library(readxl)
library(maps)
library(showtext)
library(monochromeR)
library(paletteer)


url <- "https://antalyadf.org/en/participants-2022/"
html <- read_html(url)

participants <- html %>%
  html_nodes("h4") %>%
  html_text() %>%
  str_trim() %>%
  str_split(" ", n=14, simplify = TRUE) %>%
  as.data.frame()

participants$V14 <- str_trim(participants$V14)

participants <- participants %>%
  mutate(
    participants$name <- paste(participants$V1, participants$V2, participants$V3, participants$V4, participants$V5, participants$V6,
                               participants$V7, participants$V8, participants$V9, participants$V10, participants$V11, participants$V12,
                               participants$V13, sep = " ")
  ) %>%
  select(-c(V1:V13))

participants <- participants[, c(2, 1)]
colnames(participants)[1] <- "name"
colnames(participants)[2] <- "role"

country_name <- countrycode::countryname_dict %>%
  select(country.name.en) %>%
  distinct()

pat <- paste0("\\b", paste(country_name$country.name.en, collapse="\\b|\\b"), "\\b")
participants$nationality <- str_extract_all(participants$role, regex(pat, ignore_case = TRUE), simplify = TRUE)


# as some participants' roles do not signify his/her nationality, I had to find them through google search
# writexl::write_xlsx(participants, "participants.xlsx")

participants_updated <- read_excel("participants.xlsx") %>%
  group_by(nationality) %>%
  mutate(sum = n()) %>%
  select(nationality, sum) %>%
  distinct() 

participants_updated$nationality <- str_to_title(participants_updated$nationality)

participants_updated <- participants_updated %>% 
  mutate(nationality = case_when(nationality == "United Kingdom"~"UK",
                                 nationality == "United States"~"USA",
                                 nationality == "Bosnia And Herzegovina"~ "Bosnia and Herzegovina",
                                 nationality == "United States of America"~"USA",
                                 nationality == "North Macedonia"~"Macedonia",
                                 nationality == "Democratic Republic Of Congo"~"Democratic Republic of the Congo",
                                 nationality == "Republic Of The Congo"~"Republic of Congo",
                                 nationality == "Côte D'ivoire"~"Ivory Coast",
                                 TRUE ~ nationality))


world_countries <- map_data("world") %>%
  as.data.frame() %>%
  count(region) %>%
  select(-n) 

names(participants_updated)[1] <- "region"
pariticipants_origins <- participants_updated$region %>%
  as.data.frame()
names(pariticipants_origins)[1] <- "region"
setdiff(pariticipants_origins,world_countries)

map_world <- map_data("world")
merged <- left_join(map_world, participants_updated, by = c("region"))

font_add_google("Noto Serif Display", family = "title")
font_add_google("Playfair Display", family = "subtitle")
font_add_google("Libre Franklin", family = "caption")
showtext_auto()

merged$sum[is.na(merged$sum)] <- 0
quantile(participants_updated$sum)
barplot(participants_updated$sum)
arrange(participants_updated, desc(sum))


paletteer_d(`"nord::frost"`)
monochromeR::generate_palette("#2596BE",
                              modification = "go_darker", 
                              n_colours = 6, view_palette = T)
monochromeR::generate_palette("#E28743",
                              modification = "go_darker", 
                              n_colours = 6, view_palette = T)



monochromeR::generate_palette("#5E81ACFF", modification = "blend", 
                              blend_colour = "#81A1C1FF", 
                              n_colours = 5, view_palette = T)

plot <- ggplot(merged, aes(long, lat, group = group, fill = sum)) +
  geom_polygon () +
  ggthemes::theme_map() +
  labs(title = "Who attends the Antalya Diplomacy Forum in 2022?",
       subtitle = "The Antalya Diplomacy Forum is an elevated meeting occassion for experts in the field of diplomacy. This map visualizes\n this year's participants based on their country of origin.",
       caption = "Source: https://antalyadf.org/en/participants-2022/ | Plot: @muhammetozkrca") +
  scale_fill_gradientn(breaks=c(0,1,6,11,20), 
                       labels = c("0","1-5","6-10","11-20","20+"),
                       name= "Number of\nParticipant", 
                       colours = c("#DEE5EE", "#E28743", "#BD7138", "#995B2D", "#754622"),
                       guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(2, units = "mm"), 
                                           label.position = "bottom", title.position = 'top', nrow=6,
                                           title.hjust = .5)) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "title", size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "subtitle", size = 14),
    plot.caption = element_text(hjust = 0.5, family = "subtitle", size = 11),
    plot.background = element_rect(fill = "white", color = "white"),
    legend.box.background = element_rect(fill="white",color= "white"),
    legend.background = element_rect(fill="white",color="white"),
    legend.position = c(0.1, 0.2),
    legend.direction = "vertical"
  )


ggsave("map.png", height = 7, width = 11)


















