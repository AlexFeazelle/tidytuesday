library(tidyverse)
library(ggplot2)
library(ggstar)
library(extrafont)





#------Loading Data and Fonts------

historical_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')
gifts_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_age.csv')
gifts_gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_gender.csv')

font_import()
loadfonts()
extrafont::choose_font()
fonts()

#------Cleaning Data--------

gift_age_new <- gifts_age |> 
  pivot_longer(cols = c("SpendingCelebrating","Candy","Flowers","Jewelry",
                        "GreetingCards","EveningOut","Clothing","GiftCards"),
               names_to = "activities",
               values_to = "Percentage")


#------Loading Color Palette----

mycolors_tt <- c("#C00645","#AB3392","#69BABB","#7C1512","#F3A4CB","#FD8067")



#-------Making the Graph------


valentines_tbl <- ggplot(gift_age_new, aes(x = factor(activities,level = c("SpendingCelebrating","Candy","Flowers",
                                                                           "Jewelry","Clothing","EveningOut","GiftCards",
                                                                           "GreetingCards"))
                                                                  , y = Percentage, group = Age,)) +
  xlab(element_blank()) +
  geom_line(linewidth = 1.5, alpha = 0.8, aes(colour = Age)) +
  ggstar::geom_star(starshape = 16, size = 3) +
  scale_color_manual(values = mycolors_tt) +
  theme(axis.line = element_line(color='black'),
        plot.background = element_rect(fill = '#F2DAD8', colour = "#F2DAD8") ,
        panel.background = element_rect(fill = "#F2DAD8", colour = "#F2DAD8"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family = "Arial Rounded MT Bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#C79185", colour = "#C79185" ),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8, hjust = -0.05)) +
  scale_x_discrete(labels = c("Celebrating","Candy","Flowers","Jewelry","Clothing",
                               "Date Night","Gift Cards","Greeting Cards")) +
  scale_y_continuous(breaks = seq(0,80,5), labels = scales::label_percent(scale = 1, suffix = "%")) +
  labs(title = "Valentines Day is a Young Man's (and Woman's) Game",
       subtitle = "Celebrating % is out of the whole sample, while the rest is what the % of the participating couples are gifting their SO",
       caption = "Data | NRF Survey; organized by Suraj Das                               X; @AlexFeazelle")
#valentines_tbl

#----------Saving Graphic----
ggsave("tidy_tuesday_021424_final.png",plot = valentines_tbl, device = "png", height = 10,
       width = 10, dpi = "retina" )


