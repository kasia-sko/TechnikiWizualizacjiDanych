library(ggplot2)
library(dplyr)

data <- read.csv("data.csv")

plot <- data %>%
  mutate(Percentage_diff = Harris - Trump,
         Fill_color = case_when(
           Percentage_diff >= 0 ~ "Harris",
           Percentage_diff < 0 ~ "Trump"
         ),
         Date = as.Date(Date, format = "%d-%m-%Y")) %>%
  arrange(Date) %>%
  mutate(Date = factor(Date, levels = unique(Date))) %>%
  ggplot(aes(x = Date, y = Percentage_diff, fill = Fill_color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_y_continuous(limits = c(-3, 3),
                     expand = c(0, 0),
                     #labels = scales::label_percent(scale = 1, suffix = "%")
                     labels = function(x) paste0(abs(x), "%")
  ) +
  labs(x= "Date",
       y = "Percentage Lead",
       fill = "Candidate",
       title = "Percentage Lead in the US 2024 Election Over Time") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70,size = 10, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 14))

plot

