#loading packages
library(dplyr)
library(tidyr)
library(rstatix)#for t_test(), add_significance()

#loading data sheet
data <- read.csv("economic.csv")

#here waste is equal to the investment in the article
#make data sheet in long format
df_long <- data %>%
  group_by(Crop, Site) %>%
  summarise(Profit = sum(Profit),
            Waste  = sum(Waste), .groups = "drop") %>%
  pivot_longer(cols = c(Profit, Waste),
               names_to = "Category",
               values_to = "Value")

#function to run t-test only if both groups have >=2 replicates between investment (waste) vs profit
safe_ttest <- function(df) {
  counts <- table(df$Category)
  if (all(counts >= 2)) {
    return(t_test(df, Value ~ Category, var.equal = FALSE))
  } else {
    return(tibble(Crop = unique(df$Crop),
                  .y. = "Value",
                  group1 = "Profit",
                  group2 = "Waste",
                  n1 = counts["Profit"],
                  n2 = counts["Waste"],
                  statistic = NA, df = NA,
                  p = NA, p.adj = NA, p.signif = NA))
  }
}

#calculate t-values for each crop
t_results <- df_long %>%
  group_by(Crop) %>%
  group_modify(~ safe_ttest(.x)) %>%
  ungroup() %>%
  adjust_pvalue(method = "BH") %>%
  add_significance("p.adj")

#summarizing the data sheet of t_results
t_results %>%
  select(Crop, group1, group2, n1, n2, statistic, df, p, p.adj, p.signif)


#write the results to a CSV file
write.csv(t_results, "t_test_results-economics.csv", row.names = FALSE)


#creating percentage investment-waste plot
a1 <- ggplot(df_long, aes(x = Crop, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  stat_compare_means(method = "t.test",
                     label = "p.signif",
                     aes(group = Category),
                     size = 1) +
  theme_minimal() +
  labs(
    x = "Crop",
    y = "Cost percentage (%)",
    fill = "Cost category"
  ) +
  scale_fill_manual(values = c("percent_profit" = "#0072B2", "percent_waste" = "#E69F00"),
                    labels = c("Profit", "Investment")) +
  theme(
    axis.title.x = element_text(face = "bold", size = 24),
    axis.title.y = element_text(face = "bold", size = 24),
    axis.text = element_text(size = 20, color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),  # <-- Added this line
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "black", size = 1),
    legend.title = element_text(face = "bold", size = 18),
    legend.text = element_text(size = 18),
    legend.position = c(0.85, 0.90)
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1)

a1

#Saving the figure
ggsave("Waste-Profit.jpg", width = 11, height = 8, dpi = 600)
