setwd("/Users/dinelkathilakarathnegmail.com/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Education/Publications/InReview/ByShalikaMadam/2024-CropDamage/DataAnalysis")

env <-read.csv("all.csv", header = TRUE, sep = ',', na.strings = "NA") 
head(env)
summary(env)


# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(patchwork)
library(stringr)
library(bipartite)


#Check the difference of damage among pests between rainy and non-rainy periods
summary(env$Species)


#---------------------------------#
           #Figure S1#
#---------------------------------#

#Polytunnel and open field climatic data comparison-Figure S1
t.test(E ~ Field.Type, data = df, var.equal = FALSE)
t.test(RH ~ Field.Type, data = df, var.equal = FALSE)
t.test(PH ~ Field.Type, data = df, var.equal = FALSE)
t.test(RF ~ Field.Type, data = df, var.equal = FALSE) #Significant t = 5.2995, df = 112.77, p-value = 5.828e-07
t.test(T ~ Field.Type, data = df, var.equal = FALSE)


# Boxplot with t-test
a <- ggboxplot(df, x = "Field.Type", y = "RH", fill = "Field.Type") +
        stat_compare_means(method = "t.test", 
                     comparisons = list(c("Agriculture", "Polytunnel")), # groups to compare
                     label = "p.format",
                     size = 6) +
        theme_minimal() +
        labs(x = NULL,
            y = "Relative humidity (%)") +
        theme(axis.title.y = element_text(face = "bold", size = 22),
              axis.text = element_text(size = 18, color = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(color = "black", size = 1),
              legend.position = "none") +
        geom_hline(yintercept = 40, color = "black", size = 1)
a


b <- ggboxplot(df, x = "Field.Type", y = "PH", fill = "Field.Type") +
        stat_compare_means(method = "t.test", 
                     comparisons = list(c("Agriculture", "Polytunnel")), # groups to compare
                     label = "p.format",
                     size = 6) +
        theme_minimal() +
        labs(x = NULL,
            y = "Soil pH") +
        theme(axis.title.y = element_text(face = "bold", size = 22),
              axis.text = element_text(size = 18, color = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(color = "black", size = 1),
              legend.position = "none"
              ) +
        geom_hline(yintercept = 3, color = "black", size = 1)
b

c <- ggboxplot(df, x = "Field.Type", y = "RF", fill = "Field.Type") +
        stat_compare_means(method = "t.test", 
                     comparisons = list(c("Agriculture", "Polytunnel")), 
                     label = "p.format",
                     size = 6) +
        theme_minimal() +
        labs(x = NULL,
            y = "Rainfall (mm per day)") +
        theme(axis.title.y = element_text(face = "bold", size = 22),
              axis.text = element_text(size = 18, color = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(color = "black", size = 1),
              legend.position = "none"
              ) +
        geom_hline(yintercept = 0, color = "black", size = 1)
c


d <- ggboxplot(df, x = "Field.Type", y = "T", fill = "Field.Type") +
        stat_compare_means(method = "t.test", 
                     comparisons = list(c("Agriculture", "Polytunnel")), # groups to compare
                     label = "p.format",
                     size = 6) +
        theme_minimal() +
        labs(x = NULL,
            y = "Temperature (°C)") +
        theme(axis.title.y = element_text(face = "bold", size = 22),
              axis.text = element_text(size = 18, color = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(color = "black", size = 1),
              legend.position = "none"
              ) +
        geom_hline(yintercept = 15, color = "black", size = 1)
d

figureS1 <- ggarrange(
                      a, b, c, d,
                      nrow = 2, ncol = 2,
                      common.legend = TRUE,   
                      legend = "none",      
                      labels = c("A", "B", "C", "D"),  
                      font.label = list(size = 20, face = "bold", color = "black")
                    )
figureS1 <- annotate_figure(
  figureS1,
  bottom = text_grob("Field Type", size = 22, face = "bold")
)


ggsave("figureS1.jpg",  
       plot = figureS1,
       width = 8, height = 11,
       dpi = 600)

#---------------------------------#
           #Figure 3#
#---------------------------------#

#summarise data according to plant parts and season to calculate SE
env_summary_part <- env %>%
  group_by(Part, Season) %>%
  summarise(
    mean_damage = mean(Damage, na.rm = TRUE),
    se_damage   = sd(Damage, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


p1 <- ggplot(env_summary_part, aes(x = Part, y = mean_damage, fill = Season)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
          geom_errorbar(
            aes(ymin = mean_damage - se_damage, ymax = mean_damage + se_damage),
            position = position_dodge(width = 0.9),
            width = 0.2,
            size = 1
          ) +
          # Add mean ± SE text above bars
          geom_text(
            aes(
              label = sprintf("%.1f ± %.1f", mean_damage, se_damage),
              y = mean_damage + se_damage + 1   # push text a little above error bar
            ),
            position = position_dodge(width = 0.9),
            vjust = 0,
            hjust = -0.0001,
            size = 4,
            angle = 90
          ) +
          theme_minimal() +
          labs(
            x = "Crop part",
            y = "Degree of physical damage (%)"
          ) +
          scale_fill_manual(
            values = c("rainy" = "#0072B2", "non-rainy" = "#E69F00"),
            labels = c("rainy" = "Rainy", "non-rainy" = "Non-Rainy")
          ) +
          theme(
            axis.title.y = element_blank(),
            axis.title.x = element_text(face = "bold", size = 22),
            axis.text =  element_text(size = 19, color = "black"),
            legend.text = element_text(size = 19),
            legend.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(color = "black", size = 1)
          ) +
          geom_hline(yintercept = 0, color = "black", size = 1)

p1


ggsave("CropParts.jpg", dpi = 600)


#summarise data according to field type and season to calculate SE
env_summary_field <- env %>%
  group_by(Field.Type, Season) %>%
  summarise(
    mean_damage = mean(Damage, na.rm = TRUE),
    se_damage   = sd(Damage, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


p2 <- ggplot(env_summary_field, aes(x = Field.Type, y = mean_damage, fill = Season)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
          geom_errorbar(
            aes(ymin = mean_damage - se_damage, ymax = mean_damage + se_damage),
            position = position_dodge(width = 0.9),
            width = 0.2,
            size = 1
          ) +
          # Add mean ± SE text above bars
          geom_text(
            aes(
              label = sprintf("%.1f ± %.1f", mean_damage, se_damage),
              y = mean_damage + se_damage + 1   # push text a little above error bar
            ),
            position = position_dodge(width = 0.9),
            vjust = 0,
            hjust = -0.0001,
            size = 4,
            angle = 90
          ) +
          theme_minimal() +
          labs(
            x = "Field Type",
            y = "Degree of physical damage (%)"
          ) +
          scale_fill_manual(
            values = c("rainy" = "#0072B2", "non-rainy" = "#E69F00"),
            labels = c("rainy" = "Rainy", "non-rainy" = "Non-Rainy")
          ) +
          theme(
            axis.title.x = element_text(face = "bold", size = 22),
            axis.title.y = element_blank(),
            axis.text =  element_text(size = 19, color = "black"),
            legend.text = element_text(size = 19),
            legend.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(color = "black", size = 1)
          ) +
          geom_hline(yintercept = 0, color = "black", size = 1)

p2

ggsave("FieldType.jpg", dpi = 300)


#Create the faceted bar graph for life history
env$Stage <- factor(env$Stage, levels = c("Nursery", "Young", "Mature"))

#summarise data according to stage and season to calculate SE
env_summary_stage <- env %>%
  group_by(Stage, Season) %>%
  summarise(
    mean_damage = mean(Damage, na.rm = TRUE),
    se_damage   = sd(Damage, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


p3 <- ggplot(env_summary_stage, aes(x = Stage, y = mean_damage, fill = Season)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
          geom_errorbar(
            aes(ymin = mean_damage - se_damage, ymax = mean_damage + se_damage),
            position = position_dodge(width = 0.9),
            width = 0.2,
            size = 1
          ) +
          # Add mean ± SE text above bars
          geom_text(
            aes(
              label = sprintf("%.1f ± %.1f", mean_damage, se_damage),
              y = mean_damage + se_damage + 1   # push text a little above error bar
            ),
            position = position_dodge(width = 0.9),
            vjust = 0,
            hjust = -0.0001,
            size = 4,
            angle = 90
          ) +
          theme_minimal() +
          labs(
            x = "Life History Stage",
            y = "Degree of physical damage (%)"
          ) +
          scale_fill_manual(
            values = c("rainy" = "#0072B2", "non-rainy" = "#E69F00"),
            labels = c("rainy" = "Rainy", "non-rainy" = "Non-Rainy")
          ) +
          theme(
            axis.title.x = element_text(face = "bold", size = 22),
            axis.title.y = element_blank(),
            axis.text =  element_text(size = 19, color = "black"),
            legend.text = element_text(size = 19),
            legend.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(color = "black", size = 1)
          ) +
          geom_hline(yintercept = 0, color = "black", size = 1)

p3

ggsave("Stage.jpg", dpi = 600)



combined_plot <- ggarrange(
  p1,  
  ggarrange(
    p3, p2, 
    ncol = 2, 
    labels = c("B", "C"), 
    font.label = list(size = 24, face = "bold"),
    label.x = 0.89,  
    label.y = 1     
  ),  
  common.legend = TRUE, 
  nrow = 2, 
  labels = "A", 
  font.label = list(size = 24, face = "bold"),
  label.x = 0.95,   
  label.y = 1      
)

# Add common y-axis label
final_plot <- annotate_figure(
  combined_plot,
  left = text_grob("Degree of physical damage (%)", 
                   rot = 90, face = "bold", size = 22)
)

# Save
ggsave("combined-Figure3b.jpg", final_plot, width = 8, height = 11, dpi = 600)


#---------------------------------#
           #Figure 4#
#---------------------------------#
# assuming your data frame is called df
df1 <- env %>%
  group_by(SpeciesAb, Crop) %>%
  summarise(total_abundance = sum(Abundance, na.rm=TRUE),
            mean_damage = mean(Damage, na.rm=TRUE),
            .groups="drop")

summary(df1)


mat <- df1 %>%
  select(SpeciesAb, Crop, total_abundance) %>%
  pivot_wider(names_from = Crop, values_from = total_abundance, values_fill = 0) %>%
  as.data.frame()
mat

rownames(mat) <- mat$SpeciesAb
mat$SpeciesAb <- NULL
mat <- as.matrix(mat)

colnames(mat) <- abbreviate(colnames(mat), minlength = 6)
rownames(mat) <- abbreviate(rownames(mat), minlength = 6)

#plotting network between crop-pest species
jpeg("NetworkPlot.jpg", width = 11, height = 8, units = "in", res = 600)
plotweb(mat, 
        method="normal", 
        col.high="lightgreen",
        col.low="lightblue",
        bor.col.interaction="grey50",
        text.rot=90,
        text.high.col = "black",
        labsize = 2)
dev.off()



#---------------------------------#
           #Figure 6#
#---------------------------------#

#making figure 6 with error bar using original data
df <- read.csv("questionnariere.csv", header = TRUE, sep = ',')
head(df)

#helper function to format percentage
format_pct <- function(x) sprintf("%.2f", x * 100)

#binary variables (Molluscicide, Crop rotation, Scientific advice)
binary_summary <- df %>%
  summarise(
    Molluscicide = mean(Molluscide.use),
    CropRotation = mean(Crop.rotation),
    Scientific = mean(Scientific.advice)
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Mean") %>%
  mutate(SE = c(
    sd(df$Molluscide.use)/sqrt(nrow(df)),
    sd(df$Crop.rotation)/sqrt(nrow(df)),
    sd(df$Scientific.advice)/sqrt(nrow(df))
  ),
  Label = paste0(format_pct(Mean), " ± ", format_pct(SE)))

p4 <- ggplot(binary_summary, aes(x = Variable, y = Mean * 100)) +
            geom_col(fill = "#0072B2", width = 0.4) +
            geom_errorbar(aes(ymin = (Mean - SE) * 100, ymax = (Mean + SE) * 100), width = 0.2) +
            geom_text(aes(label = Label, y = (Mean + SE) * 100 + 5), size = 5.5, angle = 0) +
            scale_y_continuous(limits = c(0, 100)) +
            scale_x_discrete(labels = c("CropRotation" = "CR", 
                                        "Molluscicide" = "MU", 
                                        "Scientific" = "SS")) +
            labs(x = "Pest controlling methods",
                y = "Number of Fields (%)") +  # remove % from label
            theme_minimal() +
            theme(axis.title = element_text(face = "bold", size = 20),
                  axis.text = element_text(size = 18, color = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line.y = element_line(color = "black", size = 1),
                  legend.position = "none") +
            geom_hline(yintercept = 0, color = "black", size = 1)

p4

ggsave("PestControlMeasures.jpg", dpi = 600)


#Resting places
rest_summary <- df %>%
  group_by(Rest.place) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count),
         SE = sqrt((Proportion * (1 - Proportion)) / sum(Count)),
         Label = paste0(format_pct(Proportion), " ± ", format_pct(SE)))

p5 <- ggplot(rest_summary, aes(x = Rest.place, y = Proportion*100)) +
            geom_col(fill = "#0072B2", width = 0.4) +
            geom_errorbar(aes(ymin = (Proportion - SE) * 100, ymax = (Proportion + SE) * 100), width = 0.2) +
            geom_text(aes(label = Label, y = (Proportion + SE) * 100 + 5), size = 5.5, angle = 0) +
            scale_y_continuous(limits = c(0, 100)) +
            scale_x_discrete(labels = c("Damping sites" = "Damp", 
                                        "Edge" = "Edge", 
                                        "Soil" = "Soil")) +
            labs(x = "Resting places", y = "Number of fields (%)") +
            theme_minimal() +
            theme(axis.title = element_text(face = "bold", size = 20),
                  axis.text = element_text(size = 18, color = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line.y = element_line(color = "black", size = 1),
                  legend.position = "none") +
            geom_hline(yintercept = 0, color = "black", size = 1)

p5

ggsave("RestingPlaces.jpg", dpi=600)


combined_plot1 <- ggarrange(
  p5, p4, 
        ncol = 1, nrow = 2,
        labels = c("A", "B"),   
        font.label = list(size = 24, face = "bold"),
        label.x = 0.88,
        label.y = 1,
        widths = c(1, 1),       
        heights = c(1, 0.9)     
)

ggsave(filename = "Figure6.jpg", plot = combined_plot1, width = 8, height = 11, dpi = 600)
