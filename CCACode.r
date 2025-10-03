#create species abundance matrix
species_mat <- read.csv("abun.csv")

#create environmental matrix
env_mat <- read.csv("env.csv")

#loading packages
library(vegan)
library(ggplot2)
library(grid)

#CCA model 1 for qutitative data
cca_model1 <- cca(species_mat ~ Field.Type + Season + Stage + Damage + Richness + Shannon + Abundance +
                    E + PH + RF + RH + T, data = env_mat)

summary(cca_model1)

#Check overall significance of the model
anova(cca_model1)#overall test of significance
anova(cca_model1, by = "axis")#test each CCA axis
anova(cca_model1, by = "term")#test each predictor

#Look at eigenvalues and variance explained
eigenvals(cca_model1)#eigenvalues of constrained axes
summary(cca_model1)$concont#proportion of variance explained

#Assess model fit (inertia)
cca_model1$CCA$tot.chi #total constrained inertia
cca_model1$tot.chi  #total inertia (all variance)
cca_model1$CCA$tot.chi / cca_model1$tot.chi   #% explained by constraints

#Explore collinearity among predictors
vif.cca(cca_model1)

#Cross-validation
RsquareAdj(cca_model1)

#save csv file for model summary
cca_anova <- anova(cca_model1, by = "term")
cca_anova_df <- as.data.frame(cca_anova)
write.csv(cca_anova_df, "CCA_Termwise_ANOVA.csv", row.names = TRUE)

#extract scores
sites_sc   <- scores(cca_model1, display = "sites",   choices = 1:2, scaling = 2)
species_sc <- scores(cca_model1, display = "species", choices = 1:2, scaling = 2)
biplot_sc  <- scores(cca_model1, display = "bp",      choices = 1:2, scaling = 2)

sites_df   <- as.data.frame(sites_sc)
species_df <- as.data.frame(species_sc); species_df$species <- rownames(species_df)
biplot_df  <- as.data.frame(biplot_sc);  biplot_df$variable <- rownames(biplot_df)

#rescale arrows to improve visibility
arrow_scale <- 2.0
biplot_df$CCA1 <- biplot_df$CCA1 * arrow_scale
biplot_df$CCA2 <- biplot_df$CCA2 * arrow_scale

#variance explained for axis labels
eig <- cca_model1$CCA$eig
var_pct <- round(100 * eig / sum(eig), 1)
x_lab <- paste0("CCA1 (", var_pct[1], "%)")
y_lab <- paste0("CCA2 (", var_pct[2], "%)")


#plotting cca
p <- ggplot() +
  # sites
  geom_point(data = sites_df, aes(CCA1, CCA2),
             color = "grey50", size = 1.5, alpha = 0.6) +
  # species
  geom_point(data = species_df, aes(CCA1, CCA2),
             color = "#206a5d", size = 2.5) +
  geom_text(data = species_df, aes(CCA1, CCA2, label = species),
            color = "#206a5d", size = 5, vjust = -0.6) +
  # environmental arrows
  geom_segment(data = biplot_df,
               aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               lineend = "round", color = "#b30000", linewidth = 1.1) +
  geom_text(data = biplot_df, aes(CCA1, CCA2, label = variable),
            color = "#b30000", fontface = "bold", size = 5.5, vjust = -0.7) +
  #theme
  theme_classic(base_size = 18) +
  theme(
    axis.title = element_text(size = 20, face = "bold"),
    axis.text  = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "none"
  ) +
  labs(x = x_lab, y = y_lab) +
  #crop axis
  coord_cartesian(xlim = c(-2, 6), ylim = c(-3, 3))

p

#save as tiff
ggsave("CCA_plot_pub_final1.tiff", p, width = 8, height = 10, dpi = 600, compression = "lzw")


