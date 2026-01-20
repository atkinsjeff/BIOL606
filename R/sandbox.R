library(ggplot2)
library(dplyr)

data(InsectSprays)

# Calculate summary data
summary_data <- InsectSprays %>%
  group_by(spray) %>%
  summarize(
    mean_count = mean(count),
    se = sd(count) / sqrt(n()),
    .groups = "drop"
  )

# Run ANOVA and Tukey
anova_model <- aov(count ~ spray, data = InsectSprays)
tukey_results <- TukeyHSD(anova_model)

# Function to determine significance vs control (spray A)
get_sig_vs_control <- function(spray_name, tukey_results) {
  if (spray_name == "A") return("")  # Control
  
  comparison_name <- paste(spray_name, "A", sep = "-")
  p_value <- tukey_results$spray[comparison_name, "p adj"]
  
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("ns")
}

# Add significance symbols
summary_data$significance <- sapply(summary_data$spray, 
                                    get_sig_vs_control, 
                                    tukey_results = tukey_results)

# Create plot
x11()
ggplot(summary_data, aes(x = spray, y = mean_count, fill = spray)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = mean_count - se, 
                    ymax = mean_count + se),
                width = 0.3) +
  geom_text(aes(label = significance, y = mean_count + se + 1.5), 
            size = 6, fontface = "bold") +
  labs(x = "Insecticide Spray Type",
       y = "Mean Insect Count",
       title = "Insect Counts by Spray Type",
       caption = "* p < 0.05, ** p < 0.01, *** p < 0.001 compared to spray A (Tukey HSD)") +
  theme_bw() +
  theme(legend.position = "none")
#####

library(ggplot2)
library(dplyr)
library(car)

# Load the data
data(InsectSprays)

# Calculate means and standard errors by spray type
summary_data <- InsectSprays %>%
  group_by(spray) %>%
  summarize(
    mean_count = mean(count),
    se = sd(count) / sqrt(n()),
    .groups = "drop"
  )

# Create bar chart with error bars
x11()
ggplot(summary_data, aes(x = spray, y = mean_count, fill = spray)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = mean_count - se, 
                    ymax = mean_count + se),
                width = 0.3) +
  labs(x = "Insecticide Spray Type",
       y = "Mean Insect Count",
       title = "Insect Counts by Spray Type") +
  theme_bw() +
  theme(legend.position = "none")

# Run ANOVA
anova_model <- aov(count ~ spray, data = InsectSprays)

# ANOVA table
cat("\n=== ANOVA Results ===\n")
summary(anova_model)

# Calculate effect size (eta-squared)
cat("\n=== Effect Size ===\n")
ss_total <- sum((InsectSprays$count - mean(InsectSprays$count))^2)
ss_spray <- sum(anova_model$effects[-1]^2)
eta_squared <- ss_spray / ss_total
cat("Eta-squared:", round(eta_squared, 3), "\n")

# Check assumptions
cat("\n=== Assumption Tests ===\n")

# Normality of residuals
cat("\nShapiro-Wilk Test for Normality:\n")
shapiro_test <- shapiro.test(residuals(anova_model))
print(shapiro_test)

# Homogeneity of variance
cat("\nLevene's Test for Homogeneity of Variance:\n")
levene_test <- leveneTest(count ~ spray, data = InsectSprays)
print(levene_test)

# Diagnostic plots
cat("\n=== Diagnostic Plots ===\n")
par(mfrow = c(2, 2))
plot(anova_model)
par(mfrow = c(1, 1))

# Post-hoc comparisons (Tukey HSD)
cat("\n=== Tukey HSD Post-hoc Comparisons ===\n")
tukey_results <- TukeyHSD(anova_model)
print(tukey_results)

# Visualize Tukey results
plot(tukey_results, las = 1, cex.axis = 0.7)
title("Tukey HSD: Pairwise Comparisons")





#### PCA
# Run PCA (exclude species column)
data(iris)
library(ggbiplot)
iris.pca <- prcomp (~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                    data=iris,
                    scale. = TRUE)
summary(iris.pca)
iris.gg <-
  ggbiplot(iris.pca, obs.scale = 1, var.scale = 1,
           groups = iris$Species, point.size=2,
           varname.size = 5, 
           varname.color = "black",
           varname.adjust = 1.2,
           ellipse = TRUE, 
           circle = TRUE) +
  labs(fill = "Species", color = "Species") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

iris.gg


### DENDRO
# install.packages("ggdendro")
library(ggplot2)
library(ggdendro)
theme_set(theme_bw())

scaled_data <- scale(USArrests)
hc_ward <- hclust(dist(scaled_data), method="ward.D2")
ggdendrogram(hc_ward, rotate = TRUE, size = 2)
plot(hc_ward, main="", 
     xlab="States", ylab="Distance", hang=-1, cex=0.6)
# plot
ggdendrogram(hc, rotate = TRUE, size = 2)





####### 
# lter sampling
install.packages("lterdatasampler") 

# bring it in
library(lterdatasampler)
require(ggplot2)
require(tidyverse)


df <- pie_crab


x <- hbr_maples

x %>%
  filter(year == 2004 & watershed == "W1" ) %>%
  slice_sample(n = 50) %>%
  data.frame() -> w1.2004

x %>%
  filter(year == 2004 & watershed == "Reference" ) %>%
  slice_sample(n = 50) %>%
  data.frame() -> ref.2004

df.04 <- rbind(w1.2004, ref.2004)

x11()
ggplot(data = df.04, aes(x = watershed, y = stem_length)) +
  geom_boxplot(aes(color = watershed, shape = watershed),
               alpha = 0.8,
               width = 0.5) +
  geom_jitter(
    aes(color = watershed),
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  labs(
    x = "Watershed",
    y = "Stem length (millimeters)",
    title = "Stem Lengths of Sugar Maple Seedlings",
    subtitle = "Hubbard Brook LTER"
  ) +
  facet_wrap(~year) +
  theme_minimal()

shapiro.test(df.04$stem_length)
qqnorm(df.04$stem_length)
hist(df.04$stem_length)

t.test(stem_length ~ watershed, data = df.04)

write.csv(df.04, "HubbardSortedMaple.csv")

t.test(mpg ~ am, data = mtcars)

# Paired t-test
## The sleep data is actually paired, so could have been in wide format:
sleep2 <- reshape(sleep, direction = "wide",
                  idvar = "ID", timevar = "group")


require(fortedata)

df <- fd_inventory()

df %>% 
  filter(species == "BEPA" & replicate == "B" | replicate == "C") %>%
  data.frame() -> bepa

x11()
ggplot(bepa, aes(x = as.factor(replicate), y = dbh_cm))+
  geom_boxplot()

t.test(dbh_cm ~ as.factor(replicate), data = bepa)

# taking 25 of each
df %>%
  filter(health_status == "L") %>%
  filter(species %in% c("ACRU", "ACSA3", "FAGR", "BEPA", "POGR4")) %>%
  group_by(species) %>%
  sample_n(25) %>%
  data.frame() -> trees


x11()
ggplot(trees, aes(x = as.factor(species), y = dbh_cm))+
  geom_boxplot()

write.csv(trees, "FoRTE_dbh_data.csv")



### ICE COVER
ice <- ntl_icecover

ice %>%
  filter(lakeid == "Lake Mendota") %>%
  data.frame() -> men

x11()
ggplot(men, aes(x = year, y = ice_duration))+
  geom_point()
men <- na.omit(men)
m <- lm(ice_duration ~ year, data = men)



plot(m, 2)
shapiro.test(m$residuals)
cor(men$year, men$ice_duration,method = c("pearson"))

car::ncvTest(m)

write.csv(men, "LakeMendota.csv")
######
# Assuming your data is in a vector called 'biomass'
# Example data (replace with your actual data)
biomass <- c(12.3, 8.7, 15.2, 10.1, 9.8, 13.5, 11.2, 10.9, 12.1, 11.8,
             10.5, 11.9, 12.3, 11.1, 10.8, 12.0, 11.5, 11.7, 11.9, 12.1)

# Calculate cumulative mean
cumulative_mean <- cumsum(biomass) / seq_along(biomass)

# Create the performance curve
x11()
plot(1:length(biomass), cumulative_mean,
     type = "b",  # both points and lines
     xlab = "Number of Samples",
     ylab = "Cumulative Mean Biomass (mg)",
     main = "Performance Curve for Macroinvertebrate Biomass",
     pch = 19,  # solid points
     col = "blue",
     lwd = 2)

# Add a horizontal line at the final mean (optional)
abline(h = mean(biomass), lty = 2, col = "red")

# Add grid for easier reading
grid()



# Load required package
# Install if needed: install.packages("vegan")
library(vegan)

# Example 1: Simple simulated data
# Create a community matrix (rows = sites/samples, columns = species)
# Each cell contains abundance or presence/absence (0/1)

set.seed(123)  # for reproducibility

# Simulate 20 sampling sites with 15 species
community_data <- matrix(
  sample(0:5, 20 * 15, replace = TRUE, prob = c(0.3, 0.2, 0.2, 0.15, 0.1, 0.05)),
  nrow = 20,
  ncol = 15
)

# Add column names (species)
colnames(community) <- paste("Species", 1:15, sep = "_")

# Create species accumulation curve
sac <- specaccum(community, method = "random", permutations = 100)

# Plot the curve
plot(sac, 
     ci.type = "polygon",
     ci.col = "lightblue",
     ci.lty = 0,
     xlab = "Number of Samples",
     ylab = "Cumulative Number of Species",
     main = "Species Accumulation Curve",
     lwd = 2,
     col = "blue")

# Add legend
legend("bottomright", 
       legend = c("Mean", "95% CI"),
       col = c("blue", "lightblue"),
       lwd = c(2, 10),
       bty = "n")



ibrary(vegan)

# Create SAC
sac <- specaccum(community_data, method = "random", permutations = 100)

# Fit models to extrapolate
# Estimate asymptotic species richness
fit_models <- fitspecaccum(sac, "arrh")  # Arrhenius model

plot(fit_models, 
     xlab = "Number of Samples",
     ylab = "Species Richness",
     main = "Species Accumulation with Asymptote Prediction")

# Predict species richness at different sample sizes
predict(fit_models, newdata = data.frame(sites = c(30, 40, 50)))

# Check if you've sampled enough
# Rule of thumb: curve should be approaching asymptote
# Calculate slope between last few points
n <- length(sac$richness)
slope <- (sac$richness[n] - sac$richness[n-2]) / 2

cat("Slope of last 3 points:", slope, "\n")
cat("If slope < 0.1, sampling is likely adequate\n")



# differences
# 
James River 	Crump Park
6	4
5	2
5	3
7	4
6	7
6	5
8	6
4	8
5	7
4	1


grouper <- read.csv("grouper.csv")

grouper %>%
  group_by(treatment) %>%
  summarise(
    statistic = shapiro.test(no_grouper)$statistic,
    p_value = shapiro.test(no_grouper)$p.value )


shapiro.test(grouper$no_grouper)

hist(grouper$no_grouper ~ as.factor(grouper$treatment))
kruskal.test(no_grouper ~ treatment, data = grouper)

group.mod <- aov(no_grouper ~ treatment, data = grouper)
shapiro.test(residuals(group.mod))
summary(aov(no_grouper ~ treatment, data = grouper))

## Add x-labels and use a pre-fix on the main labels
ggplot(grouper, aes(x = no_grouper)) +
  geom_histogram(aes(color = treatment), fill = "white",
                 position = "identity", bins = 30) +
  scale_color_manual(values = c("#00AFBB", "#E7B800" , "green")) 




# palmer
install.packages("palmerpenguins")
require(tidyverse)
summary(penguins)
require(palmerpenguins)

sex_year_table = table(penguins$island, penguins$year)
chisq.test(island.species)
pen <- data.frame(penguins)


write.csv(pen, "palmerpenguins.csv")
island.species <- table(penguins$species, penguins$island, penguins$year)

penguins %>%
  group_by(island) %>%
  summarise(
    statistic = shapiro.test(body_mass_g)$statistic,
    p_value = shapiro.test(body_mass_g)$p.value )
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

# The diamonds dataset is natively available with R.

# Without transparency (left)
ggplot(penguins, aes(x=year, group=species, fill=island)) +
  geom_density(adjust=1.5) +
  theme_ipsum()
#p1

# With transparency (right)
p2 <- ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

# stacked area chart
ggplot(penguins, aes(x = year, y = species, fill = species)) + 
  geom_area()










#####
df <- and_vertebrates
x11()
ggplot(df, aes(x=rec_year, y = animal_weight, color = as.factor(animal_sex))) +
  geom_line()





# o layer

df <- read.csv("o_layer.csv")

df %>%
  group_by(VEG) %>%
  summarize(o.dep = mean(Odepth, na.rm = TRUE),
            o.depsd = sd(Odepth, na.rm = TRUE))


x11()
ggplot(df, aes(x = ON, group = VEG, fill = VEG))+
  geom_histogram(adjust=1.5, alpha=.4)

ggplot(penguins, aes(x=year, group=species, fill=island)) +
  geom_density(adjust=1.5) +
  theme_ipsum()


ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~cut) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
    
    
    
    df <- read.csv("NEONForestAGBv2.csv")
    
    # sort down to Virginia
    df %>%
      filter(siteID %in% c("BLAN", "MLBS", "SCBI", "SERC")) %>%
      data.frame() -> df2
    
    df2$date <- as.POSIXct(df2$date)
    
    df2$year <- year(df2$date)
    df2 %>%
      filter(year == 2022) %>%
      filter(allometry == "AGBJenkins") %>%
      filter(growthForm == "single bole tree" | growthForm == "multi-bole tree") %>%
      data.frame() -> df3
    
    df.species <- as.data.frame(table(df3$Genus, df3$siteID))
    
    
    df.mat <- as.matrix(table(df3$Genus, df3$siteID))
    write.csv(df.mat, "NEON_biodiversity_virginia_matrix.csv")
    
    df.mat <- matrix(df.species)
    
    
    # Here's how to calculate alpha, beta, and gamma diversity in R using the example dataset:
    # Method 1: Manual Calculation (Best for Understanding)
    # r# Create the community matrix (sites as rows, species as columns)
    community <- matrix(
      c(1, 1, 0, 0, 1,  # Patch A
        1, 0, 1, 0, 1,  # Patch B
        0, 0, 1, 1, 1), # Patch C
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        c("Patch_A", "Patch_B", "Patch_C"),
        c("Species_1", "Species_2", "Species_3", "Species_4", "Species_5")
      )
    )
    
    # View the matrix
    print(community)
    
    # ==================
    # ALPHA DIVERSITY
    # ==================
    # Alpha = species richness per site
    
    # Calculate alpha diversity for each patch
    alpha <- rowSums(community > 0)  # Count species present (>0) in each row
    print("Alpha diversity (per patch):")
    print(alpha)
    
    # Mean alpha diversity
    mean_alpha <- mean(alpha)
    cat("\nMean alpha diversity:", mean_alpha, "\n")
    
    # ==================
    # GAMMA DIVERSITY
    # ==================
    # Gamma = total species richness across all sites
    
    gamma <- sum(colSums(community) > 0)  # Count species present in ANY patch
    cat("Gamma diversity (total regional):", gamma, "\n")
    
    # ==================
    # BETA DIVERSITY
    # ==================
    # Beta = gamma / mean_alpha (multiplicative)
    # Or beta = gamma - mean_alpha (additive)
    
    # Multiplicative beta (Whittaker's beta)
    beta_multiplicative <- gamma / mean_alpha
    cat("Beta diversity (multiplicative):", beta_multiplicative, "\n")
    
    # Additive beta
    beta_additive <- gamma - mean_alpha
    cat("Beta diversity (additive):", beta_additive, "\n")
    
    # Interpretation
    cat("\nInterpretation:")
    cat("\n- On average, each patch has", mean_alpha, "species")
    cat("\n- Across all patches, there are", gamma, "total species")
    cat("\n- Species turnover (beta) is", beta_multiplicative, "times the local diversity\n")
    Method 2: Using the vegan Package (Standard Approach)
    rlibrary(vegan)
    
    # Using the same community matrix from above
    
    # ==================
    # ALPHA DIVERSITY
    # ==================
    # Multiple alpha diversity indices available
    
    # Species richness (what we calculated above)
    alpha_richness <- specnumber(community)
    print("Alpha diversity (species richness):")
    print(alpha_richness)
    
    # Shannon diversity index (accounts for evenness too)
    alpha_shannon <- diversity(community, index = "shannon")
    print("Alpha diversity (Shannon index):")
    print(alpha_shannon)
    
    # Simpson's diversity
    alpha_simpson <- diversity(community, index = "simpson")
    print("Alpha diversity (Simpson index):")
    print(alpha_simpson)
    
    # ==================
    # BETA DIVERSITY
    # ==================
    # Multiple ways to calculate beta diversity
    
    # Method 1: Whittaker's beta (based on species richness)
    mean_alpha <- mean(specnumber(community))
    gamma <- specnumber(colSums(community > 0))
    beta_whittaker <- gamma / mean_alpha
    
    cat("\nWhittaker's beta diversity:", beta_whittaker, "\n")
    
    # Method 2: Pairwise beta diversity (Bray-Curtis dissimilarity)
    beta_bray <- vegdist(community, method = "bray")
    print("Pairwise beta diversity (Bray-Curtis):")
    print(beta_bray)
    
    # Method 3: Jaccard dissimilarity (presence/absence)
    beta_jaccard <- vegdist(community, method = "jaccard", binary = TRUE)
    print("Pairwise beta diversity (Jaccard):")
    print(beta_jaccard)
    
    # Method 4: Using betadiver function
    beta_indices <- betadiver(community, method = "w")  # Whittaker
    print("Beta diversity indices:")
    print(beta_indices)
    
    # ==================
    # GAMMA DIVERSITY
    # ==================
    # Pool all sites together
    gamma_richness <- specnumber(colSums(community > 0))
    cat("\nGamma diversity (total richness):", gamma_richness, "\n")
    
    # ==================
    # SUMMARY TABLE
    # ==================
    diversity_summary <- data.frame(
      Patch = rownames(community),
      Alpha_Richness = specnumber(community),
      Alpha_Shannon = diversity(community, index = "shannon")
    )
    
    diversity_summary <- rbind(
      diversity_summary,
      data.frame(
        Patch = "Mean",
        Alpha_Richness = mean(specnumber(community)),
        Alpha_Shannon = mean(diversity(community, index = "shannon"))
      )
    )
    
    print("\nDiversity Summary:")
    print(diversity_summary)
    
    cat("\nGamma (Regional) Diversity:", gamma_richness)
    cat("\nBeta Diversity (Whittaker):", beta_whittaker, "\n")
    
    
    
    library(vegan)
    
    # Create more realistic abundance data
    set.seed(123)
    community <- matrix(
      c(45, 32, 0, 0, 28, 12, 5, 0, 8, 15,      # Site 1: 145 individuals
        38, 0, 25, 0, 42, 8, 0, 6, 10, 11,      # Site 2: 140 individuals  
        0, 0, 30, 18, 35, 0, 12, 15, 0, 20),    # Site 3: 130 individuals
      nrow = 3, byrow = TRUE,
      dimnames = list(
        c("Old_Growth", "Secondary", "Disturbed"),
        paste("Species", 1:10, sep = "_")
      )
    )
    df.mat <- as.matrix(table(df3$siteID, df3$Genus))
    
    
    df.online.tool <- as.matrix(table(df3$Genus, df3$siteID))
    write.csv(df.online.tool, "NEONonline.csv")
    community <- df.mat
    cat("=== COMMUNITY DATA ===\n")
    print(community)
    cat("\nTotal individuals per site:\n")
    print(rowSums(community))
    cat("\nSpecies richness per site:\n")
    print(specnumber(community))
    
    # Create rarefaction curves
    par(mfrow = c(1, 1))
    rarecurve(community,
              step = 5,  # Sample every 5 individuals for smoother curves
              col = c("darkgreen", "orange", "red", "blue"),
              lwd = 2.5,
              xlab = "Number of Individuals Sampled",
              ylab = "Expected Species Richness",
              main = "Rarefaction Curves Across Forest Types",
              cex.lab = 1.2)
    
    # Add comparison line
    min_n <- min(rowSums(community))
    abline(v = min_n, lty = 2, lwd = 2, col = "gray40")
    
    # Add text annotation
    text(min_n + 5, 8, 
         paste("Standardized\ncomparison\n(n =", min_n, ")"),
         pos = 4, cex = 0.9)
    
    # Legend
    legend("bottomright",
           legend = c(rownames(community), "Standardization point"),
           col = c("darkgreen", "orange", "red", "blue", "gray40"),
           lwd = c(2.5, 2.5, 2.5, 2),
           lty = c(1, 1, 1, 2),
           bty = "n",
           cex = 1.1)
    
    # Calculate rarefied richness at standardized sample size
    cat("\n=== RAREFIED RICHNESS (standardized to n =", min_n, ") ===\n")
    rarefied <- rarefy(community, sample = min_n)
    print(round(rarefied, 2))
    
    # Statistical comparison
    cat("\nInterpretation:")
    cat("\nWhen standardized to the same number of individuals,")
    cat("\nthe sites have similar/different species richness.\n")
    
    
    
    
    # Generate rarefaction data
    rare_data <- rarecurve(community, step = 1, tidy = TRUE)
    
    # Plot with ggplot2
    ggplot(rare_data, aes(x = Sample, y = Species, color = Site)) +
      geom_line(size = 1.2) +
      labs(x = "Number of Individuals Sampled",
           y = "Expected Species Richness",
           title = "Species Rarefaction Curves",
           color = "Patch") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    
    # pie crab
    
    require(tidyverse)
    summary(penguins)
    require(palmerpenguins)
    require(ggplot2)
    require(RColorBrewer)
    
    
    pen <- data.frame(penguins)
    
    x11(height = 6, width = 6)
    ggplot(pen, aes(x = body_mass_g, y = flipper_length_mm))+
      geom_point(size = 3, alpha = 0.9)+
      try_theme()+
      theme(legend.position = c(0.1, 0.9), legend.title = element_blank())+
      ylab("Flipper Length [mm]")+
      xlab("Body Mass [g]")+
      stat_smooth(method = "lm", se = FALSE, size = 1.25)
    
    cor(pen$body_mass_g, pen$flipper_length_mm, use = "complete.obs", method = "pearson")
    summary(lm(flipper_length_mm ~ body_mass_g, data = pen))
    
    lm_eqn <- function(df){
      m <- lm(flipper_length_mm ~ body_mass_g, data = pen);
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                       list(a = format(unname(coef(m)[1]), digits = 2),
                            b = format(unname(coef(m)[2]), digits = 2),
                            r2 = format(summary(m)$r.squared, digits = 3)))
      as.character(as.expression(eq));
    }
    
    p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)
    
    +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 4800))+ 
      facet_grid(. ~ year)
    
    pen %>%
      group_by(species, island, sex, year) %>%
      summarize(mass = mean(body_mass_g, na.rm = TRUE),
                mass.sd = sd(body_mass_g, na.rm = TRUE),
                flip = mean(flipper_length_mm, na.rm = TRUE),
                flip.sd = sd(flipper_length_mm, na.rm = TRUE)) %>%
      data.frame() -> pens
    
    
    #write.csv(pens, "pen_sums.csv")
    #
    pens %>%
      filter(species == "Adelie") %>%
      filter(year == 2007 | year == 2009) %>%
      data.frame() -> pens2
    
    pens2 <- na.omit(pens2)
    x11(width = 9, height = 5)
    ggplot(pens2, aes(x = island, y = mass, fill = sex, colour = sex)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8)+
      scale_fill_manual(values=c("#BD3039",
                                 "#0C2340"))+
      scale_color_manual(values=c("#BD3039",
                                  "#0C2340"))+
      geom_errorbar( aes(ymin = mass - mass.sd, ymax = mass + mass.sd), position = position_dodge(0.9),
                     width = 0.25, color = "black")+
      try_theme()+
      theme(legend.position = c(0.06, 0.9), legend.title = element_blank())+
      xlab("")+
      ylab("Body Mass [g]")+
      scale_y_continuous(expand = c(0, 0), limits = c(0, 4800))+ 
      facet_grid(. ~ year)
    
    
    
    
    ##### CUSTOM PLOT THEME
    try_theme <- function() {
      theme(
        # add border 1)
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        # color background 2)
        panel.background = element_rect(fill = "white"),
        # modify grid 3)
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black", family = "Times New Roman"),
        axis.title = element_text(size = 12, colour = "black", family = "Times New Roman"),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length=unit(-0.1, "cm"),
        # legend at the bottom 6)
        # legend.position = "bottom",
        strip.text.x = element_text(size=12, color="black",  family = "Times New Roman"),
        strip.text.y = element_text(size=10, color="black",  family = "Times New Roman"),
        strip.background = element_blank()
      )
    }
    
    ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Glass, colour = Glass)) + 
      geom_bar(stat = "identity", position = "dodge")  +
      geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25)