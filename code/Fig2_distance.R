
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Simulate data
################################################################################

# Data
data <- tibble(dist_km=c(25, 50, 75, 100, 200, 500, 1000, 2500),
               dist_km_chr=as.character(dist_km),
               n=c(1320, 717, 606, 246, 654, 367, 108, 40),
               biomass_kg_100m2=c(4, 3.9, 4.8, 7, 6.8, 8, 13, 14.5),
               biomass_kg_100m2_sd=seq(1,10, length.out=8)/2) %>%
  mutate(dist_km_chr=factor(dist_km_chr, levels=dist_km_chr),
         biomass_kg_100m2_lo=biomass_kg_100m2-biomass_kg_100m2_sd,
         biomass_kg_100m2_hi=biomass_kg_100m2+biomass_kg_100m2_sd)

# Effects
effects <- tibble(type=c("All", 'Near', "Far") %>% factor(., levels=c("All", "Near", "Far")),
                  mean=c(0.1, 0.2, -0.05),
                  sd1=0.1,
                  sd2=0.2) %>%
  mutate(ci1_lo=mean-sd1,
         ci1_hi=mean+sd1,
         ci2_lo=mean-sd2,
         ci2_hi=mean+sd2)

# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    plot.tag=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))
                    # Legend
                    # legend.background = element_rect(fill=alpha('blue', 0)))

# Labels
labs <- tibble(x=c("50", "500"),
               label=c("Near population center\n(high human pressure)",
                       "Far from population center\n(low human pressure)"))


# Plot biomass ~ distance
g1 <- ggplot(data, aes(x=dist_km_chr, y=biomass_kg_100m2)) +
  # Reference lines
  geom_hline(yintercept=5, color="grey60", linetype="dotted") +
  geom_vline(xintercept="100", color="grey60", linetype="dotted") +
  # Data
  geom_errorbar(aes(ymin=biomass_kg_100m2_lo, ymax=biomass_kg_100m2_hi), color="grey30", width=0) +
  geom_point() +
  # Plot text
  geom_text(data=labs, mapping=aes(x=x, y=18, label=label), size=2.4) +
  # Labels
  labs(x="Distance from population center (km)", y=expression("Biomass (kg/100m"^"2"*")"), tag="A") +
  scale_y_continuous(lim=c(0,NA)) +
  # Theme
  theme_bw() + base_theme
g1

# Plot effect size ~ distance
g2 <- ggplot(effects, aes(x=type, y=mean)) +
  # Reference line
  geom_hline(yintercept=0, color="grey60", linetype="dotted") +
  # Plot CI
  geom_errorbar(mapping=aes(x=type, ymin=ci2_lo, ymax=ci2_hi), width=0, linewidth=0.5) +
  geom_errorbar(mapping=aes(x=type, ymin=ci1_lo, ymax=ci1_hi), width=0, linewidth=1.5) +
  # Plot points
  geom_point(pch=21, fill="white", size=3) +
  # Labels
  labs(x="Site type", y="Effect size\n(log biomass ratio)", tag="B") +
  # Theme
  theme_bw() + base_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.75, 0.25))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_distance.png"),
       width=6.5, height=2.5, units="in", dpi=600)

