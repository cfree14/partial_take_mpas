
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

# Order
type_order <- c("No-take", "Multi-use", "Non-MPA")

# sites
nsites  <- 100
sites <- tibble(lat_dd=runif(nsites, min=-65, max=80),
                long_dd=runif(nsites, min=-180, max=180),
                type=sample(x=c("No-take", "Multi-use", "Non-MPA"), size=nsites, replace=T) %>%
                  factor(., levels=type_order))

# Effects
effects <- tibble(type=c('No-take', "Multi-use") %>% factor(., c('No-take', "Multi-use") ),
                  mean=c(0.45, 0.2),
                  sd1=0.2,
                  sd2=0.3) %>%
  mutate(ci1_lo=mean-sd1,
         ci1_hi=mean+sd1,
         ci2_lo=mean-sd2,
         ci2_hi=mean+sd2)

# Plot data
################################################################################

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass="sf")

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
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


# Plot map
g1 <- ggplot() +
  # Plot wold
  geom_sf(data=world, fill="grey90", color="white", lwd=0.2) +
  # Plot points
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd, fill=type), pch=21) +
  # Labels
  labs(x=" ", y="", tag="A",) +
  # Crop
  coord_sf(y=c(-65,85)) +
  # Legend
  scale_fill_manual(name="Site type", values=c("red", "blue", "grey60")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(color="white"),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.2),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot effect size
g2 <- ggplot(effects, mapping=aes(x=type, y=mean, color=type)) +
  # Reference line
  geom_hline(yintercept = 0, color="grey60", linetype="dotted") +
  # Plot CI
  geom_errorbar(mapping=aes(x=type, ymin=ci2_lo, ymax=ci2_hi, color=type), width=0, linewidth=0.5) +
  geom_errorbar(mapping=aes(x=type, ymin=ci1_lo, ymax=ci1_hi, color=type), width=0, linewidth=1.5) +
  # Plot points
  geom_point(pch=21, fill="white", size=3) +
  # Labels
  labs(x="vs. non-MPA", y="Effect size\n(log biomass ratio)", tag="B") +
  # Axis
  # scale_y_continuous(lim=c(-0.1, 0.7)) +
  # Legend
  scale_color_manual(name="Site type", values=c("red", "blue")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.75, 0.25))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_map_plus.png"),
       width=6.5, height=2.5, units="in", dpi=600)

