
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

# Read data
data_orig <- readxl::read_excel("data/fig5_data.xlsx") %>%
  mutate(effect_sd=0.2,
         effect_sd2=0.3,
         effect_lo=effect - effect_sd,
         effect_hi=effect + effect_sd,
         effect_lo2=effect - effect_sd2,
         effect_hi2=effect + effect_sd2,
         level=recode_factor(level,
                             "inadequate"="Inadequate / weak",
                             "adequate"="Adequate / strong"),
         comparison=recode_factor(comparison,
                                  "MU:0"="Multi-use\nvs. non-MPA",
                                  "NT:0"="No-take\nvs. non-MPA",
                                  "NT:MU|MU"="Multi-use\nvs. no-take"))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.x=element_text(size=6.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text = element_text(size=8),
                    plot.tag=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))

# Plot effects
g <- ggplot(data_orig, aes(x=comparison, y=effect, color=level)) +
  facet_wrap(~category) +
  # Reference line
  geom_hline(yintercept=0, color="grey60", linetype="dotted") +
  # Data
  geom_errorbar(position=position_dodge(width=0.5), width=0,
                mapping=aes(ymin=effect_lo2, ymax=effect_hi2), linewidth=0.4) +
  geom_errorbar(position=position_dodge(width=0.5), width=0,
                mapping=aes(ymin=effect_lo, ymax=effect_hi), linewidth=1.2) +
  geom_point(position=position_dodge(width=0.5), pch=21, fill="white") +
  # Labels
  labs(x="Comparison", y="Effect size\n(log biomass ratio)") +
  # Legend
  scale_color_manual(name="Strength", values=c("darkgreen", "orchid4")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "top",
        legend.margin = margin(c(0,0,-5,0)))
g


# Export
ggsave(g, filename=file.path(plotdir, "Fig4_another_fig.png"),
       width=4.5, height=2.75, units="in", dpi=600)

