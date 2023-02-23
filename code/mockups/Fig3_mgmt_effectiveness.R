
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
data1_orig <- readxl::read_excel("data/fig4_data.xlsx", 1)
data2_orig <- readxl::read_excel("data/fig4_data.xlsx", 2)

# Data1 order
data_ord <- data2_orig %>%
  filter(level=="low") %>%
  arrange(desc(effect))

# Format data
data2 <- data2_orig %>%
  mutate(level=recode_factor(level,
                             "low"="Low",
                             "high"="High")) %>%
  mutate(variable=factor(variable, levels=data_ord$variable))

# Plot data
################################################################################

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
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot regression
g1 <- ggplot(data1_orig, aes(x=mgmt_score, y=effect)) +
  # Reference line
  geom_hline(yintercept=0, color="grey60", linetype="dotted") +
  # Regression
  geom_smooth(formula = 'y ~ x', method="lm", fill="grey50", color="black") +
  # Data
  geom_point() +
  # Labels
  labs(x="Management effectiveness\n", y="Effect size\n(log biomass ratio)", tag="A") +
  scale_x_continuous(breaks=seq(10,22, 2)) +
  # Theme
  theme_bw() + base_theme
g1

# Plot effect sizes
g2 <- ggplot(data2, aes(y=variable, x=effect, color=level)) +
  # Reference line
  geom_vline(xintercept=0, color="grey60", linetype="dotted") +
  # Data
  geom_errorbar(mapping=aes(xmin=effect-0.1, xmax=effect+0.1),
                width=0, position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5)) +
  # Legend
  scale_color_manual(name="Strength", values=c("darkgreen", "orchid4")) +
  # Labels
  labs(x="Effect size\n(log biomass ratio)", y="", tag="B") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.margin = margin(c(0,0,-5,0)),
        panel.grid.major.y = element_line(color="grey70"))
g2


# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.5, 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_mgmt_effectiveness.png"),
       width=6.5, height=3.5, units="in", dpi=600)

