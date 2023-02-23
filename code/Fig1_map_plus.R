

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
codedir <- "code"
plotdir <- "figures"
datadir <- "data"

# Read helper functions
source(file.path(codedir, "my_summary_plot_functions.R"))

# Read data
load(file.path(datadir, "20230215_paper_data.RData"))


# Build and analyze data (David version)
################################################################################

# # XX.att.p.df = bayesian model lnRR results for each model
# #source(paste0(codedir,"compile_paper_data.R")) # to update with new data
# today.date <- gsub("-","",Sys.Date())

# market break point
mrkt.break <- 100000

# Organize covariate data from raw data (pred.mpa.data)
fish.dat <- pred.mpa.data %>%
  mutate(treat=ifelse(inside_aft_gis==1 & Zonetyp=="MU","MU",
                      ifelse(inside_aft_gis==1 & Zonetyp=="NT","NT","non-MPA")),
         treat.lab=as.factor(ifelse(treat=="MU","MU- multi-use",
                                    ifelse(treat=="NT","NT- no-take","non-MPA"))),
         mrkt.dist=mrkt.dist/1000, # convert to km
         shre.dist=shre.dist/1000, # convert to km
         hpop100=hpop100/1000000) %>%  # convert to million
  select(log.sum.biom,all.biom,treat,treat.lab,depth,mrkt.dist,shre.dist,hpop100,wave.exp,sstmean,sstmin,chlomean,reef15km,reef200km,lat,lat2,long,srvy.yr, mpa_age,mpa_km)


type_order <- c("No-take", "Multi-use", "Non-MPA")

#-- No covariates
# calculate lnRR as % difference
no.cov.att.p.df.pct <- no.cov.att.p.df %>%
  rename(lnRR=mean) %>%
  mutate(pct.diff=(exp(lnRR)*100)-100, # convert lnRR to %. Hopefully right
         estimand=factor(estimand, levels=c("NT:0","MU:0","NT:MU|NT","NT:MU|MU")))

#-- No covariates
p.nocov.att <- no.cov.att.p.df %>%
  filter(estimand%in%c("NT:0","MU:0")) %>%
  select(estimand:n.MPAs, lnRR=mean) %>%
  mutate(type=factor(ifelse(estimand=="NT:0","No-take","Multi-use"),
                     levels=c("No-take","Multi-use")))

# Biomass and percent differences
no.cov.att.p.df.pct %>% select(estimand,lnRR,`p>0`,pct.diff)

# NT:0 vs MU:0 (based on lnRR)
(diff.lnRR <- no.cov.att.p.df.pct$lnRR[no.cov.att.p.df.pct$estimand=="NT:0"]/
    no.cov.att.p.df.pct$lnRR[no.cov.att.p.df.pct$estimand=="MU:0"])
(exp(diff.lnRR)*100)-100

# NT:0 vs MU:0 (based on pct)
no.cov.att.p.df.pct$pct.diff[no.cov.att.p.df.pct$estimand=="NT:0"]/
  no.cov.att.p.df.pct$pct.diff[no.cov.att.p.df.pct$estimand=="MU:0"]

# calculate lnRR as % difference
mrkt.att.p.df.pct <- mrkt.att.p.df %>%
  rename(lnRR=mean,
         n=n.MPAs) %>%
  mutate(pct.diff=(exp(lnRR)*100)-100,
         estimand=factor(estimand, levels=c("NT:0","MU:0","NT:MU|NT","NT:MU|MU")),
         type=factor(rep(c( "Multi-use","No-take","NT:MU|MU","NT:MU|NT"), each = 2)))

# Biomass and percent differences
mrkt.att.p.df.pct %>% select(estimand, labs,lnRR,`p>0`,pct.diff) # high= near market

#-- Adequate staff capacity
cap.att.p.df2.pct <- cap.att.p.df %>%
  mutate(level=ifelse(labs=="low","inadequate","adequate"),
         level=factor(level, levels=c("inadequate","adequate")),
         estimand=factor(estimand, levels=c("NT:0","MU:0","NT:MU|NT","NT:MU|MU")),
         # for labels in previous figure, may not need with new figures
         estimand.lab= case_when(
           estimand=="NT:0" ~ "i) NT:0",
           estimand=="MU:0" ~ "ii) MU:0",
           estimand=="NT:MU|MU" ~ "iii) NT:MU|MU",
           TRUE ~ "iv) NT:MU|NT")) %>%
  rename(lnRR=mean,
         n=n.MPAs) %>%
  mutate(pct.diff=(exp(lnRR)*100)-100, # get percentage difference (correct?),
         estimand=factor(estimand, levels=c("NT:0","MU:0","NT:MU|NT","NT:MU|MU")),
         type=factor(rep(c( "Multi-use","No-take","NT:MU|MU","NT:MU|NT"), each = 2)))

# Biomass and percent differences
cap.att.p.df2.pct %>% select(estimand, labs,lnRR,`p>0`,pct.diff) # high =adequate

# difference between low and high capacity MU:0
(exp(cap.att.p.df2.pct$lnRR[cap.att.p.df2.pct$estimand=="MU:0" & cap.att.p.df2.pct$labs=="high"] -
       cap.att.p.df2.pct$lnRR[cap.att.p.df2.pct$estimand=="MU:0" & cap.att.p.df2.pct$labs=="low"]
)*100)-100

#-- Appropriate regulations
reg.att.p.df2.pct <- reg.att.p.df %>%
  mutate(level=ifelse(labs=="low","weak","strong"),
         level=factor(level, levels=c("weak","strong")),
         estimand=factor(estimand, levels=c("NT:0","MU:0","NT:MU|NT","NT:MU|MU")),
         # for labels in previous figure, may not need with new figures
         estimand.lab= case_when(
           estimand=="NT:0" ~ "i) NT:0",
           estimand=="MU:0" ~ "ii) MU:0",
           estimand=="NT:MU|MU" ~ "iii) NT:MU|MU",
           TRUE ~ "iv) NT:MU|NT"
         )) %>%
  rename(lnRR=mean,
         n=n.MPAs) %>%
  mutate(pct.diff=(exp(lnRR)*100)-100, # get percentage difference (correct?),
         estimand=factor(estimand, levels=c("NT:0","MU:0","NT:MU|NT","NT:MU|MU")),
         type=factor(rep(c( "Multi-use","No-take","NT:MU|MU","NT:MU|NT"), each = 2)))

# Biomass and percent differences
reg.att.p.df2.pct %>% select(estimand, labs,lnRR,`p>0`,pct.diff) # high = appropriate regulations


# Format data (Chris version)
################################################################################

# Format sites
sites <- fish.dat %>%
  # Simplify
  select(treat, lat, long) %>%
  # Unique sites
  unique() %>%
  # Order treatment
  mutate(treat=recode_factor(treat,
                             "NT"="No-take (NT)",
                             "MU"="Multi-use (MU)",
                             "non-MPA"="Non-MPA"))

# Format effects
effects <- p.nocov.att %>%
  # Rename
  rename(lnrr02='2.5%',
         lnrr10='10%',
         lnrr50='50%',
         lnrr90='90%',
         lnrr98='97.5%',
         ppos="p>0") %>%
  # Build labels
  mutate(label_hi=paste0(ppos*100, "%"),
         label_lo=paste0(n.MPAs, "\n(", n.sites, ")"))


# Plot data
################################################################################

# Get world
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # read in world map from rnaturalearth as sf

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

# Plot map
g1 <- ggplot() +
  # Plot wold
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2) +
  # Plot points
  geom_point(data=sites, mapping=aes(x=long, y=lat, fill=treat), pch=21) +
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
        # legend.position = c(0.1, 0.2),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.margin = margin(-14, 0, 2, 0),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot effect size
g2 <- ggplot(effects, mapping=aes(x=type, y=lnRR, color=type)) +
  # Reference line
  geom_hline(yintercept = 0, color="grey60", linetype="dotted") +
  # Plot CI
  geom_errorbar(mapping=aes(x=type, ymin=lnrr02, ymax=lnrr98, color=type), width=0, size=0.5) +
  geom_errorbar(mapping=aes(x=type, ymin=lnrr10, ymax=lnrr90, color=type), width=0, size=1.5) +
  # Plot points
  geom_point(pch=21, fill="white", size=3) +
  # Text annotations
  geom_text(aes(x=type, y = lnrr02 - 0.06, label=label_lo), size=2.4, vjust=1) +
  geom_text(aes(x=type, y = lnrr98 + 0.06, label=label_hi), size=2.4, vjust=0) +
  # Labels
  labs(x="vs. non-MPA", y="Effect size\n(log biomass ratio)", tag="B") +
  # Limits
  scale_y_continuous(lim=c(-0.15, 0.8), breaks=seq(-0.4, 0.8, 0.2)) +
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

