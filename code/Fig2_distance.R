
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


# unprotected biomass by mrkt distance
dist.cut <- c(seq(0,100,25),200,500,1000,2500)

biom.dist <- fish.dat %>%
  filter(treat=="non-MPA") %>%
  mutate(dist_km_chr = cut_number(mrkt.dist, breaks=dist.cut, n=length(dist.cut)-2,
                                  labels=dist.cut[-1]),
         biomass_kg_100m2=all.biom/1000) %>%
  # script I developed to create summary table (in attached my_functions script)
  my_summarize(biomass_kg_100m2,dist_km_chr) %>%
  # Add group
  mutate(catg=ifelse(dist_km_chr %in% c("25", "50", "75"), "Near", "Far"))
biom.dist

near.cut <- which(biom.dist$dist_km_chr==100) # location of the 100km mark

#-- NT:MU|MU for all sites and disaggregated by distance
p.mrkt.att <- no.cov.att.p.df %>%
  filter(estimand%in%c("NT:MU|MU")) %>%
  bind_rows(mrkt.att.p.df %>%
              filter(estimand%in%c("NT:MU|MU")) ) %>%
  select(estimand:n.MPAs, lnRR=mean) %>%
  mutate(type=c("All", "Far", 'Near'),
         type=factor(type, levels=c("All", "Near", "Far")))


# Format data for plotting (Chris)
################################################################################

# Format data for plotting
effects <- p.mrkt.att %>%
  # Add labels
  mutate(label_hi=paste0(round(`p>0`*100), "%"),
         label_lo=paste0(n.MPAs, "\n(", n.sites, ")"))


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

# Labels
labs <- tibble(x=c("50", "500"),
               label=c("Near population center\n(high human pressure)",
                       "Far from population center\n(low human pressure)"),
               catg=c("Near", "Far"))

# Plot biomass ~ distance
g1 <- ggplot(biom.dist, aes(x=dist_km_chr, y=biomass_kg_100m2)) +
  # Shaded areas
  geom_rect(xmin = 0, xmax = 4,
            ymin = 0, ymax = 20,
            alpha = 0.02, fill = "darkblue") +
  geom_rect(xmin = 4, xmax = 8.5,
            ymin = 0, ymax = 20,
            alpha = 0.02, fill = "skyblue") +
  # Reference lines
  geom_hline(yintercept=5, color="grey60", linetype="dotted") +
  geom_vline(xintercept="100", color="grey60", linetype="dotted") +
  # Data
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), color="grey30", width=0) +
  geom_point() +
  # Plot text
  geom_text(data=labs, mapping=aes(x=x, y=18, label=label), size=2.4) +
  # Labels
  labs(x="Distance from population center (km)", y=expression("Biomass (kg/100m"^"2"*")"), tag="A") +
  scale_y_continuous(lim=c(0,NA)) +
  # Theme
  theme_bw() + base_theme
g1

# Plot biomass ~ distance
g1b <- ggplot(biom.dist, aes(x=dist_km_chr, y=biomass_kg_100m2, color=catg)) +
  # Reference lines
  geom_hline(yintercept=5, color="grey60", linetype="dotted") +
  geom_vline(xintercept="100", color="grey60", linetype="dotted") +
  # Data
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper),  width=0) +
  geom_point() +
  # Plot text
  geom_text(data=labs, mapping=aes(x=x, y=18, label=label, color=catg), size=2.4) +
  # Labels
  labs(x="Distance from population center (km)", y=expression("Biomass (kg/100m"^"2"*")"), tag="A") +
  scale_y_continuous(lim=c(0,NA)) +
  # Legend
  scale_color_manual(values=c("dodgerblue2", "navy")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1b


# Plot effect size ~ distance
g2 <- ggplot(effects, aes(x=type, y=lnRR, color=type)) +
  # Reference line
  geom_hline(yintercept=0, color="grey60", linetype="dotted") +
  # Plot CI
  geom_errorbar(mapping=aes(x=type, ymin=`2.5%` , ymax=`97.5%`), width=0, size=0.5) +
  geom_errorbar(mapping=aes(x=type, ymin=`10%`, ymax=`90%`), width=0, size=1.5) +
  # Plot points
  geom_point(pch=21, fill="white", size=3) +
  # Text annotations
  geom_text(aes(x=type, y = `2.5%` - 0.04, label=label_lo), size=2.2, vjust=1) +
  geom_text(aes(x=type, y = `97.5%` + 0.04, label=label_hi), size=2.2, vjust=0) +
  # Labels
  labs(x="Site type", y="Effect size\n(log biomass ratio)", tag="B") +
  scale_y_continuous(breaks=seq(-0.2, 0.4, 0.2), lim=c(-0.3, 0.45)) +
  # Legend
  scale_color_manual(name="Site type", values=c("black", "navy", "dodgerblue2")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
g2

# Merge
g <- gridExtra::grid.arrange(g1b, g2, nrow=1, widths=c(0.75, 0.25))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_distance.png"),
       width=6.5, height=2.5, units="in", dpi=600)
