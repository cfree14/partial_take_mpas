
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
  my_summarize(biomass_kg_100m2,dist_km_chr)
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

# get mean intercept
alpha.lnRR <- no.cov.att.p.df$mean[no.cov.att.p.df$var=="alpha" & no.cov.att.p.df$estimand=="NT:MU|MU"]

# New facet label names for supp variable
supp.labs <- sort(names(select(munt.mod.dat,DA_INCL:CAP_BDGT,DEVOL)))
names(supp.labs) <- c("Budget capacity", "Staff capacity", "Inclusive decison-making","Devolution","Active monitoring","Enforcement capacity",
                      "Management plan","Clear boundaries","Legally gazetted","Appropriate regulations")
supp.labs

# re.mpa= model MPA level lnRR results from bayesian model
# adding MPA specific intercepts as they represent deviations from mean intercept (not 100% sure about this one, might have been adjusted already)
re.mpa <- re.mpa %>%
  mutate(lnRR.orig=lnRR,
         lnRR = lnRR+alpha.lnRR)
summary(re.mpa)

#join data, summarize to MPA level (79 MPAs)
mpa.dat <- munt.mod.dat %>% # model input data: munt.match.out.cov
  mutate(near.mrkt=ifelse(mrkt.dist<=mrkt.break,1,0)) %>%   # mrkt.break <- 100000
  group_by(ECOID,MPAID,near.mrkt) %>%  # average to site level
  summarise(raw.lnRR=mean(tau.hat.adj)) %>%
  group_by(MPAID,near.mrkt) %>%  # average to MPA level
  summarise(raw.lnRR=mean(raw.lnRR), n.sites=n()) %>%
  mutate(estimand="NT:MU|MU") %>%
  left_join(re.mpa,by="MPAID") %>%
  left_join(mgmt.data,by="MPAID") %>%  # management effectiveness data
  mutate(mgt.sum=rowSums(across(DA_INCL:CAP_BDGT), na.rm=TRUE)) # total management scores

# subset to only focus on MPAs close to shore (47 MPAs)
near.dat <- filter(mpa.dat,near.mrkt==1)
sum(near.dat$n.sites[!is.na(near.dat$Year.of.assessment)]) # number of sites
nrow(near.dat[!is.na(near.dat$Year.of.assessment),]) # number of MPAs

# --- spot check to see if this looks right, comparing raw lnRR to modelled MPA-level intercepts
summary(select(mpa.dat,lnRR,raw.lnRR))
ggplot(mpa.dat,aes(raw.lnRR, lnRR)) +
  geom_point(shape = 16, position = pd) +
  geom_smooth(method = "lm", se = T)

# plots
data1_orig <- near.dat %>% # model input data: munt.match.out.cov
  filter(mgt.sum>0) %>%
  rename(effect=lnRR) %>%
  arrange(desc(effect))

# Threshold version
data2.orig <- near.dat %>%
  select(-(SurveyYear:DEVOL)) %>%
  pivot_longer(cols = DEVOL2:DA_INCL2,
               names_to = "var",
               values_to = "level",
               values_drop_na = TRUE)
data2 <- my_summarize(data2.orig,lnRR,var,level) %>%
  filter(!is.na(level) & level!="NA") %>%
  rename(effect=lnRR) %>%
  mutate(lab=paste0(level,"\n(",n,")"),
         variable=factor(var, levels=paste0(supp.labs,2), labels = names(supp.labs)))  # add labels
head(data2)


# Format data for plotting (Chris)
################################################################################

# Format data
data_orig <- cap.att.p.df %>%
  mutate(category="Staff capacity") %>%
  bind_rows(reg.att.p.df %>%
              mutate(category="Sustainable use regulations")) %>%
  filter(estimand%in%c("NT:0","MU:0","NT:MU|MU")) %>%
  select(estimand:n.MPAs,category, effect=mean) %>%
  mutate(level=recode_factor(labs,
                             "low"="Inadequate / weak",
                             "high"="Adequate / strong"),
         comparison=recode_factor(estimand,
                                  "MU:0"="Multi-use\nvs. non-MPA",
                                  "NT:0"="No-take\nvs. non-MPA",
                                  "NT:MU|MU"="Multi-use\nvs. no-take"))


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

# Plot effects
g <- ggplot(data_orig, aes(x=comparison, y=effect, color=level)) +
  facet_wrap(~category) +
  # Reference line
  geom_hline(yintercept=0, color="grey60", linetype="dotted") +
  # Data
  geom_errorbar(position=position_dodge(width=0.5), width=0,
                mapping=aes(ymin=`2.5%` , ymax=`97.5%`), size=0.4) +
  geom_errorbar(position=position_dodge(width=0.5), width=0,
                mapping=aes(ymin=`10%`, ymax=`90%`), size=1.2) +
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
ggsave(g, filename=file.path(plotdir, "Fig4_capacity_regulations.png"),
       width=4.5, height=2.75, units="in", dpi=600)
