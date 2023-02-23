# Functions
sum2=function(x){sum(x,na.rm=TRUE)}
mean2=function(x){mean(x,na.rm=TRUE)}
median2=function(x){median(x,na.rm=TRUE)}
sd2 <- function(x){sd(x,na.rm=TRUE)}
min2 <- function(x){min(x,na.rm=TRUE)}
max2 <- function(x){max(x,na.rm=TRUE)}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
scale2 <- function(x) (x - mean2(x)) / sd2(x)

pd <- ggplot2::position_dodge(width=.3) # move them .05 to the left and right

# for violin plots
give.sample.size <- function(x) {
  return(c(y = min(x)-(max(x)-min(x))/10, label = length(x)))}

#warn.supress <- function(w) if( any( grepl("NaNs produced", w) ) ) invokeRestart( "muffleWarning" )
#suppressMessages(message("In qt(0.975, df = n - 1) : NaNs produced"))

# modification of group_by & summarise to produce a range of summary statistics (replaces Rmisc:aggregate)
# my_summarize(data,var,group1,group2,...)
my_summarize <-  function (.data,x,...) {
  # needed to use variable names in function
  group_var <- quos(...)
  enquo_x <- enquo(x)  # measurement variable
  mean_name <- paste0(quo_name(enquo_x)) # renaming variables
  # group and summarize
  .data %>%
    # filter(!is.na(UQ(enquo_x))) %>%
    group_by(!!!group_var) %>%
    summarise(m= mean2(UQ(enquo_x)), med=median2(UQ(enquo_x)),mode=Mode(UQ(enquo_x)),
              sd= sd2(UQ(enquo_x)),n=n(),n_NAs=sum(is.na(UQ(enquo_x))), se=sd/sqrt(sum(!is.na(UQ(enquo_x)))),
              min=min2(UQ(enquo_x)),max=max2(UQ(enquo_x)), qu_1st=quantile(UQ(enquo_x),0.25,na.rm=T), qu_3rd=quantile(UQ(enquo_x),0.75,na.rm=T),
              ci=1.96*se,se_lower=m-se,se_upper=m+se,ci_lower=m-ci,ci_upper=m+ci)  %>%
    rename(!!mean_name:=m)
}

# Alternate CI: ci=qt(0.95,df=n-1)*se


# Bar plot with standard error bars
# my_barplot(data,'x','y')
my_barplot_se <- function(.data,x,y){
  ggplot(.data,aes_string(x=x,y=y,label='n')) +
    geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
    geom_line( position = pd) +
    geom_errorbar(aes(ymin=se_upper, ymax=se_lower), width=0.2, position = pd ) +
    geom_text(aes( y = ci_lower - 0.05),position = position_dodge(0.9),vjust = 0, size=3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

}

# my_barplot_ci(data,'x','y') # Bar plot with 95% confidence intervals
my_barplot_ci<- function(.data,x,y){
  ggplot(.data,aes_string(x=x,y=y,label='n')) +
    geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
    geom_line( position = pd) +
    geom_errorbar(aes(ymin=ci_upper, ymax=ci_lower), width=0.2, position = pd ) +
    geom_text(aes( y = ci_lower - 0.05),position = position_dodge(0.9),vjust = 0, size=3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
}

# my_pointplot_se(.data,'x','y','group.var') # plot with points and standard errors
my_pointplot_se<- function(.data,x,y,group.var){
  ggplot(.data,aes_string(x=x,y=y,group=group.var,label='n')) +
    geom_errorbar(aes(ymin=se_lower, ymax=se_upper), width=0, position = pd ) +
    geom_line(position =pd) +
    geom_point(size=2, position =pd)+ theme_bw() +
    geom_text(aes( y = ci_lower - 0.05),position = position_dodge(0.9),vjust = 0, size=3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
}

# my_pointplot_ci(.data,'x','y','group.var') # plot with points and 95% confidence intervals
my_pointplot_ci<- function(.data,x,y,group.var){
  ggplot(.data,aes_string(x=x,y=y,group=group.var,label='n')) +
    geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=0, position = pd ) +
    geom_text(aes( y = ci_lower - 0.05),position = position_dodge(0.9),vjust = 0, size=3) +
    geom_line(position =pd) +
    geom_point(size=2, position =pd)+ theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
}

# my_scatterplot_ci(.data,'x','y','group.var') # plot with points and 95% confidence intervals
my_scatterplot_ci<- function(.data,x,y){
  ggplot(.data,aes_string(x=x,y=y)) +
    geom_point() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
}
