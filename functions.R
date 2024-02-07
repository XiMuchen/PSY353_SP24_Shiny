intro_text <- "You can use this website to view your personalized data report based
on the ESM data we collected during the last two weeks. \n 
Step 1: Enter your user key to the text field below and click on 'Create Report'. \n
Step 2: In the navigation bar at the top." 

missing_text_2<-" of the surveys we sent you 6 times per day for a full week. 
Below you can see which surveys you have completed (blue) and which 
you have missed (grey). Of the 42 surveys we sent you,
you completed" 

# all_poss <- 1:(85 * 4) # Define all possible values for surveys
miss_data<- # Create a data table with missing data placeholders
  as.data.frame(cbind(
    date = rep(c("26-Jan-2024", "27-Jan-2024","28-Jan-2024","29-Jan-2024","30-Jan-2024","31-Jan-2024","01-Feb-2024"), each = 6),
    time = rep(c("10:00", "12:30", "15:00", "17:30", "20:00", "22:30"), 7),
    date_c = rep(c(1:7), each = 6), 
    time_c = rep(c(1:6),7), 
    dt_c = seq(1,42,1),
    timeline_finished = NA
  )) %>%
  unite(dat_time, date,time, sep = " ")

# Function to create a dataframe with missing data indicators
create_df_missing <- function(miss_dat=miss_data, par_dat) {
  miss_dat$timeline_finished <- miss_data$dat_time %in% par_dat$SCHEDULED_TS
  return(miss_dat)
}

# create_df_missing <-function(df_long, all_possible=all_poss,
#                              missing_data=miss_data){ 
#   # Get unique and sorted values of completed surveys
#   finished<-df_long$counter_old %>% unique() %>% sort 
#   # Create a vector indicating which surveys have been missed/completed
#   missing_data$timeline_finished <- all_possible %in%  finished
#   return(missing_data)
# }

# Function to plot missing data as a heatmap
plot_missing_data <- function(missing_dat){
  ggplot(missing_dat, aes(x = date_c,
                          y = rev(time_c),
                          fill = as.factor(timeline_finished))) +
    geom_tile(color = "white",
              lwd = 1,
              linetype = 1) +
    scale_fill_manual(values = c("#D3D3D3", "#0464A3"),
                      labels = c("Missed", "Finished")) +
    scale_x_discrete(
      labels = c("26-Jan-2024", "27-Jan-2024","28-Jan-2024","29-Jan-2024","30-Jan-2024","31-Jan-2024","01-Feb-2024")
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_blank(),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
    ) +
    scale_y_discrete(
      labels = c("22:30", "20:00", "17:30", "15:00", "12:30", "10:00") # Set the limits in reverse order
    ) +
    labs(y = "Time of Day", x = "Date", fill = "Status") +
    guides(fill = guide_legend(reverse = TRUE))
}


# Function to plot BFI change across time points 
plot_PxTime <- function(plot_data, Trait, Color){ 
  ggplot(plot_data, aes(x=SCHEDULED_TS,y=plot_data[[Trait]], group = 1)) +
    geom_point(color = Color, size =3) + 
    geom_line(color = Color, size =1) +
    scale_x_discrete(limits = miss_data$dat_time) + 
    scale_y_continuous(limits = c(1,7), 
                       breaks = seq(1,7, by=1)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size= 10),
          axis.text.y = element_text(size= 10),
          axis.title = element_text(size = 14), 
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) + 
    labs(x = "Time of Measurements", y = paste(Trait, "State"), title = paste("Your changes in", Trait, "states across time"))
}


# Function to plot distribution
plot_TraitDist <- function(plot_data) {
  plot_data %>%
    pivot_longer(cols = c("Extraversion", "Conscientiousness", "Agreeableness","Neuroticism", "Openness"), names_to = "Trait", values_to = "Value") %>% 
    ggplot(aes(x=Value,y=Trait,color=Trait, fill = Trait)) + 
    stat_slab(alpha=0.5) + 
    stat_pointinterval(position = position_dodge(width = 0, preserve = "single")) +
    scale_x_continuous(limits = c(1,7), 
                       breaks = seq(1,7, by=1)) + 
    scale_y_discrete(limits = c("Openness","Neuroticism", "Agreeableness", "Conscientiousness", "Extraversion"))+
    theme_classic() + 
    labs(title = "Density Distribution for Personality States", y= "Traits", x="Ratings")+ 
    theme(axis.text.x = element_text(size= 10),
          axis.text.y = element_text(size= 10), 
          axis.title = element_text(size = 14), 
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "none")
}

# Function to plot distribution when interacting vs not
plot_IntDist <- function(plot_data, trait) {
  plot_data %>%
    ggplot(aes(x=plot_data[[trait]], color = as.factor(INTERACTING),  fill = as.factor(INTERACTING))) + 
    stat_slab(alpha=0.5) + 
    stat_pointinterval(color = "black", position = position_dodge(width = 0, preserve = "single")) +
    scale_x_continuous(limits = c(1,7), 
                       breaks = seq(1,7, by=1)) + 
    scale_y_continuous(breaks = NULL) + 
    labs(title = paste("Density Distribution for", trait, "\nDuring Social Interaction vs. Not"), fill = "INTERACTING", x=trait) +
    theme_classic() +
    theme(axis.text.x = element_text(size= 10),
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    scale_fill_discrete(labels = c("No","Yes")) + guides(fill = guide_legend(reverse = TRUE)) +
    scale_color_discrete(guide = "none")
} 

# Function to plot distribution in activities
plot_ActDist <- function(plot_data, trait) {
  act_vec <- c("Class/Schoolwork", "Paid/Volunteer Work", "Using Social Media", "Watching TV", "Videogames", "Reading the News", 
               "Eating", "Exercising", "Hobby", "Errands", "Driving/Commuting", "Resting/Relaxing", "Socializing", "Spiritual")
  
  dat_new <- data.frame(Activities = character(), trait = numeric())
  
  for (i in act_vec){
    part_dat <- plot_data %>% 
      filter(.[[i]]==1) %>% select(c(i, trait)) %>%
      mutate(Activities = i) %>% select("Activities", trait) 
    dat_new <- rbind(dat_new,part_dat)
  }
  
  dat_new %>% ggplot(aes(x = dat_new[[trait]], y = Activities, fill = Activities)) + 
    stat_halfeye() +
    scale_x_continuous(limits = c(1,7), 
                       breaks = seq(1,7, by=1)) + 
    labs(title = paste0("Density Distribution for ", trait, "\nin Different Activities"), fill = "Activities", 
         x = trait, y = "Activities") +
    theme_classic() +
    theme(axis.text.x = element_text(size= 10),
          axis.text.y = element_text(size= 10),
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
}

# Function to plot distribution for interacting with different partner
plot_PartDist <- function(plot_data, trait) {
  part_vec <- c("Friend", "Romantic Partner", "Roommate", "Family member", "Coworker", "Customer", "Casual Aquaintance", "Stranger", "Other")
  
  dat_new <- data.frame(Partners = character(), trait = numeric())
  
  for (i in part_vec){
    part_dat <- plot_data %>% 
      filter(.[[i]]==1) %>% select(c(i, trait)) %>%
      mutate(Partners = i) %>% select("Partners", trait) 
    dat_new <- rbind(dat_new,part_dat)
  }
  
  dat_new %>% ggplot(aes(x = dat_new[[trait]], y = Partners, fill = Partners)) + 
    stat_halfeye() +
    scale_x_continuous(limits = c(1,7), 
                       breaks = seq(1,7, by=1)) + 
    labs(title = paste0("Density Distribution for ", trait, "\nWhen Interacting with Different Partners"), fill = "Partners", 
         x = trait, y = "Partners") +
    theme_classic() +
    theme(axis.text.x = element_text(size= 10),
          axis.text.y = element_text(size= 10),
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
}
