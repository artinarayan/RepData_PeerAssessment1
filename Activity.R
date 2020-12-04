## Loading and preprocessing the data

      library(tidyverse)
      unzip('activity.zip')
      activity_df<-read.csv('activity.csv',header = TRUE)
      activity_df[[2]]<-as.Date(activity_df[[2]])
## What is mean total number of steps taken per day?

## 1. Number of steps per day
      steps_df<-activity_df %>%
            group_by(date) %>%
            summarize(StepsByDay=sum(steps))

## 2. Histogram of steps per day (continuous)

      ggplot(data=steps_df,aes(x=StepsByDay)) +  
            geom_histogram(boundary =0,binwidth=2500,col='black',fill='cyan') + 
            scale_x_continuous(breaks=seq(0,25000,2500))+
            scale_y_continuous(breaks=seq(0,18,2)) +
            labs(title = "Histogram of steps per day",
                  x = "Steps per day",
                  y = "Frequency") +
            theme(plot.title = element_text(hjust = 0.5))
## 3. Mean and Median of total number of steps      
      steps_df %>%
            summarize(Mean=mean(StepsByDay,na.rm = T),
                      Median=median(StepsByDay,na.rm = T))

## What is the average daily activity pattern?
## 1. Time series plot 
## 5-minute interval (x-axis),, 
## Avg # of steps taken, averaged across all days (y-axis)
      avg_steps_by_interval_df <- activity_df %>% 
            group_by(interval) %>%
            summarize(avg_steps=mean(steps, na.rm=T))
      
      ggplot(data=avg_steps_by_interval_df,aes(x=interval,y=avg_steps)) + 
            geom_line(color = "blue",size=1) +
            labs(title = "Average steps per time interval",
                 x = "Time Interval",
                 y = "Steps") +
            theme(plot.title = element_text(hjust = 0.5))
           
## 2. Which 5 minute interval, on avearge containst the maximum number of steps
      avg_steps_by_interval_df %>%
            filter(avg_steps ==max(avg_steps))
      
## Imputing missing values
      activity_df %>%
            summarise(missing_values = sum(is.na(steps)))
      
      activity_df<-activity_df %>% 
            group_by(interval) %>%
            mutate(cleaned_steps = ifelse(is.na(steps), mean(steps,na.rm=TRUE), steps))
            
      clean_activity_df<-activity_df %>%
            select(date,interval,cleaned_steps)
      
      clean_steps_df<-clean_activity_df %>%
            group_by(date) %>%
            summarize(CleanedStepsByDay=sum(cleaned_steps))
      
      ## 2. Histogram of steps per day (continuous)
      
      ggplot(data=clean_steps_df,aes(x=CleanedStepsByDay)) +  
            geom_histogram(boundary =0,binwidth=2500,col='black',fill='lightgreen') + 
            scale_x_continuous(breaks=seq(0,25000,2500))+
            scale_y_continuous(breaks=seq(0,26,2)) +
            labs(title = "Histogram of steps per day",
                 x = "Steps per day",
                 y = "Frequency") +
            theme(plot.title = element_text(hjust = 0.5))

      clean_steps_df %>%
            summarize(Mean=mean(CleanedStepsByDay,na.rm = T),
                      Median=median(CleanedStepsByDay,na.rm = T))
      
## Are there differences in activity patterns between weekdays and weekends?
      clean_activity_df<-clean_activity_df %>%
            mutate(weekday=weekdays(date)) %>%
            mutate(daytype=ifelse(weekday == "Saturday" | weekday == "Sunday", "Weekend", "Weekday"))
      
       
       
       week_clean_activity_df<-clean_activity_df %>%
             group_by(interval,daytype) %>%
             summarize(steps = mean(cleaned_steps,na.rm = T))
  
   
      ggplot(data=week_clean_activity_df,aes(x=interval,y=steps,color=daytype)) + 
            geom_line(show.legend = F) +
            facet_grid(daytype~.)+
            labs(title = "Average steps per time interval (Weekend Vs. Weekday)",
                 x = "Time Interval",
                 y = "Steps") +
            theme(plot.title = element_text(hjust = 0.5))
      
      