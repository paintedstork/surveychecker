library(tidyverse)
library(stringi)
library(data.table)

first_time <<- TRUE
numsegments <- 10
weather_list <- c("SU", "WI", "MI", "CL", "CS", "RA")
habitat_list <- c("SH", "EG", "DE", "PL", "RI","SC", "GL", "HA", "WL")

downloadDir <- "C:\\Users\\91990\\Downloads\\"
SurveyDate1 <- "2022-12-03"
SurveyDate2 <- "2022-12-04"

# Test
#SurveyDate1 <- "2022-11-28"
#SurveyDate2 <- "2022-11-29"

Traveling <- "eBird - Traveling Count"

data_summary <- NULL
transect_summary <- NULL
camp_summary <- NULL

# This function fills one line per sample
fillUnsampledSegments <- function (data, unsampled)
{
  for (i in 1:nrow(unsampled))
  {
    index <- data [ (as.Date(data$Date) == as.Date(unsampled$Date[i])) &
                    (data$Transect == unsampled$Transect[i]) &
                    (data$Segment == unsampled$Segment[i]) &
                    (data$AMPM == unsampled$AMPM[i]),] %>% rownames() %>% as.numeric()
    print(paste(unsampled$Transect[i],unsampled$Segment[i]))
    if(index > 0)
    {
      if (is.na(data$List[index]))
      {
#        print(data$Transect[index])
        data$List[index] <- "SUnsampled"
      }
      else
      {
#        print(paste("Reported as unsampled", unsampled$TransectSegment,
#                                             unsampled$Date,
#                                             unsampled$AMPM,
#                                             unsampled$Lead,
#                                             data$List))
      }
    }
    else
    { # Nothing to do
      
    }
  }
  return (data)
}
  
# This function fills one line per sample
fillTransectSample <- function (data, tr_sg, date, am)
{
  tr <- strsplit(tr_sg,"_")[[1]][1]
  sg <- strsplit(tr_sg,"_")[[1]][2]
  
  quad <- str_sub(tr,str_length(tr),str_length(tr))
  grid <- str_sub(tr,1, str_length(tr) - 1)
  data_f <- data.frame (Transect = tr,
                        Segment = sg,
                        Date = date,
                        AMPM = ifelse(am, "AM","PM"),
                        List = NA,
                        Weather = "NS",
                        Habitat = "NS"
  )

  data <- data %>% 
              filter (Date == date, AM == am) 

  
  quad_c  <- c(quad, toupper(quad))
  sep_c  <- c(""," ","-",",")
  seg_c  <- c("","s","S")
  ter_c  <- c(" ", ",","\n")
  
  comb_grid <- expand.grid(grid,sep_c,quad_c,sep_c,seg_c,sg,ter_c,"|")
  combinations <- paste0 (
                            comb_grid$Var1,
                            comb_grid$Var2,
                            comb_grid$Var3,
                            comb_grid$Var4,
                            comb_grid$Var5,
                            comb_grid$Var6,
                            comb_grid$Var7,
                            comb_grid$Var8
  ) 
  combinations <- paste(combinations, sep = '', collapse = '')
  combinations <- paste0("(",
                            str_sub(combinations, 1, str_length(combinations)-1  -1),
                          ")")
  re <- paste0("^",combinations,"\\w*")
  if(first_time == TRUE){
    print(re)
    first_time <<- FALSE
  }
  data <- data %>% filter (str_detect (Checklist.Comments, re))
  
  if(nrow(data) == 1)
  {
    data_f$List     <- data$Submission.ID
    data_f$Weather  <- sapply (stri_extract_all_regex(data$Checklist.Comments, 
                                                     paste(weather_list, collapse = '|')), toString)
    data_f$Habitat  <- sapply (stri_extract_all_regex(data$Checklist.Comments, 
                                                     paste(habitat_list, collapse = '|')), toString)

  }
  else if (nrow(data) > 1)
  {
    print(paste("Duplicate entries for",data_f$Transect,
                data_f$Segment,
                data_f$Date,
                data_f$AM))
    print(data$Submission.ID)
  }
  else
  {
  }
  return (data_f)
}

# 
fillTransectSegment <- function (tr_sg, data)
{
  # Bind all four samples for a single transect segment
  data_summary <- rbind (
    fillTransectSample (data, tr_sg, SurveyDate1, TRUE),
    fillTransectSample (data, tr_sg, SurveyDate1, FALSE),
    fillTransectSample (data, tr_sg, SurveyDate2, TRUE),
    fillTransectSample (data, tr_sg, SurveyDate2, FALSE))
  
  return (data_summary)  
}


loadData <- function()
{
  transects <- read.csv("Transects.csv")
  unsampledsegments <- read.csv("UnsampledSegments.csv")
  
  df <- file.info(list.files(downloadDir, pattern = '.zip', full.names = T))
  filename <- rownames(df)[which.max(df$mtime)]
  
  print(filename)
  
  ret <- unzip(filename, list = FALSE, files ="MyEBirdData.csv", overwrite = TRUE)
  data <- read.csv("MyEBirdData.csv")

  data_t <- data %>% select("Submission.ID",
                            "Date",
                            "Time",
                            "Protocol",
                            "All.Obs.Reported",
                            "Checklist.Comments") %>%
                     distinct(Submission.ID, .keep_all = TRUE ) %>% 
                     filter (Date == SurveyDate1 | Date == SurveyDate2,
                               Protocol == Traveling, 
                               All.Obs.Reported == 1) %>% 
                               mutate (AM = ifelse (str_detect(Time, "AM"),
                                                  TRUE, FALSE))
#  data_t <- read.csv("sample.csv",sep=",")
  data_t <- data_t %>% select("Submission.ID",
                            "Date",
                            "Checklist.Comments",
                            "AM")
  
  # Generate all combinations of transect segments  
  tr_sg <- lapply(seq(1:10), function (i) paste0(transects$Transect,"_",i)) %>% 
              unlist()
  
  print("Generating data summaries")

  data_summary <- do.call (rbind, lapply (tr_sg, fillTransectSegment, data = data_t))

  data_summary <- fillUnsampledSegments(data_summary, unsampledsegments)
  print("Generating transect summaries")
  # Removes transects where we obtained all lists
  transect_summary <- data_summary %>% 
                          group_by (Transect, Date, AMPM) %>%
                          summarize (PendingLists = sum (is.na(List)), 
                                     NoHabitatLists = sum (Habitat == "NA"),
                                     NoWeatherLists = sum (Weather == "NA")) %>%
                          ungroup() %>% 
                          filter ((PendingLists >0) | 
                                   (NoHabitatLists > 0) |
                                   (NoWeatherLists > 0))
                    
  print(names(transect_summary))
  print(names(transects))
  
  transect_summary <- inner_join (transect_summary, transects)
  
  write.csv(transect_summary, "transect.csv")
  print("Generating camp summaries")
  camp_summary <- transect_summary %>% 
                        group_by (Camp) %>% 
                        summarize (PendingLists = sum(PendingLists),
                                   NoHabitatLists = sum(NoHabitatLists),
                                   NoWeatherLists = sum(NoWeatherLists)) %>%
                        ungroup()

  print(sum(!is.na(data_summary$List)))
  print(data_summary %>% filter (List == "SUnsampled") %>% nrow())
  
  print("Processing done")
  return (list (camp_summary, transect_summary, data_summary))
}
