library(tidyverse)
library(lubridate)

getwd()
setwd("M:/Customer Analytics/Fadi/1- Working File Backup/3- Call Center Reporting/History/2019_01")
setwd("M:/Customer Analytics/Fadi/1- Working File Backup/3- Call Center Reporting/Target Optical/Return Files")

filenames = dir(pattern="*.csv")

invisible(lapply(filenames, function(i){
	temp <- read.csv(i, colClasses = "character")
	temp_clean <- temp %>%
		mutate(RUN_DATE = as.character(date(parse_date_time(RUN_DATE, orders = c('mdy HMS', 'ymd HMS')))),
					 DispDateTime = as.character(parse_date_time(DispDateTime, orders = c('ymd HMS')))
		) %>%
		replace_na(list(RUN_DATE = "", DispDateTime = ""))
	write.csv(temp_clean, i)
})
)
