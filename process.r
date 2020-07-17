library(rjson)
library(tidyverse)
library(lubridate)

JSON <- fromJSON(file = "ReformattedData.json")
channels <- vector("list", length(JSON$Channels))
for (i in 1:length(channels)) {
  channels[[i]] <-  bind_rows(JSON$Channels[[i]]$Messages)
  channels[[i]]$TimeSent <- as.POSIXct(channels[[i]]$TimeSent, format="%Y-%m-%dT%H:%M:%S")
  names(channels)[i] <- JSON$Channels[[i]]$Name
  channels[[i]] <- channels[[i]] %>%
    mutate(channel_name = JSON$Channels[[i]]$Name)
}
people <- bind_rows(JSON$People)
people$ID <- as.factor(people$ID)
attachments <- bind_rows(JSON$Attachments)
reactions <- bind_rows(JSON$Reactions)
messages <- bind_rows(channels)
messages$channel_name <- as.factor(messages$channel_name)
messages$Author <- as.factor(messages$Author)