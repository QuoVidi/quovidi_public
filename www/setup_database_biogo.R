# Copyright © 2019, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2019 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


rm(list = ls())

# REQURIED LIBRARIES 
library(RSQLite)
library(DBI)
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rjson)


#--------------------------------------------------
# SETUP DATABASE

con <- dbConnect(RSQLite::SQLite(), "www/database.sql")

# NAME OF THE GOOGLE SHEET WITH THE GAME DATA
# REPLACE WITH YOUR OWN SHEET HERE
SHEET_NAME <- "1-AH4w4k7kyIRvzisJZOQ1xnTVK7OgfjcWmYVXTaDrCc" 

#--------------------------------------------------
# LANGUAGE TABLE
# Get the list of quests from google sheet

lang <- read_sheet(SHEET_NAME, sheet = "language") 
dbWriteTable(con, "language", lang, overwrite = TRUE)



#--------------------------------------------------
# QUEST TABLE
# Get the list of quests from google sheet

  quests <- read_sheet(SHEET_NAME, sheet = "quests") 
  dbWriteTable(con, "quests", quests, overwrite = TRUE)
  
#--------------------------------------------------  
# ZONES TABLE
# Get the list of zones form the JSON delim files
  
  zones_raw <- fromJSON(file = "www/zones.json")[[2]]
  zones_delim <- NULL
  zones <-  read_sheet(SHEET_NAME, sheet = "zones")
  
  for( i in c(1:length(zones_raw))){
    temp <- data.frame(matrix(unlist(zones_raw[[i]][[3]][2]), ncol=2, byrow=T))
    max_id <- 0
    if(!is.null(zones_delim)) max_id <- max(zones_delim$id)
    zones_delim <- rbind(zones_delim, 
                         data.frame(id = c((max_id + 1) : (max_id + nrow(temp))),
                                    group=i, 
                                    latitude=temp$X2, 
                                    longitude = temp$X1, 
                                    zone_id = zones$id[i]))
  }
  dbWriteTable(con, "zones", zones, overwrite = TRUE)
  dbWriteTable(con, "zones_delim", zones_delim, overwrite = TRUE)
  
#--------------------------------------------------
# GROUPS TABLE
# Get the list of groups from google sheet
  
  groups <- read_sheet(SHEET_NAME, sheet = "groups") 
  dbWriteTable(con, "groups", groups, overwrite = TRUE)

#--------------------------------------------------
# CORRECTION TABLE
# Get the list of groups from google sheet

corr <- read_sheet(SHEET_NAME, sheet = "corrections") 
dbWriteTable(con, "corrections", corr, overwrite = TRUE)
  
  
#--------------------------------------------------
# USER TABLE
# Get the list of users from google sheet
  
  users <- read_sheet(SHEET_NAME, sheet = "users") 
  dbWriteTable(con, "users", users, overwrite = T) 
  
#--------------------------------------------------
# BOUNTIES TABLE
# Get the list of bounties from google sheet
  
  bounties <- read_sheet(SHEET_NAME, sheet = "bounties")  
  dbWriteTable(con, "bounties", bounties, overwrite = T) 
  
#--------------------------------------------------
# PLANNING TABLE
# Get the planning of the game
  
  planning <- read_sheet(SHEET_NAME, sheet = "planning") 
  dbWriteTable(con, "planning", planning, overwrite = TRUE)
  

#--------------------------------------------------
# LOG TABLE
# get user logging history
  
  log <- read_sheet(SHEET_NAME, sheet = "log")  
  dbWriteTable(con, "log", log, overwrite = TRUE)
  
  #--------------------------------------------------
  # PARAM TABLE
  # get user logging history
  
  params <- read_sheet(SHEET_NAME, sheet = "params") 
  dbWriteTable(con, "params", params, overwrite = TRUE)
  