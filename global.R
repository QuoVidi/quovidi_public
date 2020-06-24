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

rm(list = ls(all.names = TRUE))

# LOAD ALL REQUIRED LIBRARIES
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(miniUI)
library(leaflet)
library(OpenImageR)
library(stringi)
library(exifr)
library(DBI)
library(RMySQL)
library(DT)
library(data.table)
library(formattable)
library(tidyverse)
library(shinydashboard)
library(digest)
library(lubridate)
library(sp)
library(cowplot)
source('src/lightbox.R')
source('src/photoswipe.R')
library(dplyr)
library(plyr)
library(RSQLite)
library(pander)


# Get the tables from the database
print("Game initializing")

dbcon <- function(local){
  con <- dbConnect(RSQLite::SQLite(), "www/database.sql")
  return(con)
}

isLocal <- F


print("Connected to database")
con <- dbcon(isLocal)
all_tables        <- c("bounties", "groups", "log", 
                       "planning", "quests", "users", 
                       "zones", "zones_delim", "params")
all_quests        <- dbGetQuery(con, "SELECT * FROM quests")
all_zones_delim   <- dbGetQuery(con, "SELECT * FROM zones_delim")
all_zones         <- dbGetQuery(con, "SELECT * FROM zones")
all_groups        <- dbGetQuery(con, "SELECT * FROM groups")
all_users         <- dbGetQuery(con, "SELECT * FROM users")
all_logs          <- dbGetQuery(con, "SELECT * FROM log")
all_params        <- dbGetQuery(con, "SELECT * FROM params")
all_bounties      <- dbGetQuery(con, "SELECT * FROM bounties")
planning          <- dbGetQuery(con, "SELECT * FROM planning")

all_types          <- unique(all_quests$type)

# GAME PARAMETERS. CAN BE CHANGE IN THE INTERFACE
lang_id <- all_params$value[all_params$param == "lang"]
max_found_quest <- as.numeric(all_params$value[all_params$param == "max_found_quest"]) # min number of zones to visit
req_bounties <- as.numeric(all_params$value[all_params$param == "req_bounties"]) # min number of zones to visit
req_bounties_corr <- as.numeric(all_params$value[all_params$param == "req_bounties_corr"]) 
req_out_zones <- as.numeric(all_params$value[all_params$param == "req_out_zones"]) 


# Download the language tables
lang  <- dbGetQuery(con, paste0("SELECT * FROM language WHERE lang ='",lang_id,"'")) %>% 
  filter(!is.na(id)) %>% 
  spread(id, item)

dbDisconnect(con)

print("Tables loaded")


# Create a list with all the zones 
zone_list         <- split(all_zones$id, all_zones$name)

# Create a list with all the tables structures 
# This is used when uplodaing data to a table, to check
# consistency in format
tables_str        <- list(all_bounties = paste0(colnames(all_bounties),collapse = "-"),
                         all_groups = paste0(colnames(all_groups),collapse = "-"),
                         log = paste0(colnames(all_logs),collapse = "-"),
                         planning = paste0(colnames(planning),collapse = "-"),
                         all_quests = paste0(colnames(all_quests),collapse = "-"),
                         all_users = paste0(colnames(all_users),collapse = "-"),
                         all_zones = paste0(colnames(all_zones),collapse = "-"),
                         all_zones_delim = paste0(colnames(all_zones_delim),collapse = "-"),
                         all_params = paste0(colnames(all_params),collapse = "-")
                       )






# OTHERS
# Color scale for dashboard plots
mycols <- c( "#CD534CFF", "#EFC000FF", "#0073C2FF", "#65ac54")

help_bounty <- "Here you can see all your bounties, submitted or not, as well as their current evaluation status. You can also see here the ranking of all the groups. "


