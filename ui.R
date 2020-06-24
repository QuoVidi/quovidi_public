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


ui <- miniPage(
  gadgetTitleBar(img(src='logo.png'),  
                 right = miniTitleBarButton("tasks", lang$task_button, 
                                            primary = T),
                 left = miniTitleBarButton("rules", lang$rules_button, 
                                           primary = T)),
  
  useShinyjs(),
  
  
  tags$head(tags$style(
    type="text/css",
    " img {max-width: 100%; height: auto}
    .tabbable {z-index : 999999}
    .nav-tabs {
      background-color: #006747;
    }"
  )),
  
  miniTabstripPanel(
    
    #----------------------------------------------------------------------------
    # SUBMIT PANEL 
    # This panel is for the submission of images
    #----------------------------------------------------------------------------
    
    miniTabPanel(lang$submit_title, icon = icon("image"),
                 miniContentPanel(
                   helpText(lang$help_submit),
                   tags$hr(),
                   fileInput("file", label = lang$file_label),
                   selectInput("submit_quest", label = lang$submit_quest_label, choices = c(1,2)),
                   selectInput("submit_zone", label = lang$submit_quest_label, choices = c(1,2)),
                   h3(lang$image_label),
                   imageOutput("submit_img", width="auto"),
                   h3("Localisation"),
                   leafletOutput("submit_map")
                   
                 ),
                 miniButtonBlock(actionButton("submit", label = lang$submit_label, icon("paper-plane"), 
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 border = "top")
    ),
    
    
    #----------------------------------------------------------------------------
    # BOUNTY
    # In this panel, users can see their own bounties, submitted or not
    # together with their evaluation status. Users can also see the ranking of all the groups
    #----------------------------------------------------------------------------
    
    miniTabPanel(lang$bounty_title, icon = icon("star outline"),
                 miniContentPanel(
                   helpText(lang$help_bounty),
                   tags$hr(),
                   materialSwitch(inputId = "show_all_bounties",
                                  label = lang$show_all_group_label,
                                  status = "success", right = TRUE, value = FALSE),
                   conditionalPanel(
                     condition = "input.show_all_bounties == true",
                     tags$hr(),
                     htmlOutput("summary_all_bounties"),
                     tags$hr(),
                     dataTableOutput("all_bounties")
                   ),
                   conditionalPanel(
                     condition = "input.show_all_bounties == false",
                     tags$hr(),
                     htmlOutput("summary_group_bounties"),
                     tags$hr(),
                     dataTableOutput("group_bounties"), 
                     tags$hr(), 
                     htmlOutput("summary_image_bounty"),
                     imageOutput("bounty_img", width="auto") # Display the selected image
                     
                   )
                 )
    ),
    
    #----------------------------------------------------------------------------
    # PEER REVIEW
    # In this panel, users can correct quests from the other groups, hence 
    # having point swithout havoing to go outside. 
    #----------------------------------------------------------------------------
    
    miniTabPanel("Review", icon = icon("users outline"),
                 miniContentPanel(
                   fluidRow(
                     column(3,
                            wellPanel(
                              tags$h3(lang$peer_title),
                              helpText(lang$peer_text)
                            )
                     ),
                     column(5,
                            # buttons are disabled by defaukt. Thy will be enabled when enough pictures are
                            # available for correction
                            actionButton("button_correct_peer", lang$peer_correct, 
                                         icon = icon("check"),
                                         style="color: #fff; background-color: #65ac54; border-color: #65ac54; "),
                            tags$hr(),
                            actionButton("button_false_peer", lang$peer_false, 
                                         icon = icon("remove"),
                                         style="color: #fff; background-color: #d83429; border-color: #d83429"),
                            tags$hr(),
                            actionButton("button_unknown_peer", lang$peer_next, 
                                         icon = icon("chevron-right")),
                            tags$hr(),
                            htmlOutput("img_title_peer"),
                            imageOutput("myImage_peer", width="100%")

                     ),
                     column(4,
                            plotOutput('peer_review_plot'),
                            plotOutput('peer_review_pass')
                     )
                   )
                   
                   
                 )
    ),
    
    #----------------------------------------------------------------------------
    # QUESTS /  ADMIN PANEL
    # This panel changes if the users is a "player" or a "master"
    # Player : list of all quests and zones
    # Master : admin panel (dashboard, table view, table update, correction interface and game settings)
    #----------------------------------------------------------------------------
    
    miniTabPanel(lang$quest_title, icon = icon("list alternate outline"),
                 miniContentPanel(
                   
                   #--------------------------------
                   # QUESTS PANEL, if the user is a player
                   #--------------------------------
                   conditionalPanel(
                     condition = "output.role == 1",
                     materialSwitch(inputId = "show_zones",
                                    label = lang$show_zones_label,
                                    status = "success", right = TRUE, value = FALSE),
                     tags$hr(),
                     conditionalPanel(
                       condition = "input.show_zones == false",
                       tags$hr(),
                       downloadButton("downloadQuests", lang$download_quests_label),       
                       tags$hr(),
                       dataTableOutput("all_quests")
                     ),
                     conditionalPanel(
                       condition = "input.show_zones == true",
                       leafletOutput("zones_map")
                     )
                   ),
                   
                   # ADMIN PANEL, if the user is a master of a GOD
                   conditionalPanel(
                     condition = "output.role <= 0",
                     selectInput("select_view", label = "Choose view", 
                                 choices = c("Dashboard" = 0,
                                             "Correction interface" = 1,
                                             "View DB tables" = 2,
                                             "Update DB table" = 3,
                                             "Game parameters" = 4, 
                                             "Reset user password" = 5,
                                             "Map of bounties" = 7,
                                             "Recompute group scores" = 8)),
                     # ),
                     tags$hr(),
                     
                     # BOUNTIES MAP
                     # View all the bounties o a map
                     conditionalPanel(
                       condition = "input.select_view == 7",
                       selectInput("map_types", "Bounties type", 
                                   all_types,multiple = T, selected = all_types),
                       selectInput("map_status", "Bounties status", 
                                   c("stored" = 0, 
                                     "submitted" = 1),
                                   multiple = T, selected = 1),
                       selectInput("map_correction", "Bounties corrections", 
                                   c("to correct"=0, 
                                     "false"=-1, 
                                     "correct"=1, 
                                     "not visible"=-2,
                                     "to keep"=4,
                                     "to check"=-4,
                                     "whatthefuck"=-3),
                                   multiple = T, selected = c(0, -1, 1, -2, 4, 3)),
                       leafletOutput("bounties_map", height = "500px")
                     ),
                     
                     
                     # RECOMPUTE GROUP SCORES
                     # Add the id of an user that cannot connect to GSP data
                     conditionalPanel(
                       condition = "input.select_view == 8",
                       helpText("Recompute the scores for all the groups"),
                       miniButtonBlock(actionButton("recompute_group", label = "Recompute", icon("cogs"),
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       border = "top")
                     ),
                     
                     
                     # RESET USER PASSWORD
                     # Small panel to reset user password
                     conditionalPanel(
                       condition = "input.select_view == 5",
                       textInput("noma_input", "NOMA"),
                       miniButtonBlock(actionButton("reset_password", label = "Reset password", icon("cogs"), 
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       border = "top")
                     ),
                     
                     
                     
                     # DASHBOARD PANEL
                     # This is a panel showing synthetic data about the users 
                     conditionalPanel(
                       condition = "input.select_view == 0",
                       box(
                         # tags$h3("Stored quests"),
                         htmlOutput("stored_quests_text"),
                         plotOutput("dashQuestsStored"),
                         width = 4
                       ),
                       
                       box(
                         # tags$h3("Submitted quests"),
                         htmlOutput("submitted_quests_text"),
                         plotOutput("dashQuests"),
                         width = 4
                       ),
                       
                       box(
                         tags$h3("Corrections status"),
                         plotOutput("dashCorrections"),
                         width = 4
                       ) , 
                       
                       box(
                         htmlOutput("bounties_text"),
                         plotOutput("dashSubmissions"),
                         width = 4
                       ), 
                       
                       box(
                         htmlOutput("connections_text"),
                         plotOutput("dashConnections"),
                         width = 4
                       ),
                       box(
                         htmlOutput("connections_text_users"),
                         plotOutput("dashConnectionsUsers"),
                         width = 4
                       )
                       
                     ),
                     
                     
                     # GAME PARAMETERS
                     # update the parameters of the game
                     conditionalPanel(
                       condition = "input.select_view == 4",
                       helpText("Max number of quests each group can submit during the game"),
                       sliderInput("req_bounties", "Max number of quests", 0, 200, req_bounties),
                       helpText("Max number of quests each group can collect during the game"),
                       sliderInput("req_bounties_corr", "Max number of quests", 0, 200, req_bounties_corr),
                       helpText("Maximum number of out-of-zone bounties allowed for each group"),
                       sliderInput("req_out_zones", "Maximum out-of-zone bounties ", 0,req_bounties , req_bounties),
                       helpText("Maximum number of time a single quest can be found. If a quest reach this limit, it will be listed as inactive"),
                       sliderInput("max_found_quest", "Maximum bounties for a quest ", 0, 200, max_found_quest),
                       
                       miniButtonBlock(actionButton("update_params", label = "Update parameters", icon("cogs"), 
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       border = "top")
                       
                     ),
                     
                     
                     # VIEW DATA
                     # View the data contained in the different tables of the database
                     conditionalPanel(
                       condition = "input.select_view == 2",
                       selectInput("select_table", label = "Choose table", choices = all_tables),
                       downloadButton("download_table_data", "Download"),
                       downloadButton("download_all_table_data", "Download all database"),
                       tags$hr(),
                       dataTableOutput("table_data")
                       
                     ),
                     
                     # UPLOAD INTERFACE
                     # Allow the user (if GOD), to upload new data to a datatable 
                     # The cana can also be from a backup file
                     conditionalPanel(
                       condition = "input.select_view == 3",
                       materialSwitch(inputId = "from_backup_file",
                                      label = "Update from backup file",
                                      status = "success", right = TRUE, value = FALSE),
                       conditionalPanel(
                         condition = "!input.from_backup_file",
                         fileInput("input_file", label = "Choose file to upload")
                       ),
                       conditionalPanel(
                         condition = "input.from_backup_file",
                         selectInput("select_table_bu", label = "Choose backup table", choices = c(1,2))
                       ),
                       
                       tags$hr(),
                       
                       dataTableOutput("input_table_data"),
                       selectInput("select_table_1", label = "Choose table to update", choices = all_tables),
                       selectInput("append_data", label = "Choose action", 
                                   choices = c("Override existing data" = 1,
                                               "Append data" = 2)),
                       tags$hr(),
                       miniButtonBlock(actionButton("update_table", label = "Update table", icon("database"), 
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       border = "top")
                       
                     ),
                     
                     
                     # CORRECTION INTERFACE
                     # Allow the masters to correct the bounties in the database.
                     # The bounties can be filter by type, status, etc.
                     conditionalPanel(
                       condition = "input.select_view == 1",
                       fluidRow(
                         column(4,
                                wellPanel(
                                  selectInput("correction_type", "Which quest type(s) to display?", unique(all_quests$type),
                                              multiple = T, selected = unique(all_quests$type)),
                                  
                                  selectInput("correction_quest", "Choose quest", "wait"),
                                  selectInput("correction_val", "Which quest validation type(s) to display?", 
                                              c("to correct"=0, 
                                                "false"=-1, 
                                                "correct"=1, 
                                                "not visible"=-2,
                                                "to keep"=4,
                                                "to check"=-4,
                                                "whatthefuck"=-3),
                                              multiple = T, selected = 0),
                                  
                                  tags$hr(),
                                  htmlOutput("to_do"),
                                  htmlOutput("done")
                                )       
                         ),
                         column(4,
                                actionButton("button_correct", "Correct", icon = icon("check"), 
                                             style="color: #fff; background-color: #65ac54; border-color: #65ac54; "),
                                actionButton("button_tocheck", "To keep", icon = icon("thumbs-o-up"),
                                             style="color: #fff; background-color: #65ac54; border-color: #65ac54; "),
                                actionButton("button_false", "False", icon = icon("remove"), 
                                             style="color: #fff; background-color: #d83429; border-color: #d83429"),
                                actionButton("button_flou", "Not visible", icon = icon("eye-slash"), 
                                             style="color: #fff; background-color: #e57c21; border-color: #e57c21"),
                                actionButton("button_marrant", "WTF???", icon = icon("rocket"), 
                                             style="color: #fff; background-color: #f260e0; border-color: #f260e0"),
                                
                                
                                actionButton("button_unknown", "NEXT", icon = icon("chevron-right")),
                                
                                tags$hr(),
                                htmlOutput("img_title"),
                                imageOutput("myImage", width="100%"),   
                                #uiOutput('myImage'),
                                tags$hr()
                                
                         ),
                         column(4, 
                                tags$h3("Correct images (previously corrected)"),
                                sliderInput("n_correct_img", "Number of images to show", 1, 20, 5),
                                uiOutput('knowledge_base')
                         )
                       )
                     )
                   )
                 )
    )
    
  )
  
)