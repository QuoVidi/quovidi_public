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

library(shiny)

shinyServer(function(input, output, clientData, session) {
  
  
  
  
  rs <- reactiveValues(current_user = 1,
                       current_role = "master",
                       current_group = 1,
                       current_group_name = NULL,
                       current_group_info = NULL,
                       current_user_name = NULL,
                       resized_img = NULL,
                       current_img_name  = NULL,
                       current_img_gps = NULL,
                       all_bounties = all_bounties,
                       active_bounties = NULL,
                       update = 1,
                       input_file = NULL, 
                       to_correct = NULL, 
                       current_img = NULL,
                       update_correction = 1,
                       to_correct_current = NULL,
                       correction_done = NULL,
                       current_img_peer = NULL,
                       update_correction_peer = 1)
  
  passw <- "test"
  usrname <- "test"
  # observe(modalLogin()) # If this is commented, not login is required to access the app
  
  #--------------------------------------------------
  # UPDATE THE UI WITH CONTENT FROM THE DATABASE
  #--------------------------------------------------
  observe({
    req(input$submit_img)
    req(rs$update)
    
    con = dbcon(isLocal)
    
    # Get the name of the current group of the user, based on the loggin information
    rs$current_group_name <- all_groups$name[all_groups$id == rs$current_group] 
    
    # Get the name of the current user, based on the loggin information
    rs$current_user_name <- all_users$name[all_users$id == rs$current_user] 
    
    # Get all the active boutines, from the current group
    rs$active_bounties <- dbReadTable(con, "bounties") %>%  
      filter(id_group == rs$current_group, submitted == 0) 
    dbDisconnect(con)
  })
  
  
  
  
  
  #--------------------------------------------------------------------------------
  #----------------------  LOGIN FUNCTIONS ----------------------------------------
  #--------------------------------------------------------------------------------
  
  
  #--------------------------------------------------
  # LOGIN WITH MODAL DIALOGS
  # Modals are used to manage the users login. 
  # Not sure this is the best option, but it works
  #--------------------------------------------------
  
  
  # This is the basic modal login. it appears when the app is open
  # Disapears when the Action button is clicked. Then the login and password
  # are checked. If they are not OK, this modal is re-openned.
  modalLogin <- function(){
    showModal(modalDialog(
      textInput("username", lang$username,
                placeholder = 'sacha'
      ),
      passwordInput("password", lang$password,
                    placeholder = 'pikachu'
      ),
      footer = tagList(
        actionButton("ok", "OK")
      )
    ))
  }
  
  # Failed login modal dialog. Appears if the password and login check is not successful. 
  modalFailed <- function(){
    showModal(modalDialog(
      tags$b(lang$wrong_username),
      footer = tagList(
        actionButton("ok_2", "OK")
      )
    ))
  }
  
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    con = dbcon(isLocal)
    user <- dbReadTable(con, "users") %>%  filter(username == input$username)
    dbDisconnect(con)
    
    if(nrow(user) == 0){
      removeModal()
      sendSweetAlert(session, title = lang$wrong_username, 
                     text = NULL, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      modalLogin()
    }else if (digest(input$password, algo = c("sha256")) == user$password) { # TODO
      removeModal()
      rs$current_user = user$id
      rs$current_user_name <- user$name
      rs$current_group = user$group_id
      rs$current_role = user$role
      con = dbcon(isLocal)
      rs$current_group_info <- dbReadTable(con, "groups") %>% filter(id == user$group_id)
      dbDisconnect(con)
      
      # If this is the first time the user logs in, then 
      # the user needs to reset the password. 
      if(user$login == 0){
        statement <- paste0("UPDATE users ",
                            "SET login = 1 ",
                            "WHERE username = '",input$username,"'")
        
        con = dbcon(isLocal)
        dbSendStatement(con, statement)
        dbDisconnect(con)
        
        modalPassword()
      } 
      
      
      
      # Record the user loggin in the log table
      temp  <- data.frame(datetime = as.character(now()), 
                          username = input$username, 
                          user_id = rs$current_user)
      con = dbcon(isLocal)
      row.names(temp) <- max(as.numeric(dbReadTable(con, "log")$row_names))+1
      dbWriteTable(con, "log", temp, append = TRUE)   
      dbDisconnect(con)
      
      
    }else{
      # IF loggin is incorrect, then send Failed modal message
      removeModal()
      sendSweetAlert(session, title = lang$wrong_username, 
                     text = NULL, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      modalLogin()
    }
    
  })
  
  
  # Change the password when login for the first time 
  modalPassword <- function(){
    showModal(modalDialog(
      helpText(lang$first_log),
      textInput("current_noma", "NOMA",
                placeholder = "noma"
      ),
      passwordInput("new_password", lang$new_password,
                    placeholder = "password"
      ),
      footer = tagList(
        actionButton("psw_change", "OK")
      )
    ))
  }
  
  
  # Update the password of the user
  observeEvent(input$psw_change, {
    
    if(input$current_noma != rs$current_user){
      sendSweetAlert(session, title = lang$noma_incorrect, 
                     text = NULL, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      modalPassword()
    }else{
      
      
      statement <- paste0("UPDATE users ",
                          "SET password = '",digest(input$new_password, algo = c("sha256")),"' ",
                          "WHERE id = '",input$current_noma,"'")
      con = dbcon(isLocal)
      dbSendStatement(con, statement)
      dbDisconnect(con)
      
      removeModal()
      sendSweetAlert(session, title = lang$password_changed, 
                     text = NULL, type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
    
  })
  
  # Set the current user's role, for access from the UI
  # This is needed as the UI changes depending on the user's role
  output$role <- reactive({
    req(rs$current_role)
    switch(rs$current_role,
           "player" = 1,
           "master" = 0, 
           "god" = -1
    )
  })
  
  outputOptions(output, "role", suspendWhenHidden = FALSE)
  
  
  
  #--------------------------------------------------
  # CREATE A MODAL WITH CURRENT TASKS
  # This modal can be triggered by the users
  # It shows the remaining tasks to finish the game. 
  # These tasks are defined by the game parameters. 
  #--------------------------------------------------
  
  observeEvent(input$tasks, {
    
    con = dbcon(isLocal)
    
    # Get the group data
    n_out <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group & id_zone == 0) %>% 
      nrow() 
    temp_g <- dbReadTable(con, "groups")   %>%  
      filter(id == rs$current_group)# 
    dbDisconnect(con)
    
    mess <- lang$task_message
    mess <- gsub("rep1", as.character(rs$current_user_name), mess)
    mess <- gsub("rep2", as.character((input$req_bounties - temp_g$n_quests)), mess)
    mess <- gsub("rep3", as.character(temp_g$total_points_th), mess)
    mess <- gsub("rep4", as.character(temp_g$total_points), mess)
    
    showModal(modalDialog(
      title = paste0(lang$team_progess,rs$current_group_name),
      HTML(mess),
      easyClose = TRUE,
      footer = tagList(
        actionButton("group_name", lang$change_group, icon=icon("cogs")),
        modalButton(lang$dismiss)
      )
    ))
  })
  
  
  
  
  
  #--------------------------------------------------
  # CREATE A MODAL TO CHANGE GROUP NAME
  # Allows the user to change the name of the group
  # A name is assigned to the group by default
  #--------------------------------------------------
  
  observeEvent(input$group_name, {
    removeModal()
    modalGroup()
  })
  modalGroup <- function(){
    showModal(modalDialog(
      textInput("new_group_name", lang$new_group_name,
                placeholder = rs$current_group_name
      ),
      footer = tagList(
        actionButton("group_change", "OK")
      )
    ))
  }
  # Update the name of the group
  observeEvent(input$group_change, {
    con = dbcon(isLocal)
    groups <- dbGetQuery(con, "SELECT * FROM groups")
    dbDisconnect(con)
    
    if(input$new_group_name %in% groups$name){
      print("error")
      removeModal()
      sendSweetAlert(session, title = lang$name_taken_title, 
                     text = lang$name_taken_text, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      print("changing")
      
      statement <- paste0("UPDATE groups ",
                          "SET name = '",input$new_group_name,"' ",
                          "WHERE id = '",rs$current_group,"'")
      con = dbcon(isLocal)
      dbSendStatement(con, statement)
      dbDisconnect(con)
      rs$current_group_name = input$new_group_name
      removeModal()
      sendSweetAlert(session, title = lang$name_changed, 
                     text = paste0(lang$new_group_name,input$new_group_name), 
                     type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
    
  })
  
  
  
  
  #--------------------------------------------------
  # CREATE A MODAL WITH THE RULES
  # Can be triggered by the users. 
  # Displays the rules of the game. 
  #--------------------------------------------------
  
  observeEvent(input$rules, {
    # Check that data object exists and is data frame.
    showModal(modalDialog(
      title = NULL,
      includeMarkdown("www/rules.md"),
      easyClose = TRUE
    ))
  })
  
  
  
  #--------------------------------------------------------------------------------
  #----------------------  SUBMIT PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  
  #--------------------------------------------------
  # UPDATE THE UI WITH CONTENT FROM THE DATABASE
  #--------------------------------------------------
  observe({
    req(rs$update)
    
    # Get all the active quests
    con = dbcon(isLocal)
    active_quests <- dbReadTable(con, "quests") %>%  filter(found < input$max_found_quest)
    dbDisconnect(con)
    
    quest_list <- split(active_quests$id, active_quests$name)
    
    # Update the STORE panel
    updateSelectInput(session, "submit_quest", choices = quest_list, selected=1)
    updateSelectInput(session, "submit_zone", choices = zone_list, selected=1)
    
  })
  
  
  #--------------------------------------------------
  # LOAD THE IMAGE
  # The image is loaded, resize, then stored in the 
  # dedicated forlder for later use. 
  #--------------------------------------------------
  observe({
    req(input$file)
    
    inFile <- input$file
    if(is.null(inFile)) return(NULL)
    
    f <- gsub("JPG", "jpg", inFile$datapath)
    f <- gsub("JPEG", "jpg", f)
    f <- gsub("jpeg", "jpg", f)
    file.rename(inFile$datapath, f)
    
    tryCatch({
      # GET THE IMAGE
      img <- readImage(f)
      new_height <- round(dim(img)[2] / dim(img)[1] * 500)
      if(dim(img)[1] < dim(img)[2]){
        img <- resizeImage(image = img, width=500, height = new_height)
      }else{
        img <- resizeImage(image = img, height=500, width = new_height)
      }
      # the image is assigned a random id. That id is stored. 
      rs$current_img_name <- paste0(stri_rand_strings(1, 15), ".jpg")
      writeImage(img, paste0("www/temp/", rs$current_img_name))
      
      # GET THE EXIF
      temp <- read_exif(f)
      dat <- data.frame(image=rs$current_img_name,
                        longitude=0,
                        latitude=0,
                        date="-")
      
      if(!is.null(temp$GPSLongitude)) dat$longitude <- temp$GPSLongitude
      if(!is.null(temp$GPSLatitude)) dat$latitude <- temp$GPSLatitude
      if(!is.null(temp$DateTimeOriginal)) dat$date <- temp$DateTimeOriginal
      rs$current_img_gps <- dat
      
      # FIND THE DEFAULT ZONE (by finding if the photo is within a predefined zone)
      updateSelectInput(session, "submit_zone", choices = zone_list, selected=0)
      for(z in unique(all_zones$id)){
        if(z != 0){
          temp <- all_zones_delim %>% filter(zone_id == z)
          if(point.in.polygon(dat$latitude, dat$longitude, temp$latitude, temp$longitude) == 1){
            updateSelectInput(session, "submit_zone", choices = zone_list, selected=z)
          }
        }
      }
    }, error=function(cond) {
      rs$current_img_name <- NULL
      sendSweetAlert(session, title = lang$error_image, 
                     text = lang$error_image_text, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      return(NULL)
    })
  })
  
  #--------------------------------------------------
  # DISPLAY THE IMAGE IN THE STORE PANEL
  #--------------------------------------------------
  output$submit_img <- renderImage({
    req(rs$current_img_name)
    filename <- normalizePath(file.path('./www/temp/', rs$current_img_name))
    
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image number", rs$current_img_name))
  }, deleteFile = F)
  
  
  #--------------------------------------------------
  # DISPLAY THE MAP OF THE CURRENT IMAGE
  #--------------------------------------------------
  output$submit_map <- renderLeaflet({
    
    req(rs$current_img_gps)
    
    current_zone <- all_zones_delim %>% 
      filter(zone_id == input$submit_zone)
    zone_to_plot <- merge(current_zone, all_zones, by.x = "zone_id", by.y = "id")
    
    if(!is.null(rs$current_img_gps)){
      map <-  leaflet(rs$current_img_gps) %>%
        addTiles() %>%
        addMarkers(~ longitude, ~ latitude,
                   options = popupOptions(closeButton = FALSE),
                   clusterOptions = markerClusterOptions()) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      for(i in unique(zone_to_plot$group)){
        temp <- zone_to_plot %>% filter(group == i)
        map <- map %>% addPolygons(lat = temp$latitude, lng = temp$longitude, 
                                   opacity = 0.0, fillOpacity = 0.02,
                                   color = temp$color, 
                                   popup = paste0("<b>",temp$name,"</b><hr>Zone ",temp$zone_id),
                                   popupOptions(closeButton = F)
        )
      }
    }else{
      map <-  leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      for(i in unique(zone_to_plot$group)){
        temp <- zone_to_plot %>% filter(group == i)
        map <- map %>% addPolygons(lat = temp$latitude, lng = temp$longitude, 
                                   opacity = 0.0, fillOpacity = 0.02,
                                   color = temp$color, 
                                   popup = paste0("<b>",temp$name,"</b><hr>Zone ",temp$zone_id),
                                   popupOptions(closeButton = F)
        )
      }
    }
    map
    
    
    
  })
  
  
  
  #-------------------------------------------------
  # SUBMIT THE BOUNTY
  # Submit the current image in the database. The image
  # will be evaluated by the game masters
  #-------------------------------------------------
  
  observeEvent(input$submit, {
    # Check that data object exists and is data frame.
    req(rs$current_img_name)
    
    con = dbcon(isLocal)
    temp <- data.frame(id = max(dbReadTable(con, "bounties")$id)+1, 
                       id_quest = input$submit_quest,
                       id_group = rs$current_group,
                       id_zone = input$submit_zone,
                       code = rs$current_img_name,
                       name = "image.jpg",
                       date = rs$current_img_gps$date,
                       longitude = rs$current_img_gps$longitude,
                       latitude = rs$current_img_gps$latitude,
                       submitted = 1,
                       validated = 0,
                       date_submitted = NA,
                       date_stored = as.character(today()),
                       date_validated = NA, 
                       stored_by = rs$current_user,
                       submitted_by = NA,
                       validated_by = NA)
    
      active_bounties <- dbReadTable(con, "bounties") %>%  
        filter(id_group == rs$current_group) 
      rs$update = -rs$update

      if(input$submit_quest %in% active_bounties$id_quest){
        # Check that data object exists and is data frame.
        sendSweetAlert(session, title = lang$bounty_already_title, 
                       text = lang$bounty_already_text,
                       type = "error",btn_labels = "Ok", html = FALSE, 
                       closeOnClickOutside = TRUE)
        
      }else if (rs$current_group_info$n_quest >= input$req_bounties | 
                rs$current_group_info$total_quest >= input$req_bounties_corr){
        # Check if the total number of quest is below the threshold
        sendSweetAlert(session, title = lang$no_more_quests_title, 
                       text = lang$no_more_quests_text,
                       type = "error", btn_labels = "Ok", html = FALSE, 
                       closeOnClickOutside = TRUE)
        
      } else{
        # Update the database
        row.names(temp) <- max(as.numeric(dbReadTable(con, "bounties")$row_names))+1
        dbWriteTable(con, "bounties", temp, append = TRUE)   
        
        # Store the image 
        img <- readImage(normalizePath(file.path('./www/temp/', rs$current_img_name)))
        file.remove(normalizePath(file.path('./www/temp/', rs$current_img_name)))
        writeImage(img, paste0("www/img/", rs$current_img_name))
        
        # update the group table with the synthetic informations
        current_quest <- dbReadTable(con, "quests") %>% 
          filter(id == input$submit_quest)
        
        temp_g <- dbReadTable(con, "groups")   %>%  
          filter(id == rs$current_group)
        inc_zone <- 0
        if(!input$submit_zone %in% active_bounties$id_zone) inc_zone <- 1 

        statement <- paste0("UPDATE groups ",
                            "SET n_zones = ",(temp_g$n_zones + inc_zone),", ",
                            "n_quests = ",(temp_g$n_quests + 1),", ",
                            "total_quests = ",(temp_g$total_quests + 1),", ",
                            "total_points_th = ",(temp_g$total_points_th + current_quest$point),", ",
                            "total_points = ",(temp_g$total_points_act)," ",
                            "WHERE id = ",rs$current_group)  
        dbSendStatement(con, statement)
        sendSweetAlert(session, title = lang$bounty_submitted, text = NULL, type = "success",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)        
      }
      dbDisconnect(con)

  })
  
  
  #--------------------------------------------------------------------------------
  #----------------------  QUESTS PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  #-----------------------------------
  # DISPLAY A TABLE WITH ALL QUESTS
  # this table simply display all the
  # quests from the game
  #-----------------------------------
  output$all_quests = renderDataTable({
    
    con = dbcon(isLocal)
    
    # Check wether this user wants to see the 
    # active or inactive quests
    
    temp <- dbReadTable(con, "quests") %>% 
      filter(found < input$max_found_quest) %>% 
      select(id, type, group, name, points, found)
    dbDisconnect(con)
    
    dt <- as.datatable(formattable(temp, list(
      points = color_tile("#e5f5f9", "#2ca25f"),
      found = color_tile("#e5f5f9", "#2ca25f")
    )),
    rownames = F,
    options = list(pageLength = 50))
    
    dt
  })
  
  
  #-------------------------------------------------
  # DOWNLOAD THE QUEST LIST FOR OFFLINE USE
  #-------------------------------------------------
  
  output$downloadQuests <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), "_all_quests.pdf")      
    },
    
    content <- function(file) {
      file.copy("www/quests.pdf", file)
    },
    contentType = "application/pdf"
  )
  
  
  #-------------------------------------------------
  # DISPLAY A MAP WITH THE DIFFERENT ZONES
  # Show the active zones in the game
  #-------------------------------------------------
  output$zones_map <- renderLeaflet({
    
    to_plot <- merge(all_zones_delim, all_zones, by.x = "zone_id", by.y = "id")
    
    map <-  leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    for(i in unique(to_plot$group)){
      temp <- to_plot %>% filter(group == i)
      map <- map %>% addPolygons(lat = temp$latitude, lng = temp$longitude, 
                                 opacity = 0.0, fillOpacity = 0.02,
                                 color = temp$color, 
                                 popup = paste0("<b>",temp$name,"</b><hr>Zone ",temp$zone_id),
                                 popupOptions(closeButton = F)
      )
    }
    map
    
  })
  
  
  #--------------------------------------------------------------------------------
  #----------------------  BOUNTY PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  
  #----------------------------------------------
  # DISPLAY A TABLE WITH ALL GROUPS POINTS
  # This table diplays the points of all the 
  # different groupes
  #----------------------------------------------
  
  output$all_bounties = renderDataTable({
    req(rs$update)
    
    con = dbcon(isLocal)
    
    temp <-  dbReadTable(con, "groups") %>% 
      mutate(group = name, bounties = n_quests, theoretical_points = total_points_th, real_points = total_points) %>% 
      select(group, bounties, theoretical_points, real_points) %>% 
      arrange(desc(theoretical_points))
    
    dbDisconnect(con)
    
    dt <- as.datatable(formattable(temp, list(
      real_points = color_tile("#e5f5f9", "#2ca25f")
    )),
    rownames = F,
    options = list(pageLength = 50))
    
    dt
  })
  
  
  #-------------------------------------------
  # DISPLAY A SUMMARY WITH THE GROUP BOUNTIES
  # This text informs the user about synthetic
  # information regarding his/her bounties
  #-------------------------------------------
  
  output$summary_group_bounties <- renderUI({
    con = dbcon(isLocal)
    n_quests <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group) %>% 
      nrow()
    n_quests_val <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group & validated > 0) %>% 
      nrow()
    
    temp <- dbReadTable(con, "groups")   %>%  
      filter(id == rs$current_group)
    
    text <- lang$group_points
    
    text <- gsub("rep2", as.character(temp$n_quests), text)
    text <- gsub("rep3", as.character(temp$total_points_th), text)
    text <- gsub("rep4", as.character(n_quests_val), text)
    text <- gsub("rep5", as.character(temp$total_points_act), text)
    
    HTML(text)
  })
  
  #-------------------------------------------
  # DISPLAY A TABLE WITH THE GROUP BOUNTIES
  # This table displays the bounties stored by
  # by the user. 
  #-------------------------------------------
  output$group_bounties = renderDataTable({
    req(rs$update)
    req(rs$update_correction)
    con = dbcon(isLocal)
    
    temp1 <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group) %>% 
      select(id_quest, submitted, validated)
    dbDisconnect(con)
    
    temp2 <- all_quests %>% 
      select(id, type, group, name, points)
    
    temp <- merge(temp1, temp2, by.x = "id_quest", by.y = "id") %>% 
      mutate(sub = submitted,
             val = validated) %>% 
      select(id_quest, type, name, group, points, sub, val)
    
    dt <- as.datatable(formattable(temp, list(
      points = color_tile("#e5f5f9", "#2ca25f"),
      sub = formatter("span", 
                      style = x ~ style(color = ifelse(x == 1, "green", "red")),
                      x ~ icontext(ifelse(x == 1, "ok", "remove"))),
      val = formatter("span", 
                      style = x ~ style(color = ifelse(x >=1, "green", 
                                                       ifelse(x == 0, "orange", "red"))),
                      x ~ icontext(ifelse(x >= 1, "ok", 
                                          ifelse(x == 0, "time", "remove"))))#,
    )),
    rownames = F,
    options = list(pageLength = 50), selection=list(mode="single"))
    
    dt
  })
  
  #---------------------------------------------
  # DISPLAY THE IMAGE IN THE BOUNTY PANEL
  # Display the image of the quest selected in 
  # the 'group_bounties' table
  #---------------------------------------------
  output$bounty_img <- renderImage({
    req(input$group_bounties_rows_selected)
    con = dbcon(isLocal)
    
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group) %>% 
      arrange(id_quest)
    dbDisconnect(con)
    
    temp <- temp[input$group_bounties_rows_selected, ] # Get the select row only
    
    filename <- normalizePath(file.path('./www/img/', temp$code))
    
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image name", temp$code))
  }, deleteFile = F)
  
  #---------------------------------------------
  # DISPLAY INFORMATION AbOUT IMAGE IN THE BOUNTY PANEL
  # Display information about the image of the quest selected in 
  # the 'group_bounties' table
  #---------------------------------------------
  output$summary_image_bounty <- renderUI({
    req(input$group_bounties_rows_selected)
    con = dbcon(isLocal)
    
    temp <- dbReadTable(con, "bounties")   %>%  
      filter(id_group == rs$current_group) %>% 
      arrange(id_quest)
    dbDisconnect(con)
    temp <- temp[input$group_bounties_rows_selected, ] # Get the select row only
    
    status <- lang$not_submitted
    if(temp$submitted == 1 & temp$validated == 0) status <- lang$img_waiting
    if(temp$submitted == 1 & temp$validated == 1) status <- lang$img_correct
    if(temp$submitted == 1 & temp$validated == 4) status <- lang$img_great
    if(temp$submitted == 1 & temp$validated == -1) status <- lang$img_incorrect
    if(temp$submitted == 1 & temp$validated == -2) status <- lang$img_invisible
    if(temp$submitted == 1 & temp$validated == -3) status <- lang$img_wtf
    
    text <- paste0("Image status : <b>", status,"</b>")
    
    HTML(text)  
  })
  
  
  
  
  #--------------------------------------------------------------------------------
  #----------------------  PEER REVIEW PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
  
  # Load the data already corrected in this session
  observe({
    req(rs$current_group)
    req(rs$update_correction_peer)
    if(is.null(rs$current_group)) return(NULL)
    
    # check if there is an uncorrected correction in the group
    # database
    con = dbcon(isLocal)
    current <- dbReadTable(con, "corrections") %>%
      filter(id_group == rs$current_group) %>%
      filter(answer == -4)
    dbDisconnect(con)
    
    if(nrow(current) > 0){
      # If there is one, then it is the one to be corrected
      con = dbcon(isLocal)
      rs$current_img_peer <- dbReadTable(con, "bounties") %>%
        filter(code == current$code)
      dbDisconnect(con)
      
    }else{
      
      con = dbcon(isLocal)
      done <- dbReadTable(con, "corrections") %>%
        filter(id_group == rs$current_group) %>%
        filter(answer != -2)
      dbDisconnect(con)
      
      if(nrow(done) < 100){
        con = dbcon(isLocal)
        temp <- dbReadTable(con, "bounties") %>%
          filter(validated %in% c(1, -1, 4)) %>%
          filter(!id_quest %in% done$id_quest) %>% # pick a quest that was not done already
          filter(!id_group %in% rs$current_group) # pick a quest from a different group
        
        # Sample the same number of right and wrong images. Then sample one of them
        # this is done to insure to have the same probability to have wrong and
        # right images in the batch
        wrong <- temp %>% filter(validated < 0)
        right <- temp %>% filter(validated > 0) %>% sample_n(nrow(wrong))

        if(nrow(wrong) > 20){
          shinyjs::enable("button_false_peer")
          shinyjs::enable("button_correct_peer")
          shinyjs::enable("button_unknown_peer")
          rs$current_img_peer <- rbind(right, wrong) %>% sample_n(1)
          # rs$current_img_peer <- rbind(right) %>% sample_n(1)
          
          # Send the data in the database, to keep track of the current correction
          # If the user disconnect itself from the interface, the quest uncorrected will
          # be displayed again.
          n_corrs <- nrow(dbReadTable(con, "corrections"))
          temp <- data.frame(id = (n_corrs + 1),
                             id_group = rs$current_group,
                             id_quest = rs$current_img_peer$id_quest,
                             code = rs$current_img_peer$code,
                             answer = -4,
                             point = 0,
                             date = as.character(today()))
          dbWriteTable(con, "corrections", temp, append = TRUE)
        }else{
          shinyjs::disable("button_false_peer")
          shinyjs::disable("button_correct_peer")
          shinyjs::disable("button_unknown_peer")
        }
        dbDisconnect(con)
        
      }else{
        rs$current_img_peer <- NULL
      }
    }
    
  })
  
  # Function to use when the quest is incorrect
  peer_correction <- function(answer, point){
    con = dbcon(isLocal)
    temp_g <- dbReadTable(con, "groups") %>%
      filter(id == rs$current_group)
    
    # is this a joker action (pass the correction)
    pass <- 0
    if(answer == -2) pass <- 1
    
    # Add the points to the group tally.
    statement1 <- paste0("UPDATE groups ",
                         "SET total_points_act = ",temp_g$total_points_act + point,", ",
                         "total_points_corrections = ",temp_g$total_points_corrections + point,", ",
                         "total_points_th = ",temp_g$total_points_th + point,", ",
                         "n_corrections = ",temp_g$n_corrections + 1,", ",
                         "n_corrections_pass = ",temp_g$n_corrections_pass + pass,", ",
                         "total_points = ",temp_g$total_points_act +
                           point +
                           temp_g$penality_plants +
                           temp_g$penality_animals +
                           temp_g$penality_zones,", ",
                         "total_points_tp = ",temp_g$total_points_act +
                           point +
                           temp_g$penality_plants +
                           temp_g$penality_animals +
                           temp_g$penality_zones," ",
                         " WHERE id = '",temp_g$id,"'")
    dbSendStatement(con, statement1)
    
    # Add an entry into the correction data table
    n_corrs <- nrow(dbReadTable(con, "corrections"))
    # temp <- data.frame(id = (n_corrs + 1),
    #                    id_group = rs$current_group,
    #                    id_quest = rs$current_img_peer$id_quest,
    #                    code = rs$current_img_peer$code,
    #                    answer = answer,
    #                    point = point,
    #                    date = as.character(today()))
    
    # Update the 'corrections' datatable with the current correction of the group
    statement2 <- paste0("UPDATE corrections ",
                         "SET answer = ",answer,", ",
                         "point = ", point,", ",
                         "date = ",as.character(today())," ",
                         " WHERE id_group = ",rs$current_group," AND code = '",rs$current_img_peer$code,"'")
    dbSendStatement(con, statement2)
    
    # dbWriteTable(con, "corrections", temp, append = TRUE)
    dbDisconnect(con)
    rs$current_img_peer <- NULL
    rs$update_correction_peer <- -rs$update_correction_peer
    
    if(answer != -2 & point == 0){
      sendSweetAlert(session, title = "Wrong answer",
                     text = NULL, type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else if(answer != -2 & point == 1){
      sendSweetAlert(session, title = "Correct answer",
                     text = NULL, type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
    
    
  }
  
  
  # The image is CORRECT
  # Store the result in the database, with a value = 1
  observeEvent(input$button_correct_peer, {
    con = dbcon(isLocal)
    done <- dbReadTable(con, "corrections") %>%
      filter(id_group == rs$current_group) %>%
      filter(answer != -2)
    dbDisconnect(con)
    if(nrow(done) >= 100){
      sendSweetAlert(session, title = "Out of peer-reviews",
                     text = "Sorry, you used your 100 peer-reviews already", type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      val <- 0
      if(rs$current_img_peer$validated > 0) val <- 1
      peer_correction(1, val)
    }
  })
  
  # The image is INCORRECT
  # Store the result in the database, with a value = -0
  observeEvent(input$button_false_peer, {
    
    con = dbcon(isLocal)
    done <- dbReadTable(con, "corrections") %>%
      filter(id_group == rs$current_group) %>%
      filter(answer != -2)
    dbDisconnect(con)
    if(nrow(done) >= 100){
      sendSweetAlert(session, title = "Out of peer-reviews",
                     text = "Sorry, you used your 100 peer-reviews already", type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      val <- 0
      if(rs$current_img_peer$validated < 0) val <- 1
      peer_correction(0, val)
    }
  })
  
  
  # Pass image
  # Store the result in the database, with a value = -0
  observeEvent(input$button_unknown_peer, {
    con = dbcon(isLocal)
    temp_g <- dbReadTable(con, "groups") %>%
      filter(id == rs$current_group)
    dbDisconnect(con)
    
    print(temp_g)
    if(temp_g$n_corrections_pass >= 5){
      sendSweetAlert(session, title = "Out of jokers",
                     text = "Sorry, you used your 5 jokers already", type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      peer_correction(-2, 0)
    }
  })
  
  
  # Display the current image title, to be displayed on top of the image
  # The title include other information about the image
  
  output$img_title_peer <- renderText({
    req(rs$current_img_peer)
    
    con = dbcon(isLocal)
    quest <- dbReadTable(con, "quests") %>%
      filter(id == rs$current_img_peer$id_quest)
    dbDisconnect(con)
    
    text <- paste0("<h3>",quest$name,"</h3>")
    text
  })
  
  # Display the image to correct
  
  output$myImage_peer <- renderImage({
    req(rs$current_img_peer)
    
    # readImage(normalizePath(file.path('./www/img/', rs$current_img_peer$code))) %>%
    #   writeImage("./www/temp/correct.jpg")
    #
    img <- image_read(normalizePath(file.path('./www/img/', rs$current_img_peer$code)))
    image_write(img, "./www/temp/correct_peer.jpg")
    
    filename <- normalizePath(file.path("./www/temp/correct_peer.jpg"))
    
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image name", rs$current_img_peer$code))
  }, deleteFile = F)
  
  
  
  # plot the evokution of the correction
  output$peer_review_plot <- renderPlot({
    
    req(rs$update_correction_peer)
    con = dbcon(isLocal)
    done <- dbReadTable(con, "corrections") %>%
      filter(id_group == rs$current_group) %>%
      filter(answer != -2)
    dbDisconnect(con)
    done$point[done$point == 1] <- "correct"
    done$point[done$point == 0] <- "wrong"
    
    var <- done$point  # the categorical data
    
    todo <- rep("to do", (100 - length(var)))
    var <- c(var, todo)
    
    
    ## Prep data (nothing to change here)
    nrows <- 10
    df <- expand.grid(y = 1:nrows, x = 1:nrows)
    categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
    df$category <- factor(rep(names(categ_table), categ_table))
    
    ## Plot
    ggplot(df, aes(x = x, y = y, fill = category)) +
      geom_tile(color = "black", size = 0.5) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
      scale_fill_brewer(palette = "Set3") +
      coord_equal() +
      labs(title=lang$peer_plot_evolution) +
      theme_void()
  })
  
  # Plot the evolution of jokers
  output$peer_review_pass <- renderPlot({
    
    req(rs$update_correction_peer)
    con = dbcon(isLocal)
    done <- dbReadTable(con, "corrections") %>%
      filter(id_group == rs$current_group) %>%
      filter(answer == -2)
    dbDisconnect(con)
    
    done$point[done$point == 0] <- "joker used"
    
    var <- done$point  # the categorical data
    print(var)# if(length(var) > 5){
    todo <- rep("available", (5 - length(var)))
    var <- c(var, todo)
    # }
    
    
    ## Prep data (nothing to change here)
    nrows <- 1
    df <- expand.grid(y = 1:nrows, x = 1:5)
    categ_table <- round(table(var) * ((nrows*5)/(length(var))))
    df$category <- factor(rep(names(categ_table), categ_table))
    
    
    ## Plot
    ggplot(df, aes(x = x, y = y, fill = category)) +
      geom_tile(color = "black", size = 0.5) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
      scale_fill_brewer(palette = "Set3") +
      coord_equal() +
      labs(title=lang$peer_plot_joker) +
      theme_void()
  })
  
  
  
  
  
  
  
  
  
  #--------------------------------------------------------------------------------
  #----------------------  ADMIN PANEL FUNCTIONS ----------------------------------
  #--------------------------------------------------------------------------------
  
 
  #-------------------------------------------------
  # RESET PASSWORD
  # Reset the password for one user
  # This should not be done during the game!
  #-------------------------------------------------
  
  observeEvent(input$reset_password, {
    
    con = dbcon(isLocal)
    statement <- paste0("UPDATE users ",
                        "SET login = 0, ", 
                        "password = '",digest("test", algo = c("sha256")),"' ",
                        "WHERE id = '",input$noma_input,"'")
    print(statement)
    dbSendStatement(con, statement)
    dbDisconnect(con)
    sendSweetAlert(session, title = "Password reset", text = NULL, type = "success",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  #-------------------------------------------------
  # PARAMETERS PANEL  
  # This panel allows GOD to change the game parameters
  # This should not be done during the game!
  #-------------------------------------------------
  
  observeEvent(input$update_params, {
    
    con = dbcon(isLocal)
    
    statement <- paste0("UPDATE params ",
                        "SET value = ",input$req_bounties," ",
                        "WHERE param = 'req_bounties'")
    dbSendStatement(con, statement)
    
    statement <- paste0("UPDATE params ",
                        "SET value = ",input$req_bounties_corr," ",
                        "WHERE param = 'req_bounties_corr'")
    dbSendStatement(con, statement)
    
    statement <- paste0("UPDATE params ",
                        "SET value = ",input$req_out_zones," ",
                        "WHERE param = 'req_out_zones'")
    dbSendStatement(con, statement)
    
    statement <- paste0("UPDATE params ",
                        "SET value = ",input$max_found_quest," ",
                        "WHERE param = 'max_found_quest'")
    dbSendStatement(con, statement)
    dbDisconnect(con)
    sendSweetAlert(session, title = "Parameters changed", text = NULL, type = "success",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  
  #-------------------------------------------------
  # BOUNTIES MAP
  # A map displaying all the bounties from the players
  #-------------------------------------------------
  
  output$bounties_map <- renderLeaflet({
    con = dbcon(isLocal)
    temp <- merge(dbReadTable(con, "bounties") , 
                  all_quests, 
                  by.x = "id_quest", by.y = "id") %>% 
      filter(latitude != 0 &  
             submitted %in% input$map_status & 
             type %in% input$map_types & 
             validated %in% input$map_correction) %>% 
      mutate(lab = paste0("<b>",name.y,"</b></br>",
                      "<img src = img/", code," width=100%></br>",
                      "</br>Found on ",date_stored, " by group ",id_group ))
    
    print(temp)
    dbDisconnect(con)
    
    map <-  leaflet(temp) %>%
      addTiles() %>%
      addMarkers(~ longitude, ~ latitude, popup = ~lab,
                 clusterOptions = markerClusterOptions(),
                 options = popupOptions(minWidth = 500,maxWidth = 500)) %>%
      addProviderTiles(providers$CartoDB.Positron)
      
    map
    
    
    
  })
  
  
  #-------------------------------------------------
  # CORRECTION PANEL
  # This section allows the masters to correct the 
  # quests submitted by the users. The database is then 
  # directly updated and the corrections visible by the users. 
  #-------------------------------------------------
  
  # Update the different fields of the corrections section
  observe({
    
    # This can be trigger on demand by the different actions in the section
    req(rs$update_correction)
    
    # Load only if the current user is a master
    if(rs$current_role == "player") return(NULL) 
    
    con = dbcon(isLocal)
    
    # Load the images to correct. Images are selected based 
    # on the user choices. Only submitted images are selected
    rs$to_correct  <- merge(dbReadTable(con, "bounties") , 
                            all_quests, 
                            by.x = "id_quest", by.y = "id") %>% 
      mutate(name = name.x, 
             quest = name.y) %>% 
      filter(submitted == 1,
             validated %in% input$correction_val,
             type %in% input$correction_type, 
             !code %in% rs$correction_done)
    
    
    # Get the list of quests to correct 
    # (to be used in the dropdown menu)
    to_correct_quests <- rs$to_correct %>% 
      distinct(id_quest, quest)
    
    # Create the list of quests to be used on the menu
    to_correct <- to_correct_quests$id
    names(to_correct) <- to_correct_quests$quest
    
    # Update the drop down menu based on the user choices
    updateSelectInput(session, "correction_quest", choices = to_correct, selected = to_correct[1])

    
    dbDisconnect(con)
  })
  
  
  # Load the data already corrected in this session
  observe({
    if(rs$current_role == "player") return(NULL)
    req(input$correction_quest)
    rs$to_correct_current <-rs$to_correct %>% 
      filter(id_quest == input$correction_quest) %>% 
      arrange(date_submitted)
  })
  
  # Define the image that is displayed for correction
  # 
  observe({
    req(rs$update_correction)
    req(rs$to_correct_current)
    rs$current_img <- rs$to_correct_current %>%
      slice(1)
    
  })
  
  
  # The master is UNABLE TO CORRECT the image
  # Move to the next one without updating the database
  observeEvent(input$button_unknown, {
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The image is CORRECT
  # Store the result in the database, with a value = 1
  observeEvent(input$button_correct, {
    
    con = dbcon(isLocal)
    temp_b <- dbReadTable(con, "bounties") %>%  filter(code == rs$current_img$code)
    temp_g <- dbReadTable(con, "groups") %>%  filter(id == temp_b$id_group)
    temp_q <- dbReadTable(con, "quests") %>%  filter(id == temp_b$id_quest)
    
    statement <- paste0("UPDATE bounties ",
                        "SET validated = 1 ,",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code = '",rs$current_img$code,"'")
    
    # Update the summary group information
    statement1 <- paste0("UPDATE groups ",
                         "SET total_points_act = ",temp_g$total_points_act + temp_q$points,", ",
                         "total_points = ",temp_g$total_points_act + temp_q$points ," ",                
                         " WHERE id = '",temp_g$id,"'")
    
    dbSendStatement(con, statement)
    dbSendStatement(con, statement1)
    dbDisconnect(con)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The image is CORRECT and VERY NICE (and therefore worth keeping on the side)
  # Store the result in the database, with a value = 1
  observeEvent(input$button_tocheck, {
    
    con = dbcon(isLocal)
    temp_b <- dbReadTable(con, "bounties") %>%  filter(code == rs$current_img$code)
    temp_g <- dbReadTable(con, "groups") %>%  filter(id == temp_b$id_group)
    temp_q <- dbReadTable(con, "quests") %>%  filter(id == temp_b$id_quest)
    
    statement <- paste0("UPDATE bounties ",
                        "SET validated = 4, ",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code = '",rs$current_img$code,"'")
    
    # Update the summary group information
    statement1 <- paste0("UPDATE groups ",
                         "SET total_points_act = ",temp_g$total_points_act + temp_q$points,", ",
                         "total_points = ",temp_g$total_points_act + temp_q$points ," ",                
                         " WHERE id = '",temp_g$id,"'")
    
    dbSendStatement(con, statement)
    dbSendStatement(con, statement1)
    dbDisconnect(con)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The image is INCORRECT 
  # Store the result in the database, with a value = -0
  observeEvent(input$button_false, {
    con = dbcon(isLocal)
    temp_b <- dbReadTable(con, "bounties") %>%  filter(code == rs$current_img$code)
    temp_g <- dbReadTable(con, "groups") %>%  filter(id == temp_b$id_group)
    
    statement <- paste0("UPDATE bounties ",
                        "SET validated = -1, ",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code = '",rs$current_img$code,"'")
    
    # Update the summary group information
    statement1 <- paste0("UPDATE groups ",
                         "SET n_quests = ",temp_g$n_quests - 1 ," ",
                         " WHERE id = '",temp_g$id,"'")
    
    # update the number of found bounties submitted to this quests, and make it
    # inactive if necessary (if nmber of bounties is above the threshold)
    current_quest <- dbReadTable(con, "quests") %>% 
      filter(id == rs$current_img$id_quest)
    print(current_quest)
    
    statement2 <- paste0("UPDATE quests ",
                         "SET found = ",(current_quest$found - 1)," ",
                         "WHERE id = ",current_quest$id)
    
    dbSendStatement(con, statement)
    dbSendStatement(con, statement1)
    dbSendStatement(con, statement2)
    dbDisconnect(con)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The image is INCORRECT and ABSURD (and therefore worth keeping on the side)
  # Store the result in the database, with a value = -3
  observeEvent(input$button_marrant, {
    
    con = dbcon(isLocal)
    temp_b <- dbReadTable(con, "bounties") %>%  filter(code == rs$current_img$code)
    temp_g <- dbReadTable(con, "groups") %>%  filter(id == temp_b$id_group)
    
    statement <- paste0("UPDATE bounties ",
                        "SET validated = -3, ",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code = '",rs$current_img$code,"'")
    
    # Update the summary group information
    statement1 <- paste0("UPDATE groups ",
                         "SET n_quests = ",temp_g$n_quests - 1 ," ",
                         " WHERE id = '",temp_g$id,"'")
    
    # update the number of found bounties submitted to this quests, and make it
    # inactive if necessary (if nmber of bounties is above the threshold)
    current_quest <- dbReadTable(con, "quests") %>% 
      filter(id == rs$current_img$id_quest)
    print(current_quest)
    statement2 <- paste0("UPDATE quests ",
                         "SET found = ",(current_quest$found - 1)," ",
                         "WHERE id = ",current_quest$id)
    
    dbSendStatement(con, statement)
    dbSendStatement(con, statement1)
    dbSendStatement(con, statement2)
    dbDisconnect(con)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  # The quest in the image is NOT VISIBLE (and therefore considered as false)
  # Store the result in the database, with a value = -2
  observeEvent(input$button_flou, {
    con = dbcon(isLocal)
    temp_b <- dbReadTable(con, "bounties") %>%  filter(code == rs$current_img$code)
    temp_g <- dbReadTable(con, "groups") %>%  filter(id == temp_b$id_group)
    
    statement <- paste0("UPDATE bounties ",
                        "SET validated = -2,",
                        " date_validated = '",as.character(today()), "',",
                        " validated_by = ",rs$current_user,
                        " WHERE code = '",rs$current_img$code,"'")
    
    # Update the summary group information
    statement1 <- paste0("UPDATE groups ",
                         "SET n_quests = ",temp_g$n_quests - 1 ," ",
                         " WHERE id = '", temp_g$id ,"'")
    
    # update the number of found bounties submitted to this quests, and make it
    # inactive if necessary (if nmber of bounties is above the threshold)
    current_quest <- dbReadTable(con, "quests") %>% 
      filter(id == rs$current_img$id_quest)
    print(current_quest)
    statement1 <- paste0("UPDATE quests ",
                         "SET found = ",(current_quest$found - 1)," ",
                         "WHERE id = ",current_quest$id)
    
    dbSendStatement(con, statement)
    dbSendStatement(con, statement1)
    dbSendStatement(con, statement2)
    dbDisconnect(con)
    rs$update_correction <- -rs$update_correction
    rs$correction_done <- c(rs$correction_done, rs$current_img$code) # store the current corrections
  })
  
  
  # Display the current image title, to be displayed on top of the image
  # The title include other information about the image
  
  output$img_title <- renderText({
    req(rs$current_img)
    text <- paste0("<h3>",rs$current_img$quest,"</h3>",
                   "<b>title</b> = ",rs$current_img$name,
                   " / <b>code</b> = ",rs$current_img$code,
                   " / <b>date</b> = ", rs$current_img$date_submitted)
    if(rs$current_img$validated == 1){
      text <- paste0(text, "</br> STATUS = <b>CORRECT")
    }else if(rs$current_img$validated == 4){
      text <- paste0(text, "</br> STATUS = <b>CORRECT AND NICE</b>")
    }else if(rs$current_img$validated == 0){
      text <- paste0(text, "</br> STATUS = <b>Non-corrigé</b>")
    }
    else if(rs$current_img$validated == -1){
      text <- paste0(text, "</br> STATUS = <b>FAUX</b>")
    }
    else if(rs$current_img$validated == -2){
      text <- paste0(text, "</br> STATUS = <b>FLOU</b>")
    }else if(rs$current_img$validated == -3){
      text <- paste0(text, "</br> STATUS = <b>WTF</b>")
    }
    text
  })
  
  
  # Display the number of images left to check in this section
  output$to_do <- renderText({
    req(rs$to_correct_current)
    n <- rs$to_correct_current %>% 
      filter(validated %in% input$correction_val,
             type %in% input$correction_type)  %>% 
      nrow()
    paste0(n, " remaining images for this quest")
  })
  
  # Display the number of images done in this correction session
  output$done <- renderText({
    req(rs$correction_done)
    paste0(length(rs$correction_done), " image(s) done")
  })
  
  
  # Display the image to correct
  
  output$myImage <- renderImage({
    req(rs$current_img)
    
    readImage(normalizePath(file.path('./www/img/', rs$current_img$code))) %>% 
      writeImage("./www/temp/correct.jpg")
    
    filename <- normalizePath(file.path("./www/temp/correct.jpg"))
    
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Image name", rs$current_img$code))
  }, deleteFile = F)
  
  
  # Display the correct images. These images are previously corrected ones from the 
  # current BioGO game. They serve a guideline for the masters during the corrections.
  
  output$knowledge_base <-  renderUI({
    
    if(rs$current_role == "player") return(NULL)
    require(input$correction_quest)
    
    con = dbcon(isLocal)
    
    temp <- dbReadTable(con, "bounties") %>% 
      filter(id_quest == input$correction_quest) %>% 
      filter(validated >= 1) %>% 
      arrange(desc(validated))
    dbDisconnect(con)
    
    if(nrow(temp) > 0){
      # load the images to correct
      for(i in c(1:nrow(temp))){
        readImage(normalizePath(file.path('./www/img/', temp$code[i]))) %>% 
          writeImage(paste0("./www/img_corr/",temp$code))
      }
      
      # Get the correct image
      images <- data.frame(src = list.files('www/img_corr/')) %>% 
        filter(src %in% temp$code) 
      
      images <- images %>% 
        sample_n(min(input$n_correct_img, nrow(images)))
      
      images$key <- c(1:nrow(images))
      
      lightbox_gallery(images,  "gallery",display = TRUE, path = "img_corr")
    }
  })
  
  
  
  #-------------------------------------------------
  # RECOMPUTE GROUP POINTS
  # Recompute the points of all the groups
  #-------------------------------------------------
  
  observeEvent(input$recompute_group, {
    
    con <- dbcon(isLocal)
    gr <- dbGetQuery(con, "SELECT * FROM groups")
    
    bou <- merge(dbReadTable(con, "bounties"),
                 dbReadTable(con, "quests"),
                 by.x="id_quest", by.y = "id")
    
    new_gr <- NULL
    
    for(g in unique(gr$id)){
      
      # All the quests
      temp <- bou %>%
        filter(id_group == g & submitted == 1) %>%
        arrange(date)
      
      # All the corrections
      temp_corr <- dbReadTable(con, "corrections") %>%
        filter(id_group == g) %>%
        filter(answer != -2)
      point_corr <- sum(temp_corr$point)
      n_corr <- length(temp_corr$point)
      
      temp_pass <- dbReadTable(con, "corrections") %>%
        filter(id_group == g) %>%
        filter(answer == -2)
      n_pass <- nrow(temp_pass)

      # Compute for all the quests
      temp1 <- gr %>% filter(id == g)
      temp1$n_animals <- nrow(temp[temp$type == "animal",])
      temp1$n_plants <- nrow(temp[temp$type == "plant",])
      temp1$n_zones <- length(unique(temp$id_zone))
      temp1$penality_animals <- 0
      temp1$penality_plants <- 0
      temp1$penality_zones <- 0
      temp1$n_quests <- nrow(temp) - nrow(temp[temp$validated < 0,])
      temp1$n_errors <- nrow(temp[temp$validated < 0,])
      temp1$n_corrections <- n_corr
      temp1$n_corrections_pass <- n_pass
      temp1$total_points_corrections <- point_corr
      temp1$total_points_th <- sum(temp$points[temp$validated >= 0]) + point_corr
      temp1$total_points_act <- sum(temp$points[temp$validated >= 1]) + point_corr
      temp1$total_points <- temp1$total_points_act 
      temp1$total_points_tp <- temp1$total_points
      
      new_gr <- rbind(new_gr, temp1)
    }
    
    ti <- gsub(" ", "-", gsub(":", "", gsub("-", "", paste0(ymd_hms(now()), "_groups.csv"))))
    write_csv(gr, paste0("www/bu_db/", ti))
    
    dbWriteTable(con, "groups", new_gr, overwrite = T)
    
    dbDisconnect(con)
    
    sendSweetAlert(session, title = "Points recomputed", text = NULL, type = "success",
                   btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  })
  
  
  
  
  #--------------------------------------------------
  # DISPLAY A TABLE WITH ALL GROUPS POINTS
  # This section allow the masters to visualize 
  # the data in the database. 
  #--------------------------------------------------
  
  # Visualize the selected datatable (via input$select_table)
  output$table_data = renderDataTable({
    if(input$select_table == 1) return(NULL)
    con = dbcon(isLocal)
    temp <- dbReadTable(con, input$select_table)
    dbDisconnect(con)
    as.datatable(formattable(temp),
                 rownames = F,
                 options = list(pageLength = 50))
    
  })
  
  # Download a csv file of the selected dataset
  output$download_table_data <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_", input$select_table, ".csv")
    },
    content = function(file) {
      con = dbcon(isLocal)
      temp <- dbReadTable(con, input$select_table) 
      dbDisconnect(con)
      write.csv(temp, file, row.names = FALSE)
    }
  )
  
  
  
  # Download the whole database file
  output$download_all_table_data <- downloadHandler(
    filename <- function() {
      paste0(Sys.Date(), "_database_biogo.sql")      
    },
    
    content <- function(file) {
      file.copy("www/database1.sql", file)
    },
    contentType = "application/sql"
  )
  
  
  #--------------------------------------------------
  # LOAD NEW DATA IN THE DATABASE
  # This section allow the masters to add new data in the 
  # database. The data can be either append to or replace
  # the current one. 
  #--------------------------------------------------
  
  # Display the data to be added into the database. 
  # The data can come either from a new CSV file
  # of from a back datafiles stored in the app. 
  # The data is then stored in rs$input_file
  output$input_table_data <- renderDataTable({
    
    # Load data from a loaded csv files
    if(!input$from_backup_file){
      inFile <- input$input_file
      if (is.null(inFile)) return(NULL)
      temp <- read.csv(inFile$datapath)  
      # Load data from a backup file
    }else{
      temp <- read.csv(paste0("www/bu_db/", input$select_table_bu))  
    }
    
    dt <- as.datatable(formattable(temp),
                       rownames = F,
                       options = list(pageLength = 50))
    rs$input_file = temp
    
    dt
  })
  
  # Update the list of available backup datafiles
  # These will be displayed and can be used to replace the 
  # current data in the database
  observe({
    req(input$from_backup_file)
    all_bu <- list.files("www/bu_db/")
    updateSelectInput(session, "select_table_bu", choices = all_bu, selected = all_bu[1])
  })
  
  
  # Select the table in the DB that match the input file
  # This is done based on the structure of the loaded table
  # The loaded table should have the same column names as one of
  # the tables in the database. This is done only when
  # a table is displayed. Work with both new data (from csv)
  # and with the backup data (as the data come from rs$input_file)
  observe({
    req(rs$input_file)
    temp <- paste0(colnames(rs$input_file),collapse = "-")
    match <- NULL 
    k <- 0
    for(i in tables_str){
      k <- k+1
      if(temp == i){
        match <- k
      }
    }
    updateSelectInput(session, "select_table_1",  
                      choices = all_tables[match], 
                      selected = all_tables[match])
  })
  
  # # Select the table in the DB that match the backup file
  # # This is done based on the name of the backup table
  # # This is done only when a table is loaded. 
  # observe({
  #   req(input$select_table_bu)
  #   print("backup update")
  #   match <- substr(".csv", "", strsplit(input$select_table_bu, "_")[[1]][2])
  #   updateSelectInput(session, "select_table_1",  
  #                     choices = all_tables[match], 
  #                     selected = all_tables[match])
  # })
  
  
  # Update one of the data table with the loaded data (from CSV file)
  # The data can either replace the current data (input$append_data == 1), or
  # be appened to it (input$append_data == 0)
  observeEvent(input$update_table, {
    
    if(input$append_data == 1){
      # replace the current data in the DB by the new one. 
      # make a backup of current datatable before erasing it
      con = dbcon(isLocal)
      temp <- dbReadTable(con, input$select_table_1) 
      write_csv(temp, paste0("www/bu_db/", Sys.time(), "_", input$select_table_1, ".csv"))
      dbWriteTable(con, input$select_table_1, rs$input_file, overwrite = TRUE)      
      dbDisconnect(con)
    }else{
      # Add the new data to the current one in the DB. 
      # Update the id field in the new data, for consistency. 
      if(input$select_table_1 != "params") rs$input_file$id <- rs$input_file$id + max(temp$id) + 1 
      con = dbcon(isLocal)
      dbAppendTable(con, name = input$select_table_1, value = rs$input_file)      
      dbDisconnect(con)
    }
    
    # Success alert
    sendSweetAlert(session, title = paste0("Table ",input$select_table_1," updated"), 
                   text = NULL, type = "success",
                   btn_labels = "Ok", html = FALSE, 
                   closeOnClickOutside = TRUE)
    
  })
  
  
  
  #--------------------------------------------------
  # PLOT DASHBOARD DATA
  # Plot different information accessibles via the admin tab 
  # in the interface. 
  #--------------------------------------------------
  
  # Plot the repartition of the different types of submitted quests
  output$dashQuests <- renderPlot({
    con = dbcon(isLocal)
    pl <- merge(dbReadTable(con, "bounties"), 
                dbReadTable(con, "quests"), 
                by.x="id_quest", by.y = "id") %>% 
      filter(submitted == 1) %>% 
      ddply(.(difficulty), summarise, prop=length(difficulty)) %>%
      arrange(desc(difficulty)) %>%
      mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>% 
      ggplot(aes(x = 2, y = prop, fill = as.factor(difficulty))) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = prop), color = "white", size=10)+
      scale_fill_manual(values = mycols) +
      theme_void() +
      theme(text = element_text(size=6),
            legend.text = element_text(size=10),
            legend.title = element_text(size=10)) + 
      xlim(0.5, 2.5)
    dbDisconnect(con)
    pl
    
  })
  
  # Text title for the stored quests
  output$stored_quests_text <- renderUI({
    con = dbcon(isLocal)
    n_stored <- dbReadTable(con, "bounties") %>% nrow()
    dbDisconnect(con)
    text <- paste0("<h3>Stored bounties (",n_stored,")</h3>")
    HTML(text)
  })
  
  # Text title for the stored quests
  output$bounties_text <- renderUI({
    con = dbcon(isLocal)
    n_stored <- dbReadTable(con, "bounties") %>% nrow()
    dbDisconnect(con)
    text <- paste0("<h3>Bounties storage (",n_stored,")</h3>")
    HTML(text)
  })
  
  # Text title for the connections
  output$connections_text <- renderUI({
    con = dbcon(isLocal)
    n_stored <- dbReadTable(con, "log") %>% nrow()
    dbDisconnect(con)
    text <- paste0("<h3>Connections (",n_stored,")</h3>")
    HTML(text)
  })
  
  # Text title for the connections (unique users)
  output$connections_text_users <- renderUI({
    con = dbcon(isLocal)
    n_stored <- dbReadTable(con, "log") %>% 
      distinct(user_id) %>% 
      nrow()
    dbDisconnect(con)
    text <- paste0("<h3>Unique users (",n_stored,")</h3>")
    HTML(text)
  })
  
  # Text title for the subitted quests
  output$submitted_quests_text <- renderUI({
    con = dbcon(isLocal)
    n_submitted <- dbReadTable(con, "bounties") %>% 
      filter(submitted == 1) %>% 
      nrow()
    dbDisconnect(con)
    text <- paste0("<h3>Submitted bounties (",n_submitted,")</h3>")
    HTML(text)
  })
  
  # Plot the repartition of the different types of stored quests
  output$dashQuestsStored <- renderPlot({
    con = dbcon(isLocal)
    pl <- merge(dbReadTable(con, "bounties"), 
                dbReadTable(con, "quests"), 
                by.x="id_quest", by.y = "id") %>% 
      ddply(.(difficulty), summarise, prop=length(difficulty)) %>%
      arrange(desc(difficulty)) %>%
      mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>% 
      ggplot(aes(x = 2, y = prop, fill = as.factor(difficulty))) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = prop), color = "white", size=10)+
      scale_fill_manual(values = mycols) +
      theme_void() +
      theme(text = element_text(size=6),
            legend.text = element_text(size=10),
            legend.title = element_text(size=10)) + 
      xlim(0.5, 2.5)
    dbDisconnect(con)
    pl
    
  })
  
  
  
  # Plot the repartition of the different corrections types
  # for all the quests submitted
  
  output$dashCorrections <- renderPlot({
    con = dbcon(isLocal)
    
    pl <- dbReadTable(con, "bounties") %>% 
      filter(submitted == 1) %>% 
      mutate(corr = ifelse(validated == 0, "waiting", "correct")) %>% 
      mutate(corr = ifelse(validated > 0, "correct", corr)) %>% 
      mutate(corr = ifelse(validated < 0, "false", corr)) %>% 
      ddply(.(corr), summarise, prop=length(corr)) %>%
      arrange(desc(corr)) %>%
      mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>% 
      ggplot(aes(x = 2, y = prop, fill = corr)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = prop), color = "white", size=10)+
      scale_fill_manual(values = mycols) +
      theme_void() +
      theme(text = element_text(size=6),
            legend.text = element_text(size=10),
            legend.title = element_text(size=10)) + 
      xlim(0.5, 2.5)
    dbDisconnect(con)
    pl
  })
  
  
  # Plot the number of submissions per days since
  # the beginning of the game
  
  output$dashSubmissions <- renderPlot({
    con = dbcon(isLocal)
    
    pl <- dbReadTable(con, "bounties") %>% 
      filter(date_stored != "string") %>% 
      mutate(date = ymd(date_stored)) %>% 
      ddply(.(date), summarise, prop=length(date)) %>% 
      ggplot(aes(ymd(date), prop)) + 
      geom_bar(stat = "identity", fill="lightgrey")+
      xlab("Date") +
      ylab("Number of stored bounties")
    dbDisconnect(con)
    pl
    
  })
  
  
  # Plot the number of connections to the app per days since
  # the beginning of the game
  
  output$dashConnections <- renderPlot({
    
    con = dbcon(isLocal)
    
    pl <- dbReadTable(con, "log") %>% 
      filter(datetime != "test") %>% 
      mutate(date = ymd(substr(datetime, 0, 10))) %>% 
      ddply(.(date), summarise, prop=length(date)) %>% 
      ggplot(aes(ymd(date), prop)) + 
      geom_bar(stat = "identity", fill="lightgrey")+
      xlab("Date") +
      ylab("Number of connections")
    
    dbDisconnect(con)
    
    pl
    
  })
  
  # Plot the number of connections to the app per days/users since
  # the beginning of the game
  
  output$dashConnectionsUsers <- renderPlot({
    
    con = dbcon(isLocal)
    
    pl <- dbReadTable(con, "log") %>% 
      filter(datetime != "test") %>% 
      mutate(date = ymd(substr(datetime, 0, 10))) %>% 
      distinct(user_id, date) %>% 
      ddply(.(date), summarise, prop=length(date)) %>% 
      ggplot(aes(ymd(date), prop)) + 
      geom_bar(stat = "identity", fill="lightgrey")+
      xlab("Date") +
      ylab("Number of connections")
    
    dbDisconnect(con)
    
    pl
    
  })
})