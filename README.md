# QuoVidi
This repository contain the source code of the web application. The web application is developped in R and uses the Shiny package. The app can be installed on a Shiny server and used to play the QuoVidi game. 


## Personalise the game data
Before starting the game, the data need to be personalized. You need indeed to add your own users, groups, quests and zones.  To do so, you need to go to the setup google sheet here : [https://docs.google.com/spreadsheets/d/1-AH4w4k7kyIRvzisJZOQ1xnTVK7OgfjcWmYVXTaDrCc/edit?usp=sharing]() and copy it to your own account. The  you will be able to modify the different sheets. 

### 1. Groups

The game is played in groups. There can be as many users per group as you wish (1-x). For each group, you just need to enter : 
- an `id`, that is unique
- a `name`, that is unique as well

All the remaining columns need to be set to `0`

### 2. Users

For each user, you need to enter : 

- **id** : the unique ID of the user. 
- **group_id** : the id of the group the user belong to
- **name** : the first name of the user. Does not need to be unique
- **username** : a unique username for the user to log in with
- **password** : Cripted and defined by each users. Copy the default one for each user. 
- **role** : users need to have the role `player`. Game managers have the role `master`. Thi will give them access to the admin interface in the game. 
- **login** : needs to be set to `0`

### 3. Quests : 

For each quest, you need to enter:

- **id** : the unique ID of the quest.
- **name** : the text describing the quest. It should be enough for the players to understand the quest
- **points** : The number of points the quest will be for the user. 
- **difficulty** : Same as the points
- **type** : Quests can be grouped in different `types`, to allow the users to sort them more easily. 
- **group** : Quests can be in `groups`, which is hierchically below the `type`. Again, this is to help users navigate the game quests. 
- **status** : put `active`
- **found** : put `0`


### 4. Zones 

For each zone, you need to enter : 

- **id** : the unique ID of the quest.
- **name** : the name of the zone

For the zones, you also need to have a file containing the limits of each zone. You can create a json with the limits of your zones at [http://geojson.io/#map=2/20.0/0.0](). Your own json needs to replace the one called `www/zones.json`.


## Setup the game. 

1. Copy or fork the files locally. 
4. In RStudio, open the file `setup_database_QuoVidi.R`
5. Change the name of the setup google sheet with your own 
6. Run the whole `setup_database_QuoVidi.R` file. This will enter the game data, from the Google sheet into the dedicated QuoVidi database. 
7. In RStudio, open the file `quests.rmd` and `knit` it


## Install the game. 

You can now copy the whole QuoVidi folder to your installation of Shiny Server. Explanation on how to install your server can be found here : [https://www.r-bloggers.com/install-shiny-server-for-r-on-ubuntu-the-right-way/]() 





