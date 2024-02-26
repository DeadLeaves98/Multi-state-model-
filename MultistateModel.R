# potentially the code for the multistate models 
# Steps
# 1: Tidy the data --> remove chicks 
# 2: create encounter history 
################################
# How to format the data file 
#   10 - A0  ;alive and active 
#   00 - 00  ;not detected 
#   11 - AA  ; dead in A 
###############################
# States:
#   A = Adult no chicks/nest 
#   B = Brooding Adult 
#   N = Incubating a nest
#############################################################################
#                             Data Cleaning ----
##############################################################################
library(dplyr); library(stringr); library(lubridate); library(tidyr); library(sf)
nobo1 <- read.csv("Orton_Bobwhite_Telemetry_Data_Entry_0.csv")
head(nobo1)
nrow(nobo1) # 38810

#### Remove chicks ---- 
# clean this file up by removing the columns we do not need
nobo1 <- dplyr::select(nobo1, -GlobalID, -Time, -Burn.Status, -Habitat.Type,
                       -Associated.Bird..1, -Associated.Bird..2, -Associated.Bird..3,
                       -Associated.Bird..4, -Associated.Bird..5, -Associated.Bird..6,
                       -Associated.Bird..7, -Associated.Bird..8, -Associated.Bird..9,
                       -Associated.Bird..10, -Parent.Present., -Enter.Adult.ID,
                       -Comments, -CreationDate, -Creator, -EditDate, -Editor)

head(nobo1)
nobo1 <- subset(nobo1, Location.Type != "Chick") # remove chicks
nobo1 <- arrange(nobo1, ObjectID) # re-order rows in order of objectID
# nrow = 31k 

# dates are currently characters; need to convert to date format (mm/dd/yyyy)
nobo1 <- nobo1 %>% mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

## modifying the dates to something easier to handle
nobodates <- data.frame("Date" = nobo1$Date) # create a seperate dataframe 
nobodates <- separate(nobodates, Date, into = c("year", "month", "day"), sep = "-") # split up month, day, year
nobo2 <- cbind(nobo1, nobodates) # col bind the OG data with this date dataframe 

# make numeric
nobo2$month <- as.numeric(nobo2$month)
nobo2$day <- as.numeric(nobo2$day)
nobo2$year <- as.numeric(nobo2$year)

nobo2$CombinedDate <- paste0(nobo2$day, "_", nobo2$month, "_", nobo2$year) # for the lookup table 
nobo2$CombinedDate0 <- paste0(nobo2$day, "_", nobo2$month) # for the lookup table 

nobo1 = nobo2
# Some chicks were entered as broods and we need to remove these chicks from the data 
nrow(nobo1) # 31877
nobo1$chick <- paste0("band" = substr(nobo1$Bird.ID, 9, 11)) # isolate the first 3 numbers of band number

# subset the entire year of 2022 (1st line) then eliminate chick bands (which start with 225)
nobosub2022 <- nobo1[nobo1$Date >= "2022-01-01" & nobo1$Date <= "2022-12-31", ]
nobosub2022 <- subset(nobosub2022, chick!=225)

# subset the entire year of 2023 (1st line) then eliminate chick bands (which start with 235)
nobosub2023 <- nobo1[nobo1$Date >= "2023-01-01" & nobo1$Date <= "2023-12-31", ]
nobosub2023 <- subset(nobosub2023, chick!=235)

# re-combine the two chunks from 2022-23
nobo2 <- rbind(nobosub2022, nobosub2023)

nobo1 <- nobo2 # rename nobo1
nrow(nobo1) # 31828

# bird fixes ----
# 162.603_220346 should actually be called 162.376_220019 so we need to fix that
nobo1$Bird.ID <- str_replace(nobo1$Bird.ID , "162.603_220346", "162.376_220019")

# i want to count the number of times each bird is fated and put the sum in its own column 
test = nobo1 %>%
  group_by(Bird.ID) %>%
  summarize(count = sum(Bird.Status == 'Fate'))
errors = subset(test, count > 1)# list of birds that are errors 

#I am going to do this the long way but I am sure there is an easy way to do this 
a = subset(nobo1, Bird.ID == "164.436_205016") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "164.436_205016") #remove it from the OG data set 
a <- a[a$Date < "2022-08-25", ] # keep all data before the fate date 

b = subset(nobo1, Bird.ID == "164.408_220599") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "164.408_220599") #remove it from the OG data set 
b <- b[b$Date < "2022-08-26", ] # keep all data before the fate date 

c = subset(nobo1, Bird.ID == "164.345_220284") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "164.345_220284") #remove it from the OG data set 
c <- c[c$Date < "2022-08-26", ] # keep all data before the fate date 

d = subset(nobo1, Bird.ID == "164.318_220389") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "164.318_220389") #remove it from the OG data set 
d <- d[d$Date < "2022-08-23", ] # keep all data before the fate date 

e = subset(nobo1, Bird.ID == "164.164_220386") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "164.164_220386") #remove it from the OG data set 
e <- e[e$Date < "2022-08-23", ] # keep all data before the fate date 

f = subset(nobo1, Bird.ID == "164.029_214754") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "164.029_214754") #remove it from the OG data set 
f <- f[f$Date < "2022-07-19", ] # keep all data before the fate date 

g = subset(nobo1, Bird.ID == "164.020_204594") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "164.020_204594") #remove it from the OG data set 
g <- g[g$Date < "2022-07-28", ] # keep all data before the fate date 

h = subset(nobo1, Bird.ID == "163.621_220379") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.621_220379") #remove it from the OG data set 
h <- h[h$Date < "2022-09-01", ] # keep all data before the fate date 

i = subset(nobo1, Bird.ID == "163.544_220071") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.544_220071") #remove it from the OG data set 
i <- i[i$Date < "2022-09-22", ] # keep all data before the fate date 

j = subset(nobo1, Bird.ID == "163.532_220122") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.532_220122") #remove it from the OG data set 
j <- j[j$Date < "2023-09-30", ] # keep all data before the fate date 

k = subset(nobo1, Bird.ID == "163.458_220410") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.458_220410") #remove it from the OG data set 
k <- k[k$Date < "2022-08-24", ] # keep all data before the fate date 

l = subset(nobo1, Bird.ID == "163.217_220637") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.217_220637") #remove it from the OG data set 
l <- l[l$Date < "2023-04-20", ] # keep all data before the fate date 

m = subset(nobo1, Bird.ID == "163.164_220829") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.164_220829") #remove it from the OG data set 
m <- m[m$Date < "2023-06-08", ] # keep all data before the fate date 

n = subset(nobo1, Bird.ID == "163.129_220309") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.129_220309") #remove it from the OG data set 
n <- n[n$Date < "2022-08-24", ] # keep all data before the fate date 

o = subset(nobo1, Bird.ID == "163.110_220137") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.110_220137") #remove it from the OG data set 
o <- o[o$Date < "2023-09-21", ] # keep all data before the fate date 

p = subset(nobo1, Bird.ID == "163.055_220866") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.055_220866") #remove it from the OG data set 
p <- p[p$Date < "2023-05-24", ] # keep all data before the fate date 

q = subset(nobo1, Bird.ID == "162.787_220228") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "162.787_220228") #remove it from the OG data set 
q <- q[q$Date < "2022-07-28", ] # keep all data before the fate date 

r = subset(nobo1, Bird.ID == "162.244_220522") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "162.244_220522") #remove it from the OG data set 
r <- r[r$Date < "2022-05-31", ] # keep all data before the fate date 

s = subset(nobo1, Bird.ID == "162.202_214532") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "162.202_214532") #remove it from the OG data set 
s <- s[s$Date < "2022-07-11", ] # keep all data before the fate date 

t = subset(nobo1, Bird.ID == "162.137_220155") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "162.137_220155") #remove it from the OG data set 
t <- t[t$Date < "2023-04-08", ] # keep all data before the fate date 

u = subset(nobo1, Bird.ID == "148.958_214642") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "148.958_214642") #remove it from the OG data set 
u <- u[u$Date < "2022-01-19", ] # keep all data before the fate date 

v = subset(nobo1, Bird.ID == "163.471_220402") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "163.471_220402") #remove it from the OG data set 
v <- v[v$Date < "2022-09-10", ] # keep all data before the fate date 

w = subset(nobo1, Bird.ID == "164.514_220524") #subset this bird
nobo1 = subset(nobo1, Bird.ID != "164.514_220524") #remove it from the OG data set 
w <- w[w$Date < "2022-09-22", ] # keep all data before the fate date 

# bind it all back together 
nobo1 = rbind(nobo1, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
##### Finished removing anything after the first fate 

head(nobo1)
unique(nobo1$Bird.Status)
lookuptable1 <- read.csv("E:/NOBO R Projects_Github/NOBO Survival/Date_lookuptable.csv")
head(lookuptable1) # the intervals column is 4 day intervals starting from April 1st 
# until the max calendar day of a brood/nest for both years (Approximately oct 21) 

# What is the oldest brood date?
x = subset(nobo1, Location.Type=="Brood") # Oct 17 oldest brood in 2023
max(x$Date)
y = subset(x, year == "2022")
max(y$Date) # Oct 18 is the oldest brood in 2022 

# What is the oldest nest date?
x = subset(nobo1, Location.Type=="Nest") # O9-21 oldest nest in 2023
max(x$Date) # 
y = subset(x, year == "2022")
max(y$Date)  # oct 19th is the oldest nest in 2022

# Subset the data into groups to work on them seperately then rbind back together 
# groups: Nests, broods, adults 
nests = subset(nobo1, Location.Type == "Nest")
nrow(nests) # 3377
unique(nests$Bird.Status)

broods = subset(nobo1, Location.Type == "Brood")
nrow(broods) #5496 
unique(broods$Bird.Status)

adults = subset(nobo1, Location.Type == "Regular")
nrow(adults) #22495
unique(adults$Bird.Status)

fck = subset(adults, Bird.Status == "Nest") # this was probably me -- logic was
nrow(fck)# this was probably me -- logic was the bird may be a nest, but it is off the nest and therefore it is a reg
# telemetry location. It still makes sense to me but maybe not the best way to setup our fieldmaps
# I think best course of action is to just rbind these specific birds into the 'nests' object 

nests = rbind(nests, fck)
nrow(nests) # 3439 nest locations 
adults = subset(adults, Bird.Status != "Nest") # perf 
 
boo = subset(adults, Bird.Status == "Brood")
head(boo) # alright, so here it appears we have broods with the location type as regular 
nrow(boo) # about 43 
# I am just going to rbind these to the 'broods' object and then take it out of the 'adults' object 
broods = rbind(broods, boo)
adults = subset(adults, Bird.Status !="Brood")

# check them again 
unique(adults$Bird.Status)
unique(broods$Bird.Status)
unique(nests$Bird.Status) # all suspected nests are nests (double checked it) 

# can I just assign a letter to each of these bad bois? Is it that simple? 
# Maybe....
# lets seperate the alive locations, the undetected locations, and the fates for each of these subsets 
# screams # 

#plan for forloop, using this I can then take the max # for the interval 
# to note: lets say there is a bird that is in the nest state one interval, it hatches during that interval, 
# but is found dead during the next interval during the brooding state 
# I believe MARK is automatically programmed to base the death off the previous state. 
# This might create a bias we want to think about 
# Question ----


# CHEAT SHEET CODING SEQ ---- 
# FIRST DIGIT: 1 = ADULT, 2 = NEST, 3 = BROOD
# SECOND DIGIT: 1 = ALIVE, 2 = DEAD 
# DEFAULT CENSOR: 00 

unique(adults$Bird.Status)

# birds tracked alive
adults_alive = subset(adults, Bird.Status == "Alive & Active" | Bird.Status == "Alive & Inactive" |
                         Bird.Status == "Alive - Mort Check Only" | Bird.Status == "Suspected Nest")
# birds tracked but not detected 
adults_undetected = subset(adults, Bird.Status == "Suspected Fate - RF" | Bird.Status == "Suspected Fate - DNH" | Bird.Status == "Suspected Fate - RIP") # 351 rows
# Birds tracked and fated: censored officially or dead 
adults_fate = subset(adults, Bird.Status == "Fate")

# Add the detection 
# Adults 
adults_alive$State = "A0"
adults_alive$encounter = "11"
adults_undetected$State = "00"
adults_undetected$encounter = "00"

unique(adults_fate$Fate)
adults_fate_censor = subset(adults_fate, Fate == "Censor") 
adults_fate_dead = subset(adults_fate, Fate != "Censor")
adults_fate_censor$State = "00"
adults_fate_censor$encounter = "00"
adults_fate_dead$State = "AA"
adults_fate_dead$encounter = "12"

adults = rbind(adults_alive, adults_undetected, adults_fate_dead, adults_fate_censor) # rbind it all back togetehr 
nrow(adults) # = 22390 

######################################################################################
# Broods 
unique(broods$Bird.Status) 

# birds tracked alive
broods_alive = subset(broods, Bird.Status == "Alive & Active" | Bird.Status == "Alive & Inactive" |
                        Bird.Status == "Alive - Mort Check Only" | Bird.Status == "Suspected Nest"| Bird.Status =="Brood")
# birds tracked but not detected 
broods_undetected = subset(broods, Bird.Status == "Suspected Fate - RF" | Bird.Status == "Suspected Fate - DNH" | Bird.Status == "Suspected Fate - RIP") 
# Birds tracked and fated: censored officially or dead 
broods_fate = subset(broods, Bird.Status == "Fate")

broods
broods_alive$State = "B0"
broods_alive$encounter = "31"
broods_undetected$State = "00"
broods_undetected$encounter = "00" 

unique(broods_fate$Fate)
broods_fate_censor = subset(broods_fate, Fate == "Censor")
broods_fate_dead = subset(broods_fate, Fate != "Censor")
broods_fate_censor$State = "00"
broods_fate_censor$encounter = "00"
broods_fate_dead$State = "BB"
broods_fate_dead$encounter = "32"

broods = rbind(broods_alive, broods_undetected, broods_fate_dead, broods_fate_censor) # rbind it all back togetehr 
nrow(broods) #6752 
######################################################################################
# Nests 
unique(nests$Bird.Status) 

# birds tracked alive
nests_alive = subset(nests, Bird.Status == "Alive & Active" | Bird.Status == "Alive & Inactive" |
                        Bird.Status == "Alive - Mort Check Only" | Bird.Status == "Suspected Nest"| Bird.Status =="Nest")
# birds tracked but not detected 
nests_undetected = subset(nests, Bird.Status == "Suspected Fate - RF" | Bird.Status == "Suspected Fate - DNH" | Bird.Status == "Suspected Fate - RIP") 
# Birds tracked and fated: censored officially or dead 
nests_fate = subset(nests, Bird.Status == "Fate")

nests
nests_alive$State = "N0"
nests_alive$encounter = "21"
nests_undetected$State = "00"
nests_undetected$encounter = "00"

unique(nests_fate$Fate)
nests_fate_censor = subset(nests_fate, Fate == "Censor")
nests_fate_dead = subset(nests_fate, Fate != "Censor")
nests_fate_censor$State = "00"
nests_fate_censor$encounter = "00"
nests_fate_dead$State = "NN"
nests_fate_dead$encounter = "22"

nests = rbind(nests_alive, nests_undetected, nests_fate_dead, nests_fate_censor) # rbind it all back togetehr 
nrow(nests) #3438
# okay now can I rbind  all of them? 
head(broods)
head(nests)

nobo2 = rbind(nests, broods, adults)
nrow(nobo2) #32580 okay all back to normal 

#############
# split the data ----
# subset the data for april 1st - Oct 22 
# this is the first day of breeding seaosn until the last day there was a recorded brood 
# or nest 

x <- nobo2[nobo2$Date >= "2023-04-01" & nobo2$Date <= "2023-10-21", ] #2023
nrow(x)
y <- nobo2[nobo2$Date >= "2022-04-01" & nobo2$Date <= "2022-10-21", ] #2022
nrow(y)
nobo3 = rbind(x, y)
nrow(nobo3) # 26695 pts included 


# look up which 4-day interval each observation falls into 
head(lookuptable1)
nrow(lookuptable1)
head(nobo3)#  combined date is month_day_year 
nobo3$day_month = nobo3$CombinedDate0 # create a matching column 
nrow(nobo3) #26695

df_merge <- merge(nobo3, lookuptable1, by ="CombinedDate") 
head(df_merge)
nrow(df_merge) # 26695 -- perfect 

# remove unnecessary columns 

nobo3 = dplyr::select(df_merge, "CombinedDate", "Date", "Bird.ID", "Bird.Status", "Location.Type", "x", "y", 
               "year.x", "State", "ordinal", "week", "intervals", "encounter")
head(nobo3)
#View(nobo3)
nrow(nobo3) 
unique(nobo3$intervals)

#### Double  check the fate column 
test = nobo3 %>%
  group_by(Bird.ID) %>%
  summarize(count = sum(Bird.Status == 'Fate'))
errors = subset(test, count > 1)# list of birds that are errors 

# I am going to combine year with bird ID. This will allow to differentiate between 
# breeding seasons easier and prevent issues with carry over birds (example: 166.346)
nobo3$Bird.ID = str_c(nobo3$Bird.ID, '_', nobo3$year.x)

# Alright now is the hard part, for every interval 


#### For() loop that creates input file
birdlist <- unique(nobo3$Bird.ID) # the list of birds to be processed
length(unique(nobo3$Bird.ID)) # how many? n = 694
nobo3 <- nobo3[order(nobo3$Date),] # reorder by year
#nobo3 <- nobo3[order(nobo3$intervals),] # then reorder by week
length(unique(nobo3$intervals)) #51
max(nobo3$intervals)
nrow(nobo3) #25976

# to check specific birds 
x = subset(nobo3, Bird.ID == "166.346_204659")
courses <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/shapefiles/OrtonCourses.shp")

# 53 intervals within the breeding season 

inp_df <- data.frame("birdID" = NA, 
                     "i1" = NA, "i2" = NA,
                     "i3" = NA, "i4" = NA, "i5" = NA, "i6" = NA,
                     "i7" = NA, "i8" = NA, "i9" = NA, "i10" = NA,
                     "i11" = NA, "i12" = NA, "i13" = NA, "i14" = NA,
                     "i15" = NA, "i16" = NA, "i17" = NA, "i18" = NA,
                     "i19" = NA, "i20" = NA, "i21" = NA, "i22" = NA,
                     "i23" = NA, "i24" = NA, "i25" = NA, "i26" = NA, "i27" = NA,
                     "i28" = NA, "i29" = NA, "i30" = NA, "i31" = NA, 
                     "i32" = NA, "i33" = NA, "i34" = NA, "i35" = NA, 
                     "i36" = NA, "i37" = NA, "i38" = NA, "i39" = NA,
                     "i40" = NA, "i41" = NA, "i42" = NA, "i43" = NA, "i44" = NA,
                     "i45" = NA, "i46" = NA, "i47" = NA, "i48" = NA, "i49" = NA,
                     "i50" = NA, "i51" = NA, 
                     "course" = NA, "year" = NA, "x" = NA, "y" = NA, 
                     "TrackedAlive" = NA)

int <- c(1:51) # make a "sequential" list of intervals 

#View(subset(nonbreeding1, Bird.ID == "166.346_204659")) # this bird is a good "test" bird

for(i in 1:length(birdlist)){ # for every bird in the bird list...
  #i = 119 # bird 119 , "164.646_220961 "
  bird_i_data <- subset(nobo3, Bird.ID == birdlist[i]) # subset data to get bird i's data
  bird_i_vals <- c() # make object to hold values
  # bird_i_data # print bird i's data if we want...
  
  for(j in 1:length(int)){ # for every week in the week list (NBweeks)...
    # this generates a detection history of sorts; 0 = not tracked, 1 = alive, 2 = dead, NA = not tracked
    #j = 47
    int_j = int[j] # pull week number from the sequential list above
    int_j_data <- subset(bird_i_data, intervals == int_j)
    val_j <- ifelse(nrow(int_j_data) == 0, NA, max(int_j_data$encounter)) # if no data, give NA, if data, take max of encounter
    bird_i_vals <- c(bird_i_vals, val_j)
    bird_i_vals
  }
  
  # add course
  meanX <- mean(bird_i_data$x) # average the x coords
  meanY <- mean(bird_i_data$y) # average the y coords
  mean_loc_i <- SpatialPoints(coords = data.frame("x" = meanX, "y" = meanY)) # convert DF to Spatial Points
  crs(mean_loc_i) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  mean_loc_i <- sp::spTransform(mean_loc_i, crs(courses))
  
  # plot(courses); plot(mean_loc_i, add = TRUE)
  course_i <- over(x = mean_loc_i, y = courses)
  course_i <- course_i$course 
  
  # add "YEAR"
  year_i = bird_i_data$year.x[1] # take the first value for Year
  
  # combine all pieces into a new row
  newrow <- data.frame("birdID" = birdlist[i], 
                       "i1" = bird_i_vals[1], "i2" = bird_i_vals[2],
                       "i3" = bird_i_vals[3], "i4" = bird_i_vals[4], "i5" = bird_i_vals[5], "i6" = bird_i_vals[6],
                       "i7" = bird_i_vals[7], "i8" = bird_i_vals[8], "i9" = bird_i_vals[9], "i10" = bird_i_vals[10],
                       "i11" = bird_i_vals[11], "i12" = bird_i_vals[12], "i13" = bird_i_vals[13], "i14" = bird_i_vals[14],
                       "i15" = bird_i_vals[15], "i16" = bird_i_vals[16], "i17" = bird_i_vals[17], "i18" = bird_i_vals[18],
                       "i19" = bird_i_vals[19], "i20" = bird_i_vals[20], "i21" = bird_i_vals[21], "i22" = bird_i_vals[22],
                       "i23" = bird_i_vals[23], "i24" = bird_i_vals[24], "i25" = bird_i_vals[25], "i26" = bird_i_vals[26], 
                       "i27" = bird_i_vals[27],
                       "i28" = bird_i_vals[28], "i29" = bird_i_vals[29], "i30" = bird_i_vals[30], "i31" = bird_i_vals[31], 
                       "i32" = bird_i_vals[32], "i33" = bird_i_vals[33], "i34" = bird_i_vals[34], "i35" = bird_i_vals[35], 
                       "i36" = bird_i_vals[36], "i37" = bird_i_vals[37], "i38" = bird_i_vals[38], "i39" = bird_i_vals[39],
                       "i40" = bird_i_vals[40], "i41" = bird_i_vals[41], "i42" = bird_i_vals[42], "i43" = bird_i_vals[43], 
                       "i44" = bird_i_vals[44],
                       "i45" = bird_i_vals[45], "i46" = bird_i_vals[46], "i47" = bird_i_vals[47], "i48" = bird_i_vals[48], "i49" = bird_i_vals[49],
                       "i50" = bird_i_vals[50], "i51" = bird_i_vals[51], 
                       "course" = course_i, "year" = year_i, "x" = meanX, "y" = meanY, 
                       "TrackedAlive" = sum(lengths(regmatches(bird_i_vals, gregexpr("1", bird_i_vals)))))
  
  
  # add new row to "inp_df"
  inp_df <- rbind(inp_df, newrow)
}
inp_df

# CHEAT SHEET CODING SEQ ---- AS A REMINDER 
# FIRST DIGIT: 1 = ADULT, 2 = NEST, 3 = BROOD
# SECOND DIGIT: 1 = ALIVE, 2 = DEAD 
# DEFAULT CENSOR: 00 

inp_df <- inp_df[2:(nrow(inp_df)), ] # remove the blank row at the top 

# Replace NA Values 
inp_df[is.na(inp_df)] <- "00"

# Replace based on specific values 
inp_df[inp_df=="11"]<- "A0"
inp_df[inp_df=="12"]<- "AA"
inp_df[inp_df=="21"]<- "N0"
inp_df[inp_df=="22"]<- "NN"
inp_df[inp_df=="31"]<- "B0"
inp_df[inp_df=="32"]<- "BB"

#remove birds with 0 tracked alive occassions 
nrow(inp_df)
x = sum(inp_df$TrackedAlive == 0)
inp_df <- subset(inp_df, TrackedAlive > 0) # 658 birds 

# seems right but should double check this ---- 
eh = paste0(inp_df[,2], inp_df[,3], inp_df[,4], inp_df[,5], inp_df[,6], inp_df[,7], inp_df[,8], inp_df[,9], 
           inp_df[,10], inp_df[,11], inp_df[,12], inp_df[,13], inp_df[,14], inp_df[,15], inp_df[,16], 
           inp_df[,17], inp_df[,18], inp_df[,19], inp_df[,20], inp_df[,21], inp_df[,22], inp_df[,23], 
           inp_df[,24], inp_df[,25], inp_df[,26], inp_df[,27], inp_df[,28], inp_df[,29], inp_df[,30], 
           inp_df[,31], inp_df[,32], inp_df[,33], inp_df[,34], inp_df[,35], inp_df[,36], inp_df[,37], 
           inp_df[,38], inp_df[,39], inp_df[,40], inp_df[,41], inp_df[,42], inp_df[,43], inp_df[,44], 
           inp_df[,45], inp_df[,46], inp_df[,47], inp_df[,48], inp_df[,49], inp_df[,50], inp_df[,51], inp_df[,52])
eh <- data.frame("eh" = eh)

pre_inp <- data.frame("birdid" = inp_df$birdID, "eh" = eh$eh)# adds bird id, encounter history, average covariates per individual, time varying covariates


new_inp <- data.frame("forMARK" = paste0("/*", pre_inp$birdid, "*/", # bird ID
                                         pre_inp$eh, " ", # encounter history
                                         1, ";"))
nrow(pre_inp)
# this might potentially be it 

# write.csv(new_inp, "E:/NOBO R Projects_Github/NOBO Survival/multistate.inp", row.names = FALSE)


