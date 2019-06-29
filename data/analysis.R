#######################################
#Cerberus Online Survey Data Analysis
#######################################
#######################################
#Very low counts...four responses total
#######################################
#interpret results accordingly ;)
#######################################

cerberus = read.csv("Cerberus_online_survey_data.csv")

#take a look at responses...
#except flag questions (Q2/Q4) 
#and open-ended (Q8/Q9/Q10) for now
xtabs(~Q1, data=cerberus)
xtabs(~Q3, data=cerberus)
xtabs(~Q5, data=cerberus)
xtabs(~Q6, data=cerberus)
xtabs(~Q7, data=cerberus)

#there were not surprises for Q4_x...
#therefore, nothing to analyze/report there for now ;)

#zero really drags avg down...
mean(cerberus$Q3)

#Q3 has fairly diverse responses...
#and everyone seems to have varying
#degrees of feature use, so....
#create breaks for profiling
cerberus$featureSum = rowSums(
  cerberus[
    ,
    c(
    "Q2_1", 
    "Q2_2", 
    "Q2_3", 
    "Q2_4", 
    "Q2_5", 
    "Q2_6", 
    "Q2_7", 
    "Q2_8"
    )
  ], 
  na.rm = TRUE
)
xtabs(~featureSum, cerberus)

cerberus$featureCat = ifelse(
  #lower use...
  cerberus$featureSum < 3,
  1,
  ifelse(
    #medium use...
    cerberus$featureSum == 3,
    2,
    ifelse(
      #higher use...
      cerberus$featureSum > 3,
      3,
      NA
    )
  )
)
xtabs(~featureCat, cerberus)

cerberus$homeCat = ifelse(
  #home less often...
  cerberus$Q5 < 50,
  1,
  ifelse(
    #home somewhat often...
    cerberus$Q5 %in% c(50:75),
    2,
    ifelse(
      #home quite often...
      cerberus$Q5 >= 76,
      3,
      NA
    )
  )
)
xtabs(~homeCat, cerberus)

#categorize devices...
cerberus$deviceProfile = ifelse(
  #cheap...
  cerberus$Q1 == 1,
  1,
  #not cheap...;)
  2
)
xtabs(~deviceProfile, cerberus)

#personas...pretty subjective
#because of the low counts!
#kind of smooshing people into
#my previously arrived-at conclusions
#but using different measurements
#NOT ideal...not really good analysis either
#more participation might have helped ;)
#at any rate - my theoretical framework
#from structured interview data (also few participants)
#is somewhat at odds with the survey data
xtabs(~featureCat+homeCat+deviceProfile, cerberus)

cerberus$persona = ifelse(
 #practical busybody...cheap device, happens to also be home a lot
 #meh...kind of weak since they use a lot of features
 cerberus$deviceProfile == 1 & cerberus$homeCat == 3,
 1,
 ifelse(
   #sophisticated shut-in (better device, home a lot, uses a lot of features)
   #this jives well...
   cerberus$deviceProfile == 2 & cerberus$homeCat == 3 & cerberus$featureCat == 2,
   2,
   ifelse(
     #sophisticated socialite (better device, not home so much)
     #oddly, they are in cat 1/2 for feature use...not ideal
     cerberus$deviceProfile == 2 & cerberus$homeCat %in% c(1:2) & cerberus$featureCat %in% c(1,2),
     3,
     NA
   )
 )
)

xtabs(~persona, cerberus)

#how do personas stack up on recommendations?
xtabs(~persona+Q3, cerberus)

#how do personas stack up on visits?
xtabs(~persona+Q6, cerberus)
xtabs(~persona+Q7, cerberus)
#nothing interesting here...

#found some support for features in open-ended:
#add feature for silence when the customer wants no disturbances 
#(for example pets/children going nuts when the doorbell rings...)
#support for high-end devices that include local storage...
#nobody is using the cloud feature:
xtabs(~Q2_5, cerberus)
#womp...womp...