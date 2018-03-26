;;;=======================================================
;;; Matthew Stulz
;;; CSC 425
;;; Programming Assignment #2
;;; Due 3/1/17
;;;
;;; This expert system diagnosis's a person with depression
;;; and which of the possible 6 types that it could be. With
;;; multiple symptoms overlapping, here is where I used forward
;;; chaining to see what already was in working memory and
;;; if the symtpoms matched with symptoms that were already
;;; in working memory so I wouldn't have to prompt the user
;;; again.
;;; 
;;;=======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************

;Used to ask the user a question and bind the answer to a variable
;along with checking user input. If input isn'tyes, no, y, or n, the
;user continues to be prompted with the same question.
(deffunction ProposeQuestion (?question $?allowed-values)
	(printout t ?question)
	(bind ?answer (read))
	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))
	(while (not (member ?answer ?allowed-values)) do
		(printout t ?question)
		(bind ?answer (read))
		(if (lexemep ?answer)
			then (bind ?answer (lowcase ?answer))))
	?answer
)

(deffunction yesOrNo (?question)
	(bind ?response (ProposeQuestion ?question yes no y n))
	(if (or (eq ?response yes) (eq ?response y))
		then yes
		else no)
)

;;****************
;;* DEFTEMPLATES *
;;****************

;Used for a type of depression with a list of symptoms that match it.
(deftemplate Diagnosis
	(slot depression-id)
	(multislot input-symptoms)
)

;All symptoms are binded with a symptom-id to check for diagnosis.
(deftemplate Symptoms
	(slot symptom-id)
)

;;*******************
;;* DEFACTS/DEFULES *
;;*******************

;The Diagnosis needs to be added in the beginning with the corresponding symptoms but Symptoms will be added through getting user input.
(deffacts SymptomDiagnosis
	; depression type 0->6 and the symptoms are what needs to be
	(Diagnosis(depression-id 0)(input-symptoms 7 8 9 10 11 12 13 14 15 45 46))			;Major Depression Disorder
	(Diagnosis(depression-id 1)(input-symptoms 15 16 14 17 7 18 19 20 21 9 46))			;Seasonal Affective Disorder (Winter)			
	(Diagnosis(depression-id 2)(input-symptoms 15 22 10 11 46))					;Seasonal Affective Disorder (Summer)
	(Diagnosis(depression-id 3)(input-symptoms 46 47 12 14 7 8 20 23))				;Dysthymia
	(Diagnosis(depression-id 4)(input-symptoms 17 7 10 14 12 15 24 9 46 25 26 27))			;Atypical Disorder
	(Diagnosis(depression-id 5)(input-symptoms 7 8 12 9 10 28 14 15 29 30 24 33 34 46)) 		;Melancholic Depression
	(Diagnosis(depression-id 6)(input-symptoms 35 36 37 38 7 30 9 46 10 39 11 12 14 15 29 40 22 41 42 43 44)) ;Catatonic Depression	
	(match inProgress))

;Start of all the questions prompted to the user.
(defrule GetSadnessOrIrritability
	(not(sadOrIrritable ?))
	=> 
	(assert(sadOrIrritable(yesOrNo "Do you feel sad or irritable most of the day, nearly every day? (yes|no): "))))

(defrule GetInterestOfPastActivities
	(sadOrIrritable yes)
	(not(pastActivities ?))
	=>
	(assert(Symptoms(symptom-id 7)))
	(assert(pastActivites(yesOrNo "Are you feeling less interested in most activites you once enjoyed? (yes|no): "))))
	
(defrule GetInterestOfPastActivitiesNo
	(sadOrIrritable no)
	(not(pastActivites?))
	=>
	(assert(pastActivites(yesOrNo "Are you feeling less interested in most activites you once enjoyed? (yes|no): "))))
	
(defrule GetWeight
	(pastActivites yes)
	(not(weight ?))
	=>
	(assert(Symptoms(symptom-id 8)))
	(assert(weight(yesOrNo "Have you suddenly lost or gained weight? (yes|no): "))))
	
(defrule GetWeightNo
	(pastActivites no)
	(not(weight ?))
	=>
	(assert(weight(yesOrNo "Have you suddenly lost or gained weight? (yes|no): "))))
	
(defrule GetAppetite
	(weight yes)
	(not(appetite ?))
	=>
	(assert(Symptoms(symptom-id 46)))
	(assert(appetite(yesOrNo "Are you having issues with your appetite? (yes|no): "))))
	
(defrule GetAppetiteNo
	(weight no)
	(not(appetite ?))
	=>
	(assert(appetite(yesOrNo "Are you having issues with your appetite? (yes|no): "))))
	
(defrule GetSleep
	(appetite yes)
	(not(sleep ?))
	=>
	(assert(Symptoms(symptom-id 9)))
	(assert(sleep(yesOrNo "Do you have trouble falling asleep or want to sleep more than usual? (yes|no): "))))
	
(defrule GetSleepNo
	(appetite no)
	(not(sleep ?))
	=>
	(assert(sleep(yesOrNo "Do you have trouble falling asleep or want to sleep more than usual? (yes|no): "))))
	
(defrule GetRestlessness
	(sleep yes)
	(not(restlessness ?))
	=>
	(assert(Symptoms(symptom-id 10)))
	(assert(restlessness(yesOrNo "Are you currently experiencing feelings of restlessness? (yes|no): "))))
	
(defrule GetRestlessnessNo
	(sleep no)
	(not(restlessness ?))
	=>
	(assert(restlessness(yesOrNo "Are you currently experiencing feelings of restlessness? (yes|no): "))))
	
(defrule GetTiredness
	(restlessness yes)
	(not(tiredness ?))
	=>
	(assert(Symptoms(symptom-id 11)))
	(assert(tiredness(yesOrNo "Have you felt tiresome or having a lack of energy? (yes|no): "))))
	
(defrule GetTirednessNo
	(restlessness no)
	(not(tiredness ?))
	=>
	(assert(tiredness(yesOrNo "Have you felt tiresome or having a lack of energy? (yes|no): "))))
	
(defrule GetGuilt
	(tiredness yes)
	(not(guilt ?))
	=>
	(assert(Symptoms(symptom-id 12)))
	(assert(guilt(yesOrNo "Is there any feeling of guilt or worthlessness about things that wouldn't normally make you feel this way? (yes|no): "))))
	
(defrule GetGuiltNo
	(tiredness no)
	(not(guilt ?))
	=>
	(assert(guilt(yesOrNo "Is there any feeling of guilt or worthlessness about things that wouldn't normally make you feel this way? (yes|no): "))))
	
(defrule GetAttention
	(guilt yes)
	(not(attention ?))
	=>
	(assert(Symptoms(symptom-id 13)))
	(assert(attention(yesOrNo "Are you having difficulty concentrating, thinking, or making decisions? (yes|no): "))))
	
(defrule GetAttentionNo
	(guilt no)
	(not(attention ?))
	=>
	(assert(attention(yesOrNo "Are you having difficulty concentrating, thinking, or making decisions? (yes|no): "))))
	
(defrule GetSuicide
	(attention yes)
	(not(suicide ?))
	=>
	(assert(Symptoms(symptom-id 14)))
	(assert(suicide(yesOrNo "Have you ever thought about harming yourself or committing suicide? (yes|no): "))))
	
(defrule GetSuicideNo
	(attention no)
	(not(suicide ?))
	=>
	(assert(suicide(yesOrNo "Have you ever thought about harming yourself or committing suicide? (yes|no): "))))
	
(defrule GetPain
	(suicide yes)
	(not(pain ?))
	=>
	(assert(Symptoms(symptom-id 15)))
	(assert(pain(yesOrNo "Have you experienced pain without a clear cause? (yes|no): "))))
	
(defrule GetPainNo
	(suicide no)
	(not(pain ?))
	=>
	(assert(pain(yesOrNo "Have you experienced pain without a clear cause? (yes|no): "))))
	
(defrule GetLastSymptomForGDD
	(pain yes)
	=>
	(assert(Symptoms(symptom-id 45))))

(defrule GetWinterSeasonPattern
	(sadOrIrritable yes)
	(weight yes)
	(sleep yes)
	(restlessness yes)
	(attention yes)
	(suicide yes)
	(appetite yes)
	(not(winter ?))
	=>
	(assert(winter(yesOrNo "Are your symptoms more prevalant during the winter months of the year? (yes|no): "))))
	
(defrule GetSummerSeasonPattern
	(suicide yes)
	(sleep yes)
	(restlessness yes)
	(weight yes)
	(match inProgress)
	(not(summer ?))
	=>
	(assert(summer(yesOrNo "Are your symptoms more prevalant during the summer months of the year? (yes|no): "))))

(defrule GetDaytimeFatigue
	(winter yes)
	(not (daytimeFatigue ?))
	=> 
	(assert(daytimeFatigue(yesOrNo "Have you experienced time of fatigue during the daytime primarily? (yes|no): "))))
	
(defrule GetHopelessness
	(daytimeFatigue yes)
	(not(hopelessness ?))
	=>
	(assert(Symptoms(symptom-id 16)))
	(assert(hopelessness(yesOrNo "Do you have the feeling of hoplessness? (yes|no): "))))
	
(defrule GetSocialActivites
	(hopelessness yes)
	(not(socialActivities ?))
	=>
	(assert(Symptoms(symptom-id 17)))
	(assert(socialActivities(yesOrNo "Have you noticed a sudden lack in social activites? (yes|no): "))))
	
(defrule GetSocialActivitesNo
	(hopelessness no)
	(not(socialActivities ?))
	=>
	(assert(socialActivities(yesOrNo "Have you noticed a sudden lack in social activites? (yes|no): "))))	
	
(defrule GetLethargy
	(socialActivities yes)
	(not(lethargy ?))
	=>
	(assert(Symptoms(symptom-id 18)))
	(assert(lethargy(yesOrNo "Have you felt lethargic at all? (yes|no): "))))
	
(defrule GetSexualInterest
	(lethargy yes)
	(not(sexualInterest ?))
	=>
	(assert(Symptoms(symptom-id 19)))
	(assert(sexualInterest(yesOrNo "Is there a sudden decrease in sexual interest? (yes|no): "))))
	
(defrule GetNoSeasonal
	(winter no)
	(not(sexualInterest ?))
	=>
	(assert(sexualInterest(yesOrNo "Is there a sudden decrease in sexual interest? (yes|no): "))))
	
(defrule GetUnhappiness
	(sexualInterest yes)
	(not(unhappiness ?))
	=>
	(assert(Symptoms(symptom-id 20)))
	(assert(unhappiness(yesOrNo "Are you overwhelmed with unhappiness? (yes|no): "))))
	
(defrule GetUnhappinessNo
	(sexualInterest no)
	(not(unhappiness ?))
	=>
	(assert(unhappiness(yesOrNo "Are you overwhelmed with unhappiness? (yes|no): "))))
	
(defrule GetLastSymptomForSADWinter
	(unhappiness yes)
	=>
	(assert(Symptoms(symptom-id 21))))
	
(defrule GetAgitation
	(summer yes)
	(not(agitation ?))
	=>
	(assert(agitation(yesOrNo "Have you felt as if your life is full of agitation? (yes|no): "))))
	
(defrule GetLastSymptomForSAD
	(agitation yes)
	=>
	(assert(Symptoms(symptom-id 22))))
	
(defrule GetInHyperSomnia
	(appetite yes)
	(tiredness yes)
	(attention yes)
	(sadOrIrritable yes)
	(pastActivites yes)
	(sexualInterest yes)
	(not(inhypersomnia ?))
	=>
	(assert(inhypersomnia(yesOrNo "Are you experiencing any form of insomnia or hypersomnia? (yes|no): "))))
	
(defrule GetPhysicalSymptoms
	(inhypersomnia yes)
	(not(physicalSymptoms ?))
	=>
	(assert(Symptoms(symptom-id 47)))
	(assert(physicalSymptoms(yesOrNo "Have you experienced any unexplained physical symptoms? (yes|no): "))))
	
(defrule GetLastSymptomForDysthymia
	(physicalSymptoms yes)
	=>
	(assert(Symptoms(symptom-id 23))))
	
(defrule GetMoodChanges
	(sadOrIrritable yes)
	(sleep yes)
	(attention yes)
	(tiredness yes)
	(suicide yes)
	(weight yes)
	(appetite yes)
	(not(moodChanges ?))
	=>
	(assert(moodChanges(yesOrNo "Do you ever experience a mood that temporarily lifts or brightnes in response to positive events or good news? (yes|no): "))))
	
(defrule GetHeavyLimbs
	(moodChanges yes)
	(not(heavyLimbs ?))
	=>
	(assert(Symptoms(symptom-id 24)))
	(assert(heavyLimbs(yesOrNo "Do you have heavy feelings in the arms or legs? (yes|no): "))))
	
(defrule getHeavyLimbsNo
	(moodChanges no)
	(not(heavyLimbs ?))
	=>
	(assert(heavyLimbs(yesOrNo "Do you have heavyfeelings in the arms or legs? (yes|no): "))))
	
(defrule GetAches
	(heavyLimbs yes)
	(not(aches ?))
	=>
	(assert(Symptoms(symptom-id 25)))
	(assert(aches(yesOrNo "Are you experiencing any sort of body aches or headaches? (yes|no): "))))
	
(defrule GetResponse
	(aches yes)
	(not(response ?))
	=>
	(assert(Symptoms(symptom-id 26)))
	(assert(response(yesOrNo "Have you reacted with a extreme negative response to perceived critism or rejection? (yes|no): "))))
	
(defrule GetLastSymptomForAtypical
	(response yes)
	=>
	(assert(Symptoms(symptom-id 27))))
	
(defrule GetMovement
	(sadOrIrritable yes)
	(pastActivites yes)
	(tiredness yes)
	(weight yes)
	(sleep yes)
	(guilt yes)
	(attention yes)
	(suicide yes)
	(moodChanges yes)
	(not(bodyMovement ?))
	=>
	(assert(bodyMovement(yesOrNo "Are you experiencing changes in body movement such as jiggling your leg when you didn't before? (yes|no): "))))
	
(defrule GetSuicideAttempt
	(bodyMovement yes)
	(not(suicideAttempt ?))
	=>
	(assert(Symptoms(symptom-id 28)))
	(assert(suicideAttempt(yesOrNo "Have you ever attempted an act of suicide? (yes|no): "))))
	
(defrule GetDailyActivities
	(suicideAttempt yes)
	(not(dailyActivites ?))
	=>
	(assert(Symptoms(symptom-id 29)))
	(assert(dailyActivites(yesOrNo "Is there a loss of pleasure in all or most daily activites? (yes|no): "))))

(defrule GetSleepDisruptions
	(dailyActivites yes)
	(not(sleepDisruptions ?))
	=>
	(assert(Symptoms(symptom-id 30)))
	(assert(sleepDisruptions(yesOrNo "Have you noticed any sort of sleep disruptions including sleep disorders? (yes|no): "))))
	
(defrule GetMorning
	(sleepDisruptions yes)
	(not(morning ?))
	=>
	(assert(Symptoms(symptom-id 33)))
	(assert(morning(yesOrNo "Are your symptoms worse in the morning? (yes|no): "))))
	
(defrule GetLastSymptomForMelancholic
	(morning yes)
	=>
	(assert(Symptoms(symptom-id 34))))
	
(defrule GetStayingStill
	(sadOrIrritable yes)
	(weight yes)
	(sleep yes)
	(appetite yes)
	(restlessness yes)
	(tiredness yes)
	(attention yes)
	(suicide yes)
	(suicideAttempt yes)
	(dailyActivites yes)
	(not(stayingStill ?))
	=>
	(assert(stayingStill(yesOrNo "Are you noticing you're staying in the same position for long periods of time? (yes|no): "))))
	
(defrule GetSpeech
	(stayingStill yes)
	(not(speech ?))
	=>
	(assert(Symptoms(symptom-id 35)))
	(assert(speech(yesOrNo "Have you noticed the lack of desire to talk or communicate? (yes|no): "))))
	
(defrule GetFastMovements
	(speech yes)
	(not(fastMovements ?))
	=>
	(assert(Symptoms(symptom-id 36)))
	(assert(fastMovements(yesOrNo "Are your movements very quick and awkward? (yes|no): "))))
	
(defrule GetAbnormalMovements
	(fastMovements yes)
	(not(abnormalMovements ?))
	=>
	(assert(Symptoms(symptom-id 37)))
	(assert(abnormalMovements(yesOrNo "Have you noticed your movements to be a bit abnormal or unusual? (yes|no): "))))
	
(defrule GetOutOfBed
	(abnormalMovements yes)
	(not(outOfBed ?))
	=>
	(assert(Symptoms(symptom-id 38)))
	(assert(outOfBed(yesOrNo "Do you have trouble getting out of bed in the morning? (yes|no): "))))
	
(defrule GetNegativism
	(outOfBed yes)
	(not(negativism ?))
	=>
	(assert(Symptoms(symptom-id 39)))
	(assert(negativism(yesOrNo "Have your noticed yourself being extremely negative? (yes|no): "))))
	
(defrule GetAbilityToMove
	(negativism yes)
	(not(ability ?))
	=>
	(assert(Symptoms(symptom-id 40)))
	(assert(ability(yesOrNo "Is there times where you lack the ability to move at will? (yes|no): "))))

(defrule GetSpeaking
	(ability yes)
	(not(speaking ?))
	=>
	(assert(Symptoms(symptom-id 41)))
	(assert(speaking(yesOrNo "Are you having trouble speaking due to extreme anxiety? (yes|no): "))))
	
(defrule GetImitating
	(speaking yes)
	(not(imitating ?))
	=>
	(assert(Symptoms(symptom-id 42)))
	(assert(imitating(yesOrNo "Is there moments where you are imitating another person'sspeech or movements? (yes|no): "))))
	
(defrule GetRefuse
	(imitating yes)
	(not(refuse ?))
	=>
	(assert(Symptoms(symptom-id 43)))
	(assert(refuse(yesOrNo "Are you refusing to eat or drink? (yes|no): "))))
	
(defrule GetLastSymptomForCatatonic
	(imitating yes)
	=>
	(assert(Symptoms(symptom-id 44))))

;Loops through all the symptoms that have been added to working memory to see if there
;is a match with the facts that were added at the beginning.
(defrule All-symptoms
	(Diagnosis(depression-id ?id))
	(forall(Diagnosis(depression-id ?id)(input-symptoms $? ?symptom $?))
		(Symptoms (symptom-id ?symptom))) 
	=>
		(assert (matchID ?id)))

;If there is an id that matches, print out the diagnosis to the user.		
(defrule MDD
	(matchID 0)
	=>
	(printout t "Depression diagnosis is Major Depression Disorder" crlf)
	(assert(match yes)))
			
(defrule SADW
	(matchID 1)
	=>
	(printout t "Depression diagnosis is Season Affective Disorder (Winter)" crlf)
	(assert(match yes)))
	
(defrule SADS
	(matchID 2)
	=>
	(printout t "Depression diagnosis is Season Affective Disorder (Summer)" crlf)
	(assert(match yes)))
	
(defrule DYS
	(matchID 3)
	=>
	(printout t "Depression diagnosis is Dysthymia Depression" crlf)
	(assert(match yes)))
	
(defrule ATY	
	(matchID 4)
	=>
	(printout t "Depression diagnosis is Atypical Depression" crlf)
	(assert(match yes)))
	
(defrule MEL	
	(matchID 5)
	=>
	(printout t "Depression diagnosis is Melancholic Depression" crlf)
	(assert(match yes)))
				
(defrule CAT	
	(matchID 6)
	=>
	(printout t "Depression diagnosis is Catatonic Depression" crlf)
	(assert(match yes)))	
	
(defrule startUp
	(match inProgress)
	=>
	(printout t "Thank you for choosing this expert system. Please answer the following questions." crlf))
	
;If there isn't a diagnosis that matches the users symptoms. Using salience will print it out last as a default.	
(defrule NoMatches
	(declare(salience -1))
	(match inProgress)
	=>
	(printout t "Your symptoms don't completely match a diagnosis, Try seeking out a human expert." crlf))


