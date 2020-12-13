extensions[csv]
breed [houses house]

patches-own[
  mortgage ; Mortgage for each patch is representative of mortgage credit avaliability for nieghborhood.
  norfolk_owned
  empty ; boolean variable representing avaibility of mortgages in The Area, if true removes mortage credit
  bid_failures ; a counting variable: the number of times norfolk offer is less than that of the agents unique valaution
]

houses-own[
  valuation ;  how much the agent representing a household evalautes their property
  monetary_valuation ; value representing the monetary valuation of their property
  monetary_quality ; value representing the monetary valuation of their property after accounting for the rest of the market
  soc_valuation ; value representing the evaluation of a household's social network
  soc_quality ; value representing the evaluation of a household's social network after accounting for the rest of the neighborhood
  tenure ;  how long a household  has had a particular  mortgage
  social_preference ;  a weight of continuous value between 0 and 1 representing the importance of social ties to a household
  network_threshold ; the amount of the network quality that signals the agent to want to sell. calcautated from intial network assessment.
  network_quality ; household agent valuation of their network
  unique_valuation  ; variable for the final valuation of the household in one round of bidding to be compared with the buyer's offer
  flagged ; boolean varible marking  whether or not the agent has been offered a bid
  initial_valuation ; holds the initial value of the mortgage for the underlying patch
]

links-own [] ; no link variables for current version

globals[
  ;########## Setup and output vars #############
  csv ; a data object that holds imported mortgage data
  patch_data ; list of mortgage infomation from the csv data object
  patch_data2 ; a back up list for sampling
  high_value ; weight to modify the sampple distribution of the high patch data variable in dist_list
  med_value ; weight to modify sample distirbution of med patch data variable in dist_list
  low_value ; weight to modify sampel distrbution of low patch data variable in dist_list
  dist_list ; A list of sampled mortgages
  gamma_list ; list of mortgage values pulled from gamma dist
  mortgage-buyout-ratio ; a variable for plotting the ratio of norfolk's buyout price to the underlying patch mortgage value

  ;############## Buyer values #########################
  norfolk_offer ; the offer calculated from our utility funciton that is compared to a houseshold's unique vlauation variable
  interest_rate ; could represent change in  perception of area due to development. The value could be positive or negative. For this model the interest rates are held at a constant of zero
  blocklist; a list of agent repreenitn household that will compare norfolk offer to it's unique valutions.
  non-normalized-norfolk-offer ; a variable storing the non-normalized norfolk offer so that it can be used to find the ratio of the offer to the mortgage
  success-flag

  ;##################### file system ############################
  counter ; counter for file system
  m_counter ; coutner for meta file ssytem
  active_file ; a place to hold file names
  meta_file ; hold metafiel names
  hold_out_ratio ; holds the ratio of refused bids to the total number of houses that have rejected bids, this variable represents how resistant the collective households are to being bought out
  success-total ; holds total number of successful bids
  bid_refused_total ; holds the number of refused bids
]

;;;;;;;; Patch  Operations ;;;;;;;;;;;;;;;;;;;;;;;;

to value_loss ; sets the color of the credit impacted patches
  ask patches [if empty = True
                [set mortgage  0 ; to represent that a patch has no available credit
      set pcolor black
     ]]
end

to patch_effects ; mj
  value_loss
  ask patches [ set pcolor scale-color green mortgage 110000 350000] ;MJ
end

;;;;;;;;;;;;;;;;;;;;;;Standard Distribution ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup_patches ;MJ initializes the patches, giving each patch a mortgage value that is used by turtles to determine their unique valuations
  ; vacancies are implemented after the resident investment is set, and the then empty properties have their mortgage reduced to 1
  clear-patches
 ; ask patches [set mortgage one-of patch_data]
  ;ask patches [set patch_data remove mortgage patch_data]

  loop [ ifelse (any? patches with [mortgage = 0])
        [ ifelse (empty? patch_data  )
          [ask one-of patches with [mortgage = 0] [set mortgage one-of patch_data

            set patch_data remove-item (position mortgage patch_data) patch_data


           ]] ;removes already assigned mortgage ---needs to iterate

           [set patch_data (patch_data2)
            ask one-of patches with [mortgage = 0] [set mortgage one-of patch_data ; how to remove item from list after selection
            set patch_data remove-item (position mortgage patch_data) patch_data]]

       ]
       [stop]
       ]
  ;( 0.01 * ( abs min-pxcor * 1.414 ) ) ( 0.99 * ( abs min-pxcor * 1.414 ) )]

end
;;;;;;;;;;;;;;;;;;;;;;;; xxxxxxxxxxxxxxxxxxxxxxxxxxxx ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;Patch Modification  OPERATIONS  ############################################################################

to set_data_import
   set patch_data []
  ifelse(file-exists? "full_dist.txt")
    [
      file-open "full_dist.txt"  ; txt file easier than csv
      set csv csv:from-file "full_dist.txt" ; imporper funciton usage but it works
      set patch_data  one-of csv ; imporper funciton useage  but it works
      set patch_data2 patch_data

    ;print csv

    file-close]
  [user-message "There is no distribution file"]
end




;;; Gamma Distribtuion modifier --generate a range of disitrbution mirroring then contorting as we change gui parameters.

to gamma_dist_generation
 set gamma_list []
 let instance_count  0
 ; print count patches  with [empty != True]
  loop [ ifelse (count patches with [mortgage = 0] > instance_count + 100)
    [ print "gamma list creation"
      set gamma_list lput (random-gamma ((mean_mortgage ^ 2 / mort_stnd_dev ^ 2 )) (1 / ((mort_stnd_dev ^ 2) / mean_mortgage ))) gamma_list
        ;  this should take the random gamme input for alpha =  and lamba =
      set instance_count instance_count + 1 ]

      [print "stopped" stop]
       ]
 ;print gamma_list
  ; this shoould create a list of a unified gamma disotrbution for every patch that has not been assigned empty
 ; improvement  to old code in that it relies on interna list generation
  ; instance count also  coutns the number of patches that are on the baord


end

to gamma_patch_set_up
  clear-patches
  loop [ Ifelse(any? patches with [mortgage = 0])
       [ ifelse (empty? gamma_list )
          [ask one-of patches with [mortgage = 0] [set mortgage one-of gamma_list]
             ask patches [set gamma_list remove-item (position mortgage gamma_list) gamma_list ]]

      [set gamma_list lput (random-gamma ((mean_mortgage ^ 2 / mort_stnd_dev ^ 2 )) (1 / ((mort_stnd_dev ^ 2) / mean_mortgage ))) gamma_list
        ask one-of patches with[mortgage = 0] [set mortgage one-of gamma_list
        set gamma_list remove-item (position mortgage gamma_list) gamma_list ]]
    ]
    [stop]
  ]
        ; like a deck of cards  thi shoud pull a gamma list value and remove that unique instance of said value from dist list to have selection with exclusion
  ; could not be working  if the instance of the mortgage call and assignment is not the same as removal
  ;  SD = 31,048,
end
;;;;;;;;;;;;;;;; NETWORK Structure  OPERATIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to  prefer_network
  ; preferential attachment network development, from the Preferential Attachment Network Simple Example in the  model library
  ask one-of patches with [empty != true ][ sprout-houses 1 ]
  ask one-of patches with [empty != true and any? turtles-here = false] [ sprout-houses 1 ]
  ask  house 0  [create-link-with  house 1]
  loop [ ifelse (count houses < residential_density)
             [let partner one-of [both-ends] of one-of links
                  create-houses 1 [
                          move-to one-of patches with [empty != true and any? turtles-here = false]
        create-link-with partner ]
    ] [stop]
  ]

end

to random_network ; builds a random network among the agents representing households
  ask n-of residential_density patches with [empty != true and any? turtles-here = false] [sprout-houses 1]
  ask houses [let other-house random (count houses) if (other-house != [who] of self)[ create-link-with house other-house]]

end

to colorize_houses ;color by number of network connections
   ask houses[
                if ((count my-out-links) = 0) [set color gray]
                if ((count my-out-links) > 0)[set color red]
                if ((count my-out-links) >= 2)[set color yellow]
                if ((count my-out-links) >= 4) [set color blue]
  ]
end


;##################################### Network Quality;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to network_q_effect
      ask houses [set network_threshold (count my-out-links * tenure * social_preference) ]

end

to social_preference_value ;;;; ### add switch for homogenous vs heterogenous social preference;;;
  ; assign vlaue to socal preference variable the will determien network quality for agents, soical preference value is determined my social-inteisity how postive or negative this relaitoships is
  if social_type = "Hetero"
  [  ask  houses[set social_preference  precision (random-float social_affinity * .01)2]  ]
  if social_type = "Homo"
  [  ask  houses[set social_preference  precision (social_affinity * .01)2]  ]
  ; remove social density as a controller as it only allow to shirnk or expand number of houses with a valaution of any sort
   ; social intensity is a measure of socialiability--postive threshold-- the higher the number the gernally more content everyone is
end


to social_network_assessment ; this is for nested desionc makings
  ask houses[
    set network_quality ((count my-out-links)* (tenure + ticks) ) ] ; create a unique network quality of tenure, tick link and random value on social_preference scale
end

to set_social_valuation
  set soc_valuation ((count my-out-links) * (tenure + ticks))
end

;;;;;;;;;;;;;;;;;;;; Monetary_operations::::::::::::::::::::::::::::::::::::::::::::

to set_monetary_valuation ; creates mortage with interest inter action
  set monetary_valuation (( mortgage + mean[mortgage] of patches with [empty != true and norfolk_owned != true] in-radius 1)/ 2 *(1 + (interest_rate * .01)))

end
;############## attributes of houses in network adn network interacting variables##################################

to turtle_initial_attributes ; update valuation to be  intial-valaution
  ask houses [set initial_valuation ( mortgage + mean[mortgage] of patches with [empty != true and norfolk_owned != true] in-radius 1)/ 2]
  ask houses[set tenure(1 + random 70)]
  social_preference_value
  social_network_assessment
  social_preference_value ; assigns social preference as dummy variable - can now assing percentage of true values

  ; [UPDATES JUNE 3RD --- have postive and negative intervest rate that is change step wise -----...... this valaution will be normalized by maximum of houses at a particualr time-step  ]
  ; July 12- new intial attributes   sets tenure, sets social prefernce that impacts social_valuation, SNA fucniton sets up network quality for nest fucniton
end




;;;;;;;;;;;;;;;;;;;;;;;;;; Houses  & Network Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup_houses
  ;# Network Buidling  both random and preferential #
   ifelse prefered_network = True
      [prefer_network]
      [random_network]
  network_q_effect
  colorize_houses
  turtle_initial_attributes
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEY FUNCTIONS FOR INQUIRY IN SOCIAL & DEVELOPMENT QUESTIONS

to linear_res_valuation ; new resident valuation aka WILLINGNESS TO PAY  is used networkd valution of linked houses averaged out with central resdient investment  in surruonding (mortgage investment)area
  ask houses [
     if network_quality = 0 [stop]
;Step 1 valuation with-out networked effects/linked nieghhors considerations ---- there is an issue with when and where to normalize!!!!!
    ; step 1a monetary valution takes  mortge mean moritgae *  interest rate increase   will be impacted by seconday effect splits up step 1 ....
    set_monetary_valuation ; interest intergrated and works on the assumption houses will havea YOY increase or decrese in monetary vaulautuion
           ;set monetary_valuation ((1 - social_preference )*( mortgage + mean[mortgage] of patches with [empty != true and norfolk_owned != true] in-radius 1)/ 2) ; contains mortgage assestment based on limited sample of available mortgages -- average is used hen adust by interest rate
      ;step 1b create social valutation
    set_social_valuation ;socialprefrence intergrated into "set_social_valuation" function
    ]
    ;Componest are normalized below --this assumes a linear normative relaitonships amoong dynamics
    ; step 1a-a
  ask houses[
     set monetary_quality (monetary_valuation / max [monetary_valuation] of houses)
     ; step 1b-a
     if max [soc_valuation] of houses > 0
        [set soc_quality (soc_valuation / max [soc_valuation] of houses)]
     ]

  ask houses [
    ;step 1c  create valuation from  social value and montary valuae funcitonign independantly
     set valuation  ((1 - social_preference) * monetary_quality) + (social_preference * soc_quality)  ;create updated network quality impacts, mortgage and interest interaction per iteration
    ; operates on the assumption that there is an expectation of YOY change in  valuation given adequate "soical cohesion Interest rate"
     ]

;Step 2 valuation  -- valuation here from above  plus added network and stats space. all normalized
 ask houses [
  ifelse any? in-link-neighbors
        [set unique_valuation ((((valuation + (mean[valuation] of link-neighbors)))/ 2))] ;network quality is consider through linke neighbor valuation now better off or worse off neighbori nfleince agent decison making
        ; doesn't capture negative interest rates [set unique_valuation (((( monetary_valuation + (mean[monetary_valuation] of link-neighbors)))/ 2)/ max [monetary_valuation] of houses)  + (soc_valuation / max[soc_valuation] of houses)]
         [set unique_valuation valuation];thiscreate a valuation without network impacgts
            ]
end

; own valution with out nieghbors  without based inital invesmtnet  + quality of network-- initial
; average own valauiton + niehgborhood radius 1 +  network quality
;Code updates
           ;initial investment,  step 1 valuaiton without neihgbords(add network quality--this make it udate overtime) , average of valution with links + radius 1
           ;  step 1 valuation w/o nieghbordh ( add network quality) _=== is compost of "last steps valution" still add network quality ( in function)
; net step 1 funciton for  resdient invesemnt  valuation = $ x (1 - social_preference) + network_quality x social_preference  / max valuation

; updated step 1 - last step valuation coming forward--postive growth, concern about normalization here
; update step 2 in june 10th model step 2 uses previous unique valution when they have no more niehgbors to consutl


; June 3rd notes
; own valution with out nieghbors  without based inital invesmtnet  + quality of network-- initial
; average own valauiton + niehgborhood radius 1 +  network quality
;Code updates
           ;initial investment,  step 1 valuaiton without neihgbords(add network quality--this make it udate overtime) , average of valution with links + radius 1
           ;  step 1 valuation w/o nieghbordh ( add network quality) _=== is composed of "last steps valution" still add network quality (in function)
            ; next steps for   valuation = $resdient invesemnt  x (1 - social_preference) + network_quality x social_preference  / max valuation

to nested_resident_valuation ;

   ask houses [
               if network_quality = 0 [stop]
               set_monetary_valuation ; interest intergrated and works on the assumption houses will havea YOY increase or decrese in monetary vaulautuion
               set monetary_quality (monetary_valuation / max [monetary_valuation] of houses)
               set_social_valuation ;socialprefrence intergrated into "set_social_valuation" function
               if max [soc_valuation] of houses > 0
                  [set soc_quality (soc_valuation / max [soc_valuation] of houses)]


               set valuation  (((1 - social_preference) * monetary_quality) + (social_preference * soc_quality))  ;create updated network quality impacts, mortgage and interest interaction per iteration
                                     ;monetary preference = 1 - social preference

               ifelse social_preference > 0 ; network threshold - intial network qulaity = network threshold
                   ; have network quality multipled by social preference in  inequality with network threshold ---- if bleow threshodl engae in linear bid.  (<=)
                     [
                        ifelse ((network_threshold - network_quality ) <= 0) ; really assess network pressure, once pressure out wiegh intial threhold quality has to be bigger than threshold
                          [ifelse any? in-link-neighbors
                               [set unique_valuation ((((valuation + (mean[valuation] of link-neighbors)))/ 2)) ] ; change by mutliplying
                               [set unique_valuation ((((valuation + unique_valuation)/ 2)))]]
                          [set unique_valuation ("no bid")]
                     ]

                    [ ifelse any? in-link-neighbors
                         [set unique_valuation ((((valuation + (mean[valuation] of link-neighbors)))/ 2))] ;network quality is consider through linke neighbor valuation now better off or worse off neighbori nfleince agent decison making
                         [set unique_valuation valuation];thiscreate a valuation without network impacgts
                    ]

  ]
  ;print valuation
end

to strategy_select
  if resident_strategy = "Linear"
        [linear_res_valuation ]; resident assess housing value
  if resident_strategy = "Nested"
        [nested_resident_valuation ]
end


;################################################## NOrfolk Operations#########################################################################

to norfolk_bid ; bid function is inside of search function

  set mortgage-buyout-ratio (non-normalized-norfolk-offer / monetary_valuation);x)] ; #mj
  buyout-output

  ifelse  unique_valuation = "no bid"
          [ask patch-here [set bid_failures bid_failures + 1 print "Failed bid" print bid_failures]]
  [ifelse norfolk_offer > unique_valuation ; ife else statemen that if residnet vlaaution higher thatn norfolk add to bid failure
    [
      set success-flag 1
      set success-total success-total + 1

      ask patch-here[
        set mortgage-buyout-ratio (non-normalized-norfolk-offer /(first [monetary_valuation] of houses-here));x)]
        buyout-output
        set norfolk_owned  True
        ask neighbors[if norfolk_owned != true and impact_effects = true [secondary_impacts]] ;valuation change as oppose mortgage--- stretch work is to add specualtive aspect to  work.  ask agent to behave in specualtive fashion.
                                                                    ; update montery-valuaiton of nieghborhood patches in seconday impacts
        set pcolor blue
        ;if impact_effects = true []
        ask houses-here [die]
      ]
      set bid_failures 0
    ]
    [
      set bid_refused_total bid_refused_total + 1
      set bid_failures bid_failures + 1 print "Failed bid: " print bid_failures
      set success-flag 0
    ]
  ]


  print "Norfolk bid occured"
  print "Norfolk offer"
  print norfolk_offer
  set-current-plot "Purchase Price to Household Valuation Ratio" ; the Mortgage* here refers to the monetary valuation, as that value is a composite variable that is much more robust adn refelctive on our intent to show purchasing value to  house valuation
  plot mortgage-buyout-ratio
end


to norfolk_valuation ; offer_adjustment * bid failure can be changed to be a percentage interaction of specfic houses offer
  set norfolk_offer (((mortgage * (.01 * offer_adjustment ) * bid_failures) + mean[mortgage] of patches with [norfolk_owned != True and empty != True] )/ max [mortgage] of patches) ; cleaned up aspects and normali
  set non-normalized-norfolk-offer (offer_adjustment * bid_failures) + mean[mortgage] of patches with [norfolk_owned != True and empty != True]
  print "norfolk offer"
  print norfolk_offer
end


to norfolk_bid_search
  if (any? houses = true) [
    repeat blockSize [
      if any? houses with [flagged = 0] [ask one-of houses with [ flagged = 0 ] ; create block list of 'flagged' houses
      [ set flagged 1 ]; house selection will be random, I don't think that proximity is going to be a determinign factor for block list
      ]
    ]
  ] ; this create an accountingg of instacnes of events---outside of the ticks

  set blockList houses with [flagged = 1]

  ask blockList [
    norfolk_valuation
    norfolk_bid
  ] ; this is asking things concurrently
end

to secondary_impacts
  ask neighbors[if norfolk_owned = true [set mortgage mortgage - (mortgage * (.01 * negative_impact))]] ; changed so valaution of nieghbors-agent valution is decreased.
  ; change of variable name: set monetary_quality  monetary_quality * (1 - negative_impact) , now negative impact now percentage between 0-> 5%
  ; update monteary valuation 8 nieghbors that are occupied
end

to define_hold_out_ratio
  let x  bid_refused_total / count houses with [ bid_failures > 0]

end

;##################### Graph Plotting ##################################
to do-plotting
  set-current-plot "Mortgage Distribution" ; is there a way to make diffent buckets groups bucket 1 1 and 221100, 221101 and 252149, 252150+  , SD = 31,048
  let minimum_x min [mortgage] of patches with [mortgage > 1]
  set-plot-x-range minimum_x (max [mortgage] of patches)
  set-plot-y-range 0 count patches with [mortgage > 1]
  set-histogram-num-bars 5
  ; ###
  set-current-plot "Network Distribution"
  set-plot-x-range 0 max [count link-neighbors] of houses
  set-plot-y-range 0 count houses
  set-histogram-num-bars 20

  set-current-plot "Hold Out Ratio"
  set-plot-y-range 0 7
  ; ###
end


;################## file output#################################

to check_file
  let date-time  date-and-time
  set date-time remove ":" date-time
  show date-time
   ifelse(file-exists? (word "simulation_output.txt" ))
           [ set counter  counter + 1
            file-open (word "simulation_output"date-time counter".txt")
             set active_file (word "simulation_output" date-time counter".txt") ]
           [ file-open "simulation_output.txt"
             set active_file "simulation_output.txt"                 ]

end


to write_meta_file
  ifelse(file-exists? (word "meta_data.csv"))
           [ set m_counter  m_counter + 1
              file-open (word "meta_data"active_file".csv")
               set meta_file (word "meta_data"active_file".csv")     ]
            [ file-open(word"meta_data.csv")
             set meta_file (word "meta_data.csv")                      ]

 file-open (word meta_file)
  ;file-print ("t icks mortgage-buyout-ratio non-normalized-norfolk-offer success-flag success-total")
  file-write csv:to-row
  (list
    "blocksize"int (blocksize)
    "resident_strategy"resident_strategy
    "social_affinity"social_affinity
    "social_type" social_type ; TRUE means the social preference calculations include a stochastic element, FALSE means that every home has the same value for social preference
    "residential_density"residential_density
    "negative_impact"negative_impact
    "Impact_effects" impact_effects
    "network_pref" prefered_network
    "residential_density" residential_density
    "distrubution" distribution
    "Offer"  offer_adjustment
    "social_type" social_type
    "mean_mtg" mean_mortgage
    "std_dev" mort_stnd_dev
    )
  ; updated 10/28 to add more values to the meta file for tracking and differtiation

  file-close
end
; above here is stuff we want we ahve to do the let x be various values

to  write_header
  file-open(word active_file)
   file-write csv:to-row
   (list
     "ticks" "mortgage-buyout-ratio" "soc_quality" "monetary_valuation" "non-normalized-norfolk-offer" "success-flag""success-total" "houses" "Houses_network_4_plus" "Houses_network_3" "Houses_network_2" "Total_Links" "hold_out_ratio" "initial_val"
     )
end



to buyout-output ; appends to the mortgage buyout file to track the ratio over time (by tick) and when bids were successful
 file-open (word active_file)
 ; let x  insert-item 0 ["tick"]  ticks
 ; file-write behaviorspace-run-number
  ask houses-here [
                    let y soc_quality
                    let m monetary_valuation
                    let h initial_valuation  ;EJ
                    let neiih "hold"
                    let lih "hold"
    ask neighbors4[ifelse (norfolk_owned = true or empty = true) ;  fix nieghbor call to asked houses to ask patches  #feb_1
                                                     [set neiih true ] ; neighbor isolated houses
                        [set neiih false]]
                    ifelse  count link-neighbors = 0
                        [set lih true ] ; link isolated houses
                        [set lih false]
    if (count houses with [bid_failures > 0] > 0)
    [set hold_out_ratio  bid_refused_total / count houses with [ bid_failures > 0]  ; we need to do networkes and isolated modifications to this later
   ]
      ;let net_ho  bid-refusal of houses with links / count houses [with bid_failure > 0 and links > 0]
   file-write csv:to-row
   (list
    int ticks
    mortgage-buyout-ratio
    int y
    int m
    int non-normalized-norfolk-offer
    int success-flag
    int success-total
    int count houses
    int count houses with[count my-out-links >= 4]
    int count houses with[count my-out-links = 3]
    int count houses with [count my-out-links = 2]
    int count links
    hold_out_ratio
    int h ; EJ
    neiih
    lih
    mean_mortgage ; EJ
    mort_stnd_dev ; EJ
     )
                   ]
  ;file-write csv:to-row (sentence ticks mortgage-buyout-ratio m non-normalized-norfolk-offer success-flag success-total y count houses  count houses with[count my-out-links >= 2] ) ]
 ;
  ; for meta data for success flag 1 or 0 will be true of false

  file-close-all
end

; FUNCTION CALL  OPERATIONS MAIN CALL LIST ############################################################################################
to setup
  clear-all
  reset-ticks
  set-default-shape houses "house"
  if simulation-output = true[
    check_file
    write_meta_file
    write_header
  ]
  ;#Patch Data Import#
  if distribution = "Real"
      [set_data_import  setup_patches patch_effects]
  if distribution = "Simulated"
      [gamma_dist_generation gamma_patch_set_up patch_effects]

  let std-dev standard-deviation [mortgage] of patches
  let avg mean [mortgage] of patches
  set high_value count patches with[mortgage >= avg + (std-dev * 2)]
  set med_value count patches with [mortgage <= (avg + std-dev) and mortgage >= (avg - std-dev)]
  set low_value count patches with [mortgage <= avg - (std-dev * 2)]

  setup_houses
  ask houses [set_monetary_valuation ]

  do-plotting

  reset-ticks
end



to go
  if (any? houses = false) [stop]
 ; social_network_assessment ; network assesmemnt + visualizaiton update
  strategy_select  ; resident willingness to pay calculation strategies
  norfolk_bid_search ; norfolk search  strategies and bidding
   ask houses [
    set flagged 0
  ]
   colorize_houses
  tick
end
@#$#@#$#@
GRAPHICS-WINDOW
248
19
685
457
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
11
15
77
48
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
717
244
857
289
>= 2 Std Dev Mortgages
high_value
17
1
11

MONITOR
717
197
856
242
+-1 Std Dev Mortgages
med_value\n;these buckes are based on standard deviation from mean: \n;Low Value = 2 below \n;Middle value = within 1 standard deviateion\n;High value = 2 above\n;each had equally  6,000 entries generated
17
1
11

MONITOR
716
149
855
194
<= -2 Std Dev Mortgages
low_value
17
1
11

BUTTON
98
15
161
48
Run
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
86
188
119
residential_density
residential_density
50
1080
500.0
10
1
NIL
HORIZONTAL

SWITCH
13
260
169
293
prefered_network
prefered_network
0
1
-1000

SLIDER
249
473
406
506
blocksize
blocksize
1
1080
20.0
1
1
NIL
HORIZONTAL

CHOOSER
11
124
149
169
resident_strategy
resident_strategy
"Linear" "Nested"
1

SLIDER
248
508
407
541
offer_adjustment
offer_adjustment
1
100
10.0
1
1
%
HORIZONTAL

PLOT
14
398
229
518
Network Distribution
# of Links
Households
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [count link-neighbors] of houses"

MONITOR
96
350
170
395
Network  4+
count turtles with [color = blue]
17
1
11

MONITOR
13
350
85
395
Network 1
count turtles with [color = red]
17
1
11

MONITOR
96
296
170
341
Network 2+
count turtles with [color = yellow]
17
1
11

MONITOR
12
296
86
341
Network 0
count turtles with [color = grey]
17
1
11

PLOT
718
296
978
448
Mortgage Distribution
Mortgage Amount
# of Mortgage
0.0
10.0
0.0
300000.0
true
false
"" ""
PENS
"default" 20.0 1 -16777216 true "" "histogram [mortgage] of patches"

SLIDER
693
504
874
537
negative_impact
negative_impact
0
25
15.0
1
1
%
HORIZONTAL

SWITCH
694
465
841
498
impact_effects
impact_effects
0
1
-1000

SLIDER
11
172
183
205
social_affinity
social_affinity
10
100
50.0
10
1
%
HORIZONTAL

PLOT
248
597
685
747
Purchase Price to Household Valuation Ratio
Bids
Ratio
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mortgage-buyout-ratio"

PLOT
422
463
683
591
Norfolk Offer
NIL
NIL
0.0
10.0
150000.0
300000.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot non-normalized-norfolk-offer"

MONITOR
249
545
409
590
NIL
non-normalized-norfolk-offer
17
1
11

PLOT
694
543
966
747
Hold Out Ratio
Ticks
Holdout Ratio
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot hold_out_ratio"

CHOOSER
716
17
873
62
Distribution
Distribution
"Simulated" "Real"
1

SLIDER
718
70
890
103
mean_mortgage
mean_mortgage
50000
300000
300000.0
25000
1
NIL
HORIZONTAL

SLIDER
716
110
888
143
mort_stnd_dev
mort_stnd_dev
5000
100000
9500.0
1500
1
NIL
HORIZONTAL

CHOOSER
12
209
150
254
Social_type
Social_type
"Hetero" "Homo"
0

SWITCH
11
50
162
83
simulation-output
simulation-output
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

This model simulates the systematic purchasing of real estate in a residential neighborhood by a corporation intending to redevelop the land. The environment of this model does not reflect geographic space. Instead, each patch represents a mortgage/land value. The interactions in this model allow the user to explore the effects of property acquisition on neighborhood residents and the underlying dynamics affecting outcomes such as displacement and gentrification.

## HOW IT WORKS

At the beginning of the model a matrix of patches are assigned mortgage values from a distribution. Higher mortgage values have a lighter color patch. Patches spawn houses based on the residential density variable set by the user. Houses create links based on a prefered or random style and adopt unique valuations of their property based on the mortgage and social values. Each tick follows a cycle of resident valuation followed by a bidding process in which Norfolk makes a bid on a house that is either refused or accepted.

During the valuation stage of the cycle houses assess a valuation of their property. The valuation includes their monetary valuation of the property and a social valuation that assess their neighbors property valuations. Depending on which strategy the user selects the valuation can be done using a linear valuation algorithm or a nested one. The linear valuation has each house assess their value based on the social and monetary valuations and includes the valuation of any other houses in their network, if any network links are present. The nested valuation strategy incorporates a decision tree based on network links and social preferences that creates a valuation incorporating the average valuation of linked and nearby houses. 

The bidding stage utilizes the observer to act as the buyer entity. The observer flags a random set of houses based on the block size variable and sets an offer for each flagged house based on the underlying patch mortgage, the offer adjustment variable, the number of failed bids, the average mortgage value of already purchased property and the average value of all patches. If the offer is not higher than a house's valuation the bid fails. If the offer is higher the house and its underlying patch are purchased. If negative impact is set to on, the mortgage value of adjacent patches not owned by the observer are reduced by the negative impact variable. The house on the purchased patch is removed and network links are severed.

These two stages of the cycle continue until all of the houses have been purchased. 

## HOW TO USE IT

To run the simulation press setup. Once the market and houses have populated the landscape you can press go for the simulation to run to completion. 

## THINGS TO NOTICE

Pay attention to the network distribution, the Purchase Price to Household Valuation Ratio plot, and the holdout ratio. Their changes throughout each simulation highlight key dynamics to consider in understanding the mechanisms of an assymetrical housing market with a powerful purchaser.

One important output of this model is the individual bidding actions output at the end of each simulation within the model folder. You can keep these for analysis or discard them, but they capture details on each individual bid made by the buyer entity and information on how the household responded.

## THINGS TO TRY

Try changing the prefered_network variable and see how that effects the purchasing process. 

Change the distribution to a higher or lower mean in the simulated distribution and see how neighborhood wealth affects outcomes. Do they lead to more holdouts?

Move the social affinity variable around, change the social type to introduce stochasticity into the social preferences of the neighborhood. What happens when residents weight their decision to sell more toward their social ties?

Introduce negative impacts and modify the intensity of their effects. Does this cause the buyer entity to spend more or less?

## EXTENDING THE MODEL

Including a interest-rate slider for the user to modify. There is already a reference to interest-rate in the model code, but we weren't able to get around to implementing it.

Include an ability for the agents to create a community benefits agreement, or a point where the residents refuse to sell as an act of collective defiance. 

## NETLOGO FEATURES

Our model utilizes the output and plot drawing functions in ways that we don't think the software was written to include. Our output data is a collection of every sale action within the simulation, providing data that happens between each tick. This was something we had to jerry rig to produce and clean using python scripting. 

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Default_Apr18" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random_social">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Nested" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Linear" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Linear&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_PrefOn" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_PrefOff" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Dens540" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Dens270" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="270"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Dens810" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="810"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Block25" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Block135" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="135"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Block270" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="270"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Block405" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="405"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Block540" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="540"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_NegOff" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Neg10" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Neg5" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Neg25" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Offer20" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Offer5" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Offer50" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_DistLeft_skew" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_DistRight_skew" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="100000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="89500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_DistOff" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Soc90" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Soc50" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_Soc10" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="May18_Soc2" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_HomSoc" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Homo&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apr18_HetSoc" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="May18_Soc5" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="May18_Soc7" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="May18_Block50" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="May18_Block80" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="May18_Block105" repetitions="15" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="negative_impact">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="impact_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high_value_homes">
      <value value="849"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="offer_adjustment">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_constraint">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prefered_network">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Distribution_modifier">
      <value value="&quot;Off&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="residential_density">
      <value value="540"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="med_value_homes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low_value_homes">
      <value value="837"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_affinity">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social_type">
      <value value="&quot;Hetero&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resident_strategy">
      <value value="&quot;Nested&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_mortgage">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norfolk_strategy">
      <value value="&quot;Block&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mort_stnd_dev">
      <value value="71500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="blocksize">
      <value value="105"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
