###THIS IS HOW I DID THE TRAJECTORY MATCHING



DMeas <- Csnippet("
         double OUTPUT_ADULT;
  //For the life of me I can't seem to sum up all of the subcompartments
  //in the reproductive adult

  OUTPUT_ADULT = log((Ar1+ Ar2+ Ar3+ Ar4+ Ar5+ Ar6+ Ar7+ Ar8+ Ar9+
  Ar10+ Ar11+ Ar12+ Ar13+ Ar14+ Ar15+ Ar16+ Ar17+
  Ar18+ Ar19+ Ar20+ Ar21+ Ar22+ Ar23+ Ar24+ Ar25)/2+1);

  //This takes into account the missing data in the actual adata
  if (ISNA(ARC)){
  lik = (give_log) ? 0:1;
  } else{

  //During the least squares 
  lik=dnorm(OUTPUT_ADULT, ARC,2,give_log);
  }
  //This just prints out the output if I get a non-numeric value
  if (!R_FINITE(lik))
  Rprintf(\"%lg\\n\",lik);"
  )


###YOU NEED TO SET THIS UP 
FIT_COBLYA <- traj_objfun(
  POMP_CM,
  est = c(
    #Parameters associated with diapause development
    "alphaDL_a",#1
    "alphaDL_b", #2
    "alphaDL_c", #3
    #Parameters associated with larval mortality - I fixed optimum temperature
    #of survivorship
    "MORT_L_A", #4
    "MORT_L_B", #5
    #Parameter associated with diapause induction- I fixed DIA_B (0.5) and 
    #DIA_C (225)
    "DIA_A",
    #Parameter associated with the allee
    "C",
    #Paramater associated with the competition 
    "COMP"#9)
  ),
  params =  c(
    alphaE_a = 0.1927,
    alphaE_b = 0.3039,
    alphaE_c = 18.5929,
    alphaL1_a = 0.30,
    alphaL1_b = 0.327,
    alphaL1_c = 17.60 ,
    alphaL2_a = 0.457,
    alphaL2_b = 0.2301,
    alphaL2_c = 21.23,
    alphaL3_a = 0.338,
    alphaL3_b = 0.350,
    alphaL3_c = 18.45,
    alphaL4_a = 0.305,
    alphaL4_b = 0.429,
    alphaL4_c = 21.54,
    alphaL5_a = 0.33,
    alphaL5_b = 0.2144,
    alphaL5_c = 20.94,
    alphaP_a = 0.09287,
    alphaP_b = 0.28966,
    alphaP_c = 18.48736,
    alphaA_a = 0.13,
    alphaA_b = 0.18,
    alphaA_c = 20.2206,
    alphaDL_a = 4.81 * 10 ^ (-2) ,
    alphaDL_b = 0.23 ,
    alphaDL_c = 0.53 ,
    MORT_E_A = 2.063e+00 ,
    MORT_E_B = 1.306e-04 ,
    T_E_OPT = 1.357e+01 ,

    MORT_L_A = 3.8 ,
    MORT_L_B = 2.2e-04  ,
    T_L_OPT = 16.24,
   
     MORT_P_A = 2.862e+00 ,
    MORT_P_B = 1.036e-05  ,
    T_P_OPT = 5.461e+00 ,
    
    MORT_A_A = 2.862e+00  ,
    MORT_A_B = 1.036e-05 ,
    T_A_OPT = 5.461e+00 ,
    
    MORT_DL_A = 6.491816 ,
    MORT_DL_B = 0.000321  ,
    T_DL_OPT = 3.725070    ,
    
    DIA_A = 0.525,
    DIA_B = 0.500887360,
    DIA_C = 225.999338679,
    
    C = 0.00934506 ,
    COMP = 0.001224080
  ),
  dmeasure = DMeas,
  fail.value = 1e5,
  covarnames = c("TT", "PP_DIFF"),
  obsnames = c("ARC"),
  statenames = c(
    sprintf("E%1d", seq_len(nE)),
    sprintf("L1%1d", seq_len(nL1)),
    sprintf("L2%1d", seq_len(nL2)),
    sprintf("L3%1d", seq_len(nL3)),
    sprintf("L4%1d", seq_len(nL4)),
    sprintf("L5%1d", seq_len(nL5)),
    sprintf("DIAL5%1d", seq_len(nDL5)),
    sprintf("P%1d", seq_len(nP)),
    sprintf("Ar%1d", seq_len(nAr)),
    "As"
  ),
  ode_control = list(method = 'bdf')
)


traj_match_TRY_FINAL <- cobyla(
  fn =
    FIT_COBLYA,
  x0 = c(
    4.81 * 10 ^ (-2),
    #1"alphaDL_a",
    0.23,
    #2"alphaDL_b",
    0.53,
    #3"alphaDL_c",
    3.8,
    #4"MORT_L_A",
    2.2e-04 ,
    #5 MORT_L_B,
    0.525,

    #7"DIA_B",
    0.00934506,
    #8"C",
    0.001224080
  ),

  lower = c(0.04, 0, 0, 3.5, 1e-05 , 0.5, 0, 0),
  upper = c(0.1, 1, 1, 5, 5e-03, 1, 1, 0.1),
  control = list(maxeval = 1000)
)


