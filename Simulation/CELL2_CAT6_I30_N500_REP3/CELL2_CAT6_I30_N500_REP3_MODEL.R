model <- " model {

# specify prior distributions

for (j in 1:J) {

b[j] ~ dnorm(0,0.25);

a[j] ~ dnorm(0,4);

tau[j,1] <- 0;

}

tau[1,2] ~ dlnorm(0.248011, 1);
tau[1,3] ~ dlnorm(0.127938, 1);
tau[1,4] ~ dlnorm(-0.008545, 1);
tau[1,5] ~ dlnorm(-0.166645, 1);
tau[1,6] ~ dlnorm(-0.354521, 1);
tau[2,2] ~ dlnorm(0.066462, 1);
tau[2,3] ~ dlnorm(-0.05991, 1);
tau[2,4] ~ dlnorm(-0.204594, 1);
tau[2,5] ~ dlnorm(-0.373811, 1);
tau[2,6] ~ dlnorm(-0.577614, 1);
tau[3,2] ~ dlnorm(0.144554, 1);
tau[3,3] ~ dlnorm(0.021037, 1);
tau[3,4] ~ dlnorm(-0.119915, 1);
tau[3,5] ~ dlnorm(-0.284046, 1);
tau[3,6] ~ dlnorm(-0.48051, 1);
tau[4,2] ~ dlnorm(0.277635, 1);
tau[4,3] ~ dlnorm(0.158483, 1);
tau[4,4] ~ dlnorm(0.023188, 1);
tau[4,5] ~ dlnorm(-0.133319, 1);
tau[4,6] ~ dlnorm(-0.318948, 1);
tau[5,2] ~ dlnorm(0.305403, 1);
tau[5,3] ~ dlnorm(0.187088, 1);
tau[5,4] ~ dlnorm(0.052873, 1);
tau[5,5] ~ dlnorm(-0.102191, 1);
tau[5,6] ~ dlnorm(-0.285792, 1);
tau[6,2] ~ dlnorm(0.263343, 1);
tau[6,3] ~ dlnorm(0.14375, 1);
tau[6,4] ~ dlnorm(0.007887, 1);
tau[6,5] ~ dlnorm(-0.149381, 1);
tau[6,6] ~ dlnorm(-0.336083, 1);
tau[7,2] ~ dlnorm(-0.159888, 1);
tau[7,3] ~ dlnorm(-0.295975, 1);
tau[7,4] ~ dlnorm(-0.453544, 1);
tau[7,5] ~ dlnorm(-0.640672, 1);
tau[7,6] ~ dlnorm(-0.871067, 1);
tau[8,2] ~ dlnorm(0.144783, 1);
tau[8,3] ~ dlnorm(0.021274, 1);
tau[8,4] ~ dlnorm(-0.119668, 1);
tau[8,5] ~ dlnorm(-0.283785, 1);
tau[8,6] ~ dlnorm(-0.480229, 1);
tau[9,2] ~ dlnorm(-0.096112, 1);
tau[9,3] ~ dlnorm(-0.229226, 1);
tau[9,4] ~ dlnorm(-0.382821, 1);
tau[9,5] ~ dlnorm(-0.564365, 1);
tau[9,6] ~ dlnorm(-0.786342, 1);
tau[10,2] ~ dlnorm(-0.114318, 1);
tau[10,3] ~ dlnorm(-0.248261, 1);
tau[10,4] ~ dlnorm(-0.40296, 1);
tau[10,5] ~ dlnorm(-0.586051, 1);
tau[10,6] ~ dlnorm(-0.810349, 1);
tau[11,2] ~ dlnorm(-0.089504, 1);
tau[11,3] ~ dlnorm(-0.222321, 1);
tau[11,4] ~ dlnorm(-0.37552, 1);
tau[11,5] ~ dlnorm(-0.556512, 1);
tau[11,6] ~ dlnorm(-0.777663, 1);
tau[12,2] ~ dlnorm(0.130406, 1);
tau[12,3] ~ dlnorm(0.006388, 1);
tau[12,4] ~ dlnorm(-0.135216, 1);
tau[12,5] ~ dlnorm(-0.300233, 1);
tau[12,6] ~ dlnorm(-0.497968, 1);
tau[13,2] ~ dlnorm(-0.345665, 1);
tau[13,3] ~ dlnorm(-0.491641, 1);
tau[13,4] ~ dlnorm(-0.662629, 1);
tau[13,5] ~ dlnorm(-0.86901, 1);
tau[13,6] ~ dlnorm(-1.129364, 1);
tau[14,2] ~ dlnorm(-0.240225, 1);
tau[14,3] ~ dlnorm(-0.380352, 1);
tau[14,4] ~ dlnorm(-0.543366, 1);
tau[14,5] ~ dlnorm(-0.738229, 1);
tau[14,6] ~ dlnorm(-0.980486, 1);
tau[15,2] ~ dlnorm(-0.269841, 1);
tau[15,3] ~ dlnorm(-0.411547, 1);
tau[15,4] ~ dlnorm(-0.576701, 1);
tau[15,5] ~ dlnorm(-0.774633, 1);
tau[15,6] ~ dlnorm(-1.02166, 1);
tau[16,2] ~ dlnorm(-0.225199, 1);
tau[16,3] ~ dlnorm(-0.364545, 1);
tau[16,4] ~ dlnorm(-0.526501, 1);
tau[16,5] ~ dlnorm(-0.719854, 1);
tau[16,6] ~ dlnorm(-0.959776, 1);
tau[17,2] ~ dlnorm(-0.208993, 1);
tau[17,3] ~ dlnorm(-0.34751, 1);
tau[17,4] ~ dlnorm(-0.508346, 1);
tau[17,5] ~ dlnorm(-0.700103, 1);
tau[17,6] ~ dlnorm(-0.937569, 1);
tau[18,2] ~ dlnorm(-0.254061, 1);
tau[18,3] ~ dlnorm(-0.39492, 1);
tau[18,4] ~ dlnorm(-0.558924, 1);
tau[18,5] ~ dlnorm(-0.755206, 1);
tau[18,6] ~ dlnorm(-0.999663, 1);
tau[19,2] ~ dlnorm(0.069812, 1);
tau[19,3] ~ dlnorm(-0.056433, 1);
tau[19,4] ~ dlnorm(-0.20095, 1);
tau[19,5] ~ dlnorm(-0.369939, 1);
tau[19,6] ~ dlnorm(-0.57341, 1);
tau[20,2] ~ dlnorm(-0.050024, 1);
tau[20,3] ~ dlnorm(-0.18111, 1);
tau[20,4] ~ dlnorm(-0.332009, 1);
tau[20,5] ~ dlnorm(-0.509795, 1);
tau[20,6] ~ dlnorm(-0.726175, 1);
tau[21,2] ~ dlnorm(-0.147775, 1);
tau[21,3] ~ dlnorm(-0.283282, 1);
tau[21,4] ~ dlnorm(-0.440074, 1);
tau[21,5] ~ dlnorm(-0.626104, 1);
tau[21,6] ~ dlnorm(-0.854837, 1);
tau[22,2] ~ dlnorm(0.02525, 1);
tau[22,3] ~ dlnorm(-0.102725, 1);
tau[22,4] ~ dlnorm(-0.249515, 1);
tau[22,5] ~ dlnorm(-0.421621, 1);
tau[22,6] ~ dlnorm(-0.629634, 1);
tau[23,2] ~ dlnorm(-0.032964, 1);
tau[23,3] ~ dlnorm(-0.163323, 1);
tau[23,4] ~ dlnorm(-0.31326, 1);
tau[23,5] ~ dlnorm(-0.489711, 1);
tau[23,6] ~ dlnorm(-0.704113, 1);
tau[24,2] ~ dlnorm(0.018247, 1);
tau[24,3] ~ dlnorm(-0.110007, 1);
tau[24,4] ~ dlnorm(-0.257164, 1);
tau[24,5] ~ dlnorm(-0.429777, 1);
tau[24,6] ~ dlnorm(-0.63853, 1);
tau[25,2] ~ dlnorm(0.150639, 1);
tau[25,3] ~ dlnorm(0.027335, 1);
tau[25,4] ~ dlnorm(-0.11334, 1);
tau[25,5] ~ dlnorm(-0.277095, 1);
tau[25,6] ~ dlnorm(-0.47302, 1);
tau[26,2] ~ dlnorm(0.293748, 1);
tau[26,3] ~ dlnorm(0.175084, 1);
tau[26,4] ~ dlnorm(0.04042, 1);
tau[26,5] ~ dlnorm(-0.115244, 1);
tau[26,6] ~ dlnorm(-0.299687, 1);
tau[27,2] ~ dlnorm(0.02899, 1);
tau[27,3] ~ dlnorm(-0.098836, 1);
tau[27,4] ~ dlnorm(-0.245431, 1);
tau[27,5] ~ dlnorm(-0.417269, 1);
tau[27,6] ~ dlnorm(-0.62489, 1);
tau[28,2] ~ dlnorm(0.206161, 1);
tau[28,3] ~ dlnorm(0.084739, 1);
tau[28,4] ~ dlnorm(-0.05349, 1);
tau[28,5] ~ dlnorm(-0.213939, 1);
tau[28,6] ~ dlnorm(-0.405145, 1);
tau[29,2] ~ dlnorm(0.289348, 1);
tau[29,3] ~ dlnorm(0.170552, 1);
tau[29,4] ~ dlnorm(0.035717, 1);
tau[29,5] ~ dlnorm(-0.120175, 1);
tau[29,6] ~ dlnorm(-0.30494, 1);
tau[30,2] ~ dlnorm(0.048447, 1);
tau[30,3] ~ dlnorm(-0.078617, 1);
tau[30,4] ~ dlnorm(-0.224209, 1);
tau[30,5] ~ dlnorm(-0.394671, 1);
tau[30,6] ~ dlnorm(-0.600284, 1);

  #specify model
for (i in 1:N) {				
  theta[i] ~ dnorm(0,1);
  for (j in 1:J) {	
    for (z in 1:K){
      cp[i,j,z]<- exp(a[j]*((z-1)*(theta[i]-b[j] )+ sum(tau[j,1:z])))+ exp(a[j]*(((2*K)-z)*(theta[i]-b[j] )+ sum(tau[j,1:z])));
    }
    den[i,j] <- sum(cp[i,j,1:K]);
    for (z in 1:K){
      prob[i,j,z] <- cp[i,j,z]/den[i,j];	
    } 
    r[i,j] ~ dcat(prob[i,j,]);	
  }
}
}   "          
 
# specify parameters
# N is sample size
# J is number of items
# K is response categories
# D is number of dimensions
                                  
resp <- matrix(data, nrow= 500 , ncol= 30 , byrow=TRUE)

N <- 500 

J <- 30 

K <- 6 

data_list <- list("r" = resp, "N" = N, "J" = J, "K" = K)

# specify starting values for all estimated parameters

starts <-  list(a=
c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
),
b=c( -1.5870674232965518, 
                    -1.0719861302474263, 
                    -1.2821404541060049, 
                    -1.6803514638520927, 
                    -1.7703347049702578, 
                    -1.6350025550574196, 
                    -0.5478789338146246, 
                    -1.2827797774693825, 
                    -0.6837536055089654, 
                    -0.6440780789269318, 
                    -0.6983341687495095, 
                    -1.242837615621328, 
                    -0.19806421447573697, 
                    -0.3886034056601683, 
                    -0.33303901332509406, 
                    -0.41742811708261757, 
                    -0.44900702818517524, 
                    -0.36243924494988766, 
                    1.0806673176662016, 
                    0.7874802812557977, 
                    0.5730236477992559, 
                    0.9675200737539598, 
                    0.8271043487548837, 
                    0.9501931016295202, 
                    1.2992139214251586, 
                    1.7322606091183144, 
                    0.9768243723961814, 
                    1.4599084715413617, 
                    1.7180040568961286, 
                    1.0257917530246174 ),
tau=matrix(c( NA,2.1127929152649516,1.8737430047137518,1.6346930941625517,1.3956431836113516,1.1565932730601516, 
                    NA,1.7620225546984971,1.5528473591441463,1.3436721635897957,1.134496968035445,0.9253217724810943, 
                    NA,1.9051376492461893,1.683773502908041,1.4624093565698928,1.2410452102317446,1.0196810638935963, 
                    NA,2.1763193468832753,1.931858961979854,1.6873985770764326,1.4429381921730111,1.1984778072695896, 
                    NA,2.2375979340847456,1.9879185211964705,1.7382391083081956,1.4885596954199207,1.2388802825316458, 
                    NA,2.1454367399941026,1.903606591800772,1.6617764436074418,1.4199462954141115,1.1781161472207813, 
                    NA,1.4051055539277595,1.2263285757665112,1.047551597605263,0.8687746194440147,0.6899976412827664, 
                    NA,1.9055730284566494,1.6841718013634253,1.462770574270201,1.241369347176977,1.0199681200837527, 
                    NA,1.4976362053516057,1.3109784962320856,1.1243207871125656,0.9376630779930455,0.7510053688735254, 
                    NA,1.4706171717492407,1.2862606431714787,1.1019041145937165,0.9175475860159544,0.7331910574381924, 
                    NA,1.5075655689184162,1.3200621871309446,1.132558805343473,0.9450554235560014,0.7575520417685297, 
                    NA,1.8783724162381246,1.6592878345320874,1.4402032528260504,1.2211186711200135,1.0020340894139763, 
                    NA,1.166881730057977,1.0083940056183842,0.8499062811787914,0.6914185567391986,0.5329308322996058, 
                    NA,1.2966389192545749,1.127099921726285,0.9575609241979952,0.7880219266697054,0.6184829291414156, 
                    NA,1.258799568074389,1.0924833053015335,0.9261670425286781,0.7598507797558226,0.5935345169829671, 
                    NA,1.3162685477332625,1.1450577169424707,0.9738468861516789,0.8026360553608871,0.6314252245700953, 
                    NA,1.3377737861941046,1.1647313785593645,0.9916889709246242,0.8186465632898839,0.6456041556551437, 
                    NA,1.2788211258108735,1.11079964960378,0.9427781733966865,0.774756697189593,0.6067352209824994, 
                    NA,1.7679344433306834,1.5582557389060436,1.3485770344814039,1.138898330056764,0.9292196256321243, 
                    NA,1.5682740715351982,1.3756002152223619,1.1829263589095256,0.9902525025966893,0.797578646283853, 
                    NA,1.4222291041512931,1.2419937325789363,1.0617583610065795,0.8815229894342227,0.7012876178618659, 
                    NA,1.6908811702264468,1.487765005948717,1.2846488416709874,1.0815326773932576,0.8784165131155279, 
                    NA,1.5952580615020757,1.4002860092742924,1.2053139570465092,1.010341904818726,0.8153698525909427, 
                    NA,1.6790815022097034,1.4769703023151912,1.274859102420679,1.0727479025261668,0.8706367026316546, 
                    NA,1.9167646804905332,1.694410273047874,1.4720558656052147,1.2497014581625554,1.0273470507198963, 
                    NA,2.2116694748095718,1.9641983594807098,1.7167272441518475,1.4692561288229853,1.221785013494123, 
                    NA,1.6972173976017997,1.4935615840028211,1.2899057704038426,1.086249956804864,0.8825941432058855, 
                    NA,2.026197669119667,1.7945229777702683,1.5628482864208693,1.3311735950714703,1.0994989037220713, 
                    NA,2.201960762746263,1.9553165274462878,1.7086722921463124,1.462028056846337,1.2153838215463617, 
                    NA,1.7305641838097647,1.5240682621343369,1.317572340458909,1.1110764187834812,0.9045804971080533 ), ncol= 6 , nrow= 30 , byrow=TRUE),
theta = c(
 0.04373922514318718, 
                    0.6147060014088748, 
                    0.009868818272641855, 
                    0.47481434130602196, 
                    0.3865557844926586, 
                    -0.3751864148229057, 
                    0.46685655023417794, 
                    -0.3559413151345414, 
                    0.4528104046894703, 
                    -0.3003482634845069, 
                    0.2930798925795999, 
                    0.037323230088626036, 
                    0.016888440345428135, 
                    0.4641199456636693, 
                    -0.31162675113676774, 
                    -0.2505216738838141, 
                    -0.25342381869797836, 
                    0.19364812739684756, 
                    0.2346797038134797, 
                    -0.15697495831209785, 
                    -0.40059642883039376, 
                    0.13464654473708315, 
                    0.6401874782924732, 
                    -0.4085834522746097, 
                    0.6463442441065961, 
                    -0.14255939450770183, 
                    0.37064999996063386, 
                    -0.3033877357311852, 
                    0.11022955983398974, 
                    0.34954077769746217, 
                    -0.35967877917009533, 
                    -0.34491830578996435, 
                    -0.16853247057880116, 
                    -0.06521882932845002, 
                    0.19765898272266935, 
                    -0.18131919539757102, 
                    -0.17166715244125208, 
                    0.3275999076528351, 
                    -0.7251219892308343, 
                    -0.18754845334089565, 
                    -0.16504386888411204, 
                    -0.31143538599881354, 
                    0.5801976980197454, 
                    -0.03423845431393446, 
                    -0.0880478902259425, 
                    0.30429448285736016, 
                    0.3350838009538666, 
                    -0.7423428056904934, 
                    -0.4854033372777904, 
                    -1.8127219888941504e-4, 
                    0.5412132181033698, 
                    -0.4868581333277322, 
                    0.34918329112154267, 
                    -0.4071006037931806, 
                    0.2091305525508812, 
                    -0.44422021122311356, 
                    -0.18552835208570784, 
                    -0.14783465757905123, 
                    -0.7048776732325556, 
                    0.23016106717434826, 
                    -0.5516938808004069, 
                    0.150263474375404, 
                    0.04705191194684766, 
                    -0.10499257406988427, 
                    -0.4264234696706559, 
                    0.1736299185887329, 
                    0.6203205500752664, 
                    -0.6029286412272964, 
                    0.21195166218258588, 
                    -0.2254067042036354, 
                    -0.3453603099883343, 
                    0.40744955025041407, 
                    -0.2694210992450693, 
                    -0.5168397948270035, 
                    -0.22901816050141122, 
                    -0.1535450120351627, 
                    -0.3616899633900516, 
                    -0.5528339262443548, 
                    -0.524226971804036, 
                    0.6227677533755573, 
                    0.3276861317457709, 
                    0.4843428627441506, 
                    -0.6739281555738608, 
                    -0.6938399093664217, 
                    -0.17778061669727951, 
                    -0.3104495946962941, 
                    0.3433705915075089, 
                    -0.48041517383419247, 
                    0.4565379228556419, 
                    0.16886239392272084, 
                    -0.37642690528893313, 
                    -0.003190371170693984, 
                    0.4110398326007978, 
                    0.03702234010667482, 
                    0.2716871508335479, 
                    -0.14346669980930116, 
                    -0.3618942165792346, 
                    0.05294691071569557, 
                    -0.5038102238633737, 
                    0.10030993143824174, 
                    0.07067878143729833, 
                    -0.2630332856118114, 
                    -0.24907721767132707, 
                    0.544479777637239, 
                    0.16371816648565085, 
                    0.09980946297873428, 
                    0.36989316256821203, 
                    0.15183024972251669, 
                    0.4147311647368144, 
                    -0.4126898507210189, 
                    0.12219489968922281, 
                    -0.7649793420484532, 
                    -0.22717350016182536, 
                    -0.05684937578097704, 
                    0.49055663924681436, 
                    -0.08131735918749317, 
                    -0.6654725451801358, 
                    -0.08177597394678915, 
                    0.5268221534209834, 
                    -0.3788151144489481, 
                    0.3291596327402284, 
                    0.00902943474219986, 
                    -0.3381612433730001, 
                    -0.3671649660037079, 
                    -0.6610713914354692, 
                    0.3126784896006338, 
                    -0.6654414584575543, 
                    0.571708890751393, 
                    0.4369381336194711, 
                    0.09479447804764041, 
                    0.11801213217953344, 
                    -0.6889297749696642, 
                    0.0933606173240028, 
                    0.06623672879313391, 
                    -0.34245178472716525, 
                    0.5610922414635331, 
                    0.45361235663868693, 
                    -0.40831860202936265, 
                    -0.6849670821398757, 
                    0.42216388970323915, 
                    -0.18130904032514283, 
                    -0.5595959546207593, 
                    -0.5576926950097665, 
                    0.3995180141584671, 
                    -0.20933432215573922, 
                    -0.2181653986003903, 
                    -0.1926509882647114, 
                    -0.5972345001177374, 
                    -0.2853420675568587, 
                    -0.5702513813002703, 
                    -0.4472733753798877, 
                    0.5339958713097608, 
                    0.555449918273826, 
                    -0.2569240943268042, 
                    0.14228426673629668, 
                    -0.44645948723866735, 
                    0.13962778011966226, 
                    0.40240178465819354, 
                    -0.10645102377930304, 
                    -0.14698671742413982, 
                    -0.4594916355090696, 
                    0.5309812651626906, 
                    0.5802092855441736, 
                    -0.7268919914664654, 
                    -0.2044420267506406, 
                    0.6188022340312491, 
                    0.5887262577123658, 
                    0.0455789056594722, 
                    0.6269469101131364, 
                    -0.6407382560341324, 
                    -0.49524249740595966, 
                    0.4923055659420592, 
                    0.2960459495612122, 
                    0.050810772921451974, 
                    0.4829628361989837, 
                    -0.12640309848233355, 
                    0.20367886833225857, 
                    0.49994188701778164, 
                    -0.15377974761664515, 
                    0.007892250058069816, 
                    0.23046587124884432, 
                    0.4241150781847549, 
                    0.40613735652292426, 
                    -0.27542534927347906, 
                    -0.6320348166909037, 
                    -0.027067361985806282, 
                    0.2223078704228778, 
                    0.32868252714851476, 
                    0.22135239676982044, 
                    -0.39849626594758375, 
                    -0.338859728596412, 
                    -0.2653375259749181, 
                    0.43836315191687003, 
                    0.273054874568498, 
                    -0.3498166730082328, 
                    -0.08842783228820517, 
                    0.1542371431870604, 
                    0.28710557486894994, 
                    -0.28585622777646463, 
                    0.276582037073118, 
                    -0.2332597869992139, 
                    0.24151030701577958, 
                    -0.6203644679772459, 
                    0.5557698392718209, 
                    -0.3236857463399682, 
                    -0.6139311290117409, 
                    -0.23505924358241304, 
                    -0.023150072052050885, 
                    0.012941472394242481, 
                    0.4431285024907008, 
                    0.3922664555105284, 
                    -0.6580291908894783, 
                    -0.4617235981384184, 
                    0.39314632740082467, 
                    -0.10263934814752262, 
                    0.4968706384274466, 
                    -0.5349622140109244, 
                    -0.25877247410447446, 
                    0.38024337796558316, 
                    -0.5631131847627809, 
                    0.6203595056093183, 
                    0.5291838127672668, 
                    -0.5026448418981664, 
                    0.45462470193276205, 
                    -0.022131070366797068, 
                    0.21883558229571654, 
                    -0.28168855770391316, 
                    -0.1532341951633902, 
                    -0.0670508978569685, 
                    0.2123879411088142, 
                    -0.07595094838881289, 
                    -0.7305487044133558, 
                    -0.48022202488779125, 
                    0.010620916729290775, 
                    0.5821116219889653, 
                    0.5826699031653049, 
                    0.17895636989199004, 
                    0.417069845556288, 
                    0.0054984396083391784, 
                    0.3428095556577886, 
                    0.3357229934092132, 
                    0.4410667850151151, 
                    -0.41522799960314666, 
                    0.49125911438449976, 
                    -0.7415101250035416, 
                    -0.0029985901883290422, 
                    -0.26502169040928913, 
                    -0.15109152635834655, 
                    0.10568327011774514, 
                    0.014417385225697799, 
                    0.6144340743694117, 
                    0.302228338894309, 
                    0.5567716376191776, 
                    0.629029866044287, 
                    0.4102159164516501, 
                    0.11311904256778149, 
                    -0.17675078283983903, 
                    0.573388947004106, 
                    -0.3024885587108287, 
                    0.701335950189393, 
                    0.10238913550545681, 
                    0.34201282986313264, 
                    -0.1946790958943153, 
                    0.35721657892858083, 
                    0.2307392532974466, 
                    -0.17669731313844883, 
                    0.5998310882549762, 
                    0.07649795478424748, 
                    0.1298226801303004, 
                    -0.13090255756551095, 
                    0.47272761811332, 
                    -0.0827327089068417, 
                    0.5988942326839568, 
                    0.5810825393948003, 
                    0.13887148131613625, 
                    0.367056694890316, 
                    -0.37433389286146806, 
                    -0.295055862652659, 
                    -0.20586033679097482, 
                    -0.32678812475972263, 
                    -0.1570099690754907, 
                    -0.39638340835007335, 
                    0.18756032789201682, 
                    -0.25653321152003106, 
                    -0.662988686504699, 
                    -0.12172247780277767, 
                    -0.07848121421888288, 
                    0.024957244927861333, 
                    -0.17986506256740287, 
                    0.2076308355940656, 
                    0.5637857138066444, 
                    0.10565197442370844, 
                    0.18200695451882964, 
                    0.49477094305366043, 
                    0.65123442556072475, 
                    -0.24781966266318656, 
                    0.3889009279747695, 
                    -0.013425726456290588, 
                    0.5142316131436673, 
                    -0.19342537937240967, 
                    -0.27207528700110856, 
                    -0.028439648275369933, 
                    0.23939559857077053, 
                    0.19427176905406196, 
                    0.22946709088456274, 
                    -0.12408132366214053, 
                    -0.28589534586530835, 
                    0.05246190752103619, 
                    0.4089614224479283, 
                    0.05085578169977101, 
                    -0.09555409874749399, 
                    -0.516192321108973, 
                    -0.294137103077014, 
                    0.41968225585977725, 
                    0.04395352960276, 
                    0.4151938968459421, 
                    -0.6885270938499841, 
                    0.6354293264417221, 
                    -0.5715403169315819, 
                    -0.6075466786939301, 
                    0.028929818724602097, 
                    -0.34321423560183123, 
                    0.14896107005126624, 
                    -0.03854900732855626, 
                    -0.3864502597309486, 
                    0.6075280738613258, 
                    0.22382340478818818, 
                    0.30537969453397884, 
                    0.46401678086105735, 
                    -0.039848800071438095, 
                    0.27643756734263425, 
                    0.18754312965649456, 
                    -0.5392130407433982, 
                    0.3312489846672142, 
                    -0.07004121388052298, 
                    0.008612795623399672, 
                    0.2767815580101749, 
                    -0.5492349868134101, 
                    0.16980210573617838, 
                    0.5925525043202791, 
                    -0.6570566009275621, 
                    -0.47729086473400584, 
                    -0.2394720312574875, 
                    0.26395592861401895, 
                    0.022487796060177367, 
                    -0.6573026242018603, 
                    0.4949684787025088, 
                    -0.29709130254854327, 
                    0.21265627734327763, 
                    0.2266517211904876, 
                    0.13484074707672122, 
                    0.37165198945523215, 
                    -0.16100749673971038, 
                    -0.1106679399747823, 
                    -0.06558907119523993, 
                    0.1725386279679043, 
                    -0.13523109993631077, 
                    -0.6864919425303626, 
                    0.1971844091671443, 
                    0.1969885744284322, 
                    0.05516508122016528, 
                    -0.06352325731097908, 
                    -0.0958563959437877, 
                    -0.2090715871989356, 
                    0.2510376461582984, 
                    -0.44586892786744753, 
                    0.3229281562781101, 
                    -0.30768038086690663, 
                    0.3916026902800631, 
                    0.006699163564912625, 
                    0.4152878351028859, 
                    -0.21838777158446054, 
                    0.2846181424622076, 
                    0.11163772366737701, 
                    -0.0660981582606407, 
                    -0.2884111053307761, 
                    -0.09990988084765051, 
                    0.3159456342251501, 
                    0.2252038396550492, 
                    0.06233146947709056, 
                    0.4441519459646792, 
                    0.2839996605139701, 
                    -0.4179920865925785, 
                    0.4406539502535436, 
                    -0.14715100022723637, 
                    0.1996191059499336, 
                    0.2657276339099932, 
                    0.46704827498677504, 
                    -0.3826133200065501, 
                    0.5922740548560439, 
                    0.39969462460095984, 
                    -0.31322354839686267, 
                    -0.04945592170190516, 
                    0.34257012809004017, 
                    -0.030516996123509, 
                    -0.5105403366123493, 
                    -0.21143232640641707, 
                    0.07530198129475507, 
                    0.08594456572700493, 
                    0.32793518824451096, 
                    0.1844557589828556, 
                    -0.6463182089044281, 
                    0.04380699819501965, 
                    -0.48955967863295846, 
                    0.13782825109321306, 
                    0.617002463542714, 
                    0.4357690044651039, 
                    -0.4670065925168316, 
                    0.0765314302636888, 
                    0.20698570041842557, 
                    -0.23563983318054138, 
                    -0.03769282298362886, 
                    0.15284989994247955, 
                    0.2788732699149665, 
                    -0.6804454486107043, 
                    0.3622606269114068, 
                    -0.06748752387756418, 
                    0.57100535678750175, 
                    -0.39428809450960206, 
                    -0.6190338849827188, 
                    0.43222245725579933, 
                    -0.25713332124527966, 
                    0.20418883974311852, 
                    0.5892319881665138, 
                    0.6374421608542247, 
                    0.6759818439369674, 
                    -0.6865194992793586, 
                    0.08260106383048871, 
                    -0.3273996879128121, 
                    0.2878811733749622, 
                    -0.2496679228527603, 
                    0.35851220851330834, 
                    0.5907858194624993, 
                    0.12308039220184741, 
                    0.300810111571048, 
                    0.5026017813504275, 
                    0.3119508442235943, 
                    -0.26348610685081797, 
                    -0.5690445722650824, 
                    -0.11364766806084947, 
                    -0.19130292939031934, 
                    -0.5448266598474575, 
                    0.5587165950101156, 
                    0.645331278271235, 
                    -0.4084583366734913, 
                    0.25552945705039454, 
                    -0.43606843731277356, 
                    -0.498295019498138, 
                    0.012152400534281682, 
                    -0.5657840957728107, 
                    -0.6360710781048671, 
                    -0.3501485244689132, 
                    -0.30291030816399883, 
                    -0.4130560184677796, 
                    0.3413160760685008, 
                    -0.11752261026369515, 
                    0.18431279733797112, 
                    0.09281177828954457, 
                    -0.36722890641966516, 
                    -0.23243945608634398, 
                    -0.20214394555751114, 
                    0.20061576642420187, 
                    0.32596140601678125, 
                    0.503538980728377, 
                    0.4029662514641632, 
                    0.4845324982496111, 
                    0.010276660426949857, 
                    -0.030075449802414944, 
                    -0.5201000887232887, 
                    0.5776408703059115, 
                    -0.10584146057944621, 
                    0.3905359093058556, 
                    -0.3801806950350358, 
                    -0.7563567177107767, 
                    0.347708459902103, 
                    -0.611934232551976, 
                    -0.13472686182438243, 
                    0.013950090690106554, 
                    0.41326525926353397, 
                    0.22394676160387283, 
                    0.21216947758297056, 
                    -0.10677533591995003, 
                    -0.42840980276528656, 
                    -0.09787456707822051, 
                    -0.2569113652002273, 
                    -0.09282443105780391, 
                    -0.23058091331585273, 
                    -0.21379724076217765, 
                    -0.4148331458207144, 
                    -0.3882932683294198, 
                    -0.07157783388666572, 
                    -0.6647753053963418, 
                    0.5463959697412668, 
                    0.0364011460507333, 
                    -0.5226049374220538, 
                    -0.5539624598710716, 
                    0.503267766834476, 
                    -0.5801313815283063, 
                    0.4120106059933599, 
                    -0.13846323725258292 
)
)

