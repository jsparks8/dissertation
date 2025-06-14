model <- " model {

# specify prior distributions

for (j in 1:J) {

b[j] ~ dnorm(0,0.25);

a[j] ~ dnorm(0,4);

tau[j,1] <- 0;

}

tau[1,2] ~ dlnorm(0.221644, 1);
tau[1,3] ~ dlnorm(0.100728, 1);
tau[1,4] ~ dlnorm(-0.036845, 1);
tau[1,5] ~ dlnorm(-0.196411, 1);
tau[1,6] ~ dlnorm(-0.386364, 1);
tau[2,2] ~ dlnorm(0.170744, 1);
tau[2,3] ~ dlnorm(0.048134, 1);
tau[2,4] ~ dlnorm(-0.091637, 1);
tau[2,5] ~ dlnorm(-0.25417, 1);
tau[2,6] ~ dlnorm(-0.448345, 1);
tau[3,2] ~ dlnorm(0.129389, 1);
tau[3,3] ~ dlnorm(0.005335, 1);
tau[3,4] ~ dlnorm(-0.136316, 1);
tau[3,5] ~ dlnorm(-0.301397, 1);
tau[3,6] ~ dlnorm(-0.499225, 1);
tau[4,2] ~ dlnorm(0.126472, 1);
tau[4,3] ~ dlnorm(0.002315, 1);
tau[4,4] ~ dlnorm(-0.139473, 1);
tau[4,5] ~ dlnorm(-0.304739, 1);
tau[4,6] ~ dlnorm(-0.502832, 1);
tau[5,2] ~ dlnorm(-0.334598, 1);
tau[5,3] ~ dlnorm(-0.47993, 1);
tau[5,4] ~ dlnorm(-0.650033, 1);
tau[5,5] ~ dlnorm(-0.855125, 1);
tau[5,6] ~ dlnorm(-1.113426, 1);
tau[6,2] ~ dlnorm(-0.24871, 1);
tau[6,3] ~ dlnorm(-0.389284, 1);
tau[6,4] ~ dlnorm(-0.552904, 1);
tau[6,5] ~ dlnorm(-0.748634, 1);
tau[6,6] ~ dlnorm(-0.992234, 1);
tau[7,2] ~ dlnorm(0.018397, 1);
tau[7,3] ~ dlnorm(-0.109851, 1);
tau[7,4] ~ dlnorm(-0.257001, 1);
tau[7,5] ~ dlnorm(-0.429602, 1);
tau[7,6] ~ dlnorm(-0.63834, 1);
tau[8,2] ~ dlnorm(-0.054666, 1);
tau[8,3] ~ dlnorm(-0.185951, 1);
tau[8,4] ~ dlnorm(-0.337115, 1);
tau[8,5] ~ dlnorm(-0.51527, 1);
tau[8,6] ~ dlnorm(-0.732196, 1);
tau[9,2] ~ dlnorm(0.024943, 1);
tau[9,3] ~ dlnorm(-0.103044, 1);
tau[9,4] ~ dlnorm(-0.24985, 1);
tau[9,5] ~ dlnorm(-0.421978, 1);
tau[9,6] ~ dlnorm(-0.630024, 1);
tau[10,2] ~ dlnorm(0.07879, 1);
tau[10,3] ~ dlnorm(-0.047116, 1);
tau[10,4] ~ dlnorm(-0.191189, 1);
tau[10,5] ~ dlnorm(-0.35957, 1);
tau[10,6] ~ dlnorm(-0.562161, 1);

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
                                  
resp <- matrix(data, nrow= 500 , ncol= 10 , byrow=TRUE)

N <- 500 

J <- 10 

K <- 6 

data_list <- list("r" = resp, "N" = N, "J" = J, "K" = K)

# specify starting values for all estimated parameters

starts <-  list(a=
c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
),
b=c( 1.5063346249481135, 
                    1.3563760483093397, 
                    1.240034251907566, 
                    1.232009080806023, 
                    0.21713200698022872, 
                    0.37251572984676895, 
                    -0.9505620517083442, 
                    -0.7768161326425052, 
                    -0.9667584869617049, 
                    -1.1040792849444556 ),
tau=matrix(c( NA,2.0578138795896654,1.823446471342675,1.5890790630956844,1.3547116548486937,1.120344246601703, 
                    NA,1.9556920888986602,1.7300222780967185,1.5043524672947768,1.278682656492835,1.0530128456908934, 
                    NA,1.8764633255490526,1.6575413389384137,1.4386193523277748,1.219697365717136,1.000775379106497, 
                    NA,1.8709981840289014,1.652541657342152,1.4340851306554028,1.2156286039686535,0.9971720772819042, 
                    NA,1.1798668967535355,1.0202732403486823,0.860679583943829,0.7010859275389758,0.5414922711341226, 
                    NA,1.2856832120256498,1.1170772996945373,0.9484713873634245,0.77986547503231185,0.6112595627011992, 
                    NA,1.6793327572133825,1.4772001582142984,1.2750675592152145,1.0729349602161307,0.8708023612170466, 
                    NA,1.5610117863295458,1.3689564506362806,1.1769011149430153,0.98484577924975,0.7927904435564848, 
                    NA,1.690362529620921,1.4872905373771421,1.2842185451333632,1.0811465528895843,0.8780745606458054, 
                    NA,1.7838779930471744,1.5728413945203958,1.3618047959936175,1.1507681974668391,0.9397315989400605 ), ncol= 6 , nrow= 10 , byrow=TRUE),
theta = c(
 0.34993325730361113, 
                    0.47818761588949077, 
                    0.4159112505434789, 
                    -0.18873124814340786, 
                    0.10765017861404558, 
                    -0.5468307760521075, 
                    0.2937432283062552, 
                    0.7360763548954673, 
                    -0.15860273494180088, 
                    0.19528796000542414, 
                    -0.3705015670429854, 
                    0.44111123402137575, 
                    -0.3974518945198062, 
                    0.2922607124233664, 
                    0.15893977954810068, 
                    -0.5227001166934029, 
                    -0.06505443526421173, 
                    0.5934309432731678, 
                    -0.4228942001028254, 
                    -0.1393037860456074, 
                    0.309809900955595, 
                    0.5358106229299576, 
                    0.08910983001168704, 
                    0.051852383808624336, 
                    -0.13315755478324187, 
                    0.09034555560848023, 
                    0.0758814270828394, 
                    -0.25639140744556854, 
                    -0.37890068719793357, 
                    0.3335452528752326, 
                    0.22513492900699894, 
                    0.4594809705061794, 
                    0.259919707723557, 
                    -0.40063494198704264, 
                    -0.2951466652163019, 
                    -0.4370834485655824, 
                    -0.43049215619370185, 
                    -0.20966101675095372, 
                    0.6050459709196593, 
                    0.2417173277930994, 
                    -0.4125702783724114, 
                    0.8155242173196092, 
                    0.30072952361733585, 
                    -0.4176229454586937, 
                    0.04367993303171591, 
                    -0.5621631331581339, 
                    0.055296181034569236, 
                    -0.4234768750990767, 
                    -0.225302827925059, 
                    -0.0031344524959628917, 
                    -0.0826502652357235, 
                    0.7196546381147452, 
                    0.10904723757548584, 
                    0.02373577980314312, 
                    -0.30822752940892767, 
                    -0.25192055672533115, 
                    -0.5204872318096131, 
                    -0.25199169175275526, 
                    0.225006300526446, 
                    -0.04728360758064798, 
                    -0.14146121921528265, 
                    0.7789839813156675, 
                    -0.35577862923205317, 
                    -0.5165525504566172, 
                    -0.20390648939250827, 
                    -0.015594159558762644, 
                    0.6110340788514078, 
                    0.30907972743055856, 
                    -0.31679724328424774, 
                    0.15232465983459276, 
                    0.0023557358909398474, 
                    -0.26624089614418656, 
                    0.16353944380566077, 
                    -0.363185525801039, 
                    -0.08772102424953387, 
                    -0.3277426208438019, 
                    0.14254459269516018, 
                    0.7632194561052695, 
                    0.257027166565592, 
                    -0.5218341196098, 
                    -0.2213887577432564, 
                    -0.05448033978775779, 
                    -0.5134571868994863, 
                    -0.35026857507630244, 
                    0.8010784723664955, 
                    -0.3933750330861729, 
                    -0.23865160157809256, 
                    -0.3485334378505286, 
                    -0.2556150877782113, 
                    -0.20342810439800674, 
                    0.7194530762375323, 
                    0.1846604064746955, 
                    -0.4823963075362759, 
                    -0.5151515842611138, 
                    -0.26054052185735543, 
                    0.7153456445201779, 
                    -0.3304330352713996, 
                    0.43768824807575135, 
                    -0.13590244451878908, 
                    0.6773346440382443, 
                    0.3399719937795005, 
                    -0.34345003406932134, 
                    -0.429610375574799, 
                    0.06499032040313923, 
                    -0.27484598504541197, 
                    0.3327605502673836, 
                    -0.26853355723556044, 
                    0.579286868808058, 
                    -0.2297143359506547, 
                    0.28249437677887435, 
                    0.14622310811953998, 
                    0.41620277769254965, 
                    0.09330334730757184, 
                    0.05320176962839174, 
                    0.2621057488067122, 
                    -0.2002005971699058, 
                    0.6849610003238215, 
                    -0.31145759657676203, 
                    -0.07055986906649464, 
                    -0.13970204023774446, 
                    -0.06912508176661775, 
                    -0.12100990680886647, 
                    0.2709632496483425, 
                    0.625079279394935, 
                    0.2882416794925371, 
                    -0.4365679577353052, 
                    0.42462849028033245, 
                    0.2619891039664929, 
                    0.6224869507975727, 
                    0.44332957125422323, 
                    0.2977618932784363, 
                    -0.24302105379683947, 
                    0.33078441562346217, 
                    -0.28104650965065536, 
                    -0.23673251349816193, 
                    0.34680963869216086, 
                    -0.3591676985576134, 
                    -0.13803742371291094, 
                    0.1016738227727878, 
                    -0.285635319378497, 
                    -0.21737345717277834, 
                    -0.5040277336398855, 
                    -0.032148083916224146, 
                    0.06395694273859032, 
                    0.11253192120704314, 
                    0.7487145394767735, 
                    -0.3819744778942994, 
                    -0.15189819427721823, 
                    -0.4091654229674109, 
                    -0.4862131580295188, 
                    -0.1256109070561512, 
                    0.6474056964230509, 
                    0.3195142195094699, 
                    -0.04720910838130843, 
                    0.7650996177376089, 
                    0.5871769228354028, 
                    -0.4411084546636295, 
                    0.5266843622598646, 
                    0.04011435889542503, 
                    0.5055634765489586, 
                    -0.5290315285040428, 
                    0.027386668430544936, 
                    0.008662112995446725, 
                    -0.06678381020564006, 
                    -0.4182854262432504, 
                    0.27870274608457235, 
                    -0.3042432296117898, 
                    -0.4566545128757023, 
                    0.18113657950413342, 
                    0.7389526350271794, 
                    -0.24144116061150478, 
                    -0.2092577012936203, 
                    -0.24728826317323865, 
                    -0.19272590282029362, 
                    0.7019302155914179, 
                    -0.4014064471275556, 
                    -0.22373778225970203, 
                    -0.1539585802054324, 
                    0.26632105727044675, 
                    0.7420881438492407, 
                    0.03699942480124785, 
                    -0.34393784757218293, 
                    -0.13132935577274962, 
                    0.04082807793139698, 
                    0.052200098435746356, 
                    0.7554314378656098, 
                    0.23592777811051469, 
                    0.2568704410638639, 
                    0.43583325030132336, 
                    0.03152170734120663, 
                    0.7073683379413706, 
                    0.9113345756641761, 
                    -0.4029759743314269, 
                    0.6154013453533046, 
                    -0.1482153197064161, 
                    0.41578323489740077, 
                    0.32815600598834227, 
                    -0.1981175621722348, 
                    -0.1574333738713865, 
                    -0.48429352096645134, 
                    -0.07336994595131846, 
                    0.19578460447367896, 
                    -0.3370633891031987, 
                    -0.5283591508156378, 
                    0.35370626735656696, 
                    0.18650575270018888, 
                    -0.17214624777842935, 
                    0.6169072621925867, 
                    0.7586106428394578, 
                    -0.3063608841661542, 
                    -0.08611187016463806, 
                    -0.0536519039481923, 
                    -0.37113649278170646, 
                    -0.39259536354709434, 
                    -0.43031701475157513, 
                    0.29143356944297916, 
                    0.09374393387028468, 
                    -0.47966342167368436, 
                    -0.08237491399091457, 
                    -0.33625910840029305, 
                    -0.02379953152537273, 
                    0.6453142662604928, 
                    -0.23732128988770412, 
                    0.8761659451064981, 
                    0.5592873330825362, 
                    -0.4742065343569323, 
                    -0.04475755816187921, 
                    -0.29748181428806614, 
                    0.7369236993653164, 
                    0.07594181429490654, 
                    -0.2650011458201911, 
                    -0.4760319691210654, 
                    0.03874947178029997, 
                    0.08618982330741798, 
                    -0.1769382567078993, 
                    -0.4721693340268399, 
                    -0.42796439527766955, 
                    -0.25391125380585483, 
                    -0.3645443579948551, 
                    -0.5621631331581339, 
                    -0.11831271420442574, 
                    -0.1476786005876305, 
                    -0.5227001166934029, 
                    -0.09751920320291574, 
                    0.04994803752623955, 
                    0.004854268077392088, 
                    0.3057996279100178, 
                    -0.2601898400936211, 
                    -0.36222226385112355, 
                    0.7228898215647422, 
                    -0.5234548366019681, 
                    -0.05422328772846574, 
                    0.17102834833223768, 
                    -0.343012703090809, 
                    -0.3850747308825492, 
                    -0.37252526532423375, 
                    0.4229913845656613, 
                    0.28765316176583366, 
                    -0.4446408102137799, 
                    0.4130448690481603, 
                    0.7861885741835336, 
                    -0.11682229163998803, 
                    -0.4392269203028363, 
                    -0.1825201965407241, 
                    0.46758269506683703, 
                    -0.2970585258619766, 
                    0.6889392970464966, 
                    -0.43031701475157513, 
                    -0.2839084694757024, 
                    0.28185391464330134, 
                    0.2923142989579187, 
                    0.5073622697816967, 
                    -0.09978930908023204, 
                    -0.14031524486176317, 
                    0.18713572626558894, 
                    0.5127014400648492, 
                    0.41384143403508167, 
                    -0.00820726752251133, 
                    0.45966804440552933, 
                    -0.5434763693034332, 
                    -0.039810405783940794, 
                    0.6749805410002961, 
                    -0.364214471470486, 
                    -0.12794609947061636, 
                    -0.5370245802504449, 
                    -0.44659329088151267, 
                    0.7103349569218308, 
                    -0.2227364354292682, 
                    -0.5621631331581339, 
                    -0.2938339845648793, 
                    0.31732523405789104, 
                    0.4727035509015217, 
                    -0.2593137295441777, 
                    -0.4356610433200344, 
                    0.6018966706355419, 
                    -0.32576573598419023, 
                    -0.06712946717887464, 
                    -0.2883030046598667, 
                    -0.41334175833624287, 
                    -0.2802898711741686, 
                    -0.48939248299911503, 
                    0.10505879780270921, 
                    0.24817480579309026, 
                    0.22651746160816133, 
                    -0.49440486115610016, 
                    -0.5370245802504449, 
                    0.254562552174016, 
                    0.8565642481010941, 
                    -0.3943259725851918, 
                    -0.3819120125794248, 
                    0.26098627437260824, 
                    -0.11092175261332199, 
                    -0.5351428063270378, 
                    0.017062384902010175, 
                    0.7459412659363086, 
                    -0.402927485475006, 
                    -0.34524883387896593, 
                    0.2880823262010902, 
                    -0.45673731087582364, 
                    0.369221593337126, 
                    -0.2399160918017697, 
                    0.6723602186923215, 
                    0.20677753112897357, 
                    0.14131244736447302, 
                    -0.39238369914252, 
                    -0.26652826120425843, 
                    0.558614468856682, 
                    -0.4272520650133549, 
                    -0.21082396720825053, 
                    0.32320566135643747, 
                    -0.09393449827774575, 
                    0.032949519764031154, 
                    0.45789417835767166, 
                    -0.2882800140751611, 
                    0.1404958300182758, 
                    -0.529139594775923, 
                    -0.31036417548585693, 
                    -0.0022309246765318536, 
                    0.1363230066679646, 
                    -0.5232399978685148, 
                    0.2625802018763995, 
                    -0.12238569269764349, 
                    -0.12667741167728186, 
                    0.47124218275402774, 
                    -0.46238724679893295, 
                    -0.2452288115748692, 
                    0.12003250616634864, 
                    -0.4158189662177671, 
                    -0.471964890426249, 
                    0.6757155888917142, 
                    0.1824520098758271, 
                    0.6602450772054959, 
                    0.14130821326233334, 
                    -0.4771923274304155, 
                    -0.30142718685112835, 
                    0.5254956192302579, 
                    -0.04412920781017682, 
                    0.6280793533684778, 
                    -0.3904079232857613, 
                    0.35520609524935487, 
                    -0.36007662786975514, 
                    -0.4071248485784764, 
                    -0.3881355391652961, 
                    -0.5290315285040428, 
                    0.7766437621234247, 
                    0.3448333677205526, 
                    -0.38819293117962334, 
                    -0.26447198534862776, 
                    -0.28335530117231716, 
                    -0.17677353350465708, 
                    0.001746917004097348, 
                    -0.287849814997669, 
                    -0.03667477016146237, 
                    -0.3116266395069489, 
                    -0.05503555461330911, 
                    -0.5086014455599069, 
                    0.7504349364886703, 
                    -0.16550543590607136, 
                    -0.11324242495098719, 
                    0.02639055348254371, 
                    0.5270751157584296, 
                    -0.5547613055897074, 
                    0.5699816455944163, 
                    -0.4999493588798231, 
                    0.04155054976990169, 
                    -0.5621631331581339, 
                    -0.0048208522149630895, 
                    -0.5434763693034332, 
                    -0.4266759331167118, 
                    0.12020057418751795, 
                    -0.38666524289464266, 
                    -0.5073877590979441, 
                    -0.37210395682586206, 
                    0.09083303546424393, 
                    0.025034014499780932, 
                    0.3416156825277753, 
                    -0.45120246706550426, 
                    -0.07714443337177734, 
                    0.1829046514245123, 
                    0.03729760260889414, 
                    0.6466214655800443, 
                    0.6092681753445753, 
                    -0.013211708115526855, 
                    -0.03470159171127973, 
                    -0.42028437687731246, 
                    0.23045048972133086, 
                    -0.16182711175026399, 
                    -0.11224021865964617, 
                    -0.4135576678424875, 
                    0.6568051229969089, 
                    0.8497562905971535, 
                    -0.5547613055897074, 
                    0.7546470779135929, 
                    -0.5002747724656348, 
                    0.7164221484711798, 
                    -0.09068386068271361, 
                    -0.214428245778294, 
                    -0.27558784824459104, 
                    -0.3734767214218375, 
                    -0.08191521957184278, 
                    -0.10561961661809405, 
                    0.27407863823347733, 
                    0.17775430437574402, 
                    -0.1185560814547828, 
                    0.018348044755943094, 
                    -0.33039675830344506, 
                    -0.003033183396306982, 
                    0.8015442996454923, 
                    0.1616600697513494, 
                    0.0022831985489080164, 
                    0.4009445499747246, 
                    -0.19617336349121273, 
                    0.007705088295756002, 
                    0.2107318035504694, 
                    -0.2598893549075232, 
                    -0.0719854894681316, 
                    0.8381790911877168, 
                    -0.5547613055897074, 
                    0.7636941155111076, 
                    0.7509534847376348, 
                    -0.07034601873401602, 
                    -0.12562591547263852, 
                    -0.4304446291016608, 
                    0.04837384942193801, 
                    0.6815879433456374, 
                    -0.20951156515078978, 
                    -0.3988133812492244, 
                    -0.4988345643168341, 
                    0.046249068763241974, 
                    0.09503403616552342, 
                    -0.5370245802504449, 
                    -0.32843096005505756, 
                    0.5134566840477607, 
                    -0.009235709868769537, 
                    -0.4977351655632246, 
                    -0.48102142577658447, 
                    0.5176344778020655, 
                    0.48729732118844715, 
                    0.5873271354360242, 
                    -0.3926129897454212, 
                    -0.45083525599551066, 
                    0.6183566262859373, 
                    0.524321854266589, 
                    -0.23429242315546117, 
                    0.47673456159002237, 
                    -0.02201441411320515, 
                    -0.2821014996660974, 
                    0.04510768997038428, 
                    0.09512769102905905, 
                    0.16667498601295938, 
                    -0.5040277336398855, 
                    -0.5621631331581339, 
                    -0.2678746322196479, 
                    -0.04028224028572003, 
                    0.8323668873738194, 
                    0.7060448965927734, 
                    -0.5468307760521075, 
                    -0.4801479553830075, 
                    -0.23972488376301987, 
                    0.831691934457483, 
                    -0.16621317646906447, 
                    0.32590347766059136, 
                    0.25678923424032685, 
                    0.14048718974311725, 
                    0.4198992196030187, 
                    0.13788685118334865, 
                    -0.22029227116889322, 
                    -0.5078461090949907, 
                    -0.1541110735249553, 
                    -0.2615786149885678, 
                    -0.1354177943558756, 
                    0.021421353852159597, 
                    0.46319879372852013, 
                    -0.48429352096645134, 
                    0.7357708433550466, 
                    0.09187250700121163, 
                    0.09397574481039561, 
                    0.5408286155846217, 
                    0.06310779041172632, 
                    -0.24774119861572197 
)
)

