model <- " model {

# specify prior distributions

for (j in 1:J) {

b[j] ~ dnorm(0,0.25);

a[j] ~ dnorm(0,4);

tau[j,1] <- 0;

}

tau[1,2] ~ dlnorm(0.006929, 1);
tau[1,3] ~ dlnorm(-0.121781, 1);
tau[1,4] ~ dlnorm(-0.269538, 1);
tau[1,5] ~ dlnorm(-0.442978, 1);
tau[1,6] ~ dlnorm(-0.652944, 1);
tau[2,2] ~ dlnorm(0.23823, 1);
tau[2,3] ~ dlnorm(0.117848, 1);
tau[2,4] ~ dlnorm(-0.019036, 1);
tau[2,5] ~ dlnorm(-0.177674, 1);
tau[2,6] ~ dlnorm(-0.366313, 1);
tau[3,2] ~ dlnorm(0.1455, 1);
tau[3,3] ~ dlnorm(0.022017, 1);
tau[3,4] ~ dlnorm(-0.118892, 1);
tau[3,5] ~ dlnorm(-0.282965, 1);
tau[3,6] ~ dlnorm(-0.479345, 1);
tau[4,2] ~ dlnorm(0.246241, 1);
tau[4,3] ~ dlnorm(0.126112, 1);
tau[4,4] ~ dlnorm(-0.010443, 1);
tau[4,5] ~ dlnorm(-0.16864, 1);
tau[4,6] ~ dlnorm(-0.356653, 1);
tau[5,2] ~ dlnorm(0.023221, 1);
tau[5,3] ~ dlnorm(-0.104834, 1);
tau[5,4] ~ dlnorm(-0.25173, 1);
tau[5,5] ~ dlnorm(-0.423983, 1);
tau[5,6] ~ dlnorm(-0.63221, 1);
tau[6,2] ~ dlnorm(0.192014, 1);
tau[6,3] ~ dlnorm(0.070122, 1);
tau[6,4] ~ dlnorm(-0.068716, 1);
tau[6,5] ~ dlnorm(-0.229986, 1);
tau[6,6] ~ dlnorm(-0.422361, 1);
tau[7,2] ~ dlnorm(0.151129, 1);
tau[7,3] ~ dlnorm(0.027842, 1);
tau[7,4] ~ dlnorm(-0.11281, 1);
tau[7,5] ~ dlnorm(-0.276535, 1);
tau[7,6] ~ dlnorm(-0.472417, 1);
tau[8,2] ~ dlnorm(0.142318, 1);
tau[8,3] ~ dlnorm(0.018722, 1);
tau[8,4] ~ dlnorm(-0.122332, 1);
tau[8,5] ~ dlnorm(-0.286603, 1);
tau[8,6] ~ dlnorm(-0.483266, 1);
tau[9,2] ~ dlnorm(0.214977, 1);
tau[9,3] ~ dlnorm(0.093845, 1);
tau[9,4] ~ dlnorm(-0.04401, 1);
tau[9,5] ~ dlnorm(-0.203954, 1);
tau[9,6] ~ dlnorm(-0.394443, 1);
tau[10,2] ~ dlnorm(0.117841, 1);
tau[10,3] ~ dlnorm(-0.006626, 1);
tau[10,4] ~ dlnorm(-0.148819, 1);
tau[10,5] ~ dlnorm(-0.314635, 1);
tau[10,6] ~ dlnorm(-0.51352, 1);
tau[11,2] ~ dlnorm(0.107224, 1);
tau[11,3] ~ dlnorm(-0.017629, 1);
tau[11,4] ~ dlnorm(-0.160324, 1);
tau[11,5] ~ dlnorm(-0.326826, 1);
tau[11,6] ~ dlnorm(-0.526699, 1);
tau[12,2] ~ dlnorm(-0.089649, 1);
tau[12,3] ~ dlnorm(-0.222473, 1);
tau[12,4] ~ dlnorm(-0.375681, 1);
tau[12,5] ~ dlnorm(-0.556684, 1);
tau[12,6] ~ dlnorm(-0.777853, 1);
tau[13,2] ~ dlnorm(-0.276358, 1);
tau[13,3] ~ dlnorm(-0.418417, 1);
tau[13,4] ~ dlnorm(-0.584052, 1);
tau[13,5] ~ dlnorm(-0.782677, 1);
tau[13,6] ~ dlnorm(-1.030784, 1);
tau[14,2] ~ dlnorm(-0.218608, 1);
tau[14,3] ~ dlnorm(-0.357615, 1);
tau[14,4] ~ dlnorm(-0.519113, 1);
tau[14,5] ~ dlnorm(-0.711813, 1);
tau[14,6] ~ dlnorm(-0.950728, 1);
tau[15,2] ~ dlnorm(-0.302973, 1);
tau[15,3] ~ dlnorm(-0.446503, 1);
tau[15,4] ~ dlnorm(-0.614142, 1);
tau[15,5] ~ dlnorm(-0.815657, 1);
tau[15,6] ~ dlnorm(-1.068301, 1);
tau[16,2] ~ dlnorm(-0.214569, 1);
tau[16,3] ~ dlnorm(-0.35337, 1);
tau[16,4] ~ dlnorm(-0.514589, 1);
tau[16,5] ~ dlnorm(-0.706891, 1);
tau[16,6] ~ dlnorm(-0.945195, 1);
tau[17,2] ~ dlnorm(-0.38431, 1);
tau[17,3] ~ dlnorm(-0.532598, 1);
tau[17,4] ~ dlnorm(-0.70677, 1);
tau[17,5] ~ dlnorm(-0.917811, 1);
tau[17,6] ~ dlnorm(-1.185643, 1);
tau[18,2] ~ dlnorm(-0.280704, 1);
tau[18,3] ~ dlnorm(-0.423, 1);
tau[18,4] ~ dlnorm(-0.588958, 1);
tau[18,5] ~ dlnorm(-0.788048, 1);
tau[18,6] ~ dlnorm(-1.036882, 1);
tau[19,2] ~ dlnorm(-0.020592, 1);
tau[19,3] ~ dlnorm(-0.150432, 1);
tau[19,4] ~ dlnorm(-0.299683, 1);
tau[19,5] ~ dlnorm(-0.475183, 1);
tau[19,6] ~ dlnorm(-0.688182, 1);
tau[20,2] ~ dlnorm(0.026404, 1);
tau[20,3] ~ dlnorm(-0.101525, 1);
tau[20,4] ~ dlnorm(-0.248255, 1);
tau[20,5] ~ dlnorm(-0.420278, 1);
tau[20,6] ~ dlnorm(-0.62817, 1);
tau[21,2] ~ dlnorm(0.00934, 1);
tau[21,3] ~ dlnorm(-0.119272, 1);
tau[21,4] ~ dlnorm(-0.266901, 1);
tau[21,5] ~ dlnorm(-0.440164, 1);
tau[21,6] ~ dlnorm(-0.64987, 1);
tau[22,2] ~ dlnorm(0.037358, 1);
tau[22,3] ~ dlnorm(-0.090139, 1);
tau[22,4] ~ dlnorm(-0.236299, 1);
tau[22,5] ~ dlnorm(-0.407541, 1);
tau[22,6] ~ dlnorm(-0.614292, 1);
tau[23,2] ~ dlnorm(-0.166533, 1);
tau[23,3] ~ dlnorm(-0.302942, 1);
tau[23,4] ~ dlnorm(-0.460943, 1);
tau[23,5] ~ dlnorm(-0.648679, 1);
tau[23,6] ~ dlnorm(-0.88, 1);
tau[24,2] ~ dlnorm(0.048533, 1);
tau[24,3] ~ dlnorm(-0.078527, 1);
tau[24,4] ~ dlnorm(-0.224115, 1);
tau[24,5] ~ dlnorm(-0.394571, 1);
tau[24,6] ~ dlnorm(-0.600176, 1);
tau[25,2] ~ dlnorm(0.070697, 1);
tau[25,3] ~ dlnorm(-0.055515, 1);
tau[25,4] ~ dlnorm(-0.199988, 1);
tau[25,5] ~ dlnorm(-0.368916, 1);
tau[25,6] ~ dlnorm(-0.5723, 1);
tau[26,2] ~ dlnorm(0.129842, 1);
tau[26,3] ~ dlnorm(0.005805, 1);
tau[26,4] ~ dlnorm(-0.135826, 1);
tau[26,5] ~ dlnorm(-0.300878, 1);
tau[26,6] ~ dlnorm(-0.498665, 1);
tau[27,2] ~ dlnorm(0.257566, 1);
tau[27,3] ~ dlnorm(0.137793, 1);
tau[27,4] ~ dlnorm(0.001698, 1);
tau[27,5] ~ dlnorm(-0.155882, 1);
tau[27,6] ~ dlnorm(-0.343024, 1);
tau[28,2] ~ dlnorm(0.091002, 1);
tau[28,3] ~ dlnorm(-0.034448, 1);
tau[28,4] ~ dlnorm(-0.177923, 1);
tau[28,5] ~ dlnorm(-0.345489, 1);
tau[28,6] ~ dlnorm(-0.546899, 1);
tau[29,2] ~ dlnorm(0.180514, 1);
tau[29,3] ~ dlnorm(0.058236, 1);
tau[29,4] ~ dlnorm(-0.081104, 1);
tau[29,5] ~ dlnorm(-0.243053, 1);
tau[29,6] ~ dlnorm(-0.436395, 1);
tau[30,2] ~ dlnorm(0.109451, 1);
tau[30,3] ~ dlnorm(-0.015321, 1);
tau[30,4] ~ dlnorm(-0.15791, 1);
tau[30,5] ~ dlnorm(-0.324267, 1);
tau[30,6] ~ dlnorm(-0.523931, 1);

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
b=c( 0.9224446226438263, 
                    1.556872044337159, 
                    1.2847883566918623, 
                    1.5815810882580932, 
                    0.9624880368190978, 
                    1.4181115961286248, 
                    1.300594362774388, 
                    1.2758901292355727, 
                    1.4862563490545257, 
                    1.2083984174462234, 
                    1.1796318025689085, 
                    0.6980132235735372, 
                    -0.3210323575474387, 
                    0.43020907186283686, 
                    0.2727992923624609, 
                    0.4380832563956516, 
                    0.13310934069963576, 
                    0.313068881210546, 
                    -0.8562664366510171, 
                    -0.9703867066754321, 
                    -0.9283292368752649, 
                    -0.9977671887849475, 
                    -0.5342120715208794, 
                    -1.0260103046460827, 
                    -1.0829651147036625, 
                    -1.2412830509967314, 
                    -1.6168543345452364, 
                    -1.1362663777338866, 
                    -1.384569840661335, 
                    -1.185641150424048 ),
tau=matrix(c( NA,1.660184788020446,1.459682999907104,1.259181211793762,1.05867942368042,0.858177635567078, 
                    NA,2.092229862193606,1.8549312836220504,1.617632705050495,1.3803341264789397,1.1430355479073844, 
                    NA,1.9069408709071582,1.6854231462190303,1.4639054215309022,1.242387696842774,1.0208699721546461, 
                    NA,2.109056721103762,1.8703250179847923,1.6315933148658228,1.3928616117468533,1.1541299086278838, 
                    NA,1.6874543530738058,1.484630046938298,1.2818057408027903,1.0789814346672826,0.8761571285317749, 
                    NA,1.9977339969635934,1.7684835243881332,1.539233051812673,1.3099825792372126,1.0807321066617523, 
                    NA,1.917704761049358,1.6952702880084436,1.4728358149675291,1.2504013419266147,1.0279668688857002, 
                    NA,1.9008811780094248,1.6798795505137616,1.4588779230180984,1.2378762955224352,1.016874668026772, 
                    NA,2.044140573706132,1.8109377054609697,1.577734837215807,1.3445319689706445,1.111329100725482, 
                    NA,1.8549193222808784,1.6378322140689974,1.4207451058571163,1.2036579976452353,0.9865708894333542, 
                    NA,1.8353292575494267,1.61991061300043,1.4044919684514332,1.1890733239024365,0.9736546793534397, 
                    NA,1.5073470052535791,1.3198622382863139,1.1323774713190486,0.9448927043517834,0.7574079373845182, 
                    NA,1.2506230354898058,1.0850031587520543,0.9193832820143029,0.7537634052765514,0.5881435285387999, 
                    NA,1.324972377938592,1.1530202517705475,0.9810681256025029,0.8091159994344583,0.6371638732664138, 
                    NA,1.2177763180988361,1.0549539591418133,0.8921316001847905,0.7293092412277677,0.566486882270745, 
                    NA,1.3303346976054387,1.1579258687344909,0.9855170398635431,0.8131082109925953,0.6406993821216476, 
                    NA,1.122647461016452,0.9679271192558732,0.8132067774952942,0.6584864357347153,0.5037660939741364, 
                    NA,1.2451999081043819,1.08004191299417,0.9148839178839585,0.7497259227737468,0.5845679276635352, 
                    NA,1.6151174433593425,1.4184539900335835,1.2217905367078246,1.0251270833820656,0.8284636300563066, 
                    NA,1.6928333472459693,1.4895509182587943,1.2862684892716192,1.082986060284444,0.879703631297269, 
                    NA,1.6641922103120554,1.46334911457329,1.2625060188345247,1.0616629230957595,0.860819827356994, 
                    NA,1.7114794555625492,1.5066089586130222,1.3017384616634953,1.0968679647139683,0.8919974677644413, 
                    NA,1.3957984207057188,1.2178141205575077,1.0398298204092968,0.8618455202610859,0.6838612201128749, 
                    NA,1.7307130174639824,1.5242044197945095,1.3176958221250368,1.111187224455564,0.9046786267860911, 
                    NA,1.7694992431131944,1.559687266460382,1.3498752898075694,1.1400633131547568,0.9302513365019445, 
                    NA,1.8773137577287742,1.6583193407709638,1.4393249238131534,1.220330506855343,1.0013360898975325, 
                    NA,2.133077801825306,1.8923002504216822,1.6515226990180585,1.4107451476144348,1.1699675962108111, 
                    NA,1.805797403236777,1.5928939533282116,1.379990503419646,1.1670870535110804,0.9541836036025151, 
                    NA,1.9748920614903689,1.7475870107320115,1.5202819599736541,1.2929769092152967,1.0656718584569393, 
                    NA,1.8394216234387768,1.6236544367141819,1.4078872499895871,1.1921200632649924,0.9763528765403975 ), ncol= 6 , nrow= 30 , byrow=TRUE),
theta = c(
 -0.5231541681683686, 
                    -0.6413845037085086, 
                    0.04243969160042316, 
                    0.04649686941765807, 
                    0.39514671991489436, 
                    0.13662602914647326, 
                    -0.325043446346278, 
                    -0.5736587231341091, 
                    0.26493008143349783, 
                    0.10763312465491226, 
                    0.18186259071347222, 
                    0.5789993981250487, 
                    -0.28062024174507505, 
                    0.2702770491292844, 
                    -0.556768528520136, 
                    0.4188669068913813, 
                    0.5203218280826958, 
                    0.12434318090995511, 
                    -0.28099610394799956, 
                    -0.1726728727005301, 
                    0.13911687358255942, 
                    -0.15104512777034823, 
                    0.17066521770050647, 
                    0.2822374943872873, 
                    -0.12232619713537829, 
                    0.6721063303007397, 
                    -0.42505828934774303, 
                    0.3506581926138197, 
                    0.23173548595685278, 
                    -0.008259022339887623, 
                    0.3232000732770707, 
                    0.042094053253959984, 
                    -0.12988998681332886, 
                    -0.4477287732694868, 
                    -0.1389144537411765, 
                    0.6387951309073514, 
                    -0.4495105185325081, 
                    0.4501391888288626, 
                    0.39757597273150913, 
                    0.4399739347650915, 
                    0.3473387704444846, 
                    0.20626262129447992, 
                    0.07786168904596125, 
                    0.2420759137133366, 
                    0.28073768819828815, 
                    0.7029553223817673, 
                    -0.37697033055916446, 
                    0.39170561866960996, 
                    0.4095771780998503, 
                    -0.12811172652508085, 
                    0.29152611367711456, 
                    0.12842596628505132, 
                    -0.5537093963150246, 
                    -0.3150857917358825, 
                    0.20520472910322052, 
                    0.279561259948207, 
                    0.6281821936159557, 
                    0.5900416541383184, 
                    -0.03301613331581654, 
                    -0.5729651970149685, 
                    -0.2847819987315481, 
                    -0.5839764244562943, 
                    -0.3806280967931434, 
                    0.7452458521088585, 
                    -0.3682987411661944, 
                    0.10559245701550446, 
                    0.11835879580943731, 
                    0.5380784803790726, 
                    -0.4029997445798352, 
                    -0.13428216184399122, 
                    -0.15870305866687767, 
                    0.4838094452055719, 
                    0.5598528962475922, 
                    -0.5408141120599451, 
                    -0.35034143766279924, 
                    -0.4837753772480449, 
                    0.191564124220981, 
                    -0.018651005483049654, 
                    -0.10754536071951126, 
                    -0.6020617432168542, 
                    -0.380493313440243, 
                    0.05472240640889836, 
                    0.217206169238512, 
                    -0.27661163136438477, 
                    -0.33912395276224927, 
                    -0.37866719014418837, 
                    -0.012690503915906315, 
                    -0.32463997262322836, 
                    0.7111236028593306, 
                    0.7041289132318966, 
                    -0.061118403888384965, 
                    -0.030278071518828553, 
                    0.31865392248912816, 
                    -0.5540901520932693, 
                    -0.5696258892235748, 
                    -0.42398058348010076, 
                    0.5834100017187567, 
                    0.6676117795005496, 
                    0.1751523214145534, 
                    -0.4693709405800547, 
                    -0.5814771724665366, 
                    -0.0906990015721304, 
                    -0.25480788073862354, 
                    0.04729277286424094, 
                    0.10253618411116006, 
                    -0.02621229962583449, 
                    0.043526690959105885, 
                    -0.460365949449402, 
                    0.46188995440666836, 
                    -0.3645503499771053, 
                    -0.40881459443231793, 
                    -0.4678378070914573, 
                    0.33357589586480496, 
                    -0.0358676737499839, 
                    0.11377320041482442, 
                    0.18988564550446818, 
                    0.03966876092473748, 
                    -0.08529266784099843, 
                    -0.1505496145643715, 
                    0.596349187564865, 
                    0.6581948635374517, 
                    0.1702760843831843, 
                    0.6365288058932432, 
                    0.6228893794126615, 
                    0.6119412315435433, 
                    0.15522226526237637, 
                    -0.5508350492163696, 
                    -0.46181655209301997, 
                    -0.5999902093913839, 
                    -0.19900180462909794, 
                    -0.169252346921474, 
                    0.11516826653022838, 
                    0.5194791240931413, 
                    0.008353676859709669, 
                    -0.5068037988379971, 
                    -0.21721318994696442, 
                    0.025028495645215032, 
                    0.6456399030286073, 
                    -0.2837478936701765, 
                    0.5192096205618797, 
                    0.3577106613485368, 
                    0.14177841088326837, 
                    0.14509392538100385, 
                    0.143539772277258, 
                    -0.46433793937079504, 
                    -0.42647515360014443, 
                    0.24686848624934055, 
                    -0.46712428096999803, 
                    -0.4009993125699264, 
                    0.42880800036194744, 
                    -0.2824272025434369, 
                    0.19709868556688892, 
                    -0.5165853154453603, 
                    -0.5214207802164377, 
                    -0.19029528596609557, 
                    -0.25465338935290066, 
                    -0.14971068704539037, 
                    0.3380764715782032, 
                    0.3054528640615082, 
                    -0.03385037159765769, 
                    0.7287290018448283, 
                    0.01089647400548266, 
                    0.12238871614528979, 
                    -0.4043403432201733, 
                    -0.07902414226081833, 
                    -0.14211635482234802, 
                    -0.15240068609302504, 
                    0.5101536740512282, 
                    -0.49429165844363, 
                    -0.21541388745096085, 
                    0.425428012109186, 
                    -0.44877366973193056, 
                    0.27341428906897114, 
                    0.5997629801229551, 
                    0.22010305674177044, 
                    -0.13874265682226294, 
                    -0.5659446087771484, 
                    -0.5263061162795032, 
                    -0.07715721179620505, 
                    0.07075035902392812, 
                    0.323513910669187, 
                    0.4320994601386572, 
                    -0.17823929493040147, 
                    -0.11006846726270614, 
                    -0.5911328096821304, 
                    0.48159529937932666, 
                    0.05337825779776839, 
                    -0.4437795613798283, 
                    0.24387028568954128, 
                    0.36169397782068446, 
                    -0.2760326558525862, 
                    -0.5480855419853072, 
                    -0.14444780225621245, 
                    0.007237799640592013, 
                    -0.26251160829674153, 
                    0.38426844790563397, 
                    -0.3014487035360627, 
                    0.26049274984450455, 
                    -0.2811531769261357, 
                    0.008766096782486743, 
                    -0.06054580431841794, 
                    0.36833130592229724, 
                    0.08413770310158486, 
                    0.18154577281510187, 
                    -0.4469512513374059, 
                    -0.36175748519498185, 
                    0.42434844978344044, 
                    0.1375776531239009, 
                    0.41404751672014517, 
                    0.7203389431549898, 
                    0.09287918219344149, 
                    -0.20308074260628278, 
                    0.6111696924616976, 
                    -0.5170686297679112, 
                    -0.4953070335029668, 
                    0.24967399312451777, 
                    -0.0807043168717203, 
                    0.04282587956048711, 
                    -0.29740211184916493, 
                    -0.3289472075793309, 
                    -0.5292955341900325, 
                    -0.20985060823128754, 
                    0.20041977046497783, 
                    0.6277055636985127, 
                    0.02958507882677519, 
                    -0.15365467527702592, 
                    -0.508880333529235, 
                    -0.14267915505721984, 
                    -0.45130625482529324, 
                    -0.22791159100695596, 
                    0.30178504469287826, 
                    0.019040524516956592, 
                    0.026268249307870195, 
                    -0.2519647905467108, 
                    -0.03260895401829078, 
                    -0.12052201479193547, 
                    0.22951076134495296, 
                    -0.4253286959672943, 
                    0.5318273152093373, 
                    -0.4273266317825186, 
                    0.5292586889470849, 
                    -0.3884644839085569, 
                    0.5001202281570591, 
                    0.09685016475144603, 
                    0.1016205717899431, 
                    -0.41215791067615504, 
                    0.23731131696673713, 
                    0.5098489237554, 
                    0.22792397049251667, 
                    0.7091031459740995, 
                    0.21310350575701298, 
                    -0.4607239278403789, 
                    0.6825755750872187, 
                    -0.1844847565201857, 
                    -0.21433022928597406, 
                    -0.3703087102552322, 
                    -0.20183511491877043, 
                    -0.5608725081307574, 
                    0.5006557352846195, 
                    -0.5955730700414903, 
                    0.6738734875783936, 
                    0.58348917268217, 
                    -0.011585742693320467, 
                    -0.10386172477550726, 
                    -0.47494023051595413, 
                    0.11404291649325649, 
                    -0.04273942875136405, 
                    -0.10843335846125601, 
                    0.6325027694129564, 
                    0.5985731547519992, 
                    -0.3892743394435564, 
                    0.17334818093502868, 
                    0.12624235478278711, 
                    0.1768900858894269, 
                    0.12469813044103761, 
                    -0.49165030911552504, 
                    -0.38233944418801513, 
                    -0.18146776597060493, 
                    0.0505692016977658, 
                    0.5578267375729349, 
                    0.15493374855316988, 
                    -0.5108898952389267, 
                    0.03405567250043817, 
                    -0.4129004960049982, 
                    -0.11746649710921475, 
                    0.28268671288962555, 
                    0.03860282582414298, 
                    -0.21300668002831524, 
                    0.16622673202363558, 
                    -0.3119104351667556, 
                    -0.3150039533755274, 
                    -0.5484952882000924, 
                    -0.030488663362917112, 
                    -0.05389510580943391, 
                    0.39194277859901083, 
                    -0.08644956662024184, 
                    -0.039197687973623774, 
                    -0.3863004096221484, 
                    -0.12549374203210784, 
                    -0.2801508517932164, 
                    0.7621018087087805, 
                    -0.048120238451001995, 
                    0.5580471578705051, 
                    -0.06051371962193508, 
                    -0.4686447704336968, 
                    0.3918770528569464, 
                    -0.20965353239586398, 
                    -0.2401397065285823, 
                    -0.29761846676932957, 
                    -0.5169825803032864, 
                    0.25075242526581865, 
                    -0.006795886189016609, 
                    0.5515891339834713, 
                    -0.33216926373936606, 
                    -0.38507099023307867, 
                    0.3476443007018225, 
                    -0.2965538871714371, 
                    0.26037790502890645, 
                    -0.3457029497148227, 
                    -0.3126928487674355, 
                    -0.28304980219336434, 
                    0.13716348549523405, 
                    -0.18746359369313176, 
                    0.25903468281572273, 
                    0.6181051464779226, 
                    0.12301671570491712, 
                    -0.2680429454829765, 
                    0.3821268670188058, 
                    -0.13636356207615097, 
                    0.5054852433035699, 
                    -0.2332918339491868, 
                    -0.370924110796375, 
                    0.03425618444927381, 
                    0.11521978309275038, 
                    0.4297289891026266, 
                    0.5762749266413467, 
                    0.2687584648221799, 
                    -0.5129993006307512, 
                    0.5330525988337211, 
                    -0.3573269939417775, 
                    -0.6037459687261731, 
                    0.35957175102805494, 
                    0.5586383725745394, 
                    -0.04136982762615182, 
                    0.46483709596279055, 
                    0.24960264432034263, 
                    0.05479603298166902, 
                    0.3579598108399701, 
                    -0.5372661397044601, 
                    -0.5198273739154726, 
                    -0.06993860595573065, 
                    -0.5017211454155626, 
                    -0.35752238399022856, 
                    0.23158419727266977, 
                    0.43581585803339307, 
                    0.03820008730595115, 
                    0.18795522106428553, 
                    0.3777133064634459, 
                    0.6976945635784599, 
                    -0.42742363090595853, 
                    -0.1544668560869319, 
                    -0.016429526260556493, 
                    0.13084329500207692, 
                    0.65190288593696, 
                    -0.31593389766513896, 
                    0.37123147975501714, 
                    0.07225731737966334, 
                    0.18314333905421687, 
                    -0.06865781356824208, 
                    0.09017682677666217, 
                    0.45004824888758477, 
                    0.14984818133700128, 
                    0.16234000434761897, 
                    -0.3053424629291408, 
                    -0.6016716991006353, 
                    0.48835862530263907, 
                    -0.1974018138035047, 
                    -0.34283145489505984, 
                    -0.25116056568534867, 
                    -0.009383183780973692, 
                    0.15223589100859003, 
                    -0.3643830183580478, 
                    0.5606405698858283, 
                    0.4351619738459809, 
                    0.5875781723508555, 
                    0.5196830715484886, 
                    -0.43604143115600114, 
                    0.3815641976086923, 
                    -0.033248635391759485, 
                    -0.3204847194123604, 
                    0.6298877548650756, 
                    -0.5637227942092199, 
                    -0.5742592986704935, 
                    -0.32326905651441407, 
                    -0.3904063472706399, 
                    0.36910407006241863, 
                    0.15642212817485268, 
                    0.15842810122924322, 
                    -0.3850201231327142, 
                    -0.16711860257407662, 
                    -0.3822932721592957, 
                    -0.08543632362143905, 
                    0.34788967974776386, 
                    -0.5455456597225657, 
                    -0.5110845230246868, 
                    -0.11718027245939688, 
                    0.08434926815066868, 
                    0.5854141596666886, 
                    0.1952650403824897, 
                    0.5227004456961244, 
                    0.09310014687222556, 
                    -0.5796423273594155, 
                    -0.33433166376708656, 
                    0.5441426003889271, 
                    -0.451401832347998, 
                    -0.12535174263473536, 
                    -0.38831072228207797, 
                    -0.18049828533817958, 
                    -0.4125624344180797, 
                    0.26605655738679723, 
                    -0.32041577533891763, 
                    0.2644223377118817, 
                    0.47189056248276895, 
                    -0.07801574509464337, 
                    -0.06834292360498861, 
                    -0.036500082927818145, 
                    -0.3849861612216559, 
                    0.08841385115557021, 
                    -0.520153923877259, 
                    0.39652789067228444, 
                    -0.6006120920918628, 
                    0.34716875160578275, 
                    -0.10344712009416146, 
                    -0.32729041461034225, 
                    0.1466430037757641, 
                    -0.22113972248033098, 
                    -0.13581500588718887, 
                    -0.16934049265568235, 
                    -0.31906417975481166, 
                    0.23235291967969252, 
                    -0.5479436296000231, 
                    -0.5569420583118944, 
                    0.09575613398983929, 
                    -0.12782123067709195, 
                    -0.20362107727840623, 
                    0.34393588475299897, 
                    -0.5034780374082284, 
                    -0.45184667662873546, 
                    0.34009375457078717, 
                    -0.5743376528664644, 
                    -0.517704373385741, 
                    0.30587070244980197, 
                    -0.576306948196486, 
                    0.6159648178641324, 
                    -0.10383625808198127, 
                    -0.10085342737270431, 
                    -0.08062264951597586, 
                    -0.40980118287134326, 
                    -0.06372056288824335, 
                    -0.04989525415644469, 
                    -0.40495906966601003, 
                    0.028583635233407345, 
                    0.02534177048258557, 
                    -0.6008820813834292, 
                    -0.3326368998538009, 
                    -0.3380208833443009, 
                    -0.19456676973608905, 
                    -0.40000868379942606, 
                    0.5091779206852913, 
                    -0.4689069678623342, 
                    -0.2657454983946424, 
                    0.6771947059804144, 
                    -0.4047783012815689, 
                    -0.28117289352827646, 
                    0.3871735744815845, 
                    -0.22408583187703918, 
                    0.4995552889148005, 
                    -0.13452079416916374, 
                    0.5743931837073479, 
                    -0.23316629428963292, 
                    -0.18006047183679613, 
                    -0.23476002998169615, 
                    0.5445170124243888, 
                    -0.29439592679765647, 
                    0.011520821395422876, 
                    0.19352692566145357, 
                    -0.12262759139377344, 
                    0.19617039890968258, 
                    0.3586483166369936, 
                    -0.20127316621689778, 
                    0.04111744608352008, 
                    -0.27673524390341947, 
                    -0.3458354799705558, 
                    0.4069933048905474, 
                    0.2242169907304904, 
                    0.25288042990205717, 
                    -0.5197119691288525, 
                    0.23443444790472667, 
                    -0.5414432616059986, 
                    0.3759592266217753 
)
)

