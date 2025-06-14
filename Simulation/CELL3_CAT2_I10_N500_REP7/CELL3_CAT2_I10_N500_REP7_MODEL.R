model <- " model {

# specify prior distributions

for (j in 1:J) {

b[j] ~ dnorm(0,0.25);

a[j] ~ dnorm(0,4);

tau[j,1] <- 0;

}

tau[1,2] ~ dlnorm(-0.179159, 1);
tau[2,2] ~ dlnorm(-0.386877, 1);
tau[3,2] ~ dlnorm(-0.159378, 1);
tau[4,2] ~ dlnorm(-0.303824, 1);
tau[5,2] ~ dlnorm(-0.50401, 1);
tau[6,2] ~ dlnorm(-0.542259, 1);
tau[7,2] ~ dlnorm(-0.307659, 1);
tau[8,2] ~ dlnorm(-0.389359, 1);
tau[9,2] ~ dlnorm(-0.26341, 1);
tau[10,2] ~ dlnorm(-0.07493, 1);

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

K <- 2 

data_list <- list("r" = resp, "N" = N, "J" = J, "K" = K)

# specify starting values for all estimated parameters

starts <-  list(a=
c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 
),
b=c( -1.2523098050539612, 
                    -0.6765462173525315, 
                    -1.3136327163995216, 
                    -0.8925191472364047, 
                    -0.4008866727723522, 
                    -0.31764295586574287, 
                    0.8821468766414491, 
                    0.6703642161986489, 
                    1.0042805659172318, 
                    1.5895252101643602 ),
tau=matrix(c( NA,1.3782871024692285, 
                    NA,1.1197692515912867, 
                    NA,1.4058210896633851, 
                    NA,1.216741097109146, 
                    NA,0.9959981160747862, 
                    NA,0.9586216871837185, 
                    NA,1.2120839476120107, 
                    NA,1.1169935330731935, 
                    NA,1.2669219740968372, 
                    NA,1.5296968193637979 ), ncol= 2 , nrow= 10 , byrow=TRUE),
theta = c(
 0.014907639820250396, 
                    0.24564973022591902, 
                    0.07540275895736076, 
                    -0.2806773205838623, 
                    0.24564973022591902, 
                    -0.06462590402004492, 
                    -0.0075792762933468705, 
                    0.20809688448647484, 
                    -0.2121976702839345, 
                    -0.1513223918778785, 
                    0.20809688448647484, 
                    -0.011411958252444454, 
                    -0.2762158556213884, 
                    -0.2762158556213884, 
                    -0.05963456281940904, 
                    -0.152448856438144, 
                    -0.02163130269132968, 
                    0.24564973022591902, 
                    0.20809688448647484, 
                    0.1457857612072604, 
                    0.24564973022591902, 
                    0.11298878443110011, 
                    0.24564973022591902, 
                    0.24564973022591902, 
                    0.1034774528732968, 
                    -0.11744987262180032, 
                    0.11682146639019769, 
                    0.20254730335936771, 
                    -0.193347667365909, 
                    0.20809688448647484, 
                    -0.24986717481276774, 
                    0.07540275895736076, 
                    -0.07072206457588232, 
                    -0.08440594924076894, 
                    0.014907639820250396, 
                    0.17003541215779822, 
                    0.20809688448647484, 
                    0.1341056501971319, 
                    0.20809688448647484, 
                    -0.3458734665572315, 
                    -0.1959824029556887, 
                    -0.08440594924076894, 
                    0.009054344174512785, 
                    -0.04997979369682021, 
                    0.24564973022591902, 
                    0.17003541215779822, 
                    -0.24986717481276774, 
                    0.24564973022591902, 
                    0.17003541215779822, 
                    0.08636804643945944, 
                    -0.0873679387355506, 
                    0.07540275895736076, 
                    0.24564973022591902, 
                    0.1457857612072604, 
                    0.24564973022591902, 
                    -0.12930112690719311, 
                    0.24564973022591902, 
                    0.02857859616049141, 
                    0.009054344174512785, 
                    -0.1959824029556887, 
                    0.03279918759273748, 
                    0.20809688448647484, 
                    0.031985424568578114, 
                    0.24564973022591902, 
                    -0.06927426227669836, 
                    -0.1959824029556887, 
                    -0.12930112690719311, 
                    -0.21967305568044299, 
                    -0.08440594924076894, 
                    0.20809688448647484, 
                    0.07540275895736076, 
                    -0.08440594924076894, 
                    0.17003541215779822, 
                    0.008590226281738222, 
                    -0.25504137333480603, 
                    -0.005305714479901302, 
                    0.07540275895736076, 
                    -0.08440594924076894, 
                    -0.2121976702839345, 
                    0.20809688448647484, 
                    0.02857859616049141, 
                    -0.2501290666528348, 
                    -0.04290830930116285, 
                    -0.08440594924076894, 
                    0.12023649277483761, 
                    -0.18879806829240733, 
                    -0.28722989035902513, 
                    0.20254730335936771, 
                    0.14227469330757742, 
                    0.1341056501971319, 
                    0.20254730335936771, 
                    0.014907639820250396, 
                    -0.24249571107309836, 
                    -0.27143044733859384, 
                    0.19668841127087827, 
                    0.20809688448647484, 
                    0.20809688448647484, 
                    0.08636804643945944, 
                    -0.16363597474377953, 
                    -0.011411958252444454, 
                    -0.26336548293050976, 
                    -0.0075792762933468705, 
                    -0.3038237988126798, 
                    -0.3439914325111323, 
                    0.17003541215779822, 
                    -0.11744987262180032, 
                    -0.12930112690719311, 
                    -0.23979920828212875, 
                    0.02857859616049141, 
                    -0.18879806829240733, 
                    -0.16363597474377953, 
                    -0.18879806829240733, 
                    0.24564973022591902, 
                    -0.22142190531011685, 
                    0.08636804643945944, 
                    -0.3495410136382396, 
                    -0.12930112690719311, 
                    -0.18290366064727456, 
                    0.07540275895736076, 
                    -0.26336548293050976, 
                    0.08276081636030874, 
                    -0.0075792762933468705, 
                    -0.18879806829240733, 
                    -0.3038237988126798, 
                    0.20809688448647484, 
                    0.07540275895736076, 
                    0.24564973022591902, 
                    0.08636804643945944, 
                    0.07540275895736076, 
                    -0.08440594924076894, 
                    0.10753220278166864, 
                    0.24564973022591902, 
                    -0.04213898790644771, 
                    -0.3038237988126798, 
                    -0.20730306128059686, 
                    0.24564973022591902, 
                    -0.08440594924076894, 
                    -0.08440594924076894, 
                    -0.26336548293050976, 
                    -0.06982863632670805, 
                    -0.18879806829240733, 
                    0.11682146639019769, 
                    -0.12930112690719311, 
                    0.014907639820250396, 
                    0.24564973022591902, 
                    0.20809688448647484, 
                    0.20254730335936771, 
                    -0.10930762667008145, 
                    0.07540275895736076, 
                    -0.16077657614878266, 
                    0.10942343265234022, 
                    0.0439839206732377, 
                    0.08636804643945944, 
                    -0.12930112690719311, 
                    0.20809688448647484, 
                    0.042627260659757193, 
                    0.013091635678146196, 
                    -0.00562330832723873, 
                    0.24564973022591902, 
                    -0.152448856438144, 
                    0.17003541215779822, 
                    -0.05963456281940904, 
                    0.014553022715549424, 
                    -0.3038237988126798, 
                    -0.21589518768463645, 
                    0.20254730335936771, 
                    -0.22568036425260873, 
                    0.042627260659757193, 
                    0.19668841127087827, 
                    -5.995449954709908e-4, 
                    -0.06324179289855975, 
                    0.20809688448647484, 
                    0.11161873408353457, 
                    -0.08440594924076894, 
                    -0.1959824029556887, 
                    0.24564973022591902, 
                    -0.032420746806129774, 
                    0.1457857612072604, 
                    0.20809688448647484, 
                    -0.21967305568044299, 
                    0.20254730335936771, 
                    -0.14340684737515938, 
                    -0.22720920184854304, 
                    -0.027469586541118085, 
                    0.20809688448647484, 
                    -0.011411958252444454, 
                    0.07540275895736076, 
                    0.20809688448647484, 
                    -0.22142190531011685, 
                    0.19668841127087827, 
                    -0.2762158556213884, 
                    -0.08440594924076894, 
                    -0.3038237988126798, 
                    0.1034774528732968, 
                    -0.011411958252444454, 
                    0.20254730335936771, 
                    -0.22142190531011685, 
                    -0.08440594924076894, 
                    0.1341056501971319, 
                    0.14227469330757742, 
                    0.1841700003873557, 
                    -0.06324179289855975, 
                    0.12023649277483761, 
                    0.20809688448647484, 
                    -0.17634026473694853, 
                    -0.0075792762933468705, 
                    -0.06324179289855975, 
                    0.08276081636030874, 
                    -0.3256141295391204, 
                    0.20254730335936771, 
                    -0.18879806829240733, 
                    0.10753220278166864, 
                    -0.12232858474481029, 
                    -0.17609377829923836, 
                    0.07540275895736076, 
                    -0.11332550656218374, 
                    0.07540275895736076, 
                    -0.07503659910738542, 
                    0.07540275895736076, 
                    -0.05053750290539066, 
                    -0.08440594924076894, 
                    0.08636804643945944, 
                    0.1457857612072604, 
                    0.24564973022591902, 
                    0.17003541215779822, 
                    0.042627260659757193, 
                    0.1697718050617641, 
                    0.08276081636030874, 
                    -0.199160815328878, 
                    -0.2876702320141581, 
                    -0.06324179289855975, 
                    0.11682146639019769, 
                    -0.1959824029556887, 
                    0.19668841127087827, 
                    -0.24986717481276774, 
                    -0.14924019698979246, 
                    -0.152448856438144, 
                    -0.06462590402004492, 
                    -0.04734172021311084, 
                    0.17003541215779822, 
                    -0.0075792762933468705, 
                    -0.0075792762933468705, 
                    0.17003541215779822, 
                    0.20809688448647484, 
                    -0.0075792762933468705, 
                    -0.3256141295391204, 
                    0.24564973022591902, 
                    -0.1184959027857127, 
                    0.07540275895736076, 
                    0.08276081636030874, 
                    -0.28059420446677097, 
                    0.11682146639019769, 
                    -0.042795459033538874, 
                    0.20254730335936771, 
                    -0.1488078061169937, 
                    0.24564973022591902, 
                    -0.1581035000885267, 
                    -0.0075792762933468705, 
                    -0.011411958252444454, 
                    -0.2502258669862724, 
                    -0.1959824029556887, 
                    0.20254730335936771, 
                    0.02857859616049141, 
                    -0.05963456281940904, 
                    0.24564973022591902, 
                    0.07015277911121359, 
                    0.17003541215779822, 
                    0.24564973022591902, 
                    0.014907639820250396, 
                    -0.152448856438144, 
                    -0.3495410136382396, 
                    0.20809688448647484, 
                    0.1034774528732968, 
                    0.24564973022591902, 
                    -0.28059420446677097, 
                    0.19668841127087827, 
                    -0.038836582850795554, 
                    0.07540275895736076, 
                    -0.06324179289855975, 
                    0.24564973022591902, 
                    0.08636804643945944, 
                    0.20809688448647484, 
                    -0.12930112690719311, 
                    -0.12930112690719311, 
                    0.008590226281738222, 
                    0.14227469330757742, 
                    -0.31121593421352883, 
                    -0.15492962195702917, 
                    -0.06324179289855975, 
                    0.008590226281738222, 
                    0.17003541215779822, 
                    0.07540275895736076, 
                    -0.08440594924076894, 
                    0.20254730335936771, 
                    -0.1959824029556887, 
                    -0.10525948650803771, 
                    0.08276081636030874, 
                    0.20809688448647484, 
                    0.20809688448647484, 
                    0.08636804643945944, 
                    0.20809688448647484, 
                    -0.17554253051241075, 
                    0.24564973022591902, 
                    -0.05963456281940904, 
                    -0.08440594924076894, 
                    0.17003541215779822, 
                    0.1697718050617641, 
                    -0.07072206457588232, 
                    0.24564973022591902, 
                    0.0251658320130011, 
                    0.11682146639019769, 
                    0.11682146639019769, 
                    -0.12930112690719311, 
                    0.009054344174512785, 
                    -0.23979920828212875, 
                    0.20809688448647484, 
                    -0.0075792762933468705, 
                    -0.3256141295391204, 
                    -0.0075792762933468705, 
                    -0.16777632775850312, 
                    0.08636804643945944, 
                    -0.12930112690719311, 
                    -0.1959824029556887, 
                    0.20809688448647484, 
                    -0.038836582850795554, 
                    0.19668841127087827, 
                    0.11161873408353457, 
                    -0.06462590402004492, 
                    -0.05925721279949486, 
                    -0.12930112690719311, 
                    -0.08440594924076894, 
                    -0.20267780488266146, 
                    -0.26492716491602264, 
                    0.11682146639019769, 
                    0.05911878566543233, 
                    -0.08440594924076894, 
                    -0.08440594924076894, 
                    0.24564973022591902, 
                    -0.0075792762933468705, 
                    0.1225613145255548, 
                    0.19668841127087827, 
                    0.20809688448647484, 
                    0.17003541215779822, 
                    -0.1419141091929998, 
                    0.07540275895736076, 
                    -0.25419740360772025, 
                    -0.32023751930817523, 
                    -0.12930112690719311, 
                    0.19668841127087827, 
                    0.20809688448647484, 
                    -0.05963456281940904, 
                    0.17003541215779822, 
                    -0.26336548293050976, 
                    0.07540275895736076, 
                    -0.028815637354611012, 
                    0.17003541215779822, 
                    -0.011411958252444454, 
                    -0.08440594924076894, 
                    0.11161873408353457, 
                    0.11682146639019769, 
                    -0.2680280937954179, 
                    -0.1273011503913094, 
                    0.19668841127087827, 
                    0.15721542752689738, 
                    -0.038836582850795554, 
                    0.11298878443110011, 
                    0.17003541215779822, 
                    0.08636804643945944, 
                    0.19668841127087827, 
                    -0.18879806829240733, 
                    0.014907639820250396, 
                    -0.30481088145932717, 
                    -0.0075792762933468705, 
                    0.014907639820250396, 
                    0.24564973022591902, 
                    -0.14481550085840755, 
                    -0.12612925836544914, 
                    -0.26336548293050976, 
                    0.009003441588442074, 
                    -0.19255553206519432, 
                    -0.19670142696542806, 
                    0.24564973022591902, 
                    0.02857859616049141, 
                    0.17003541215779822, 
                    -0.20860299334852858, 
                    0.24564973022591902, 
                    0.20809688448647484, 
                    0.10942343265234022, 
                    0.20254730335936771, 
                    -0.0489837386763754, 
                    -0.3038237988126798, 
                    0.028998513972098683, 
                    -0.2107217685228032, 
                    0.24564973022591902, 
                    -0.011411958252444454, 
                    0.07540275895736076, 
                    0.07540275895736076, 
                    0.20809688448647484, 
                    -0.011411958252444454, 
                    0.07540275895736076, 
                    -0.2876702320141581, 
                    -0.26336548293050976, 
                    0.24564973022591902, 
                    -0.3475473850274585, 
                    -0.3475473850274585, 
                    -0.2851209139893144, 
                    0.20809688448647484, 
                    -0.22738025869917047, 
                    0.17003541215779822, 
                    0.24564973022591902, 
                    -0.13157908458073256, 
                    -0.08440594924076894, 
                    -0.07503659910738542, 
                    0.008590226281738222, 
                    -0.25419740360772025, 
                    -0.011411958252444454, 
                    0.20809688448647484, 
                    0.20809688448647484, 
                    -0.06689435315566661, 
                    -0.07503659910738542, 
                    -0.16967757797440514, 
                    0.07540275895736076, 
                    -0.011411958252444454, 
                    0.08636804643945944, 
                    -0.3038237988126798, 
                    -0.26336548293050976, 
                    0.20254730335936771, 
                    -0.07503659910738542, 
                    0.24564973022591902, 
                    -0.011411958252444454, 
                    -0.025617490481526117, 
                    0.17003541215779822, 
                    -0.09390743251569361, 
                    -0.3038237988126798, 
                    -0.2501290666528348, 
                    -0.30481088145932717, 
                    0.11298878443110011, 
                    -0.06462590402004492, 
                    -0.08440594924076894, 
                    0.1457857612072604, 
                    -0.13157908458073256, 
                    0.14227469330757742, 
                    -0.12930112690719311, 
                    0.08276081636030874, 
                    0.07540275895736076, 
                    -0.193347667365909, 
                    0.1697718050617641, 
                    -0.1959824029556887, 
                    0.17003541215779822, 
                    0.24564973022591902, 
                    -0.14481550085840755, 
                    0.17003541215779822, 
                    0.014907639820250396, 
                    0.24564973022591902, 
                    0.11161873408353457, 
                    -0.011411958252444454, 
                    -0.12996194032454672, 
                    0.17003541215779822, 
                    -0.13157908458073256, 
                    0.10753220278166864, 
                    -0.08440594924076894, 
                    0.24564973022591902, 
                    0.17003541215779822, 
                    0.24564973022591902, 
                    0.20254730335936771, 
                    0.008590226281738222, 
                    0.20254730335936771, 
                    0.20809688448647484, 
                    0.05744848911804462, 
                    0.17003541215779822, 
                    0.11298878443110011, 
                    -0.05963456281940904, 
                    -0.043509038254013255, 
                    0.08423623510084405, 
                    0.08020950585504882, 
                    0.018641216805253547, 
                    -0.2501290666528348, 
                    -0.0075792762933468705, 
                    0.031985424568578114, 
                    -0.0075792762933468705, 
                    0.11298878443110011, 
                    0.17003541215779822, 
                    0.014553022715549424, 
                    -0.12996194032454672, 
                    -0.011411958252444454, 
                    0.051485430085695894, 
                    -0.0075792762933468705, 
                    -0.26336548293050976, 
                    -0.1832781129625197, 
                    -0.08440594924076894, 
                    0.20809688448647484, 
                    0.008590226281738222, 
                    0.07540275895736076, 
                    0.24564973022591902, 
                    -0.02163130269132968, 
                    -0.15120865015128207, 
                    0.1034774528732968, 
                    0.06171157828374352, 
                    -0.2235475767400729, 
                    -0.08440594924076894 
)
)

