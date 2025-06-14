model <- " model {

# specify prior distributions

for (j in 1:J) {

b[j] ~ dnorm(0,0.25);

a[j] ~ dnorm(0,4);

tau[j,1] <- 0;

}

tau[1,2] ~ dlnorm(-0.293268, 1);
tau[2,2] ~ dlnorm(-0.149128, 1);
tau[3,2] ~ dlnorm(-0.265145, 1);
tau[4,2] ~ dlnorm(-0.117307, 1);
tau[5,2] ~ dlnorm(-0.592323, 1);
tau[6,2] ~ dlnorm(-0.613464, 1);
tau[7,2] ~ dlnorm(-0.255337, 1);
tau[8,2] ~ dlnorm(-0.309202, 1);
tau[9,2] ~ dlnorm(-0.227101, 1);
tau[10,2] ~ dlnorm(-0.359978, 1);

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
b=c( 0.9212751632272207, 
                    1.345891711021057, 
                    0.9993870112344365, 
                    1.4481683282672204, 
                    -0.2133876225188294, 
                    0.17090460549776731, 
                    -1.027152432163492, 
                    -0.8779837381693065, 
                    -1.108612298243566, 
                    -0.7445402148010822 ),
tau=matrix(c( NA,1.2296525482890222, 
                    NA,1.4203053782484547, 
                    NA,1.2647247680442621, 
                    NA,1.466227579391982, 
                    NA,0.9118110425109545, 
                    NA,0.8927361678684975, 
                    NA,1.2771914420414079, 
                    NA,1.2102146984380187, 
                    NA,1.3137669219113612, 
                    NA,1.150298556445686 ), ncol= 2 , nrow= 10 , byrow=TRUE),
theta = c(
 0.20369733756956993, 
                    -0.2038483861696567, 
                    0.09593582717740978, 
                    -0.14504095431295422, 
                    -0.1005973415173519, 
                    -0.18042632419044263, 
                    0.0503458521486968, 
                    -0.18042632419044263, 
                    0.11195767565106646, 
                    -0.03211106524140839, 
                    -0.18042632419044263, 
                    -0.20316701214471575, 
                    -0.18042632419044263, 
                    -0.11102553975368507, 
                    0.40204805193581156, 
                    -0.02701982361140376, 
                    0.034581563801062726, 
                    -0.024403658196968103, 
                    -0.020627535033518496, 
                    -0.20316701214471575, 
                    0.11195767565106646, 
                    -0.12864617383190086, 
                    0.22976758704184697, 
                    -0.2038483861696567, 
                    -0.040214785552015214, 
                    -0.2038483861696567, 
                    0.10761812853955444, 
                    0.047717150709243705, 
                    -0.18042632419044263, 
                    -0.20316701214471575, 
                    0.020814869134574837, 
                    -0.07383347671445958, 
                    -0.020025961422913602, 
                    -0.02701982361140376, 
                    0.25729523455024506, 
                    0.1053552897421931, 
                    -0.02701982361140376, 
                    -0.2038483861696567, 
                    -0.18042632419044263, 
                    0.2222688905380945, 
                    -0.11854725925356777, 
                    0.14797043699074647, 
                    0.11002446033036523, 
                    -0.11854725925356777, 
                    -0.09064291035447211, 
                    0.16215246372606773, 
                    0.14797043699074647, 
                    -0.13922239245009174, 
                    -0.1239779169922394, 
                    -0.1239779169922394, 
                    0.2848029114870128, 
                    0.2618233954013316, 
                    -0.2038483861696567, 
                    -0.18042632419044263, 
                    -0.02701982361140376, 
                    0.24502339575324977, 
                    -0.18042632419044263, 
                    -0.0846266387517566, 
                    -0.13922239245009174, 
                    -0.11854725925356777, 
                    0.039655683625402144, 
                    -0.18042632419044263, 
                    -0.2038483861696567, 
                    -0.030085692488421634, 
                    -0.0051533970977113985, 
                    0.15364492885825853, 
                    -0.18042632419044263, 
                    -0.2038483861696567, 
                    0.054964193200275735, 
                    -0.0051533970977113985, 
                    0.34287073724033507, 
                    -0.2038483861696567, 
                    0.31542129238200683, 
                    0.034581563801062726, 
                    -0.2038483861696567, 
                    -0.09989643060033092, 
                    0.23668317554273582, 
                    0.2101446413056211, 
                    -0.2038483861696567, 
                    -0.20316701214471575, 
                    0.05975595002851536, 
                    0.11195767565106646, 
                    -0.05364688976000179, 
                    -0.002285444184533164, 
                    0.24502339575324977, 
                    0.16273593984418094, 
                    0.24502339575324977, 
                    0.08836791659370141, 
                    -0.10698963009523711, 
                    -0.1005973415173519, 
                    0.1276877239155158, 
                    -0.1005973415173519, 
                    -0.02571877666352318, 
                    0.2826865332706162, 
                    0.17896415258133475, 
                    0.17589167356367674, 
                    -0.11854725925356777, 
                    -0.11854725925356777, 
                    -0.10698963009523711, 
                    0.24106841354573383, 
                    -0.11854725925356777, 
                    -0.18042632419044263, 
                    0.20565824072691685, 
                    -0.11854725925356777, 
                    0.31099430312353854, 
                    0.052809159061687205, 
                    0.03937332062930238, 
                    0.015384211395903208, 
                    0.10849020918949587, 
                    0.10603118017885199, 
                    0.2650931156333086, 
                    -0.03211106524140839, 
                    -0.002285444184533164, 
                    0.22418816932704583, 
                    -0.11854725925356777, 
                    -0.045645443290686816, 
                    -0.11854725925356777, 
                    0.20348613852509495, 
                    0.020814869134574837, 
                    -0.059047786634913585, 
                    -0.12864617383190086, 
                    0.2173869000376435, 
                    -0.18042632419044263, 
                    -0.09064291035447211, 
                    0.03937332062930238, 
                    -0.03211106524140839, 
                    0.12480659847669912, 
                    0.3067680196625246, 
                    -0.09064291035447211, 
                    -0.06331509887112322, 
                    -0.18042632419044263, 
                    0.01540093623184946, 
                    -0.04654497335412272, 
                    -0.06331509887112322, 
                    0.09031584822633026, 
                    0.2983780890874977, 
                    -0.2038483861696567, 
                    -0.18042632419044263, 
                    -0.06874575660979482, 
                    0.361019006972169, 
                    0.31542129238200683, 
                    -0.11102553975368507, 
                    -0.2038483861696567, 
                    0.09232024231672098, 
                    0.2650931156333086, 
                    -0.20316701214471575, 
                    -0.053767608978072146, 
                    0.04771791743168258, 
                    -0.2038483861696567, 
                    -0.030085692488421634, 
                    -0.11854725925356777, 
                    0.2650931156333086, 
                    -0.20316701214471575, 
                    0.11195767565106646, 
                    -0.11102553975368507, 
                    0.36210750491339544, 
                    -0.2038483861696567, 
                    0.026931326581088794, 
                    -0.2038483861696567, 
                    -0.11854725925356777, 
                    0.11327343891087466, 
                    0.16559470893419337, 
                    -0.04867636734806752, 
                    0.07688438638581158, 
                    -0.10698963009523711, 
                    -0.2038483861696567, 
                    -0.09064291035447211, 
                    0.07173618168271972, 
                    -0.1397835182971176, 
                    0.14653705145905777, 
                    -0.024403658196968103, 
                    0.3560989012205326, 
                    0.039655683625402144, 
                    0.0333570490265869, 
                    0.08836791659370141, 
                    -0.18042632419044263, 
                    0.009763637040714007, 
                    -0.13351737475249648, 
                    0.0333570490265869, 
                    0.030280760634518872, 
                    0.11195767565106646, 
                    0.3560989012205326, 
                    -0.18042632419044263, 
                    0.2650931156333086, 
                    -0.05364688976000179, 
                    -0.11854725925356777, 
                    -0.2038483861696567, 
                    -0.09064291035447211, 
                    -0.11854725925356777, 
                    -0.05364688976000179, 
                    0.2717238814116038, 
                    -0.04867636734806752, 
                    -0.20316701214471575, 
                    -0.010291083528369577, 
                    -0.010291083528369577, 
                    -0.02701982361140376, 
                    -0.2038483861696567, 
                    -0.053558357848518534, 
                    0.026061373695018708, 
                    0.2688171484708366, 
                    0.11195767565106646, 
                    -0.13922239245009174, 
                    -0.10698963009523711, 
                    -0.059047786634913585, 
                    0.2848029114870128, 
                    0.04647412477514634, 
                    -0.0846266387517566, 
                    -0.04867636734806752, 
                    -0.05864959947852316, 
                    -0.030085692488421634, 
                    -0.1397835182971176, 
                    -0.10698963009523711, 
                    0.028565292198347247, 
                    0.36210750491339544, 
                    -0.13922239245009174, 
                    0.025368911351673434, 
                    0.2618233954013316, 
                    -0.02701982361140376, 
                    0.22736013216809908, 
                    0.09963889160096678, 
                    -0.2038483861696567, 
                    -0.05364688976000179, 
                    0.24475719190313427, 
                    0.29192155561134076, 
                    -0.09064291035447211, 
                    -0.2038483861696567, 
                    0.1350508430061882, 
                    -0.11102553975368507, 
                    -0.14504095431295422, 
                    0.11195767565106646, 
                    0.32100071009680803, 
                    -0.18042632419044263, 
                    -0.2038483861696567, 
                    0.095669676370151, 
                    -0.07395321314360925, 
                    -0.2038483861696567, 
                    -0.11854725925356777, 
                    -0.16152291963268756, 
                    0.2311238339461213, 
                    0.1276877239155158, 
                    0.2650931156333086, 
                    0.361019006972169, 
                    -0.059047786634913585, 
                    -0.20316701214471575, 
                    0.27695329246535266, 
                    0.039655683625402144, 
                    0.32944469354463884, 
                    0.1049332187003606, 
                    -0.18042632419044263, 
                    0.24502339575324977, 
                    0.020814869134574837, 
                    -0.07826683330776849, 
                    -0.03211106524140839, 
                    0.12480659847669912, 
                    -0.10698963009523711, 
                    0.17056609287318625, 
                    0.17896415258133475, 
                    -0.11854725925356777, 
                    -0.13922239245009174, 
                    0.17906093492498476, 
                    0.10849020918949587, 
                    0.1794678742740578, 
                    0.047717150709243705, 
                    0.174369337992675, 
                    0.11195767565106646, 
                    0.20045078419310247, 
                    0.17014483089942106, 
                    -0.11854725925356777, 
                    -0.0846266387517566, 
                    -0.18042632419044263, 
                    0.32100071009680803, 
                    -0.20316701214471575, 
                    -0.09265948303847707, 
                    -0.2038483861696567, 
                    0.26668789541350674, 
                    0.1074321275355069, 
                    -0.13351737475249648, 
                    0.22569317620744972, 
                    -0.16522754126741201, 
                    -0.2038483861696567, 
                    -0.0846266387517566, 
                    0.05975595002851536, 
                    -0.2038483861696567, 
                    -0.0051533970977113985, 
                    -0.18042632419044263, 
                    0.20544020221568032, 
                    -0.040214785552015214, 
                    0.09963889160096678, 
                    -0.20316701214471575, 
                    0.18742901329220163, 
                    0.019625853920648556, 
                    0.11002446033036523, 
                    -0.2038483861696567, 
                    -0.13922239245009174, 
                    0.2370730597022872, 
                    -0.15569586102682453, 
                    0.016555370306294087, 
                    -0.020627535033518496, 
                    -0.020627535033518496, 
                    -0.11854725925356777, 
                    -0.04338204024867376, 
                    -0.1239779169922394, 
                    -0.2038483861696567, 
                    -0.040487194179435776, 
                    -0.040214785552015214, 
                    -0.13922239245009174, 
                    0.17896415258133475, 
                    -0.03366385906010516, 
                    -0.11854725925356777, 
                    -0.11854725925356777, 
                    0.24610520154140825, 
                    -0.09064291035447211, 
                    0.2311238339461213, 
                    0.020814869134574837, 
                    -0.18042632419044263, 
                    0.039655683625402144, 
                    -0.18042632419044263, 
                    0.10297514903059124, 
                    -0.03211106524140839, 
                    -0.11854725925356777, 
                    0.08543385772587925, 
                    -0.2038483861696567, 
                    -0.13922239245009174, 
                    -0.18042632419044263, 
                    -0.11102553975368507, 
                    -0.06874575660979482, 
                    0.09649882862377662, 
                    0.2717238814116038, 
                    0.1941630951504582, 
                    0.22048571967363534, 
                    0.08268583314884431, 
                    -0.18042632419044263, 
                    -0.2038483861696567, 
                    -0.13352816433235187, 
                    -0.02571877666352318, 
                    -0.2038483861696567, 
                    -0.17822890430188348, 
                    0.02763331028431909, 
                    -0.18042632419044263, 
                    -0.1239779169922394, 
                    0.039655683625402144, 
                    -0.045645443290686816, 
                    -0.2038483861696567, 
                    0.036529545175534084, 
                    0.10297514903059124, 
                    0.22247814166764807, 
                    0.05975595002851536, 
                    0.02763331028431909, 
                    0.034581563801062726, 
                    -0.11854725925356777, 
                    -0.2038483861696567, 
                    -0.18042632419044263, 
                    -0.2038483861696567, 
                    0.039655683625402144, 
                    0.28306123921950416, 
                    -0.11854725925356777, 
                    -0.18042632419044263, 
                    -0.04867636734806752, 
                    -0.2038483861696567, 
                    -0.11854725925356777, 
                    -0.09064291035447211, 
                    0.05975595002851536, 
                    -0.20316701214471575, 
                    0.194732488054504, 
                    0.20130504908092317, 
                    -0.053767608978072146, 
                    0.2688171484708366, 
                    -0.18042632419044263, 
                    0.2222688905380945, 
                    0.16539776669461353, 
                    0.030054653846531942, 
                    -0.1239779169922394, 
                    0.06245960629092856, 
                    -0.1005973415173519, 
                    -0.09064291035447211, 
                    -0.18042632419044263, 
                    0.14591813929739622, 
                    -0.05364688976000179, 
                    0.32100071009680803, 
                    -0.2038483861696567, 
                    -0.11854725925356777, 
                    -0.2038483861696567, 
                    0.11195767565106646, 
                    -0.1239779169922394, 
                    -0.11854725925356777, 
                    -0.2038483861696567, 
                    0.09914497786916315, 
                    0.3560989012205326, 
                    0.11836468054087929, 
                    0.2361543371020799, 
                    -0.2038483861696567, 
                    -0.11854725925356777, 
                    0.10520135661073854, 
                    -0.07092008170290715, 
                    -0.07143087256322272, 
                    0.17843352496904497, 
                    -0.05364688976000179, 
                    0.17589167356367674, 
                    -0.12864617383190086, 
                    0.2983780890874977, 
                    0.10603118017885199, 
                    0.03477850604064259, 
                    -0.20316701214471575, 
                    0.31542129238200683, 
                    0.361019006972169, 
                    0.01134791377247965, 
                    -0.11854725925356777, 
                    0.05975595002851536, 
                    -0.18042632419044263, 
                    0.10520135661073854, 
                    -0.18042632419044263, 
                    -0.09064291035447211, 
                    -0.02701982361140376, 
                    0.11195767565106646, 
                    0.2755841303047216, 
                    -0.14281873148306667, 
                    0.3491368568800188, 
                    -0.18042632419044263, 
                    0.34287073724033507, 
                    -0.11854725925356777, 
                    -0.02701982361140376, 
                    -0.040214785552015214, 
                    -0.18042632419044263, 
                    -0.02701982361140376, 
                    -0.02701982361140376, 
                    0.04647412477514634, 
                    -0.035892379590776013, 
                    -0.13922239245009174, 
                    0.07091934067110667, 
                    0.25705609040218386, 
                    0.277481821504703, 
                    -0.11854725925356777, 
                    0.05975595002851536, 
                    0.024760326747138184, 
                    -0.11854725925356777, 
                    0.2650931156333086, 
                    -0.045645443290686816, 
                    0.1310956343669426, 
                    -0.11854725925356777, 
                    -0.053558357848518534, 
                    -0.06430179647387213, 
                    -0.16522754126741201, 
                    -0.0846266387517566, 
                    -0.2038483861696567, 
                    -0.18042632419044263, 
                    -0.053558357848518534, 
                    0.20565824072691685, 
                    -0.040214785552015214, 
                    0.03937332062930238, 
                    0.17896415258133475, 
                    -0.18042632419044263, 
                    -0.0051533970977113985, 
                    -0.18042632419044263, 
                    0.1276877239155158, 
                    0.22310359715365569, 
                    -0.11854725925356777, 
                    0.15279136024456863, 
                    -0.18042632419044263, 
                    -0.2038483861696567, 
                    -0.0846266387517566, 
                    -0.11854725925356777, 
                    0.27695329246535266, 
                    -0.02701982361140376, 
                    -0.007716101923204821, 
                    0.2835813354008268, 
                    -0.18042632419044263, 
                    -0.2038483861696567, 
                    0.20348613852509495, 
                    -0.0846266387517566, 
                    -0.11854725925356777, 
                    -0.05841419475914944, 
                    -0.18042632419044263, 
                    -0.015509750191563004, 
                    0.03758623329495911, 
                    -0.05364688976000179, 
                    -0.20316701214471575, 
                    0.1701895235228531, 
                    -0.13922239245009174, 
                    0.34287073724033507, 
                    0.11836468054087929, 
                    -0.11102553975368507, 
                    -0.18042632419044263, 
                    -0.1046279095808635, 
                    -0.11854725925356777, 
                    -0.11854725925356777, 
                    -0.05364688976000179, 
                    -0.05364688976000179, 
                    -0.13922239245009174, 
                    -0.02701982361140376, 
                    0.38663848883994195, 
                    0.007095674985883732, 
                    0.325608834153352, 
                    -0.2038483861696567, 
                    0.030054653846531942, 
                    0.17896415258133475, 
                    0.20348613852509495, 
                    0.22591637741230686, 
                    0.016555370306294087, 
                    0.1276877239155158, 
                    -0.18042632419044263 
)
)

