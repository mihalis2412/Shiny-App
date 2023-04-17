
###########################################            Shiny app

library(readxl)
data <- read_excel("./brca_metabric_clinical_data_1980.xlsx",col_names = T) # 1980 obs of 39 variables

# Convert the character missing values
data[ data == "NA" ] <- NA

# Data left after omitting the missing values
df <- na.omit(data) 
str(df) # 1092 obs

# Creating our variables
event <- df$`Overall Survival Status` # Target variable whether the patient is alive of dead.
table(event)

event[event == '0:LIVING'] <- 0
event[event == '1:DECEASED'] <- 1
event <- as.numeric(event)
table(event)

age <- df$`Age at Diagnosis` # Age of the patient at diagnosis time

type_breast_surg <- factor(df$`Type of Breast Surgery`) # Breast cancer surgery type: 1- MASTECTOMY, which refers to a surgery to 
# remove all breast tissue from a breast as a way to treat or prevent breast cancer. 2- BREAST CONSERVING, which refers to a urgery 
# where only the part of the breast that has cancer is removed

cancer_type <- factor(df$`Cancer Type`) # Breast cancer types: 1- Breast Cancer or 2- Breast Sarcoma
table(cancer_type) # 1 level!

cancer_type_det <- factor(df$`Cancer Type Detailed`)

cell <- factor(df$Cellularity) # Cancer cellularity post chemotherapy, which refers to the amount of tumor cells in the specimen and 
# their arrangement into clusters

chemo <- factor(df$Chemotherapy) # Whether or not the patient had chemotherapy as a treatment (yes/no)

Pam50 <- factor(df$`Pam50 + Claudin-low subtype`) # Pam 50: is a tumor profiling test that helps show whether some estrogen 
# receptor-positive (ER-positive), HER2-negative breast cancers are likely to metastasize (when breast cancer spreads to other organs). 
# The claudin-low breast cancer subtype is defined by gene expression characteristics, most prominently: Low expression of cell–cell
# adhesion genes, high expression of epithelial–mesenchymal transition (EMT) genes, and stem cell-like/less differentiated gene 
# expression patterns

cohort <- factor(df$Cohort) # Cohort is a group of subjects who share a defining characteristic (It takes a value from 1 to 5)

er_ihc <- factor(df$`ER status measured by IHC`) # To assess if estrogen receptors are expressed on cancer cells by using
# immune-histochemistry (a dye used in pathology that targets specific antigen, if it is there, it will give a color, it is not there, 
# the tissue on the slide will be colored) (positive/negative)

er <- factor(df$`ER Status`) # Cancer cells are positive or negative for estrogen receptors

nhg <- factor(df$`Neoplasm Histologic Grade`) # Determined by pathology by looking the nature of the cells, do they look aggressive or
# not (It takes a value from 1 to 3)

her2_snp6 <- factor(df$`HER2 status measured by SNP6`) # To assess if the cancer positive for HER2 or not by using advance molecular 
# techniques (Type of next generation sequencing)

her2 <- factor(df$`HER2 Status`) # Whether the cancer is positive or negative for HER2

tohs <- factor(df$`Tumor Other Histologic Subtype`) # Type of the cancer based on microscopic examination of the cancer tissue 
# (It takes a value of 'Ductal/NST', 'Mixed', 'Lobular', 'Tubular/ cribriform', 'Mucinous', 'Medullary', 'Other', 'Metaplastic' )

hormone <- factor(df$`Hormone Therapy`) # Whether or not the patient had hormonal as a treatment (yes/no)

ims <- factor(df$`Inferred Menopausal State`) # Whether the patient is is post menopausal or not (post/pre)

int_clust <- factor(df$`Integrative Cluster`) # Molecular subtype of the cancer based on some gene expression 
# (It takes a value from '4ER+', '3', '9', '7', '4ER-', '5', '8', '10', '1', '2', '6')

ptl <- factor(df$`Primary Tumor Laterality`) # Whether it is involving the right breast or the left breast

lnep <- as.numeric(df$`Lymph nodes examined positive`) # To take samples of the lymph node during the surgery and see if there were 
# involved by the cancer

mc <- as.numeric(df$`Mutation Count`) # Number of gene that has relevant mutations

npi <- df$`Nottingham prognostic index` # It is used to determine prognosis following surgery for breast cancer. Its value is 
# calculated using three pathological criteria: the size of the tumour; the number of involved lymph nodes; and the grade of the tumour.

oc <- factor(df$`Oncotree Code`) # The OncoTree is an open-source ontology that was developed at Memorial Sloan Kettering Cancer 
# Center (MSK) for standardizing cancer type diagnosis from a clinical perspective by assigning each diagnosis a unique OncoTree code.

Time <- df$`Overall Survival (Months)` # Duration from the time of the intervention to death

prs <- factor(df$`PR Status`) # Cancer cells are positive or negative for progesterone receptors

radio <- factor(df$`Radio Therapy`) # Whether or not the patient had radio as a treatment (yes/no)

sex <- factor(df$Sex) # Careful there's 1 level

gcs <- factor(df$`3-Gene classifier subtype`) # Three Gene classifier subtype It takes a value from 'ER-/HER2-', 
# 'ER+/HER2- High Prolif', nan, 'ER+/HER2- Low Prolif','HER2+'

tmb <- df$`TMB (nonsynonymous)` # TMB, defined as the number of somatic mutations per megabase of interrogated genomic sequence,
# varies across malignancies

tumor_size <- as.numeric(df$`Tumor Size`) # Tumor size measured by imaging techniques

tumor_stage <- factor(df$`Tumor Stage`) # Stage of the cancer based on the involvement of surrounding structures, lymph nodes and 
# distant spread

vital_status <- df$`Patient's Vital Status` # Died of disease etc



##############################################       Merging some levels


table(tohs)

# Merge tohs levels
tohs[tohs == 'Medullary'] <- 'Other'
tohs[tohs == 'Mucinous'] <- 'Other'
tohs[tohs == 'Tubular/ cribriform'] <- 'Other'
table(tohs)

tohs <- factor(tohs,levels = c('Ductal/NST','Lobular','Mixed','Other'))
table(tohs)

# Merge oc levels
levels(oc) <- c(levels(oc),'Other')
oc[oc == 'BREAST'] <- 'Other'
oc[oc == 'IMMC'] <- 'Other'
oc[oc == 'ILC'] <- 'Other'
table(oc)

oc <- factor(oc, levels=c('IDC','MDLC','Other'))
table(oc)




# Merge cancer_type_det levels
levels(cancer_type_det) <- c(levels(cancer_type_det),'Other')
cancer_type_det[cancer_type_det == 'Breast'] <- 'Other'
cancer_type_det[cancer_type_det == 'Breast Invasive Mixed Mucinous Carcinoma'] <- 'Other'
cancer_type_det[cancer_type_det == 'Invasive Mixed Mucinous Carcinoma'] <- 'Other'

table(cancer_type_det)

cancer_type_det <- factor(cancer_type_det, levels=c('Breast Invasive Ductal Carcinoma',
                                                    'Breast Invasive Lobular Carcinoma','Breast Mixed Ductal and Lobular Carcinoma',
                                                    'Other'))

table(cancer_type_det)





# Create a data frame with all the predictors 
predictors <- data.frame( age = age, type_breast_surg = type_breast_surg,
                          cancer_type_det = cancer_type_det, tumor_size = tumor_size,  
                          cell = cell, chemo = chemo, Pam50 = Pam50, cohort = cohort, er = er,  nhg = nhg, her2 = her2, tohs = tohs, 
                          hormone = hormone, ims = ims, ptl = ptl, lnep = lnep, mc = mc, npi = npi, oc = oc,
                          prs = prs, radio = radio, gcs = gcs, tumor_size = tumor_size, tumor_stage = tumor_stage, tmb = tmb,
                          Time = Time, event = event )

str(predictors) 
# 26 predictors (including Time and Event) without: tmb (correlated with mc), int_clust, her2_snp6, er_ihc, study ID, patient ID,
# relapse free status x2, Number of Samples Per Patient, Sample Type, Sex (1 level), Patient's Vital Status, cancer_type (1 level)







##############################################       Cut off data along with the desirable KM plot


# number of patients
n_pts <- dim(predictors)[1]
n_pts

# new dataset: observation time <= 118 months for all subjects 
# (therefore for all pts with Time > 118 we must have event = 0, everything else stays the same)
cut_off_dat <- predictors
for (i in 1:n_pts){
  if (predictors$Time[i] > 118){ 
    cut_off_dat$Time[i] <- 118
    cut_off_dat$event[i] <- 0
  }
}

# check
sum(cut_off_dat$Time > 118) # 0 cases -> ok!
sum(predictors$Time > 118) # initially, 541 cases had Time > 118 months
sum(cut_off_dat$Time == 118) # also 541 cases as expected
# we now check if the event indicator changed 
x <- predictors[predictors$Time > 118,]
sum(x$event) # 198 events after 118 months
length(which(cut_off_dat$event != predictors$event)) # indeed we have 198 cases with dif indicator 

library(survival)
#library(ggfortify)

km <- survfit(Surv(Time, event) ~ 1, type = "kaplan-meier", data = cut_off_dat)

plot(km , xlab="Months", ylab='S(t)', conf.int = F, main = 'Kaplan Meyer Plot',xlim = c(0,337), lwd = 2, las = 1)
# autoplot(km ,conf.int = T, xlab = 'Months', main = 'Kaplan Meier Plot', xlim=c(0,337) )
# print(km,print.rmean = T)

cut_off_dat <- cut_off_dat[,-23] 

str(cut_off_dat) # 26 predictors






######################################         Dummy coding



#cut_off_dat$cancer_type_det_1 <- ifelse(cut_off_dat$cancer_type_det == 'Breast Invasive Ductal Carcinoma', 1, 0)
cut_off_dat$cancer_type_det_2 <- ifelse(cut_off_dat$cancer_type_det == 'Breast Invasive Lobular Carcinoma', 1, 0)
cut_off_dat$cancer_type_det_3 <- ifelse(cut_off_dat$cancer_type_det == 'Breast Mixed Ductal and Lobular Carcinoma', 1, 0)
cut_off_dat$cancer_type_det_4 <- ifelse(cut_off_dat$cancer_type_det == 'Other', 1, 0)

#cancer_type_det_1 <- cut_off_dat$cancer_type_det_1
cancer_type_det_2 <- cut_off_dat$cancer_type_det_2
cancer_type_det_3 <- cut_off_dat$cancer_type_det_3
cancer_type_det_4 <- cut_off_dat$cancer_type_det_4




#cut_off_dat$gcs_1 <- ifelse(cut_off_dat$gcs == 'ER-/HER2-', 1, 0)
cut_off_dat$gcs_2 <- ifelse(cut_off_dat$gcs == 'ER+/HER2- High Prolif', 1, 0)
cut_off_dat$gcs_3 <- ifelse(cut_off_dat$gcs == 'ER+/HER2- Low Prolif', 1, 0)
cut_off_dat$gcs_4 <- ifelse(cut_off_dat$gcs == 'HER2+', 1, 0)


#gcs_1 <- cut_off_dat$gcs_1
gcs_2 <- cut_off_dat$gcs_2
gcs_3 <- cut_off_dat$gcs_3
gcs_4 <- cut_off_dat$gcs_4









library(shiny)
library(survival)
library(flexsurv)
library(dplyr)
library(rms)


d <- datadist(cut_off_dat)





# Define server
server <- function(input, output) {
  
  # Fit a Kaplan-Meier model to the breast cancer full data set
  km_fit <- survfit(Surv(Time, event) ~ 1, data = data)
  
  # Create a Kaplan-Meier plot
  output$km_plot <- renderPlot({
    plot(km_fit, xlab = "Time (Years)", ylab = "Survival Probability", las = 1, lwd = 3, conf.int = input$km_ci, xlim = c(0,441),
         main = 'Survival Curves')
    
    # Add the predicted survival probabilities if the checkbox is checked
    if (input$show_expo) {
      
      # Fit an Exponential model via survival package
      model.expo <- survreg(Surv(Time, event) ~ age + tumor_size + lnep + gcs_2 + gcs_3 + gcs_4 + cancer_type_det_2 + 
                              cancer_type_det_3 + cancer_type_det_4, data = cut_off_dat, dist = "exp")
      
      # Generate time points for plotting the survival curve
      times <- seq(119, 441, 1)
      
      # For the Exponential distribution: Assuming that h(t) = λ = exp(−Intercept), we calculate λ by:
      lambda1 <- exp(-(model.expo$coefficients[1] + model.expo$coefficients[2]*mean(age) + model.expo$coefficients[3]*mean(tumor_size) +
                         model.expo$coefficients[4]*mean(lnep) + model.expo$coefficients[5]*mean(gcs_2) + model.expo$coefficients[6]*mean(gcs_3) +
                         model.expo$coefficients[7]*mean(gcs_4) + model.expo$coefficients[8]*mean(cancer_type_det_2) + 
                         model.expo$coefficients[9]*mean(cancer_type_det_3) + model.expo$coefficients[10]*mean(cancer_type_det_4 )))
      
      
      St2 <- exp(-(lambda1 * times))
      
      ex <- as.data.frame(cbind(t = times, St = St2))
      
      lines(times, ex$St, xlab = "Time (Years)", ylab = "Survival Probability", col = 'blue', lwd = 3)
      
    }
    if (input$show_expo_rms) {
     
      # Fit an Exponential model via the rms package
      expo <- psm((Surv(Time,event) ~ age + tumor_size + lnep + gcs_2 + gcs_3 + gcs_4 + cancer_type_det_2 + cancer_type_det_3 + cancer_type_det_4 ), 
                  dist = 'exponential', data = cut_off_dat)
      
      times <- seq(119, 441, 1)
      
      probs_expo <- survest(expo, cut_off_dat, times = seq(119,441,1), conf.int = FALSE, what = 'survival')
      
      mean_surv_month_expo <- apply(probs_expo,2,mean)
      
      lines(times, mean_surv_month_expo, xlab = "Time (Years)", ylab = "Survival Probability", col = 'red', lwd = 3)
      
    }
    if (input$show_weibull) {
      
      # Fit a Weibull AFT model via survival package
      wei_ <- survreg( Surv(Time,event) ~ age + tumor_size + lnep + gcs_2 + gcs_3 + gcs_4 + cancer_type_det_2 + cancer_type_det_3 +
                         cancer_type_det_4,
                       data = cut_off_dat, dist = "weibull")
      
      # Generate time points for plotting the survival curve
      times <- seq(119, 441, 1)
      
      p1 <- 1/wei_$scale # Calculate the shape
      
      # Calculate the scale
      lambda2 <- exp(-(wei_$coefficients[1] + wei_$coefficients[2]*mean(age) + wei_$coefficients[3]*mean(tumor_size)
                       + wei_$coefficients[4]*mean(lnep) + wei_$coefficients[5]*mean(gcs_2) + wei_$coefficients[6]*mean(gcs_3)
                       + wei_$coefficients[7]*mean(gcs_4) + wei_$coefficients[8]*mean(cancer_type_det_2)
                       + wei_$coefficients[9]*mean(cancer_type_det_3) + wei_$coefficients[10]*mean(cancer_type_det_4) ))
      
      
      
      St <- exp(-(lambda2*times)^p1)
      
      weib <- as.data.frame(cbind(times=times,St=St))
      
      lines(times, weib$St, xlab = "Time (Years)", ylab = "Survival Probability", col = 'purple', lwd = 3)
      
    }
    
    if (input$show_weibull_rms) {
      
      wei.rms <- psm((Surv(Time,event) ~ age + tumor_size + lnep + gcs_2 + gcs_3 + gcs_4 + cancer_type_det_2 + cancer_type_det_3 + cancer_type_det_4 ), 
                     dist = 'weibull', data = cut_off_dat)
      
      probs_wei.rms <- survest(wei.rms, cut_off_dat, times = seq(119,441,1), conf.int = FALSE, what='survival')
      
      mean_surv_month_wei.rms <- apply(probs_wei.rms,2,mean)
      
      # Generate time points for plotting the survival curve
      times <- seq(119, 441, 1)
      
      weib_rms <- as.data.frame(cbind(times=times,St=mean_surv_month_wei.rms))
      
      lines(times, weib_rms$St, xlab = "Time (Years)", ylab = "Survival Probability", col = 'orange', lwd = 3, las = 1)
      
    }
    
    
    
    if (input$show_gompertz) {
      
      
      # Fit a Gompertz model via the survival package
      model.gomp <- flexsurvreg(Surv(Time, event) ~ age + tumor_size + lnep + gcs_2 + gcs_3 + gcs_4 + cancer_type_det_2 + cancer_type_det_3 +
                                  cancer_type_det_4, data=cut_off_dat, dist="gompertz")
      print(model.gomp)
      
      
      # rate parameter, MUST BE POSITIVE!
      b <- exp(model.gomp$coefficients[2] + model.gomp$coefficients[3]*mean(age) +
                 model.gomp$coefficients[4]*mean(tumor_size) + model.gomp$coefficients[5]*mean(lnep) +
                 model.gomp$coefficients[6]*mean(gcs_2) + model.gomp$coefficients[7]*mean(gcs_3) +
                 model.gomp$coefficients[8]*mean(gcs_4) + model.gomp$coefficients[9]*mean(cancer_type_det_2) +
                 model.gomp$coefficients[10]*mean(cancer_type_det_3) + model.gomp$coefficients[11]*mean(cancer_type_det_4) ) # Calculate the rate parameter
      
      
      ## shape parameter
      # Note that a < 0 is permitted, in which case S(t) tends to a non-zero probability as t increases, i.e. a probability of living forever.
      a <- model.gomp$coefficients[1]
      
      
      times <- seq(119, 441, 1)
      
      # Create a vector to store the values of the survivor function
      gomp_survivor_func <- numeric(length(times)) 
      
      # Survivor function for the Gompertz distribution
      for (i in 1:length(times)){
        gomp_survivor_func[i] <- exp( -(b/a)*(exp(a*times[i])-1))
      }
      
      lines(times, gomp_survivor_func, type = "l", xlab = "Time (Years)", ylab = "Survival Probability", col = 'cyan', lwd = 2)
      
    }
    
    if (input$show_loglogistic) {
      
      log.logistic <- survreg( Surv(Time, event) ~ age + tumor_size + lnep + gcs_2 + gcs_3 + 
                                 gcs_4 + cancer_type_det_2 + cancer_type_det_3 + cancer_type_det_4 , data = cut_off_dat, dist = "loglogistic")
      
      p2 <- 1/log.logistic$scale
      
      lambda3 <- exp(-(log.logistic$coefficients[1] + log.logistic$coefficients[2]*mean(age) + log.logistic$coefficients[3]*mean(tumor_size)
                       + log.logistic$coefficients[4]*mean(lnep) + log.logistic$coefficients[5]*mean(gcs_2) + log.logistic$coefficients[6]*mean(gcs_3)
                       + log.logistic$coefficients[7]*mean(gcs_4) + log.logistic$coefficients[8]*mean(cancer_type_det_2)
                       + log.logistic$coefficients[9]*mean(cancer_type_det_3) + log.logistic$coefficients[10]*mean(cancer_type_det_4) ))
      
      times <- seq(119, 441, 1)
      
      St <- 1/( 1 + (lambda3*times)^p2 )
      
      log.logis <- as.data.frame(cbind(times=times,St=St))
      
      lines(times, log.logis$St, type = "l", xlab = "Time (Years)", ylab = "Survival Probability", col = 'green', lwd = 3)
      
    }
    if (input$show_loglogistic_rms) {
     
      log.log.rms <- psm((Surv(Time,event) ~ age + tumor_size + lnep + gcs_2 + gcs_3 + gcs_4 + cancer_type_det_2 + cancer_type_det_3 + cancer_type_det_4 ), 
                         dist = 'loglogistic', data = cut_off_dat)
      
      probs_log.log.rms <- survest(log.log.rms, cut_off_dat, times = seq(119,441,1), conf.int = FALSE, what='survival')
      
      mean_surv_month_log.log.rms <- apply(probs_log.log.rms,2,mean)
      
      # Generate time points for plotting the survival curve
      times <- seq(119, 441, 1)
      
      lines(times, mean_surv_month_log.log.rms, type = "l", xlab = "Time (Years)", ylab = "Survival Probability", col = 'lightblue', lwd = 3)
      
      
    }
    
    
    
    if (input$show_lognormal_rms) {
     
      log.norm.rms <- psm((Surv(Time,event) ~ age + tumor_size + lnep + gcs_2 + gcs_3 + gcs_4 + cancer_type_det_2 + cancer_type_det_3 + cancer_type_det_4 ), 
                          dist = 'lognormal', data = cut_off_dat)
      
      probs_log.norm.rms <- survest(log.norm.rms, cut_off_dat, times = seq(119,441,1), conf.int = FALSE, what='survival')
      
      mean_surv_month_log.norm.rms <- apply(probs_log.norm.rms,2,mean)
      
      # Generate time points for plotting the survival curve
      times <- seq(119, 441, 1)
      
      lines(times, mean_surv_month_log.norm.rms, type = "l", xlab = "Time (Years)", ylab = "Survival Probability", col = 'darkred', lwd = 3)
      
    }
    
    if (input$show_expo || input$show_expo_rms || input$show_weibull || input$show_weibull_rms || input$show_gompertz || input$show_loglogistic || input$show_loglogistic_rms || input$show_lognormal_rms)  {
      legend("topright", legend = c("Exponential - Survival/survHE", "Exponential - RMS", "Weibull (AFT) - Survival/survHE",
                                    "Weibull (AFT) - RMS", "Gompertz - Survival/survHE",
                                    "Log-Logistic - Survival/survHE","Log-Logistic - RMS","Log-Normal - RMS"),
             col=c("blue","red","orange","purple","cyan",
                         "green","lightblue","darkred"), lwd = 2, cex = 1.5)
    }
    
    
    
    
  }, height = 750, width = 1800)
  
}





















