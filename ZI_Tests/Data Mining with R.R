## Data mining with R - K. Denis & Ph. Grosjean

## Adapt this to your system... This must be the directory where you
## unzipped the example dataset
rootpath <- "ZI_Tests/"

smps <- c("./data/Samples/MTPS.2004-10-20.V5.zidb",
          "./data/Samples/MTLG.2005-05-24.H1.zidb")

## Install zooimage & mlearning packages first
## install.packages(c("zooimage", "mlearning"))
## Load ZooImage and it also loads mlearning
library(zooimage)

# Zooimage requires to set default stringsAsFactors to TRUE
old_stringsAsFactors <- getOption("stringsAsFactors")
options(stringsAsFactors = TRUE)

## Switch working directory
old_dir <- setwd(rootpath)


################################################################################
#### Importation and exploration of the data
## After image analysis, one got one directory per sample with vignettes and
## .zim files, like in ./data/Samples
##
## Create a ZIDB for one sample (MTPS.2004-10-20.V5.zidb is created)
zidbMake("./data/Samples/MTPS.2004-10-20.V5")

## Batch creation of ZIDB files for all samples in a dir (and delete source)
## Note: use delete.source = TRUE to clean up the disk and keep only ZIDB files
zidbMakeAll("./data/Samples", delete.source = FALSE, replace = TRUE)

## Load and explore data from one ZIDB file
dat1 <- zidbDatRead(smps[1])
head(dat1)
attr(dat1, "metadata")
summary(dat1[, c("Area", "Perim.", "Skew", "Kurt")])
plot(dat1$Area, dat1$Perim., xlab = "Area", ylab = "Perimeter")

## Lazy loading data from one ZIDB file in R
db1 <- zidbLink(smps[1])
items1 <- ls(db1) # Contains data in *_dat1 and vignettes in *_nn
vigs1 <- items1[-grep("_dat1", items1)]
## Display a 5*5 thumbnail of the first 25 vignettes
zidbPlotNew("The 25 first vignettes in MTPS.2004-10-20.H1")
for (i in 1:25)
    zidbDrawVignette(db1[[vigs1[i]]], item = i, nx = 5, ny = 5)


################################################################################
#### Training set preparation
prepareTrain("./data/_train", zidbfiles = smps, template = "[Detailed]")
## Inspect this directory and its subdirs, then ...
## (you are supposed to move vignettes manually in the classes folders now;
## not all vignettes need to be sorted: those left in "_", or one of its
## subdirectories will *not* be used in the training set)

## A ZIC file is used to describe initial classes in your training set.
## Make your own ZIC file by inspiring you from Basic/Detailed/Very detailed
file.show(system.file("etc", "Detailed.zic", package = "zooimage"))

## Read a training set - here, our example training set
train <- getTrain("./data/Training set")
head(train)
sort(table(train$Class))
## It is faster to start from the .Rdata file => use save()/load()
#save(train, file = "./data/Training set.RData")
#load("./data/Training set.RData")

## Since there is a hierachy in the classes, one can recode the training set
## at various depth in that hierarchy
## Here is it a the first depth level
table(recode(train, depth = 1)$Class)
## ... and at the second depth level
table(recode(train, depth = 2)$Class)


################################################################################
#### Classifier with all classes (no recoding)
## ZIClass trains a classifier and also performs k-fold cross-validation
## an the training set if cv.k is not NULL
## See ?mlearning and ?cvpredict in package mlearning for more details
classif <- ZIClass(Class ~ ., train, method = "mlRforest", calc.vars = calcVars,
    ntree = 200, cv.k = 10) # Note than method, calc.vars & cv.k are default values
classif

## The various steps involved here are:
## 1) Calculation of derived/transformed variables, and also, possibly,
##    selection of variables is done by calcVars() (or your awn function)
names(train) # Original variables
names(traincalc <- calcVars(train)) # Variables as seen be classifier
## 2) Train a classifier on transformed data frame
classifrf <- mlRforest(Class ~ ., data = traincalc, ntree = 200)
## 3) Apply x-fold cross-validation on the same training set with the choosen
##    algorithm (here, 10-fold cross-validation)
classifcv <- predict(classifrf, method = "cv", cv.k = 10)

## Classifier performance checking
summary(classif) # Many different statistics based on the confusion matrix
summary(classif, type = c("Fscore", "Recall", "Precision")) # Selected ones
conf <- confusion(classif)
conf # 1/5 of particles incorrectly classified
## In the specific case of random forest, you could decide as well to use the
## out-of-bag prediction at this stage:
confoob <- confusion(classif, predict(classif, method = "oob"))
## If you combine this, with ZIClass(...., cv.k = NULL), you short circuit
## the time-consumming cross-validation step on the creation of your ZIClass
## object, and you can fine-tune it faster. However, out-of-bag is not
## available for all machine learning algorithms, and it is also perhaps
## not as reliable as stratified cross-validation

plot(conf, type = "image")
## By default, classes are sorted according to (recall + precision)/2
## Here s the dendrogram for the sorting
plot(conf, type = "dendrogram")
## Two plots to view F-score and F-score decomposition into recall / precision
plot(conf, type = "barplot")
plot(conf, type = "stars")

## Comparison with another classification algorithm
classif2 <- ZIClass(Class ~ ., train, method = "mlSvm", kernel = "linear")
classif2
summary(classif2)
conf2 <- confusion(classif2)
conf2
plot(conf2)
plot(conf2, type = "barplot")

## Comparison of both classifiers
plot(conf, conf2)

## The confusion matrix depends on relative frequencies in the various groups
## that can change dramatically for a real sample. Change prior to reflect
## a given situation. E.g., a sample with many Calanoida, Chaetognatha and
## half of the particles being marine snow/debris/fiber:
priors <- rep(20, 25)
names(priors) <- rownames(conf)
priors[c("Calanoida", "Chaetognatha")] <- c(300, 100)
priors[c("debris", "fiber", "marine snow")] <- 266
priors
prior(conf) <- priors
summary(conf) # Error rate increases to 1/4 in this case!
plot(conf)
plot(conf, type = "barplot")
## We see that the large amount of marine snow and debris cause lots of
## contamination in other groups, mainly in each other (no problems)
## but also to small groups like Decapoda or Poecilostomatoida
prior(conf) <- NULL # Priors back to training set proportions


################################################################################
#### Testing performances of the classifier
## The use of a "representative" sample, completelly classified manually is the
## best approach here... keeping in mind that observed statistics may vary
## if relative proportions of the classes change in other samples
prepareTest("./data/_test", zidbfiles = smps[1], template = classif)
## Note that smps[1] was *not* used in the training set
## (here, you are supposed to sort all items in the respective classes...)
## Note the '_' subdirectory that is added: it must contain all the particles
## you cannot sort (unidentifiable, multiples, parts, individuals from
## taxonomic groups not included in the other classes, etc.)
##
## Here, we collect results from an already sorted tree...
test <- getTest("./data/Test sets/MTPS.2004-10-20.V5")
## Create and study the confusion matrix on this test set
conf3 <- confusion(predict(classif, test), test$Class)
conf3  # Almost 1/2 particle incorrectly classified... because >15% of the
## particles are in the unknown (NA) class! => 100% error for these!
plot(conf3)
plot(conf3, type = "barplot")
## These are typical predictions for unknown samples... missclassification
## is too high for it to be of any use. Current practice is to perform
## a manual correction for *all* particles... (see Gorsky et al 2010).


################################################################################
#### Look at classification results and/or manually validate that classification
## (this is the current practice in plankton studies...
## of course, to be improved/replace by better strategies, e.g., error
## correction after *partial* manual validation)
prepareTest("./data/_test2", smps[2], template = classif, classes = classif)
## (now, you can inspect the subdirectories and reclassify manually wrong items)
## Let's pretend this is the result from your validation
test2 <- getTest("./data/Test sets/MTLG.2005-05-24.H1")
### Create and study the confusion matrix on this test set
conf4 <- confusion(predict(classif, test2), test2$Class)
conf4  # 1/3 of the particles incorrectly classified... because >25% of them
### are in the unknown (NA) class! and thus, considered as error
plot(conf4)
plot(conf4, type = "barplot")


################################################################################
#### Calculation of samples statistics after particles classification
dat2 <- zidbDatRead(smps[2]) # Read a ZIDat object
## Add manually validated items
dat2 <- addClass(dat2, test2)
## Add a 'Predicted' column
dat2 <- predict(classif, dat2, class.only = FALSE)
names(dat2)
## Calculate abundances per classes
## Here we take into account how the sample was prepared to give a weight to
## each particle according to its "dilution"... This is is dat1$Dil
## Dil = 1/SubPart/CellPart/Replicates/VolIni in nbr ind/m^3
subsmp <- attr(dat2, "metadata")$Subsample
1/subsmp$SubPart/subsmp$CellPart/subsmp$Replicates/subsmp$VolIni
## Equivalent to
unique(dat2$Dil)

## Abundances (total and per class)
## Here is the total abundance of particles / m^3
processSample(dat2) # env. 1 particles per liter
## However, since we are only interested by zooplankton, we should keep only
## those classes, ignoring the rest
zoo <- levels(dat2$Predicted) # All classes
## We used first uppercase for zooplankton groups, thus:
zoo <- zoo[grepl("^[A-Z]", zoo)] # Zooplankton only
zoo
processSample(dat2, keep = zoo) # 0.3 zooplankton individual per liter
## This is because we have a very clear coral reef water
## Now, we also want to calculate separate abundances for most abundant classes
## i.e., those with at least 50 individuals measured
detail <- zoo[zoo %in% levels(dat2$Class)[table(dat2$Class) >= 50]]
detail
processSample(dat2, keep = zoo, detail = detail)

## Biomasses, using individual size to biomass conversion functions
## (of course, you must provide parameters for these conversion functions)
## Here we use the general equation for mesozooplankton as calculated by
## Hernandez-Leon & Montero (2006):
## Ind. dry weight (ug) = 45.7 * Area (mm^2)^1.19, or ...
## Ind. dry weight (ug) = 34.3 * ECD (mm)^2.38
## Biomass calculation is P1 * ECD^P3 + P2
## In a real study, independent relationships for the different classes would
## be use... It this case, present your conversion factors that way:
read.delim(system.file("etc", "Conversion.txt", package = "zooimage"))
processSample(dat2, keep = zoo, biomass = c(34.3, 0, 2.38))
processSample(dat2, keep = zoo, detail = detail, biomass = c(34.3, 0, 2.38))

## Size spectra, total and per class
processSample(dat2, keep = zoo, breaks = seq(0.2, 2, by = 0.2))
processSample(dat2, keep = zoo, detail = detail, breaks = seq(0.2, 2, by = 0.2))

## The whole at once
processSample(dat2, keep = zoo, detail = detail, biomass = c(34.3, 0, 2.38),
    breaks = seq(0.2, 2, by = 0.2))

## Process all samples located in a directory in batch
## Note that this is done from the ZIDB files, without using any
## manual validation information here!
processSampleAll("./data/Samples", ZIClass = classif, keep = zoo,
    detail = detail, biomass = c(34.3, 0, 2.38), breaks = seq(0.2, 2, by = 0.2))

## Restore the system configuration
setwd(old_dir)
options(stringsAsFactors = old_stringsAsFactors)
