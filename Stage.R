allEL <- allDogList[[1]]
allVL <- allDogList[[2]]
allNet <- allDogList[[3]]
allAdjMat <- allDogList[[4]]

yaEL <- youngAdultList[[1]]
yaVL <- youngAdultList[[2]]
yaNet <- youngAdultList[[3]]
yaAdjMat <- youngAdultList[[4]]

miEL <- middleAdultList[[1]]
miVL <- middleAdultList[[2]]
miNet <- middleAdultList[[3]]
miAdjMat <- middleAdultList[[4]]

maEL <- matureAdultList[[1]]
maVL <- matureAdultList[[2]]
maNet <- matureAdultList[[3]]
maAdjMat <- matureAdultList[[4]]

sEL <- seniorList[[1]]
sVL <- seniorList[[2]]
sNet <- seniorList[[3]]
sAdjMat <- seniorList[[4]]

catTable <- data.frame(ageCat = c("All Dogs","Young Adult", "Middle Adult", 
                                  "Mature Adult", "Senior"),
                       index = c(1:5))

ELList <- list(allEL, yaEL, miEL, maEL, sEL)
VLList <- list(allVL, yaVL, miVL, maVL, sVL)



ageToIndex <- function(AgeCat){
  catTable %>% 
    filter(ageCat == AgeCat) %>%
    pull(index) %>%
    return()
}


