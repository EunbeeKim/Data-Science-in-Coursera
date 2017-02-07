setwd("C:/Users/admin/Downloads")

pollutantmean <- function(directory, pollutant, id = 1:332) {
        files <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        
        for(i in id){
                dat <- rbind(dat, read.csv(files[i]))
        }
        mean_data <- mean(dat[ ,pollutant], na.rm = TRUE)
        print(mean_data)
}

complete <- function(directory, id = 1:332){
        files_full <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        
        for(i in id) {
                moni_i <-read.csv(files_full[i])
                nobs <- sum(complete.cases(moni_i))
                tmp <- data.frame(i, nobs)
                dat <- rbind(dat, tmp)
        }
        colnames(dat) <- c("id", "nobs")
        dat
}

corr <- function(directory, threshold = 0) {
        files_full <- list.files(directory, full.names = TRUE)
        dat <- vector(mode = "numeric", length = 0)
        
        for(i in 1:length(files_full)) {
                moni_i <- read.csv(files_full[i])
                csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
                if(csum > threshold) {
                        tmp <- moni_i[which(!is.na(moni_i$sulfate)),]
                        submoni_i <- tmp[which(!is.na(tmp$nitrate)),]
                        dat <- c(dat, cor(submoni_i$sulfate, submoni_i$nitrate))
                }
        }
        dat
}