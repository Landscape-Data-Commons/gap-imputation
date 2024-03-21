### Author: Joe Brehm ###
### Last updated 10/31/2023 ###

### 1 Load Packages and set paths and run parameters ####
library(tidyverse)
library(terradactyl)
library(lmtest)
library(MASS)
library(dplyr)
library(caret)
library(grid)
library(lemon)

path_out <- "C:/Users/jrbrehm/Documents/Data/Gap Imputation"
path_BLM_AIM <- "C:/Users/jrbrehm/Documents/Data/BLM_AIM/Tall"
path_NRI <- "C:/Users/jrbrehm/Documents/Data/NRI_2004-2020/Tall/"

getnewdata =  F

### 1 Get Data ####
if(getnewdata){
  gap_tall_AIMLMF <- readRDS(file.path(path_BLM_AIM, "gap_tall.rdata"))
  gap_tall_NRI <- readRDS("gap_tall.rdata")
  
  header_AIMLMF <- readRDS(file.path(path_BLM_AIM, "header.rdata"))
  header_NRI <- readRDS(file.path(path_NRI, "header.rdata"))
  
  gap_tall_AIMLMF_filter <- subset(gap_tall_AIMLMF, !is.na(Measure) & PrimaryKey %in% header_AIMLMF$PrimaryKey)
  gap_tall_NRI_filter <- subset(gap_tall_NRI, !is.na(Measure) & PrimaryKey %in% header_NRI$PrimaryKey)
  
  # remove cases in tall_gap where Gap == 0
  gap_tall_AIMLMF_filter <- gap_tall_AIMLMF %>% dplyr::filter(Gap != 0)
  gap_tall_NRI_filter <- gap_tall_NRI_filter %>% dplyr::filter(Gap != 0)
  
  # remove cases where canopy and perennial gap are identical
  gap_tall_AIMLMF_filter_P <- subset(gap_tall_AIMLMF_filter, RecType == "P") %>% 
    dplyr::select(PrimaryKey, Gap.P = Gap, SeqNo.P = SeqNo, RecType.P = RecType)
  gap_tall_AIMLMF_filter_C <- subset(gap_tall_AIMLMF_filter, RecType == "C") %>% 
    dplyr::select(PrimaryKey, Gap.C = Gap, SeqNo.C = SeqNo, RecType.C = RecType)
  gap_tall_NRI_filter_P <- subset(gap_tall_NRI_filter, RecType == "P") %>% 
    dplyr::select(PrimaryKey, Gap.P = Gap, SeqNo.P = SeqNo, RecType.P = RecType)
  gap_tall_NRI_filter_C <- subset(gap_tall_NRI_filter, RecType == "C") %>% 
    dplyr::select(PrimaryKey, Gap.C = Gap, SeqNo.C = SeqNo, RecType.C = RecType)
  
  gap_tall_AIMLMF_filter_PC <- full_join(gap_tall_AIMLMF_filter_P, gap_tall_AIMLMF_filter_C, by = "PrimaryKey")
  gap_tall_NRI_filter_PC <- full_join(gap_tall_NRI_filter_P, gap_tall_NRI_filter_C, by = "PrimaryKey")
  badpkeys_AIMLMF <- 
    gap_tall_AIMLMF_filter_PC %>% 
    dplyr::filter(Gap.P == Gap.C & SeqNo.P == SeqNo.C) %>%
    dplyr::pull(PrimaryKey)
  badpkeys_NRI <- 
    gap_tall_NRI_filter_PC %>% 
    dplyr::filter(Gap.P == Gap.C & SeqNo.P == SeqNo.C) %>%
    dplyr::pull(PrimaryKey)
  
  
  gap_tall_AIMLMF <- gap_tall_AIMLMF_filter %>% dplyr::filter(!(PrimaryKey %in% badpkeys_AIMLMF))
  gap_tall_NRI <- gap_tall_NRI_filter %>% dplyr::filter(!(PrimaryKey %in% badpkeys_NRI))
  
  ### calculate indicators
  gap_AIMLMF_P <- gap_cover(gap_tall = gap_tall_AIMLMF,
                            tall = F,
                            type = "perennial canopy",
                            by_line = F)$percent %>%
    dplyr::rowwise() %>% 
    dplyr::select(PrimaryKey, GapCover_25_50 = "25-51", 
                  GapCover_51_100 = "51-101", 
                  GapCover_101_200 = "101-201", 
                  GapCover_200_plus = "201-Inf") %>% 
    dplyr::mutate(GapCover_25_plus = sum(c(GapCover_25_50, GapCover_51_100, GapCover_101_200, GapCover_200_plus)))
  
  gap_AIMLMF_B <- gap_cover(gap_tall = gap_tall_AIMLMF,
                            tall = F,
                            type = "basal",
                            by_line = F)$percent %>%
    dplyr::rowwise() %>% 
    dplyr::select(PrimaryKey, GapCover_25_50 = "25-51", 
                  GapCover_51_100 = "51-101", 
                  GapCover_101_200 = "101-201", 
                  GapCover_200_plus = "201-Inf") %>% 
    dplyr::mutate(GapCover_25_plus = sum(c(GapCover_25_50, GapCover_51_100, GapCover_101_200, GapCover_200_plus)))
  
  gap_AIMLMF_C <- gap_cover(gap_tall = gap_tall_AIMLMF,
                            tall = F,
                            type = "canopy",
                            by_line = F)$percent %>%
    dplyr::rowwise() %>% 
    dplyr::select(PrimaryKey, GapCover_25_50 = "25-51", 
                  GapCover_51_100 = "51-101", 
                  GapCover_101_200 = "101-201", 
                  GapCover_200_plus = "201-Inf") %>% 
    dplyr::mutate(GapCover_25_plus = sum(c(GapCover_25_50, GapCover_51_100, GapCover_101_200, GapCover_200_plus)))
  
  ### nri
  gap_NRI_P <- gap_cover(gap_tall = gap_tall_NRI,
                         tall = F,
                         type = "perennial canopy",
                         by_line = F)$percent %>%
    dplyr::rowwise() %>% 
    dplyr::select(PrimaryKey, GapCover_25_50 = "25-51", 
                  GapCover_51_100 = "51-101", 
                  GapCover_101_200 = "101-201", 
                  GapCover_200_plus = "201-Inf") %>% 
    dplyr::mutate(GapCover_25_plus = sum(c(GapCover_25_50, GapCover_51_100, GapCover_101_200, GapCover_200_plus)))
  
  gap_NRI_B <- gap_cover(gap_tall = gap_tall_NRI,
                         tall = F,
                         type = "basal",
                         by_line = F)$percent %>%
    dplyr::rowwise() %>% 
    dplyr::select(PrimaryKey, GapCover_25_50 = "25-51", 
                  GapCover_51_100 = "51-101", 
                  GapCover_101_200 = "101-201", 
                  GapCover_200_plus = "201-Inf") %>% 
    dplyr::mutate(GapCover_25_plus = sum(c(GapCover_25_50, GapCover_51_100, GapCover_101_200, GapCover_200_plus)))
  
  gap_NRI_C <- gap_cover(gap_tall = gap_tall_NRI,
                         tall = F,
                         type = "canopy",
                         by_line = F)$percent %>%
    dplyr::rowwise() %>% 
    dplyr::select(PrimaryKey, GapCover_25_50 = "25-51", 
                  GapCover_51_100 = "51-101", 
                  GapCover_101_200 = "101-201", 
                  GapCover_200_plus = "201-Inf") %>% 
    dplyr::mutate(GapCover_25_plus = sum(c(GapCover_25_50, GapCover_51_100, GapCover_101_200, GapCover_200_plus)))
  
  colnames(gap_AIMLMF_B)[2:6] <- paste0("B_", colnames(gap_AIMLMF_B)[2:6])
  colnames(gap_AIMLMF_C)[2:6] <- paste0("C_", colnames(gap_AIMLMF_C)[2:6])
  colnames(gap_AIMLMF_P)[2:6] <- paste0("P_", colnames(gap_AIMLMF_P)[2:6])
  
  colnames(gap_NRI_B)[2:6] <- paste0("B_", colnames(gap_NRI_B)[2:6])
  colnames(gap_NRI_C)[2:6] <- paste0("C_", colnames(gap_NRI_C)[2:6])
  colnames(gap_NRI_P)[2:6] <- paste0("P_", colnames(gap_NRI_P)[2:6])
  
  gap_AIMLMF_all <- 
    dplyr::full_join(gap_AIMLMF_B, gap_AIMLMF_C, by = "PrimaryKey") %>%
    dplyr::full_join(gap_AIMLMF_P, by = "PrimaryKey")
  gap_NRI_all <- 
    dplyr::full_join(gap_NRI_B, gap_NRI_C, by = "PrimaryKey") %>%
    dplyr::full_join(gap_NRI_P, by = "PrimaryKey")
  
  ### get lpi data
  lpi_NRI_allvars <- read.csv("C:/Users/jrbrehm/Documents/Data/NRI_2004-2020/geoIndicators.csv") 
  lpi_NRI <- lpi_NRI_allvars %>% 
    dplyr::select(PrimaryKey, AH_AnnGrassCover, AH_ForbCover, AH_PerenGrassCover, AH_ShrubCover)
  lpi_AIMLMF_allvars <- read.csv("C:/Users/jrbrehm/Documents/Data/BLM_AIM/For Ingest/geoIndicators.csv")
  lpi_AIMLMF <- lpi_AIMLMF_allvars %>% 
    dplyr::select(PrimaryKey, AH_AnnGrassCover, AH_ForbCover, AH_PerenGrassCover, AH_ShrubCover)
  
  gaplpi_AIMLMF <- dplyr::left_join(gap_AIMLMF_all, lpi_AIMLMF, by = "PrimaryKey") %>%
    dplyr::mutate(source = "AIMLMF")
  gaplpi_NRI <- dplyr::left_join(gap_AIMLMF_all, lpi_NRI, by = "PrimaryKey") %>%
    dplyr::mutate(source = "NRI")
  
  all_canopy_clean <- rbind(gaplpi_AIMLMF, gaplpi_NRI)%>%
    dplyr::select(
      PrimaryKey,
      source,
      AH_AnnGrassCover,
      AH_ForbCover,
      AH_PerenGrassCover,
      AH_ShrubCover,
      # CP_... in my code indicates gaps stopped only by perennial plants
      CP_percent_25plus = P_GapCover_25_plus,
      CP_percent_100to200 = P_GapCover_101_200,
      CP_percent_200plus = P_GapCover_200_plus,
      CP_percent_25to50 = P_GapCover_25_50,
      CP_percent_50to100 = P_GapCover_51_100,
      # CA_... in my code indicates gaps stopped by both perennial and annual plants
      CA_percent_25plus = C_GapCover_25_plus,
      CA_percent_100to200 = C_GapCover_101_200,
      CA_percent_200plus = C_GapCover_200_plus,
      CA_percent_25to50 = C_GapCover_25_50,
      CA_percent_50to100 = C_GapCover_51_100) %>%
    # filter to only data with all variables present
    na.omit()
  
  # New 5/30 Get mean gap size and sd of gap size
  gap_distributionstats_aim <- 
    dplyr::full_join(
      gap_tall_AIMLMF_filter_P %>% 
        dplyr::select("PrimaryKey", "Gap.P") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CP_MeanGap = mean(Gap.P)),
      gap_tall_AIMLMF_filter_C %>% 
        dplyr::select("PrimaryKey", "Gap.C") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CA_MeanGap = mean(Gap.C))
    ) %>% dplyr::full_join(
      gap_tall_AIMLMF_filter_P %>% 
        dplyr::select("PrimaryKey", "Gap.P") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CP_SDGap = sd(Gap.P))
    ) %>% dplyr::full_join(
      gap_tall_AIMLMF_filter_C %>% 
        dplyr::select("PrimaryKey", "Gap.C") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CA_SDGap = sd(Gap.C))
    ) %>% dplyr::full_join(
      gap_tall_AIMLMF_filter_P %>% 
        dplyr::select("PrimaryKey", "Gap.P") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CP_MedianGap = median(Gap.P))
    ) %>% dplyr::full_join(
      gap_tall_AIMLMF_filter_C %>% 
        dplyr::select("PrimaryKey", "Gap.C") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CA_MedianGap = median(Gap.C))
    )
  
  gap_distributionstats_nri <- 
    dplyr::full_join(
      gap_tall_NRI_filter_P %>% 
        dplyr::select("PrimaryKey", "Gap.P") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CP_MeanGap = mean(Gap.P)),
      gap_tall_NRI_filter_C %>% 
        dplyr::select("PrimaryKey", "Gap.C") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CA_MeanGap = mean(Gap.C))
    ) %>% dplyr::full_join(
      gap_tall_NRI_filter_P %>% 
        dplyr::select("PrimaryKey", "Gap.P") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CP_SDGap = sd(Gap.P))
    ) %>% dplyr::full_join(
      gap_tall_NRI_filter_C %>% 
        dplyr::select("PrimaryKey", "Gap.C") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CA_SDGap = sd(Gap.C))
    ) %>% dplyr::full_join(
      gap_tall_NRI_filter_P %>% 
        dplyr::select("PrimaryKey", "Gap.P") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CP_MedianGap = median(Gap.P))
    ) %>% dplyr::full_join(
      gap_tall_NRI_filter_C %>% 
        dplyr::select("PrimaryKey", "Gap.C") %>% 
        dplyr::group_by(PrimaryKey) %>% 
        dplyr::summarise(CA_MedianGap = median(Gap.C))
    )
  gap_distributionstats <- rbind(gap_distributionstats_nri, gap_distributionstats_aim)
  
  # Filter to remove bad primary keys
  gap_distributionstats <- gap_distributionstats %>% dplyr::filter(!(PrimaryKey %in% c(badpkeys_AIMLMF, badpkeys_NRI)))
  
  # Attach it to the main dataframe
  all_canopy_clean <- dplyr::left_join(all_canopy_clean, gap_distributionstats)  
  
  # Clean up
  rm(gaplpi_AIMLMF, gaplpi_NRI,
     header_AIMLMF, header_NRI,
     lpi_AIMLMF, lpi_NRI,
     gap_AIMLMF_all, gap_AIMLMF_B, gap_AIMLMF_C, gap_AIMLMF_P, gap_tall_AIMLMF,
     gap_NRI_all, gap_NRI_B, gap_NRI_C, gap_NRI_P, gap_tall_NRI,
     gap_tall_AIMLMF_filter, gap_tall_AIMLMF_filter_C, gap_tall_AIMLMF_filter_P, gap_tall_AIMLMF_filter_PC,
     gap_tall_NRI_filter, gap_tall_NRI_filter_C, gap_tall_NRI_filter_P, gap_tall_NRI_filter_PC,
     badpkeys_AIMLMF, badpkeys_NRI,
     lpi_AIMLMF_allvars, lpi_NRI_allvars)
  
  
  # Create the open ended indicators
  all_canopy_clean$CA_percent_100plus <- all_canopy_clean$CA_percent_100to200 + all_canopy_clean$CA_percent_200plus
  all_canopy_clean$CP_percent_100plus <- all_canopy_clean$CP_percent_100to200 + all_canopy_clean$CP_percent_200plus
  all_canopy_clean$CA_percent_50plus <- all_canopy_clean$CA_percent_100to200 + all_canopy_clean$CA_percent_200plus + all_canopy_clean$CA_percent_50to100
  all_canopy_clean$CP_percent_50plus <- all_canopy_clean$CP_percent_100to200 + all_canopy_clean$CP_percent_200plus + all_canopy_clean$CP_percent_50to100
  
  # Drop indicators where the value is greater than 
  # drop records where CP_percent_25plus is zero and CA_percent_25plus is more than 1/2 std. dev.
  # because CA theoretically shouldn't be higher than CP ever, but sometimes the
  # transect line shifts around as you're measuring and you can get CA slightly higher
  # than CP. One standard deviation might not be the most appropriate filter here...
  sd_gap100to200 <- sd(all_canopy_clean$CP_percent_100to200, na.rm = T)
  sd_gap100plus <- sd(all_canopy_clean$CP_percent_100plus, na.rm = T)
  sd_gap200plus <- sd(all_canopy_clean$CP_percent_200plus, na.rm = T)
  sd_gap25plus <- sd(all_canopy_clean$CP_percent_25plus, na.rm = T)
  sd_gap50plus <- sd(all_canopy_clean$CP_percent_50plus, na.rm = T)
  sd_gap25to50 <- sd(all_canopy_clean$CP_percent_25to50, na.rm = T)
  sd_gap50to100 <- sd(all_canopy_clean$CP_percent_50to100, na.rm = T)
  
  all_canopy_clean <- all_canopy_clean %>%
    # dplyr::filter(!(CP_percent_25plus >= (sd_gap25plus/1 + CA_percent_25plus)) &
    #                 !(CP_percent_100plus >= (sd_gap100plus/1 + CA_percent_100plus)) &   
    #                 !(CP_percent_200plus >= (sd_gap200plus/1 + CA_percent_200plus)) &
    #                 !(CP_percent_100to200 >= (sd_gap100to200/1 + CA_percent_100to200)) &
    #                 !(CP_percent_25to50 >= (sd_gap25to50/1 + CA_percent_25to50)) &
    #                 !(CP_percent_50to100 >= (sd_gap50to100/1 + CA_percent_50to100)))     
    dplyr::filter(!(CA_percent_25plus >= (sd_gap25plus/0.5 + CP_percent_25plus)))
  
  ### NEW 3/27 check instances where there is only p gap on plot, and the crew just copied over to the C gap data ###
  gap_presence <- sf::st_read(dsn = "C:/Users/jrbrehm/Documents/Data/BLM_AIM/AIMTerrestrialEdtBackup5-12-23.gdb",
                              layer = "POINT") %>% dplyr::select(PrimaryKey, 
                                                                 BASAL_GAPS_NESW, CANOPY_GAPS_NESW, PERENNIAL_CANOPY_GAPS_NESW,
                                                                 BASAL_GAPS_NWSE, CANOPY_GAPS_NWSE, PERENNIAL_CANOPY_GAPS_NWSE,
                                                                 GAPS_DIFFERENT_NESW, GAPS_DIFFERENT_NWSE)
  
  uncopied_gap <- gap_presence %>% 
    dplyr::filter((GAPS_DIFFERENT_NESW != "N" | is.na(GAPS_DIFFERENT_NESW)) & 
                    (GAPS_DIFFERENT_NWSE != "N" | is.na(GAPS_DIFFERENT_NWSE)))
  
  
  all_canopy_clean <- all_canopy_clean %>% dplyr::filter(PrimaryKey %in% uncopied_gap$PrimaryKey)
  rm(uncopied_gap, sd_gap100plus, sd_gap100to200, sd_gap200plus, sd_gap25plus, sd_gap25to50, sd_gap50to100, gap_presence)
  
  # box cox transformation requries all response vars to be >0
  # all_canopy_clean[,3:ncol(all_canopy_clean)] <- all_canopy_clean[,3:ncol(all_canopy_clean)] + 1e-10
  # all_canopy_clean[,3:ncol(all_canopy_clean)] <- all_canopy_clean[,3:ncol(all_canopy_clean)] + 1e-10
  
  ### 1.5 save data
  write.csv(all_canopy_clean, file = file.path(path_out, "all_canopy_clean.csv"), row.names = F)
} else {
  all_canopy_clean <- read.csv(file = file.path(path_out, "all_canopy_clean.csv"))
}

colnames(all_canopy_clean)

### 2 Write analysis function ####
fn_makemodels <- function(indata){
  # 
  # if(any(indata <= 0) & doBC){
  #   warning("Negative values found. Can't do the box cox transformation here")
  #   doBC = F
  # }
  # Filter data to remove non-zeroes 
  indata_200plus <- subset(indata, CP_percent_200plus > 0 & CA_percent_200plus > 0)
  indata_100plus <- subset(indata, CP_percent_100plus > 0 & CA_percent_100plus > 0)
  indata_50plus <- subset(indata, CP_percent_50plus > 0 & CA_percent_50plus > 0)
  indata_25plus <- subset(indata, CP_percent_25plus > 0 & CA_percent_25plus > 0)
  indata_100to200 <- subset(indata, CP_percent_100to200 > 0 & CA_percent_100to200 > 0)
  indata_50to100 <- subset(indata, CP_percent_50to100 > 0 & CA_percent_50to100 > 0)
  indata_25to50 <- subset(indata, CP_percent_25to50 > 0 & CA_percent_25to50 > 0)
  indata_mean <- subset(indata, CP_MeanGap > 0 & CA_MeanGap > 0)
  indata_median <- subset(indata, CP_MedianGap > 0 & CA_MedianGap > 0)
  indata_sd <- subset(indata, CP_SDGap > 0 & CA_SDGap > 0)
  
  # Remove validation dataset
  set.seed(0)
  indata_200plus_v <- dplyr::sample_n(indata_200plus, size = ceiling(nrow(indata_200plus) * 0.3))
  indata_200plus_t <- subset(indata_200plus, !(PrimaryKey %in% indata_200plus_v$PrimaryKey))
  
  set.seed(0)
  indata_100plus_v <- sample_n(indata_100plus, size = ceiling(nrow(indata_100plus) * 0.3))
  indata_100plus_t <- subset(indata_100plus, !(PrimaryKey %in% indata_100plus_v$PrimaryKey))
  
  set.seed(0)
  indata_50plus_v <- sample_n(indata_50plus, size = ceiling(nrow(indata_50plus) * 0.3))
  indata_50plus_t <- subset(indata_50plus, !(PrimaryKey %in% indata_50plus_v$PrimaryKey))
  
  set.seed(0)
  indata_25plus_v <- sample_n(indata_25plus, size = ceiling(nrow(indata_25plus) * 0.3))
  indata_25plus_t <- subset(indata_25plus, !(PrimaryKey %in% indata_25plus_v$PrimaryKey))
  
  set.seed(0)
  indata_100to200_v <- sample_n(indata_100to200, size = ceiling(nrow(indata_100to200) * 0.3))
  indata_100to200_t <- subset(indata_100to200, !(PrimaryKey %in% indata_100to200_v$PrimaryKey))
  
  set.seed(0)
  indata_50to100_v <- sample_n(indata_50to100, size = ceiling(nrow(indata_50to100) * 0.3))
  indata_50to100_t <- subset(indata_50to100, !(PrimaryKey %in% indata_50to100_v$PrimaryKey))
  
  set.seed(0)
  indata_25to50_v <- sample_n(indata_25to50, size = ceiling(nrow(indata_25to50) * 0.3))
  indata_25to50_t <- subset(indata_25to50, !(PrimaryKey %in% indata_25to50_v$PrimaryKey))
  
  set.seed(0)
  indata_mean_v <- sample_n(indata_mean, size = ceiling(nrow(indata_mean) * 0.3))
  indata_mean_t <- subset(indata_mean, !(PrimaryKey %in% indata_mean_v$PrimaryKey))
  
  set.seed(0)
  indata_median_v <- sample_n(indata_median, size = ceiling(nrow(indata_median) * 0.3))
  indata_median_t <- subset(indata_median, !(PrimaryKey %in% indata_median_v$PrimaryKey))
  
  set.seed(0)
  indata_sd_v <- sample_n(indata_sd, size = ceiling(nrow(indata_sd) * 0.3))
  indata_sd_t <- subset(indata_sd, !(PrimaryKey %in% indata_sd_v$PrimaryKey))
  
  
  l_indata <- list(indata_200plus_t, indata_100plus_t, indata_50plus_t, indata_25plus_t, indata_100to200_t, indata_50to100_t, indata_25to50_t, indata_mean_t, indata_median_t, indata_sd_t)
  l_indata_v <- list(indata_200plus_v, indata_100plus_v, indata_50plus_v, indata_25plus_v, indata_100to200_v, indata_50to100_v, indata_25to50_v, indata_mean_v, indata_median_v, indata_sd_v)
  
  names(l_indata) <- c("g200plus", "g100plus", "g50plus", "g25plus", "g100to200", "g50to100", "g25to50", "mean", "median", "sd")
  names(l_indata_v) <- c("g200plus", "g100plus", "g50plus", "g25plus", "g100to200", "g50to100", "g25to50", "mean", "median", "sd")
  
  rm(indata_25to50, indata_50to100, indata_50plus, indata_100to200, indata_25plus, indata_100plus, indata_200plus, indata_mean, indata_median, indata_sd)
  
  lm_predict_cp200plus <- lm(CP_percent_200plus ~ CA_percent_200plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_200plus_t)
  lm_predict_cp100plus <- lm(CP_percent_100plus ~ CA_percent_100plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_100plus_t)
  lm_predict_cp50plus <- lm(CP_percent_50plus ~ CA_percent_50plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_50plus_t)
  lm_predict_cp25plus <- lm(CP_percent_25plus ~ CA_percent_25plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_25plus_t)
  lm_predict_cp100to200 <- lm(CP_percent_100to200 ~ CA_percent_100to200 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_100to200_t)
  lm_predict_cp50to100 <- lm(CP_percent_50to100 ~ CA_percent_50to100 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_50to100_t)
  lm_predict_cp25to50 <- lm(CP_percent_25to50 ~ CA_percent_25to50 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_25to50_t)
  lm_predict_cpMean <- lm(CP_MeanGap ~ CA_MeanGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_mean_t)
  lm_predict_cpMedian <- lm(CP_MedianGap ~ CA_MedianGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_median_t)
  lm_predict_cpSD <- lm(CP_SDGap ~ CA_SDGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_sd_t)
  
  ### model set 2, predict canopy gap from perennial gap ##
  lm_predict_ca200plus <- lm(CA_percent_200plus ~ CP_percent_200plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_200plus_t)
  lm_predict_ca100plus <- lm(CA_percent_100plus ~ CP_percent_100plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_100plus_t)
  lm_predict_ca50plus <- lm(CA_percent_50plus ~ CP_percent_50plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_50plus_t)
  lm_predict_ca25plus <- lm(CA_percent_25plus ~ CP_percent_25plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_25plus_t)
  lm_predict_ca100to200 <- lm(CA_percent_100to200 ~ CP_percent_100to200 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_100to200_t)
  lm_predict_ca50to100 <- lm(CA_percent_50to100 ~ CP_percent_50to100 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_50to100_t)
  lm_predict_ca25to50 <- lm(CA_percent_25to50 ~ CP_percent_25to50 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_25to50_t)
  lm_predict_caMean <- lm(CA_MeanGap ~ CP_MeanGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_mean_t)
  lm_predict_caMedian <- lm(CA_MedianGap ~ CP_MedianGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_median_t)
  lm_predict_caSD <- lm(CA_SDGap ~ CP_SDGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = indata_sd_t)
  
  lm_predict_all <- list(lm_predict_cp200plus, lm_predict_cp100plus, 
                         lm_predict_cp50plus, 
                         lm_predict_cp25plus, lm_predict_cp100to200,
                         lm_predict_cp50to100, lm_predict_cp25to50,
                         lm_predict_cpMean, lm_predict_cpMedian, lm_predict_cpSD,
                         
                         lm_predict_ca200plus, lm_predict_ca100plus, 
                         lm_predict_ca50plus,
                         lm_predict_ca25plus, lm_predict_ca100to200,
                         lm_predict_ca50to100, lm_predict_ca25to50,
                         lm_predict_caMean, lm_predict_caMedian, lm_predict_caSD)
  
  names(lm_predict_all) <- c("lm_predict_cp200plus", "lm_predict_cp100plus",
                             "lm_predict_cp50plus", 
                             "lm_predict_cp25plus", "lm_predict_cp100to200",
                             "lm_predict_cp50to100", "lm_predict_cp25to50",
                             "lm_predict_cpMean", "lm_predict_cpMedian", "lm_predict_cpSD",
                             
                             
                             "lm_predict_ca200plus", "lm_predict_ca100plus",
                             "lm_predict_ca50plus", 
                             "lm_predict_ca25plus", "lm_predict_ca100to200",
                             "lm_predict_ca50to100", "lm_predict_ca25to50",
                             "lm_predict_caMean", "lm_predict_caMedian", "lm_predict_caSD")
  
  lm_r2 <- t(as.data.frame(sapply(lm_predict_all, summary)["r.squared",]))
  
  lm_p <- as.data.frame(sapply(lm_predict_all, summary)["coefficients",])
  lm_p <- lm_p[,grepl("\\.Pr\\.\\.\\.t\\.\\.", colnames(lm_p))]
  
  linearity_p <- as.data.frame(sapply(lm_predict_all, function(m){
    coef(summary(m))[,4]
  }))
  
  shapiro_p <- sapply(lm_predict_all, function(m){
    shapiro.test(m$residuals)$p.value
  })
  
  bp_p <- sapply(lm_predict_all, function(m){
    bptest(m)$p.value
  })
  
  out1 <- list(lm_predict_all,
               lm_r2,
               lm_p,
               linearity_p,
               shapiro_p,
               bp_p,
               l_indata,
               l_indata_v)
  names(out1) <- c("models", "model_r2", "model_p", "linearity_p",
                   "shapiro_p", "bp_p", "training_data", "validation_data")
  
  # drop by cooks distance and repeat
  thresh <- 4 / nrow(indata)
  
  l_data_cooked <- c(l_indata, l_indata) # have to duplicate it because the models were run twice. This is used as the input data for the cooked models below
  
  for(n in 1:length(lm_predict_all)){
    m <- lm_predict_all[[n]]
    possible_outliers <- cooks.distance(m) > thresh
    l_data_cooked[[n]] <- c(l_indata, l_indata)[[n]][!possible_outliers,]
  }
  
  lm_predict_cp200plus_cook <- lm(CP_percent_200plus ~ CA_percent_200plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[1]])
  lm_predict_cp100plus_cook <- lm(CP_percent_100plus ~ CA_percent_100plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[2]])
  lm_predict_cp50plus_cook <- lm(CP_percent_50plus ~ CA_percent_50plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[3]])
  lm_predict_cp25plus_cook <- lm(CP_percent_25plus ~ CA_percent_25plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[4]])
  lm_predict_cp100to200_cook <- lm(CP_percent_100to200 ~ CA_percent_100to200 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[5]])
  lm_predict_cp50to100_cook <- lm(CP_percent_50to100 ~ CA_percent_50to100 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[6]])
  lm_predict_cp25to50_cook <- lm(CP_percent_25to50 ~ CA_percent_25to50 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[7]])
  lm_predict_cpMean_cook <- lm(CP_MeanGap ~ CA_MeanGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[8]])
  lm_predict_cpMedian_cook <- lm(CP_MedianGap ~ CA_MedianGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[9]])
  lm_predict_cpSD_cook <- lm(CP_SDGap ~ CA_SDGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[10]])
  
  ### model set 2, predict canopy gap from perennial gap ##
  lm_predict_ca200plus_cook <- lm(CA_percent_200plus ~ CP_percent_200plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[11]])
  lm_predict_ca100plus_cook <- lm(CA_percent_100plus ~ CP_percent_100plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[12]])
  lm_predict_ca50plus_cook <- lm(CA_percent_50plus ~ CP_percent_50plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[13]])
  lm_predict_ca25plus_cook <- lm(CA_percent_25plus ~ CP_percent_25plus + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[14]])
  lm_predict_ca100to200_cook <- lm(CA_percent_100to200 ~ CP_percent_100to200 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[15]])
  lm_predict_ca50to100_cook <- lm(CA_percent_50to100 ~ CP_percent_50to100 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[16]])
  lm_predict_ca25to50_cook <- lm(CA_percent_25to50 ~ CP_percent_25to50 + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[17]])
  lm_predict_caMean_cook <- lm(CA_MeanGap ~ CP_MeanGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[18]])
  lm_predict_caMedian_cook <- lm(CA_MedianGap ~ CP_MedianGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[19]])
  lm_predict_caSD_cook <- lm(CA_SDGap ~ CP_SDGap + AH_AnnGrassCover + AH_PerenGrassCover + AH_ShrubCover + AH_ForbCover, data = l_data_cooked[[20]])
  
  lm_predict_all_cook <- list(lm_predict_cp200plus_cook, lm_predict_cp100plus_cook,
                              lm_predict_cp50plus_cook,
                              lm_predict_cp25plus_cook, lm_predict_cp100to200_cook,
                              lm_predict_cp50to100_cook, lm_predict_cp25to50_cook,
                              lm_predict_cpMean_cook, lm_predict_cpMedian_cook, lm_predict_cpSD_cook,
                              
                              lm_predict_ca200plus_cook, lm_predict_ca100plus_cook,
                              lm_predict_ca50plus_cook,
                              lm_predict_ca25plus_cook, lm_predict_ca100to200_cook,
                              lm_predict_ca50to100_cook, lm_predict_ca25to50_cook,
                              lm_predict_caMean_cook, lm_predict_caMedian_cook, lm_predict_caSD_cook)
  
  names(lm_predict_all_cook) <- c("lm_predict_cp200plus_cook", "lm_predict_cp100plus_cook", 
                                  "lm_predict_cp50plus_cook",
                                  "lm_predict_cp25plus_cook", "lm_predict_cp100to200_cook",
                                  "lm_predict_cp50to100_cook", "lm_predict_cp25to50_cook",
                                  "lm_predict_cpMean_cook", "lm_predict_cpMedian_cook", "lm_predict_cpSD_cook",
                                  
                                  "lm_predict_ca200plus_cook", "lm_predict_ca100plus_cook", 
                                  "lm_predict_ca50plus_cook",
                                  "lm_predict_ca25plus_cook", "lm_predict_ca100to200_cook",
                                  "lm_predict_ca50to100_cook", "lm_predict_ca25to50_cook",
                                  "lm_predict_caMean_cook", "lm_predict_caMedian_cook", "lm_predict_caSD_cook")
  
  names(l_data_cooked) <- names(lm_predict_all_cook)
  
  lm_r2_cook <- t(as.data.frame(sapply(lm_predict_all_cook, summary)["r.squared",]))
  
  lm_p_cook <- as.data.frame(sapply(lm_predict_all_cook, summary)["coefficients",])
  lm_p_cook <- lm_p_cook[,grepl("\\.Pr\\.\\.\\.t\\.\\.", colnames(lm_p_cook))]
  
  linearity_p_cook <- as.data.frame(sapply(lm_predict_all_cook, function(m){
    coef(summary(m))[,4]
  }))
  
  shapiro_p_cook <- sapply(lm_predict_all_cook, function(m){
    shapiro.test(m$residuals)$p.value
  })
  
  bp_p_cook <- sapply(lm_predict_all_cook, function(m){
    bptest(m)$p.value
  })
  
  
  ### validate model using reserved dataset
  # http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
  
  l_indata_v$g200plus$predict_cp <- predict(lm_predict_all_cook$lm_predict_cp200plus_cook, l_indata_v$g200plus)
  l_indata_v$g100plus$predict_cp <- predict(lm_predict_all_cook$lm_predict_cp100plus_cook, l_indata_v$g100plus)
  l_indata_v$g50plus$predict_cp <- predict(lm_predict_all_cook$lm_predict_cp50plus_cook, l_indata_v$g50plus)
  l_indata_v$g25plus$predict_cp <- predict(lm_predict_all_cook$lm_predict_cp25plus_cook, l_indata_v$g25plus)
  l_indata_v$g100to200$predict_cp <- predict(lm_predict_all_cook$lm_predict_cp100to200_cook, l_indata_v$g100to200)
  l_indata_v$g50to100$predict_cp <- predict(lm_predict_all_cook$lm_predict_cp50to100_cook, l_indata_v$g50to100)
  l_indata_v$g25to50$predict_cp <- predict(lm_predict_all_cook$lm_predict_cp25to50_cook, l_indata_v$g25to50)
  l_indata_v$mean$predict_cp <- predict(lm_predict_all_cook$lm_predict_cpMean_cook, l_indata_v$mean)
  l_indata_v$median$predict_cp <- predict(lm_predict_all_cook$lm_predict_cpMedian_cook, l_indata_v$median)
  l_indata_v$sd$predict_cp <- predict(lm_predict_all_cook$lm_predict_cpSD_cook, l_indata_v$sd)
  
  l_indata_v$g200plus$predict_ca <- predict(lm_predict_all_cook$lm_predict_ca200plus_cook, l_indata_v$g200plus)
  l_indata_v$g100plus$predict_ca <- predict(lm_predict_all_cook$lm_predict_ca100plus_cook, l_indata_v$g100plus)
  l_indata_v$g50plus$predict_ca <- predict(lm_predict_all_cook$lm_predict_ca50plus_cook, l_indata_v$g50plus)
  l_indata_v$g25plus$predict_ca <- predict(lm_predict_all_cook$lm_predict_ca25plus_cook, l_indata_v$g25plus)
  l_indata_v$g100to200$predict_ca <- predict(lm_predict_all_cook$lm_predict_ca100to200_cook, l_indata_v$g100to200)
  l_indata_v$g50to100$predict_ca <- predict(lm_predict_all_cook$lm_predict_ca50to100_cook, l_indata_v$g50to100)
  l_indata_v$g25to50$predict_ca <- predict(lm_predict_all_cook$lm_predict_ca25to50_cook, l_indata_v$g25to50)
  l_indata_v$mean$predict_ca <- predict(lm_predict_all_cook$lm_predict_caMean_cook, l_indata_v$mean)
  l_indata_v$median$predict_ca <- predict(lm_predict_all_cook$lm_predict_caMedian_cook, l_indata_v$median)
  l_indata_v$sd$predict_ca <- predict(lm_predict_all_cook$lm_predict_caSD_cook, l_indata_v$sd)
  
  validation_summary <- data.frame(
    r2_predict = c(caret::R2(l_indata_v$g200plus$predict_cp, l_indata_v$g200plus$CP_percent_200plus),
                   caret::R2(l_indata_v$g100plus$predict_cp, l_indata_v$g100plus$CP_percent_100plus),
                   caret::R2(l_indata_v$g50plus$predict_cp, l_indata_v$g50plus$CP_percent_50plus),
                   caret::R2(l_indata_v$g25plus$predict_cp, l_indata_v$g25plus$CP_percent_25plus),
                   caret::R2(l_indata_v$g100to200$predict_cp, l_indata_v$g100to200$CP_percent_100to200),
                   caret::R2(l_indata_v$g50to100$predict_cp, l_indata_v$g50to100$CP_percent_50to100),
                   caret::R2(l_indata_v$g25to50$predict_cp, l_indata_v$g25to50$CP_percent_25to50),
                   caret::R2(l_indata_v$mean$predict_cp, l_indata_v$mean$CP_MeanGap),
                   caret::R2(l_indata_v$median$predict_cp, l_indata_v$median$CP_MedianGap),
                   caret::R2(l_indata_v$sd$predict_cp, l_indata_v$sd$CP_SDGap),
                   
                   caret::R2(l_indata_v$g200plus$predict_ca, l_indata_v$g200plus$CA_percent_200plus),
                   caret::R2(l_indata_v$g100plus$predict_ca, l_indata_v$g100plus$CA_percent_100plus),
                   caret::R2(l_indata_v$g50plus$predict_ca, l_indata_v$g50plus$CA_percent_50plus),
                   caret::R2(l_indata_v$g25plus$predict_ca, l_indata_v$g25plus$CA_percent_25plus),
                   caret::R2(l_indata_v$g100to200$predict_ca, l_indata_v$g100to200$CA_percent_100to200),
                   caret::R2(l_indata_v$g50to100$predict_ca, l_indata_v$g50to100$CA_percent_50to100),
                   caret::R2(l_indata_v$g25to50$predict_ca, l_indata_v$g25to50$CA_percent_25to50),
                   caret::R2(l_indata_v$mean$predict_ca, l_indata_v$mean$CA_MeanGap),
                   caret::R2(l_indata_v$median$predict_ca, l_indata_v$median$CA_MedianGap),
                   caret::R2(l_indata_v$sd$predict_ca, l_indata_v$sd$CA_SDGap)),
    rmse_predict = c(caret::RMSE(l_indata_v$g200plus$predict_cp, l_indata_v$g200plus$CP_percent_200plus),
                     caret::RMSE(l_indata_v$g100plus$predict_cp, l_indata_v$g100plus$CP_percent_100plus),
                     caret::RMSE(l_indata_v$g50plus$predict_cp, l_indata_v$g50plus$CP_percent_50plus),
                     caret::RMSE(l_indata_v$g25plus$predict_cp, l_indata_v$g25plus$CP_percent_25plus),
                     caret::RMSE(l_indata_v$g100to200$predict_cp, l_indata_v$g100to200$CP_percent_100to200),
                     caret::RMSE(l_indata_v$g50to100$predict_cp, l_indata_v$g50to100$CP_percent_50to100),
                     caret::RMSE(l_indata_v$g25to50$predict_cp, l_indata_v$g25to50$CP_percent_25to50),
                     caret::RMSE(l_indata_v$mean$predict_cp, l_indata_v$mean$CP_MeanGap),
                     caret::RMSE(l_indata_v$median$predict_cp, l_indata_v$median$CP_MedianGap),
                     caret::RMSE(l_indata_v$sd$predict_cp, l_indata_v$sd$CP_SDGap),
                     
                     caret::RMSE(l_indata_v$g200plus$predict_ca, l_indata_v$g200plus$CA_percent_200plus),
                     caret::RMSE(l_indata_v$g100plus$predict_ca, l_indata_v$g100plus$CA_percent_100plus),
                     caret::RMSE(l_indata_v$g50plus$predict_ca, l_indata_v$g50plus$CA_percent_50plus),
                     caret::RMSE(l_indata_v$g25plus$predict_ca, l_indata_v$g25plus$CA_percent_25plus),
                     caret::RMSE(l_indata_v$g100to200$predict_ca, l_indata_v$g100to200$CA_percent_100to200),
                     caret::RMSE(l_indata_v$g50to100$predict_ca, l_indata_v$g50to100$CA_percent_50to100),
                     caret::RMSE(l_indata_v$g25to50$predict_ca, l_indata_v$g25to50$CA_percent_25to50),
                     caret::RMSE(l_indata_v$mean$predict_ca, l_indata_v$mean$CA_MeanGap),
                     caret::RMSE(l_indata_v$median$predict_ca, l_indata_v$median$CA_MedianGap),
                     caret::RMSE(l_indata_v$sd$predict_ca, l_indata_v$sd$CA_SDGap)),
    per_predict = c(caret::RMSE(l_indata_v$g200plus$predict_cp, l_indata_v$g200plus$CP_percent_200plus)/mean(l_indata_v$g200plus$CP_percent_200plus),
                    caret::RMSE(l_indata_v$g100plus$predict_cp, l_indata_v$g100plus$CP_percent_100plus)/mean(l_indata_v$g100plus$CP_percent_100plus),
                    caret::RMSE(l_indata_v$g50plus$predict_cp, l_indata_v$g50plus$CP_percent_50plus)/mean(l_indata_v$g50plus$CP_percent_50plus),
                    caret::RMSE(l_indata_v$g25plus$predict_cp, l_indata_v$g25plus$CP_percent_25plus)/mean(l_indata_v$g25plus$CP_percent_25plus),
                    caret::RMSE(l_indata_v$g100to200$predict_cp, l_indata_v$g100to200$CP_percent_100to200)/mean(l_indata_v$g100to200$CP_percent_100to200),
                    caret::RMSE(l_indata_v$g50to100$predict_cp, l_indata_v$g50to100$CP_percent_50to100)/mean(l_indata_v$g50to100$CP_percent_50to100),
                    caret::RMSE(l_indata_v$g25to50$predict_cp, l_indata_v$g25to50$CP_percent_25to50)/mean(l_indata_v$g25to50$CP_percent_25to50),
                    caret::RMSE(l_indata_v$mean$predict_cp, l_indata_v$mean$CP_MeanGap)/mean(l_indata_v$mean$CP_MeanGap),
                    caret::RMSE(l_indata_v$median$predict_cp, l_indata_v$median$CP_MedianGap)/mean(l_indata_v$median$CP_MedianGap),
                    caret::RMSE(l_indata_v$sd$predict_cp, l_indata_v$sd$CP_SDGap)/mean(l_indata_v$sd$CP_SDGap),
                    
                    caret::RMSE(l_indata_v$g200plus$predict_ca, l_indata_v$g200plus$CA_percent_200plus)/mean(l_indata_v$g200plus$CA_percent_200plus),
                    caret::RMSE(l_indata_v$g100plus$predict_ca, l_indata_v$g100plus$CA_percent_100plus)/mean(l_indata_v$g100plus$CA_percent_100plus),
                    caret::RMSE(l_indata_v$g50plus$predict_ca, l_indata_v$g50plus$CA_percent_50plus)/mean(l_indata_v$g50plus$CA_percent_50plus),
                    caret::RMSE(l_indata_v$g25plus$predict_ca, l_indata_v$g25plus$CA_percent_25plus)/mean(l_indata_v$g25plus$CA_percent_25plus),
                    caret::RMSE(l_indata_v$g100to200$predict_ca, l_indata_v$g100to200$CA_percent_100to200)/mean(l_indata_v$g100to200$CA_percent_100to200),
                    caret::RMSE(l_indata_v$g50to100$predict_ca, l_indata_v$g50to100$CA_percent_50to100)/mean(l_indata_v$g50to100$CA_percent_50to100),
                    caret::RMSE(l_indata_v$g25to50$predict_ca, l_indata_v$g25to50$CA_percent_25to50)/mean(l_indata_v$g25to50$CA_percent_25to50),
                    caret::RMSE(l_indata_v$mean$predict_ca, l_indata_v$mean$CA_MeanGap)/mean(l_indata_v$mean$CA_MeanGap),
                    caret::RMSE(l_indata_v$median$predict_ca, l_indata_v$median$CA_MedianGap)/mean(l_indata_v$median$CA_MedianGap),
                    caret::RMSE(l_indata_v$sd$predict_ca, l_indata_v$sd$CA_SDGap)/mean(l_indata_v$sd$CA_SDGap)
    ),
    
    model = names(l_data_cooked))
  
  out2 <- list(
    l_data_cooked,
    lm_predict_all_cook,
    lm_r2_cook,
    lm_p_cook,
    linearity_p_cook,
    shapiro_p_cook,
    bp_p_cook,
    validation_summary)
  names(out2) <- c("data_cooked", "models_cook", "model_r2_cook", "model_p_cook", "linearity_p_cook",
                   "shapiro_p_cook", "bp_p_cook", "validation_summary_cook")
  
  out <- c(out1, out2)
  
  return(out)
}

### 3 Apply analysis function ####
l_models_notransform <- fn_makemodels(indata = all_canopy_clean)

l_models_notransform$validation_summary_cook

### 4 Transform data ####
all_canopy_clean_sqrt <- all_canopy_clean
all_canopy_clean_sqrt[,3:ncol(all_canopy_clean)] <- sqrt(all_canopy_clean[,3:ncol(all_canopy_clean)])

all_canopy_clean_log <- all_canopy_clean
all_canopy_clean_log[,3:ncol(all_canopy_clean)] <- log(all_canopy_clean[,3:ncol(all_canopy_clean)])
all_canopy_clean_log[all_canopy_clean_log == -Inf] <- 0 #### TO DO IS THIS APPROPRIATE

all_canopy_clean_sq <- all_canopy_clean
all_canopy_clean_sq[,3:ncol(all_canopy_clean)] <- (all_canopy_clean[,3:ncol(all_canopy_clean)])^2

l_models_log <- fn_makemodels(indata = all_canopy_clean_log)
l_models_sqrt <- fn_makemodels(indata = all_canopy_clean_sqrt)
l_models_sq <- fn_makemodels(indata = all_canopy_clean_sq)


### Get Model Summaries ####
packup <- function(indata, doCook = T, bc = F){
  if(bc){
    out <- cbind(apply(indata$linearity_p_bc < 0.05, 2,  sum) / 6,
                 indata$shapiro_p_bc) %>%
      cbind(indata$bp_p_bc) %>%
      cbind(indata$model_r2_bc)
    colnames(out) <- c("pct_of_vars_linear_bc", "shapiro_p_bc", "breusch-pagan_p_bc", "r2_bc")
    out <- as.data.frame(out)
  } else {
    out <- cbind(apply(indata$linearity_p < 0.05, 2,  sum) / 6,
                 indata$shapiro_p) %>%
      cbind(indata$bp_p) %>%
      cbind(indata$model_r2)
    colnames(out) <- c("pct_of_vars_linear", "shapiro_p", "breusch-pagan_p", "r2")
    out <- as.data.frame(out)
  }
  
  if(doCook){
    out_cook <- cbind(apply(indata$linearity_p_cook < 0.05, 2,  sum) / 6,
                      indata$shapiro_p_cook) %>%
      cbind(indata$bp_p_cook) %>%
      cbind(indata$model_r2_cook) %>% 
      cbind(indata$validation_summary_cook %>% dplyr::select(r2_predict, rmse_predict, per_predict))
    colnames(out_cook) <- c("pct_of_vars_linear_cook", "shapiro_p_cook", "breusch-pagan_p_cook", "r2_cook", "r2_validation", "rmse_validation", "per_validation")
    out_cook <- as.data.frame(out_cook)
    return(cbind(out, out_cook))
  } else {
    return(out)
  }
}


modelsummary_notransform <- packup(l_models_notransform)
modelsummary_log <- packup(l_models_log)
modelsummary_sq <- packup(l_models_sq)
modelsummary_sqrt <- packup(l_models_sqrt)

modelsummary_notransform$name <- paste0(rownames(modelsummary_notransform), "_NT")
modelsummary_sq$name <- paste0(rownames(modelsummary_sq), "_SQ")
modelsummary_sqrt$name <- paste0(rownames(modelsummary_sqrt), "_SQRT")
modelsummary_log$name <- paste0(rownames(modelsummary_log), "_LOG")

modelsummary_nobc <- bind_rows(list(modelsummary_notransform,
                                    modelsummary_log,
                                    modelsummary_sq,
                                    modelsummary_sqrt))

# modelsummary_all <- full_join(modelsummary_nobc, modelsummary_bc, by = "name")
modelsummary_all <- modelsummary_nobc

rownames(modelsummary_all) <- NULL

vec_modelcodes <- sapply(strsplit(modelsummary_all$name, split = "_"), function(x) x[3])
modelsummary_all$transformation <- sapply(strsplit(modelsummary_all$name, split = "_"), function(x) x[4]) %>%
  dplyr::recode("NT" = "No transformation",
                "SQ" = "Squared",
                "SQRT" = "Square root",
                "LOG" = "Log")


modelsummary_all$direction <- 
  substr(vec_modelcodes, 1,2) %>% 
  dplyr::recode("cp" = "C to P",
                "ca" = "P to C")

modelsummary_all$gap_measure <- substr(vec_modelcodes, 3, 10)

modelsummary_all$direction <- 
  substr(vec_modelcodes, 1,2) %>% 
  dplyr::recode("cp" = "C to P",
                "ca" = "P to C")


modelsummary_all <- 
  modelsummary_all[c("name", "direction", "gap_measure", "transformation", 
                     colnames(modelsummary_all)[!colnames(modelsummary_all) %in% c("name", "direction", "gap_measure", "transformation")])]

modelsummary_all

write.csv(modelsummary_all, paste0(path_out, "/model_summary_", Sys.Date(), ".csv"), row.names = F)

### Output: equation text, model summary stats, and graphs ####
write_equation <- function(m){
  xvar = names(m$model)[2]
  yvar = names(m$model)[1]
  
  measurename = strsplit(xvar, "_")[[1]][length(strsplit(xvar, "_")[[1]])] %>% 
    recode(
      "200plus" = "> 200cm",
      "100plus" = "> 100cm",
      "50plus" = "> 50cm",
      "25plus" = "> 25cm",
      "100to200" = "100-200cm",
      "50to100" = "50-100cm",
      "25to50" = "25-50cm",
      "MeanGap" = "mean gap size",
      "MedianGap" = "median gap size",
      "SDGap" = "std dev of gap size")
  
  gapname.x = paste(strsplit(xvar, "_")[[1]][1] %>% recode("CA" = "AllGap", "CP" = "PerenGap"), measurename)
  gapname.y = paste(strsplit(yvar, "_")[[1]][1] %>% recode("CA" = "AllGap", "CP" = "PerenGap"), measurename)
  
  coefficients <- round(m$coefficients, 2)
  print(paste0("(", gapname.y, ") = ",
               coefficients[1], " + ",
               coefficients[2], "*(", gapname.x, ") + ",
               coefficients[3], "*AG + ",
               coefficients[4], "*PG + ",
               coefficients[5], "*SH + ",
               coefficients[6], "*F")
  )
}

write_equation(l_models_notransform$models_cook$lm_predict_ca200plus_cook)
write_equation(l_models_notransform$models_cook$lm_predict_ca100plus_cook)
write_equation(l_models_notransform$models_cook$lm_predict_ca50plus_cook)
write_equation(l_models_notransform$models_cook$lm_predict_ca25plus_cook)

write_equation(l_models_notransform$models_cook$lm_predict_cp200plus_cook)
write_equation(l_models_notransform$models_cook$lm_predict_cp100plus_cook)
write_equation(l_models_notransform$models_cook$lm_predict_cp50plus_cook)
write_equation(l_models_notransform$models_cook$lm_predict_cp25plus_cook)

modelsummary_chosenfew <- subset(modelsummary_all,
                                 (gap_measure %in% c("25plus", "50plus", "100plus", "200plus") & 
                                    transformation == "No transformation") | (gap_measure %in% c("Mean", "Median", "SD") & transformation == "Log")) %>%
  dplyr::select(
    Direction = direction,
    Measure = gap_measure,
    `Shapiro-Wilks p` = shapiro_p_cook,
    `Breusch-Pagan p` = `breusch-pagan_p_cook`,
    R2 = r2_cook,
    `Validation R2` = r2_validation,
    `Validation RMSE` = rmse_validation
  ) %>%
  dplyr::mutate(
    `Shapiro-Wilks p` = gsub("^0$", "<0.001", as.character(round(`Shapiro-Wilks p`, 3))),
    `Breusch-Pagan p` = gsub("^0$", "<0.001", as.character(round(`Breusch-Pagan p`, 3))),
    R2 = round(R2, 2),
    `Validation R2` = round(`Validation R2`, 2),
    `Validation RMSE` = round(`Validation RMSE`, 2),
    Measure = gsub("plus", " plus", Measure)
  ) 

modelsummary_chosenfew$Equation <- 
  c(write_equation(l_models_notransform$models_cook$lm_predict_cp200plus_cook),
    write_equation(l_models_notransform$models_cook$lm_predict_cp100plus_cook),
    write_equation(l_models_notransform$models_cook$lm_predict_cp50plus_cook),
    write_equation(l_models_notransform$models_cook$lm_predict_cp25plus_cook),
    write_equation(l_models_notransform$models_cook$lm_predict_ca200plus_cook),
    write_equation(l_models_notransform$models_cook$lm_predict_ca100plus_cook),
    write_equation(l_models_notransform$models_cook$lm_predict_ca50plus_cook),
    write_equation(l_models_notransform$models_cook$lm_predict_ca25plus_cook),
    write_equation(l_models_log$models_cook$lm_predict_cpMean_cook),
    write_equation(l_models_log$models_cook$lm_predict_cpMedian_cook),
    write_equation(l_models_log$models_cook$lm_predict_cpSD_cook),
    write_equation(l_models_log$models_cook$lm_predict_caMean_cook),
    write_equation(l_models_log$models_cook$lm_predict_caMedian_cook),
    write_equation(l_models_log$models_cook$lm_predict_caSD_cook)
  )

write.csv(modelsummary_chosenfew, file.path(path_out, paste0("selectedmodels_", Sys.Date(), ".csv")), row.names = F)

## Get predictions
all_canopy_forplot <- all_canopy_clean_sqrt

all_canopy_forplot$predCP_200plus <- predict(l_models_sqrt$models_cook$lm_predict_cp200plus, all_canopy_clean_sqrt)
all_canopy_forplot$predCP_100plus <- predict(l_models_sqrt$models_cook$lm_predict_cp100plus, all_canopy_clean_sqrt)
all_canopy_forplot$predCP_50plus <- predict(l_models_sqrt$models_cook$lm_predict_cp50plus, all_canopy_clean_sqrt)
all_canopy_forplot$predCP_25plus <- predict(l_models_sqrt$models_cook$lm_predict_cp25plus, all_canopy_clean_sqrt)
all_canopy_forplot$predCA_200plus <- predict(l_models_sqrt$models_cook$lm_predict_ca200plus, all_canopy_clean_sqrt)
all_canopy_forplot$predCA_100plus <- predict(l_models_sqrt$models_cook$lm_predict_ca100plus, all_canopy_clean_sqrt)
all_canopy_forplot$predCA_50plus <- predict(l_models_sqrt$models_cook$lm_predict_ca50plus, all_canopy_clean_sqrt)
all_canopy_forplot$predCA_25plus <- predict(l_models_sqrt$models_cook$lm_predict_ca25plus, all_canopy_clean_sqrt)

all_canopy_forplot_backtransform <- all_canopy_forplot


predCP_200plus <- data.frame(PrimaryKey = l_models_notransform$validation_data$g200plus$PrimaryKey,
                             predCP_200plus = predict(l_models_notransform$models_cook$lm_predict_cp200plus_cook, 
                                                      l_models_notransform$validation_data$g200plus),
                             CA_percent_200plus = l_models_notransform$validation_data$g200plus$CA_percent_200plus)
predCP_100plus <- data.frame(PrimaryKey = l_models_notransform$validation_data$g100plus$PrimaryKey,
                             predCP_100plus = predict(l_models_notransform$models_cook$lm_predict_cp100plus_cook, 
                                                      l_models_notransform$validation_data$g100plus),
                             CA_percent_100plus = l_models_notransform$validation_data$g100plus$CA_percent_100plus)
predCP_50plus <- data.frame(PrimaryKey = l_models_notransform$validation_data$g50plus$PrimaryKey,
                            predCP_50plus = predict(l_models_notransform$models_cook$lm_predict_cp50plus_cook, 
                                                    l_models_notransform$validation_data$g50plus),
                            CA_percent_50plus = l_models_notransform$validation_data$g50plus$CA_percent_50plus)
predCP_25plus <- data.frame(PrimaryKey = l_models_notransform$validation_data$g25plus$PrimaryKey,
                            predCP_25plus = predict(l_models_notransform$models_cook$lm_predict_cp25plus_cook, 
                                                    l_models_notransform$validation_data$g25plus),
                            CA_percent_25plus = l_models_notransform$validation_data$g25plus$CA_percent_25plus)

predCP_mean <- data.frame(PrimaryKey = l_models_log$validation_data$mean$PrimaryKey,
                          predCP_mean = predict(l_models_log$models_cook$lm_predict_cpMean_cook, 
                                                l_models_log$validation_data$mean),
                          CA_MeanGap = l_models_log$validation_data$mean$CA_MeanGap)
predCP_median <- data.frame(PrimaryKey = l_models_log$validation_data$median$PrimaryKey,
                            predCP_median = predict(l_models_log$models_cook$lm_predict_cpMedian_cook, 
                                                    l_models_log$validation_data$median),
                            CA_MedianGap = l_models_log$validation_data$mean$CA_MedianGap)
predCP_sd <- data.frame(PrimaryKey = l_models_log$validation_data$sd$PrimaryKey,
                        predCP_SD = predict(l_models_log$models_cook$lm_predict_cpSD_cook, 
                                            l_models_log$validation_data$sd),
                        CA_SDGap = l_models_log$validation_data$sd$CA_SDGap)

predCP_all <- full_join(predCP_200plus, predCP_100plus) %>% full_join(predCP_50plus) %>% full_join(predCP_25plus) %>%
  full_join(predCP_mean) %>% full_join(predCP_median) %>% full_join(predCP_sd)

predCA_200plus <- data.frame(PrimaryKey = l_models_notransform$validation_data$g200plus$PrimaryKey,
                             predCA_200plus = predict(l_models_notransform$models_cook$lm_predict_ca200plus_cook, 
                                                      l_models_notransform$validation_data$g200plus),
                             CP_percent_200plus = l_models_notransform$validation_data$g200plus$CP_percent_200plus)
predCA_100plus <- data.frame(PrimaryKey = l_models_notransform$validation_data$g100plus$PrimaryKey,
                             predCA_100plus = predict(l_models_notransform$models_cook$lm_predict_ca100plus_cook, 
                                                      l_models_notransform$validation_data$g100plus),
                             CP_percent_100plus = l_models_notransform$validation_data$g100plus$CP_percent_100plus)
predCA_50plus <- data.frame(PrimaryKey = l_models_notransform$validation_data$g50plus$PrimaryKey,
                            predCA_50plus = predict(l_models_notransform$models_cook$lm_predict_ca50plus_cook, 
                                                    l_models_notransform$validation_data$g50plus),
                            CP_percent_50plus = l_models_notransform$validation_data$g50plus$CP_percent_50plus)
predCA_25plus <- data.frame(PrimaryKey = l_models_notransform$validation_data$g25plus$PrimaryKey,
                            predCA_25plus = predict(l_models_notransform$models_cook$lm_predict_ca25plus_cook, 
                                                    l_models_notransform$validation_data$g25plus),
                            CP_percent_25plus = l_models_notransform$validation_data$g25plus$CP_percent_25plus)

predCA_mean <- data.frame(PrimaryKey = l_models_log$validation_data$mean$PrimaryKey,
                          predCA_mean = predict(l_models_log$models_cook$lm_predict_caMean_cook, 
                                                l_models_log$validation_data$mean),
                          CP_MeanGap = l_models_log$validation_data$mean$CP_MeanGap)
predCA_median <- data.frame(PrimaryKey = l_models_log$validation_data$median$PrimaryKey,
                            predCA_median = predict(l_models_log$models_cook$lm_predict_caMedian_cook, 
                                                    l_models_log$validation_data$median),
                            CP_MedianGap = l_models_log$validation_data$mean$CP_MedianGap)
predCA_sd <- data.frame(PrimaryKey = l_models_log$validation_data$sd$PrimaryKey,
                        predCA_SD = predict(l_models_log$models_cook$lm_predict_caSD_cook, 
                                            l_models_log$validation_data$sd),
                        CP_SDGap = l_models_log$validation_data$sd$CP_SDGap)



predCA_all <- full_join(predCA_200plus, predCA_100plus) %>% full_join(predCA_50plus) %>% full_join(predCA_25plus) %>%
  full_join(predCA_mean) %>% full_join(predCA_median) %>% full_join(predCA_sd)

pred_all <- full_join(predCA_all, predCP_all)

## graphing function
ggplotRegression2 <- function (data, xvar, yvar, r2) {
  # Drop 0 values, they were excluded from the model
  data[data[,xvar] == 0 | is.na(data[,xvar]),xvar] <- NA
  data[data[,yvar] == 0 | is.na(data[,yvar]),yvar] <- NA
  
  # Drop NA values
  data <- drop_na(data[,c(xvar, yvar)])
  
  gapname.x = strsplit(xvar, "_")[[1]][1] %>% recode("CA" = "All canopy gap", "CP" = "Perennial canopy gap")
  gapname.y = strsplit(yvar, "_")[[1]][1] %>% recode("predCA" = "Predicted all canopy gap", "predCP" = "Predicted perennial canopy gap", "CA" = "All canopy gap", "CP" = "Perennial canopy gap")
  measurename = strsplit(xvar, "_")[[1]][length(strsplit(xvar, "_")[[1]])] %>% 
    recode(
      "200plus" = "> 200cm",
      "100plus" = "> 100cm",
      "50plus" = "> 50cm",
      "25plus" = "> 25cm",
      "100to200" = "100-200cm",
      "50to100" = "50-100cm",
      "25to50" = "25-50cm",
      "MeanGap" = "mean gap size",
      "MedianGap" = "median gap size",
      "SDGap" = "std dev of gap size")
  
  outtitle = paste0(gsub(">", "plus", paste0(gapname.y, " From ", gapname.x, "_", measurename)), "_", Sys.Date(), ".png")
  
  if(r2 == "auto"){
    fit <- lm(data[,yvar] ~ data[,xvar])
    r2 <- round(summary(fit)$r.squared, digits = 2)
  }
  
  if(!is.null(r2)){
    r2label = paste0("R^2 == ", r2)
  } else {
    r2label = NULL
  }
  
  p <- ggplot(data, aes_string(x = xvar, y = yvar)) + 
    geom_hex() +
    scale_fill_viridis_c(limits = c(1,15)) +
    stat_smooth(method = "lm", col = "red", se = TRUE, fill = "white", alpha = 0.75, size = 0.5) +
    xlab(paste(gapname.x, measurename, "(%)")) + ylab(paste(gapname.y, measurename, "(%)")) +
    xlim(0, 100) + ylim(0, 100) + 
    annotate("text", 10, 100, label = r2label, size = 4, parse = T) +
    theme(text = element_text(size = 12))
  
  return(p)
}

## Make graphs
p9.1 <- ggplotRegression2(data = pred_all, xvar = "CP_percent_25plus", yvar = "predCP_25plus", 
                          r2 = "auto")
p10.1 <- ggplotRegression2(data = pred_all, xvar = "CP_percent_50plus", yvar = "predCP_50plus", 
                           r2 = "auto")
p11.1 <- ggplotRegression2(data = pred_all, xvar = "CP_percent_100plus", yvar = "predCP_100plus", 
                           r2 = "auto")
p12.1 <- ggplotRegression2(data = pred_all, xvar = "CP_percent_200plus", yvar = "predCP_200plus", 
                           r2 = "auto")
g5 <- lemon::grid_arrange_shared_legend(p9.1, p10.1, p11.1, p12.1, ncol = 2, nrow = 2)#, 

p13.1 <- ggplotRegression2(data = all_canopy_clean, xvar = "CP_percent_25plus", yvar = "CA_percent_25plus", 
                           r2 = "auto")
p14.1 <- ggplotRegression2(data = all_canopy_clean, xvar = "CP_percent_50plus", yvar = "CA_percent_50plus", 
                           r2 = "auto")
p15.1 <- ggplotRegression2(data = all_canopy_clean, xvar = "CP_percent_100plus", yvar = "CA_percent_100plus", 
                           r2 = "auto")
p16.1 <- ggplotRegression2(data = all_canopy_clean, xvar = "CP_percent_200plus", yvar = "CA_percent_200plus", 
                           r2 = "auto")
g6 <- lemon::grid_arrange_shared_legend(p13.1, p14.1, p15.1, p16.1, ncol = 2, nrow = 2)#, 

## g5 = fig 6b
ggsave(plot = g5, filename = file.path(path_out, paste0("Predicted Perennial and Observed Perennial_", Sys.Date(), ".tiff")),
       height = 190,
       width = 190,
       units = "mm",
       dpi = 500)
## g6 = fig 6a
ggsave(plot = g6, filename = file.path(path_out, paste0("Observed Canopy and Observed Perennial_", Sys.Date(), ".tiff")),
       height = 190,
       width = 190,
       units = "mm",
       dpi = 500)
