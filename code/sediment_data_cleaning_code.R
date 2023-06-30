setwd("C:/Users/19106/Dropbox (Personal)/BOEM_data/Papers/data/sediment")
#install.packages(c("tidyverse", "lubridate", "janitor", "readxl"))
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
data <- data_raw %>%
  mutate(
    topo = case_when(
      topo == 1 ~ 'Ridge',
      TRUE ~ 'Swale'),
    region = case_when(
      region == 1 ~ 'Bull',
      region == 2 ~ 'Chester',
      region == 3 ~ 'Dredge N',
      region == 4 ~ 'Dredge S'),
    site = str_replace(data_raw$murie_id, '-','')
  )

# read in code list
new_code = read_excel("boem_sediment_chl.xlsx", skip = 3)
# remove the blank rows and columns
new_code <- new_code[-c(1:2),-c(2:3)]
# need to rearrange the columns to make them easier to handle
new = new_code %>%
  pivot_longer(!Site, names_to = 'site') %>%
  # got sites right but put all other variables into 1 col
  # take output and make wider
  pivot_wider(names_from = 'Site', values_from = 'value')
# make new cols using the sitenames aka R = ridge, starting letter = region, then have to do dates in excel
data_raw = read_excel("boem_sediment_chl.xlsx", skip = 7, sheet = 'Sheet2') %>%
  select(c(1:17))
glimpse(data_raw)
data <- data_raw %>%
  mutate(
    topo = case_when(
      topo == 1 ~ 'Ridge',
      TRUE ~ 'Swale'),
    region = case_when(
      region == 1 ~ 'Bull',
      region == 2 ~ 'Chester',
      region == 3 ~ 'Dredge N',
      region == 4 ~ 'Dredge S'),
    site = str_replace(data_raw$murie_id, '-','')
  )
data <- data_raw %>%
  mutate(
    topo = case_when(
      topo == 1 ~ 'Ridge',
      TRUE ~ 'Swale'),
    region = case_when(
      region == 1 ~ 'Bull',
      region == 2 ~ 'Chester',
      region == 3 ~ 'Dredge N',
      region == 4 ~ 'Dredge S'),
    site = str_replace(data_raw$murie_id, '-','')
  )
View(data)
data <- data_raw %>%
  mutate(
    topo = case_when(
      topo == 1 ~ 'Ridge',
      TRUE ~ 'Swale'),
    region = case_when(
      region == 1 ~ 'Bull',
      region == 2 ~ 'Chester',
      region == 3 ~ 'Dredge N',
      region == 4 ~ 'Dredge S'),
    site = str_replace(murie_id, '-',''),
    site = str_replace(site, '-', '')
  )
View(data)
unique(data$site)
data[528,]
data[526:530,]
a = data[526:530,]
View(a)
data %>%
  filter(is.na(site))
a =data %>%
  filter(is.na(site))
View(a)
data <- data_raw %>%
  mutate(
    topo = case_when(
      topo == 1 ~ 'Ridge',
      TRUE ~ 'Swale'),
    region = case_when(
      region == 1 ~ 'Bull',
      region == 2 ~ 'Chester',
      region == 3 ~ 'Dredge N',
      region == 4 ~ 'Dredge S'),
    site = str_replace(murie_id, '-',''),
    site = str_replace(site, '-', ''),
    site = str_replace(site, ' ', '')
  )
unique(data$site)
data <- data_raw %>%
  mutate(
    topo = case_when(
      topo == 1 ~ 'Ridge',
      TRUE ~ 'Swale'),
    region = case_when(
      region == 1 ~ 'Bull',
      region == 2 ~ 'Chester',
      region == 3 ~ 'Dredge N',
      region == 4 ~ 'Dredge S'),
    site = str_replace(murie_id, '-',''),
    site = str_replace(site, '-', ''),
    site = str_replace(site, ' ', ''),
    site = str_replace(site, 'DSNW 622', 'DSNW622')
  )
unique(data$site)
data <-  data %>%
  filter(!is.na(site))
combined <- inner join(data, new, by = 'site')
combined <- inner_join(data, new, by = 'site')
View(combined)
names(data)
data_clean = data %>%
  select(date, site, region, topo, sta, corr_chl, sed_corr_chl)
View(data_clean)
data_clean = data %>%
  select(date, site, region, topo, sta, corr_chl, sed_corr_chl)
data_clean = data %>%
  select(date, site, region, topo, sta, corr_chl, sed_corr_chl) %>%
  mutate(div = corr_chl/sed_corr_chl)
View(data_clean)
a = anti_join(data_clean, new, by = 'site')
View(a)
dredge_n = c('DSNW', 'DSNE', 'DRNE', 'DRNW')
dredge_n = c('DSNW', 'DSNE', 'DRNE', 'DRNW')
data <- data %>%
  mutate(tester = case_when(
    str_detect(site, dredge_n) ~ 'Dredge N'
  ))
data <- data %>%
  mutate(tester = case_when(
    site %in% dredge_n ~ 'Dredge N'
  ))
data <- data %>%
  mutate(tester = case_when(
    str_detect(site, 'D') ~ 'Dredge S',
    str_detect(site, 'B') ~ 'Bull'
    site %in% dredge_n ~ 'Dredge N'
    data <- data %>%
      mutate(tester = case_when(
        str_detect(site, 'D') ~ 'Dredge S',
        str_detect(site, 'B') ~ 'Bull',
        str_detect(site, 'C') ~ 'Chester'
        site %in% dredge_n ~ 'Dredge N'
        data <- data %>%
          mutate(tester = case_when(
            str_detect(site, 'D') ~ 'Dredge S',
            str_detect(site, 'B') ~ 'Bull',
            str_detect(site, 'C') ~ 'Chester',
            site %in% dredge_n ~ 'Dredge N'
          ))
        View(data)
        a = data %>%
          select(site, murie_id, region, tester)
        View(a)
        data <- data %>%
          mutate(tester = case_when(
            #str_detect(site, 'D') ~ 'Dredge S',
            str_detect(site, 'B') ~ 'Bull',
            str_detect(site, 'C') ~ 'Chester',
            site %in% dredge_n ~ 'Dredge N'
          ))
        a = data %>%
          select(site, murie_id, region, tester)
        View(a)
        data <- data %>%
          mutate(tester = case_when(
            #str_detect(site, 'D') ~ 'Dredge S',
            str_detect(site, 'B') ~ 'Bull',
            str_detect(site, 'C') ~ 'Chester',
            str_detect(site, dredge_n)~ 'Dredge N'
          ))
        str_c(dredge_n, sep = ' ', collapse = '|')
        dredge_n = str_c(dredge_n, sep = ' ', collapse = '|')
        data <- data %>%
          mutate(tester = case_when(
            #str_detect(site, 'D') ~ 'Dredge S',
            str_detect(site, 'B') ~ 'Bull',
            str_detect(site, 'C') ~ 'Chester',
            str_detect(site, dredge_n)~ 'Dredge N'
          ))
        View(a)
        dredge_n = str_c(dredge_n, sep = '', collapse = '|')
        dredge_n = c('DSNW', 'DSNE', 'DRNE', 'DRNW')
        dredge_n = str_c(dredge_n, sep = '', collapse = '|')
        data <- data_raw %>%
          mutate(
            # make the topo varialbe with the proper names
            topo = case_when(
              topo == 1 ~ 'Ridge',
              TRUE ~ 'Swale'),
            # make the region variable with the proper names
            region = case_when(
              region == 1 ~ 'Bull',
              region == 2 ~ 'Chester',
              region == 3 ~ 'Dredge N',
              region == 4 ~ 'Dredge S'),
            # remove the - and spaces from the murie ids in the sample and make new col ' site'
            site = str_replace(murie_id, '-',''),
            site = str_replace(site, '-', ''),
            site = str_replace(site, ' ', ''),
            site = str_replace(site, 'DSNW 622', 'DSNW622')
          )
        data <- data %>%
          mutate(tester = case_when(
            #str_detect(site, 'D') ~ 'Dredge S',
            str_detect(site, 'B') ~ 'Bull',
            str_detect(site, 'C') ~ 'Chester',
            str_detect(site, dredge_n)~ 'Dredge N'
          ))
        View(data)
        a = data %>%
          select(site, murie_id, region, tester)
        View(a)
        data <- data %>%
          mutate(tester = case_when(
            str_detect(site, 'D') ~ 'Dredge S',
            str_detect(site, 'B') ~ 'Bull',
            str_detect(site, 'C') ~ 'Chester',
            str_detect(site, dredge_n)~ 'Dredge N'
          ))
        a = data %>%
          select(site, murie_id, region, tester)
        View(a)
        dredge_s = c('DSsW', 'DSSE', 'DRSE', 'DRSW')
        dredge_s = str_c(dredge_s, sep = '', collapse = '|')
        data <- data %>%
          mutate(tester = case_when(
            str_detect(site, dredge_s)~ 'Dredge s'
            str_detect(site, 'B') ~ 'Bull',
            data <- data %>%
              mutate(tester = case_when(
                str_detect(site, dredge_s)~ 'Dredge S',
                str_detect(site, 'B') ~ 'Bull',
                str_detect(site, 'C') ~ 'Chester',
                str_detect(site, dredge_n)~ 'Dredge N'
              ))
            a = data %>%
              select(site, murie_id, region, tester)
            View(a)
            dredge_s = c('DSSW', 'DSSE', 'DRSE', 'DRSW')
            dredge_s = str_c(dredge_s, sep = '', collapse = '|')
            data <- data_raw %>%
              mutate(
                # make the topo varialbe with the proper names
                topo = case_when(
                  topo == 1 ~ 'Ridge',
                  TRUE ~ 'Swale'),
                # make the region variable with the proper names
                region = case_when(
                  region == 1 ~ 'Bull',
                  region == 2 ~ 'Chester',
                  region == 3 ~ 'Dredge N',
                  region == 4 ~ 'Dredge S'),
                # remove the - and spaces from the murie ids in the sample and make new col ' site'
                site = str_replace(murie_id, '-',''),
                site = str_replace(site, '-', ''),
                site = str_replace(site, ' ', ''),
                site = str_replace(site, 'DSNW 622', 'DSNW622')
              )
            data <- data %>%
              mutate(tester = case_when(
                str_detect(site, dredge_s)~ 'Dredge S',
                str_detect(site, 'B') ~ 'Bull',
                str_detect(site, 'C') ~ 'Chester',
                str_detect(site, dredge_n)~ 'Dredge N'
              ))
            a = data %>%
              select(site, murie_id, region, tester)
            View(a)
            is.na(a$tester)
            a[is.na(a$tester)]
            a[is.na(a$tester),]
            data[site == 'cssw497']
            data%site[site == 'cssw497']
            data$site[site == 'cssw497']
            data$site[data$site == 'cssw497']
            data <- data_raw %>%
              mutate(
                # make the topo varialbe with the proper names
                topo = case_when(
                  topo == 1 ~ 'Ridge',
                  TRUE ~ 'Swale'),
                # make the region variable with the proper names
                region = case_when(
                  region == 1 ~ 'Bull',
                  region == 2 ~ 'Chester',
                  region == 3 ~ 'Dredge N',
                  region == 4 ~ 'Dredge S'),
                # remove the - and spaces from the murie ids in the sample and make new col ' site'
                site = str_replace(murie_id, '-',''),
                site = str_replace(site, '-', ''),
                site = str_replace(site, ' ', ''),
                site = str_replace(site, 'DSNW 622', 'DSNW622'),
                site = str_replace(site, 'cssw497', 'CSSW497')
              )
            data <- data %>%
              mutate(tester = case_when(
                str_detect(site, dredge_s)~ 'Dredge S',
                str_detect(site, 'B') ~ 'Bull',
                str_detect(site, 'C') ~ 'Chester',
                str_detect(site, dredge_n, )~ 'Dredge N',
                str_detect(site, 'css497')
              ))
            data <- data %>%
              mutate(tester = case_when(
                str_detect(site, dredge_s)~ 'Dredge S',
                str_detect(site, 'B') ~ 'Bull',
                str_detect(site, 'C') ~ 'Chester',
                str_detect(site, dredge_n, )~ 'Dredge N'
              ))
            a = data %>%
              select(site, murie_id, region, tester)
            a[is.na(a$tester),]
            a[is.na(a$tester),]
            unique(data$region)
            data <- data_raw %>%
              mutate(
                # make the topo varialbe with the proper names
                topo = case_when(
                  topo == 1 ~ 'Ridge',
                  TRUE ~ 'Swale'),
                # make the region variable with the proper names
                region = case_when(
                  region == 1 ~ 'Bull',
                  region == 2 ~ 'Chester',
                  region == 3 ~ 'Dredge N',
                  region == 4 ~ 'Dredge S'),
                # remove the - and spaces from the murie ids in the sample and make new col ' site'
                site = str_replace(murie_id, '-',''),
                site = str_replace(site, '-', ''),
                site = str_replace(site, ' ', ''),
                site = str_replace(site, 'DSNW 622', 'DSNW622'),
                site = str_replace(site, 'cssw497', 'CSSW497')
              )
            unique(data$region)
            data <- data %>%
              mutate(site = case_when(
                str_detect(site, dredge_s)~ 'Dredge S',
                str_detect(site, 'B') ~ 'Bull',
                str_detect(site, 'C') ~ 'Chester',
                str_detect(site, dredge_n, )~ 'Dredge N'
              ))
            a = data %>%
              select(site, murie_id, region)
            # take a look
            a[is.na(a$site),]
            # remove sites with na sites
            data <-  data %>%
              filter(!is.na(site))
            glimpse(data)
            unique(data$site)
            data <- data_raw %>%
              mutate(
                # make the topo varialbe with the proper names
                topo = case_when(
                  topo == 1 ~ 'Ridge',
                  TRUE ~ 'Swale'),
                # make the region variable with the proper names
                region = case_when(
                  region == 1 ~ 'Bull',
                  region == 2 ~ 'Chester',
                  region == 3 ~ 'Dredge N',
                  region == 4 ~ 'Dredge S'),
                # remove the - and spaces from the murie ids in the sample and make new col ' site'
                site = str_replace(murie_id, '-',''),
                site = str_replace(site, '-', ''),
                site = str_replace(site, ' ', ''),
                site = str_replace(site, 'DSNW 622', 'DSNW622'),
                site = str_replace(site, 'cssw497', 'CSSW497')
              )
            data <- data_raw %>%
              mutate(
                # make the topo varialbe with the proper names
                topo = case_when(
                  topo == 1 ~ 'Ridge',
                  TRUE ~ 'Swale'),
                # make the region variable with the proper names
                region = case_when(
                  region == 1 ~ 'Bull',
                  region == 2 ~ 'Chester',
                  region == 3 ~ 'Dredge N',
                  region == 4 ~ 'Dredge S'),
                # remove the - and spaces from the murie ids in the sample and make new col ' site'
                site = str_replace(murie_id, '-',''),
                site = str_replace(site, '-', ''),
                site = str_replace(site, ' ', ''),
                site = str_replace(site, 'DSNW 622', 'DSNW622'),
                site = str_replace(site, 'cssw497', 'CSSW497')
              )
            data <- data_raw %>%
              mutate(
                # make the topo varialbe with the proper names
                topo = case_when(
                  topo == 1 ~ 'Ridge',
                  TRUE ~ 'Swale'),
                # make the region variable with the proper names
                region = case_when(
                  region == 1 ~ 'Bull',
                  region == 2 ~ 'Chester',
                  region == 3 ~ 'Dredge N',
                  region == 4 ~ 'Dredge S'),
                # remove the - and spaces from the murie ids in the sample and make new col ' site'
                site = str_replace(murie_id, '-',''),
                site = str_replace(site, '-', ''),
                site = str_replace(site, ' ', ''),
                site = str_replace(site, 'DSNW 622', 'DSNW622'),
                site = str_replace(site, 'cssw497', 'CSSW497')
              )
            data <- data %>%
              mutate(region = case_when(
                str_detect(site, dredge_s)~ 'Dredge S',
                str_detect(site, 'B') ~ 'Bull',
                str_detect(site, 'C') ~ 'Chester',
                str_detect(site, dredge_n, )~ 'Dredge N'
              ))
            # take a look
            names(data)
            unique(data$site)
            # remove sites with na sites
            data <-  data %>%
              filter(!is.na(site))
            unique(data$site)
            # remove sites with na sites
            data <-  data %>%
              filter(!is.na(region))
            data <- data_raw %>%
              mutate(
                # make the topo varialbe with the proper names
                topo = case_when(
                  topo == 1 ~ 'Ridge',
                  TRUE ~ 'Swale'),
                # make the region variable with the proper names
                region = case_when(
                  region == 1 ~ 'Bull',
                  region == 2 ~ 'Chester',
                  region == 3 ~ 'Dredge N',
                  region == 4 ~ 'Dredge S'),
                # remove the - and spaces from the murie ids in the sample and make new col ' site'
                site = str_replace(murie_id, '-',''),
                site = str_replace(site, '-', ''),
                site = str_replace(site, ' ', ''),
                site = str_replace(site, 'DSNW 622', 'DSNW622'),
                site = str_replace(site, 'cssw497', 'CSSW497')
              )
            data <- data %>%
              mutate(region = case_when(
                str_detect(site, dredge_s)~ 'Dredge S',
                str_detect(site, 'B') ~ 'Bull',
                str_detect(site, 'C') ~ 'Chester',
                str_detect(site, dredge_n, )~ 'Dredge N'
              ))
            # take a look
            names(data)
            unique(data$site)
            # remove sites with na sites
            data <-  data %>%
              filter(!is.na(region))
            unique(data$site)
            data_clean = data %>%
              select(date, site, region, topo, sta, corr_chl, sed_corr_chl) %>%
              mutate(div = corr_chl/sed_corr_chl)
            a = anti_join(data_clean, new, by = 'site')
            View(a)
            a = anti_join(data_clean, new, by = 'site') %>%
              arrange(date)
            View(a)

library(openxlsx)
write.xlsx(data, 'sediment_chl_clean.xlsx')            
