library(tidyverse)
library(ggpubr)
library(scales)
library(glue)
library(plotly)
library(ggplot2)
library(stringr)
library(lubridate )
library(dplyr)
library(zoo)
library(hrbrthemes)
library(viridis)
library(dashboardthemes)
options(scipen=123)

library(shiny)
library(shinydashboard)


hls <- read_csv("data/hls.csv")
rls <- read_csv("data/rls.csv")
hls <- hls %>% mutate(nama_variabel = as.factor(nama_variabel),
                      jenis_kelamin = as.factor(jenis_kelamin),
                      provinsi = as.factor(provinsi) ,
                      angka  = as.numeric(angka),
                      tahun = as.factor(tahun))
rls <- rls %>%mutate(nama_variabel = as.factor(nama_variabel),
                     jenis_kelamin = as.factor(jenis_kelamin),
                     provinsi = as.factor(provinsi) ,
                     angka  = as.numeric(angka),
                     tahun = as.factor(tahun))
rls <- distinct(rls, nama_variabel,jenis_kelamin,tahun,angka,provinsi, .keep_all= TRUE)


rls_provinsi <- rls[rls$jenis_kelamin != "INDONESIA", ]
rls_clean <- rls_provinsi[rls_provinsi$jenis_kelamin != "Laki-laki", ]
hls_provinsi <- hls[hls$jenis_kelamin != "INDONESIA", ]
hls_clean <- hls_provinsi[hls_provinsi$jenis_kelamin != "Laki-laki", ]

hls_rls_clean <- bind_rows(hls_clean, rls_clean)

##### DATA PENYELESAIAN PENDIDIKAN
penyelesaian_daerah <- read_csv("data/penyelesaian-daerah.csv")
penyelesaian_kelamin <- read_csv("data/penyelesaian-kelamin.csv")

penyelesaian_kelamin <- penyelesaian_kelamin %>% mutate(tingkat_pendidikan = as.factor(tingkat_pendidikan),
                                                       jenis_kelamin = as.factor(jenis_kelamin),
                                                       nama_variabel = as.factor(nama_variabel) ,
                                                       persen  = as.numeric(persen),
                                                       tahun = as.factor(tahun))
penyelesaian_daerah <- penyelesaian_daerah %>%mutate(tingkat_pendidikan = as.factor(tingkat_pendidikan),
                                                     provonsi = as.factor(provonsi),
                                                     nama_variabel = as.factor(nama_variabel) ,
                                                     persen  = as.numeric(persen),
                                                     tahun = as.factor(tahun))
penyelesaian_kelamin <- penyelesaian_kelamin  %>% filter(jenis_kelamin == "Perempuan")
pdk <- bind_rows(penyelesaian_daerah, penyelesaian_kelamin)
pdk <- pdk %>% rename(provinsi = provonsi)
pdk <- pivot_longer(data = pdk,
                    cols = c("tingkat_pendidikan","jenis_kelamin"),
                    names_to = "variabel")
pdk <- pdk %>% rename(jenis = value)
pdk <- pdk %>% na.omit()
pdk <- pdk  %>% select (-c("variabel", "nama_variabel"))



b <- read.csv("data/bt.csv")
b <- b %>% mutate(nama_variabel = as.factor(nama_variabel),
                  jenis_kelamin = as.factor(jenis_kelamin),
                  provinsi = as.factor(provinsi) ,
                  persen  = as.numeric(persen),
                  tahun = as.factor(tahun))
b_provinsi <- b[b$provinsi != "INDONESIA", ]
b_clean <- b_provinsi[b_provinsi$jenis_kelamin != "Laki-Laki", ]
b_clean$persen<-format(round(b_clean$persen,2),nsmall=2)
b_clean <- b_clean %>% mutate(persen = as.numeric(persen))

#DATA PENGANGGURAN TERBUKA
pt_kelamin <- read_csv("data/PT-kelamin.csv")
pt_pend <- read_csv("data/PT-pend.csv")
pt_bulan <- read_csv("data/PT-bln.csv")


pt_pend <- pt_pend %>% select(-c("nama_variabel")) %>% mutate(
  tingkat_pendidikan = as.factor(tingkat_pendidikan),
  persen  = as.numeric(persen),
  tahun = as.factor(tahun))
pt_bulan <- pt_bulan %>% filter_all(any_vars(. %in% c('Jumlah (Ribu orang)'))) %>%  select(-c("nama_variabel","satuan")) %>%  mutate(persen  = as.numeric(persen),
    tahun = as.factor(tahun),
    bulan = as.factor(bulan))
pt_kelamin <- pt_kelamin %>% select(-c("nama_variabel")) %>% mutate(
  jenis_kelamin = as.factor(jenis_kelamin),
  persen  = as.numeric(persen),
  tahun = as.factor(tahun))

pt_bulan <- pt_bulan %>%
  group_by(bulan) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = bulan, values_from = persen) %>%
  select(-row) %>% 
  rowwise() %>% mutate(total = mean(c(Februari, Agustus), na.rm=T)) %>%
  select(-c(Februari, Agustus))
pt_bulan <- distinct(pt_bulan, tahun,total, .keep_all= TRUE)

ptk_ptb <- bind_rows(pt_kelamin, pt_bulan)
ptk_ptb <- pivot_wider(data = ptk_ptb, 
                       names_from = jenis_kelamin, 
                       values_from = c(persen, total))
ptk_ptb <- ptk_ptb %>% rename(persen_l = `persen_Laki - Laki`) %>% select(-c("total_Laki - Laki", "total_Perempuan", "persen_NA"))
ptk_ptb$persen_l <- ptk_ptb$persen_l / 100
ptk_ptb$persen_Perempuan <- ptk_ptb$persen_Perempuan / 100
ptk_ptb$perempuan <- ptk_ptb$total_NA * ptk_ptb$persen_Perempuan
ptk_ptb$perempuan <- format(round(ptk_ptb$perempuan,2),nsmall=2)
ptk_ptb$total_keseluruhan <- format(round(ptk_ptb$total_NA,2),nsmall=2)
ptk_ptb <- ptk_ptb %>% select(-c("persen_l", "persen_Perempuan", "total_NA"))
ptk_ptb <- pivot_longer(data = ptk_ptb,
                        cols = c("perempuan","total_keseluruhan"),
                        names_to = "variabel")
ptk_ptb %>% mutate(variabel = as.factor(variabel))

p <- read_csv("data/p.csv")
p <- p %>% mutate(nama_variabel = as.factor(nama_variabel),
                  jenis_kelamin = as.factor(jenis_kelamin),
                  provinsi = as.factor(provinsi) ,
                  total  = as.numeric(total),
                  tahun = as.factor(tahun))
p <- p[p$jenis_kelamin != "Laki-laki", ]
p <- p %>% select(-c("jenis_kelamin"))
p <- distinct(p, nama_variabel,tahun,total,provinsi, .keep_all= TRUE)
p_provinsi <- p[p$provinsi != "INDONESIA", ]


spp <- read_csv("data/spp.csv")
spp <- spp %>% mutate(nama_variabel = as.factor(nama_variabel),
                      provinsi = as.factor(provinsi) ,
                      persen  = as.numeric(persen),
                      tahun = as.factor(tahun))

spp <- distinct(spp, tahun,persen,provinsi, .keep_all= TRUE)
spp$persen<-format(round(spp$persen,2),nsmall=2)
spp_provinsi <- spp[spp$provinsi != "INDONESIA", ]

p_spp <- bind_rows(p, spp)
p_spp_prov <- p_spp[p_spp$provinsi != "INDONESIA", ]


hlsarr <- hls_clean %>% arrange(-angka) %>% filter(tahun == "2021") %>% rename(harapan = angka) %>% select(-c(nama_variabel, jenis_kelamin, tahun))
rlsarr <- rls_clean %>% arrange(-angka) %>% filter(tahun == "2021") %>% rename(realitas = angka)%>% select(-c(nama_variabel, jenis_kelamin, tahun))
hlsrlsarr <- bind_rows(hlsarr, rlsarr)
hlsrlsarr <- pivot_longer(data = hlsrlsarr,
                          cols = c("harapan","realitas"),
                          names_to = "variabel")
rlsarr <- rlsarr[rlsarr$provinsi != "INDONESIA", ]
hlsrlsarr <- hlsrlsarr[hlsrlsarr$provinsi != "INDONESIA", ]
hlsrlsarr <- hlsrlsarr %>% na.omit()
hlsrlsarr$variabel <- as.factor(hlsrlsarr$variabel)


