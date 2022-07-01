library(rio)
library(readr)
library(dplyr)

########################################### QUESTÃO 1 #####################################################################



quest1_url1 <- "https://weather.uwyo.edu/cgi-bin/sounding?region=samer&TYPE=TEXT%3ALIST&YEAR=2022&MONTH=04&FROM=2812&TO=2812&STNM=78954"
# Grantley Adams lat=13.07

quest1_url2 <- "https://weather.uwyo.edu/cgi-bin/sounding?region=seasia&TYPE=TEXT%3ALIST&YEAR=2022&MONTH=04&FROM=2812&TO=2812&STNM=47169"
# Heuksando lat=34.68

quest1_url3 <- "https://weather.uwyo.edu/cgi-bin/sounding?region=np&TYPE=TEXT%3ALIST&YEAR=2022&MONTH=04&FROM=2812&TO=2812&STNM=01004"
# Ny-Alesund Ii lat=78.91

quest1_txt1 <- read_lines(quest1_url1)
quest1_txt2 <- read_lines(quest1_url2)
quest1_txt3 <- read_lines(quest1_url3)


quest1_arq1 <- "quest1_1_20220428_12z.txt"
quest1_arq2 <- "quest1_2_20220428_12z.txt"
quest1_arq3 <- "quest1_3_20220428_12z.txt"

write_lines(
  x = quest1_txt1,
  file = quest1_arq1
)

write_lines(
  x = quest1_txt2,
  file = quest1_arq2
)

write_lines(
  x = quest1_txt3,
  file = quest1_arq3
)

quest1_df1 <- import(
  file = quest1_arq1,
  format = "fwf",
  header = FALSE,
  col.names = c(
    "PRES_hPa",
    "HGHT_m",
    "TEMP_C",
    "DWPT",
    "RELH",
    "MIXR",
    "DRCT",
    "SKNT",
    "THTA",
    "THTE",
    "THTV"
  ),
  nrows = 78,
  skip = 9,
  widths = rep(7, 11)
)

quest1_df2 <- import(
  file = quest1_arq2,
  format = "fwf",
  header = FALSE,
  col.names = c(
    "PRES_hPa",
    "HGHT_m",
    "TEMP_C",
    "DWPT",
    "RELH",
    "MIXR",
    "DRCT",
    "SKNT",
    "THTA",
    "THTE",
    "THTV"
  ),
  nrows = 101,
  skip = 9,
  widths = rep(7, 11)
)

quest1_df3 <- import(
  file = quest1_arq3,
  format = "fwf",
  header = FALSE,
  col.names = c(
    "PRES_hPa",
    "HGHT_m",
    "TEMP_C",
    "DWPT",
    "RELH",
    "MIXR",
    "DRCT",
    "SKNT",
    "THTA",
    "THTE",
    "THTV"
  ),
  nrows = 97,
  skip = 9,
  widths = rep(7, 11)
)

#----------------------------------- CÁLCULOS ---------------------------------------------#

Rd <- 287
g <- 9.81

tk1 <- quest1_df1[,3]
tk1 <- tk1 + 273.13

tk2 <- quest1_df2[,3]
tk2 <- tk2 + 273.13

tk3 <- quest1_df3[,3]
tk3 <- tk3 + 273.13

tkmed1 <- mean(tk1[1:which(quest1_df1[,1] == 500.0)])
tkmed2 <- mean(tk2[1:which(quest1_df2[,1] == 500.0)])
tkmed3 <- mean(tk3[1:which(quest1_df3[,1] == 500.0)])

p1 <- quest1_df1[1,1]
p2 <- quest1_df2[1,1]
p3 <- quest1_df3[1,1]
ps1 <- ps2 <- ps3 <- 500

esp_est1 <- (-(Rd*tkmed1/(g)))*log(ps1/p1)
esp_est2 <- (-(Rd*tkmed2/(g)))*log(ps2/p2)
esp_est3 <- (-(Rd*tkmed3/(g)))*log(ps3/p3)

esp_real1 <- quest1_df1[which(quest1_df1[,1] == 500.0), 2] - quest1_df1[1,2]
esp_real2 <- quest1_df2[which(quest1_df2[,1] == 500.0), 2] - quest1_df2[1,2]
esp_real3 <- quest1_df3[which(quest1_df3[,1] == 500.0), 2] - quest1_df3[1,2]

diff_pct1 <- (1 - (esp_est1/esp_real1))*100
diff_pct2 <- (1 - (esp_est2/esp_real2))*100
diff_pct3 <- (1 - (esp_est3/esp_real3))*100

H1 <- (Rd*mean(tk1))/g
H2 <- (Rd*mean(tk2))/g
H3 <- (Rd*mean(tk3))/g

rho1_s <- quest1_df1[1,1]*100/(Rd*mean(tk1[1]))
rho2_s <- quest1_df2[1,1]*100/(Rd*mean(tk2[1]))
rho3_s <- quest1_df3[1,1]*100/(Rd*mean(tk3[1]))

rho1_850 <- 850*100/(Rd*mean(tk1[which(quest1_df1[,1] == 850)]))
rho2_850 <- 850*100/(Rd*mean(tk2[which(quest1_df2[,1] == 850)]))
rho3_850 <- 850*100/(Rd*mean(tk3[which(quest1_df3[,1] == 850)]))

rho1_700 <- 700*100/(Rd*mean(tk1[which(quest1_df1[,1] == 700)]))
rho2_700 <- 700*100/(Rd*mean(tk2[which(quest1_df2[,1] == 700)]))
rho3_700 <- 700*100/(Rd*mean(tk3[which(quest1_df3[,1] == 700)]))

rho1_500 <- 500*100/(Rd*mean(tk1[which(quest1_df1[,1] == 500)]))
rho2_500 <- 500*100/(Rd*mean(tk2[which(quest1_df2[,1] == 500)]))
rho3_500 <- 500*100/(Rd*mean(tk3[which(quest1_df3[,1] == 500)]))

rho1_400 <- 400*100/(Rd*mean(tk1[which(quest1_df1[,1] == 400)]))
rho2_400 <- 400*100/(Rd*mean(tk2[which(quest1_df2[,1] == 400)]))
rho3_400 <- 400*100/(Rd*mean(tk3[which(quest1_df3[,1] == 400)]))

rho1_250 <- 250*100/(Rd*mean(tk1[which(quest1_df1[,1] == 250)]))
rho2_250 <- 250*100/(Rd*mean(tk2[which(quest1_df2[,1] == 250)]))
rho3_250 <- 250*100/(Rd*mean(tk3[which(quest1_df3[,1] == 250)]))

########################################### QUESTÃO 2 #####################################################################

quest2_url <- "https://raw.githubusercontent.com/lhmet/dados-termo-ufsm/main/us-standard-atmosphere"

quest2_txt <- read_lines(quest2_url)
quest2_arq <- "lista_dados_ex3.txt"
quest2_arq_alt <- "lista_dados_ex3_alt.txt"

write_lines(
  x = quest2_txt,
  file = quest2_arq
)

quest2_txt_alt <- gsub(
  pattern = " × 10",
  x = quest2_txt,
  ignore.case = FALSE,
  replacement = "e"
)

quest2_txt_alt <- gsub(
  pattern = substr(quest2_txt_alt[40], 87,87),
  x = quest2_txt_alt,
  ignore.case = FALSE,
  replacement = "-"
)

quest2_txt_alt <- gsub(
  pattern = ")  ",
  x = quest2_txt_alt,
  ignore.case = FALSE,
  replacement = ");  "
)

write_lines(
  x = quest2_txt_alt,
  file = quest2_arq_alt
)

names_col <- import(
  file = quest2_arq_alt,
  format = "tsv",
  header = FALSE,
  skip = 1,
  nrows = 1
)
names(names_col) <- NULL


uni_col <- import(
  file = quest2_arq_alt,
  format = "csv",
  header = FALSE,
  skip = 2,
  nrows = 1
)

names(uni_col) <- NULL

quest2_df <- import(
  file = quest2_arq_alt,
  format = "tsv",
  header = FALSE,
  #col.names = c(
  #  "PRES_hPa",
  #  "HGHT_m",
  #  "TEMP_C",
  #  "DWPT",
  #  "RELH",
  #  "MIXR",
  #  "DRCT",
  #  "SKNT",
  #  "THTA",
  #  "THTE",
  #  "THTV"
  #),
  nrows = 40,
  skip = 1
  #widths = rep(7, 11)
)

names(quest2_df) <- NA

#names(quest2_df) <- paste(names_col , uni_col)
quest2_df

quest2_df <- rbind(quest2_df[,1:5], quest2_df[,6:10])
names(quest2_df) <- paste(names_col[1:5] , uni_col[1:5])

quest2_df <- quest2_df[-80,]

ind_alt_12km <- which(quest2_df[,1] == 12.0)

plot(quest2_df[1:ind_alt_12km,1], quest2_df[1:ind_alt_12km,3], type = "l", lwd = 3, col = "blue")

pres_calc <- quest2_df[1,3]*exp(-(g*quest2_df[1:ind_alt_12km,1]*1000)/(Rd*mean(quest2_df[1:ind_alt_12km,4])))

plot(quest2_df[1:ind_alt_12km,1], quest2_df[1:ind_alt_12km,3], type = "l", lwd = 3, col = "blue")
lines(quest2_df[1:ind_alt_12km,1], pres_calc, type = "l", lwd = 3, col = "red")


