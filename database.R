library (haven)
library(tidyverse)
library(dplyr)
library(stringr)

#errase data bases from environment
rm(BD00, BD01,BD02, BD03, BD04, BD05,BD06, BD07, BD08, BD09,BD10,
   BD11,BD12,BD13,BD14,BD15,BD16,BD17,BD18,BD19,BD20,BD21)

#Data Bases 

#Data base 2000
BD00<-read_sav("Documents/Memoire/BD/2000/empnov00.sav")
BD00 <- BD00%>% rename (educ=nivinst, educ_years=anoinst,age=edad,sex=sexo,city=ciudad,literacy=sabele,
                        salary=ingasa,work=trabajo, work_hors=hortrasa, income=ingpat)
BD00$Year<- "2000"
BD00 <- BD00%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)

#Data base 2001
BD01<-read_sav("Documents/Memoire/BD/2001/empleo.sav")
BD01 <- BD01%>% rename (educ=NIVINST, educ_years=ANOINST,age=EDAD,sex=SEXO,city=CIUDAD,literacy=SABELE,
                        salary=PE63,work=TRABAJO, work_hors=HORTRASA, income=PE61)
BD01$Year<- "2001"
BD01 <- BD01%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2002
BD02<-read_sav("Documents/Memoire/BD/2002/eupe02.sav")
BD02 <- BD02%>% rename (educ=nivinst, educ_years=anoinst,age=edad,sex=sexo,city=ciudad,literacy=sabele,
                        salary=pe63,work=trabajo, work_hors=hortrasa, income=pe61)
BD02$Year<- "2002"
BD02 <- BD02%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2003
BD03<-read_sav("Documents/Memoire/BD/2003/Per0203.sav")
BD03 <- BD03%>% rename (educ=NIVINST, educ_years=ANOINST,age=EDAD,sex=SEXO,city=CIUDAD,literacy=SABELE,
                        salary=PE63,work=TRABAJO, work_hors=HORTRASA, income=PE61)
BD03$Year<- "2003"
BD03 <- BD03%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2004
BD04<-read_sav("Documents/Memoire/BD/2004/Per0404.sav")
BD04 <- BD04%>% rename (educ=NIVINST, educ_years=ANOINST,age=EDAD,sex=SEXO,city=CIUDAD,literacy=SABELE,
                        salary=PE63,work=TRABAJO, work_hors=HORTRASA, income=PE61)
BD04$Year<- "2004"
BD04 <- BD04%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2005
BD05<-read_sav("Documents/Memoire/BD/2005/Per0405.sav")
BD05 <- BD05%>% rename (educ=NIVINST, educ_years=ANOINST,age=EDAD,sex=SEXO,city=CIUDAD,literacy=SABELE,
                        salary=PE63,work=TRABAJO, work_hors=HORTRASA, income=PE61)
BD05$Year<- "2005"
BD05 <- BD05%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2006
BD06<-read_sav("Documents/Memoire/BD/2006/per0406.sav")
BD06 <- BD06%>% rename (educ=NIVINST, educ_years=ANOINST,age=EDAD,sex=SEXO,city=CIUDAD,literacy=SABELE,
                        salary=PE63,work=TRABAJO, work_hors=HORTRASA, income=PE61)
BD06$Year<- "2006"
BD06 <- BD06%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2007
BD07<-read_sav("Documents/Memoire/BD/2007/200712_EnemduBDD_15anios.sav")
BD07 <- BD07%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                        work=p20, work_hors=p24,salary=p66, income=p63)
BD07$Year<- "2007"
BD07 <- BD07%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2008
BD08<-read_sav("Documents/Memoire/BD/2008/200812_EnemduBDD_15anios.sav")
BD08 <- BD08%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                        work=p20, work_hors=p24,salary=p66, income=p63)
BD08$Year<- "2008"
BD08 <- BD08%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2009
BD09<-read_sav("Documents/Memoire/BD/2009/200912_EnemduBDD_15anios.sav")
BD09 <- BD09%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                        work=p20, work_hors=p24,salary=p66, income=p63)
BD09$Year<- "2009"
BD09 <- BD09%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2010
BD10<-read_sav("Documents/Memoire/BD/2010/201012_EnemduBDD_15anios.sav")
BD10 <- BD10%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                        work=p20, work_hors=p24,salary=p66, income=p63)
BD10$Year<- "2010"
BD10 <- BD10%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2011
BD11<-read_sav("Documents/Memoire/BD/2011/201112_EnemduBDD_15anios.sav")
BD11 <- BD11%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                        work=p20, work_hors=p24,salary=p66, income=p63)
BD11$Year<- "2011"
BD11 <- BD11%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2012
BD12<-read_sav("Documents/Memoire/BD/2012/201212_EnemduBDD_15anios.sav")
BD12 <- BD12%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                        work=p20, work_hors=p24,salary=p66, income=p63)
BD12$Year<- "2012"
BD12 <- BD12%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2013
BD13<-read_sav("Documents/Memoire/BD/2013/201312_EnemduBDD_15anios.sav")
BD13 <- BD13%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                        work=p20, work_hors=p24,salary=p66, income=p63)
BD13$Year<- "2013"
BD13 <- BD13%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2014
BD14<-read_sav("Documents/Memoire/BD/2014/201412_EnemduBDD_15anios.sav")
BD14<- BD14%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                        work=p20, work_hors=p24,salary=p66, income=p63)
BD14$Year<- "2014"
BD14 <- BD14%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2015
BD15<-read_sav("Documents/Memoire/BD/2015/201512_EnemduBDD_15anios.sav")
BD15<- BD15%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                       work=p20, work_hors=p24,salary=p66, income=p63)
BD15$Year<- "2015"
BD15 <- BD15%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2016
BD16<-read_sav("Documents/Memoire/BD/2016/122016_EnemduBDD_completa.sav")
BD16<- BD16%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                       work=p20, work_hors=p24,salary=p66, income=p63)
BD16$Year<- "2016"
BD16 <- BD16%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2017
BD17<-read_sav("Documents/Memoire/BD/2017/201712_EnemduBDD_15anios.sav")
BD17<- BD17%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                       work=p20, work_hors=p24,salary=p66, income=p63)
BD17$Year<- "2017"
BD17 <- BD17%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2018
BD18<-read_sav ("Documents/Memoire/BD/2018/ENEMDU_PERSONAS_2018_12_hom.sav")
BD18<- BD18%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                       work=p20, work_hors=p24,salary=p66, income=p63)
BD18$Year<- "2018"
BD18 <- BD18%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2019
BD19<-read_sav ("Documents/Memoire/BD/2019/enemdu_persona_201912.sav")
BD19<- BD19%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                       work=p20, work_hors=p24,salary=p66, income=p63)
BD19$Year<- "2019"
BD19 <- BD19%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)

#Data base 2020
BD20<-read.csv("Documents/Memoire/BD/2020/enemdu_persona_2020_12.csv", sep=";")
BD20<- BD20%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                       work=p20, work_hors=p24,salary=p66, income=p63)
BD20$Year<- "2020"
BD20 <- BD20%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#Data base 2021
BD21<-read.csv("Documents/Memoire/BD/2021/enemdu_persona_2021_12.csv", sep=";")
BD21<- BD21%>% rename (educ=p10a, educ_years=p10b,age=p03,sex=p02,city=ciudad,literacy=p11,
                       work=p20, work_hors=p24,salary=p66, income=p63)
BD21$Year<- "2021"
BD21 <- BD21%>% select(city, age, sex, educ, educ_years, literacy, work, work_hors, salary,income,Year)


#merging all data bases 
BDfull <- rbind(BD00, BD01,BD02, BD03, BD04, BD05,BD06, BD07, BD08, BD09,BD10,
                BD11,BD12,BD13,BD14,BD15,BD16,BD17,BD18,BD19,BD20,BD21)

#replacing 999999 by NA
BDfull<-BDfull%>%mutate(salary = case_when(salary >= 999999 ~ NA_real_, TRUE ~ salary))
 
BDfull<-BDfull%>%mutate(income = case_when(income >= 999999 ~ NA_real_, TRUE ~ income))

#years of schooling

BDfull<-BDfull%>%mutate(schooling = case_when(Year <= 2013 & educ==1 ~ 0, #methodology before 2013
                                              Year <= 2013 & educ==2 & educ_years==0 ~ 0,
                                              Year <= 2013 & educ==2 & educ_years==1 ~ 3,
                                              Year <= 2013 & educ==2 & educ_years==2 ~ 5,
                                              Year <= 2013 & educ==2 & educ_years==3 ~ 7,
                                              Year <= 2013 & educ==3 ~ 1,
                                              Year <= 2013 & educ==4 ~ 1+educ_years,
                                              Year <= 2013 & educ==5 ~ educ_years,
                                              Year <= 2013 & educ==6 ~ 7+educ_years,
                                              Year <= 2013 & educ==7 ~ 10+educ_years,
                                              Year <= 2013 & educ==8 ~ 13+educ_years,
                                              Year <= 2013 & educ==9 ~ 13+educ_years,
                                              Year <= 2013 & educ==10 ~ 18+educ_years,
                                              
                                              Year >= 2014 & educ==1 ~ 0,#methodology after 2014 
                                              Year >= 2014 & educ==2 & educ_years==0 ~ 0,
                                              Year >= 2014 & educ==2 & educ_years==1 ~ 2,
                                              Year >= 2014 & educ==2 & educ_years==2 ~ 4,
                                              Year >= 2014 & educ==2 & educ_years==3 ~ 6,
                                              Year >= 2014 & educ==2 & educ_years==4 ~ 7,
                                              Year >= 2014 & educ==2 & educ_years==5 ~ 8,
                                              Year >= 2014 & educ==2 & educ_years==6 ~ 9,
                                              Year >= 2014 & educ==2 & educ_years==7 ~ 10,
                                              Year >= 2014 & educ==2 & educ_years==8 ~ 11,
                                              Year >= 2014 & educ==2 & educ_years==9 ~ 12,
                                              Year >= 2014 & educ==2 & educ_years==10 ~ 13,
                                              Year >= 2014 & educ==3 ~ 1,
                                              Year >= 2014 & educ==4 ~ 1+educ_years,
                                              Year >= 2014 & educ==5 ~ educ_years,
                                              Year >= 2014 & educ==6 ~ 7+educ_years,
                                              Year >= 2014 & educ==7 ~ 10+educ_years,
                                              Year >= 2014 & educ==8 ~ 13+educ_years,
                                              Year >= 2014 & educ==9 ~ 13+educ_years,
                                              Year >= 2014 & educ==10 ~ 18+educ_years,
                                              TRUE ~ NA_real_))


#Name Canton 

BDfull <- transform(BDfull , city = as.numeric(city))
BDfull$city<- str_pad(BDfull$city, width=6, pad="0")


#canton names from code 
BDfull<- BDfull %>% mutate(Canton = case_when(startsWith(city, "0101") ~ "Cuenca",startsWith(city, "0102") ~ "Giron",
                                            startsWith(city, "0103") ~ "Gualaceo",startsWith(city, "0104") ~ "Nabon",
                                            startsWith(city, "0105")~"Paute",startsWith(city, "0106") ~ "Pucara",
                                            startsWith(city, "0107")~"San Fernando",startsWith(city, "0108") ~ "Santa Isabel",
                                            startsWith(city, "0109")~"Sigsig",startsWith(city, "0110") ~ "Oña",
                                            startsWith(city, "0111")~"Chordeleg",startsWith(city, "0112") ~ "El Pan",
                                            startsWith(city, "0113")~"Sevilla de oro",startsWith(city, "0114") ~ "Guachapala",
                                            startsWith(city, "0115")~"Camilo Ponce Enriquez",startsWith(city, "0201") ~ "Guaranda",
                                            startsWith(city, "0202")~"Chillanes",startsWith(city, "0203") ~ "Chimbo",
                                            startsWith(city, "0204")~"Echeandia", startsWith(city, "0205") ~ "San Miguel",
                                            startsWith(city, "0206")~"Caluma",startsWith(city, "0207") ~ "Las Naves",
                                            startsWith(city, "0301")~"Azogues",startsWith(city, "0302") ~ "Biblian",
                                            startsWith(city, "0303")~"Cañar",startsWith(city, "0304") ~ "La Troncal",
                                            startsWith(city, "0305")~"El Tambo",startsWith(city, "0306") ~ "Deleg",
                                            startsWith(city, "0307")~"Suscal",startsWith(city, "0401") ~ "Tulcan",
                                            startsWith(city, "0402")~"Bolivar",startsWith(city, "0403") ~ "Espejo",
                                            startsWith(city, "0404")~"Mira",startsWith(city, "0405") ~ "Montufar",
                                            startsWith(city, "0406")~"San Pedro de Huaca",startsWith(city, "0501") ~ "Latacunga",
                                            startsWith(city, "0502")~"La Mana",startsWith(city, "0503") ~ "Pangua",
                                            startsWith(city, "0504")~"Pujili",startsWith(city, "0505") ~ "Salcedo",
                                            startsWith(city, "0506")~"Saquisili",startsWith(city, "0507") ~ "Sigchos",
                                            startsWith(city, "0601")~"Riobamba",startsWith(city, "0602") ~ "Alausi",
                                            startsWith(city, "0603")~"Colta",startsWith(city, "0604") ~ "Chambo",
                                            startsWith(city, "0605")~"Chunchi",startsWith(city, "0606") ~ "Guamote",
                                            startsWith(city, "0607")~"Guano",startsWith(city, "0608") ~ "Pallatanga",
                                            startsWith(city, "0609")~"Penipe",startsWith(city, "0610") ~ "Cumanda",
                                            startsWith(city, "0701")~"Machala",startsWith(city, "0702") ~ "Arenillas",
                                            startsWith(city, "0703")~"Atahualpa",startsWith(city, "0704") ~ "Balsas",
                                            startsWith(city, "0705")~"Chilla",startsWith(city, "0706") ~ "El Guabo",
                                            startsWith(city, "0707")~"Huaquillas",startsWith(city, "0708") ~ "Marcabeli",
                                            startsWith(city, "0709")~"Pasaje",startsWith(city, "0710") ~ "Piñas",
                                            startsWith(city, "0711")~"Portovelo",startsWith(city, "0712") ~ "Santa Rosa",
                                            startsWith(city, "0713")~"Zaruma",startsWith(city, "0714") ~ "Las Lajas",
                                            startsWith(city, "0801")~"Esmeraldas",startsWith(city, "0802") ~ "Eloy Alfaro",
                                            startsWith(city, "0803")~"Muisne",startsWith(city, "0804") ~ "Quininde",
                                            startsWith(city, "0805")~"San Lorenzo", startsWith(city, "0806") ~ "Atacames",
                                            startsWith(city, "0807")~"Rioverde", startsWith(city, "0901") ~ "Guayaquil",
                                            startsWith(city, "0902")~"Alfredo Baquerizo Moreno",startsWith(city, "0903") ~ "Balao",
                                            startsWith(city, "0904")~"Balzar",startsWith(city, "0905") ~ "Colimes",
                                            startsWith(city, "0906")~"Daule",startsWith(city, "0907") ~ "Duran",
                                            startsWith(city, "0908")~"El Empalme",startsWith(city, "0909") ~ "El Triunfo",
                                            startsWith(city, "0910")~"Milagro",startsWith(city, "0911") ~ "Naranjal",
                                            startsWith(city, "0912")~"Naranjito",startsWith(city, "0913") ~ "Palestina",
                                            startsWith(city, "0914")~"Pedro Carbo",startsWith(city, "0916") ~ "Samborondon",
                                            startsWith(city, "0918")~"Santa Lucia", startsWith(city, "0919") ~ "Salitre",
                                            startsWith(city, "0920")~"San Jacinto de Yaguachi", startsWith(city, "0921") ~ "Playas",
                                            startsWith(city, "0922")~"Simon Bolivar",startsWith(city, "0923") ~ "Coronel Marcelino Mariueña",
                                            startsWith(city, "0924")~"Lomas de Sargetillo",startsWith(city, "0925") ~ "Nobol",
                                            startsWith(city, "0927")~"General Antonio Elizalde",startsWith(city, "0928") ~ "Isidro Ayora",
                                            startsWith(city, "1001")~"Ibarra", startsWith(city, "1002") ~ "Antonio Ante",
                                            startsWith(city, "1003")~"Cotacachi", startsWith(city, "1004") ~ "Otavalo",
                                            startsWith(city, "1005")~"Pimampiro", startsWith(city, "1006") ~ "San Miguel de Urcuqui",
                                            startsWith(city, "1101")~"Loja", startsWith(city, "1102") ~ "Calvas",
                                            startsWith(city, "1103")~"Catamayo", startsWith(city, "1104") ~ "Celica",
                                            startsWith(city, "1105")~"Chaguarpamba", startsWith(city, "1106") ~ "Espindola",
                                            startsWith(city, "1107")~"Gonzanama", startsWith(city, "1108") ~ "Macara",
                                            startsWith(city, "1109")~"Paltas", startsWith(city, "1110") ~ "Puyango",
                                            startsWith(city, "1111")~"Saraguro", startsWith(city, "1112") ~ "Sozoranga",
                                            startsWith(city, "1113")~"Zapotillo", startsWith(city, "1114") ~ "Pindal",
                                            startsWith(city, "1115")~"Quilanga", startsWith(city, "1116") ~ "Olmedo",
                                            startsWith(city, "1201")~"Babahoyo", startsWith(city, "1202") ~ "Baba",
                                            startsWith(city, "1203")~"Montalvo", startsWith(city, "1204") ~ "Puebloviejo",
                                            startsWith(city, "1205")~"Quevedo", startsWith(city, "1206") ~ "Urdaneta",
                                            startsWith(city, "1207")~"Ventanas", startsWith(city, "1208") ~ "Vinces",
                                            startsWith(city, "1209")~"Palenque", startsWith(city, "1210") ~ "Buena Fe",
                                            startsWith(city, "1211")~"Valencia", startsWith(city, "1212") ~ "Mocache", 
                                            startsWith(city, "1213")~"Quinsaloma", startsWith(city, "1301") ~ "Portoviejo", 
                                            startsWith(city, "1302")~"Bolivar", startsWith(city, "1303") ~ "Chone", 
                                            startsWith(city, "1304")~"El Carmen", startsWith(city, "1305") ~ "Flavio Alfaro", 
                                            startsWith(city, "1306")~"Jipijapa", startsWith(city, "1307") ~ "Junin", 
                                            startsWith(city, "1308")~"Manta", startsWith(city, "1309") ~ "Montecristi", 
                                            startsWith(city, "1310")~"Pajan", startsWith(city, "1311") ~ "Pichincha", 
                                            startsWith(city, "1312")~"Rocafuerte", startsWith(city, "1313") ~ "Santa Ana", 
                                            startsWith(city, "1314")~"Sucre", startsWith(city, "1315") ~ "Tosagua", 
                                            startsWith(city, "1316")~"24 de Mayo", startsWith(city, "1317") ~ "Pedernales", 
                                            startsWith(city, "1318")~"Olmedo, Manabi", startsWith(city, "1319") ~ "Puerto Lopez", 
                                            startsWith(city, "1320")~"Jama", startsWith(city, "1321") ~ "Jaramijo",
                                            startsWith(city, "1322")~"San Vicente", startsWith(city, "1401") ~ "Morona",
                                            startsWith(city, "1402")~"Gualaquiza", startsWith(city, "1403") ~ "Limon Indanza",
                                            startsWith(city, "1404")~"Palora", startsWith(city, "1405") ~ "Santiago",
                                            startsWith(city, "1406")~"Sucua", startsWith(city, "1407") ~ "Huamboya",
                                            startsWith(city, "1408")~"San Juan Bosco", startsWith(city, "1409") ~ "Taisha",
                                            startsWith(city, "1410")~"Logroño", startsWith(city, "1411") ~ "Pablo Sexto",
                                            startsWith(city, "1412")~"Tiwintza", startsWith(city, "1501") ~ "Tena",
                                            startsWith(city, "1503")~"Archidona", startsWith(city, "1504") ~ "El Chaco",
                                            startsWith(city, "1507")~"Quijos", startsWith(city, "1509") ~ "Carlos Julio Arosemeda Tola",
                                            startsWith(city, "1601")~"Pastaza", startsWith(city, "1602") ~ "Mera",
                                            startsWith(city, "1603")~"Santa Clara",
                                            startsWith(city, "1604")~"Arajuno", startsWith(city, "1701") ~ "Quito",
                                            startsWith(city, "1702")~"Cayambe", startsWith(city, "1703") ~ "Mejia",
                                            startsWith(city, "1704")~"Pedro Moncayo", startsWith(city, "1705") ~ "Rumiñahui",
                                            startsWith(city, "1707")~"San Miguel de los Bancos", startsWith(city, "1708") ~ "Pedro Vicente Maldonado",
                                            startsWith(city, "1709")~"Puerto Quito", startsWith(city, "1801") ~ "Ambato",
                                            startsWith(city, "1802")~"Baños de Agua Santa", startsWith(city, "1803") ~ "Cevallos",
                                            startsWith(city, "1804")~"Mocha", startsWith(city, "1805") ~ "Patate",
                                            startsWith(city, "1806")~"Quero", startsWith(city, "1807") ~ "San Pedro de Pelileo",
                                            startsWith(city, "1808")~"Santiago de Pillaro", startsWith(city, "1809") ~ "Tisaleo",
                                            startsWith(city, "1901")~"Zamora", startsWith(city, "1902") ~ "Chinchipe",
                                            startsWith(city, "1903")~"Nangaritza", startsWith(city, "1904") ~ "Yacuambi",
                                            startsWith(city, "1905")~"Yantzaza", startsWith(city, "1906") ~ "El Pangui",
                                            startsWith(city, "1907")~"Centinela del Condor", startsWith(city, "1908") ~ "Palanda",
                                            startsWith(city, "1909")~"Paquisha", startsWith(city, "2001") ~ "San Cristobal",
                                            startsWith(city, "2002")~"Isabela", startsWith(city, "2003") ~ "Santa Cruz",
                                            startsWith(city, "2101")~"Lago Agrio", startsWith(city, "2102") ~ "Gonzalo Pizarro",
                                            startsWith(city, "2103")~"Putumayo", startsWith(city, "2104") ~ "Shushufindi",
                                            startsWith(city, "2105")~"Sucumbios", startsWith(city, "2106") ~ "Cascales",
                                            startsWith(city, "2107")~"Cuyabeno", startsWith(city, "2201") ~ "Francisco de Orellana",
                                            startsWith(city, "2202")~"Aguarico", startsWith(city, "2203") ~ "La Joya de los Sachas",
                                            startsWith(city, "2204")~"Loreto", startsWith(city, "2301") ~ "Santo Domingo",
                                            startsWith(city, "2302")~"La Concordia", startsWith(city, "2401") ~ "Santa Elena",
                                            startsWith(city, "2402")~"La Libertad", startsWith(city, "2403") ~ "Salinas",
                                            TRUE~"Non-Delimited Areas"))



#UEMS build until 2020
BDfull<- BDfull%>%mutate(UEMs = case_when(Canton=="Aguarico"~1,Canton=="Ambato"~2,Canton=="Arajuno"~1,
                                          Canton=="Babahoyo"~3,Canton=="Balao"~1, Canton=="Bolivar"~1,
                                          Canton=="Catamayo"~1,Canton=="Cayambe"~2,Canton=="Celica "~1,
                                          Canton=="Chone"~1, Canton=="Cotacachi"~1, Canton=="Cuenca"~6,
                                          Canton=="Cuyabeno"~1,Canton=="Daule"~1,Canton=="El Chaco"~1,
                                          Canton=="El Guabo"~1, Canton=="El Pangui"~1,Canton=="Esmeraldas"~2,
                                          Canton=="Espejo"~1,Canton=="Gonzalo Pizarro"~2,Canton=="Gualaceo"~1,
                                          Canton=="Guamote"~1,Canton=="Guano"~1,Canton=="Guaranda"~1,
                                          Canton=="Guayaquil"~9,Canton=="Huaquillas"~1,Canton=="Isidro Ayora"~1,
                                          Canton=="Jama"~1,Canton=="Jaramijo"~1,Canton=="La Joya de los Sachas"~2,
                                          Canton=="Junin"~1,Canton=="La Concordia"~1, Canton=="La Mana"~1,
                                          Canton=="La Troncal"~3,Canton=="Lago Agrio"~3,Canton=="Loja"~1,
                                          Canton=="Loreto"~1,Canton=="Macara"~1, Canton=="Macara"~1,
                                          Canton=="Manta"~1, Canton=="Mejia"~1, Canton=="Milagro"~1,
                                          Canton=="Morona"~1,Canton=="Nangaritza"~1,Canton=="Francisco de Orellana"~4,
                                          Canton=="Otavalo"~1,Canton=="Pastaza"~1,Canton=="Pedernales"~2,
                                          Canton=="Pedro Moncayo"~1,Canton=="Pedro Vicente Maldonado"~1,Canton=="Penipe"~1,
                                          Canton=="Santiago de Pillaro"~1,Canton=="Portoviejo"~1,Canton=="Pujili"~2,
                                          Canton=="Putumayo"~1,Canton=="Quevedo"~2,Canton=="Quininde"~1,
                                          Canton=="Quito"~6,Canton=="Rumiñahui"~1,Canton=="Salinas"~1,
                                          Canton=="Samborondon"~1, Canton=="Samborondon"~1,Canton=="San Juan Bosco"~1,
                                          Canton=="San Lorenzo"~1,Canton=="San Miguel"~1,Canton=="Santa Ana"~1,
                                          Canton=="Santa Elena"~2, Canton=="Santa Lucia"~1,Canton=="Santo Domingo"~1,
                                          Canton=="Saquisili"~2,Canton=="Shushufindi"~1,Canton=="Sigchos"~1,
                                          Canton=="Tena"~2,Canton=="Tulcan"~1,Canton=="San Miguel de Urcuqui"~1,
                                          Canton=="Ventanas"~1,Canton=="Yantzaza"~1,TRUE~0))




#year of building (by cantons)
BDfull<- BDfull%>%mutate(building = case_when(Canton=="San Lorenzo" ~"2009",Canton=="Otavalo" ~"2009",
                                              Canton=="Huaquillas" ~"2011",Canton=="Macara"~"2011",Canton=="Penipe"~"2011",
                                              Canton=="Cotacachi"~"2012",  Canton=="Santo Domingo"~"2012",
                                              Canton=="Cuyabeno"~"2013", Canton=="La Concordia"~"2013",Canton=="Morona"~"2013",Canton=="Pastaza"~"2013",
                                              Canton== "Pedro Vicente Maldonado"~"2013",Canton=="San Miguel"~"2013",
                                              Canton=="Aguarico"~"2014", Canton=="Ambato"~"2014",Canton== "Bolivar"~"2014",Canton=="El Chaco"~"2014",
                                              Canton=="El Guabo"~"2014",Canton=="Gualaceo"~"2014",Canton=="Guano"~"2014",
                                              Canton=="Guaranda"~"2014",Canton=="Junin"~"2014",Canton=="Manta"~"2014",
                                              Canton=="Quevedo"~"2014",Canton=="Shushufindi"~"2014",Canton=="Tulcan"~"2014",Canton=="Yantzaza"~"2014",
                                              Canton=="El Pangui" ~"2015",Canton=="Isidro Ayora" ~"2015", Canton=="Jaramijo"~"2015",
                                              Canton=="Loja" ~"2015", Canton=="San Juan Bosco"~"2015", Canton=="Santa Elena" ~"2015",Canton=="Tena" ~"2015",
                                              Canton=="Nangaritza"~"2016",Canton=="Putumayo"~"2016",
                                              Canton=="Babahoyo"~"2017",Canton=="Catamayo"~"2017",Canton=="Celica"~"2017",Canton=="Chone"~"2017",
                                              Canton=="Espejo"~"2017",Canton=="Guamote"~"2017", Canton=="Jama"~"2017",Canton=="La Joya de los Sachas"~"2017",
                                              Canton=="La Mana"~"2017", Canton=="Lago Agrio"~"2017",  Canton=="Loreto"~"2017",
                                              Canton=="Mejia"~"2017",  Canton=="Francisco de Orellana"~"2017",Canton=="Santiago de Pillaro"~"2017",
                                              Canton=="Portoviejo"~"2017", Canton=="Pujili"~"2017", Canton=="Quininde"~"2017",
                                              Canton=="Saquisili"~"2017", Canton=="Sigchos"~"2017",
                                              Canton=="Arajuno"~"2018", Canton=="Cayambe"~"2018", Canton=="Cuenca"~"2018",Canton=="Esmeraldas"~"2018",
                                              Canton=="Pedro Moncayo"~"2018", Canton=="Santa Ana"~"2018", Canton=="Ventanas"~"2018",
                                              Canton=="Balao"~"2019",Canton=="Daule"~"2019",Canton=="Gonzalo Pizarro"~"2019",Canton=="Guayaquil"~"2019",
                                              Canton=="La Troncal"~"2019",Canton=="Rumiñahui"~"2019",Canton=="Salinas"~"2019",
                                              Canton=="Santa Lucia"~"2019",Canton=="San Miguel de Urcuqui"~"2019",
                                              Canton=="Samborondon"~"2020"))


write.csv(BDfull,"Documents/Memoire/BD\\DataBase.csv", row.names = FALSE)

#reordering 
colnames(BDfull)
BDfull <- BDfull[, c(11, 1, 12, 2, 3, 4, 5, 14, 6, 13, 7, 8, 9, 10 )]

                                          
                                            

                                            

                                            
                                            
                                            
                                            
                                            
                                    
                                            
                           
                                           
                         
                                          










