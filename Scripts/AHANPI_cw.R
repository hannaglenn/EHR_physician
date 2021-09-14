library(dplyr)
library(readr)

# -------------------------------   Create NPI-AHA Crosswalk -----------------------------
#                                   Hanna Glenn
#                                   8/27/2021

# This script reads in data from the AHA survey with connected NPI numbers. Many hospitals
# will skip this question in all or some years. I will fill in any NPI from NPI lookup
# that are missing and match in name and location. I will also observe when there are 
# multiple NPIs connected to one AHAID and address this. The resulting dataset is 
# "AHANPI_cw.csv". 

AHA.NPI <- read_csv(paste0(path.currentdata,"AHA-NPI.csv"))
  # 44077 obs of 10 variables

AHA.NPI <- AHA.NPI %>%
  filter(MSTATE %in% c("AL","AK","AZ","AR","CA","CO", "CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
                       "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")) %>%
  rename(AHAID=ID, Hospname=MNAME, Street=MLOCADDR, City=MLOCCITY, Beds=BDTOT, State=MSTATE, Zip=MLOCZIP, NPI=NPINUM) %>%
  select(YEAR, AHAID, NPI, Hospname, Street, City, State, Zip, Beds)
  # 43615 obs of 9 variables

# Create a dataset to see how many AHA IDs are missing an NPI (number of obs gives number of unique AHA IDs)
num_missing_npi <- AHA.NPI %>%
  group_by(AHAID) %>%
  filter(is.na(NPI)) %>%
  count() 
  # 3162 obs 

# Manually fill in missing NPIs that have a match by name and address in NPI Lookup
AHA.NPI <- AHA.NPI %>%
  mutate(NPI=ifelse(AHAID==6110150 & YEAR==2009, 1023057809, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6110150 & YEAR==2010, 1023272853, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6110150 & YEAR==2009, 1023057809, NPI))  %>%
  mutate(NPI=ifelse(AHAID==6120275, 1134332315, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6130212 & YEAR==2009, 1386689875, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6130212 & YEAR==2010, 1538442199, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6130250, 1982743969, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6140002, 11417972647, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6140014, 1770731937, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6140023, 1902308174, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6140221, 1265830061, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6140330, 1134351786, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6140990, 1225181860, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6142210, 1437216132, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6142315, 1134281280, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6144001, 1225002983, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6160370, 1982658035, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6160727, 1174516041, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6210021, 1609312875, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6210057, 1386640084, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6210240, 1437383692, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6210250 & YEAR==2009, 1215021225, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6210250 & YEAR==2010, 1306930318, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6210665, 14877724712, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6210970, 1700886322, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6211145, 1437383692, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6211775, 1992707608, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6212230 & YEAR==2009, 1548220908, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6212230 & YEAR==2010, 1558321919, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6213055, 1316112097, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6213710, 1770593956, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6214053 & YEAR==2009, 1659360709, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6214053 & YEAR==2010, 1891842308, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6214650, 1275553521, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6214765, 1346213469, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6220021, 1962580803, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6220024, 1093713521, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6220160, 1467440743, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6220234, 1578543468, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6220237, 1568762961, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6221360, 1205880200, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6229075, 1770515165, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230044, 1295804219, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230059, 1497754667, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230066, 1447329461, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230067, 1073682001, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230072, 1225037294, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230074, 1689673550, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230076, 1477553329, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230123, 1144391756, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230455, 1265664544, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230510 & YEAR==2009, 1326017104, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230510 & YEAR==2010, 1841268877, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230515, 1104192590, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230516, 1427097864, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6230979, 1013985126, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6231965, 1114953619, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6233240, 1104921428, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6310011, 1396745725, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6320440, 1215926431, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6340008 & YEAR==2009, 1437196508, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6340008 & YEAR==2010, 1528005691, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6340023 & YEAR==2009, 1275866584, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6340023 & YEAR==2010, 1376876664, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6340233, 1881631943, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6340340, 1780694372, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6340470 & YEAR==2009, 1043274475, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6340470 & YEAR==2010, 1124082417, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6360027, 1306845482, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6360028, 1457351884, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6360375, 1588605141, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6360427, 1770640575, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6380026, 1750383584, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6380030, 1609001312, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6380033, 1588664007, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6380610, 1477726107, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6380655, 1982873212, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6380715, 1275587263, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390048, 1386681468, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390135, 1356336069, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390146, 1275689820, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390147, 1598964157, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390148, 1992990972, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390265, 1326027723, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390489, 1144274879, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390615, 1720019995, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390679, 1932138823, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390700, 1689611501, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6390843, 1144274317, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6391130, 1770515991, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410022 & YEAR==2009, 1164436234, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410022 & YEAR==2010, 1184638942, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410038, 1326118597, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410039, 1134152234, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410059, 1871592386, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410062, 1043210495, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410064, 1861492217, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410066, 1649276593, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410196, 1447302013, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410197, 1285785535, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6410730, 1124117205, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6411725 & YEAR==2009, 1295044733, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6411725 & YEAR==2010, 1790775161, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420009, 1558446021, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420030, 1003944109, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420037, 1104825140, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420038, 1073565131, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420041, 1205816832, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420042, 1063414886, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420051, 1033115605, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420066, 1760659205, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420225, 1427114883, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6420289, 1952447103, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6421330, 1578617627, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6421460 & YEAR==2009, 1154395333, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6421460 & YEAR==2010, 1336113513, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6430400, 1982643961, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6431045, 1477690873, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6431735, 1952409765, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6440028, 1609800614, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6440031, 1912906934, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6440037, 1720088370, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6440031, 1912906934, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6440045, 1205836806, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6440055, 1407803810, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6441870, 1417035536, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6451630, 1154415024, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6452230, 1124063573, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6510255, 1225161524, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6510710, 1184789034, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6510970, 1306879614, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520017, 1801960315, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520033, 1902806722, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520041, 1588664999, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520169, 1942442710, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520297, 1548581598, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520353 & YEAR==2010, 1558672543, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520353 & YEAR==2011, 1710905609, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520649, 1558499483, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6520920, 1164590386, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6521111, 1104804566, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6521220, 1366441370, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6530530, 1952427874, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6530769 & YEAR==2010, 1366511156, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6530769 & YEAR==2011, 1942380357, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6530810 & YEAR==2011, 1114095049, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6530810 & YEAR==2010, 1316015241, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6539015, 1164423679, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540027, 1255330163, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540029, 1518967280, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540135, 1992706303, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540165, 1215965249, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540413, 1154560290, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540505 & YEAR==2010, 1649503699, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540505 & YEAR==2011, 1669705612, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540607, 1811924954, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540610, 1619922283, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6540755, 1306882246, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6549080, 1144204561, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6549170, 1053450320, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6549175, 1932143989, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6610687, 1730248907, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6610840, 1649220724, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6610910, 1770585382, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6640011, 1508205741, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6640056, 1154352169, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6640215, 1982674560, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6640436, 1043280696, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6650015, 1407819253, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6650018, 1053311597, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6660014, 1518967124, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6660100, 1093802688, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6660115, 1609816149, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6660138, 1609007525, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6660780, 1609965805, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6660810, 1639101199, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6660995 & YEAR==2010, 1255470092, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6660995 & YEAR==2011, 1801867494, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6669150, 1831188705, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720018, 1376511493, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720041, 1841244092, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720097, 1275700189, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720129, 1629039193, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720136, 1912951112, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720154, 1376540153, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720158, 1134126659, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720169, 1588631311, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720171, 1700975083, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720183, 1285835561, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720192, 1851487086, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720200, 1366506826, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720205, 1518960806, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720246, 1588614036, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720318, 1093793408, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720362, 1558388470, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720366, 1982695441, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6720391, 1831283670, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6730046, 1699745893, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6730460, 1811253206, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6730574, 1144268723, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6741390, 1669472387, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6743755, 1205999232, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6810122, 1356536312, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6820008, 1720040132, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6820011, 1114925047, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6820018, 1023079092, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6820085, 1891732905, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6840029, 1508842964, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6840465, 1861577439, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6840980, 1083640239, NPI))%>%
  mutate(NPI=ifelse(AHAID==6849170, 1619962321, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6850023, 1083787345, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6850800, 1225103922, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860008, 1891795415, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860012, 1336118322, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860024, 1396745832, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860032, 1295709954, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860034, 1073572590, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860047, 1235212721, NPI)) %>%
  mutate(NPI=ifelse(AHAID==8680053, 1245273853, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860153 & YEAR==2010, 1740293950, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860153 & YEAR==2011, 1780761866, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860263, 1932543428, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6860405, 1043264278, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6870007, 1669446563, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6870190, 1487607669, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6870372, 1770821571, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6880006, 1831189638, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6880073 & YEAR==2010, 1104114925, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6880073 & YEAR==2011, 1417947490, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6880075 & YEAR==2010, 1154317964, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6880075 & YEAR==2011, 1790073575, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6880150, 1750498010, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6889060 & YEAR==2010, 1619050549, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6889060 & YEAR==2011, 1720037799, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6910165, 1760485221, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6920007, 1558436006, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6930039, 1881781482, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6930065, 1205834694, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6930090, 1881786366, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6930830, 1386746337, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6930955, 1508939349, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6930970, 1275578817, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6931570, 1770543761, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6932620, 1194758623, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6932980, 1154428688, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6933488 & YEAR==2010, 1114059557, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6933488 & YEAR==2011, 1780668434, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6933610, 1538163886, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6933695, 1245203447, NPI)) %>%
  mutate(NPI=ifelse(AHAID==6939972, 1952311953, NPI)) 

num_missing_npi <- AHA.NPI %>%
  group_by(AHAID) %>%
  filter(is.na(NPI)) %>%
  count() 
  # Now there are 2959 unique AHA IDs that are missing an NPI (filled in 203 of them)

# Create a dataset that reveals which NPIs are linked to which AHA IDs
NPItoAHAgrouping <- AHA.NPI %>%
  group_by(NPI) %>%
  summarize(AHAvalues = paste(sort(unique(AHAID)),collapse=','))

# Filter this dataset to only the observations without a unique AHA ID
NPItoAHA_multiples <- NPItoAHAgrouping %>%
  filter(str_detect(AHAvalues,","))
# There are 53 hospital NPIs that are linked to multiple different AHA values. 
# These pose a problem in linking NPI to AHA because we don't 
# know which one is correct. These could occur due to a merger or input error. 
# I drop these observations. 
# TO DO: THINK ABOUT THE IMPLICATIONS OF DROPPING THESE IF THEY ARE MERGERS

# Filter to observations where NPI is not missing (these couldn't be matched in the NPI lookup)
AHA.NPI <- AHA.NPI %>%
  filter(!is.na(NPI))
  # 31,818 obs (lost over 10,000 AHAs)

# Merge the NPI multiples dataset back to the AHA.NPI dataset to filter
AHA.NPI <- AHA.NPI %>%
  ungroup() %>%
  left_join(NPItoAHA_multiples, by="NPI") %>%
  filter(is.na(AHAvalues)) %>%
  mutate(NPI=as.numeric(NPI))
  # 31,339 obs (only lost 479 obs)

# Finally, here is the crosswalk I'll merge to the shared patient data. 
# Since I only have unique NPI, AHA pairs, I can use min, max, mean... they will all be the same
AHA.NPI_cw <- AHA.NPI %>%
  group_by(NPI) %>%
  summarize(AHAID=min(AHAID))
  # 5,810 obs of 11 variables

saveRDS(AHANPI_cw,file="AHANPI_cw.rds")
write.csv(AHA.NPI_cw,file="AHANPI_cw.csv")


