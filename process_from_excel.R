#PROCESS ONS REGIONAL/SECTOR GVA EXCEL SHEET DATA INTO A MORE R-FRIENDLY FORMAT
library(tidyverse)

#Each table from the original ONS file has been manually edited so it's a well formatted CSV and saved separately.

#Using 'current prices' for location quotients (LQs). LQs are purely proportional at single time points
#So across-time comparisons for proportions, not the nominal values (which you can't do with current prices as they don't include inflation)
#The GVA current price values actually sum correctly across industries and within regions (unlike chained volume values) - without that, LQ not possible


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ITL2 CURRENT PRICE DATA PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Remove any rows that are totals of other rows
#Only want to keep unique SIC values that then sum to the national total
#Different regional scales have slightly different SIC categories, so need a different remove list for each
ITL2_SICremoves = c(
  'Total',#leave this in the CSV by commenting out, to check the categories left over total correctly in lines 42-63 below (then can remove by uncommenting)
  'A-E',
  'A (1-3)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
  'F (41-43)',
  'G-T',
  'G (45-47)',
  'H (49-53)',
  'I (55-56)',
  'J (58-63)',
  'K (64-66)',
  'L (68)',#real estate activities - leaves in "Real estate activities, excluding imputed rental" & "Owner-occupiers' imputed rental" as separate categories
  'M (69-75)',
  'N (77-82)',
  'Q (86-88)',
  'R (90-93)',
  'S (94-96)'
)

#Load the current price data
itl2.cp <- read_csv('data/Table 2c ITL2 UK current price estimates pounds million.csv')

#Filter out duplicate value rows and make long by year
#Also convert year to numeric
itl2.cp <- itl2.cp %>% filter(!`SIC07 code` %in% ITL2_SICremoves) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Check totals match... 
# chk1 <- itl2.cp %>%
#   filter(`SIC07 code`!='Total') %>%
#   group_by(year, `ITL region name`) %>%
#   summarise(sum = sum(value))
# 
# chk2 <- itl2.cp %>%
#   filter(`SIC07 code`=='Total')
# 
# both <- chk1 %>%
#   left_join(
#     chk2,
#     by = c('year','ITL region name')
#   )
# 
# #tick, within rounding error
# max(abs(both$sum-both$value))
# table(abs(both$sum-both$value)==0)




#Check on just water and air transport, where the neg values are
# itl2.cp %>% filter(`SIC07 description` == "Water and air transport") %>% View

#RANDOM NEGATIVE VALUES IN THERE
#Looking, I think just typos given previous data
#(And also makes no logical sense, so...)
# itl2.cp %>% filter(value < 0)

#NA any negative values in GVA, don't think can be correct
#Only some 2021 values for water transport
itl2.cp <- itl2.cp %>% 
  mutate(value = ifelse(value < 0, NA, value))

#Turn any columns with spaces into snake case
names(itl2.cp) <- gsub(x = names(itl2.cp), pattern = ' ', replacement = '_')

write_csv(itl2.cp, 'data/ITL2currentprices_long.csv')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ITL3 CURRENT PRICE DATA PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Remove any rows that are totals of other rows
#Only want to keep unique SIC values that then sum to the national total
#Different regional scales have slightly different SIC categories, so need a different remove list for each
ITL3_SICremoves = c(
  'Total',#leave this in the CSV by commenting out, to check the categories left over total correctly in lines 42-63 below (then can remove by uncommenting)
  'A-E',
  'C (10-33)',
  'F (41-43)',
  'G-T',
  'G (45-47)',
  'H (49-53)',
  'I (55-56)',
  'J (58-63)',
  'K (64-66)',
  'L (68)',#real estate activities - leaves in "Real estate activities, excluding imputed rental" & "Owner-occupiers' imputed rental" as separate categories
  'M (69-75)',
  'N (77-82)',
  'Q (86-88)',
  'R (90-93)',
  'S (94-96)'
)

#Using 'current prices' - LQs are purely proportional at single time points
#So across-time comparisons only matter for the proportions, not the nominal values
#Given that - the GVA current price values actually sum correctly across industries and within regions (unlike chained volume)
itl3.cp <- read_csv('data/Table 3c ITL3 UK current price estimates pounds million.csv')

#Filter out duplicate value rows and make long by year
#Also convert year to numeric
itl3.cp <- itl3.cp %>% filter(!`SIC07 code` %in% ITL3_SICremoves) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Check totals match... 
# chk1 <- itl3.cp %>% 
#   filter(`SIC07 code`!='Total') %>% 
#   group_by(year, `ITL region name`) %>% 
#   summarise(sum = sum(value))
# 
# chk2 <- itl3.cp %>% 
#   filter(`SIC07 code`=='Total')
# 
# both <- chk1 %>% 
#   left_join(
#     chk2,
#     by = c('year','ITL region name')
#   )

#tick, within rounding error
# max(abs(both$sum-both$value))
# table(abs(both$sum-both$value)==0)


#Check on just water and air transport, where the neg values are
# itl3.cp %>% filter(`SIC07 description` == "Water and air transport") %>% View

#RANDOM NEGATIVE VALUES IN THERE
#Looking, I think just typos given previous data
#(And also makes no logical sense, so...)
# itl3.cp %>% filter(value < 0)

#Any neg values in ITL3 like with 2? Yup. Water again.
# table(itl3.cp$value < 0)
# itl3.cp %>% filter(value < 0) %>% View

#NA any negative values in GVA
#Only 2021 values for water transport, for four places, again, same as itl2
itl3.cp <- itl3.cp %>% 
  mutate(value = ifelse(value < 0, NA, value))

#Turn any columns with spaces into snake case
names(itl3.cp) <- gsub(x = names(itl3.cp), pattern = ' ', replacement = '_')

write_csv(itl3.cp, 'data/ITL3currentprices_long.csv')





