library("writexl")
install.packages("xlsx")
library("xlsx")


write.csv(contracts1,"C:\\Users\\Stepan Polikanov\\Documents\\cntrcts.csv")

write.csv(data_omsk3, file = "CC:\\Users\\Stepan Polikanov\\Documents\\cntrcts.csv",
           sheetName = "Omsk", append = FALSE)
write_xlsx(contracts1, 'C:\\Users\\Stepan Polikanov\\Documents\\cntrcts.xlsx')
dir.create(tempdir())
x

workdata <- read.csv(file.choose())
