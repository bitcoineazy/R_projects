###### HISTOGRAMS ######
#�������� ������
dat <- read.csv("/home/noble6/DEV/R_projects/Graphics/financials.csv")
View(dat)
#������ ����������� �������� ����� � ���������� � ���������

hist(dat$Dividend.Yield)

hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts")
     
# ������� ������    
colors()
#
?hist()
# breaks �������� �� ���� ���������� �������� 
hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20)

# (a, b] right = TRUE
# (10, 20] (20, 30]
# [a, b) right = FALSE
# [10, 20) [20, 30)
# right �������� ��� �������� ��������
hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20, 
     right  = FALSE)
# freq �� ��� y �� ������� � �������� ���������  
hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20, 
     right = FALSE, freq = FALSE)
#������� ������� �������� ����� 
hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20, 
     right = FALSE, freq = FALSE, border = "navy")
#������� �� ����������� ������� �������� ������� ������

hist(dat$Dividend.Yield, main = "Dividends", col = "hotpink", 
     xlab = "Dividend Yield", ylab = "Counts", breaks = 20, 
     right = FALSE, freq = FALSE, border = "navy")
abline(v = mean(dat$Dividend.Yield), col = "limegreen", 
       lty = 2, lwd = 2)

??lty
