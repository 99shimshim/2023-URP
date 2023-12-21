setwd('C:/Users/mrg07/Desktop')

library(mice)

final_na <- read.csv(file = '최종모델링용_with_nas.csv', header = T, encoding = 'utf-8')
final_na <- subset(final_na, select=-Target)
summary(final_na)

final_imp = mice(final_na, method = 'rf')

#결측치가 채워진 5개 자료 중에 3번째 자료 저장
final_comp <- complete(data = final_imp, 3)
summary(final_comp)

#모든 결측치가 대체되었는지 확인
sum(is.na(final_comp))


final_na <- read.csv(file = '최종모델링용_with_nas.csv', header = T, encoding = 'utf-8')

final_comp$Target <- final_na$Target

write.csv(final_comp,file = '[URP]모델링용_결측치대체완.csv')
