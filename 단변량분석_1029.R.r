library(scorecard)
library(creditmodel)
library(ggplot2)
library(moonBook)


# 데이터로드

df_train <- read.csv("./넘블_훈련용_1030.csv", fileEncoding = "UTF-8")

df_test <- read.csv("./넘블_시험용_1030.csv", fileEncoding = "UTF-8")



# 타겟 비율

table(df_train$TARGET)

prop.table(table(df_train$TARGET))


# 타겟 비율

table(df_test$TARGET)

prop.table(table(df_test$TARGET))



# 단변량 분석

x_list <- get_x_list(dat_train = df_train, ex_cols = c("사업자번호",  # key 컬럼
                                                       'CMP_NM', # 타겟 
                                                       'CMP_ENM', # 타겟
                                                       'TARGET')) # key 컬럼


psi_filter <- psi_iv_filter(dat= df_train, dat_test = df_test, target = "TARGET", x_list = x_list, iv_i = 0.01, pos_flag = "1", psi_i = 0.05, cos_i = 0.99)


psi_filter         

# IV 0.5 미만 변수만 추출

psi_filter_min <- psi_filter[(psi_filter$IV <= 0.5 ), ]

x_list_filter <- psi_filter_min$Feature

write.csv(psi_filter_min, './넘블_단변량_분석결과_iv_0.01_1029.csv', fileEncoding = 'CP949')


# 상관분석(1/2)

cor_mat <- cor(df_train[,c(x_list_filter)], use = "complete.obs")

cor_mat

# write.csv(cor_mat, './corr_139개_0926.csv')

cor_heat_plot(cor_mat)


# cor_selector

x_list_filter_corr <- high_cor_selector(
  cor_mat,
  p = 0.7,
  x_list = NULL,
  com_list = psi_filter_min,
  retain = TRUE
)

length(x_list_filter_corr)

write.csv(x_list_filter_corr, './corr_selector_1030.csv')


# 상관분석(2/2)

cor_mat_2 <- cor(df_train[,c(x_list_filter_corr)], use = "complete.obs")

cor_mat_2

cor_heat_plot(cor_mat_2)

# break

breaks_list=list(
  미수금_median=c("3934.5", "22806.5"), 
  재고자산_mean=c("305742", "1140485", "2937441"), 
  유보액_납입자본_max=c("331.832", "1054.567"), 
  자본금_max=c("200000", "450000", "950000"), 
  매출채권_min=c("178776", "802610", "1884629"), 
  비유동부채비율_min=c("22.434", "76.697", "1e+12"), 
  기업순이익률_min=c("-5.405", "0.564", "3.023", "7.405"), 
  매출액영업이익률_max=c("1.8", "9.963"), 
  법인세비용_min=c("71094"), 
#  부채총계대매출액_max=c("34.368", "69.179", "150.066", "1950.2"), 
  총자산영업이익률_max=c("0.7198033724", "3.791336561"), 
#  총자산영업이익률_min=c("-6.260412802", "1.77612717", "4.989960998"), 
  영업비율_min=c("82.288", "91.99", "96", "99.342"), 
  총자산이익률_min=c("-0.08596560594", "-0.01503547101", "0.03841768139"), 
  총자산이익률_median=c("-0.02017402078", "0.008317390962", "0.07051286665"), 
  유보액_납입자본_median=c("772.984"), 
  총자산이익률_max=c("0.004231324732", "0.0302239115"), 
  수지비율_min=c("92.209", "96.514", "99.508"), 
  순운전자본비율_min=c("31.309"), 
  유보액_납입자본_min=c("553.486"), 
  기업순이익률_max=c("3.2", "1e+12"), 
  총자산회전율_min=c("0.136649166", "1.539870552"), 
  부채총계대매출액_median=c("26.866", "761.876"), 
  자기자본비율_min=c("18.622", "85.182"), 
  순운전자본비율_mean=c("41.58225"), 
#  금융비용대매출액비율_mean=c("0.528"), 
  HDOF_BR_GB=c("1", "2", "3"), 
  VENT_YN=c("1"), 
  재고자산회전율_max=c("8.274", "1e+12"), 
  중소기업계약학과참여기업여부=c("1"), 
  FR_IVST_CORP_YN=c("1"), 
#  ESTB_GB=c("1"), 
  녹색제품인증기업지정이력=c("1"), 
  인재육성형중소기업지정이력=c("1")
)


x_list_filter_corr <- names(breaks_list)

length(x_list_filter_corr)


# 훈련용

coarse_class_train <- woebin(df_train, 
                             y = "TARGET", 
                             x = x_list_filter_corr, 
                             positive = "1", 
                             #save_breaks_list = './train',
                             breaks_list = breaks_list,
                             method = "freq", 
                             count_distr_limit = 0.01, 
                             bin_num_limit = 5)


# 시험용

coarse_class_test <- woebin(df_test, 
                            y = "TARGET", 
                            x = x_list_filter_corr,
                            positive = "1",
                            breaks_list = breaks_list,
                            method = "freq",
                            count_distr_limit = 0.01,
                            bin_num_limit = 5)



# plotting

# coarse class plotting

coarse_class_train_plot <- woebin_plot(coarse_class_train, title = 'coarse_train')

coarse_class_test_plot <- woebin_plot(coarse_class_test, title = 'coarse_test')




coarse_class_train_plot[1]

coarse_class_test_plot[1]


coarse_class_train_plot[2]

coarse_class_test_plot[2]


coarse_class_train_plot[3]

coarse_class_test_plot[3]


coarse_class_train_plot[4]

coarse_class_test_plot[4]


coarse_class_train_plot[5]

coarse_class_test_plot[5]


coarse_class_train_plot[6]

coarse_class_test_plot[6]


coarse_class_train_plot[7]

coarse_class_test_plot[7]


coarse_class_train_plot[8]

coarse_class_test_plot[8]


coarse_class_train_plot[9]

coarse_class_test_plot[9]


coarse_class_train_plot[10]

coarse_class_test_plot[10]


coarse_class_train_plot[11]

coarse_class_test_plot[11]


coarse_class_train_plot[12]

coarse_class_test_plot[12]


coarse_class_train_plot[13]

coarse_class_test_plot[13]


coarse_class_train_plot[14]

coarse_class_test_plot[14]


coarse_class_train_plot[15]

coarse_class_test_plot[15]


coarse_class_train_plot[16]

coarse_class_test_plot[16]


coarse_class_train_plot[17]

coarse_class_test_plot[17]


coarse_class_train_plot[18]

coarse_class_test_plot[18]


coarse_class_train_plot[19]

coarse_class_test_plot[19]


coarse_class_train_plot[20]

coarse_class_test_plot[20]


coarse_class_train_plot[21]

coarse_class_test_plot[21]


coarse_class_train_plot[22]

coarse_class_test_plot[22]


coarse_class_train_plot[23]

coarse_class_test_plot[23]


coarse_class_train_plot[24]

coarse_class_test_plot[24]


coarse_class_train_plot[25]

coarse_class_test_plot[25]


coarse_class_train_plot[26]

coarse_class_test_plot[26]


coarse_class_train_plot[27]

coarse_class_test_plot[27]


coarse_class_train_plot[28]

coarse_class_test_plot[28]


coarse_class_train_plot[29]

coarse_class_test_plot[29]


coarse_class_train_plot[30]

coarse_class_test_plot[30]


coarse_class_train_plot[31]

coarse_class_test_plot[31]





# woe로 구성된 데이터셋 만들기

train <- df_train[,c(x_list_filter_corr, "TARGET")]

train_woe <- woebin_ply(train, coarse_class_train)

# 시험용 데이터

test <- df_test[,c(x_list_filter_corr, "TARGET")]


# 로지스틱 회귀분석

logistic <- glm(TARGET ~ .,
                family = binomial(),
                data = train_woe)


summary(logistic)

vif(logistic)

df_vif <- vif(logistic)

write.csv(df_vif, './넘블_vif_수정전_1031.csv')

# 오즈비 시각화

ORplot(logistic, type = 2, show.CI = TRUE, main='Plot for Odds Ratio')


logistic_step <- step(logistic)

summary(logistic_step)

vif(logistic_step)

df_vif <- vif(logistic_step)

write.csv(df_vif, './넘블_vif_수정후_1031.csv')

# 오즈비 시각화

ORplot(logistic_step, type = 2, show.CI = TRUE, main='Plot for Odds Ratio')



# 로지스틱 모형(21항목 전체)으로 스코어카드 산출

score_card <- scorecard(coarse_class_train, logistic_step)

score_card

# 스코어카드 적용

score <- scorecard_ply(train, score_card, only_total_score = FALSE, var_kp = c("TARGET"))

score$TARGET <- as.factor(score$TARGET)


# (train)스코어 분포

ggplot(score, aes(x=score, fill = TARGET, color = TARGET)) +
  geom_line(stat = "density") + 
  geom_density(alpha = .5) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("훈련용 스코어 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))



# 모형성능평가
perf_eva(pred = score$score, label = score$TARGET,
         show_plot = c('ks', 'lift', 'gain', 'roc', 'lz', 'pr', 'f1', 'density'), title = 'train')



# (test)스코어카드 적용
test_score <- scorecard_ply(test, score_card, only_total_score = FALSE, var_kp = c("TARGET"))

test_score$TARGET <- as.factor(test_score$TARGET)


# (test)스코어 분포

ggplot(test_score, aes(x=score, fill = TARGET, color = TARGET)) +
  geom_line(stat = "density") + 
  geom_density(alpha = .5) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("시험용 스코어 분포") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="스코어", y="인원수") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))



# 모형성능평가
perf_eva(pred = test_score$score, label = test_score$TARGET,
         show_plot = c('ks', 'lift', 'gain', 'roc', 'lz', 'pr', 'f1', 'density'), title = 'test')



# psi 평가
########## train-test
# expect : 기준시점(데이터셋 : score), actual : 비교되는 실제 시점

psi_test <- perf_psi(score = list(actual = test_score$score, expect = score$score), 
                     label = list(actual = test_score$TARGET, expect = score$TARGET),
                     show_plot = TRUE, title = 'train-test', positive = "1", threshold_variable = 20)


psi_test


# 리포트 산출(훈련용)

report(list(train=df_train), "TARGET", x_list_filter_corr, breaks_list, seed = 222, save_report = '모형정의서_1101')


report(list(train=df_train, test=df_test), "TARGET", x_list_filter_corr, breaks_list, seed = 222, save_report = '모형정의서_시험용포함_1101')




# 10등급

grade_train_10 <-  gains_table(score = score$score, 
                               label = score$TARGET, 
                               positive = "1", 
                               breaks_by = c(893, 799, 704, 610, 515, 420, 340, 280, 180))

grade_train_10


grade_test_10 <-  gains_table(score = test_score$score, 
                              label = test_score$TARGET, 
                              positive = "1", 
                              breaks_by = c(893, 799, 704, 610, 515, 420, 340, 280, 180))

grade_test_10

