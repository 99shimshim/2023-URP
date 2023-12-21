library(scorecard)
library(creditmodel)
library(ggplot2)
library(moonBook)


# �����ͷε�

df_train <- read.csv("./�Ѻ�_�Ʒÿ�_1030.csv", fileEncoding = "UTF-8")

df_test <- read.csv("./�Ѻ�_�����_1030.csv", fileEncoding = "UTF-8")



# Ÿ�� ����

table(df_train$TARGET)

prop.table(table(df_train$TARGET))


# Ÿ�� ����

table(df_test$TARGET)

prop.table(table(df_test$TARGET))



# �ܺ��� �м�

x_list <- get_x_list(dat_train = df_train, ex_cols = c("����ڹ�ȣ",  # key �÷�
                                                       'CMP_NM', # Ÿ�� 
                                                       'CMP_ENM', # Ÿ��
                                                       'TARGET')) # key �÷�


psi_filter <- psi_iv_filter(dat= df_train, dat_test = df_test, target = "TARGET", x_list = x_list, iv_i = 0.01, pos_flag = "1", psi_i = 0.05, cos_i = 0.99)


psi_filter         

# IV 0.5 �̸� ������ ����

psi_filter_min <- psi_filter[(psi_filter$IV <= 0.5 ), ]

x_list_filter <- psi_filter_min$Feature

write.csv(psi_filter_min, './�Ѻ�_�ܺ���_�м����_iv_0.01_1029.csv', fileEncoding = 'CP949')


# ����м�(1/2)

cor_mat <- cor(df_train[,c(x_list_filter)], use = "complete.obs")

cor_mat

# write.csv(cor_mat, './corr_139��_0926.csv')

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


# ����м�(2/2)

cor_mat_2 <- cor(df_train[,c(x_list_filter_corr)], use = "complete.obs")

cor_mat_2

cor_heat_plot(cor_mat_2)

# break

breaks_list=list(
  �̼���_median=c("3934.5", "22806.5"), 
  ����ڻ�_mean=c("305742", "1140485", "2937441"), 
  ������_�����ں�_max=c("331.832", "1054.567"), 
  �ں���_max=c("200000", "450000", "950000"), 
  ����ä��_min=c("178776", "802610", "1884629"), 
  ��������ä����_min=c("22.434", "76.697", "1e+12"), 
  ��������ͷ�_min=c("-5.405", "0.564", "3.023", "7.405"), 
  ����׿������ͷ�_max=c("1.8", "9.963"), 
  ���μ����_min=c("71094"), 
#  ��ä�Ѱ������_max=c("34.368", "69.179", "150.066", "1950.2"), 
  ���ڻ꿵�����ͷ�_max=c("0.7198033724", "3.791336561"), 
#  ���ڻ꿵�����ͷ�_min=c("-6.260412802", "1.77612717", "4.989960998"), 
  ��������_min=c("82.288", "91.99", "96", "99.342"), 
  ���ڻ����ͷ�_min=c("-0.08596560594", "-0.01503547101", "0.03841768139"), 
  ���ڻ����ͷ�_median=c("-0.02017402078", "0.008317390962", "0.07051286665"), 
  ������_�����ں�_median=c("772.984"), 
  ���ڻ����ͷ�_max=c("0.004231324732", "0.0302239115"), 
  ��������_min=c("92.209", "96.514", "99.508"), 
  �������ں�����_min=c("31.309"), 
  ������_�����ں�_min=c("553.486"), 
  ��������ͷ�_max=c("3.2", "1e+12"), 
  ���ڻ�ȸ����_min=c("0.136649166", "1.539870552"), 
  ��ä�Ѱ������_median=c("26.866", "761.876"), 
  �ڱ��ں�����_min=c("18.622", "85.182"), 
  �������ں�����_mean=c("41.58225"), 
#  �����������׺���_mean=c("0.528"), 
  HDOF_BR_GB=c("1", "2", "3"), 
  VENT_YN=c("1"), 
  ����ڻ�ȸ����_max=c("8.274", "1e+12"), 
  �߼ұ������а������������=c("1"), 
  FR_IVST_CORP_YN=c("1"), 
#  ESTB_GB=c("1"), 
  �����ǰ������������̷�=c("1"), 
  �����������߼ұ�������̷�=c("1")
)


x_list_filter_corr <- names(breaks_list)

length(x_list_filter_corr)


# �Ʒÿ�

coarse_class_train <- woebin(df_train, 
                             y = "TARGET", 
                             x = x_list_filter_corr, 
                             positive = "1", 
                             #save_breaks_list = './train',
                             breaks_list = breaks_list,
                             method = "freq", 
                             count_distr_limit = 0.01, 
                             bin_num_limit = 5)


# �����

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





# woe�� ������ �����ͼ� �����

train <- df_train[,c(x_list_filter_corr, "TARGET")]

train_woe <- woebin_ply(train, coarse_class_train)

# ����� ������

test <- df_test[,c(x_list_filter_corr, "TARGET")]


# ������ƽ ȸ�ͺм�

logistic <- glm(TARGET ~ .,
                family = binomial(),
                data = train_woe)


summary(logistic)

vif(logistic)

df_vif <- vif(logistic)

write.csv(df_vif, './�Ѻ�_vif_������_1031.csv')

# ����� �ð�ȭ

ORplot(logistic, type = 2, show.CI = TRUE, main='Plot for Odds Ratio')


logistic_step <- step(logistic)

summary(logistic_step)

vif(logistic_step)

df_vif <- vif(logistic_step)

write.csv(df_vif, './�Ѻ�_vif_������_1031.csv')

# ����� �ð�ȭ

ORplot(logistic_step, type = 2, show.CI = TRUE, main='Plot for Odds Ratio')



# ������ƽ ����(21�׸� ��ü)���� ���ھ�ī�� ����

score_card <- scorecard(coarse_class_train, logistic_step)

score_card

# ���ھ�ī�� ����

score <- scorecard_ply(train, score_card, only_total_score = FALSE, var_kp = c("TARGET"))

score$TARGET <- as.factor(score$TARGET)


# (train)���ھ� ����

ggplot(score, aes(x=score, fill = TARGET, color = TARGET)) +
  geom_line(stat = "density") + 
  geom_density(alpha = .5) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("�Ʒÿ� ���ھ� ����") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="���ھ�", y="�ο���") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))



# ����������
perf_eva(pred = score$score, label = score$TARGET,
         show_plot = c('ks', 'lift', 'gain', 'roc', 'lz', 'pr', 'f1', 'density'), title = 'train')



# (test)���ھ�ī�� ����
test_score <- scorecard_ply(test, score_card, only_total_score = FALSE, var_kp = c("TARGET"))

test_score$TARGET <- as.factor(test_score$TARGET)


# (test)���ھ� ����

ggplot(test_score, aes(x=score, fill = TARGET, color = TARGET)) +
  geom_line(stat = "density") + 
  geom_density(alpha = .5) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("����� ���ھ� ����") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  labs(x="���ھ�", y="�ο���") +
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "black"))



# ����������
perf_eva(pred = test_score$score, label = test_score$TARGET,
         show_plot = c('ks', 'lift', 'gain', 'roc', 'lz', 'pr', 'f1', 'density'), title = 'test')



# psi ��
########## train-test
# expect : ���ؽ���(�����ͼ� : score), actual : �񱳵Ǵ� ���� ����

psi_test <- perf_psi(score = list(actual = test_score$score, expect = score$score), 
                     label = list(actual = test_score$TARGET, expect = score$TARGET),
                     show_plot = TRUE, title = 'train-test', positive = "1", threshold_variable = 20)


psi_test


# ����Ʈ ����(�Ʒÿ�)

report(list(train=df_train), "TARGET", x_list_filter_corr, breaks_list, seed = 222, save_report = '�������Ǽ�_1101')


report(list(train=df_train, test=df_test), "TARGET", x_list_filter_corr, breaks_list, seed = 222, save_report = '�������Ǽ�_���������_1101')




# 10���

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

