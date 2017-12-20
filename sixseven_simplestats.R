#talk you're on camera
#collapsing month, and only doing counts
countvals_long_norm_collapsed <- countvals_long_norm %>% 
  dplyr::select(-month, -a_tot_nosilsk, -aud_tot_nosil, -vid_total_min) %>% 
  group_by(subj, norm_meas, audio_video, meas_type) %>% 
  summarise_all(mean, na.rm=T)

countvals_long_collapsed <- countvals_long %>% 
  dplyr::select(-month) %>% 
  group_by(subj, count_meas, audio_video, meas_type) %>% 
  summarise_all(mean, na.rm=T)

sixseven_spreadAV_normmin_collapsed <- sixseven_spreadAV_normmin %>% 
  dplyr::select(-month,-a_tot_nosilsk, -aud_tot_nosil, -vid_total_min) %>% 
  group_by(subj) %>% 
    summarise_all(mean, na.rm=T)

sixseven_spreadAV_collapsed <- sixseven_spreadAV %>% 
  dplyr::select(-SubjectNumber, -v_total_min, -month,-v_n_op, -a_n_op, -v_total_min, -a_total_min, -a_tot_nosil, -a_tot_nosilsk) %>% 
  group_by(subj) %>% 
  summarise_all(mean, na.rm=T)

# Count Vars normed: a vs. v, wilcoxon ------------------------------------------------------
##only FAT doesn't differ
#nb there's no subj 17 for 06 audio, so that line is missing in orig df deliberately

w1 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_numtokens, sixseven_spreadAV_normmin_collapsed$a_numtokens,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "w_numtok")
w2 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_numtypes, sixseven_spreadAV_normmin_collapsed$a_numtypes,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "w_numtyp")
w3 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_numspeakers, sixseven_spreadAV_normmin_collapsed$a_numspeakers,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "w_numsp")
w4 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_MOT, sixseven_spreadAV_normmin_collapsed$a_MOT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "w_MOT")
w5 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_FAT, sixseven_spreadAV_normmin_collapsed$a_FAT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "w_FAT")
w6 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_d, sixseven_spreadAV_normmin_collapsed$a_d,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "w_d")
w7 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_q, sixseven_spreadAV_normmin_collapsed$a_q,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "w_q")
w8 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_r, sixseven_spreadAV_normmin_collapsed$a_r,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "w_r")
w9 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_s, sixseven_spreadAV_normmin_collapsed$a_s,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "w_s")
w10 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_i, sixseven_spreadAV_normmin_collapsed$a_i,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "w_i")
w11 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_n, sixseven_spreadAV_normmin_collapsed$a_n,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "w_n")
w12 <- wilcox.test(sixseven_spreadAV_normmin_collapsed$v_y_op, sixseven_spreadAV_normmin_collapsed$a_y_op,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "w_yop")

ws <- bind_rows(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
ws %>% filter(pval_adj<.05)


# Count Vars normed: a vs. v, kendall's corr ------------------------------------------------------
## only singing doesn't correlate
#nb there's no subj 17 for 06 audio, so that line is missing in orig df deliberately

c1 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_numtokens+a_numtokens,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c_numtok")
c2 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_numtypes+a_numtypes,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c_numtyp")
c3 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_numspeakers+a_numspeakers,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c_numsp")
c4 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_MOT+a_MOT,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c_MOT")
c5 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_FAT+a_FAT,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c_FAT")
c6 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_d+a_d,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c_d")
c7 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_q+a_q,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c_q")
c8 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_r+a_r,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c_r")
c9 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_s+a_s,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c_s")
c10 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_i+a_i,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c_i")
c11 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_n+a_n,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c_n")
c12 <- cor.test(data=sixseven_spreadAV_normmin_collapsed, ~v_y_op+a_y_op,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c_yop")

cs <- bind_rows(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
cs %>% filter(pval_adj<.05)
c_taus_sig <- filter(cs, pval_adj<.05)



#foregoing the proportion versions for now
# Prop Vars: a vs. v, wilcoxon ------------------------------------------------------
# ##aud vs. vid, month6; wilcoxon 6/10 do not differ (op, d, q and ttr do)
# propw1 <- wilcox.test(sixseven_spreadAV_collapsed$v_prop_op,
#                          sixseven_spreadAV_collapsed$a_prop_op, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_op")
# propw2 <- wilcox.test(sixseven_spreadAV_collapsed$v_prop_mom,
#                          sixseven_spreadAV_collapsed$a_prop_mom, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_MOT")
# propw3 <- wilcox.test(sixseven_spreadAV_collapsed$v_prop_dad,
#                          sixseven_spreadAV_collapsed$a_prop_dad, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_FAT")
# propw4 <- wilcox.test(sixseven_spreadAV_collapsed$v_propd,
#                          sixseven_spreadAV_collapsed$a_propd, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_d")
# propw5 <- wilcox.test(sixseven_spreadAV_collapsed$v_propi,
#                          sixseven_spreadAV_collapsed$a_propi, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_i")
# propw6 <- wilcox.test(sixseven_spreadAV_collapsed$v_propr,
#                          sixseven_spreadAV_collapsed$a_propr, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_r")
# propw7 <- wilcox.test(sixseven_spreadAV_collapsed$v_propq,
#                          sixseven_spreadAV_collapsed$a_propq, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_q")
# propw8 <- wilcox.test(sixseven_spreadAV_collapsed$v_props,
#                          sixseven_spreadAV_collapsed$a_props, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_s")
# propw9 <- wilcox.test(sixseven_spreadAV_collapsed$v_propn,
#                          sixseven_spreadAV_collapsed$a_propn, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_n")
# propw10 <- wilcox.test(sixseven_spreadAV_collapsed$v_type_token_ratio,
#                           sixseven_spreadAV_collapsed$a_type_token_ratio, conf.int=T, paired = T)%>% 
#   tidy()%>% mutate(comp = "propw_ttr")
# 
# propws <- bind_rows(propw1, propw2, propw3, propw4, propw5, propw6, propw7, propw8, propw9, propw10) %>% 
#   mutate(pval_adj = p.adjust(p.value, method = "holm"))
# propws %>% filter(pval_adj<.05)
# 
# 
# # Prop Vars: a vs. v, kendall's corr ------------------------------------------------------
# ##aud vs. vid, month6; corr 9/10 do not corr (prop_i does)
# propc1 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_prop_op+a_prop_op,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_op")
# propc2 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_prop_mom+a_prop_mom,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_MOT")
# propc3 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_prop_dad+a_prop_dad,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_FAT")
# propc4 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_propd+a_propd,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_d")
# propc5 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_propi+a_propi,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_i")
# propc6 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_propr+a_propr,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_r")
# propc7 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_propq+a_propq,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_q")
# propc8 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_props+a_props,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_s")
# propc9 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_propn+a_propn,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_n")
# propc10 <- cor.test(data=sixseven_spreadAV_collapsed, ~v_type_token_ratio+a_type_token_ratio,conf.int=T, method = "kendall")%>%
#   tidy() %>% mutate(comp = "propc_ttr")
# 
# propcs <- bind_rows(propc1, propc2, propc3, propc4, propc5, propc6, propc7, propc8, propc9, propc10) %>% 
#   mutate(pval_adj = p.adjust(p.value, method = "holm"))
# propcs %>% filter(pval_adj<.05)
# propc_taus_sig <- filter(propcs, pval_adj<.05)

## lmer efforts failed because residuals are weird for most vars, both on norm countval space and in log+1 norm countval space
# # lmer count_norm ---------------------------------------------------------
# #weird resid
# cmod_y_op <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="y_op"), normval~audio_video + (1|subj))
# summary(cmod_y_op)
# plot(cmod_y_op)
# shapiro.test(resid(cmod_y_op))
# 
# #weird resid
# cmod_MOT <- lmer(data =subset(countvals_long_norm_collapsed, norm_meas=="MOT"), normval~audio_video + (1|subj))
# summary(cmod_MOT)
# plot(cmod_MOT)
# shapiro.test(resid(cmod_MOT))
# 
# #ns FAT, weird resid
# cmod_FAT <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="FAT"), normval~audio_video + (1|subj))
# summary(cmod_FAT)
# plot(cmod_FAT)
# shapiro.test(resid(cmod_FAT))
# 
# #weird resid
# cmod_d <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="d"), normval~audio_video + (1|subj))
# summary(cmod_d)
# plot(cmod_d)
# shapiro.test(resid(cmod_d))
# 
# #weird resid
# cmod_i <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="i"), normval~audio_video + (1|subj))
# summary(cmod_i)
# plot(cmod_i)
# shapiro.test(resid(cmod_i))
# 
# #weird resid
# cmod_n <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="n"), normval~audio_video + (1|subj))
# summary(cmod_n)
# plot(cmod_n)
# shapiro.test(resid(cmod_n))
# 
# #weird resid
# cmod_q <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="q"), normval~audio_video + (1|subj))
# summary(cmod_q)
# plot(cmod_q)
# shapiro.test(resid(cmod_q))
# 
# #weird resid
# cmod_r <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="r"), normval~audio_video + (1|subj))
# summary(cmod_r)
# plot(cmod_r)
# shapiro.test(resid(cmod_r))
# 
# #weird resid
# cmod_s <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="s"), normval~audio_video + (1|subj))
# summary(cmod_s)
# plot(cmod_s_ix)
# shapiro.test(resid(cmod_s))
# 
# #weird resid
# cmod_types <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="numtypes"), normval~audio_video + (1|subj))
# summary(cmod_types)
# plot(cmod_types)
# shapiro.test(resid(cmod_types))
# 
# #weird resid
# cmod_tokens <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="numtokens"), normval~audio_video + (1|subj))
# summary(cmod_tokens)
# plot(cmod_tokens)
# shapiro.test(resid(cmod_tokens))
# 
# #weird resid
# cmod_speakers <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="numspeakers"), normval~audio_video + (1|subj))
# summary(cmod_speakers)
# plot(cmod_speakers)
# shapiro.test(resid(cmod_speakers))
# 
# # lmer_log space count vars -----------------------------------------------
# #types, tokens, d, and op have normal resids
# #normal resids
# cmod_y_op <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="y_op"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_y_op)
# plot(cmod_y_op)
# shapiro.test(resid(cmod_y_op))
# 
# #marginally normal resid
# cmod_MOT <- lmer(data =subset(countvals_long_norm_collapsed, norm_meas=="MOT"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_MOT)
# plot(cmod_MOT)
# shapiro.test(resid(cmod_MOT))
# 
# #AV is ns, weird resid
# cmod_FAT <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="FAT"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_FAT)
# plot(cmod_FAT)
# shapiro.test(resid(cmod_FAT))
# 
# #normal resid
# cmod_d <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="d"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_d)
# plot(cmod_d)
# shapiro.test(resid(cmod_d))
# 
# #weird resids
# cmod_i <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="i"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_i)
# plot(cmod_i)
# shapiro.test(resid(cmod_i))
# 
# #weird resids
# cmod_n <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="n"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_n)
# plot(cmod_n)
# shapiro.test(resid(cmod_n))
# 
# #weird resid
# cmod_q <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="q"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_q)
# plot(cmod_q)
# shapiro.test(resid(cmod_q))
# 
# #weird resid
# cmod_r <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="r"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_r)
# plot(cmod_r)
# shapiro.test(resid(cmod_r))
# 
# #weird resids
# cmod_s <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="s"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_s)
# plot(cmod_s_ix)
# shapiro.test(resid(cmod_s))
# 
# #normal resids
# cmod_types <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="numtypes"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_types)
# plot(cmod_types)
# shapiro.test(resid(cmod_types))
# 
# #normal resids
# cmod_tokens <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="numtokens"), log(normval+1)~audio_video + (1|subj))
# cmod_tokens_ix <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="numtokens"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_tokens)
# plot(cmod_tokens)
# shapiro.test(resid(cmod_tokens))
# 
# #weird resids
# cmod_speakers <- lmer(data = subset(countvals_long_norm_collapsed, norm_meas=="numspeakers"), log(normval+1)~audio_video + (1|subj))
# summary(cmod_speakers)
# plot(cmod_speakers)
# shapiro.test(resid(cmod_speakers))