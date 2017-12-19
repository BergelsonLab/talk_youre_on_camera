#all the wilcoxon and kendall correlations for the sixseven paper
# Count Vars: six vs. seven, wilcoxon --------------------------------------------------------------
##month six vs. seven, aud. wilcoxon: no diffs
wa1 <- wilcox.test(sixseven_aud$six_numtokens, sixseven_aud$sev_numtokens, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_numtok")
wa2 <- wilcox.test(sixseven_aud$six_numtypes, sixseven_aud$sev_numtypes, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_numtyp")
wa3 <- wilcox.test(sixseven_aud$six_numspeakers, sixseven_aud$sev_numspeakers, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_numsp")
wa4 <- wilcox.test(sixseven_aud$six_MOT, sixseven_aud$sev_MOT, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_MOT")
wa5 <- wilcox.test(sixseven_aud$six_FAT, sixseven_aud$sev_FAT, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_FAT")
wa6 <- wilcox.test(sixseven_aud$six_d, sixseven_aud$sev_d, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_d")
wa7 <- wilcox.test(sixseven_aud$six_q, sixseven_aud$sev_q, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_q")
wa8 <- wilcox.test(sixseven_aud$six_r, sixseven_aud$sev_r, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_r")
wa9 <- wilcox.test(sixseven_aud$six_s, sixseven_aud$sev_s, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_s")
wa10 <- wilcox.test(sixseven_aud$six_i, sixseven_aud$sev_i, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_i")
wa11 <- wilcox.test(sixseven_aud$six_n, sixseven_aud$sev_n, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_n")
wa12 <- wilcox.test(sixseven_aud$six_y_op, sixseven_aud$sev_y_op, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wa_yop")

was <- bind_rows(wa1, wa2, wa3, wa4, wa5, wa6, wa7, wa8, wa9, wa10 ,wa11, wa12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
was %>% filter(pval_adj>.05)

#month six vs. seven, vid. wilcoxon: no diffs
wv1 <- wilcox.test(sixseven_vid$six_numtokens, sixseven_vid$sev_numtokens, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_numtok")
wv2 <- wilcox.test(sixseven_vid$six_numtypes, sixseven_vid$sev_numtypes, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_numtyp")
wv3 <- wilcox.test(sixseven_vid$six_numspeakers, sixseven_vid$sev_numspeakers, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_numsp")
wv4 <- wilcox.test(sixseven_vid$six_MOT, sixseven_vid$sev_MOT, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_MOT")
wv5 <- wilcox.test(sixseven_vid$six_FAT, sixseven_vid$sev_FAT, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_FAT")
wv6 <- wilcox.test(sixseven_vid$six_d, sixseven_vid$sev_d, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_d")
wv7 <- wilcox.test(sixseven_vid$six_q, sixseven_vid$sev_q, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_q")
wv8 <- wilcox.test(sixseven_vid$six_r, sixseven_vid$sev_r, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_r")
wv9 <- wilcox.test(sixseven_vid$six_s, sixseven_vid$sev_s, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_s")
wv10 <- wilcox.test(sixseven_vid$six_i, sixseven_vid$sev_i, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_i")
wv11 <- wilcox.test(sixseven_vid$six_n, sixseven_vid$sev_n, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_n")
wv12 <- wilcox.test(sixseven_vid$six_y_op, sixseven_vid$sev_y_op, conf.int=T, paired = T) %>% tidy() %>% mutate(comp = "wv_yop")

wvs <- bind_rows(wv1, wv2, wv3, wv4, wv5, wv6, wv7, wv8, wv9, wv10 ,wv11, wv12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
wvs %>% filter(pval_adj>.05)

# Count Vars: six vs. seven, kendall's corr --------------------------------------------------------------
#month six vs. seven, aud, corr: all correlate
ca1 <- cor.test(sixseven_aud$six_numtokens, sixseven_aud$sev_numtokens, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_numtok")
ca2 <- cor.test(sixseven_aud$six_numtypes, sixseven_aud$sev_numtypes, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_numtyp")
ca3 <- cor.test(sixseven_aud$six_numspeakers, sixseven_aud$sev_numspeakers, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_numsp")
ca4 <- cor.test(sixseven_aud$six_MOT, sixseven_aud$sev_MOT, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_MOT")
ca5 <- cor.test(sixseven_aud$six_FAT, sixseven_aud$sev_FAT, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_FAT")
ca6 <- cor.test(sixseven_aud$six_d, sixseven_aud$sev_d, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_d")
ca7 <- cor.test(sixseven_aud$six_q, sixseven_aud$sev_q, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_q")
ca8 <- cor.test(sixseven_aud$six_r, sixseven_aud$sev_r, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_r")
ca9 <- cor.test(sixseven_aud$six_s, sixseven_aud$sev_s, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_s")
ca10 <- cor.test(sixseven_aud$six_i, sixseven_aud$sev_i, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_i")
ca11 <- cor.test(sixseven_aud$six_n, sixseven_aud$sev_n, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_n")
ca12 <- cor.test(sixseven_aud$six_y_op, sixseven_aud$sev_y_op, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "ca_yop")

cas <- bind_rows(ca1, ca2, ca3, ca4, ca5, ca6, ca7, ca8, ca9, ca10 ,ca11, ca12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
cas %>% filter(pval_adj>.05)

#month six vs. seven, vid, corr: all but cv8: num_r
cv1 <- cor.test(sixseven_vid$six_numtokens, sixseven_vid$sev_numtokens, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_numtok")
cv2 <- cor.test(sixseven_vid$six_numtypes, sixseven_vid$sev_numtypes, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_numtyp")
cv3 <- cor.test(sixseven_vid$six_numspeakers, sixseven_vid$sev_numspeakers, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_numsp")
cv4 <- cor.test(sixseven_vid$six_MOT, sixseven_vid$sev_MOT, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_MOT")
cv5 <- cor.test(sixseven_vid$six_FAT, sixseven_vid$sev_FAT, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_FAT")
cv6 <- cor.test(sixseven_vid$six_d, sixseven_vid$sev_d, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_d")
cv7 <- cor.test(sixseven_vid$six_q, sixseven_vid$sev_q, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_q")
cv8 <- cor.test(sixseven_vid$six_r, sixseven_vid$sev_r, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_r")
cv9 <- cor.test(sixseven_vid$six_s, sixseven_vid$sev_s, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_s")
cv10 <- cor.test(sixseven_vid$six_i, sixseven_vid$sev_i, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_i")
cv11 <- cor.test(sixseven_vid$six_n, sixseven_vid$sev_n, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_n")
cv12 <- cor.test(sixseven_vid$six_y_op, sixseven_vid$sev_y_op, conf.int=T, method = "kendall") %>% tidy() %>% mutate(comp = "cv_yop")

cvs <- bind_rows(cv1, cv2, cv3, cv4, cv5, cv6, cv7, cv8, cv9, cv10 ,cv11, cv12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
cvs %>% filter(pval_adj>.05)

# Count Vars normed: a vs. v, wilcoxon ------------------------------------------------------
##aud vs. vid, month6; fat, r, s are not different, the rest differ significantly
#nb there's no subj 17 for 06 audio, so that line is missing in df deliberately

wsix1 <- wilcox.test(six_spreadAV_normmin$v_numtokens, six_spreadAV_normmin$a_numtokens,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c6_numtok")
wsix2 <- wilcox.test(six_spreadAV_normmin$v_numtypes, six_spreadAV_normmin$a_numtypes,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c6_numtyp")
wsix3 <- wilcox.test(six_spreadAV_normmin$v_numspeakers, six_spreadAV_normmin$a_numspeakers,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c6_numsp")
wsix4 <- wilcox.test(six_spreadAV_normmin$v_MOT, six_spreadAV_normmin$a_MOT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c6_MOT")
wsix5 <- wilcox.test(six_spreadAV_normmin$v_FAT, six_spreadAV_normmin$a_FAT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c6_FAT")
wsix6 <- wilcox.test(six_spreadAV_normmin$v_d, six_spreadAV_normmin$a_d,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c6_d")
wsix7 <- wilcox.test(six_spreadAV_normmin$v_q, six_spreadAV_normmin$a_q,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c6_q")
wsix8 <- wilcox.test(six_spreadAV_normmin$v_r, six_spreadAV_normmin$a_r,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c6_r")
wsix9 <- wilcox.test(six_spreadAV_normmin$v_s, six_spreadAV_normmin$a_s,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c6_s")
wsix10 <- wilcox.test(six_spreadAV_normmin$v_i, six_spreadAV_normmin$a_i,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c6_i")
wsix11 <- wilcox.test(six_spreadAV_normmin$v_n, six_spreadAV_normmin$a_n,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c6_n")
wsix12 <- wilcox.test(six_spreadAV_normmin$v_y_op, six_spreadAV_normmin$a_y_op,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c6_yop")

wsixs <- bind_rows(wsix1, wsix2, wsix3, wsix4, wsix5, wsix6, wsix7, wsix8, wsix9, wsix10, wsix11, wsix12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
wsixs %>% filter(pval_adj<.05)


#aud vs. vid, month7; fat, r, s are not different, the rest differ signifiacntly (same as month 6)
wseven1 <- wilcox.test(sev_spreadAV_normmin$v_numtokens, sev_spreadAV_normmin$a_numtokens,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c7_numtok")
wseven2 <- wilcox.test(sev_spreadAV_normmin$v_numtypes, sev_spreadAV_normmin$a_numtypes,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c7_numtyp")
wseven3 <- wilcox.test(sev_spreadAV_normmin$v_numspeakers, sev_spreadAV_normmin$a_numspeakers,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c7_numsp")
wseven4 <- wilcox.test(sev_spreadAV_normmin$v_MOT, sev_spreadAV_normmin$a_MOT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c7_MOT")
wseven5 <- wilcox.test(sev_spreadAV_normmin$v_FAT, sev_spreadAV_normmin$a_FAT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "c7_FAT")
wseven6 <- wilcox.test(sev_spreadAV_normmin$v_d, sev_spreadAV_normmin$a_d,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c7_d")
wseven7 <- wilcox.test(sev_spreadAV_normmin$v_q, sev_spreadAV_normmin$a_q,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c7_q")
wseven8 <- wilcox.test(sev_spreadAV_normmin$v_r, sev_spreadAV_normmin$a_r,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c7_r")
wseven9 <- wilcox.test(sev_spreadAV_normmin$v_s, sev_spreadAV_normmin$a_s,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c7_s")
wseven10 <- wilcox.test(sev_spreadAV_normmin$v_i, sev_spreadAV_normmin$a_i,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c7_i")
wseven11 <- wilcox.test(sev_spreadAV_normmin$v_n, sev_spreadAV_normmin$a_n,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c7_n")
wseven12 <- wilcox.test(sev_spreadAV_normmin$v_y_op, sev_spreadAV_normmin$a_y_op,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "c7_yop")

wsevens <- bind_rows(wseven1, wseven2, wseven3, wseven4, wseven5, wseven6, wseven7, wseven8, wseven9, wseven10, wseven11, wseven12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
wsevens %>% filter(pval_adj<.05)


# Count Vars normed: a vs. v, kendall's corr ------------------------------------------------------
##aud vs. vid, month6; only 3/10 correlate: d, q, i
#nb there's no subj 17 for 06 audio, so that line is missing in df deliberately

csix1 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_numtokens+a_numtokens,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c6_numtok")
csix2 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_numtypes+a_numtypes,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c6_numtyp")
csix3 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_numspeakers+a_numspeakers,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c6_numsp")
csix4 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_MOT+a_MOT,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c6_MOT")
csix5 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_FAT+a_FAT,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c6_FAT")
csix6 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_d+a_d,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c6_d")
csix7 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_q+a_q,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c6_q")
csix8 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_r+a_r,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c6_r")
csix9 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_s+a_s,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c6_s")
csix10 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_i+a_i,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c6_i")
csix11 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_n+a_n,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c6_n")
csix12 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="06"), ~v_y_op+a_y_op,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c6_yop")

csixs <- bind_rows(csix1, csix2, csix3, csix4, csix5, csix6, csix7, csix8, csix9, csix10, csix11, csix12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
csixs %>% filter(pval_adj<.05)
csix_taus_sig <- filter(csixs, pval_adj<.05)

#aud vs. vid, month7; everything correlates except singing
cseven1 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_numtokens+a_numtokens,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c7_numtok")
cseven2 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_numtypes+a_numtypes,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c7_numtyp")
cseven3 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_numspeakers+a_numspeakers,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c7_numsp")
cseven4 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_MOT+a_MOT,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c7_MOT")
cseven5 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_FAT+a_FAT,conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "c7_FAT")
cseven6 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_d+a_d,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c7_d")
cseven7 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_q+a_q,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c7_q")
cseven8 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_r+a_r,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c7_r")
cseven9 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_s+a_s,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c7_s")
cseven10 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_i+a_i,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c7_i")
cseven11 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_n+a_n,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c7_n")
cseven12 <- cor.test(data=subset(sixseven_spreadAV_normmin, month=="07"), ~v_y_op+a_y_op,conf.int=T, method = "kendall")%>%  tidy() %>% mutate(comp = "c7_yop")

csevens <- bind_rows(cseven1, cseven2, cseven3, cseven4, cseven5, cseven6, cseven7, cseven8, cseven9, cseven10, cseven11, cseven12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
csevens %>% filter(pval_adj<.05)
cseven_taus_sig <- filter(csevens, pval_adj<.05)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prop Vars: six vs. seven, wilcoxon ---------------------------------------------------------------
##month six vs. seven, aud. wilcoxon: no diffs
pwa1 <- wilcox.test(sixseven_aud$six_prop_op,sixseven_aud$sev_prop_op, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_op")
pwa2 <- wilcox.test(sixseven_aud$six_prop_mom,sixseven_aud$sev_prop_mom, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_MOT")
pwa3 <- wilcox.test(sixseven_aud$six_prop_dad,sixseven_aud$sev_prop_dad, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_FAT")
pwa4 <- wilcox.test(sixseven_aud$six_propd,sixseven_aud$sev_propd, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_d")
pwa5 <- wilcox.test(sixseven_aud$six_propi,sixseven_aud$sev_propi, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_i")
pwa6 <- wilcox.test(sixseven_aud$six_propr,sixseven_aud$sev_propr, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_r")
pwa7 <- wilcox.test(sixseven_aud$six_propq,sixseven_aud$sev_propq, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_q")
pwa8 <- wilcox.test(sixseven_aud$six_props,sixseven_aud$sev_props, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_s")
pwa9 <- wilcox.test(sixseven_aud$six_propn,sixseven_aud$sev_propn, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_n")
pwa10 <- wilcox.test(sixseven_aud$six_type_token_ratio,sixseven_aud$sev_type_token_ratio, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwa_ttr")

pwas <- bind_rows(pwa1, pwa2, pwa3, pwa4, pwa5, pwa6, pwa7, pwa8, pwa9, pwa10) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
pwas %>% filter(pval_adj>.05)

##month six vs. seven, vid. wilcoxon: no diffs
pwv1 <- wilcox.test(sixseven_vid$six_prop_op,sixseven_vid$sev_prop_op, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_op")
pwv2 <- wilcox.test(sixseven_vid$six_prop_mom,sixseven_vid$sev_prop_mom, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_MOT")
pwv3 <- wilcox.test(sixseven_vid$six_prop_dad,sixseven_vid$sev_prop_dad, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_FAT")
pwv4 <- wilcox.test(sixseven_vid$six_propd,sixseven_vid$sev_propd, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_d")
pwv5 <- wilcox.test(sixseven_vid$six_propi,sixseven_vid$sev_propi, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_i")
pwv6 <- wilcox.test(sixseven_vid$six_propr,sixseven_vid$sev_propr, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_r")
pwv7 <- wilcox.test(sixseven_vid$six_propq,sixseven_vid$sev_propq, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_q")
pwv8 <- wilcox.test(sixseven_vid$six_props,sixseven_vid$sev_props, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_s")
pwv9 <- wilcox.test(sixseven_vid$six_propn,sixseven_vid$sev_propn, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_n")
pwv10 <- wilcox.test(sixseven_vid$six_type_token_ratio,sixseven_vid$sev_type_token_ratio, conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "pwv_ttr")

pwvs <- bind_rows(pwv1, pwv2, pwv3, pwv4, pwv5, pwv6, pwv7, pwv8, pwv9, pwv10) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
pwvs %>% filter(pval_adj>.05)

# Prop Vars: six vs. seven, kendall's corr ---------------------------------------------------------------
##month six vs. seven, aud. corr: 7/10 corr (all except pca_op pca_q and pca_n)
pca1 <- cor.test(sixseven_aud$six_prop_op,sixseven_aud$sev_prop_op, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_op")
pca2 <- cor.test(sixseven_aud$six_prop_mom,sixseven_aud$sev_prop_mom, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_MOT")
pca3 <- cor.test(sixseven_aud$six_prop_dad,sixseven_aud$sev_prop_dad, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_FAT")
pca4 <- cor.test(sixseven_aud$six_propd,sixseven_aud$sev_propd, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_d")
pca5 <- cor.test(sixseven_aud$six_propi,sixseven_aud$sev_propi, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_i")
pca6 <- cor.test(sixseven_aud$six_propr,sixseven_aud$sev_propr, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_r")
pca7 <- cor.test(sixseven_aud$six_propq,sixseven_aud$sev_propq, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_q")
pca8 <- cor.test(sixseven_aud$six_props,sixseven_aud$sev_props, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_s")
pca9 <- cor.test(sixseven_aud$six_propn,sixseven_aud$sev_propn, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_n")
pca10 <- cor.test(sixseven_aud$six_type_token_ratio,sixseven_aud$sev_type_token_ratio, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pca_ttr")

pcas <- bind_rows(pca1, pca2, pca3, pca4, pca5, pca6, pca7, pca8, pca9, pca10) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
pcas %>% filter(pval_adj>.05)

pca_taus_sig <- filter(pcas, pval_adj<.05)

##month six vs. seven, vid corr: 2/10 corr (pcv_MOT, pcv_FAT
pcv1 <- cor.test(sixseven_vid$six_prop_op,sixseven_vid$sev_prop_op, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_op")
pcv2 <- cor.test(sixseven_vid$six_prop_mom,sixseven_vid$sev_prop_mom, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_MOT")
pcv3 <- cor.test(sixseven_vid$six_prop_dad,sixseven_vid$sev_prop_dad, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_FAT")
pcv4 <- cor.test(sixseven_vid$six_propd,sixseven_vid$sev_propd, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_d")
pcv5 <- cor.test(sixseven_vid$six_propi,sixseven_vid$sev_propi, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_i")
pcv6 <- cor.test(sixseven_vid$six_propr,sixseven_vid$sev_propr, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_r")
pcv7 <- cor.test(sixseven_vid$six_propq,sixseven_vid$sev_propq, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_q")
pcv8 <- cor.test(sixseven_vid$six_props,sixseven_vid$sev_props, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_s")
pcv9 <- cor.test(sixseven_vid$six_propn,sixseven_vid$sev_propn, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_n")
pcv10 <- cor.test(sixseven_vid$six_type_token_ratio,sixseven_vid$sev_type_token_ratio, conf.int=T, method = "kendall")%>% tidy() %>% mutate(comp = "pcv_ttr")

pcvs <- bind_rows(pcv1, pcv2, pcv3, pcv4, pcv5, pcv6, pcv7, pcv8, pcv9, pcv10) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
pcvs %>% filter(pval_adj<.05)

pcv_taus_sig <- filter(pcvs, pval_adj<.05)


# Prop Vars: a vs. v, wilcoxon ------------------------------------------------------
##aud vs. vid, month6; wilcoxon 6/10 do not differ (op, d, q and ttr do)
propwsix1 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_prop_op,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_prop_op, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_op")
propwsix2 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_prop_mom,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_prop_mom, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_MOT")
propwsix3 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_prop_dad,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_prop_dad, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_FAT")
propwsix4 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_propd,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_propd, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_d")
propwsix5 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_propi,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_propi, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_i")
propwsix6 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_propr,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_propr, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_r")
propwsix7 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_propq,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_propq, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_q")
propwsix8 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_props,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_props, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_s")
propwsix9 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_propn,
                         subset(sixseven_spreadAV, month=="06"& subj!="17")$a_propn, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_n")
propwsix10 <- wilcox.test(subset(sixseven_spreadAV, month=="06"& subj!="17")$v_type_token_ratio,
                          subset(sixseven_spreadAV, month=="06"& subj!="17")$a_type_token_ratio, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw6_ttr")

propwsixs <- bind_rows(propwsix1, propwsix2, propwsix3, propwsix4, propwsix5, propwsix6, propwsix7, propwsix8, propwsix9, propwsix10) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
propwsixs %>% filter(pval_adj<.05)

##aud vs. vid, month7; wilcoxon 7/10 do not differ (d, FAT and ttr do)
propwseven1 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_prop_op,
                         subset(sixseven_spreadAV, month=="07")$a_prop_op, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_op")
propwseven2 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_prop_mom,
                         subset(sixseven_spreadAV, month=="07")$a_prop_mom, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_MOT")
propwseven3 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_prop_dad,
                         subset(sixseven_spreadAV, month=="07")$a_prop_dad, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_FAT")
propwseven4 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_propd,
                         subset(sixseven_spreadAV, month=="07")$a_propd, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_d")
propwseven5 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_propi,
                         subset(sixseven_spreadAV, month=="07")$a_propi, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_i")
propwseven6 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_propr,
                         subset(sixseven_spreadAV, month=="07")$a_propr, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_r")
propwseven7 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_propq,
                         subset(sixseven_spreadAV, month=="07")$a_propq, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_q")
propwseven8 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_props,
                         subset(sixseven_spreadAV, month=="07")$a_props, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_s")
propwseven9 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_propn,
                         subset(sixseven_spreadAV, month=="07")$a_propn, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_n")
propwseven10 <- wilcox.test(subset(sixseven_spreadAV, month=="07")$v_type_token_ratio,
                          subset(sixseven_spreadAV, month=="07")$a_type_token_ratio, conf.int=T, paired = T)%>% 
  tidy()%>% mutate(comp = "propw7_ttr")

propwsevens <- bind_rows(propwseven1, propwseven2, propwseven3, propwseven4, propwseven5, propwseven6, propwseven7, propwseven8, propwseven9, propwseven10) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
propwsevens %>% filter(pval_adj<.05)

# Prop Vars: a vs. v, kendall's corr ------------------------------------------------------
##aud vs. vid, month6; corr 9/10 do not corr (prop_i does)
propcsix1 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_prop_op+a_prop_op,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_op")
propcsix2 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_prop_mom+a_prop_mom,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_MOT")
propcsix3 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_prop_dad+a_prop_dad,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_FAT")
propcsix4 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_propd+a_propd,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_d")
propcsix5 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_propi+a_propi,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_i")
propcsix6 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_propr+a_propr,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_r")
propcsix7 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_propq+a_propq,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_q")
propcsix8 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_props+a_props,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_s")
propcsix9 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_propn+a_propn,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_n")
propcsix10 <- cor.test(data=subset(sixseven_spreadAV, month=="06"& subj!="17"), ~v_type_token_ratio+a_type_token_ratio,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc6_ttr")

propcsixs <- bind_rows(propcsix1, propcsix2, propcsix3, propcsix4, propcsix5, propcsix6, propcsix7, propcsix8, propcsix9, propcsix10) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
propcsixs %>% filter(pval_adj<.05)
propcsix_taus_sig <- filter(propcsixs, pval_adj<.05)


##aud vs. vid, month7; corr 6/10 do not corr (MOT, FAT, d, and r do)
propcseven1 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_prop_op+a_prop_op,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_op")
propcseven2 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_prop_mom+a_prop_mom,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_MOT")
propcseven3 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_prop_dad+a_prop_dad,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_FAT")
propcseven4 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_propd+a_propd,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_d")
propcseven5 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_propi+a_propi,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_i")
propcseven6 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_propr+a_propr,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_r")
propcseven7 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_propq+a_propq,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_q")
propcseven8 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_props+a_props,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_s")
propcseven9 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_propn+a_propn,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_n")
propcseven10 <- cor.test(data=subset(sixseven_spreadAV, month=="07"), ~v_type_token_ratio+a_type_token_ratio,conf.int=T, method = "kendall")%>%
  tidy() %>% mutate(comp = "propc7_ttr")

propcsevens <- bind_rows(propcseven1, propcseven2, propcseven3, propcseven4, propcseven5, propcseven6, propcseven7, propcseven8, propcseven9, propcseven10) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
propcsevens %>% filter(pval_adj<.05)
propcseven_taus_sig <- filter(propcsevens, pval_adj<.05)


# corr tests for top words ------------------------------------------------

cor_sixavtop <-
  cor.test(subset(top100av67_avspread, month == "06")$audio,
           subset(top100av67_avspread, month == "06")$video,
           method = "kendall") %>% tidy()#.23
cor_sevavtop <-
  cor.test(subset(top100av67_avspread, month == "07")$audio,
           subset(top100av67_avspread, month == "07")$video,
           method = "kendall") %>% tidy()#.22
cor_sixsevenAtop <-
  cor.test(top100audio_monthspread$six,
           top100audio_monthspread$seven,
           method = "kendall") %>% tidy()#.67
cor_sixsevenVtop <-
  cor.test(top100video_monthspread$six,
           top100video_monthspread$seven,
           method = "kendall") %>% tidy()#.36
cor_AVtop <-cor.test(top100av_spread$audio,
                     top100av_spread$video,
                     method = "kendall") %>% tidy()
cor_AVtopnozeroes <- cor.test(top100av_spread_nozeros$audio,
                              top100av_spread_nozeros$video,
                              method = "kendall") %>% tidy()# sig cor, but ~1/2 are 0s
corAVtop67_combo <- bind_rows(cor_sixavtop, cor_sevavtop,cor_sixsevenAtop, cor_sixsevenVtop) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))

corAVtop67_combo %>% filter(pval_adj<.05)


# shapiro tests -----------------------------------------------------------
countvals_long %>% 
  group_by(count_meas, month, audio_video) %>% 
  summarise(shap.pval = shapiro.test(countval)$p.value) %>% 
  filter(shap.pval>.05)
countvals_long_norm %>% 
  group_by(norm_meas, month, audio_video) %>% 
  summarise(shap.pval = shapiro.test(normval)$p.value) %>% 
  filter(shap.pval>.05)
propvals_long %>% 
  group_by(prop_meas, month, audio_video) %>% 
  summarise(shap.pval = shapiro.test(propval)$p.value) %>% 
  filter(shap.pval>.05)
# # Lmer_prop -------------------------------------------------------------
# #6/10 have av as a sig predictor; none have age. i, n, r, and s have neither (though s has an ix)
# #normal resid: prop_op, propd, TTR
# #normal resid
# mod_prop_op <- lmer(data = sixseven_basiclevel_home_data_agg, prop_op~month+audio_video + (1|subj))
# mod_prop_opix <- lmer(data = sixseven_basiclevel_home_data_agg, prop_op~month*audio_video + (1|subj))
# summary(mod_prop_op)
# anova(mod_prop_op, mod_prop_opix)
# plot(mod_prop_op)
# shapiro.test(resid(mod_prop_op))
# 
# #weird resid
# mod_prop_mom <- lmer(data = sixseven_basiclevel_home_data_agg, prop_mom~month+audio_video + (1|subj))
# mod_prop_momix <- lmer(data = sixseven_basiclevel_home_data_agg, prop_mom~month*audio_video + (1|subj))
# summary(mod_prop_mom)
# anova(mod_prop_mom, mod_prop_momix)
# plot(mod_prop_mom)
# shapiro.test(resid(mod_prop_mom))
# 
# #weird resid
# mod_prop_dad <- lmer(data = sixseven_basiclevel_home_data_agg, prop_dad~month+audio_video + (1|subj))
# mod_prop_dadix <- lmer(data = sixseven_basiclevel_home_data_agg, prop_dad~month*audio_video + (1|subj))
# summary(mod_prop_dad)
# anova(mod_prop_dad, mod_prop_dadix)
# plot(mod_prop_dad)
# shapiro.test(resid(mod_prop_dad))
# 
# #normal resid
# mod_propd <- lmer(data = sixseven_basiclevel_home_data_agg, propd~month+audio_video + (1|subj))
# mod_propdix <- lmer(data = sixseven_basiclevel_home_data_agg, propd~month*audio_video + (1|subj))
# summary(mod_propd)
# anova(mod_propd, mod_propdix)
# plot(mod_propd)
# shapiro.test(resid(mod_propd))
# 
# #neither; weird resid
# mod_propi <- lmer(data = sixseven_basiclevel_home_data_agg, propi~month+audio_video + (1|subj))
# mod_propiix <- lmer(data = sixseven_basiclevel_home_data_agg, propi~month*audio_video + (1|subj))
# summary(mod_propi)
# anova(mod_propi, mod_propiix)
# plot(mod_propi)
# shapiro.test(resid(mod_propi))
# 
# #neither; weird resid
# mod_propn <- lmer(data = sixseven_basiclevel_home_data_agg, propn~month+audio_video + (1|subj))
# mod_propnix <- lmer(data = sixseven_basiclevel_home_data_agg, propn~month*audio_video + (1|subj))
# summary(mod_propn)
# anova(mod_propn, mod_propnix)
# plot(mod_propn)
# shapiro.test(resid(mod_propn))
# 
# #weird resid
# mod_propq <- lmer(data = sixseven_basiclevel_home_data_agg, propq~month+audio_video + (1|subj))
# mod_propqix <- lmer(data = sixseven_basiclevel_home_data_agg, propq~month*audio_video + (1|subj))
# summary(mod_propq)
# anova(mod_propq, mod_propqix)
# plot(mod_propq)
# shapiro.test(resid(mod_propq))
# 
# #neither; weird resid
# mod_propr <- lmer(data = sixseven_basiclevel_home_data_agg, propr~month+audio_video + (1|subj))
# mod_proprix <- lmer(data = sixseven_basiclevel_home_data_agg, propr~month*audio_video + (1|subj))
# summary(mod_propr)
# anova(mod_propr, mod_proprix)
# plot(mod_propr)
# shapiro.test(resid(mod_propr))
# 
# #no ME, sig interaction, weird resid both ix and reg
# mod_props <- lmer(data = sixseven_basiclevel_home_data_agg, props~month+audio_video + (1|subj))
# mod_propsix <- lmer(data = sixseven_basiclevel_home_data_agg, props~month*audio_video + (1|subj))
# summary(mod_props)
# anova(mod_props, mod_propsix)
# plot(mod_propsix)
# shapiro.test(resid(mod_propsix))
# shapiro.test(resid(mod_props))
# 
# #normal resid
# mod_type_token_ratio <- lmer(data = sixseven_basiclevel_home_data_agg, type_token_ratio~month+audio_video + (1|subj))
# mod_type_token_ratioix <- lmer(data = sixseven_basiclevel_home_data_agg, type_token_ratio~month*audio_video + (1|subj))
# summary(mod_type_token_ratio)
# anova(mod_type_token_ratio, mod_type_token_ratioix)
# plot(mod_type_token_ratio)
# shapiro.test(resid(mod_type_token_ratio))
# 
# # lmer count_norm ---------------------------------------------------------
# #11/12 have av as a sig predictor; none have age as a sig predictor; FAT has neither
# mod_y_op <- lmer(data = subset(countvals_long_norm, norm_meas=="y_op"), normval~month+audio_video + (1|subj))
# mod_y_opix <- lmer(data = subset(countvals_long_norm, norm_meas=="y_op"), normval~month*audio_video + (1|subj))
# summary(mod_y_op)
# anova(mod_y_op, mod_y_opix)
# plot(mod_y_op)
# shapiro.test(resid(mod_y_op))
# 
# mod_MOT <- lmer(data =subset(countvals_long_norm, norm_meas=="MOT"), normval~month+audio_video + (1|subj))
# mod_MOTix <- lmer(data = subset(countvals_long_norm, norm_meas=="MOT"), normval~month*audio_video + (1|subj))
# summary(mod_MOT)
# anova(mod_MOT, mod_MOTix)
# plot(mod_MOT)
# 
# #neither
# mod_FAT <- lmer(data = subset(countvals_long_norm, norm_meas=="FAT"), normval~month+audio_video + (1|subj))
# mod_FATix <- lmer(data = subset(countvals_long_norm, norm_meas=="FAT"), normval~month*audio_video + (1|subj))
# summary(mod_FAT)
# anova(mod_FAT, mod_FATix)
# plot(mod_FAT)
# 
# mod_d <- lmer(data = subset(countvals_long_norm, norm_meas=="d"), normval~month+audio_video + (1|subj))
# mod_dix <- lmer(data = subset(countvals_long_norm, norm_meas=="d"), normval~month*audio_video + (1|subj))
# summary(mod_d)
# anova(mod_d, mod_dix)
# plot(mod_d)
# 
# 
# mod_i <- lmer(data = subset(countvals_long_norm, norm_meas=="i"), normval~month+audio_video + (1|subj))
# mod_iix <- lmer(data = subset(countvals_long_norm, norm_meas=="i"), normval~month*audio_video + (1|subj))
# summary(mod_i)
# anova(mod_i, mod_iix)
# plot(mod_i)
# 
# 
# mod_n <- lmer(data = subset(countvals_long_norm, norm_meas=="n"), normval~month+audio_video + (1|subj))
# mod_nix <- lmer(data = subset(countvals_long_norm, norm_meas=="n"), normval~month*audio_video + (1|subj))
# summary(mod_n)
# anova(mod_n, mod_nix)
# plot(mod_n)
# 
# 
# mod_q <- lmer(data = subset(countvals_long_norm, norm_meas=="q"), normval~month+audio_video + (1|subj))
# mod_qix <- lmer(data = subset(countvals_long_norm, norm_meas=="q"), normval~month*audio_video + (1|subj))
# summary(mod_q)
# anova(mod_q, mod_qix)
# plot(mod_q)
# 
# #neither
# mod_r <- lmer(data = subset(countvals_long_norm, norm_meas=="r"), normval~month+audio_video + (1|subj))
# mod_rix <- lmer(data = subset(countvals_long_norm, norm_meas=="r"), normval~month*audio_video + (1|subj))
# summary(mod_r)
# anova(mod_r, mod_rix)
# plot(mod_r)
# 
# 
# mod_s <- lmer(data = subset(countvals_long_norm, norm_meas=="s"), normval~month+audio_video + (1|subj))
# mod_s_ix <- lmer(data = subset(countvals_long_norm, norm_meas=="s"), normval~month*audio_video + (1|subj))
# summary(mod_s)
# anova(mod_s, mod_s_ix)
# plot(mod_s_ix)
# 
# 
# mod_types <- lmer(data = subset(countvals_long_norm, norm_meas=="numtypes"), normval~month+audio_video + (1|subj))
# mod_types_ix <- lmer(data = subset(countvals_long_norm, norm_meas=="numtypes"), normval~month*audio_video + (1|subj))
# summary(mod_types)
# anova(mod_types, mod_types_ix)
# plot(mod_types)
# 
# mod_tokens <- lmer(data = subset(countvals_long_norm, norm_meas=="numtokens"), normval~month+audio_video + (1|subj))
# mod_tokens_ix <- lmer(data = subset(countvals_long_norm, norm_meas=="numtokens"), normval~month*audio_video + (1|subj))
# summary(mod_tokens)
# anova(mod_tokens, mod_tokens_ix)
# plot(mod_tokens)
# 
# mod_speakers <- lmer(data = subset(countvals_long_norm, norm_meas=="numspeakers"), normval~month+audio_video + (1|subj))
# mod_speakers_ix <- lmer(data = subset(countvals_long_norm, norm_meas=="numspeakers"), normval~month*audio_video + (1|subj))
# summary(mod_speakers)
# anova(mod_speakers, mod_speakers_ix)
# plot(mod_speakers)
# 
# 
# # lmer_log space count vars -----------------------------------------------
# #tokens, q, d, MOT have normal resids
# #not normal resids
# mod_y_op <- lmer(data = subset(countvals_long_norm, norm_meas=="y_op"), log(normval+1)~month+audio_video + (1|subj))
# mod_y_opix <- lmer(data = subset(countvals_long_norm, norm_meas=="y_op"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_y_op)
# anova(mod_y_op, mod_y_opix)
# plot(mod_y_op)
# shapiro.test(resid(mod_y_op))
# 
# #normal resid
# mod_MOT <- lmer(data =subset(countvals_long_norm, norm_meas=="MOT"), log(normval+1)~month+audio_video + (1|subj))
# mod_MOTix <- lmer(data = subset(countvals_long_norm, norm_meas=="MOT"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_MOT)
# anova(mod_MOT, mod_MOTix)
# plot(mod_MOT)
# shapiro.test(resid(mod_MOT))
# 
# #neither, weird resid
# mod_FAT <- lmer(data = subset(countvals_long_norm, norm_meas=="FAT"), log(normval+1)~month+audio_video + (1|subj))
# mod_FATix <- lmer(data = subset(countvals_long_norm, norm_meas=="FAT"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_FAT)
# anova(mod_FAT, mod_FATix)
# plot(mod_FAT)
# shapiro.test(resid(mod_FAT))
# 
# #normal resid
# mod_d <- lmer(data = subset(countvals_long_norm, norm_meas=="d"), log(normval+1)~month+audio_video + (1|subj))
# mod_dix <- lmer(data = subset(countvals_long_norm, norm_meas=="d"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_d)
# anova(mod_d, mod_dix)
# plot(mod_d)
# shapiro.test(resid(mod_d))
# 
# #weird resids
# mod_i <- lmer(data = subset(countvals_long_norm, norm_meas=="i"), log(normval+1)~month+audio_video + (1|subj))
# mod_iix <- lmer(data = subset(countvals_long_norm, norm_meas=="i"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_i)
# anova(mod_i, mod_iix)
# plot(mod_i)
# shapiro.test(resid(mod_i))
# 
# #weird resids
# mod_n <- lmer(data = subset(countvals_long_norm, norm_meas=="n"), log(normval+1)~month+audio_video + (1|subj))
# mod_nix <- lmer(data = subset(countvals_long_norm, norm_meas=="n"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_n)
# anova(mod_n, mod_nix)
# plot(mod_n)
# shapiro.test(resid(mod_n))
# 
# #normal resid
# mod_q <- lmer(data = subset(countvals_long_norm, norm_meas=="q"), log(normval+1)~month+audio_video + (1|subj))
# mod_qix <- lmer(data = subset(countvals_long_norm, norm_meas=="q"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_q)
# anova(mod_q, mod_qix)
# plot(mod_q)
# shapiro.test(resid(mod_q))
# 
# #av but no ix (not like un-log space); weird resid
# mod_r <- lmer(data = subset(countvals_long_norm, norm_meas=="r"), log(normval+1)~month+audio_video + (1|subj))
# mod_rix <- lmer(data = subset(countvals_long_norm, norm_meas=="r"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_r)
# anova(mod_r, mod_rix)
# plot(mod_r)
# shapiro.test(resid(mod_r))
# 
# #weird resids
# mod_s <- lmer(data = subset(countvals_long_norm, norm_meas=="s"), log(normval+1)~month+audio_video + (1|subj))
# mod_s_ix <- lmer(data = subset(countvals_long_norm, norm_meas=="s"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_s)
# anova(mod_s, mod_s_ix)
# plot(mod_s_ix)
# shapiro.test(resid(mod_s))
# 
# #weird resids
# mod_types <- lmer(data = subset(countvals_long_norm, norm_meas=="numtypes"), log(normval+1)~month+audio_video + (1|subj))
# mod_types_ix <- lmer(data = subset(countvals_long_norm, norm_meas=="numtypes"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_types)
# anova(mod_types, mod_types_ix)
# plot(mod_types)
# shapiro.test(resid(mod_types))
# 
# #normal resids
# mod_tokens <- lmer(data = subset(countvals_long_norm, norm_meas=="numtokens"), log(normval+1)~month+audio_video + (1|subj))
# mod_tokens_ix <- lmer(data = subset(countvals_long_norm, norm_meas=="numtokens"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_tokens)
# anova(mod_tokens, mod_tokens_ix)
# plot(mod_tokens)
# shapiro.test(resid(mod_tokens))
# 
# #weird resids
# mod_speakers <- lmer(data = subset(countvals_long_norm, norm_meas=="numspeakers"), log(normval+1)~month+audio_video + (1|subj))
# mod_speakers_ix <- lmer(data = subset(countvals_long_norm, norm_meas=="numspeakers"), log(normval+1)~month*audio_video + (1|subj))
# summary(mod_speakers)
# anova(mod_speakers, mod_speakers_ix)
# plot(mod_speakers)
# shapiro.test(resid(mod_speakers))
