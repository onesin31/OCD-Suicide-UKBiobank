# 初始 ----
library(data.table)
setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
# 读取文件
dt_987 <- fread("987.csv")
dt_501936 <- fread("501936.csv")
# 统一第一列的列名为"id"
setnames(dt_987, 1, "id")
setnames(dt_501936, 1, "id")
# 将 501937.csv 的行划分为匹配和不匹配
a500949 <- dt_501936[id %in% dt_987$id]   # id 在 987 中存在的行
a987 <- dt_501936[!id %in% dt_987$id] # id 不在 987 中的行

## 性别、年龄、TDI、BMI、睡眠、ocd、种族 ----
setnames(a500949, 
         c("p21022", "p22189", "p31", "p1160_i0", "p130908", "p21000_i0"),
         c("Age", "TDI", "Sex", "Sleep_Duration", "ocdtime", "Ethnicity"))

a500949 <- a500949[!is.na(TDI)]
a500949[, Sex := fcase(Sex == "Female", 0, Sex == "Male", 1, default = NA_real_)]
print(table(a500949$Sex, useNA = "ifany"))
invalid_vals <- c("Prefer not to answer", "", "Do not know")
a500949[Sleep_Duration %chin% invalid_vals, Sleep_Duration := NA_character_]
a500949 <- a500949[!is.na(Sleep_Duration)]
print(table(a500949$Sleep_Duration, useNA = "ifany"))

a500949[, state := as.integer(!is.na(ocdtime))]
print(table(a500949$state, useNA = "ifany"))
a500949[, Ethnicity := fcase(
  Ethnicity %chin% c("British", "Irish", "Any other white background"), "White",
  Ethnicity %chin% c("", "Prefer not to answer", "Do not know"), NA_character_,
  default = "Others"
)]
a500949 <- a500949[!is.na(Ethnicity)]
a500949[, Ethnicity := fcase(Ethnicity == "White", 1, Ethnicity == "Others", 0, default = NA_real_)]
print(table(a500949$Ethnicity, useNA = "ifany"))

dt_bmi <- fread("BMI.csv")
setnames(dt_bmi, c("id", "BMI"))
dt_bmi[, BMI := as.numeric(BMI)]
a500949[dt_bmi, on = "id", BMI := i.BMI]
a500949 <- a500949[!is.na(BMI)]

## 收入吸烟饮酒，入组时间，死亡时间 ----
setnames(a500949, c("p738_i0", "p20116_i0", "p20117_i0"), c("Income", "Smoking_Status", "Drinking_Status"))

# 收入
a500949[Income %chin% c("Prefer not to answer", "", "Do not know"), Income := NA_character_]
a500949[, Income := fcase(
  Income == "Less than 18,000", 1,
  Income %chin% c("18,000 to 30,999", "31,000 to 51,999"), 2,
  Income %chin% c("52,000 to 100,000", "Greater than 100,000"), 3,
  default = NA_real_
)]
a500949 <- a500949[!is.na(Income)]
print(table(a500949$Income, useNA = "ifany"))

# 吸烟
a500949[Smoking_Status %chin% c("Prefer not to answer", ""), Smoking_Status := NA_character_]
a500949[, Smoking_Status := fcase(Smoking_Status %chin% c("Never", "Previous"), 0,
                           Smoking_Status == "Current", 1,
                           default = NA_real_)]
a500949 <- a500949[!is.na(Smoking_Status)]
print(table(a500949$Smoking_Status, useNA = "ifany"))

# 饮酒
a500949[Drinking_Status %chin% c("Prefer not to answer", ""), Drinking_Status := NA_character_]
a500949[, Drinking_Status := fcase(Drinking_Status %chin% c("Never", "Previous"), 0,
                            Drinking_Status == "Current", 1,
                            default = NA_real_)]
a500949 <- a500949[!is.na(Drinking_Status)]
print(table(a500949$Drinking_Status, useNA = "ifany"))

setnames(a500949, c("p53_i0", "p40000_i0", "p40000_i1"), c("start", "died0", "died1"))
# 注意：原代码误将 died0/died1 的无效值赋值给 start，此处修正为对各自列操作
a500949[start %chin% invalid_vals, start := NA_character_]
a500949[died0 %chin% invalid_vals, died0 := NA_character_]
a500949[died1 %chin% invalid_vals, died1 := NA_character_]

# 取两列死亡时间中较晚的一个（忽略 NA）
a500949[, diedtime := pmax(died0, died1, na.rm = TRUE)]
#table(is.na(a500949$died0))
#table(is.na(a500949$died1))
#table(is.na(a500949$diedtime))
## 就业学历 ----
setnames(a500949, c("p6142_i0", "p6138_i0"), c("Employment", "Qualifications"))

# 就业映射函数（向量化版本）
map_Employment <- function(x) {
  if (is.na(x) || x == "" || x %chin% c("None of the above", "Prefer not to answer"))
    return(NA_character_)
  parts <- unlist(strsplit(x, "\\|"))
  if (any(parts %chin% c("In paid employment or self-employed", "Doing unpaid or voluntary work")))
    return("Employed")
  if (any(parts %chin% c("Unable to work because of sickness or disability", "Unemployed")))
    return("Unemployed")
  if (any(parts %chin% c("Retired", "Looking after home and/or family", "Full or part-time student")))
    return("Others")
  return(NA_character_)
}
a500949[, Employment := sapply(Employment, map_Employment)]
a500949 <- a500949[!is.na(Employment)]
a500949[, Employment := fcase(Employment == "Employed", 2,
                              Employment == "Others", 1,
                              Employment == "Unemployed", 0,
                              default = NA_real_)]
print(table(a500949$Employment, useNA = "ifany"))

# 学历映射
map_qualifications <- function(x) {
  if (is.na(x) || x == "" || x == "Prefer not to answer") return(NA_character_)
  if (x == "None of the above") return("None")
  parts <- trimws(unlist(strsplit(x, "\\|")))
  if ("College or University degree" %chin% parts) return("Degree")
  return("Others")
}
a500949[, Qualifications := sapply(Qualifications, map_qualifications)]
a500949 <- a500949[!is.na(Qualifications)]
a500949[, Qualifications := fcase(Qualifications == "Degree", 2,
                                  Qualifications == "Others", 1,
                                  Qualifications == "None", 0,
                                  default = NA_real_)]
print(table(a500949$Qualifications, useNA = "ifany"))


## 社交支持指数 ----
setnames(a500949, c("p1031_i0", "p2020_i0", "p2110_i0"), c("Support", "Loneliness", "Confide"))

# Support
a500949[Support %chin% invalid_vals, Support := NA_character_]
a500949[, Support := fcase(
  Support %chin% c("2-4 times a week", "Almost daily"), 0,
  Support %chin% c("About once a week", "About once a month"), 1,
  Support %chin% c("Never or almost never", "No friends/family outside household", "Once every few months"), 2,
  default = NA_real_
)]
a500949 <- a500949[!is.na(Support)]

# Loneliness
a500949[Loneliness %chin% invalid_vals, Loneliness := NA_character_]
a500949[, Loneliness := fcase(Loneliness == "Yes", 1, Loneliness == "No", 0, default = NA_real_)]
a500949 <- a500949[!is.na(Loneliness)]

# Confide
a500949[Confide %chin% invalid_vals, Confide := NA_character_]
a500949[, Confide := fcase(
  Confide %chin% c("2-4 times a week", "Almost daily"), 0,
  Confide %chin% c("About once a week", "About once a month"), 1,
  Confide %chin% c("Once every few months", "Never or almost never"), 2,
  default = NA_real_
)]
a500949 <- a500949[!is.na(Confide)]

# 直接相加 Support, Loneliness, Confide，不删除任何缺失值
a500949[, Support_index := ifelse(
  rowSums(is.na(.SD)) == 3,
  NA_real_,
  rowSums(.SD, na.rm = TRUE)
), .SDcols = c("Support", "Loneliness", "Confide")]

print(table(a500949$Support_index, useNA = "ifany"))

b396740 <- a500949
# 保存为 RDS 文件，保留所有属性
saveRDS(b396740, "b987.rds")
# 一轮:指数与删除 ----
library(data.table)
setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
# 读取文件
b396740 <- readRDS("b987.rds")

## 童年创伤指数 ----
# 定义需要处理的列名
cols <- c("p29076", "p29080", "p20489", "p20491")
# 第一步：将无效值替换为 NA_character_
b396740[, (cols) := lapply(.SD, function(x) {
  x[x %in% c("Prefer not to answer", "", "Do not know")] <- NA_character_
  x
}), .SDcols = cols]
# 第二步：映射为数值
b396740[, (cols) := lapply(.SD, function(x) {
  fcase(
    x %in% c("Very often true", "Often true"), 0,
    x %in% c("Sometimes true"), 1,
    x %in% c("Never true", "Rarely true"), 2,
    default = NA_real_
  )
}), .SDcols = cols]
# 查看各列转换结果（可选）
for (col in cols) {
  cat("\n列", col, ":\n")
  print(table(b396740[[col]], useNA = "ifany"))
}

cols <- c("p29077", "p29078", "p29079", "p20488", "p20487", "p20490")
# 第一步：替换无效值
b396740[, (cols) := lapply(.SD, function(x) {
  x[x %in% c("Prefer not to answer", "", "Do not know")] <- NA_character_
  x
}), .SDcols = cols]
# 第二步：映射为数值
b396740[, (cols) := lapply(.SD, function(x) {
  fcase(
    x %in% c("Never true", "Rarely true"), 0,
    x %in% c("Sometimes true"), 1,
    x %in% c("Very often true", "Often true"), 2,
    default = NA_real_
  )
}), .SDcols = cols]
# 查看各列转换结果（可选）
for (col in cols) {
  cat("\n列", col, ":\n")
  print(table(b396740[[col]], useNA = "ifany"))
}
### 童年创伤指数和 ----
# 取两列的最大值，忽略缺失值
ct_pairs <- list(
  CT1 = c("p29076", "p20489"),
  CT2 = c("p29077", "p20488"),
  CT3 = c("p29078", "p20487"),
  CT4 = c("p29079", "p20490"),
  CT5 = c("p29080", "p20491")
)
# 批量计算 CT1~CT5
for (ct_name in names(ct_pairs)) {
  col1 <- ct_pairs[[ct_name]][1]
  col2 <- ct_pairs[[ct_name]][2]
  b396740[, (ct_name) := pmax(get(col1), get(col2), na.rm = TRUE)]
  b396740[is.na(get(col1)) & is.na(get(col2)), (ct_name) := NA_real_]
  cat("\n", ct_name, "列结果：\n")
  print(table(b396740[[ct_name]], useNA = "ifany"))
}

# 直接计算 Childhood_Trauma_Index，不删除任何缺失值行
cols_ct <- c("CT1", "CT2", "CT3", "CT4", "CT5")
b396740[, Childhood_Trauma_Index := ifelse(
  rowSums(is.na(.SD)) == length(cols_ct),
  NA_real_,
  rowSums(.SD, na.rm = TRUE)
), .SDcols = cols_ct]

# 打印结果
print(table(b396740$Childhood_Trauma_Index, useNA = "ifany"))

## 生活逆境指数 ----
cols <- paste0("p290", 81:90)
# 第一步：将无效值替换为 NA_character_
b396740[, (cols) := lapply(.SD, function(x) {
  x[x %in% c("Prefer not to answer", "", "Do not know")] <- NA_character_
  x
}), .SDcols = cols]
# 第二步：映射为数值（0/1/NA）
b396740[, (cols) := lapply(.SD, function(x) {
  fcase(
    x %in% c("No, never"), 0,
    x %in% c("Yes, but not in the last 12 months", "Yes, within the last 12 months"), 1,
    default = NA_real_
  )
}), .SDcols = cols]
# 查看各列转换结果（可选）
for (col in cols) {
  cat("\n列", col, ":\n")
  print(table(b396740[[col]], useNA = "ifany"))
}

# 定义需要处理的列名
cols <- c("p20521", "p20523", "p20524")
# 第一步：将无效值替换为 NA_character_
b396740[, (cols) := lapply(.SD, function(x) {
  x[x %in% c("Prefer not to answer", "", "Do not know")] <- NA_character_
  x
}), .SDcols = cols]
# 第二步：映射为数值（0/1/NA）
b396740[, (cols) := lapply(.SD, function(x) {
  fcase(
    x %in% c("Never true", "Rarely true"), 0,
    x %in% c("Often", "Sometimes true", "Very often true"), 1,
    default = NA_real_
  )
}), .SDcols = cols]

# 查看转换结果（可选）
for (col in cols) {
  cat("\n列", col, ":\n")
  print(table(b396740[[col]], useNA = "ifany"))
}

### 生活逆境指数和 ----
# 定义配对列
pairs <- list(
  AD1 = c("p29082", "p20521"),
  AD2 = c("p29083", "p20523"),
  AD3 = c("p29084", "p20524")
)
# 批量计算 AD1, AD2, AD3
for (name in names(pairs)) {
  col1 <- pairs[[name]][1]
  col2 <- pairs[[name]][2]
  b396740[, (name) := pmax(get(col1), get(col2), na.rm = TRUE)]
  b396740[is.na(get(col1)) & is.na(get(col2)), (name) := NA_real_]
  cat("\n", name, "列结果：\n")
  print(table(b396740[[name]], useNA = "ifany"))
}

# 定义要相加的列
cols_ad_sum <- c("p29081", paste0("p290", 85:90), "AD1", "AD2", "AD3")

# 直接计算 Adversity_Events_Index，不删除缺失值行
b396740[, Adversity_Events_Index := ifelse(
  rowSums(is.na(.SD)) == length(cols_ad_sum),
  NA_real_,
  rowSums(.SD, na.rm = TRUE)
), .SDcols = cols_ad_sum]

# 打印生活逆境指数分布
print(table(b396740$Adversity_Events_Index, useNA = "ifany"))

### 删除童年创伤生活逆境原数据 ----
cols_to_remove <- c("p29076", "p20489", "p29077", "p20488", "p29078", "p20487", 
                    "p29079", "p20490", "p29080", "p20491",
                    paste0("p290", 81:90),
                    "p20521", "p20523", "p20524")
b396740[, (cols_to_remove) := NULL]

## 自杀年龄、尝试，失访 ----
setnames(b396740, "p29118", "attage")
b396740[attage %in% c("Prefer not to answer", "Do not know"), attage := NA_character_]
b396740[attage %in% c(""), attage := NA_character_]
print(table(b396740$attage, useNA = "ifany"))

setnames(b396740, "p29116", "att1")
b396740[att1 %in% c("Prefer not to answer", "Do not know"), att1 := NA_character_]
b396740[, att1 := fcase(
  att1 == "No", 0,
  att1 %in% c("Yes, once", "Yes, more than once"), 1,
  default = NA_real_
)]
print(table(b396740$att1, useNA = "ifany"))

setnames(b396740, "p20483", "att2")
b396740[att2 %in% c("Prefer not to answer", "Do not know"), att2 := NA_character_]
b396740[, att2 := fcase(
  att2 == "No", 0,
  att2 %in% c("Yes"), 1,
  default = NA_real_
)]
print(table(b396740$att2, useNA = "ifany"))

b396740[, evenatt := pmax(att1, att2, na.rm = TRUE)]
b396740[, evenatt := fcoalesce(pmax(att1, att2, na.rm = TRUE), 0)]
print(table(b396740$evenatt, useNA = "ifany"))

setnames(b396740, "p190", "lost")
b396740[lost %in% c("Prefer not to answer", "", "Do not know"), lost := NA_character_]
setnames(b396740, "p191", "losttime")
b396740[losttime %in% c("Prefer not to answer", "", "Do not know"), losttime := NA_character_]

## 共病 ----
cols_sub <- c("p130854", "p130856", "p130858", "p130860", "p130862", 
              "p130864", "p130866", "p130868", "p130870", "p130872")
b396740[, (cols_sub) := lapply(.SD, function(x) {
  ifelse(x %in% c("", " ", "NA", "N/A") | is.na(x), NA_character_, x)
}), .SDcols = cols_sub]
b396740[, (cols_sub) := lapply(.SD, function(x) ifelse(!is.na(x), 1, NA_real_)), .SDcols = cols_sub]
b396740[, `Substance-related` := do.call(pmax, c(.SD, list(na.rm = TRUE))), .SDcols = cols_sub]
b396740[`Substance-related` == -Inf, `Substance-related` := NA_real_]
# 查看结果
print(table(b396740$`Substance-related`, useNA = "ifany"))

cols_schiz <- c("p130874", "p130878", "p130880", "p130882", "p130884", 
                "p130886", "p130888", "p130990")
b396740[, (cols_schiz) := lapply(.SD, function(x) {
  ifelse(x %in% c("", " ", "NA", "N/A") | is.na(x), NA_character_, x)
}), .SDcols = cols_schiz]
b396740[, (cols_schiz) := lapply(.SD, function(x) ifelse(!is.na(x), 1, NA_real_)), .SDcols = cols_schiz]
b396740[, `Schizophrenia` := do.call(pmax, c(.SD, list(na.rm = TRUE))), .SDcols = cols_schiz]
b396740[`Schizophrenia` == -Inf, `Schizophrenia` := NA_real_]
# 查看结果
print(table(b396740$`Schizophrenia`, useNA = "ifany"))

cols_bipolar <- c("p130890", "p130892")
b396740[, (cols_bipolar) := lapply(.SD, function(x) {
  ifelse(x %in% c("", " ", "NA", "N/A") | is.na(x), NA_character_, x)
}), .SDcols = cols_bipolar]
b396740[, (cols_bipolar) := lapply(.SD, function(x) ifelse(!is.na(x), 1, NA_real_)), .SDcols = cols_bipolar]
b396740[, `bipolar` := do.call(pmax, c(.SD, list(na.rm = TRUE))), .SDcols = cols_bipolar]
b396740[`bipolar` == -Inf, `bipolar` := NA_real_]
# 查看结果
print(table(b396740$`bipolar`, useNA = "ifany"))

cols_depressive <- c("p130894", "p130896")
b396740[, (cols_depressive) := lapply(.SD, function(x) {
  ifelse(x %in% c("", " ", "NA", "N/A") | is.na(x), NA_character_, x)
}), .SDcols = cols_depressive]
b396740[, (cols_depressive) := lapply(.SD, function(x) ifelse(!is.na(x), 1, NA_real_)), .SDcols = cols_depressive]
b396740[, `depressive` := do.call(pmax, c(.SD, list(na.rm = TRUE))), .SDcols = cols_depressive]
b396740[`depressive` == -Inf, `depressive` := NA_real_]
# 查看结果
print(table(b396740$`depressive`, useNA = "ifany"))

cols_anxiety <- c("p130904", "p130906")
b396740[, (cols_anxiety) := lapply(.SD, function(x) {
  ifelse(x %in% c("", " ", "NA", "N/A") | is.na(x), NA_character_, x)
}), .SDcols = cols_anxiety]
b396740[, (cols_anxiety) := lapply(.SD, function(x) ifelse(!is.na(x), 1, NA_real_)), .SDcols = cols_anxiety]
b396740[, `Anxiety` := do.call(pmax, c(.SD, list(na.rm = TRUE))), .SDcols = cols_anxiety]
b396740[`Anxiety` == -Inf, `Anxiety` := NA_real_]
# 查看结果
print(table(b396740$`Anxiety`, useNA = "ifany"))

cols_personality <- c("p130876", "p130932", "p130934", "p130936", "p130938", 
                      "p130946", "p130948")
b396740[, (cols_personality) := lapply(.SD, function(x) {
  ifelse(x %in% c("", " ", "NA", "N/A") | is.na(x), NA_character_, x)
}), .SDcols = cols_personality]
b396740[, (cols_personality) := lapply(.SD, function(x) ifelse(!is.na(x), 1, NA_real_)), .SDcols = cols_personality]
b396740[, `Personality` := do.call(pmax, c(.SD, list(na.rm = TRUE))), .SDcols = cols_personality]
b396740[`Personality` == -Inf, `Personality` := NA_real_]
# 查看结果
print(table(b396740$`Personality`, useNA = "ifany"))

cols_developmental <- c("p130950", "p130952", "p130954", "p130958", "p130960", 
                        "p130962", "p130964", "p130966", "p130968", "p130970", 
                        "p130972", "p130974", "p130976", "p130978", "p130980", 
                        "p130982", "p130984", "p130986", "p130988")
b396740[, (cols_developmental) := lapply(.SD, function(x) {
  ifelse(x %in% c("", " ", "NA", "N/A") | is.na(x), NA_character_, x)
}), .SDcols = cols_developmental]
b396740[, (cols_developmental) := lapply(.SD, function(x) ifelse(!is.na(x), 1, NA_real_)), .SDcols = cols_developmental]
b396740[, `Developmental` := do.call(pmax, c(.SD, list(na.rm = TRUE))), .SDcols = cols_developmental]
b396740[`Developmental` == -Inf, `Developmental` := NA_real_]
# 查看结果
print(table(b396740$`Developmental`, useNA = "ifany"))

cols_others <- c("p130898", "p130900", "p130902", "p130910", "p130912", 
                 "p130914", "p130916", "p130918", "p130920")
b396740[, (cols_others) := lapply(.SD, function(x) {
  ifelse(x %in% c("", " ", "NA", "N/A") | is.na(x), NA_character_, x)
}), .SDcols = cols_others]
b396740[, (cols_others) := lapply(.SD, function(x) ifelse(!is.na(x), 1, NA_real_)), .SDcols = cols_others]
b396740[, `Others` := do.call(pmax, c(.SD, list(na.rm = TRUE))), .SDcols = cols_others]
b396740[`Others` == -Inf, `Others` := NA_real_]
# 查看结果
print(table(b396740$`Others`, useNA = "ifany"))

### 删除共病等原数据 ----
cols_to_remove <- c(
  # Substance-related
  "p130854", "p130856", "p130858", "p130860", "p130862",
  "p130864", "p130866", "p130868", "p130870", "p130872",
  # Schizophrenia
  "p130874", "p130878", "p130880", "p130882", "p130884",
  "p130886", "p130888", "p130990",
  # bipolar
  "p130890", "p130892",
  # depressive
  "p130894", "p130896",
  # Anxiety
  "p130904", "p130906",
  # Personality
  "p130876", "p130932", "p130934", "p130936", "p130938",
  "p130946", "p130948",
  # Developmental
  "p130950", "p130952", "p130954", "p130958", "p130960",
  "p130962", "p130964", "p130966", "p130968", "p130970",
  "p130972", "p130974", "p130976", "p130978", "p130980",
  "p130982", "p130984", "p130986", "p130988",
  # Others
  "p130898", "p130900", "p130902", "p130910", "p130912",
  "p130914", "p130916", "p130918", "p130920",
  "Support","Loneliness","Confide","died0","died1","att1","att2",
  "CT1","CT2","CT3","CT4","CT5",
  "AD1","AD2","AD3"
)

# 删除这些列
b396740[, (cols_to_remove) := NULL]

bb141256 <- b396740
# 保存为 RDS 文件，保留所有属性
saveRDS(bb141256, "bb987.rds")

