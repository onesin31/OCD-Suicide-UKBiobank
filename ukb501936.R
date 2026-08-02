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
a987 <- dt_501936[id %in% dt_987$id]   # id 在 987 中存在的行
a500949 <- dt_501936[!id %in% dt_987$id] # id 不在 987 中的行

# 预处理 ----
## 性别、年龄、TDI、BMI、睡眠、ocd、种族
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

## 收入吸烟饮酒，入组时间，死亡时间
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
## 就业学历
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


## 社交孤独指数
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

# 保留至少有一个非缺失的个体，计算 Support_index（三个维度之和）
a500949 <- a500949[rowSums(is.na(a500949[, .(Support, Loneliness, Confide)])) != 3]
a500949[, Support_index := rowSums(.SD, na.rm = TRUE), .SDcols = c("Support", "Loneliness", "Confide")]
print(table(a500949$Support_index, useNA = "ifany"))

b396740 <- a500949
# 保存为 RDS 文件，保留所有属性
saveRDS(b396740, "b396740.rds")

# 一轮:指数与删除 ----
library(data.table)
setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
# 读取文件
b396740 <- readRDS("b396740.rds")

# 童年创伤指数 ----
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

# 童年创伤指数和 
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

# 删除 CT1~CT5 中任意存在缺失值的参与者
cols_ct <- c("CT1", "CT2", "CT3", "CT4", "CT5")
b396740 <- b396740[complete.cases(b396740[, ..cols_ct]), ]

# 计算 Childhood_Trauma_Index（此时所有行 CT1~CT5 均无缺失）
b396740[, Childhood_Trauma_Index := rowSums(.SD), .SDcols = cols_ct]

# 打印结果
print(table(b396740$Childhood_Trauma_Index, useNA = "ifany"))

# 生活逆境指数 ----
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

# 生活逆境指数和
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
# 删除这些列中任意存在缺失值的参与者
b396740 <- b396740[complete.cases(b396740[, ..cols_ad_sum]), ]

# 计算 Adversity_Events_Index（此时已无缺失）
b396740[, Adversity_Events_Index := rowSums(.SD), .SDcols = cols_ad_sum]

# 打印生活逆境指数分布
print(table(b396740$Adversity_Events_Index, useNA = "ifany"))

# 删除童年创伤生活逆境原数据 ----
cols_to_remove <- c("p29076", "p20489", "p29077", "p20488", "p29078", "p20487", 
                    "p29079", "p20490", "p29080", "p20491",
                    paste0("p290", 81:90),
                    "p20521", "p20523", "p20524")
b396740[, (cols_to_remove) := NULL]

# 自杀年龄，失访 ----
setnames(b396740, "p29118", "attage")
b396740[attage %in% c("Prefer not to answer", "Do not know"), attage := NA_character_]
b396740 <- b396740[!is.na(attage)]
b396740[attage %in% c(""), attage := NA_character_]
print(table(b396740$attage, useNA = "ifany"))

setnames(b396740, "p190", "lost")
b396740[lost %in% c("Prefer not to answer", "", "Do not know"), lost := NA_character_]
setnames(b396740, "p191", "losttime")
b396740[losttime %in% c("Prefer not to answer", "", "Do not know"), losttime := NA_character_]

# 共病 ----
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

# 删除共病等原数据 ----
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
  "Support","Loneliness","Confide","died0","died1",
  "CT1","CT2","CT3","CT4","CT5",
  "AD1","AD2","AD3",
  "p29116","p20483"
)

# 删除这些列
b396740[, (cols_to_remove) := NULL]

bb141414 <- b396740
# 保存为 RDS 文件，保留所有属性
saveRDS(bb141414, "bb141414.rds")
## 二轮:多重插补 ----
library(data.table)
library(mice)
setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
bb141414 <- readRDS("bb141414.rds")
bb987    <- readRDS("bb987.rds")
c142401 <- rbindlist(list(bb987, bb141414), fill = TRUE)

c142401[, Sleep_Duration := as.numeric(Sleep_Duration)]
# 定义需要插补的列名
impute_cols <- c("Age", "Sex", "TDI", "Ethnicity", "Employment", "Qualifications", "BMI",
                 "Income", "Drinking_Status", "Smoking_Status", "Sleep_Duration", "Support_index",
                 "Childhood_Trauma_Index", "Adversity_Events_Index")

dt_impute <- c142401[, ..impute_cols]
# 将字符型转为因子（mice 要求）
for (col in impute_cols) {
  if (is.character(dt_impute[[col]])) {
    set(dt_impute, j = col, value = as.factor(dt_impute[[col]]))
  }
}

set.seed(42)
imp <- mice(dt_impute, m = 2, maxit = 1, printFlag = TRUE)
completed <- complete(imp, action = 1)

# 将插补后的列写回原表
for (col in impute_cols) {
  set(c142401, j = col, value = completed[[col]])
}

# 验证：无缺失值
stopifnot(all(sapply(c142401[, ..impute_cols], function(x) sum(is.na(x))) == 0))

# 辅助函数
safe_as_date <- function(x) {
  if (is.character(x)) as.Date(x) else x
}

## 时间依赖 ----
# 将相关日期列转为 Date 类型
c142401[, start := safe_as_date(start)]
c142401[, ocdtime := safe_as_date(ocdtime)]

# 筛选需要生成新行的记录
split_idx <- which(c142401$state == 1 & c142401$ocdtime > c142401$start)

if (length(split_idx) > 0) {
  # 复制这些行作为新行
  new_rows <- c142401[split_idx]
  # 新行的 start 改为 ocdtime，state 改为 0
  new_rows[, `:=`(
    start = ocdtime,
    state = 2
  )]
  
  # 将原行的 state 改为 1
  c142401[split_idx, state := 3]
  
  
  # 合并新行到原数据
  c142401 <- rbindlist(list(c142401, new_rows), use.names = TRUE, fill = TRUE)
  
  # 排序（若存在 id 列）
  if ("id" %in% names(c142401)) {
    setorder(c142401, id, start)
  } else {
    setorder(c142401, start)
  }
  
  message("已处理 ", length(split_idx), " 行记录：原行 state 改为 3，新增 state=2 行，start 更新为 ocdtime。")
} else {
  message("没有需要处理的记录。")
}

cc142748 <- c142401
# 保存为 RDS 文件，保留所有属性
saveRDS(cc142748, "cc142748.rds")

## 三轮:icd10未遂的匹配 ----
library(data.table)
setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
cc142748 <- readRDS("cc142748.rds")

# 读取行为数据，只保留 id 和 behaviortime
behavior <- fread("behavior3871.csv", select = c("id", "behaviortime"))
# 每个 id 只保留第一条行为记录（避免一对多匹配）
behavior <- unique(behavior, by = "id", fromLast = FALSE)

# 在 cc142748 中新增三列，初始为 NA
cc142748[, `:=`(behaviortime = NA_character_,
                attempt = NA_integer_,
                time = NA_integer_)]

# 匹配：将 behavior 中的 behaviortime 填入 cc142748
cc142748[behavior, on = "id", behaviortime := i.behaviortime]

# 转换日期格式（start 和 behaviortime）
cc142748[, start_date := as.Date(start, format = "%Y/%m/%d")]
cc142748[, behav_date := as.Date(behaviortime, format = "%Y/%m/%d")]

# 计算天数差（behaviortime - start）
cc142748[, diff_days := as.numeric(behav_date - start_date)]

# 若天数差为非负数，则 time 填入该差值
cc142748[!is.na(diff_days) & diff_days >= 0, time := diff_days]

# 根据 time 是否有值设置 attempt：有值则为 1，否则为 0
cc142748[!is.na(time), attempt := 1L]
cc142748[is.na(time), attempt := 0L]

# 删除临时列（behaviortime 相关）
cc142748[, c("start_date", "behav_date", "diff_days") := NULL]

# 重新计算 start_date（之前已删除）并转换 ocdtime
cc142748[, start_date := as.Date(start, format = "%Y/%m/%d")]
cc142748[, ocd_date := as.Date(ocdtime, format = "%Y/%m/%d")]

# 筛选满足条件的行
idx <- which(cc142748$state == 3 & cc142748$attempt == 1)

if (length(idx) > 0) {
  # 将 attempt 改为 0
  cc142748[idx, attempt := 0L]
  
  # 计算 ocdtime 与 start 的天数差
  cc142748[idx, ocd_diff := as.numeric(ocd_date - start_date)]
  
  # 若天数差非负，则更新 time 为该差值；否则将 time 设为 NA（因为原 time 不再适用）
  cc142748[idx, time := ifelse(!is.na(ocd_diff) & ocd_diff >= 0, ocd_diff, NA_integer_)]
  
  # 删除临时差值列
  cc142748[, ocd_diff := NULL]
}

# 删除临时日期列
cc142748[, c("start_date", "ocd_date") := NULL]

# 可选：查看结果
print(table(cc142748$attempt, useNA = "ifany"))
cat("time 列缺失值数量：", sum(is.na(cc142748$time)), "\n")

## 自我报告未遂的匹配 ----
# 将 attage 和 Age 转为数值，计算年份差（直接转换，不保留临时列）
cc142748[, age_diff_years := as.numeric(attage) - as.numeric(Age)]

# 若年份差非负且原 time 缺失，则 time 填入转换后的天数（0 年按 365 天计）
cc142748[is.na(time) & !is.na(age_diff_years) & age_diff_years >= 0, 
         time := ifelse(age_diff_years == 0, 365L, as.integer(age_diff_years * 365))]

# 根据 time 是否有值更新 attempt
cc142748[!is.na(time), attempt := 1L]
cc142748[is.na(time), attempt := 0L]

# 删除临时列
cc142748[, age_diff_years := NULL]

# 重新生成日期列（用于强制覆盖 state==2 的行）
cc142748[, start_date := as.Date(start, format = "%Y/%m/%d")]
cc142748[, ocd_date := as.Date(ocdtime, format = "%Y/%m/%d")]

idx2 <- which(cc142748$state == 3 & cc142748$attempt == 1)
if (length(idx2) > 0) {
  cc142748[idx2, attempt := 0L]
  cc142748[idx2, ocd_diff := as.numeric(ocd_date - start_date)]
  cc142748[idx2, time := ifelse(!is.na(ocd_diff) & ocd_diff >= 0, ocd_diff, NA_integer_)]
  cc142748[, ocd_diff := NULL]
}

# 删除临时日期列
cc142748[, c("start_date", "ocd_date") := NULL]

# 查看最终结果
print(table(cc142748$attempt, useNA = "ifany"))
cat("time 列缺失值数量：", sum(is.na(cc142748$time)), "\n")

## 自我报告死亡的匹配 ----
suicidedied <- fread("suicidedied.csv", select = 1)
suicide_ids <- unique(suicidedied[[1]])

# 1. 新增 suicide 列，初始依据 attempt（1 -> 1，0 -> 0）
cc142748[, suicide := ifelse(attempt == 1, 1L, 0L)]
# 2. 对于 suicide == 0 的行，若 id 在 suicidedied 中，改为 1（自杀死亡）
cc142748[suicide == 0 & id %in% suicide_ids, suicide := 1L]
# 3. 对于剩下 suicide == 0 的行，若 diedtime 列有值（非NA），改为 2（其他原因死亡）
cc142748[suicide == 0 & !is.na(diedtime), suicide := 2L]
# 4. 更新 attempt == 0 且 time 为缺失值的行：
#   若 diedtime 列有值，则 attempt == 2，time 列取 diedtime 到 start 的天数差
cc142748[attempt == 0 & is.na(time) & !is.na(diedtime), 
        `:=`(attempt = 2L,
             time = as.numeric(diedtime - start))]
# 5. 对于 state == 3 的行，限制 time 不超过 ocdtime - start 的天数差
# 重新计算日期列（原临时列可能已被删除）
cc142748[, start_date := as.Date(start, format = "%Y/%m/%d")]
cc142748[, ocd_date := as.Date(ocdtime, format = "%Y/%m/%d")]
cc142748[, ocd_diff := as.numeric(ocd_date - start_date)]

# 情况1：time 缺失，填入 ocd_diff（若非负）
cc142748[state == 3 & is.na(time) & !is.na(ocd_diff) & ocd_diff >= 0, 
         time := ocd_diff]

# 情况2：time 有值且大于 ocd_diff（ocd_diff 非负），则截断
cc142748[state == 3 & !is.na(time) & !is.na(ocd_diff) & ocd_diff >= 0 & time > ocd_diff, 
         time := ocd_diff]

# 删除临时列
cc142748[, c("start_date", "ocd_date", "ocd_diff") := NULL]

# 之后 time 为缺失值的行，time 列取 2025年9月30日 到 start 的天数差
cc142748[is.na(time), 
        time := as.numeric(as.Date("2025-09-30") - start)]

# 输出结果验证
cat("\n=== suicide 列分布 ===\n")
print(table(cc142748$suicide, useNA = "ifany"))
cat("\n=== attempt 列更新后分布 ===\n")
print(table(cc142748$attempt, useNA = "ifany"))

# 检查 state == 1 (OCD) 的样本分布
cat("\n=== 在 state == 1 (OCD) 的样本中 ===\n")
state1_data <- cc142748[state == 1, ]
cat("\n--- suicide 分布 ---\n")
print(table(state1_data$suicide, useNA = "ifany"))
cat("\n--- attempt 分布 ---\n")
print(table(state1_data$attempt, useNA = "ifany"))

# 检查 state == 2 (非OCD) 的样本分布
cat("\n=== 在 state == 2 (非OCD) 的样本中 ===\n")
state2_data <- cc142748[state == 2, ]
cat("\n--- suicide 分布 ---\n")
print(table(state2_data$suicide, useNA = "ifany"))
cat("\n--- attempt 分布 ---\n")
print(table(state2_data$attempt, useNA = "ifany"))

# 检查 state == 3 的样本分布（如果有）
cat("\n=== 在 state == 3 的样本中 ===\n")
state3_data <- cc142748[state == 3, ]
cat("\n--- suicide 分布 ---\n")
print(table(state3_data$suicide, useNA = "ifany"))
cat("\n--- attempt 分布 ---\n")
print(table(state3_data$attempt, useNA = "ifany"))

# 检查 state == 0 (Non-OCD) 的样本分布
cat("\n=== 在 state == 0 (Non-OCD) 的样本中 ===\n")
state0_data <- cc142748[state == 0, ]
cat("\n--- suicide 分布 ---\n")
print(table(state0_data$suicide, useNA = "ifany"))
cat("\n--- attempt 分布 ---\n")
print(table(state0_data$attempt, useNA = "ifany"))

ccc142748 <- cc142748
# 保存为 RDS 文件，保留所有属性
saveRDS(ccc142748, "ccc142748.rds")

## 自我报告死亡 ----
# 筛选 suicide == 1 的行（自杀死亡人群）
suicide1_data <- cc142748[suicide == 1]

# 定义精神障碍列名（与前面共病分析一致）
comorbidity_cols <- c("Substance-related", "Schizophrenia", "bipolar", "depressive", 
                      "Anxiety", "Personality", "Developmental", "Others")

# 确保这些列为数值型（若原为因子或字符则转换）
for (col in comorbidity_cols) {
  if (!is.numeric(suicide1_data[[col]])) {
    set(suicide1_data, j = col, value = as.numeric(as.character(suicide1_data[[col]])))
  }
}

# 计算每个个体的共病总数（每列为 0/1 表示有无该障碍）
suicide1_data[, comorbidity_count := rowSums(.SD, na.rm = TRUE), .SDcols = comorbidity_cols]

# 按 state 分组汇总统计量（均值、标准差、最小值、最大值、样本量）
summary_suicide_state <- suicide1_data[, .(
  N = .N,
  mean_comorbidity = mean(comorbidity_count, na.rm = TRUE),
  sd_comorbidity = sd(comorbidity_count, na.rm = TRUE),
  min_comorbidity = min(comorbidity_count, na.rm = TRUE),
  max_comorbidity = max(comorbidity_count, na.rm = TRUE)
), by = state]

# 生成各 state 下共病数量的频数分布表（宽格式）
freq_suicide_table <- suicide1_data[, .N, by = .(state, comorbidity_count)]
freq_suicide_wide <- dcast(freq_suicide_table, state ~ comorbidity_count, value.var = "N", fill = 0)

# 输出结果
cat("\n========== suicide=1 样本按 state 分组的共病数量汇总 ==========\n")
print(summary_suicide_state)

cat("\n========== 各 state 下共病数量频数分布（suicide=1） ==========\n")
print(freq_suicide_wide)

### 四轮 ----
library(data.table)
library(tidyverse)
library(survival)
library(broom)
library(cmprsk)
setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
ccc142748 <- readRDS("ccc142748.rds")

# 变量类型转换（一次性处理所有分类和连续变量）
ccc142748 <- ccc142748 %>%
  mutate(
    # 创建新的state分组：0和3为Non-OCD，1和2为OCD
    state_group = case_when(
      state %in% c(0, 3) ~ "Non-OCD",
      state %in% c(1, 2) ~ "OCD",
      TRUE ~ NA_character_
    ),
    state_group = factor(state_group, levels = c("Non-OCD", "OCD")),
    # 其他分类变量
    Sex            = factor(Sex),
    Ethnicity      = factor(Ethnicity),
    Qualifications = factor(Qualifications),
    Employment     = factor(Employment),
    Income         = factor(Income),
    Drinking_Status = factor(Drinking_Status),
    Smoking_Status  = factor(Smoking_Status),
    # 连续变量
    across(c(Support_index, Childhood_Trauma_Index, Adversity_Events_Index,
             Age, BMI, TDI, Sleep_Duration, time), as.numeric)
  )

### Cox回归分析(自杀未遂) ----
# 筛选有效时间，并定义事件：attempt == 1 为事件，0 和 2 均为删失
cox_data <- ccc142748 %>%
  filter(!is.na(time), time > 0) %>%
  mutate(event = as.numeric(attempt == 1))   # 事件指示变量

surv_obj <- Surv(time = cox_data$time, event = cox_data$event)

# 定义协变量集合
base_covars   <- c("Age", "Sex", "Ethnicity")
socio_covars  <- c("TDI", "Qualifications", "Employment", "Income")
health_covars <- c("BMI", "Drinking_Status", "Smoking_Status", "Sleep_Duration")

# 构建模型公式，使用 state_group 代替原来的 state
models <- list(
  model1 = as.formula(paste("surv_obj ~ state_group +", paste(base_covars, collapse = " + "))),
  model2 = as.formula(paste("surv_obj ~ state_group +", paste(c(base_covars, socio_covars), collapse = " + "))),
  model3 = as.formula(paste("surv_obj ~ state_group +", paste(c(base_covars, socio_covars, health_covars), collapse = " + ")))
)

# 提取 HR 的通用函数（注意变量名 state_group）
extract_hr <- function(model_fit, model_name) {
  tidy_output <- tidy(model_fit, conf.int = TRUE, exponentiate = TRUE)
  state_row <- tidy_output %>% filter(grepl("^state_group", term))
  if (nrow(state_row) == 0) stop("模型中未找到 state_group 变量")
  p_val <- state_row$p.value
  p_fmt <- ifelse(p_val < 0.001, "<0.001", round(p_val, 3))
  data.frame(
    Model = model_name,
    N = model_fit$n,
    HR = round(state_row$estimate, 3),
    CI_lower = round(state_row$conf.low, 3),
    CI_upper = round(state_row$conf.high, 3),
    P_value = as.character(p_fmt)
  )
}

results_list <- list()
for (i in seq_along(models)) {
  fit <- coxph(models[[i]], data = cox_data)
  results_list[[i]] <- extract_hr(fit, paste0("模型", i))
}
results_table <- bind_rows(results_list)
cat("\n========== Cox 回归结果（自杀未遂）==========\n")
print(results_table)

### 竞争回归分析(自杀未遂) ----
# 准备数据：定义状态 1=自杀未遂，2=死亡（竞争事件），0=删失
crr_base <- ccc142748 %>%
  filter(!is.na(time), time > 0) %>%
  mutate(status = case_when(
    attempt == 1 ~ 1,
    attempt == 2 ~ 2,
    attempt == 0 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(status))

# 改进的协变量矩阵构建函数（使用行索引而非行名）
build_cov_matrix <- function(data, covars) {
  # 添加临时行号
  data <- data %>% mutate(.temp_id = row_number())
  # 保留协变量完整的观测
  complete_ids <- data %>%
    select(all_of(covars), .temp_id) %>%
    na.omit() %>%
    pull(.temp_id)
  data_sub <- data %>% filter(.temp_id %in% complete_ids)
  # 构建模型矩阵
  formula <- as.formula(paste("~", paste(covars, collapse = " + ")))
  mm <- model.matrix(formula, data = data_sub)
  mm <- mm[, -1, drop = FALSE]   # 删除截距
  # 返回协变量矩阵和对应的原始数据行索引（用于提取时间和状态）
  list(cov = mm, idx = data_sub$.temp_id)
}

# 模型1：基础协变量（使用 state_group 代替 state）
covars1 <- c("state_group", "Age", "Sex", "Ethnicity")
res1 <- build_cov_matrix(crr_base, covars1)
crr_fit1 <- crr(ftime = crr_base$time[res1$idx], 
                fstatus = crr_base$status[res1$idx],
                cov1 = res1$cov, failcode = 1, cencode = 0)

# 检查收敛
if (!crr_fit1$converged) warning("模型1未收敛")

# 模型2：增加社会经济变量
covars2 <- c("state_group", "Age", "Sex", "Ethnicity", "TDI", "Qualifications", "Employment", "Income")
res2 <- build_cov_matrix(crr_base, covars2)
crr_fit2 <- crr(ftime = crr_base$time[res2$idx], 
                fstatus = crr_base$status[res2$idx],
                cov1 = res2$cov, failcode = 1, cencode = 0)
if (!crr_fit2$converged) warning("模型2未收敛")

# 模型3：增加健康行为变量
covars3 <- c("state_group", "Age", "Sex", "Ethnicity", "TDI", "Qualifications", "Employment", "Income",
             "BMI", "Drinking_Status", "Smoking_Status", "Sleep_Duration")
res3 <- build_cov_matrix(crr_base, covars3)
crr_fit3 <- crr(ftime = crr_base$time[res3$idx], 
                fstatus = crr_base$status[res3$idx],
                cov1 = res3$cov, failcode = 1, cencode = 0)
if (!crr_fit3$converged) warning("模型3未收敛")

# 提取竞争风险结果（匹配 state_group 变量）
extract_crr <- function(crr_obj, model_name, n_used) {
  summary_crr <- summary(crr_obj)
  coef_table <- summary_crr$coef
  # 匹配 state_group（可能是 state_groupOCD 等）
  state_rows <- grep("^state_group", rownames(coef_table), value = TRUE)
  if (length(state_rows) == 0) stop("未找到 state_group 变量")
  row <- coef_table[state_rows[1], ]
  log_hr <- row["coef"]
  se_log_hr <- row["SE"]
  hr <- exp(log_hr)
  ci_lower <- exp(log_hr - 1.96 * se_log_hr)
  ci_upper <- exp(log_hr + 1.96 * se_log_hr)
  p_val <- row["p-value"]
  p_fmt <- ifelse(p_val < 0.001, "<0.001", round(p_val, 3))
  data.frame(
    Model = model_name,
    N = n_used,
    SHR = round(hr, 3),
    CI_lower = round(ci_lower, 3),
    CI_upper = round(ci_upper, 3),
    P_value = as.character(p_fmt)
  )
}

crr_results <- bind_rows(
  extract_crr(crr_fit1, "模型1", length(res1$idx)),
  extract_crr(crr_fit2, "模型2", length(res2$idx)),
  extract_crr(crr_fit3, "模型3", length(res3$idx))
)

cat("\n========== 竞争风险回归结果（自杀未遂，死亡为竞争事件）==========\n")
print(crr_results)

### Cox回归分析(自杀行为) ----
# 筛选有效时间，定义事件：suicide == 1 为事件，0 和 2 均为删失
cox_data_suicide <- ccc142748 %>%
  filter(!is.na(time), time > 0) %>%
  mutate(event = as.numeric(suicide == 1))

surv_obj_suicide <- Surv(time = cox_data_suicide$time, event = cox_data_suicide$event)

# 协变量集合
base_covars   <- c("Age", "Sex", "Ethnicity")
socio_covars  <- c("TDI", "Qualifications", "Employment", "Income")
health_covars <- c("BMI", "Drinking_Status", "Smoking_Status", "Sleep_Duration")

# 构建模型公式（使用 state_group）
models_suicide <- list(
  model1 = as.formula(paste("surv_obj_suicide ~ state_group +", paste(base_covars, collapse = " + "))),
  model2 = as.formula(paste("surv_obj_suicide ~ state_group +", 
                            paste(c(base_covars, socio_covars), collapse = " + "))),
  model3 = as.formula(paste("surv_obj_suicide ~ state_group +", 
                            paste(c(base_covars, socio_covars, health_covars), collapse = " + ")))
)

# 提取 HR 的通用函数
extract_hr_suicide <- function(model_fit, model_name) {
  tidy_output <- tidy(model_fit, conf.int = TRUE, exponentiate = TRUE)
  state_row <- tidy_output %>% filter(grepl("^state_group", term))
  if (nrow(state_row) == 0) stop("模型中未找到 state_group 变量")
  p_val <- state_row$p.value
  p_fmt <- ifelse(p_val < 0.001, "<0.001", round(p_val, 3))
  data.frame(
    Model = model_name,
    N = model_fit$n,
    HR = round(state_row$estimate, 3),
    CI_lower = round(state_row$conf.low, 3),
    CI_upper = round(state_row$conf.high, 3),
    P_value = as.character(p_fmt)
  )
}

# 依次拟合三个模型
results_suicide_list <- list()
for (i in seq_along(models_suicide)) {
  fit <- coxph(models_suicide[[i]], data = cox_data_suicide)
  results_suicide_list[[i]] <- extract_hr_suicide(fit, paste0("模型", i))
}
results_suicide_table <- bind_rows(results_suicide_list)

cat("\n========== Cox 回归结果（自杀事件，删失包括无事件和其他死亡）==========\n")
print(results_suicide_table)


### 竞争回归分析(自杀行为) ----
# 准备数据：定义状态 1=自杀事件（suicide==1），2=其他死亡（竞争事件），0=删失
crr_base <- ccc142748 %>%
  filter(!is.na(time), time > 0) %>%
  mutate(status = case_when(
    suicide == 1 ~ 1,
    suicide == 2 ~ 2,
    suicide == 0 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(status))

# 改进的协变量矩阵构建函数（使用行索引而非行名）
build_cov_matrix <- function(data, covars) {
  # 添加临时行号
  data <- data %>% mutate(.temp_id = row_number())
  # 保留协变量完整的观测
  complete_ids <- data %>%
    select(all_of(covars), .temp_id) %>%
    na.omit() %>%
    pull(.temp_id)
  data_sub <- data %>% filter(.temp_id %in% complete_ids)
  # 构建模型矩阵
  formula <- as.formula(paste("~", paste(covars, collapse = " + ")))
  mm <- model.matrix(formula, data = data_sub)
  mm <- mm[, -1, drop = FALSE]   # 删除截距
  # 返回协变量矩阵和对应的原始数据行索引（用于提取时间和状态）
  list(cov = mm, idx = data_sub$.temp_id)
}

# 模型1：基础协变量（使用 state_group 代替 state）
covars1 <- c("state_group", "Age", "Sex", "Ethnicity")
res1 <- build_cov_matrix(crr_base, covars1)
crr_fit1 <- crr(ftime = crr_base$time[res1$idx], 
                fstatus = crr_base$status[res1$idx],
                cov1 = res1$cov, failcode = 1, cencode = 0)

# 检查收敛
if (!crr_fit1$converged) warning("模型1未收敛")

# 模型2：增加社会经济变量
covars2 <- c("state_group", "Age", "Sex", "Ethnicity", "TDI", "Qualifications", "Employment", "Income")
res2 <- build_cov_matrix(crr_base, covars2)
crr_fit2 <- crr(ftime = crr_base$time[res2$idx], 
                fstatus = crr_base$status[res2$idx],
                cov1 = res2$cov, failcode = 1, cencode = 0)
if (!crr_fit2$converged) warning("模型2未收敛")

# 模型3：增加健康行为变量
covars3 <- c("state_group", "Age", "Sex", "Ethnicity", "TDI", "Qualifications", "Employment", "Income",
             "BMI", "Drinking_Status", "Smoking_Status", "Sleep_Duration")
res3 <- build_cov_matrix(crr_base, covars3)
crr_fit3 <- crr(ftime = crr_base$time[res3$idx], 
                fstatus = crr_base$status[res3$idx],
                cov1 = res3$cov, failcode = 1, cencode = 0)
if (!crr_fit3$converged) warning("模型3未收敛")

# 提取竞争风险结果（匹配 state_group 变量）
extract_crr <- function(crr_obj, model_name, n_used) {
  summary_crr <- summary(crr_obj)
  coef_table <- summary_crr$coef
  # 匹配 state_group（可能是 state_groupOCD 等）
  state_rows <- grep("^state_group", rownames(coef_table), value = TRUE)
  if (length(state_rows) == 0) stop("未找到 state_group 变量")
  row <- coef_table[state_rows[1], ]
  log_hr <- row["coef"]
  se_log_hr <- row["SE"]
  hr <- exp(log_hr)
  ci_lower <- exp(log_hr - 1.96 * se_log_hr)
  ci_upper <- exp(log_hr + 1.96 * se_log_hr)
  p_val <- row["p-value"]
  p_fmt <- ifelse(p_val < 0.001, "<0.001", round(p_val, 3))
  data.frame(
    Model = model_name,
    N = n_used,
    SHR = round(hr, 3),
    CI_lower = round(ci_lower, 3),
    CI_upper = round(ci_upper, 3),
    P_value = as.character(p_fmt)
  )
}

crr_results <- bind_rows(
  extract_crr(crr_fit1, "模型1", length(res1$idx)),
  extract_crr(crr_fit2, "模型2", length(res2$idx)),
  extract_crr(crr_fit3, "模型3", length(res3$idx))
)

cat("\n========== 竞争风险回归结果（自杀事件，其他死亡为竞争事件）==========\n")
print(crr_results)

### 限制性立方样条分析 ----
# 加载所需包
library(rms)
library(ggplot2)
library(survival)

# 定义RCS分析函数
run_rcs_analysis <- function(data, event_col, outcome_name, var_labels = NULL) {
  # data         : 包含时间、事件及所有协变量的数据框
  # event_col    : 事件指示变量的列名（字符型）
  # outcome_name : 结局名称，用于输出标签（如 "Attempt" 或 "Suicide"）
  # var_labels   : 命名向量，用于自定义x轴标签，格式如 c("变量名" = "显示标签")
  
  # 定义需要调整的协变量集合（与模型3保持一致）
  all_covars <- c("Age", "Sex", "Ethnicity", "TDI", "Qualifications",
                  "Employment", "Income", "BMI", "Drinking_Status",
                  "Smoking_Status", "Sleep_Duration")
  
  # 筛选实际存在于数据中且非单值的协变量
  valid_covars <- all_covars[sapply(all_covars, function(v) {
    v %in% names(data) && length(unique(data[[v]])) > 1
  })]
  if (length(valid_covars) < length(all_covars)) {
    cat("移除了常数变量:", setdiff(all_covars, valid_covars), "\n")
  }
  
  # 设置数据分布环境（rms包绘图必需）
  dd <<- datadist(data)
  options(datadist = "dd")
  
  # 待分析的连续变量列表
  rcs_vars <- c("Support_index", "Childhood_Trauma_Index", "Adversity_Events_Index")
  rcs_results <- list()
  
  for (var in rcs_vars) {
    cat("\n========== 结局：", outcome_name, " | 变量：", var, "==========\n")
    
    # 检查唯一值数量，确定节点数
    unique_vals <- unique(data[[var]][!is.na(data[[var]])])
    n_unique <- length(unique_vals)
    n_knots <- min(5, max(3, n_unique - 1))
    cat("变量", var, "唯一值个数:", n_unique, "，使用节点数:", n_knots, "\n")
    
    # 构建公式
    other_covars <- valid_covars[!valid_covars %in% var]
    formula_str <- paste("Surv(time,", event_col, ") ~ state_group + rcs(", 
                         var, ",", n_knots, ") +",
                         paste(other_covars, collapse = " + "))
    formula_rcs <- as.formula(formula_str)
    
    # 拟合Cox模型
    fit_rcs <- cph(formula_rcs, data = data, x = TRUE, y = TRUE, surv = TRUE)
    
    # 方差分析（非线性检验）
    anova_res <- anova(fit_rcs)
    print(anova_res)
    
    # 提取P值
    nonlinear_row <- grep(paste0("^", var, "'"), rownames(anova_res), value = TRUE)
    nonlinear_p <- if (length(nonlinear_row) == 1) anova_res[nonlinear_row, "P"] else NA
    overall_p   <- anova_res[var, "P"]
    
    rcs_results[[var]] <- data.frame(
      Outcome = outcome_name,
      Variable = var,
      Nonlinear_P = round(nonlinear_p, 4),
      Overall_P   = round(overall_p, 4)
    )
    
    # 生成预测数据
    pred <- Predict(fit_rcs, name = var, ref.zero = TRUE, fun = exp)
    plot_data <- as.data.frame(pred)
    colnames(plot_data)[colnames(plot_data) == var] <- "xvar"
    
    # ----- 关键修正：根据 var_labels 设置 x 轴标签 -----
    if (!is.null(var_labels) && var %in% names(var_labels)) {
      x_label <- var_labels[var]
    } else {
      x_label <- var
    }
    
    p <- ggplot(plot_data, aes(x = xvar, y = yhat)) +
      geom_line(color = "steelblue", size = 1) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      labs(title = paste("Suicide behavior"),
           x = var_labels,
           y = "Hazard Ratio (95% CI)") +
      scale_x_continuous(breaks = seq(0, 10, by = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),     
        axis.text    = element_text(size = 10))
    print(p)
  }
  
  return(do.call(rbind, rcs_results))
}

# 定义变量显示标签（可根据需要修改）
var_labels <- c(
  Support_index = "Social loneliness Index",
  Childhood_Trauma_Index = "Childhood Trauma Index",
  Adversity_Events_Index = "Adversity Events Index"
)
# 使用之前已构建好的数据框：
# cox_data        : 用于 attempt 结局（事件列名为 "event"）
# cox_data_suicide: 用于 suicide 结局（事件列名为 "event"）

# 1. 分析自杀未遂（attempt）
results_attempt <- run_rcs_analysis(
  data = cox_data, 
  event_col = "event", 
  outcome_name = "Attempt",
  var_labels = var_labels          # 传入标签映射
)

# 2. 分析自杀死亡（suicide）
results_suicide <- run_rcs_analysis(
  data = cox_data_suicide, 
  event_col = "event", 
  outcome_name = "Suicide",
  var_labels = var_labels
)
# 合并两个结局的分析结果
rcs_pvalues_combined <- rbind(results_attempt, results_suicide)


### OCD × RCS spline basis 联合交互检验及结果图 ----

library(survival)
library(rms)
library(dplyr)
library(tidyr)
library(ggplot2)

# 创建结果输出目录
interaction_output_dir <- "joint_rcs_interaction_results"
dir.create(
  interaction_output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# P值格式化函数
format_p_value <- function(p) {
  if (is.na(p)) {
    return("NA")
  }
  
  if (p < 0.001) {
    return("<0.001")
  }
  
  sprintf("%.3f", p)
}


# 安全生成文件名
safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  gsub("_+$", "", x)
}


# 为绘图生成协变量参考值
# 连续变量：中位数
# 分类变量：样本中最常见水平
make_reference_row <- function(data, covars) {
  
  # 避免data.table把covars当成实际列名
  data_df <- as.data.frame(data)
  
  # 检查变量是否存在
  missing_covars <- setdiff(covars, names(data_df))
  
  if (length(missing_covars) > 0) {
    stop(
      "构建参考值时缺少变量：",
      paste(missing_covars, collapse = ", ")
    )
  }
  
  ref <- data_df[1, covars, drop = FALSE]
  
  for (v in covars) {
    
    x <- data_df[[v]]
    
    if (is.factor(x)) {
      
      non_missing_x <- x[!is.na(x)]
      
      if (length(non_missing_x) == 0) {
        stop("变量 ", v, " 全部为缺失值。")
      }
      
      most_common_level <- names(
        which.max(table(non_missing_x))
      )
      
      ref[[v]] <- factor(
        most_common_level,
        levels = levels(x),
        ordered = is.ordered(x)
      )
      
    } else if (is.character(x)) {
      
      non_missing_x <- x[!is.na(x)]
      
      if (length(non_missing_x) == 0) {
        stop("变量 ", v, " 全部为缺失值。")
      }
      
      most_common_level <- names(
        which.max(table(non_missing_x))
      )
      
      ref[[v]] <- most_common_level
      
    } else if (is.numeric(x) || is.integer(x)) {
      
      ref[[v]] <- median(
        x,
        na.rm = TRUE
      )
      
    } else {
      
      stop(
        "变量 ",
        v,
        " 的类型暂不支持：",
        class(x)[1]
      )
    }
  }
  
  # 明确返回普通data.frame
  as.data.frame(ref)
}


# 根据拟合模型和新数据构建设计矩阵
build_prediction_matrix <- function(fit, newdata) {
  
  model_terms <- delete.response(terms(fit))
  
  mm <- model.matrix(
    model_terms,
    data = newdata,
    contrasts.arg = fit$contrasts,
    xlev = fit$xlevels
  )
  
  coefficient_names <- names(coef(fit))
  
  missing_columns <- setdiff(
    coefficient_names,
    colnames(mm)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "预测设计矩阵中缺少以下模型变量：",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  mm[, coefficient_names, drop = FALSE]
}


# 完整联合交互检验和作图函数
run_joint_rcs_interaction <- function(
    data,
    event_col,
    outcome_name,
    rcs_vars = c(
      "Support_index",
      "Childhood_Trauma_Index",
      "Adversity_Events_Index"
    ),
    var_labels = c(
      Support_index =
        "Social Isolation/Loneliness Index",
      Childhood_Trauma_Index =
        "Childhood Trauma Index",
      Adversity_Events_Index =
        "Life Adversity Index"
    ),
    adjust_covars = c(
      "Age",
      "Sex",
      "Ethnicity",
      "TDI",
      "Qualifications",
      "Employment",
      "Income",
      "BMI",
      "Drinking_Status",
      "Smoking_Status",
      "Sleep_Duration"
    ),
    maximum_knots = 5,
    use_log_y = TRUE,
    output_dir = interaction_output_dir
) {
  
  model_storage <- list()
  plot_storage  <- list()
  
  for (var in rcs_vars) {
    
    cat(
      "\n====================================================\n",
      "结局：", outcome_name, "\n",
      "效应修饰变量：", var, "\n",
      "====================================================\n",
      sep = ""
    )
    
    required_variables <- c(
      "time",
      event_col,
      "state_group",
      var,
      adjust_covars
    )
    
    missing_variables <- setdiff(
      required_variables,
      names(data)
    )
    
    if (length(missing_variables) > 0) {
      stop(
        "数据中缺少变量：",
        paste(missing_variables, collapse = ", ")
      )
    }
    
    # 每个交互模型使用完全相同的完整病例样本
    analysis_data <- data %>%
      select(all_of(required_variables)) %>%
      filter(time > 0) %>%
      drop_na() %>%
      as.data.frame()
    
    # 确保参照组为Non-OCD
    analysis_data$state_group <- factor(
      as.character(analysis_data$state_group),
      levels = c("Non-OCD", "OCD")
    )
    
    analysis_data <- droplevels(analysis_data)
    
    if (nlevels(analysis_data$state_group) != 2) {
      stop(
        outcome_name,
        " × ",
        var,
        "模型中没有同时包含Non-OCD和OCD两组。"
      )
    }
    
    if (sum(analysis_data[[event_col]] == 1) == 0) {
      stop(
        outcome_name,
        " × ",
        var,
        "模型中没有结局事件。"
      )
    }
    
    # 删除在当前完整病例样本中只有一个取值的调整变量
    valid_covars <- adjust_covars[
      sapply(adjust_covars, function(v) {
        length(unique(analysis_data[[v]])) > 1
      })
    ]
    
    removed_covars <- setdiff(
      adjust_covars,
      valid_covars
    )
    
    if (length(removed_covars) > 0) {
      message(
        "以下协变量在当前分析样本中为单一取值，已从模型中移除：",
        paste(removed_covars, collapse = ", ")
      )
    }
    
    # 确定RCS节点数
    unique_values <- sort(
      unique(analysis_data[[var]])
    )
    
    number_unique <- length(unique_values)
    
    if (number_unique < 3) {
      stop(
        var,
        "的有效唯一值少于3个，不能进行限制性立方样条分析。"
      )
    }
    
    number_knots <- min(
      maximum_knots,
      max(3, number_unique - 2)
    )
    
    cat(
      "分析样本量：", nrow(analysis_data), "\n",
      "事件数：", sum(analysis_data[[event_col]] == 1), "\n",
      "唯一值数量：", number_unique, "\n",
      "计划节点数：", number_knots, "\n",
      sep = ""
    )
    
    # --------------------------------------------------
    # 生成RCS基函数
    # inclx = TRUE表示同时保留线性基函数
    # 因此后续联合检验同时包含线性和非线性交互部分
    # --------------------------------------------------
    
    spline_basis <- tryCatch(
      Hmisc::rcspline.eval(
        analysis_data[[var]],
        nk = number_knots,
        inclx = TRUE
      ),
      error = function(e) NULL
    )
    
    # 针对离散评分导致默认分位点重复的备用方案
    if (is.null(spline_basis)) {
      
      selected_positions <- unique(
        round(
          seq(
            1,
            number_unique,
            length.out = min(number_knots, number_unique)
          )
        )
      )
      
      fallback_knots <- unique_values[selected_positions]
      
      if (length(fallback_knots) < 3) {
        stop(var, "无法获得至少3个有效样条节点。")
      }
      
      spline_basis <- Hmisc::rcspline.eval(
        analysis_data[[var]],
        knots = fallback_knots,
        inclx = TRUE
      )
    }
    
    knot_locations <- attr(
      spline_basis,
      "knots"
    )
    
    basis_names <- paste0(
      "rcs_basis_",
      seq_len(ncol(spline_basis))
    )
    
    colnames(spline_basis) <- basis_names
    
    analysis_data <- bind_cols(
      analysis_data,
      as.data.frame(spline_basis)
    )
    
    cat(
      "实际节点位置：",
      paste(
        round(knot_locations, 3),
        collapse = ", "
      ),
      "\n",
      "样条基函数数量：",
      length(basis_names),
      "\n",
      sep = ""
    )
    
    # --------------------------------------------------
    # 约简模型：
    # OCD主效应 + 指数RCS主效应 + Model 3协变量
    # --------------------------------------------------
    
    reduced_terms <- c(
      "state_group",
      basis_names,
      valid_covars
    )
    
    reduced_formula <- as.formula(
      paste(
        "Surv(time,",
        event_col,
        ") ~",
        paste(reduced_terms, collapse = " + ")
      )
    )
    
    # --------------------------------------------------
    # 完整模型：
    # 在约简模型基础上增加
    # state_group × 每一个RCS基函数
    # --------------------------------------------------
    
    interaction_terms <- paste0(
      "state_group:",
      basis_names
    )
    
    full_terms <- c(
      reduced_terms,
      interaction_terms
    )
    
    full_formula <- as.formula(
      paste(
        "Surv(time,",
        event_col,
        ") ~",
        paste(full_terms, collapse = " + ")
      )
    )
    
    # 拟合两个嵌套Cox模型
    reduced_fit <- coxph(
      reduced_formula,
      data = analysis_data,
      ties = "efron",
      x = TRUE,
      model = TRUE
    )
    
    full_fit <- coxph(
      full_formula,
      data = analysis_data,
      ties = "efron",
      x = TRUE,
      model = TRUE
    )
    
    if (any(is.na(coef(full_fit)))) {
      stop(
        outcome_name,
        " × ",
        var,
        "完整交互模型存在不可估计系数。可能需要减少节点数。"
      )
    }
    
    # --------------------------------------------------
    # OCD × 全部spline basis的联合似然比检验
    # H0：所有OCD × spline basis交互系数同时为0
    # --------------------------------------------------
    
    reduced_loglik <- logLik(reduced_fit)
    full_loglik    <- logLik(full_fit)
    
    likelihood_ratio_chisq <- 2 * (
      as.numeric(full_loglik) -
        as.numeric(reduced_loglik)
    )
    
    interaction_df <- attr(
      full_loglik,
      "df"
    ) - attr(
      reduced_loglik,
      "df"
    )
    
    joint_interaction_p <- pchisq(
      likelihood_ratio_chisq,
      df = interaction_df,
      lower.tail = FALSE
    )
    
    cat(
      "联合交互似然比检验：\n",
      "Chi-square = ",
      round(likelihood_ratio_chisq, 3),
      "\n",
      "df = ",
      interaction_df,
      "\n",
      "P = ",
      format_p_value(joint_interaction_p),
      "\n",
      sep = ""
    )
    
    # --------------------------------------------------
    # 构建绘图数据：
    # 在每个指数水平，计算OCD相对于Non-OCD的HR
    # --------------------------------------------------
    
    x_grid <- sort(
      unique(
        analysis_data[[var]][
          !is.na(analysis_data[[var]])
        ]
      )
    )
    
    grid_basis <- Hmisc::rcspline.eval(
      x_grid,
      knots = knot_locations,
      inclx = TRUE
    )
    
    colnames(grid_basis) <- basis_names
    grid_basis <- as.data.frame(grid_basis)
    
    # 其他协变量设置为相同参考值
    # 因为比较的是相同协变量条件下OCD与Non-OCD，
    # 所以这些主效应会在设计矩阵差值中抵消
    reference_row <- make_reference_row(
      analysis_data,
      c("state_group", valid_covars)
    )
    
    non_ocd_data <- reference_row[
      rep(1, length(x_grid)),
      ,
      drop = FALSE
    ]
    
    ocd_data <- non_ocd_data
    
    non_ocd_data$state_group <- factor(
      rep("Non-OCD", length(x_grid)),
      levels = c("Non-OCD", "OCD")
    )
    
    ocd_data$state_group <- factor(
      rep("OCD", length(x_grid)),
      levels = c("Non-OCD", "OCD")
    )
    
    # 加入对应评分水平下的RCS基函数
    for (basis_var in basis_names) {
      non_ocd_data[[basis_var]] <- grid_basis[[basis_var]]
      ocd_data[[basis_var]]     <- grid_basis[[basis_var]]
    }
    
    # 完整模型设计矩阵
    x_non_ocd <- build_prediction_matrix(
      full_fit,
      non_ocd_data
    )
    
    x_ocd <- build_prediction_matrix(
      full_fit,
      ocd_data
    )
    
    # 每个评分水平下，OCD与Non-OCD设计矩阵的差值
    contrast_matrix <- x_ocd - x_non_ocd
    
    beta_hat <- coef(full_fit)
    variance_matrix <- vcov(full_fit)
    
    log_hr <- as.vector(
      contrast_matrix %*% beta_hat
    )
    
    log_hr_se <- sqrt(
      rowSums(
        (contrast_matrix %*% variance_matrix) *
          contrast_matrix
      )
    )
    
    plot_data <- data.frame(
      Score = x_grid,
      HR = exp(log_hr),
      CI_lower = exp(log_hr - 1.96 * log_hr_se),
      CI_upper = exp(log_hr + 1.96 * log_hr_se)
    )
    
    model_storage[[var]] <- list(
      variable = var,
      label = unname(var_labels[var]),
      knots = knot_locations,
      number_knots = length(knot_locations),
      number_basis = length(basis_names),
      n = nrow(analysis_data),
      events = sum(analysis_data[[event_col]] == 1),
      reduced_fit = reduced_fit,
      full_fit = full_fit,
      plot_data = plot_data,
      chi_square = likelihood_ratio_chisq,
      df = interaction_df,
      p_joint = joint_interaction_p,
      observed_values = unique_values
    )
  }
  
  # ----------------------------------------------------
  # 对同一结局下的三个联合交互检验进行BH校正
  # ----------------------------------------------------
  
  result_table <- bind_rows(
    lapply(model_storage, function(x) {
      data.frame(
        Outcome = outcome_name,
        Variable = x$variable,
        Variable_label = x$label,
        N = x$n,
        Events = x$events,
        Number_of_knots = x$number_knots,
        Number_of_basis_functions = x$number_basis,
        Interaction_df = x$df,
        LR_Chi_square = x$chi_square,
        P_joint_interaction = x$p_joint,
        Knot_locations = paste(
          round(x$knots, 3),
          collapse = ", "
        ),
        stringsAsFactors = FALSE
      )
    })
  )
  
  result_table <- result_table %>%
    mutate(
      P_BH_within_outcome = p.adjust(
        P_joint_interaction,
        method = "BH"
      ),
      P_joint_display = vapply(
        P_joint_interaction,
        format_p_value,
        character(1)
      ),
      P_BH_display = vapply(
        P_BH_within_outcome,
        format_p_value,
        character(1)
      )
    )
  
  # ----------------------------------------------------
  # 根据BH校正后的结果生成图形
  # ----------------------------------------------------
  
  for (i in seq_len(nrow(result_table))) {
    
    var <- result_table$Variable[i]
    stored_result <- model_storage[[var]]
    
    current_plot_data <- stored_result$plot_data
    
    variable_label <- result_table$Variable_label[i]
    raw_p <- result_table$P_joint_interaction[i]
    bh_p  <- result_table$P_BH_within_outcome[i]
    
    p <- ggplot(
      current_plot_data,
      aes(
        x = Score,
        y = HR
      )
    ) +
      geom_ribbon(
        aes(
          ymin = CI_lower,
          ymax = CI_upper
        ),
        alpha = 0.20
      ) +
      geom_line(
        linewidth = 1
      ) +
      geom_hline(
        yintercept = 1,
        linetype = "dashed",
        linewidth = 0.6
      ) +
      labs(
        title = paste0(
          outcome_name,
          ""
        ),
        subtitle = paste0(
          "P ",
          ifelse(
            raw_p < 0.001,
            "<0.001",
            paste0("= ", sprintf("%.3f", raw_p))
          ),
          "; BH-adjusted P ",
          ifelse(
            bh_p < 0.001,
            "<0.001",
            paste0("= ", sprintf("%.3f", bh_p))
          ),
          "; df = ",
          result_table$Interaction_df[i]
        ),
        x = variable_label,
        y = "Hazard ratio for Ever-recorded OCD vs Never-recorded OCD (95% CI)"
      ) +
      scale_x_continuous(
        breaks = stored_result$observed_values
      )
    
    # HR图通常使用对数纵轴更合适
    if (use_log_y) {
      p <- p +
        scale_y_log10(
          breaks = scales::log_breaks(n = 6),
          labels = scales::label_number(
            accuracy = 0.01
          )
        )
    }
    
    plot_storage[[var]] <- p
    
    output_stem <- paste0(
      safe_filename(outcome_name),
      "_",
      safe_filename(var),
      "_OCD_joint_RCS_interaction"
    )
    
    ggsave(
      filename = file.path(
        output_dir,
        paste0(output_stem, ".pdf")
      ),
      plot = p,
      width = 7.2,
      height = 5.4,
      device = cairo_pdf
    )
    
    ggsave(
      filename = file.path(
        output_dir,
        paste0(output_stem, ".png")
      ),
      plot = p,
      width = 7.2,
      height = 5.4,
      dpi = 600
    )
    
    print(p)
  }
  
  # 保存当前结局的联合检验结果
  write.csv(
    result_table,
    file = file.path(
      output_dir,
      paste0(
        safe_filename(outcome_name),
        "_joint_RCS_interaction_tests.csv"
      )
    ),
    row.names = FALSE
  )
  
  # 可选：如已安装patchwork，自动合并3张图
  combined_plot <- NULL
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    
    combined_plot <- patchwork::wrap_plots(
      plotlist = unname(plot_storage),
      ncol = 3
    ) +
      patchwork::plot_annotation(
        title = paste0(
          outcome_name,
          ": joint OCD × spline interaction analyses"
        )
      )
    
    ggsave(
      filename = file.path(
        output_dir,
        paste0(
          safe_filename(outcome_name),
          "_combined_joint_RCS_interactions.pdf"
        )
      ),
      plot = combined_plot,
      width = 18,
      height = 6,
      device = cairo_pdf
    )
    
    ggsave(
      filename = file.path(
        output_dir,
        paste0(
          safe_filename(outcome_name),
          "_combined_joint_RCS_interactions.png"
        )
      ),
      plot = combined_plot,
      width = 18,
      height = 6,
      dpi = 600
    )
    
    print(combined_plot)
  }
  
  return(
    list(
      results = result_table,
      plots = plot_storage,
      combined_plot = combined_plot,
      models = model_storage
    )
  )
}


### 运行：自杀未遂结局 ----

joint_interaction_attempt <- run_joint_rcs_interaction(
  data = cox_data,
  event_col = "event",
  outcome_name = "Suicide attempt",
  var_labels = var_labels
)


### 运行：自杀行为结局 ----
# 这里的cox_data_suicide对应你原代码中的suicide变量。
# 如果该变量实际代表复合“自杀行为”，建议使用当前标签。

joint_interaction_suicidal_behavior <- run_joint_rcs_interaction(
  data = cox_data_suicide,
  event_col = "event",
  outcome_name = "Suicidal behavior",
  var_labels = var_labels
)


### 合并两个结局的联合交互检验结果 ----

joint_interaction_results <- bind_rows(
  joint_interaction_attempt$results,
  joint_interaction_suicidal_behavior$results
) %>%
  mutate(
    # 同时提供对全部6项检验进行BH校正的结果
    P_BH_all_six_tests = p.adjust(
      P_joint_interaction,
      method = "BH"
    )
  )

cat(
  "\n========== OCD × RCS spline basis联合交互检验结果 ==========\n"
)

print(
  joint_interaction_results %>%
    select(
      Outcome,
      Variable_label,
      N,
      Events,
      Number_of_knots,
      Interaction_df,
      LR_Chi_square,
      P_joint_interaction,
      P_BH_within_outcome,
      P_BH_all_six_tests
    )
)

write.csv(
  joint_interaction_results,
  file = file.path(
    interaction_output_dir,
    "all_joint_RCS_interaction_tests.csv"
  ),
  row.names = FALSE
)


#### 四轮 ----
library(data.table)
library(dplyr)
library(survival)
library(broom)
library(ggplot2)

setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
ccc142748 <- readRDS("ccc142748.rds")

# 变量类型转换（一次性处理所有分类和连续变量）
ccc142748 <- ccc142748 %>%
  mutate(
    # 创建新的state分组：0和3为Non-OCD，1和2为OCD
    state_group = case_when(
      state %in% c(0, 3) ~ "Non-OCD",
      state %in% c(1, 2) ~ "OCD",
      TRUE ~ NA_character_
    ),
    state_group = factor(state_group, levels = c("Non-OCD", "OCD")),
    # 其他分类变量
    Sex            = factor(Sex),
    Ethnicity      = factor(Ethnicity),
    Qualifications = factor(Qualifications),
    Employment     = factor(Employment),
    Income         = factor(Income),
    Drinking_Status = factor(Drinking_Status),
    Smoking_Status  = factor(Smoking_Status),
    # 连续变量
    across(c(Support_index, Childhood_Trauma_Index, Adversity_Events_Index,
             Age, BMI, TDI, Sleep_Duration, time), as.numeric)
  )

#### Cox回归分析(自杀行为) ----
# 筛选有效时间，定义事件：suicide == 1 为事件，0 和 2 均为删失
cox_data_suicide <- ccc142748 %>%
  filter(!is.na(time), time > 0) %>%
  mutate(event = as.numeric(suicide == 1))

surv_obj_suicide <- Surv(time = cox_data_suicide$time, event = cox_data_suicide$event)

# 协变量集合
base_covars   <- c("Age", "Sex", "Ethnicity")
socio_covars  <- c("TDI", "Qualifications", "Employment", "Income")
health_covars <- c("BMI", "Drinking_Status", "Smoking_Status", "Sleep_Duration")

# 构建模型公式（使用 state_group）
models_suicide <- list(
  model1 = as.formula(paste("surv_obj_suicide ~ state_group +", paste(base_covars, collapse = " + "))),
  model2 = as.formula(paste("surv_obj_suicide ~ state_group +", 
                            paste(c(base_covars, socio_covars), collapse = " + "))),
  model3 = as.formula(paste("surv_obj_suicide ~ state_group +", 
                            paste(c(base_covars, socio_covars, health_covars), collapse = " + ")))
)

# 提取 HR 的通用函数
extract_hr_suicide <- function(model_fit, model_name) {
  tidy_output <- tidy(model_fit, conf.int = TRUE, exponentiate = TRUE)
  state_row <- tidy_output %>% filter(grepl("^state_group", term))
  if (nrow(state_row) == 0) stop("模型中未找到 state_group 变量")
  p_val <- state_row$p.value
  p_fmt <- ifelse(p_val < 0.001, "<0.001", round(p_val, 3))
  data.frame(
    Model = model_name,
    N = model_fit$n,
    HR = round(state_row$estimate, 3),
    CI_lower = round(state_row$conf.low, 3),
    CI_upper = round(state_row$conf.high, 3),
    P_value = as.character(p_fmt)
  )
}

# 依次拟合三个模型
results_suicide_list <- list()
for (i in seq_along(models_suicide)) {
  fit <- coxph(models_suicide[[i]], data = cox_data_suicide)
  results_suicide_list[[i]] <- extract_hr_suicide(fit, paste0("模型", i))
}
results_suicide_table <- bind_rows(results_suicide_list)

cat("\n========== Cox 回归结果（自杀事件，删失包括无事件和其他死亡）==========\n")
print(results_suicide_table)


#### ====================== 亚组分析（含事件数过滤） ======================
# 定义用于亚组分析的协变量全集（与模型3一致）
adjust_covars <- c(base_covars, socio_covars, health_covars)

# 定义连续变量和分类变量（从 adjust_covars 中区分）
cont_vars <- c("Age", "BMI", "TDI", "Sleep_Duration")
cat_vars  <- setdiff(adjust_covars, cont_vars)

# 构建用于亚组分析的完整数据（移除 adjust_covars 和 state_group、time、event 中的缺失值）
complete_data <- cox_data_suicide %>%
  select(time, event, state_group, all_of(adjust_covars)) %>%
  na.omit()  # 删除含缺失值的行，确保亚组模型稳定
# 设置最小事件数阈值
min_events_threshold <- 3

# 存储亚组分析结果
subgroup_results <- list()

# 辅助函数：对单个亚组变量进行分析（带事件数检查）
analyze_subgroup <- function(data, var_name, var_type = "categorical", 
                             adjust_vars, min_events = 5) {
  results <- data.frame()
  
  if (var_type == "continuous") {
    median_val <- median(data[[var_name]], na.rm = TRUE)
    group_var <- ifelse(data[[var_name]] <= median_val, "≤median", ">median")
    group_name <- var_name
    unique_levels <- c("≤median", ">median")
  } else {
    group_var <- as.character(data[[var_name]])
    group_name <- var_name
    unique_levels <- sort(unique(group_var))
  }
  
  # 构建调整协变量公式（排除当前亚组变量本身）
  other_covars <- setdiff(adjust_vars, var_name)
  rhs <- paste(c("state_group", other_covars), collapse = " + ")
  formula_str <- paste("Surv(time, event) ~", rhs)
  
  for (lev in unique_levels) {
    sub_data <- data[group_var == lev, ]
    n_total <- nrow(sub_data)
    n_events <- sum(sub_data$event)
    
    # 检查事件数是否足够
    if (n_events < min_events) {
      cat("  跳过水平", lev, "：事件数 =", n_events, "<", min_events, "\n")
      next
    }
    
    # 拟合Cox模型
    fit <- tryCatch(
      coxph(as.formula(formula_str), data = sub_data),
      error = function(e) {
        cat("  水平", lev, "模型拟合失败：", e$message, "\n")
        return(NULL)
      }
    )
    if (is.null(fit)) next
    
    tidy_out <- tidy(fit, conf.int = TRUE, exponentiate = TRUE)
    state_row <- tidy_out %>% filter(grepl("^state_group", term))
    if (nrow(state_row) == 0) next
    
    p_val <- state_row$p.value
    p_fmt <- ifelse(p_val < 0.001, "<0.001", sprintf("%.3f", p_val))
    
    results <- rbind(results, data.frame(
      Variable = var_name,
      Level = lev,
      N = n_total,
      Events = n_events,
      HR = round(state_row$estimate, 3),
      CI_lower = round(state_row$conf.low, 3),
      CI_upper = round(state_row$conf.high, 3),
      P_value = p_fmt,
      stringsAsFactors = FALSE
    ))
  }
  return(results)
}

# 执行亚组分析（连续变量）
for (v in cont_vars) {
  cat("\n正在分析连续变量：", v, "\n")
  res <- analyze_subgroup(data = complete_data, 
                          var_name = v, 
                          var_type = "continuous",
                          adjust_vars = adjust_covars,
                          min_events = min_events_threshold)
  if (nrow(res) > 0) subgroup_results[[v]] <- res
}

# 执行亚组分析（分类变量）
for (v in cat_vars) {
  cat("\n正在分析分类变量：", v, "\n")
  res <- analyze_subgroup(data = complete_data, 
                          var_name = v, 
                          var_type = "categorical",
                          adjust_vars = adjust_covars,
                          min_events = min_events_threshold)
  if (nrow(res) > 0) subgroup_results[[v]] <- res
}

subgroup_table <- bind_rows(subgroup_results)

cat("\n================== 亚组分析结果（已过滤事件数 <", min_events_threshold, "的亚组） ==================\n")
print(subgroup_table)


#### ====================== 交互作用检验（含事件数/数据有效性检查） ======================
interaction_results <- data.frame(
  Variable = character(),
  P_for_interaction = character(),
  N = integer(),
  Events = integer(),
  stringsAsFactors = FALSE
)

for (v in c(cont_vars, cat_vars)) {
  cat("\n========== 检验交互作用：", v, "==========\n")
  
  interact_data <- complete_data
  
  # 检查 state_group 与当前变量的交叉事件分布
  if (v %in% cont_vars) {
    # 连续变量先按中位数分组用于检查
    med <- median(interact_data[[v]])
    tmp_group <- ifelse(interact_data[[v]] <= med, "Low", "High")
  } else {
    tmp_group <- as.character(interact_data[[v]])
  }
  
  # 创建交叉表
  cross_tab <- table(interact_data$state_group, tmp_group, interact_data$event)
  # 检查是否有事件数为0的组合（可能导致交互项无法估计）
  event_tab <- table(interact_data$state_group, tmp_group, interact_data$event)[,, "1"]
  if (any(event_tab == 0, na.rm = TRUE)) {
    cat("  跳过：存在 state_group ×", v, "水平组合中事件数为0，模型可能无法收敛。\n")
    next
  }
  
  # 构建公式
  other_covars <- setdiff(adjust_covars, v)
  rhs_no  <- paste(c("state_group", v, other_covars), collapse = " + ")
  rhs_int <- paste(c(paste0("state_group * ", v), other_covars), collapse = " + ")
  form_no  <- as.formula(paste("Surv(time, event) ~", rhs_no))
  form_int <- as.formula(paste("Surv(time, event) ~", rhs_int))
  
  # 拟合模型
  fit_no  <- tryCatch(coxph(form_no, data = interact_data), 
                      error = function(e) { cat("无交互模型失败：", e$message, "\n"); NULL })
  fit_int <- tryCatch(coxph(form_int, data = interact_data), 
                      error = function(e) { cat("有交互模型失败：", e$message, "\n"); NULL })
  
  if (is.null(fit_no) || is.null(fit_int)) next
  
  # 似然比检验
  lr_test <- tryCatch(anova(fit_no, fit_int, test = "LRT"),
                      error = function(e) { cat("LRT失败：", e$message, "\n"); NULL })
  if (is.null(lr_test)) next
  
  p_val <- NA
  if (nrow(lr_test) >= 2) {
    p_col <- intersect(c("P(>|Chi|)", "Pr(>|Chi|)", "p"), names(lr_test))
    if (length(p_col) > 0) p_val <- lr_test[2, p_col[1]]
  }
  
  if (is.null(p_val) || length(p_val) == 0 || is.na(p_val)) {
    cat("  未提取到有效交互p值\n")
    next
  }
  
  p_fmt <- ifelse(p_val < 0.001, "<0.001", sprintf("%.3f", p_val))
  cat("  交互作用 p =", p_fmt, "\n")
  
  interaction_results <- rbind(interaction_results, data.frame(
    Variable = v,
    P_for_interaction = p_fmt,
    N = fit_int$n,
    Events = sum(interact_data$event),
    stringsAsFactors = FALSE
  ))
}

cat("\n================== 交互作用检验结果 ==================\n")
print(interaction_results)

# 合并交互 P 值到亚组表
if (nrow(interaction_results) > 0) {
  subgroup_table <- subgroup_table %>%
    left_join(interaction_results %>% select(Variable, P_for_interaction), by = "Variable")
} else {
  subgroup_table <- subgroup_table %>%
    mutate(P_for_interaction = NA_character_)
}

cat("\n================== 亚组分析结果（含交互P值） ==================\n")
print(subgroup_table)

#### ====================== 变量名称映射（重命名） ======================
# 请根据您的实际变量含义修改右侧标签
var_labels <- data.frame(
  original = c("Age", "Sex", "Ethnicity", "TDI", "Qualifications", 
               "Employment", "Income", "BMI", "Drinking_Status", 
               "Smoking_Status", "Sleep_Duration"),
  display  = c("Age", "Sex", "Ethnicity", "TDI", "Qualifications", 
               "Employment", "Income", "BMI", "Alcohol intake status", 
               "Smoking status", "Sleep duration"),
  stringsAsFactors = FALSE
)

# 将 subgroup_table 中的变量名替换为可读名称
subgroup_table <- subgroup_table %>%
  left_join(var_labels, by = c("Variable" = "original")) %>%
  mutate(Variable_display = ifelse(is.na(display), Variable, display)) %>%
  select(-display)

subgroup_table <- subgroup_table %>%
  mutate(Level = case_when(
    Variable == "Sex" & Level == "0" ~ "Female",
    Variable == "Sex" & Level == "1" ~ "Male",
    Variable == "Ethnicity" & Level == "0" ~ "Others",
    Variable == "Ethnicity" & Level == "1" ~ "White",
    Variable == "Drinking_Status" & Level == "0" ~ "Previous/Never",
    Variable == "Drinking_Status" & Level == "1" ~ "Current",
    Variable == "Smoking_Status" & Level == "0" ~ "Previous/Never",
    Variable == "Smoking_Status" & Level == "1" ~ "Current",
    Variable == "Income" & Level == "1" ~ "<18,000",
    Variable == "Income" & Level == "2" ~ "18,000-51,999",
    Variable == "Income" & Level == "3" ~ "≥52,000",
    Variable == "Employment" & Level == "0" ~ "Unemployed",
    Variable == "Employment" & Level == "1" ~ "Others",
    Variable == "Employment" & Level == "2" ~ "Employed",
    Variable == "Qualifications" & Level == "0" ~ "None",
    Variable == "Qualifications" & Level == "1" ~ "Others",
    Variable == "Qualifications" & Level == "2" ~ "Degree",
    Variable == "Sleep_Duration" & Level == "≤median" ~ "≤7",
    Variable == "Sleep_Duration" & Level == ">median" ~ ">7",
    Variable == "Age" & Level == "≤median" ~ "≤56",
    Variable == "Age" & Level == ">median" ~ ">56",
    TRUE ~ Level
  ))


#### ====================== 绘制森林图 ======================
library(forestplot)

# 1. 定义显示顺序（按需修改）
var_order <- c("Age", "Sex", "Ethnicity", "TDI", "BMI", "Sleep duration", "Qualifications",
               "Employment", "Income", "Alcohol intake status",
               "Smoking status")

# 2. 转换因子并排序
subgroup_table <- subgroup_table %>%
  mutate(Variable_display = factor(Variable_display, levels = var_order)) %>%
  arrange(Variable_display, Level)   # 如需调整水平顺序，可先处理 Level 因子

plot_data <- data.frame()
for (var in unique(subgroup_table$Variable_display)) {
  orig_var <- subgroup_table$Variable[subgroup_table$Variable_display == var][1]
  sub <- subgroup_table[subgroup_table$Variable_display == var, ]
  p_int <- sub$P_for_interaction[1]
  
  # 标题行：显示变量名和交互P值
  title_row <- data.frame(
    label      = var,
    N          = NA,
    HR_CI      = "",
    P_val      = "",
    P_int      = ifelse(is.na(p_int), "", p_int),
    mean       = NA,
    lower      = NA,
    upper      = NA,
    is.summary = TRUE,
    stringsAsFactors = FALSE
  )
  
  # 数据行：显示各水平的结果
  data_rows <- data.frame(
    label      = paste0("  ", sub$Level),
    N          = sub$N,
    HR_CI      = sprintf("%.2f (%.2f-%.2f)", sub$HR, sub$CI_lower, sub$CI_upper),
    P_val      = sub$P_value,
    P_int      = "",
    mean       = sub$HR,
    lower      = sub$CI_lower,
    upper      = sub$CI_upper,
    is.summary = FALSE,
    stringsAsFactors = FALSE
  )
  
  plot_data <- rbind(plot_data, title_row, data_rows)
}

# 构建表头行（自定义列名）
header_row <- c("Subgroup", "No. of risk intervals", "HR (95% CI)", "P value", "P interaction")

# 构建 labeltext 矩阵：第一行为表头，其余为数据行
label_matrix <- rbind(
  header_row,
  cbind(plot_data$label, plot_data$N, plot_data$HR_CI, plot_data$P_val, plot_data$P_int)
)

# 图形向量（需与 label_matrix 行数匹配，表头行对应 NA）
mean_vec   <- c(NA, plot_data$mean)
lower_vec  <- c(NA, plot_data$lower)
upper_vec  <- c(NA, plot_data$upper)
summary_vec <- c(TRUE, plot_data$is.summary)  # 第一行表头设为 TRUE 以加粗

# 绘制森林图
forestplot(
  labeltext  = label_matrix,
  mean       = mean_vec,
  lower      = lower_vec,
  upper      = upper_vec,
  is.summary = summary_vec,
  xlab       = "",
  xlog       = TRUE,
  zero       = 1,
  boxsize    = 0.7,
  lineheight = unit(0.9, "lines"),
  col        = fpColors(box = "darkblue", line = "black", summary = "black"),
  txt_gp     = fpTxtGp(label = gpar(cex = 0.8), ticks = gpar(cex = 0.4)),
  graph.pos  = 3,
  align      = c("l", "c", "c", "c", "c"),
  hrzl_lines = list(
    "2" = gpar(lwd = 1, col = "black", lty = 2),
    "1" = gpar(lwd = 1, col = "black", lty = 1), 
    "38" = gpar(lwd = 1, col = "black", lty = 1)
  ),
  title      = "",
  xlim       = c(1, 24),
  clip       = c(1, 24),
  xticks     = c(1, 2, 4, 8, 16),
  colgap     = unit(2, "mm"),
)

#### ====================== 精神疾病史亚组分析（新增） ======================
# 定义精神疾病史变量列表
psych_vars <- c("Substance-related", "Schizophrenia", "bipolar", 
                "depressive", "Anxiety", "Personality", 
                "Developmental", "Others")

# 直接在筛选后的 cox_data_suicide 中提取精神疾病变量并处理
complete_data_psych <- cox_data_suicide %>%
  select(time, event, state_group, all_of(adjust_covars), all_of(psych_vars)) %>%
  mutate(across(all_of(psych_vars), ~ ifelse(is.na(.), 0, 1))) %>%
  mutate(across(all_of(psych_vars), ~ factor(., levels = c(0, 1), 
                                             labels = c("No", "Yes")))) %>%
  na.omit()  # 删除 adjust_covars 或精神疾病变量有缺失的行（精神疾病已填0，无缺失）

# 初始化存储新亚组结果的列表
psych_subgroup_results <- list()

# 对每个精神疾病变量执行亚组分析（作为分类变量）
for (v in psych_vars) {
  cat("\n正在分析精神疾病史变量：", v, "\n")
  # 构建调整协变量（原有协变量，不包含精神疾病变量自身）
  # 注意：analyze_subgroup 函数会从 adjust_vars 中移除当前变量，这里 adjust_covars 本就不含 v，无需操作
  res <- analyze_subgroup(data = complete_data_psych,
                          var_name = v,
                          var_type = "categorical",
                          adjust_vars = adjust_covars,
                          min_events = min_events_threshold)
  if (nrow(res) > 0) psych_subgroup_results[[v]] <- res
}

# 合并新亚组结果
psych_subgroup_table <- bind_rows(psych_subgroup_results)

cat("\n================== 精神疾病史亚组分析结果 ==================\n")
print(psych_subgroup_table)

#### ====================== 精神疾病史交互作用检验 ======================
psych_interaction_results <- data.frame()

for (v in psych_vars) {
  cat("\n========== 检验交互作用（精神疾病）：", v, "==========\n")
  
  interact_data <- complete_data_psych
  
  # 检查交叉事件分布
  tmp_group <- as.character(interact_data[[v]])
  event_tab <- table(interact_data$state_group, tmp_group, interact_data$event)[,, "1"]
  if (any(event_tab == 0, na.rm = TRUE)) {
    cat("  跳过：存在 state_group ×", v, "水平组合中事件数为0。\n")
    next
  }
  
  # 构建公式
  other_covars <- adjust_covars  # 精神疾病变量不在协变量集中
  rhs_no  <- paste(c("state_group", v, other_covars), collapse = " + ")
  rhs_int <- paste(c(paste0("state_group * ", v), other_covars), collapse = " + ")
  form_no  <- as.formula(paste("Surv(time, event) ~", rhs_no))
  form_int <- as.formula(paste("Surv(time, event) ~", rhs_int))
  
  fit_no  <- tryCatch(coxph(form_no, data = interact_data), error = function(e) NULL)
  fit_int <- tryCatch(coxph(form_int, data = interact_data), error = function(e) NULL)
  if (is.null(fit_no) || is.null(fit_int)) next
  
  lr_test <- tryCatch(anova(fit_no, fit_int, test = "LRT"), error = function(e) NULL)
  if (is.null(lr_test)) next
  
  p_val <- NA
  if (nrow(lr_test) >= 2) {
    p_col <- intersect(c("P(>|Chi|)", "Pr(>|Chi|)", "p"), names(lr_test))
    if (length(p_col) > 0) p_val <- lr_test[2, p_col[1]]
  }
  if (is.null(p_val) || is.na(p_val)) next
  
  p_fmt <- ifelse(p_val < 0.001, "<0.001", sprintf("%.3f", p_val))
  cat("  交互作用 p =", p_fmt, "\n")
  
  psych_interaction_results <- rbind(psych_interaction_results, data.frame(
    Variable = v,
    P_for_interaction = p_fmt,
    N = fit_int$n,
    Events = sum(interact_data$event),
    stringsAsFactors = FALSE
  ))
}

# 将交互P值合并到精神疾病亚组表
if (nrow(psych_interaction_results) > 0) {
  psych_subgroup_table <- psych_subgroup_table %>%
    left_join(psych_interaction_results %>% select(Variable, P_for_interaction), by = "Variable")
} else {
  psych_subgroup_table <- psych_subgroup_table %>%
    mutate(P_for_interaction = NA_character_)
}

cat("\n================== 精神疾病史亚组结果（含交互P值） ==================\n")
print(psych_subgroup_table)

#### ====================== 仅绘制精神疾病史亚组森林图 ======================
library(forestplot)

# 使用之前生成的 psych_subgroup_table（已包含交互P值）
if (exists("psych_labels")) {
  psych_subgroup_table <- psych_subgroup_table %>%
    left_join(psych_labels, by = c("Variable" = "original")) %>%
    mutate(Variable_display = ifelse(is.na(display), Variable, display)) %>%
    select(-display)
} else {
  # 若未定义，则手动创建显示标签
  psych_labels <- data.frame(
    original = psych_vars,
    display  = c("Substance-related disorder", "Schizophrenia", "Bipolar disorder",
                 "Depressive disorder", "Anxiety disorder", "Personality disorder",
                 "Developmental disorder", "Other psychiatric disorders"),
    stringsAsFactors = FALSE
  )
  psych_subgroup_table <- psych_subgroup_table %>%
    left_join(psych_labels, by = c("Variable" = "original")) %>%
    mutate(Variable_display = ifelse(is.na(display), Variable, display)) %>%
    select(-display)
}

# 定义显示顺序（可按需调整）
var_order_psych <- c("Substance-related disorder", "Schizophrenia", "Bipolar disorder",
                     "Depressive disorder", "Anxiety disorder", "Personality disorder",
                     "Developmental disorder", "Other psychiatric disorders")

psych_subgroup_table <- psych_subgroup_table %>%
  mutate(Variable_display = factor(Variable_display, levels = var_order_psych)) %>%
  arrange(Variable_display, Level)

# 构建森林图所需数据结构
plot_data_psych <- data.frame()
for (var in unique(psych_subgroup_table$Variable_display)) {
  sub <- psych_subgroup_table[psych_subgroup_table$Variable_display == var, ]
  p_int <- sub$P_for_interaction[1]
  
  # 标题行：显示变量名和交互P值
  title_row <- data.frame(
    label      = var,
    N          = NA,
    HR_CI      = "",
    P_val      = "",
    P_int      = ifelse(is.na(p_int), "NA", p_int),
    mean       = NA,
    lower      = NA,
    upper      = NA,
    is.summary = TRUE,
    stringsAsFactors = FALSE
  )
  
  # 数据行：显示各水平（No/Yes）的结果
  data_rows <- data.frame(
    label      = paste0("  ", sub$Level),
    N          = sub$N,
    HR_CI      = sprintf("%.2f (%.2f-%.2f)", sub$HR, sub$CI_lower, sub$CI_upper),
    P_val      = sub$P_value,
    P_int      = "",
    mean       = sub$HR,
    lower      = sub$CI_lower,
    upper      = sub$CI_upper,
    is.summary = FALSE,
    stringsAsFactors = FALSE
  )
  
  plot_data_psych <- rbind(plot_data_psych, title_row, data_rows)
}

# 构建表头与图形向量
header_row <- c("Subgroup", "No. of risk intervals", "HR (95% CI)", "P value", "P interaction")
label_matrix_psych <- rbind(header_row, cbind(plot_data_psych$label, plot_data_psych$N, 
                                              plot_data_psych$HR_CI, plot_data_psych$P_val, plot_data_psych$P_int))
mean_vec_psych   <- c(NA, plot_data_psych$mean)
lower_vec_psych  <- c(NA, plot_data_psych$lower)
upper_vec_psych  <- c(NA, plot_data_psych$upper)
summary_vec_psych <- c(TRUE, plot_data_psych$is.summary)

# 绘制森林图
forestplot(
  labeltext  = label_matrix_psych,
  mean       = mean_vec_psych,
  lower      = lower_vec_psych,
  upper      = upper_vec_psych,
  is.summary = summary_vec_psych,
  xlab       = "",
  xlog       = TRUE,
  zero       = 1,
  boxsize    = 0.6,
  lineheight = unit(0.9, "lines"),
  col        = fpColors(box = "darkblue", line = "black", summary = "black"),
  txt_gp     = fpTxtGp(label = gpar(cex = 0.8), ticks = gpar(cex = 0.5)),
  graph.pos  = 3,
  align      = c("l", "c", "c", "c", "c"),
  hrzl_lines = list(
    "2" = gpar(lwd = 1, col = "black", lty = 2),
    "1" = gpar(lwd = 1.2, col = "black", lty = 1),
    "26" = gpar(lwd = 1.2, col = "black", lty = 1)
  ),
  title      = "",
  xlim       = c(0.5, 20),
  clip       = c(0.5, 20),
  xticks     = c(0.5, 1, 2, 4, 8, 16),
  colgap     = unit(3, "mm")
)


# 二轮-时间依赖-亚组分析 ----
library(data.table)
library(mice)
setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
bb141414 <- readRDS("bb141414.rds")
bb987    <- readRDS("bb987.rds")
c142401 <- rbindlist(list(bb987, bb141414), fill = TRUE)

c142401[, Sleep_Duration := as.numeric(Sleep_Duration)]
# 定义需要插补的列名
impute_cols <- c("Age", "Sex", "TDI", "Ethnicity", "Employment", "Qualifications", "BMI",
                 "Income", "Drinking_Status", "Smoking_Status", "Sleep_Duration", "Support_index",
                 "Childhood_Trauma_Index", "Adversity_Events_Index")

dt_impute <- c142401[, ..impute_cols]
# 将字符型转为因子（mice 要求）
for (col in impute_cols) {
  if (is.character(dt_impute[[col]])) {
    set(dt_impute, j = col, value = as.factor(dt_impute[[col]]))
  }
}

set.seed(42)
imp <- mice(dt_impute, m = 2, maxit = 1, printFlag = TRUE)
completed <- complete(imp, action = 1)

# 将插补后的列写回原表
for (col in impute_cols) {
  set(c142401, j = col, value = completed[[col]])
}

# 验证：无缺失值
stopifnot(all(sapply(c142401[, ..impute_cols], function(x) sum(is.na(x))) == 0))

# 辅助函数
safe_as_date <- function(x) {
  if (is.character(x)) as.Date(x) else x
}

## 时间依赖 ----
# 将相关日期列转为 Date 类型
c142401[, start := safe_as_date(start)]
c142401[, ocdtime := safe_as_date(ocdtime)]

# 筛选需要生成新行的记录
split_idx <- which(c142401$state == 1 & c142401$ocdtime > c142401$start)

if (length(split_idx) > 0) {
  # 复制这些行作为新行
  new_rows <- c142401[split_idx]
  # 新行的 start 改为 ocdtime，state 改为 0
  new_rows[, `:=`(
    start = ocdtime,
    state = 2
  )]
  
  # 将原行的 state 改为 1
  c142401[split_idx, state := 3]
  
  
  # 合并新行到原数据
  c142401 <- rbindlist(list(c142401, new_rows), use.names = TRUE, fill = TRUE)
  
  # 排序（若存在 id 列）
  if ("id" %in% names(c142401)) {
    setorder(c142401, id, start)
  } else {
    setorder(c142401, start)
  }
  
  message("已处理 ", length(split_idx), " 行记录：原行 state 改为 3，新增 state=2 行，start 更新为 ocdtime。")
} else {
  message("没有需要处理的记录。")
}

cc142748 <- c142401
# 保存为 RDS 文件，保留所有属性
saveRDS(cc142748, "cc142748.rds")

## icd10未遂的匹配 ----
library(data.table)
setwd("C:\\Users\\lailai\\Desktop\\daima\\ukbank")
cc142748 <- readRDS("cc142748.rds")

# 读取行为数据，只保留 id 和 behaviortime
behavior <- fread("behavior3871.csv", select = c("id", "behaviortime"))
# 每个 id 只保留第一条行为记录（避免一对多匹配）
behavior <- unique(behavior, by = "id", fromLast = FALSE)

# 在 cc142748 中新增三列，初始为 NA
cc142748[, `:=`(behaviortime = NA_character_,
                attempt = NA_integer_,
                time = NA_integer_)]

# 匹配：将 behavior 中的 behaviortime 填入 cc142748
cc142748[behavior, on = "id", behaviortime := i.behaviortime]

# 转换日期格式（start 和 behaviortime）
cc142748[, start_date := as.Date(start, format = "%Y/%m/%d")]
cc142748[, behav_date := as.Date(behaviortime, format = "%Y/%m/%d")]

# 计算天数差（behaviortime - start）
cc142748[, diff_days := as.numeric(behav_date - start_date)]

# 若天数差为非负数，则 time 填入该差值
cc142748[!is.na(diff_days) & diff_days >= 0, time := diff_days]

# 根据 time 是否有值设置 attempt：有值则为 1，否则为 0
cc142748[!is.na(time), attempt := 1L]
cc142748[is.na(time), attempt := 0L]

# 删除临时列（behaviortime 相关）
cc142748[, c("start_date", "behav_date", "diff_days") := NULL]

# 重新计算 start_date（之前已删除）并转换 ocdtime
cc142748[, start_date := as.Date(start, format = "%Y/%m/%d")]
cc142748[, ocd_date := as.Date(ocdtime, format = "%Y/%m/%d")]

# 筛选满足条件的行
idx <- which(cc142748$state == 3 & cc142748$attempt == 1)

if (length(idx) > 0) {
  # 将 attempt 改为 0
  cc142748[idx, attempt := 0L]
  
  # 计算 ocdtime 与 start 的天数差
  cc142748[idx, ocd_diff := as.numeric(ocd_date - start_date)]
  
  # 若天数差非负，则更新 time 为该差值；否则将 time 设为 NA（因为原 time 不再适用）
  cc142748[idx, time := ifelse(!is.na(ocd_diff) & ocd_diff >= 0, ocd_diff, NA_integer_)]
  
  # 删除临时差值列
  cc142748[, ocd_diff := NULL]
}

# 删除临时日期列
cc142748[, c("start_date", "ocd_date") := NULL]

# 可选：查看结果
print(table(cc142748$attempt, useNA = "ifany"))
cat("time 列缺失值数量：", sum(is.na(cc142748$time)), "\n")

## 自我报告未遂的匹配 ----
# 将 attage 和 Age 转为数值，计算年份差（直接转换，不保留临时列）
cc142748[, age_diff_years := as.numeric(attage) - as.numeric(Age)]

# 若年份差非负且原 time 缺失，则 time 填入转换后的天数（0 年按 365 天计）
cc142748[is.na(time) & !is.na(age_diff_years) & age_diff_years >= 0, 
         time := ifelse(age_diff_years == 0, 365L, as.integer(age_diff_years * 365))]

# 根据 time 是否有值更新 attempt
cc142748[!is.na(time), attempt := 1L]
cc142748[is.na(time), attempt := 0L]

# 删除临时列
cc142748[, age_diff_years := NULL]

# 重新生成日期列（用于强制覆盖 state==2 的行）
cc142748[, start_date := as.Date(start, format = "%Y/%m/%d")]
cc142748[, ocd_date := as.Date(ocdtime, format = "%Y/%m/%d")]

idx2 <- which(cc142748$state == 3 & cc142748$attempt == 1)
if (length(idx2) > 0) {
  cc142748[idx2, attempt := 0L]
  cc142748[idx2, ocd_diff := as.numeric(ocd_date - start_date)]
  cc142748[idx2, time := ifelse(!is.na(ocd_diff) & ocd_diff >= 0, ocd_diff, NA_integer_)]
  cc142748[, ocd_diff := NULL]
}

# 删除临时日期列
cc142748[, c("start_date", "ocd_date") := NULL]

# 查看最终结果
print(table(cc142748$attempt, useNA = "ifany"))
cat("time 列缺失值数量：", sum(is.na(cc142748$time)), "\n")

## 共病数量分析 ----
# 筛选 attempt == 1 的行
attempt1_data <- cc142748[attempt == 1]

# 定义精神障碍列名
comorbidity_cols <- c("Substance-related", "Schizophrenia", "bipolar", "depressive", 
                      "Anxiety", "Personality", "Developmental", "Others")

# 确保这些列是数值型，若不是则转换
for (col in comorbidity_cols) {
  if (!is.numeric(attempt1_data[[col]])) {
    set(attempt1_data, j = col, value = as.numeric(attempt1_data[[col]]))
  }
}

# 计算每个个体的共病总数（假设每个列是 0/1 表示有无该障碍）
attempt1_data[, comorbidity_count := rowSums(.SD, na.rm = TRUE), .SDcols = comorbidity_cols]

# 按 state 分组汇总
summary_by_state <- attempt1_data[, .(
  N = .N,
  mean_comorbidity = mean(comorbidity_count, na.rm = TRUE),
  sd_comorbidity = sd(comorbidity_count, na.rm = TRUE),
  min_comorbidity = min(comorbidity_count, na.rm = TRUE),
  max_comorbidity = max(comorbidity_count, na.rm = TRUE)
), by = state]

# 也可以给出每个共病数量的频数分布表
freq_table <- attempt1_data[, .N, by = .(state, comorbidity_count)]
freq_table <- dcast(freq_table, state ~ comorbidity_count, value.var = "N", fill = 0)

# 打印结果
print("按 state 分组的 attempt=1 样本共病情况汇总：")
print(summary_by_state)

print("各 state 下共病数量频数分布：")
print(freq_table)

#### 自我报告死亡 ----
suicidedied <- fread("suicidedied.csv", select = 1)
suicide_ids <- unique(suicidedied[[1]])

# 1. 新增 suicide 列，初始值为 0
cc142748[, suicide := 0L]

# 2. 若 id 在自杀死亡 ID 列表中，则 suicide 标记为 1
cc142748[id %in% suicide_ids, suicide := 1L]

# 3. 报告 suicide == 1 中不同 state 的数量
cat("\n========== suicide = 1 样本按 state 分组的频数 ==========\n")
suicide_by_state <- cc142748[suicide == 1, .N, by = state]
print(suicide_by_state)

# 若需按唯一 id 统计（因为一个 id 可能因时间拆分有多行）
unique_suicide_ids <- cc142748[suicide == 1, uniqueN(id)]
cat(sprintf("唯一自杀死亡个体数：%d\n", unique_suicide_ids))

# 筛选 suicide == 1 的行
suicide1_data <- cc142748[suicide == 1]

# 定义精神障碍列名（与之前 attempt 分析一致）
comorbidity_cols <- c("Substance-related", "Schizophrenia", "bipolar", "depressive", 
                      "Anxiety", "Personality", "Developmental", "Others")

# 确保这些列为数值型（若为因子或字符则转换）
for (col in comorbidity_cols) {
  if (!is.numeric(suicide1_data[[col]])) {
    set(suicide1_data, j = col, value = as.numeric(as.character(suicide1_data[[col]])))
  }
}

# 计算每个个体的共病总数（每列 0/1 表示有无该障碍）
suicide1_data[, comorbidity_count := rowSums(.SD, na.rm = TRUE), .SDcols = comorbidity_cols]

# 按 state 分组汇总统计量
summary_suicide_state <- suicide1_data[, .(
  N = .N,
  mean_comorbidity = mean(comorbidity_count, na.rm = TRUE),
  sd_comorbidity = sd(comorbidity_count, na.rm = TRUE),
  min_comorbidity = min(comorbidity_count, na.rm = TRUE),
  max_comorbidity = max(comorbidity_count, na.rm = TRUE)
), by = state]

# 生成各 state 下共病数量的频数分布表
freq_suicide_table <- suicide1_data[, .N, by = .(state, comorbidity_count)]
freq_suicide_wide <- dcast(freq_suicide_table, state ~ comorbidity_count, value.var = "N", fill = 0)

# 输出结果
cat("\n========== suicide=1 样本按 state 分组的共病数量汇总 ==========\n")
print(summary_suicide_state)

cat("\n========== 各 state 下共病数量频数分布（suicide=1） ==========\n")
print(freq_suicide_wide)

#### 共病条形图 ----
library(ggplot2)
library(dplyr)   # 用于数据整理

# ========== 输入数据 ==========
values1 <- c(20.26, 26.67, 29.23, 13.08, 6.41, 4.35)
values2 <- c(21.47, 26.59, 28.54, 13.17, 6.10, 4.14)
values3 <- c(45, 25, 15, 15, 0, 0)
values4 <- c(2.13, 14.89, 21.28, 17.02, 25.53, 19.15)
values5 <- c(0, 9.76, 19.51, 19.51, 29.27, 21.95)
values6 <- c(12.5, 50, 37.5, 0, 0, 0)

categories <- c("No recorded comorbidities", "One comorbidity", "Two comorbidities", 
                "Three comorbidities", "Four comorbidities", "Five or more comorbidities")

# 构建数据，并添加分组信息
plot_data <- data.frame(
  Group = rep(c("Suicide behavior", "Suicide attempts", "Suicide death",
                "Suicide behavior", "Suicide attempts", "Suicide death"),
              each = length(categories)),
  OCD_status = rep(c("Never-recorded OCD", "Ever-recorded OCD"), each = 3 * length(categories)),
  Category = factor(rep(categories, times = 6), levels = categories),
  Percent = c(values1, values2, values3, values4, values5, values6)
) %>%
  mutate(Group = factor(Group, levels = c("Suicide behavior", "Suicide attempts", "Suicide death")))

# 绘制分面堆叠百分比柱状图（条带置于底部且无背景）
ggplot(plot_data, aes(x = Group, y = Percent, fill = Category)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.85) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(0, 1, by = 0.1),           # 0%, 10%, 20%, ... 100%
    expand = expansion(mult = c(0, 0.05))
  ) +
  facet_grid(. ~ OCD_status, scales = "free_x", space = "free_x",
             switch = "x") +               
  labs(x = NULL, y = "", fill = "") +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.justification = "top",
    legend.direction = "vertical",
    legend.key.size = unit(1.8, "lines"),
    legend.key.height = unit(1.8, "lines"),
    legend.spacing = unit(0.85, "cm"),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(),    
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",    
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11)
  ) +
  guides(fill = guide_legend(reverse = TRUE)) 
