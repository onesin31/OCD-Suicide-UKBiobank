# 二轮:多重插补 ----
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

# 汇报各变量缺失值情况
missing_count <- sapply(dt_impute, function(x) sum(is.na(x)))
missing_pct <- round(missing_count / nrow(dt_impute) * 100, 2)
missing_report <- data.frame(Variable = names(missing_count),
                             Missing_Count = missing_count,
                             Missing_Percent = missing_pct)
print("插补前各变量缺失值情况：")
print(missing_report)

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

# 续写代码：计算 VIF ----
library(car)  # 提供 vif 函数

# 确保分类变量为因子类型（mice 后已是因子，但再确认一遍）
factor_vars <- c("Ethnicity", "Employment", "Qualifications", 
                 "Drinking_Status", "Smoking_Status")
for (fv in factor_vars) {
  if (!is.factor(c142401[[fv]])) {
    set(c142401, j = fv, value = as.factor(c142401[[fv]]))
  }
}

# 构建线性回归模型（以 BMI 为因变量）
model_vif <- lm(BMI ~ TDI + Ethnicity + Employment + Qualifications + 
                  Income + Drinking_Status + Smoking_Status + 
                  Sleep_Duration + Support_index + Childhood_Trauma_Index + 
                  Adversity_Events_Index,
                data = c142401)

# 计算并输出 VIF
vif_results <- vif(model_vif)
print("方差膨胀因子 (VIF) 结果：")
print(vif_results)


# ---------- 1. 确认分组变量 ----------
# 假设分组变量名为 "state"，若为其他名称请修改此处
group_var <- "state"
if (!group_var %in% names(c142401)) {
  stop("分组变量 '", group_var, "' 不存在，请检查列名。可用列名：\n", 
       paste(names(c142401), collapse = ", "))
}

# 将 state 转为因子（便于表格呈现）
c142401[, (group_var) := as.factor(get(group_var))]
group_levels <- levels(c142401[[group_var]])

# ---------- 2. 定义变量列表 ----------
# 连续变量（全部视为非正态，报告中位数和 IQR）
cont_vars <- c("Age", "TDI", "BMI", "Sleep_Duration", 
               "Support_index", "Childhood_Trauma_Index", "Adversity_Events_Index")

# 分类变量
cat_vars <- c("Sex", "Ethnicity", "Employment", "Income", "Qualifications", 
              "Drinking_Status", "Smoking_Status")

# ---------- 3. 汇总函数 ----------
# 连续变量：中位数 (IQR)
summarise_cont <- function(x) {
  med <- median(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  sprintf("%.2f (%.2f-%.2f)", med, q1, q3)
}

# 分类变量：频数 (%)
summarise_cat <- function(x) {
  tab <- table(x, useNA = "no")
  pct <- prop.table(tab) * 100
  paste0(tab, " (", round(pct, 1), "%)")
}

# ---------- 4. 生成分组描述表 ----------
# 用于存储每个变量的汇总结果
result_list <- list()

# 连续变量处理
for (v in cont_vars) {
  dt_sub <- c142401[, .(Summary = summarise_cont(get(v))), by = group_var]
  dt_sub[, Variable := v]
  setnames(dt_sub, old = group_var, new = "Group")
  result_list[[v]] <- dt_sub
}

# 分类变量处理
for (v in cat_vars) {
  # 先计算各分组内各水平的频数
  dt_sub <- c142401[, .N, by = c(group_var, v)]
  dt_sub[, Pct := round(N / sum(N) * 100, 1), by = group_var]
  dt_sub[, Summary := paste0(N, " (", Pct, "%)")]
  # 变量名包含水平信息，以便区分
  dt_sub[, Variable := paste0(v, ": ", get(v))]
  setnames(dt_sub, old = c(group_var, v), new = c("Group", "Level"))
  result_list[[paste0("cat_", v)]] <- dt_sub[, .(Group, Variable, Summary)]
}

# 合并所有结果
baseline_long <- rbindlist(result_list, use.names = TRUE, fill = TRUE)

# 转换为宽表格：一行为一个变量，两列为 state = 0 和 state = 1 的汇总值
baseline_wide <- dcast(baseline_long, Variable ~ Group, value.var = "Summary")
setnames(baseline_wide, old = group_levels, new = paste0("state_", group_levels))

# ---------- 5. 添加组间比较的 P 值（可选） ----------
# 使用 wilcox.test 比较连续变量，chisq.test 比较分类变量
p_values <- sapply(c(cont_vars, cat_vars), function(v) {
  if (v %in% cont_vars) {
    # Mann-Whitney U 检验
    test <- wilcox.test(c142401[[v]] ~ c142401[[group_var]])
    return(test$p.value)
  } else {
    # 卡方检验
    tab <- table(c142401[[v]], c142401[[group_var]])
    # 若期望频数小于5则使用 Fisher 精确检验
    if (any(chisq.test(tab)$expected < 5)) {
      test <- fisher.test(tab, simulate.p.value = TRUE)
    } else {
      test <- chisq.test(tab)
    }
    return(test$p.value)
  }
})

# 格式化 P 值
format_p <- function(p) {
  if (p < 0.001) return("<0.001")
  else return(sprintf("%.3f", p))
}
p_formatted <- sapply(p_values, format_p)

# 将 P 值添加到宽表格对应变量行
# 注意：分类变量的 Variable 名称带有水平前缀，需匹配基础变量名
p_df <- data.table(Variable = names(p_values), P_value = p_formatted)
# 对于分类变量，p 值对应的 Variable 需要与 baseline_wide 中的 Variable 匹配
# 简单处理：若 Variable 在 baseline_wide 中找不到，尝试匹配前缀
baseline_wide[, Base_Var := sub(":.*", "", Variable)]
p_df[, Base_Var := Variable]
# 合并 P 值
baseline_wide <- merge(baseline_wide, p_df[, .(Base_Var, P_value)], 
                       by = "Base_Var", all.x = TRUE, sort = FALSE)
baseline_wide[, Base_Var := NULL]

# ---------- 6. 输出结果 ----------
print("插补后按 state 分组的基线特征比较表：")
print(baseline_wide)


# ==================== 续写：汇报精神疾病变量频数和百分比 ====================

# 定义精神疾病变量名称（请根据实际列名确认）
psych_vars <- c("Substance-related", "Schizophrenia", "bipolar", "depressive", 
                "Anxiety", "Personality", "Developmental", "Others")

# 检查变量是否存在
missing_psych <- setdiff(psych_vars, names(c142401))
if (length(missing_psych) > 0) {
  warning("以下变量在数据中不存在，将被跳过：", paste(missing_psych, collapse = ", "))
  psych_vars <- intersect(psych_vars, names(c142401))
}

# 将每个变量的 NA 转换为 0，并创建因子（Yes/No）
for (v in psych_vars) {
  # 将非 1 的值（包括 NA）替换为 0
  c142401[, (v) := ifelse(get(v) == 1 & !is.na(get(v)), 1, 0)]
  # 转为因子，便于表格显示
  c142401[, (v) := factor(get(v), levels = c(0, 1), labels = c("No", "Yes"))]
}

# 为每个精神疾病变量生成分组描述
psych_list <- list()
p_psych_list <- list()  # 存储 P 值

for (v in psych_vars) {
  # 按 State 和变量水平统计频数
  dt_sub <- c142401[, .N, by = c(group_var, v)]
  dt_sub[, Pct := round(N / sum(N) * 100, 1), by = group_var]
  dt_sub[, Summary := paste0(N, " (", Pct, "%)")]
  dt_sub[, Variable := paste0(v, ": ", get(v))]
  setnames(dt_sub, old = c(group_var, v), new = c("Group", "Level"))
  
  # 只保留 "Yes" 行（通常疾病报告仅关注患病率，若需同时报告无病可注释下面一行）
  dt_sub <- dt_sub[Level == "Yes"]
  # 如果某组 Yes 频数为 0，dt_sub 可能为空，需处理
  if (nrow(dt_sub) > 0) {
    psych_list[[v]] <- dt_sub[, .(Group, Variable, Summary)]
  } else {
    # 若两组均无患病，生成空白行
    psych_list[[v]] <- data.table(Group = group_levels, 
                                  Variable = paste0(v, ": Yes"), 
                                  Summary = "0 (0.0%)")
  }
  
  # 计算 P 值（比较两组患病率）
  tab <- table(c142401[[v]], c142401[[group_var]])
  if (nrow(tab) == 2 && ncol(tab) == 2) {
    if (any(chisq.test(tab)$expected < 5)) {
      p_val <- fisher.test(tab, simulate.p.value = TRUE)$p.value
    } else {
      p_val <- chisq.test(tab)$p.value
    }
  } else {
    p_val <- NA  # 例如某组全为 No 的情况
  }
  p_psych_list[[v]] <- data.table(Variable = paste0(v, ": Yes"), P_value = p_val)
}

# 合并精神疾病结果
psych_long <- rbindlist(psych_list, use.names = TRUE, fill = TRUE)
psych_p <- rbindlist(p_psych_list)

# 转换为宽格式并与原基线表合并
psych_wide <- dcast(psych_long, Variable ~ Group, value.var = "Summary")
setnames(psych_wide, old = group_levels, new = paste0("State_", group_levels))

# 合并 P 值
psych_wide <- merge(psych_wide, psych_p, by = "Variable", all.x = TRUE)

# 格式化 P 值
psych_wide[, P_value := sapply(P_value, function(p) {
  if (is.na(p)) return("—")
  else if (p < 0.001) return("<0.001")
  else return(sprintf("%.3f", p))
})]

# 将结果追加到之前的 baseline_wide 表格（如果存在）
if (exists("baseline_wide")) {
  # 为保持一致，将 baseline_wide 中的 P_value 转为字符型（若有）
  if ("P_value" %in% names(baseline_wide)) {
    baseline_wide[, P_value := as.character(P_value)]
  }
  final_table <- rbindlist(list(baseline_wide, psych_wide), fill = TRUE)
} else {
  final_table <- psych_wide
}

print("包含精神疾病变量的完整基线特征表：")
print(final_table)

# 可选：导出为 CSV
# fwrite(final_table, "baseline_with_psych.csv")