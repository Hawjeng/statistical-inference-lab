# 區間估計與假設檢定互動教材

邱皓政統計互動教材，以 **Shiny R** 實作，使用 **Shinylive / WebR** 發布到 GitHub Pages。

教材入口：[教學與專案](https://hawjeng.github.io/resources.html)  
互動網站：https://hawjeng.github.io/statistical-inference-lab/

## 教材順序

以問題主題排列，不標章次，方便不同專書共用範例。

1. 原理說明：抽樣與標準誤、點估計與區間估計、假設檢定五步驟、尾機率與臨界值、決策錯誤。
2. 課本範例：投手球速、研究生年齡、睡眠時間、簡單與系統抽樣、比例分層、單雙尾臨界值；附 14 個 Excel 原檔。
3. 星座範例：13 個數值變數、7 個類別欄位；分成逐一星座與十二星座整體比較。

## 兩個不同的問題

- **逐一星座 vs 全體參考值**：平均數 z／t、指定成功事件的比例 z、類別分布卡方適合度。
- **十二星座整體比較**：數值變數用 Welch／一般單因子 ANOVA；類別變數用卡方獨立性檢定。整體顯著不等於已指出哪一組不同。

可調整 α、左尾／右尾／雙尾。F 與卡方檢定固定右尾。CI 一律保持雙側 (1−α)，不隨檢定方向切換。各表可收合，附 p-value 與保留／拒絕 H0 結論。

## 圖形規格

- 黑色實線表示全體 μ；橫軸以 μ±kσ 標示母體位置，下排為對應原始數值。
- σ 是全體有效資料的母體標準差，不是 SE，也不是 z-statistic 或 t-statistic。
- 星座名稱與 n 各顯示一次，星座有 1–12 編號；成對 CI 靠近，z 為實線圓點，t 為虛線菱形。
- 端點數值放在區間外側；平均值只顯示數字；CI 未涵蓋參考值標紅。
- 拒絕域與 p-value 分別繪圖。檢定圖橫軸明確寫出 z-statistic、t-statistic、F-statistic 或 chi-square statistic。

## 資料處理

調查來源為 data2.xlsx 的 Cleaned_Data，共 713 列。每個變數各自排除缺失，0 保留，不再刪除離群值。網站資料只公開彙總，不公開逐筆身分資訊。

手機品牌改為 **iPhone / 非 iPhone**：無手機為非 iPhone；不確定與無法判定的其他回答排除 7 筆，有效 N=706。上網方案改為 **吃到飽 / 非吃到飽**：限量、預付卡、Wi-Fi only 為非吃到飽；無法判定排除 3 筆，有效 N=710。其他變數使用原類別。ID 不作檢定；Z、Zname、Zodiac 是分組資訊；Gender 與 Sex 為同一性別資訊的兩種編碼。

平均數 z 的 σ 使用全體有效值、分母 N；t 的 s 使用各組、分母 n−1。比例 CI 用 Wilson score，比例 z 不作連續性修正。卡方不作連續性修正，期望次數過低時提供 R 的 Monte Carlo 模擬（5,000 次、固定種子），並標示近似條件。

星座不是隨機分配，全體包含各組。把 μ、σ、p0 作固定已知參考，是本習作的課堂設定。保留 H0 只是未能拒絕，不代表證明相等。未作有限母體或多重比較修正，不作因果推論。

課本文字依作者提供的估計與檢定校稿整理，不公開 Word 校稿檔。原始 Excel 若先將 SE 四捨五入，可能與本程式使用完整精度的答案略有差異。

## RStudio 執行

下載整個儲存庫後，在根目錄執行：

```r
install.packages(c("shiny", "jsonlite"))
shiny::runApp("app")
```

- `app/app.R`：Shiny UI 與 server。
- `app/stats.R`：R 的檢定、CI、SVG 圖形函數。
- `app/data/`：應用程式所用彙總統計與課本範例。
- `app/www/`：教材文字與樣式。
- `analysis.R`：base R 的獨立重現語法。
- `data/`：可讀取的彙總 CSV 與 JSON。
- `downloads/`：14 份課本 Excel、四變數 Excel 計算表與靜態 HTML。
- `tests/test_shiny.R`：Shiny reactive 測試，從 `app/` 執行 `source("../tests/test_shiny.R")`。

## 建置與發布

```r
install.packages("shinylive")
source("build.R")
```

`site/` 是匯出的靜態網站，支援 GitHub Pages。GitHub Actions 在 main 更新後重新匯出並部署。首次開啟需下載 WebR 執行環境，載入速度取決於網路與裝置。

## 驗證

核心公式與 R 的分配函數核對；反應式測試涵蓋課本範例、雙側 CI 不隨單尾方向改變、二分變數、適合度與獨立性、分層配置。Welch 與一般 ANOVA 以符合各組 n、平均數、SD 的重建資料對照 `oneway.test`。

## 權利

教材文字、課本範例與調查資料之權利屬原作者，供教學與學習使用；原檔不另行改授權。
