#lang sicp

;; 因为 我们的句子的阅读方向本身就是从左往右的
;; 假设 amb 方向是从右往左的，

;; 那么我们可能会遇到 (verb article noun) 颠倒过来变成 (noun article verb)
;; 我们的 parse-sentence 没有这样的解析逻辑

;; 从而会导致失败 
