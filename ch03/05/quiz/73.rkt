#lang sicp
(#%require "../../../common/data/stream.rkt")

;; RC 电路是一种带有电阻和电容的串联电路
;; 其基本特性是可以对输入信号进行滤波
;; 电容的单位是法拉，表示其储存电荷的能力
;; 1 法拉定义为当电容器两端电压变化为 1 V 时储存的电荷量为 1 库伦

;; RC 电路可以用来构建积分电路和微分电路
;; 从而近似实现连续时间信号处理操作
;; RC 电路的基本输入输出为：
;; 输入 i 电流, v0 为电容器的初始电压, R 为电阻值
;; 输出 v 为电压
;;
;; v = v0 + 1/C*∫_{0^t}i dt + R*i
;; 大概可以理解为，初始电压随着电容充电过程不断增加，
;; 充电过程中电压会逐渐升高，充电完成后电压理论上不再变化(个人理解，不保真)

(define (RC R C dt)
  (lambda (i v0)
    (add-stream (scale-stream i R)
                (integral (scale-stream i (/ 1 C)) v0 dt))))

(define RC1 (RC 5 1 0.5))
(stream-refs 100 (RC1 ones 4))
