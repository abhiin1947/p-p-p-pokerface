(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank) (Integer/valueOf (str rank)) (get replacements rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (> (count (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand))))) 0))

(defn three-of-a-kind? [hand]
  (> (count (filter (fn [x] (= x 3)) (vals (frequencies (map rank hand))))) 0))

(defn four-of-a-kind? [hand]
  (> (count (filter (fn [x] (= x 4)) (vals (frequencies (map rank hand))))) 0))

(defn flush? [hand]
  (> (count (filter (fn [x] (= x 5)) (vals (frequencies (map suit hand))))) 0))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (or (> (count (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand))))) 1) (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [otherhand (sort
                   (replace
                    {14 1}
                    (map rank hand)))
        hand2 (sort
               (map rank hand))
        gen-range (fn [x]
                    (range
                     (apply min x)
                     (+ (apply min x) 5)))]
    (or
     (=
      (gen-range hand2)
      hand2)
     (=
      (gen-range otherhand)
      otherhand))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (cond
   (straight-flush? hand)   8
   (four-of-a-kind? hand)   7
   (full-house? hand)       6
   (flush? hand)            5
   (straight? hand)         4
   (three-of-a-kind? hand)  3
   (two-pairs? hand)        2
   (pair? hand)             1
   (high-card? hand)        0))
