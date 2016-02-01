(ns hyperselection-rates.simulation)

(defn get-tournament
  [tournament-size values]
  (repeatedly tournament-size
              #(rand-nth values)))

; (get-tournament 7 (range 1000))

(defn select
  [tournament-size values]
  (let [tournament (get-tournament tournament-size values)]
    (apply min tournament)))

; (select 7 (range 1000))

(defn run-frequencies
  [tournament-size values num-selections]
    (frequencies (repeatedly num-selections #(select tournament-size values))))

(defn count-hyperselections
  [freqs num-selections limits]
  (for [l limits
        :let [lim (* l num-selections)]]
    (count (filter #(>= (second %) lim) freqs))))

(defn average [xs] (/ (reduce + xs) (count xs)))

(defn combine-hyperselection-counts
  [counts]
  (reduce (fn [[rs rc] e] [(map + rs e) (inc rc)]) [[0 0 0] 0] counts))

; (combine-hyperselection-counts [[5 8 9] [6 3 2] [0 1 2]])

(defn par-avg [counts]
  (let [[sums cnt] (combine-hyperselection-counts counts)]
    (map #(/ % cnt) sums)))

; (par-avg [[2 0 0] [3 0 0]])

(time
 (doall
  (par-avg
   (repeatedly 1000
               #(count-hyperselections (run-frequencies 7 (range 1000) 1700) 1700 [0.01 0.05 0.1])))))


