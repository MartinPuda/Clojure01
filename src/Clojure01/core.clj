(ns Clojure01.core
  (:use seesaw.border)
  (:use seesaw.color)
  (:use seesaw.core)
  (:use seesaw.font)
  (:use seesaw.options)
  (:use seesaw.widget-options)
  (:use seesaw.graphics)
  (:use seesaw.dev))

;vraci list 10 nahodnych nestejnych cisel (pozice min na hraci plose)
(defn create-mines-list [] (loop [mines '()] (if (= 15 (count mines)) mines
                                                               (recur (distinct (conj mines
                                                                                    (int (rand 100))))))))


;vraci list 100 tlacitek
(defn create-buttons [] (loop [i 0 vekt []] (if (= i 100) vekt
                                                       (recur (inc i) (conj vekt
                                                                            (button :text ""
                                                                                    :icon "gradient30.png"
                                                                                    :user-data i
                                                                                    :border (line-border :thickness 1 :color "gray")
                                                                                    :size [30 :by 30]
                                                                                    ))))))

;ciselne ohodnoceni jednoho tlacitka, zda ma minu nebo ne
(defn button-value [mines-list num]
  (if (some #(= num %) mines-list) 1 0))

;seznam sousedu tlacitka v mrizce, nevraci v prirozenem poradi
(defn button-neigh [num]
  (remove #(or (> 0 %)
               (> % 99))
          (concat (if (= (rem num 10) 0) '() (list (- num 11) (+ num 9) (- num 1)))
                  (list (+ num 10) (- num 10))
                  (if (= (rem num 10) 9) '() (list  (+ num 1) (- num 9) (+ num 11)))
                  )))

;ciselne ohodnoceni tlacitka v zavislosti na jeho okoli
(defn button-rating [mines-list num]
  (loop [val 0
         neigh (button-neigh num)]
    (if (empty? neigh) val (recur (+ val (button-value mines-list (first neigh)))
                                  (rest neigh)))))

;callback po kliknuti na tlacitko
(defn reveal [buts mines-list num]
  (let [but (nth buts num)]
    ; (println "cislo tlacitka je " num " a jeho rating " (button-rating mines-list num) " a jeho neigh " (button-neigh num) " a jeho icon " (config but :icon))
    (when (= "" (config but :text))
      (if (some #(= num %) mines-list)
        (do (config! but
                     :icon nil
                     :background (color 255 0 0)
                     :text "\uD83D\uDCA3"))
        (let [rating (button-rating mines-list num)]
          (do (config! but
                     :icon nil
                     :background (color 192 192 192)
                     :font (font :name :monospaced :size 15)
                     :text (if (= rating 0) " " rating))
                     (when (= rating 0)
                       (doseq [j (button-neigh num)] (reveal buts mines-list j)))))))))

;vraci 10x10 grid s minami
(defn create-mine-grid [game-frame]
  (let [grid (grid-panel :id :grid-pane
                         :columns 10
                         :rows 10
                         :user-data (create-mines-list)
                         :items (create-buttons))]
    (config! (select grid [:JButton])
             :listen
             [:action (fn [e] (reveal (select (select game-frame [:#grid-pane]) [:JButton])
                                      (config (select game-frame [:#grid-pane]) :user-data)
                                      (config e :user-data)))])
    grid))

;vraci tlacitko v menu
(defn create-button [game-frame]
  (let [but (button :text "New Game" :id :but)]
    (config! but
             :listen
             [:action (fn [e] (config! game-frame :content (create-mine-grid game-frame)))])
    but))

;main
(defn -main [& args]
  (println (show-options (button))
  (let [game-frame (frame :title "Minesweeper"
                          :content ""
                          :size [300 :by 300]
                          :on-close :exit
                          )]
    (config! game-frame :menubar (menubar :items (list (create-button game-frame))))
    (config! game-frame :content (create-mine-grid game-frame))
    (-> game-frame show!))))