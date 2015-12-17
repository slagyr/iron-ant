(ns iron-ant-tyson.core
  (:require [clj-http.client :as http]
            [clojure.edn :as edn]))

(def MAX_ANTS 100)

(def state (atom {}))

(defn init [host] (reset! state {:host host}))

(defn call [path]
  (let [result (http/get (str (:host @state) path))]
    (when-not (= 200 (:status result))
      (prn "Failed request:" result))
    (let [data (edn/read-string (:body result))]
      (when-not (= "ok" (:response data))
        (prn "Failed API call: " data))
      (:stat data))))

(defn food? [thing] (= :food (:type thing)))

(defn directional-coordinates [[x y]]
  {"n"  [x (dec y)]
   "ne" [(inc x) (dec y)]
   "e"  [(inc x) y]
   "se" [(inc x) (inc y)]
   "s"  [x (inc y)]
   "sw" [(dec x) (inc y)]
   "w"  [(dec x) y]
   "nw" [(dec x) (dec y)]})

(defn direction-to [[x1 y1] [x2 y2]]
  (let [ew (cond
             (= x1 x2) ""
             (> x1 x2) "w"
             :else "e")
        ns (cond
             (= y1 y2) ""
             (> y1 y2) "n"
             :else "s")
        dir (str ns ew)]
    (if (= "" dir) nil dir)))

(defn random-location [] [(- (rand-int 50) 25) (- (rand-int 50) 25)])

(defn catalog-observed-food [{:keys [surroundings]}]
  (let [stuff (apply concat (vals surroundings))
        food (filter food? stuff)]
    (swap! state
           (fn [state]
             (reduce #(assoc-in %1 [:foods (:id %2)] %2) state food)))))

(defn remove-missing-food [{:keys [location surroundings] :as stat}]
  (let [directions (directional-coordinates location)]
    (swap! state (fn [state]
                   (reduce
                     (fn [state [dir loc]]
                       (let [known (filter #(= loc (:location %)) (vals (:foods state)))
                             exists? (seq (filter food? (get surroundings dir)))]
                         (if exists?
                           state
                           (reduce #(update-in %1 [:foods] dissoc (:id %2)) state known))))
                     state directions)))))

(defn distance [[x1 y1] [x2 y2]]
  (let [a (Math/abs (- x1 x2))
        b (Math/abs (- y1 y2))]
    (Math/sqrt (+ (* a a) (* b b)))))

(defn nearest-food [location]
  (->> (:foods @state)
       vals
       (sort-by #(distance location (:location %)))
       first))

(defn process-surroundings [stat]
  (remove-missing-food stat)
  (catalog-observed-food stat))

(defn join [name]
  (let [nest (call (str "/join/" name))]
    (swap! state assoc :nest nest)))

(defn stat-nest []
  (let [nest (call (format "/%s/stat" (:id (:nest @state))))]
    (swap! state update-in [:nest] merge nest)))

(defn spawn [hunter?]
  (let [ant (call (format "/%s/spawn" (:id (:nest @state))))
        ant (assoc ant :hunter? hunter?)]
    (swap! state assoc-in [:ants (:id ant)] ant)
    (process-surroundings ant)
    (:id ant)))

(defn go [ant-id direction]
  (let [ant (call (format "/%s/go/%s" ant-id direction))]
    (swap! state update-in [:ants (:id ant)] merge ant)
    (process-surroundings ant)
    (:id ant)))

(defn has-food-destination? [ant]
  (and (:food-guess ant) (not= (:food-guess ant) (:location ant))))

(defn goto-food-destination [ant]
  (direction-to (:location ant) (:food-guess ant)))

(defn assign-and-goto-food-dest [ant]
  (let [food-guess (random-location)
        ant (assoc ant :food-guess food-guess)]
    (swap! state assoc-in [:ants (:id ant)] ant)
    (goto-food-destination ant)))

(defn go-home [ant]
  (direction-to (:location ant) [0 0]))

(defn goto-food [ant food]
  (direction-to (:location ant) (:location food)))

(defn look-for-food [ant]
  (if (has-food-destination? ant)
    (goto-food-destination ant)
    (assign-and-goto-food-dest ant)))

(defn decide-what-to-do [ant]
  (if (:got-food ant)
    (go-home ant)
    (if (:hunter? ant)
      (look-for-food ant)
      (if-let [food (nearest-food (:location ant))]
        (goto-food ant food)
        (look-for-food ant)))))

(defn ant-tick [ant-id]
  (let [ant (get-in @state [:ants ant-id])]
    (decide-what-to-do ant)))

(defn spawn? []
  (let [nest (:nest @state)]
    (and (< 0 (:food nest))
         (< (:ants nest) MAX_ANTS))))

(defn spawn-ant-thread
  ([] (spawn-ant-thread false))
  ([hunter?]
   (.start
     (Thread.
       (fn []
         (try
           (let [ant-id (spawn hunter?)]
             (while true
               (let [dir (ant-tick ant-id)]
                 (go ant-id dir))))
           (catch Exception e
             (.printStackTrace e))))))))

(defn overwatch-thread []
  (.start
    (Thread.
      (fn []
        (try
          (while true
            (Thread/sleep 1000)
            (stat-nest)
            (when (spawn?)
              (let [hunter? (< (count (:ants @state)) 2)]
                (spawn-ant-thread hunter?))))
          (catch Exception e
            (.printStackTrace e)))))))

(defn -main [& args]
  (let [name (or (first args) "iron-ant-tyson")]
    (println "Iron Ant Tyson")
    (init "http://localhost:8888")
    (join name)
    (overwatch-thread)))
