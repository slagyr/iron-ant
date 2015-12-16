(ns iron-ant-tyson.core
  (:require [clj-http.client :as http]
            [clojure.edn :as edn]))

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

(defn join []
  (let [nest (call "/join/iron-ant-tyson")]
    (swap! state assoc :nest-id (:id nest))))

(defn spawn []
  (let [ant (call (format "/%s/spawn" (:nest-id @state)))]
    (swap! state assoc-in [:ants (:id ant)] ant)
    (:id ant)))

(defn go [ant-id direction]
  (let [ant (call (format "/%s/go/%s" ant-id direction))]
    (swap! state assoc-in [:ants (:id ant)] ant)
    (:id ant)))

(defn has-food-destination? [ant]
  (and (:food-guess ant) (not= (:food-guess ant) (:location ant))))

(defn goto-food-destination [ant]
  (direction-to (:location ant) (:food-guess ant)))

(defn assign-and-goto-food-dest [ant]
  (let [food-guess (random-location)
        ant (assoc ant :food-guess food-guess)]
    (swap! state assoc-in [:ants (:id ant)] ant)
    (direction-to (:location ant) (:food-guess ant))))

(defn ant-tick [ant-id]
  (let [ant (get-in @state [:ants ant-id])]
    (if (:got-food ant)
      (direction-to (:location ant) [0 0])
      (if-let [food (nearest-food (:location ant))]
        (direction-to (:location ant) (:location food))
        (if (has-food-destination? ant)
          (goto-food-destination ant)
          (assign-and-goto-food-dest ant))))))

(defn spawn? []
  (let [n (count (:ants @state))]
    (< n 5)))

(defn -main []
  (println "Iron Ant Tyson")
  (init "http://localhost:8888")
  (join)
  (while true
    (let [workers (remove nil?
                          (cons (when (spawn?) (future (spawn)))
                                (map #(future
                                        (let [dir (ant-tick %)]
                                          (go % dir))) (keys (:ants @state)))))]
      (doseq [worker workers] (deref worker)))))
