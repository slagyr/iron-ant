(ns iron-ant-tyson.core-spec
  (:require [speclj.core :refer :all]
            [iron-ant-tyson.core :refer :all]
            [clj-http.client :as http]))

(defn success [body] {:status 200 :body body})

(describe "Iron Ant Tyson"

  (before (init "http://localhost:8888"))

  (it "joining"
    (with-redefs [http/get (constantly (success "{:response \"ok\", :stat {:type :nest, :location [0 0], :food 5, :team \"iron-ant-tyson\", :id \"45356038\"}}\n"))]
      (join "iron-ant-tyson")
      (should= "45356038" (:id (:nest @state)))))

  (it "spawning"
    (swap! state assoc :nest-id "45356038")
    (with-redefs [call (constantly {:type :ant, :location [0 0], :got-food false,
                                    :team "iron-ant-tyson", :nest "45356038", :n 1,
                                    :id   "44003385", :surroundings {"n" [], "w" [], "s" [], "sw" [], "ne" [], "e" [], "nw" [], "se" []}})]
      (let [result (spawn false)]
        (should-not-be-nil (get-in @state [:ants "44003385"]))
        (should= "44003385" result))))

  (it "goes"
    (swap! state assoc
           :nest-id "45356038"
           :ants {"44003385" {:type :ant, :location [0 0], :got-food false,
                              :team "iron-ant-tyson", :nest "45356038", :n 1,
                              :id   "44003385", :surroundings {"n" [], "w" [], "s" [], "sw" [], "ne" [], "e" [], "nw" [], "se" []}}})

    (with-redefs [call (constantly {:type :ant, :location [0 -1], :got-food false,
                                    :team "iron-ant-tyson2", :nest "45356038", :n 1,
                                    :id   "44003385", :surroundings
                                          {"n"  [], "w" [], "s"
                                                [{:type :nest, :location [0 0], :food 4, :team "micah", :id "21758968"}
                                                 {:type :nest, :location [0 0], :food 5, :team "iron-ant-tyson", :id "53721052"}
                                                 {:type :nest, :location [0 0], :food 4, :team "iron-ant-tyson2", :id "45356038"}],
                                           "sw" [], "ne" [], "e" [], "nw" [], "se" []}})]
      (let [result (go "44003385" "n")]
        (should= "44003385" result)
        (should= [0 -1] (get-in @state [:ants "44003385" :location])))))

  (it "processes surroundings"
    (process-surroundings {:location     [0 -1]
                           :surroundings {"n"  [], "w" [],
                                          "s"  [{:type :nest, :location [0 0], :food 4, :team "micah", :id "21758968"}
                                                {:type :food, :location [5 6], :id "53721052"}],
                                          "sw" [], "ne" [], "e" [], "nw" [], "se" []}})
    (should= :food (get-in @state [:foods "53721052" :type])))

  (it "turns the compass"
    (should= {"n"  [0 -1]
              "ne" [1 -1]
              "e"  [1 0]
              "se" [1 1]
              "s"  [0 1]
              "sw" [-1 1]
              "w"  [-1 0]
              "nw" [-1 -1]}
             (directional-coordinates [0 0])))

  (it "processes surroundings when food goes missing"
    (swap! state assoc-in [:foods "123"] {:type :food :location [0 0] :id "123"})
    (process-surroundings {:location     [0 -1]
                           :surroundings {"n" [], "w" [], "s" [], "sw" [], "ne" [], "e" [], "nw" [], "se" []}})
    (should= nil (get-in @state [:foods "123"])))

  (it "calculates direction to destination"
    (should= "n" (direction-to [0 0] [0 -1]))
    (should= "n" (direction-to [-5 5] [-5 -5]))
    (should= "s" (direction-to [0 -1] [0 0]))
    (should= "s" (direction-to [-5 -5] [-5 5]))
    (should= "e" (direction-to [0 0] [1 0]))
    (should= "e" (direction-to [-5 -5] [5 -5]))
    (should= "w" (direction-to [1 0] [0 0]))
    (should= "w" (direction-to [5 -5] [-5 -5]))
    (should= "ne" (direction-to [-5 5] [5 -5]))
    (should= "ne" (direction-to [-1 1] [0 0]))
    (should= "se" (direction-to [-1 -1] [0 0]))
    (should= "se" (direction-to [-5 -5] [5 5]))
    (should= "nw" (direction-to [5 5] [-5 -5]))
    (should= "nw" (direction-to [1 1] [0 0]))
    (should= "sw" (direction-to [1 -1] [0 0]))
    (should= "sw" (direction-to [5 -5] [-5 5]))
    (should= nil (direction-to [0 0] [0 0])))

  (it "searches for food when none has been found"
    (swap! state assoc
           :nest-id "45356038"
           :ants {"44003385" {:id "44003385" :type :ant :location [0 0] :got-food false}})
    (with-redefs [random-location (constantly [6 6])]
      (let [dir (ant-tick "44003385")]
        (should= [6 6] (get-in @state [:ants "44003385" :food-guess]))
        (should= "se" dir))))

  (it "uses food guess when searching"
    (swap! state assoc
           :nest-id "45356038"
           :ants {"44003385" {:id "44003385" :type :ant :location [0 0] :got-food false :food-guess [6 6]}})
    (let [dir (ant-tick "44003385")]
      (should= [6 6] (get-in @state [:ants "44003385" :food-guess]))
      (should= "se" dir)))

  (it "picks new location when at guess"
    (swap! state assoc
           :nest-id "45356038"
           :ants {"44003385" {:id "44003385" :type :ant :location [6 6] :food-guess [6 6] :got-food false}})
    (with-redefs [random-location (constantly [-4 -4])]
      (let [dir (ant-tick "44003385")]
        (should= [-4 -4] (get-in @state [:ants "44003385" :food-guess]))
        (should= "nw" dir))))

  (it "goes to nearest food when some exists"
    (swap! state assoc
           :nest-id "45356038"
           :ants {"44003385" {:id "44003385" :type :ant :location [0 0] :food-guess [6 6] :got-food false}}
           :foods {"12345" {:id "12345" :type :food :location [-9 -9]}})
    (let [dir (ant-tick "44003385")]
      (should= "nw" dir)))

  (it "hunters never stop hunting"
    (swap! state assoc
           :nest-id "45356038"
           :ants {"44003385" {:id "44003385" :type :ant :hunter? true :location [0 0] :got-food false}}
           :foods {"12345" {:id "12345" :type :food :location [-9 -9]}})
    (with-redefs [random-location (constantly [4 4])]
      (let [dir (ant-tick "44003385")]
        (should= [4 4] (get-in @state [:ants "44003385" :food-guess]))
        (should= "se" dir))))

  (it "goes home when has food"
    (swap! state assoc
           :nest-id "45356038"
           :ants {"44003385" {:got-food true :id "44003385" :type :ant :location [-4 3] :food-guess [6 6]}}
           :foods {"12345" {:id "12345" :type :food :location [-9 -9]}})
    (let [dir (ant-tick "44003385")]
      (should= "ne" dir)))

  (it "knows when to spawn"
    (swap! state assoc :nest {:id "45356038" :ants 0 :food 5})
    (should= true (spawn?))
    (swap! state assoc :nest {:id "45356038" :ants 5 :food 0})
    (should= false (spawn?))
    (swap! state assoc :nest {:id "45356038" :ants 5 :food 20})
    (should= true (spawn?))
    (swap! state assoc :nest {:id "45356038" :ants 50 :food 30})
    (should= false (spawn?)))
  )
