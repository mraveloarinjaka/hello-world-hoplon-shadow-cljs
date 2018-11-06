(ns com.murex.nostro.jenkins
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<! >!] :as async]
            [clojure.string :as string]
            [com.rpl.specter :as specter]
            ["c3" :as c3]))

(def BASE-URL "http://cje.fr.murex.com/for-mercury/job/NFR/job/nostro-safety-net/")

(def REQUIRED-ARTIFACTS {:realtime "realtime.json" :initial-load "initial_load.json"})
(def REQUIRED-ARTIFACTS-REVERSE-LOOKUP (zipmap (vals REQUIRED-ARTIFACTS) (keys REQUIRED-ARTIFACTS)))

(defn- ->api-call-url [url]
  (str url "api/json"))

(defn- retrieve-builds
  [response]
  (->> (get-in response [:body :builds])
       (sort-by #(get % :number))
       (drop 20)
       (map #(select-keys % [:number :url]))
       ))

(defn- retrieve-artifacts
  [response]
  (->> (get-in response [:body :artifacts])
       (map #(select-keys % [:fileName :relativePath]))
       (filter #((set (vals REQUIRED-ARTIFACTS)) (:fileName %)))
       ))

(defn- retrieve-artifact-url
  [base-url artifact]
  (let [artifact-key (get REQUIRED-ARTIFACTS-REVERSE-LOOKUP (:fileName artifact))]
    {artifact-key (str base-url "artifact/" (:relativePath artifact))}))

(defn- retrieve-artifacts-url
  [base-url artifacts]
  (->> artifacts
       (map (partial retrieve-artifact-url base-url))
       (reduce into {})))

(defn- camelize-string
  [^String s]
  (when s
    (some-> s
            (string/replace #"[_\s]+(.)?"
                                    (fn [[match c]]
                                      (if c (string/upper-case c) ""))))))

(defn- camelize-keys
  [m]
  (let [f (fn [[k v]] [(keyword (camelize-string (name k))) v])]
    ;; only apply to maps
    (clojure.walk/postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn- retrieve-artifacts-content
  [artifact-urls]
  (go-loop [urls artifact-urls artifacts {}]
           (let [[artifact-key artifact-url] (first urls)
                 remainings (rest urls)
                 artifact (<! (http/get artifact-url))
                 result (into artifacts {artifact-key (camelize-keys (get artifact :body {})) })]
             (if (seq remainings)
               (recur remainings result)
               result))))

(defn- retrieve-data []
  (let [builds (async/chan)
        hydrated-with-artifacts (async/chan)
        hydrated-with-artifacts-content (async/chan)
        ]
    (go
      (let [response (<! (http/get (->api-call-url BASE-URL)))]
        (async/onto-chan builds (retrieve-builds response))
        ))
    (go-loop []
             (when-let [build (<! builds)]
               ;(prn build)
               (let [response (<! (http/get (->api-call-url (:url build))))
                     artifacts (retrieve-artifacts response)]
                 (when (seq artifacts)
                   (let [hydrated-build (assoc build :artifacts-url (retrieve-artifacts-url (:url build) artifacts))]
                     (async/put! hydrated-with-artifacts hydrated-build)))
                 (recur)
                 ))
             (async/close! hydrated-with-artifacts))
    (go-loop []
             (when-let [build (<! hydrated-with-artifacts)]
               (let [artifacts-content (<! (retrieve-artifacts-content (:artifacts-url build)))
                     hydrated-build (assoc build :artifacts-content artifacts-content)]
                 (async/put! hydrated-with-artifacts-content hydrated-build))
               (recur))
             (async/close! hydrated-with-artifacts-content))
    hydrated-with-artifacts-content
    ))

(def LABELS {:realtime {:NostroSecurityTradeDate "security trade date"
                        :NostroCashTradeDate "cash trade date"
                        :NostroSecurityValueDate "security value date"
                        :NostroCashValueDate "cash value date"}
             :initial-load {:SEC-TD-5D "security trade date"
                            :CASH-TD-5D "cash trade date"
                            :SEC-VD-5D "security value date"
                            :CASH-VD-5D "cash value date"}})

(def COLLECT-KEY-THEN-CONTINUE [specter/ALL (specter/collect-one specter/FIRST) specter/LAST])

(def PATH-TO-REALTIME [(specter/collect-one :number)
                       :artifacts-content :realtime
                       COLLECT-KEY-THEN-CONTINUE
                       (specter/submap [:reference :reached])
                       COLLECT-KEY-THEN-CONTINUE])

(def PATH-TO-INITAL-LOAD [(specter/collect-one :number)
                          :artifacts-content :initial-load
                          COLLECT-KEY-THEN-CONTINUE
                          (specter/submap [:reference :reached])
                          COLLECT-KEY-THEN-CONTINUE])

(def PATH-TO-DATA {:realtime PATH-TO-REALTIME :initial-load PATH-TO-INITAL-LOAD})

(def REALTIME (atom []))
(def INITIAL-LOAD (atom []))

(defn generate-chart
  [anchor]
  (c3/generate (clj->js {:bindto anchor
                         :zoom {:enabled true}
                         :axis {:x {:type "category"
                                    :label "build"
                                    }
                                :y {:label "seconds"}
                                }
                         :data {:json {}}})))
(def REALTIME-CHART
  (generate-chart "#realtime"))

(def INITIAL-LOAD-CHART
  (generate-chart "#initial-load"))

(defn- ->label
  [k labels]
  (let [label-key (first (clojure.set/intersection k (set (keys labels))))]
    (str (get labels label-key) (if (get k :reference) " - references" ""))))

(defn- ->json-data
  [labels data-key data]
  (let [extracted-data (specter/select (get PATH-TO-DATA data-key) data)]
    (into {}
          (->> extracted-data
               (map (fn [[number projection data-type value]] {:build number (->label #{projection data-type} (get labels data-key)) value}))))))

(defn- all-data-available?
  [json-data]
  (= 9 (count json-data))
  true
  )

(defn- init-one-chart
  [{:keys [chart figures id data-key]}]
  [chart figures id data-key]
  (let [source (retrieve-data)]
    (reset! figures [])
    (go-loop []
             (when-let [data (<! source)]
               (let [json-data (->json-data LABELS data-key data)
                     data-set {:x "build"
                               :value (sort (vec (remove #(= :build %) (keys json-data))))}]
                 (when (all-data-available? json-data)
                   (swap! figures conj json-data)
                   (.load chart (clj->js {:json @figures
                                          :keys data-set
                                          :type "spline"
                                          }))))
               (recur)))))

(defn- ^{:dev/after-load true} init
  []
  (init-one-chart {:chart INITIAL-LOAD-CHART :figures INITIAL-LOAD :id "#initial-load" :data-key :initial-load})
  (init-one-chart {:chart REALTIME-CHART :figures REALTIME :id "#realtime" :data-key :realtime})
  )

(defn ^{:export true} main
  []
  (init))

#_ (def sample [{:number 34, :url "http://cje.fr.murex.com/for-mercury/job/NFR/job/nostro-safety-net/34/", :artifacts-url {:initial-load "http://cje.fr.murex.com/for-mercury/job/NFR/job/nostro-safety-net/34/artifact/results/initial-load.json", :realtime "http://cje.fr.murex.com/for-mercury/job/NFR/job/nostro-safety-net/34/artifact/results/realtime.json"}, :artifacts-content {:realtime {:NostroSecurityTradeDate {:reference "0.25", :passed true, :reached "0.20951381298380162"}, :NostroSecurityValueDate {:reference "0.38", :passed true, :reached "0.29523025554047044"}, :NostroCashTradeDate {:reference "0.32", :passed true, :reached "0.2258589652276808"}, :NostroCashValueDate {:reference "0.3", :passed true, :reached "0.22596379984980045"}}, :initial-load {:SEC-VD-5D {:reached "28.6699", :reference "33.0858", :passed true}, :SEC-TD-5D {:reached "29.1018", :reference "32.7955", :passed true}, :CASH-VD-5D {:reached "26.0449", :reference "33.3261", :passed true}, :CASH-TD-5D {:reached "28.6701", :reference "34.410", :passed true}}}}
                {:number 2, :url "http://cje.fr.murex.com/for-mercury/job/NFR/job/nostro-safety-net/2/", :artifacts-url {:initial-load "http://cje.fr.murex.com/for-mercury/job/NFR/job/nostro-safety-net/2/artifact/results/initial-load.json", :realtime "http://cje.fr.murex.com/for-mercury/job/NFR/job/nostro-safety-net/2/artifact/results/realtime.json"}, :artifacts-content {:realtime {:NostroSecurityValueDate {:passed false, :reference "0.19188691844110903", :reached "0.3495548863564768"}, :NostroCashTradeDate {:passed true, :reference "1.0016803993084185", :reached "0.9886337525066845"}, :NostroSecurityTradeDate {:passed false, :reference "0.39994744027015006", :reached "0.2273705034370882"}}, :initial-load {:CASH-TD-5D {:reference "34.410", :reached "33.7988", :passed true}, :SEC-TD-5D {:reference "32.7955", :reached "33.2557", :passed true}, :CASH-VD-5D {:reference "33.3261", :reached "32.5256", :passed true}, :SEC-VD-5D {:reference "33.0858", :reached "33.1952", :passed true}}}}])

#_ (->json-data LABELS :realtime (first sample))

#_ {#{:NostroSecurityTradeDate :reference} "0.25", #{:NostroSecurityTradeDate :reached} "0.20951381298380162", #{:NostroSecurityValueDate :reference} "0.38", #{:NostroSecurityValueDate :reached} "0.29523025554047044", #{:NostroCashTradeDate :reference} "0.32", #{:NostroCashTradeDate :reached} "0.2258589652276808", #{:NostroCashValueDate :reference} "0.3", #{:NostroCashValueDate :reached} "0.22596379984980045"}

#_ (def PATH-TO-REACHED [:artifacts-content :realtime specter/ALL (specter/collect-one specter/FIRST) specter/LAST :reached])
#_ (transduce (specter/traverse-all PATH-TO-REACHED)
              (completing (fn [result [k v]] (update result k #(conj (vec %) v))))
              {} sample)

#_ (def reached (specter/select (into [specter/all] path-to-reached) sample))
#_ (reduce (fn [result [k v]] (update result k #(conj (vec %) v))) {} reached)

#_ (specter/select [specter/ALL (specter/collect-one :number) PATH-TO-REALTIME] sample)

#_ (specter/select PATH-TO-REALTIME (first sample))


