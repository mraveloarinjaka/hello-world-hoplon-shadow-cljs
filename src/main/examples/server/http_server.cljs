(ns examples.server.http-server
  (:require [lambdaisland.glogi :as log]
            [lambdaisland.glogi.console :as glogi-console]
            [macchiato.server :as http]))

(glogi-console/install!)

(def SERVER (atom nil))

(defn- handler
  [request callback]
  (callback {:status 200
             :body "Welcome to the world of Macchiato!"}))

(defn- server []
  (log/info :server {:message "starting server"})
  (let [host "127.0.0.1"
        port 4000]
    (http/start
      {:handler    handler
       :host       host
       :port       port
       :on-success #(log/info :server {:message (str "macchiato-test started on " host ":" port)})})))

(defn start
  []
  (log/info :start {:message "starting"})
  (reset! SERVER (server)))

(defn stop
  [done]
  (log/info :stop {:message "stopping"})
  (.close (deref SERVER)
          (fn [_]
            (do
              (log/info :stop {:message (str "stopped")})
              (done))))
  (reset! SERVER nil))

(defn main
  [& cli-args]
  (start))

