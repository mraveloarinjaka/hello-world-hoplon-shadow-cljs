(ns examples.server.promises
  (:require [applied-science.js-interop :as j]
            [cljs.core.async :refer (go take!)]
            [cljs.core.async.interop :refer (<p!)]
            ["webdriverio" :refer (remote)]))

(enable-console-print!)

(defn- search
    [item]
    (go
      (try
        (let [browser (<p! (remote #js {:logLevel "trace"
                                        :capabilities #js {:browserName "chrome"}}))
              _ (j/call browser :url "https://duckduckgo.com")
              input-elem (<p! (j/call browser :$ "#search_form_input_homepage"))
              submit-btn (<p! (j/call browser :$ "#search_button_homepage"))]
          (<p! (j/call input-elem :setValue item))
          (<p! (j/call submit-btn :click))
          (<p! (j/call browser :getTitle)))
        (catch :default e
          (str e)))))

(defn main [& cli-args]
  (when-let [item (first cli-args)]
    (take! (search item)
           #(prn (str "title of the window: " %)))))

(defn start
  []
  (js/console.log "starting ...")
  (main))

(comment

  (main "WebdriverIO")

 )
