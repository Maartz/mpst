(ns mpst.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.response :refer [response file-response content-type]]
            [clojure.java.io :as io]))

(defn wrap-error-handling [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (println "Error handling request:" (.getMessage e))
        {:status 500
         :headers {"Content-Type" "text/html"}
         :body (str "<h1>Error</h1><p>" (.getMessage e) "</p>")}))))

(defn app [request]
  (let [uri (or (:uri request) "/")
        file-path (str "public" uri)]
    (if (.exists (io/file file-path))
      (-> (file-response file-path)
          (content-type "text/html"))
      {:status 404
       :headers {"Content-Type" "text/html"}
       :body "<h1>Page Not Found</h1>"})))

(defn start-server [port]
  (try
    (let [server (jetty/run-jetty (wrap-error-handling app)
                                  {:port port :join? false})]
      (println "Server started on port" port)
      server)
    (catch java.net.BindException e
      (println "Port" port "is in use. Trying port" (inc port))
      (start-server (inc port)))))

(defn -main []
  (start-server 3000))
