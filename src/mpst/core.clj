(ns mpst.core
  (:require [markdown.core :as markdown]
            [clj-time.core :as time]
            [clj-time.format :as fmt]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [hiccup.page :refer [html5]]
            [mpst.server :as server])
  (:import [java.io File] [java.nio.file Path Paths StandardWatchEventKinds FileSystems]))

(defn clean-public-dir []
  (println "🧹 Cleaning public directory...")
  (let [public-dir (io/file "public")]
    (when (.exists public-dir)
      (doseq [file (reverse (file-seq public-dir))]
        (when (.exists file)
          (.delete file))))))

(defn create-file-watcher [dir callback]
  (let [path (.toPath (io/file dir))
        watch-service (.newWatchService (FileSystems/getDefault))
        events [StandardWatchEventKinds/ENTRY_CREATE
                StandardWatchEventKinds/ENTRY_DELETE
                StandardWatchEventKinds/ENTRY_MODIFY]]
    (.register path watch-service (into-array events))
    (future
      (try
        (while true
          (let [key (.take watch-service)]
            (doseq [event (.pollEvents key)]

              (callback (.context event)))

            (.reset key)))
        (catch InterruptedException _ (println "File watcher stopped"))))))

;; Ensure directory exists
(defn ensure-directory-exists [dir]
  (let [file (File. dir)]
    (when-not (.exists file)
      (.mkdirs file))))

(defn sanitize-filename [filename]
  (-> filename
      (str/replace #"\.md$" "")
      (str/replace #"[^\w\s-]" "")
      (str/trim)
      (str/replace #"\s+" "-")
      str/lower-case))

(defn sanitize-title [filename]
  (-> filename
      (str/replace #"\.md$" "")
      (str/replace #"-" " ")
      str/trim
      str/capitalize))

(defn read-markdown-file [file]
  (try
    (let [content (slurp file)
          filename (sanitize-filename (.getName file))
          default-title (sanitize-title (.getName file))
          front-matter-pattern #"(?s)^---\n(.*?)\n---\n(.*)"  ; (?s) makes . match newlines too
          match (re-find front-matter-pattern content)]
      (if match
        (let [[_ front-matter body] match
              metadata (try
                         (read-string (str "{" front-matter "}"))
                         (catch Exception e
                           {:title default-title
                            :date (time/now)
                            :slug filename}))]
          {:metadata (update metadata :slug #(or % filename))
           :content (or body "")})
        {:metadata {:title default-title
                    :date (time/now)
                    :slug filename}
         :content content}))
    (catch Exception e
      (println "Error reading" (.getName file) "-" (.getMessage e))
      {:metadata {:title (sanitize-title (.getName file))
                  :date (time/now)
                  :slug (sanitize-filename (.getName file))}
       :content ""})))

(defn process-links [content]
  (let [link-pattern #"\[([^\]]+)\]\(([^)]+)\)"
        processed (str/replace content link-pattern
                               (fn [[_ text url]]
                                 (if (str/starts-with? url "/posts/")
                                   (str "[" text "](" url ")")
                                   (format "<a href=\"%s\" target=\"_blank\">%s</a>" url text))))]

    processed))

;; Function to convert Markdown to HTML
(defn markdown->hiccup [md]
  (let [processed-content (process-links md)
        html-content (markdown/md-to-html-string processed-content)]
    html-content))

(defn format-date [date]
  (let [datetime (if (string? date)
                   (fmt/parse (fmt/formatter "yyyy-MM-dd") date)
                   date)]
    {:datetime (fmt/unparse (fmt/formatter "yyyy-MM-dd") datetime)
     :display (fmt/unparse (fmt/formatter "MMMM d, yyyy") datetime)}))

;; Function to generate a blog post page using Hiccup
(defn blog-post-page [{:keys [title date _slug content]}]
  (let [formatted-date (format-date date)]
    (html5
     [:head
      [:title (str title)]  ; Ensure title is a string
      [:meta {:charset "UTF-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
      [:style "body { max-width: 800px; margin: 0 auto; padding: 1rem; font-family: system-ui; line-height: 1.5; } .post-date { color: #666 margin-bottom: 2rem}"]]
     [:body
      [:header
       [:h1 (str title)]  ; Ensure title is a string
       [:time {:class "post-date" :datetime (:datetime formatted-date)} (:display formatted-date)]]
      [:main
       [:article
        [:div {:class "content"}
         [:div content]]]]])))  ; Ensure content is a string

;; Helper function to check if a file is a markdown file
(defn markdown-file? [^File file]
  (and (.isFile file)  ; Make sure it's a file, not a directory
       (.endsWith (.getName file) ".md")))

;; Function to process all Markdown files and return a list of Hiccup pages
(defn process-posts []
  (let [posts-dir "content/posts/"
        dir (io/file posts-dir)]
    (when-not (.exists dir)
      (throw (IllegalStateException.
              (str "Posts directory does not exist: " posts-dir))))

    (->> (file-seq dir)
         (filter markdown-file?)
         (map (fn [file]
                (let [{:keys [metadata content]} (read-markdown-file file)]
                  {:metadata metadata :content
                   (markdown->hiccup content)}))))))

;; Function to write Hiccup data to an HTML file
(defn write-html [path content]
  (spit path (html5 content)))

;; Function to generate static HTML files for all posts
(defn generate-static-files []
  (ensure-directory-exists "public/posts/")
  (let [posts (process-posts)]
    (doseq [post posts]
      (try
        (let [metadata (:metadata post)
              content (:content post)
              slug (or (:slug metadata) (str/replace (:title metadata) #"\s+" "-"))
              file-path (str "public/posts/" slug ".html")]
          (println "Generating" file-path)
          (write-html file-path (blog-post-page (assoc metadata :content content))))
        (catch Exception e
          (println "Error generating file for post:" post)
          (println "Error:" (.getMessage e)))))))

;; Ensure static files are generated before starting the server
(defn ensure-generated []
  (when-not (->> "public/posts/" io/file .list seq)
    (generate-static-files)))

(defn handle-file-change [event]
  (println "Detected change in:" event)
  (clean-public-dir)
  (generate-static-files))

;; Entry point to start the server and ensure static files are generated
(defn dev-mode []
  (println "Starting development mode...")
  (clean-public-dir)
  (generate-static-files)
  (let [content-watcher (create-file-watcher "content/posts"
                                             (fn [_]
                                               (println "Rebuilding site...")
                                               (handle-file-change _)))]
    (try
      (server/-main)
      (catch Exception e
        (println "Error in development mode:" (.getMessage e))
        (future-cancel content-watcher)))))

;; Enhanced main function with development mode support
(defn -main [& args]
  (try
    (ensure-directory-exists "content/posts")
    (ensure-directory-exists "public/posts")

    (if (some #{"--dev"} args)
      (dev-mode)
      (do
        (println "Generating static files...")
        (clean-public-dir)
        (generate-static-files)
        (println "Starting server...")
        (server/-main)))
    (catch Exception e
      (println "Error starting application:" (.getMessage e))
      (System/exit 1))))
