(ns mpst.core
  (:require [markdown.core :as markdown]
            [clj-time.core :as time]
            [clj-time.format :as fmt]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [hiccup.page :refer [html5]]
            [mpst.server :as server])
  (:import [java.io File]))

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

(defn read-markdown-file [file]
  (try
    (let [content (slurp file)
          filename (sanitize-filename (.getName file))
          front-matter-pattern #"(?s)^---\n(.*?)\n---\n(.*)"  ; (?s) makes . match newlines too
          match (re-find front-matter-pattern content)]
      (if match
        (let [[_ front-matter body] match
              metadata (try
                         (read-string (str "{" front-matter "}"))
                         (catch Exception e
                           {:title (.getName file)
                            :date (time/now)
                            :slug filename}))]
          {:metadata (update metadata :slug #(or % filename))
           :content (or body "")})
        {:metadata {:title (.getName file)
                    :date (time/now)
                    :slug filename}
         :content content}))
    (catch Exception e
      (println "Error reading" (.getName file) "-" (.getMessage e))
      {:metadata {:title (.getName file)
                  :date (time/now)
                  :slug (sanitize-filename (.getName file))}
       :content ""})))

(defn process-links [content]
  (let [link-pattern #"\[([^\]]+)\]\(([^)]+)\)"
        processed (str/replace content link-pattern
                               (fn [[_ text url]]
                                 (if (str/starts-with? url "/posts/")
                                   (str "[" text "](" url ")")
                                   (str "[" text "](" url "){:target=_blank}"))))]
    processed))

;; Function to convert Markdown to HTML
(defn markdown->hiccup [md]
  (-> md
      process-links
      markdown/md-to-html-string))

;; Function to generate a blog post page using Hiccup
(defn blog-post-page [{:keys [title date _slug content]}]
  (html5
   [:head
    [:title (str title)]  ; Ensure title is a string
    [:meta {:charset "UTF-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:style "body { max-width: 800px; margin: 0 auto; padding: 1rem; font-family: system-ui; line-height: 1.5; }"]]
   [:body
    [:header
     [:h1 (str title)]  ; Ensure title is a string
     [:time
      {:datetime (fmt/unparse (fmt/formatter "yyyy-MM-dd")
                              (if (string? date)
                                (fmt/parse (fmt/formatter "yyyy-MM-dd") date)
                                date))}
      (fmt/unparse (fmt/formatter "MMMM d, yyyy")
                   (if (string? date)
                     (fmt/parse (fmt/formatter "yyyy-MM-dd") date)
                     date))]]
    [:main
     [:article
      [:div {:class "content"}
       [:div content]]]]]))  ; Ensure content is a string

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

;; Entry point to start the server and ensure static files are generated
(defn -main []
  (try
    (ensure-directory-exists "content/posts")  ; Create posts directory if it doesn't exist
    (ensure-directory-exists "public/posts")   ; Create public directory if it doesn't exist
    (println "Generating static files...")
    (ensure-generated)
    (println "Starting server...")
    (mpst.server/-main)
    (catch Exception e
      (println "Error starting application:" (.getMessage e))
      (System/exit 1))))
