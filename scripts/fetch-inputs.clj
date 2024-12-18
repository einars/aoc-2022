#!/usr/bin/env bb

(require '[babashka.http-client :as http])
(require '[babashka.fs :as fs])

(import java.time.LocalDate)

(def session-id (first *command-line-args*))

(when-not session-id
  (println (format "Usage: %s [SESSION-ID]" *file*))
  (System/exit 77))

; "/proj/aoc"
(def basedir 
  (.toString (.getParent (.getParent (.normalize (fs/path *file*))))))

(let [this-year (.getYear (LocalDate/now))
      this-date (.getDayOfMonth (LocalDate/now))]
  (doseq [year (range 2022 2025)]
    (doseq [day (range 1 25)]
      (let [url (format "https://adventofcode.com/%d/day/%d/input" year day)
            dir  (format "%s/resources/%d/" basedir year)
            file (format "%s/resources/%d/day%d.txt" basedir year day)]
        (when (or (< year this-year) (<= day this-date))
          (when-not (fs/exists? file)
            (let [input (-> (http/get url 
                          {:headers 
                          {:cookie (format "session=%s" session-id)}})
                :body)]
              (prn file)
              (fs/create-dirs dir)
              (spit file input))))))))

