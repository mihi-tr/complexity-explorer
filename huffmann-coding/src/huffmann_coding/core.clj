(ns huffmann-coding.core)

(defn char-frequencies
  "Calculates the frequencies of characters in a string"
  [s]
  (reduce (fn [x y] 
            (assoc x y (+ (get x y 0) 1)))
          {}
          (split-chars s)))

(defn split-chars
  "splits a string in characters"
  [s]
  (into [] s))

(defn to-forest
  "converts a frequency map to a base forest"
  [m]
  (loop [f [] r m]
  (if (first r)
    (recur (conj f {:char (first (first r))
                    :freq (second (first r))})
           (rest r))
    f)))

(defn sort-trees
  "sorts the trees in a forrest"
  [f]
  (sort (fn [x y] (- (:freq x) (:freq y)))
        f))

(defn collapse-trees
  "collapses two trees into one"
  [x y]
  {:freq (+ (:freq x) (:freq y))
   :children [x y]})

(defn build-tree
  "builds the binary tree from the forest"
  [f]
  (loop [r f]
    (if (second r)
      (recur (sort-trees (cons
                          (collapse-trees (first r)
                                          (second r))
                          (drop 2 r))))
      (first r))))
    

(defn build-tree-from-string
  "build a tree from a string"
  [s]
  (build-tree (to-forest (char-frequencies s))))

(defn join-maps
  "join two maps"
  [x y]
  (loop [r x m y]
    (if (first m)
      (recur (assoc r (first (first m)) (second (first m)))
             (rest m))
      r)))

(defn build-map
  "builds a map from a tree"
  ([t]
     (reduce (fn [x y]
               (assoc x (first y) (second y)))
             {}
      (map (fn [x] [(first x) (subs (second x) 1)])
           (build-map t "0"))))
  ([t p]
     (if (:char t)
       {(:char t) p}
       (join-maps
        (build-map (first (:children t)) (str p "0"))
        (build-map (second (:children t)) (str p "1"))
       ))))

(defn build-map-from-string
  "build a map from a string"
  [s]
  (build-map (build-tree-from-string s)))

(defn encode
  "encodes a string with a map"
  [s m]
  (map (fn [x] (get m x))
       (into [] s)))

(defn encode-string
  "encodes a string"
  [s]
  (let [m (build-map-from-string s)]
  {:map m
   :content (encode s m)
   }))

(defn bit-length
  "calculates the average bit-length for a string"
  [s]
  (let [m (build-map-from-string s)]
        (/ (reduce + (map (fn [x] (count (second x))) m))
           (count m))))

(defn shannon-information
  "calculates shannon information for a string"
  [s]
  (let [cs (count s)]
    (* -1 
       (reduce +
               (map (fn [x] 
                      (let [p (/ x cs)]
                        (* p (/ (Math/log p) (Math/log 2)))))
                    (vals (char-frequencies s)))))))

(defn compare-sh
  "compares shannon information and huffmann-coding bitlength for a string"
  [s]
  {:shannon (shannon-information s)
   :huffmann (float (bit-length s))
   :sentence s})

(defn do-comparison
  "compare text of a file"
  ([file]
  (map compare-sh (clojure.string/split (slurp file) #"\.")))
  ([file outfile]
     (with-open [w (java.io.FileWriter. outfile)]
       (doseq [x (do-comparison file)] 
         (.write w (str 
                    (clojure.string/join "," 
                        [(str \" (:sentence x) \")
                   (:shannon x)
                   (:huffmann x)]) 
                    "\n"))))))

