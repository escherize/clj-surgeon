(ns clj-surgeon.outline
  "Parse a Clojure file and return structured outline of all top-level forms.
   For CLJC files (and any file containing reader conditionals), forms inside
   #?(:clj ...) / #?@(:cljs [...]) are surfaced too, each tagged with the
   platforms it applies to."
  (:require [rewrite-clj.zip :as z]
            [clj-surgeon.forms :as forms]
            [clj-surgeon.cljc.walk :as cwalk]
            [clojure.string :as str]))

(defn- skip-return-schema
  "If child is the malli `:-` keyword, skip it and the schema node that follows."
  [child]
  (if (and child (= ":-" (z/string child)))
    (some-> child z/right z/right)
    child))

(defn- extract-arglist
  "Get arglist from a defn-shaped form. Handles plain defn, mu/defn (with `:-`
   return schemas), and multi-arity forms (where the arglist is inside an arity
   list)."
  [zloc type-str ctx]
  (when (forms/arglist-shaped? type-str ctx)
    (loop [child (some-> zloc z/down z/right z/right)]
      (when child
        (let [child (skip-return-schema child)]
          (cond
            (nil? child) nil
            (z/vector? child) (z/string child)
            (and (z/list? child)
                 (some-> child z/down z/vector?))
            (-> child z/down z/string)
            :else (recur (z/right child))))))))

(defn- preceding-comments
  "Look backwards from a form's start line to find attached comment lines.
   Comments must be contiguous (no blank lines between them and the form)."
  [lines form-line]
  (let [idx (dec form-line)] ;; 0-indexed
    (loop [i (dec idx), comment-start form-line]
      (if (neg? i)
        comment-start
        (let [line (str/trim (nth lines i ""))]
          (if (str/starts-with? line ";")
            (recur (dec i) (inc i)) ;; 1-indexed line number
            comment-start))))))

(defn- file-extension [file]
  (let [s (str file)
        i (.lastIndexOf s ".")]
    (when (pos? i) (subs s (inc i)))))

(defn outline
  "Return outline of all top-level forms in a Clojure file.
   Returns EDN map with :ns, :file, :lines, :forms, :forward-refs.

   Discovery: walks up from the file's directory looking for two optional
   config files:
   - .clj-surgeon.edn   — :def-forms, :arglist-forms (string sets matching
                          literal source text)
   - .clj-kondo/config.edn — :lint-as and :hooks/:analyze-call are merged.
                          Macros whose target is a defining form (def, defn,
                          defn-, deftype, ..., def-catch-all) are recognized
                          via the file's :require aliases and :refer'd names.

   Each form includes :platforms — the set of platforms (#{:clj}, #{:cljs},
   #{:clj :cljs}, etc.) under which it appears. For .clj/.cljs files this
   reflects the file extension; for .cljc files it surfaces reader-conditional
   structure, so a `#?(:clj (defn foo ...))` shows up as a real form with
   :platforms #{:clj}."
  [file]
  (let [source (slurp file)
        lines (str/split-lines source)
        total-lines (count lines)
        zloc (z/of-string source {:track-position? true})
        ctx (forms/classifier-for-file file zloc)
        ext (file-extension file)
        defaults (cwalk/platforms-for-extension ext)
        walked (cwalk/top-level-forms source defaults)
        forms-acc
        (mapv (fn [{:keys [zloc platforms]}]
                (let [node (z/node zloc)
                      m (meta node)
                      type-str (some-> zloc z/down z/string)
                      name-str (when (forms/defining? type-str ctx)
                                 (forms/extract-name zloc))
                      arglist (when name-str (extract-arglist zloc type-str ctx))
                      form-line (:row m)
                      comment-start (when form-line
                                      (preceding-comments lines form-line))]
                  (cond-> {:type (symbol (or type-str "?"))
                           :platforms (vec (sort platforms))}
                    form-line (assoc :line form-line)
                    (:end-row m) (assoc :end-line (:end-row m))
                    name-str (assoc :name (symbol name-str))
                    arglist (assoc :args arglist)
                    (and form-line comment-start (< comment-start form-line))
                    (assoc :comment-start comment-start))))
              walked)
        ns-name (some-> zloc
                        (z/find-value z/next 'ns)
                        z/up
                        z/down
                        z/right
                        z/string
                        symbol)]
    {:ns ns-name
     :file file
     :lines total-lines
     :form-count (count (filter :name forms-acc))
     :forms (vec (remove #(= 'ns (:type %)) forms-acc))
     :forward-refs []}))
