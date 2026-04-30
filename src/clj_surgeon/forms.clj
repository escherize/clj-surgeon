(ns clj-surgeon.forms
  "Form classification: shared logic for deciding whether a top-level list
   is a defining form, what its name is, and whether it's the private variant.
   Used by outline.clj (for :ls output) and analyze.clj (for dep graphs,
   topological sort, extraction, etc.)."
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; ============================================================
;; Built-in form classification
;; ============================================================

(def def-types
  "Top-level defining forms recognised without any config."
  #{"def" "defn" "defn-" "defonce" "defmacro" "defmethod" "defmulti"
    "defprotocol" "defrecord" "deftype" "declare"
    ">defn" ">defn-"})

(def private-types
  "Built-in private defining forms."
  #{"defn-" ">defn-"})

(def kondo-defining-targets
  "Tail names that mean 'defining form' when seen as a lint-as / hooks
   target. Robust to whichever ns the user writes (clojure.core/defn vs
   schema.core/defn both qualify by tail-match)."
  #{"def" "defn" "defn-" "defonce" "defmacro" "defmethod" "defmulti"
    "defprotocol" "defrecord" "deftype" "def-catch-all"})

(def kondo-defining-pattern
  "Regex for lint-as / hooks targets that look like defining forms — covers
   defenterprise, defendpoint, define-multi-setting, lint-defn, etc.
   Excludes non-defs like deferred-trs, deferred-tru."
  #"^def[a-z]*$|^def-[a-z-]+$|^define-[a-z-]+$|^lint-def[a-z-]*$|^def-catch-all$")

(def kondo-arglist-targets
  "Tail names that mean 'this is defn-shaped' (arglist follows form-name,
   possibly with a malli :- return schema in between)."
  #{"defn" "defn-"})

(def defn-suffix-pattern
  "Aliased defn-style macros like mu/defn, s/defn, p/defn-, malli/defn."
  #".+/defn-?")

(def defn-private-suffix-pattern
  "Aliased private defn-style macros like mu/defn-, s/defn-."
  #".+/defn-")

;; ============================================================
;; Config discovery (walking up from a file's dir)
;; ============================================================

(defn- find-up
  "Walk up from start-dir looking for a relative path. Returns java.io.File or nil."
  [start-dir relative-path]
  (loop [dir (some-> start-dir io/file .getCanonicalFile)]
    (when dir
      (let [f (io/file dir relative-path)]
        (cond
          (.isFile f) f
          (.getParentFile dir) (recur (.getParentFile dir))
          :else nil)))))

(defn- safe-read-edn
  "Read EDN from a File, returning nil on any error."
  [f]
  (when f
    (try (edn/read-string (slurp f))
         (catch Exception _ nil))))

(defn find-surgeon-config
  "Walk up from start-dir looking for .clj-surgeon.edn. Returns parsed map or nil.
   Supported keys:
     :def-forms      string set — extra symbols to treat as defining forms
                     (matched against literal source text, e.g. \"my/defentity\")
     :arglist-forms  string set — extras with a first-vector arglist
                     (defaults to :def-forms)"
  [start-dir]
  (safe-read-edn (find-up start-dir ".clj-surgeon.edn")))

(defn find-kondo-defs
  "Walk up from start-dir looking for .clj-kondo/config.edn. Merges :lint-as
   and :hooks/:analyze-call into one map of fully-qualified-sym -> target-sym.
   Both clj-kondo mechanisms describe the same thing for our purposes — each
   entry tells us a macro's shape via a target whose name we inspect.
   Returns nil if no config is found."
  [start-dir]
  (when-let [parsed (some-> (find-up start-dir ".clj-kondo/config.edn") safe-read-edn)]
    (merge (:lint-as parsed)
           (get-in parsed [:hooks :analyze-call]))))

;; ============================================================
;; ns-form parsing — extract :as aliases and :refer'd symbol names
;; ============================================================

(defn- parse-refer-vector
  "Given a zipper at a :refer vector, return its symbol names as strings."
  [vec-zloc]
  (when (and vec-zloc (z/vector? vec-zloc))
    (loop [child (some-> vec-zloc z/down), names []]
      (if (nil? child)
        names
        (let [tag (n/tag (z/node child))]
          (if (or (= :token tag) (= :symbol tag))
            (recur (z/right child) (conj names (z/string child)))
            (recur (z/right child) names)))))))

(defn parse-ns-aliases
  "Walk the (ns ...) form's :require clauses. Returns
     {:aliases {\"alias\" \"fully.qualified.ns\"}
      :refers  {\"sym\"   \"fully.qualified.ns\"}}.
   Supports common shapes:
     [foo.bar :as fb]
     [foo.bar :as-alias fb]
     [foo.bar :refer [a b]]
     [foo.bar :refer [c] :as fb]"
  [zloc]
  (let [ns-form (some-> zloc (z/find-value z/next 'ns) z/up)]
    (or (when ns-form
          (loop [child (some-> ns-form z/down), aliases {}, refers {}]
            (if (nil? child)
              {:aliases aliases :refers refers}
              (let [s (z/string child)]
                (if (and (str/starts-with? s "(:require")
                         (= ":require" (some-> child z/down z/string)))
                  (let [[a r] (loop [entry (some-> child z/down z/right), a aliases, r refers]
                                (cond
                                  (nil? entry) [a r]
                                  (z/vector? entry)
                                  (let [ns-name (some-> entry z/down z/string)
                                        alias-loc (or (z/find-value entry z/next :as)
                                                      (z/find-value entry z/next :as-alias))
                                        alias-name (some-> alias-loc z/right z/string)
                                        refer-loc (z/find-value entry z/next :refer)
                                        refer-vec (some-> refer-loc z/right)
                                        refer-syms (parse-refer-vector refer-vec)]
                                    (recur (z/right entry)
                                           (cond-> a
                                             (and ns-name alias-name) (assoc alias-name ns-name))
                                           (reduce (fn [m sym] (assoc m sym ns-name)) r refer-syms)))
                                  :else (recur (z/right entry) a r)))]
                    (recur (z/right child) a r))
                  (recur (z/right child) aliases refers))))))
        {:aliases {} :refers {}})))

;; ============================================================
;; Symbol resolution — type-str → fully-qualified target via lint-as
;; ============================================================

(defn- canonicalize
  "Given a form-type string and ns-info ({:aliases :refers}), produce the
   fully-qualified symbol string. Handles two shapes:
     \"mu/defn\"  via aliases  -> \"metabase.util.malli/defn\"
     \"defsetting\" via refers -> \"metabase.settings.core/defsetting\"
   Returns nil if neither resolves."
  [type-str ns-info]
  (when (and type-str ns-info)
    (let [idx (str/index-of type-str "/")]
      (cond
        (and idx (pos? idx))
        (let [prefix (subs type-str 0 idx)
              suffix (subs type-str (inc idx))
              ns-name (get-in ns-info [:aliases prefix])]
          (when ns-name (str ns-name "/" suffix)))

        :else
        (when-let [ns-name (get-in ns-info [:refers type-str])]
          (str ns-name "/" type-str))))))

(defn- target-tail
  "Return the unqualified-name part of a fully-qualified symbol, as a string."
  [sym]
  (let [s (str sym)
        idx (str/index-of s "/")]
    (if idx (subs s (inc idx)) s)))

(defn- kondo-defining-target?
  "True if the lint-as target's tail name is in kondo-defining-targets, or
   matches kondo-defining-pattern (covers hook-style targets like
   hooks.foo/defenterprise)."
  [target-sym]
  (when target-sym
    (let [tail (target-tail target-sym)]
      (boolean
       (or (kondo-defining-targets tail)
           (re-matches kondo-defining-pattern tail))))))

(defn- lint-as-defining?
  "True if `type-str` resolves via the file's aliases/refers to a lint-as
   entry whose target is a defining-form symbol."
  [type-str ns-info lint-as]
  (when (and lint-as (seq lint-as))
    (let [canonical (canonicalize type-str ns-info)
          target (when canonical (get lint-as (symbol canonical)))]
      (kondo-defining-target? target))))

(defn- lint-as-arglist?
  "True if `type-str` resolves to a lint-as entry whose target's tail is
   defn or defn- — meaning we should look for an arglist."
  [type-str ns-info lint-as]
  (when (and lint-as (seq lint-as))
    (let [canonical (canonicalize type-str ns-info)
          target (when canonical (get lint-as (symbol canonical)))]
      (boolean (some-> target target-tail kondo-arglist-targets)))))

;; ============================================================
;; Public: classifier predicates
;; ============================================================

(defn defining?
  "True if a form with this type-str is a top-level defining form."
  [type-str {:keys [extra-defs ns-info lint-as]}]
  (boolean
   (or (contains? def-types type-str)
       (contains? extra-defs type-str)
       (and type-str (re-matches defn-suffix-pattern type-str))
       (lint-as-defining? type-str ns-info lint-as))))

(defn private?
  "True if a defining form is private. Detects:
   - built-in private types (defn-, >defn-)
   - aliased private (mu/defn-, s/defn-) via /defn- suffix
   - lint-as target whose tail is defn-
   Note: ^:private metadata on a (def ^:private foo ...) form is detected
   by extract-name elsewhere — that's about the form's *name*, not type."
  [type-str {:keys [ns-info lint-as]}]
  (boolean
   (or (contains? private-types type-str)
       (and type-str (re-matches defn-private-suffix-pattern type-str))
       (when (and lint-as (seq lint-as))
         (let [canonical (canonicalize type-str ns-info)
               target (when canonical (get lint-as (symbol canonical)))]
           (= "defn-" (some-> target target-tail)))))))

(defn arglist-shaped?
  "True if a form has a defn-shaped arglist (vector after form-name,
   possibly with malli :- return schema in between)."
  [type-str {:keys [extra-arglist ns-info lint-as]}]
  (boolean
   (or (contains? #{"defn" "defn-" ">defn" ">defn-"} type-str)
       (contains? extra-arglist type-str)
       (and type-str (re-matches defn-suffix-pattern type-str))
       (lint-as-arglist? type-str ns-info lint-as))))

(defn- unwrap-meta
  "If zloc is a :meta node, recursively walk to the rightmost child
   (the actual value being annotated). Handles nested meta:
     ^:private ^{:arglists '(...)} foo  -> 'foo'."
  [zloc]
  (loop [loc zloc]
    (if (and loc (= :meta (n/tag (z/node loc))))
      (recur (some-> loc z/down z/rightmost))
      loc)))

(defn extract-name
  "Get the name from the second child of a form. Handles metadata like
   ^:private and stacked meta like ^:private ^{:arglists '(...)}.
   Returns the name as a string, or nil."
  [zloc]
  (loop [child (some-> zloc z/down z/right)]
    (when child
      (let [tag (n/tag (z/node child))]
        (if (= :meta tag)
          (some-> child unwrap-meta z/string)
          (if (or (= :token tag) (= :symbol tag))
            (z/string child)
            (recur (z/right child))))))))

;; ============================================================
;; Public: ready-made classifier
;; ============================================================

(defn classifier-for-file
  "Build a classifier context for the given file path (may be nil). When file
   is non-nil, walks up to find .clj-surgeon.edn and .clj-kondo/config.edn.
   When nil, returns a minimal context using only the zloc's ns aliases —
   suffix matchers still work (mu/defn, s/defn, etc.).

   Returns a map {:ns-info :lint-as :extra-defs :extra-arglist} — pass this
   as the second argument to `defining?`, `private?`, `arglist-shaped?`."
  [file zloc]
  (let [start-dir (when file (or (.getParent (io/file file)) "."))
        surgeon-cfg (when start-dir (find-surgeon-config start-dir))
        extra-defs (set (:def-forms surgeon-cfg))
        extra-arglist (set (:arglist-forms surgeon-cfg extra-defs))
        lint-as (when start-dir (find-kondo-defs start-dir))
        ns-info (parse-ns-aliases zloc)]
    {:ns-info ns-info
     :lint-as lint-as
     :extra-defs extra-defs
     :extra-arglist extra-arglist}))
