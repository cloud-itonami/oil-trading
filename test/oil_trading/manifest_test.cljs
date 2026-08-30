(ns oil_trading.manifest-test
  "Cross-file invariants: the manifest, the cljc boundary and the DID document
  are three descriptions of one actor, and nothing checks that they agree.

  These assertions were carried over from `actor-manifest.test.ts`, which has
  never run in this repo — there is no package.json and no vitest here, so the
  suite it belongs to does not exist. ADR-2608260900 retires .ts/.tsx as an
  authoring surface, so they are re-expressed in cljs rather than revived, and
  the checks the TypeScript file could not make (manifest against cell-specs)
  are added, since that is the drift that actually breaks the actor."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:path" :as path]
            [oil_trading.murakumo :as m]))

(def repo-root
  "The repo root. nbb does not define `js/__filename`, so this is the working
  directory: run the suite from the repo root. Reading is guarded below rather
  than assumed — a suite that quietly reads nothing must not be able to pass."
  (path/resolve (js/process.cwd)))

(defn- read-json
  "Parse a tracked file, or refuse. Not finding the file is a third answer,
  not a clean one — see `run_tests.cljs` for why a pass has a floor under it."
  [rel]
  (let [p (path/join repo-root rel)]
    (when-not (fs/existsSync p)
      (throw (ex-info (str "refusing to report a pass: " rel " is not readable from "
                           repo-root " — run this suite from the repo root")
                      {:path p})))
    (js->clj (js/JSON.parse (fs/readFileSync p "utf8")))))

(def manifest (read-json "actor-manifest.jsonld"))
(def did-doc (read-json ".well-known/did.json"))

(def capability-vocabulary
  "The vocabulary the manifest may draw from. Carried over verbatim from the
  `VP` set in actor-manifest.test.ts."
  #{"graph.query" "graph.write" "graph.vectorSearch" "agent.chat" "agent.invoke"
    "identity.resolve" "browser.fetch" "signal.encrypt" "consent.check"
    "derive:social" "dmn.evaluate" "form.collect"})

(defn- pipelines-of [kind]
  (filter #(= kind (get-in % ["trigger" "type"])) (get manifest "pipelines")))

(defn- nsid->legacy-cell
  "The manifest names an endpoint in dotted NSID form; cell-specs names the same
  endpoint in dashed legacy-cell form. This is the whole mapping between them."
  [nsid]
  (str/replace nsid "." "-"))

(def legacy-cells (set (map :legacy-cell (vals m/cell-specs))))

;; ── the manifest describes the actor this code implements ───────────────────

(deftest manifest-and-cljc-boundary-name-the-same-actor
  (is (= (get manifest "@id") m/actor-did)
      "the DID in the manifest and the DID the effects are signed with must agree"))

(deftest manifest-header-is-well-formed
  (is (= "https://etzhayyim.com/ns/actor/v1" (get manifest "@context")))
  (is (= "did:web:oil-trading.etzhayyim.com" (get manifest "@id")))
  (is (= "k8s-langserver" (get manifest "runtime")))
  (is (= "01ltrad3" (get manifest "nanoid")))
  (is (= "oil-trading" (get manifest "name"))))

(deftest did-document-is-readable-and-self-consistent
  (let [id (get did-doc "id")]
    (is (string? id))
    (is (str/starts-with? id "did:web:"))
    (is (every? #(str/starts-with? % (str id "#"))
                (map #(get % "id") (get did-doc "service")))
        "every service id must be a fragment of the document's own DID")))

(deftest every-declared-capability-is-in-the-vocabulary
  (let [caps (get manifest "capabilities")]
    (is (seq caps) "an actor with no capabilities would make this check vacuous")
    (doseq [c caps]
      (is (contains? capability-vocabulary c) (str c " is not a known capability")))))

(deftest no-pipeline-step-escapes-into-a-custom-function
  (let [steps (mapcat #(get % "steps") (get manifest "pipelines"))]
    (is (seq steps))
    (doseq [s steps]
      (is (not= "custom" (get s "fn"))
          (str "step " (get s "id") " must use a declared capability, not fn:custom"))
      (is (contains? capability-vocabulary (get s "fn"))
          (str "step " (get s "id") " uses " (get s "fn")
               ", which is outside the capability vocabulary")))))

(deftest pipeline-inventory-is-what-the-actor-was-built-with
  (is (= 8 (count (get manifest "pipelines"))))
  (is (= 4 (count (get manifest "actors"))))
  (let [cron (first (filter #(= "0 */8 * * *" (get-in % ["trigger" "cron"]))
                            (pipelines-of "cron")))]
    (is (some? cron) "the eight-hourly reporting pipeline must exist")
    (is (= 5 (count (get cron "steps"))))
    (is (= "counterpartyStats" (get (nth (get cron "steps") 2) "id")))))

;; ── the manifest and the cell catalogue must cover each other ───────────────

(deftest every-xrpc-endpoint-has-a-cell-that-can-serve-it
  (let [nsids (map #(get-in % ["trigger" "nsid"]) (pipelines-of "xrpc"))]
    (is (= 5 (count nsids)) "five xrpc endpoints are declared")
    (doseq [n nsids]
      (is (contains? legacy-cells (nsid->legacy-cell n))
          (str "the manifest serves " n
               " but cell-specs has no cell named " (nsid->legacy-cell n))))))

(deftest every-subscribed-collection-has-a-cell-that-can-handle-it
  (let [colls (mapcat #(get-in % ["trigger" "collections"]) (pipelines-of "subscribeRepos"))]
    (is (seq colls) "the actor subscribes to at least one upstream collection")
    (doseq [c colls]
      (is (contains? legacy-cells (nsid->legacy-cell c))
          (str "subscribed to " c " with no cell to handle it")))))

(deftest the-documented-xrpc-endpoints-are-still-the-ones-declared
  (let [nsids (set (map #(get-in % ["trigger" "nsid"]) (pipelines-of "xrpc")))]
    (doseq [n ["com.etzhayyim.apps.oilTrading.book.getTrade"
               "com.etzhayyim.apps.oilTrading.book.listTrades"
               "com.etzhayyim.apps.oilTrading.book.listOfftakes"
               "com.etzhayyim.apps.oilTrading.analytics.getBenchmarkExposure"
               "com.etzhayyim.apps.oilTrading.health"]]
      (is (contains? nsids n) (str n " is no longer served")))))

(deftest cells-that-serve-the-manifest-are-gated-like-every-other-cell
  (testing "an endpoint reachable from outside must not have a shorter gate list"
    (let [served (set (map #(nsid->legacy-cell (get-in % ["trigger" "nsid"]))
                           (pipelines-of "xrpc")))]
      (is (seq served))
      (doseq [[k spec] m/cell-specs
              :when (contains? served (:legacy-cell spec))]
        (is (= (vec m/common-gates) (vec (:required-gates spec)))
            (str k " is externally reachable and must require every gate"))
        (is (= :blocked (:status (m/cell-plan k {})))
            (str k " must refuse to serve an unattested request"))))))

(deftest hazard-the-served-did-document-does-not-claim-the-did-this-actor-stamps
  ;; NOT a guarantee -- a recorded divergence.
  ;;
  ;; `.well-known/did.json` identifies itself as
  ;;   did:web:etzhayyim.com:actor:oil-trading
  ;; (commit f072718, "migrate did:web to etzhayyim.com scheme",
  ;;  ADR-2606231200 addendum 2026-07-02), while the planner and the manifest
  ;; both stamp
  ;;   did:web:oil-trading.etzhayyim.com
  ;; and the document does not list that DID in alsoKnownAs either -- only the
  ;; `at://` handle built from the same host. A DID document whose `id` is not
  ;; the DID being resolved is not a resolution of that DID, so nothing can
  ;; verify the identity these records are written under from what this repo
  ;; serves. The sibling business-person repo carries the identical split from
  ;; the identical commit, so this is the migration's shape, not a typo.
  ;;
  ;; Which side moves is an ownership decision, not this suite's. Pinned so
  ;; that whichever side moves, this test goes red and the other side has to
  ;; move with it. When they agree, delete this test and assert the equality.
  (is (= "did:web:etzhayyim.com:actor:oil-trading" (get did-doc "id"))
      "the served DID document changed -- reconcile it with m/actor-did and delete this test")
  (is (not= m/actor-did (get did-doc "id"))
      "the divergence is resolved -- replace this test with an equality assertion")
  (is (not (some #{m/actor-did} (get did-doc "alsoKnownAs")))
      "the document now acknowledges the stamped DID -- tighten this test"))
