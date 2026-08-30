(ns oil_trading.murakumo-test
  "Invariants of the pure cljc actor boundary.

  The one this suite exists for is fail-closed planning: `cell-plan` must not
  emit a single `:mst/put-record` effect unless every gate in `common-gates`
  is attested. Everything else here — the four attestation shapes, the rkey
  derivation, the spec catalogue — is a way that invariant can be lost
  quietly, so each is pinned by name rather than by the aggregate outcome."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [oil_trading.murakumo :as m]))

(def all-attested
  "Every gate the specs require, attested as a keyword-keyed map."
  (zipmap m/common-gates (repeat true)))

;; ── fail-closed planning ────────────────────────────────────────────────────

(deftest blocked-plan-emits-no-effects
  (testing "with no attestations at all, every cell is blocked and writes nothing"
    (doseq [cell (keys m/cell-specs)]
      (let [p (m/cell-plan cell {})]
        (is (= :blocked (:status p)) (str cell " should be blocked"))
        (is (= [] (:effects p)) (str cell " should carry no effects"))
        (is (not (contains? p :records))
            (str cell " should not carry planned records while blocked"))
        (is (= (vec m/common-gates) (:missing-gates p))
            (str cell " should report every gate as missing"))))))

(deftest withholding-any-single-gate-blocks
  (testing "each gate is load-bearing on its own, not just as part of the set"
    (doseq [withheld m/common-gates]
      (let [atts (zipmap (remove #{withheld} m/common-gates) (repeat true))
            p    (m/cell-plan :trade {:attestations atts})]
        (is (= :blocked (:status p))
            (str "withholding " withheld " should block the plan"))
        (is (= [withheld] (:missing-gates p))
            (str "withholding " withheld " should name exactly that gate"))
        (is (empty? (:effects p))
            (str "withholding " withheld " should emit no effects"))))))

(deftest false-attestation-does-not-satisfy-a-gate
  (testing "an attestation recorded as false is a refusal, not a signature"
    (doseq [falsey [false nil]]
      (let [atts (assoc all-attested :no-probing-baseline falsey)
            p    (m/cell-plan :trade {:attestations atts})]
        (is (= :blocked (:status p))
            (str "attestation value " (pr-str falsey) " should not pass the gate"))
        (is (= [:no-probing-baseline] (:missing-gates p)))
        (is (empty? (:effects p)))))))

(deftest fully-attested-plan-is-ready-and-emits-one-effect-per-collection
  (let [p (m/cell-plan :trade {:attestations all-attested
                               :computed-at "2026-08-31T00:00:00Z"
                               :request-id "req-1"})]
    (is (= :ready (:status p)))
    (is (= [] (:missing-gates p)))
    (is (= (count (:collections (get m/cell-specs :trade)))
           (count (:effects p)))
        "one effect per declared collection")
    (is (= (count (:effects p)) (count (:records p)))
        "records and effects are the same plan, counted twice")))

(deftest all-cell-plans-covers-every-cell-and-is-fail-closed
  (let [plans (m/all-cell-plans {})]
    (is (= (set (keys m/cell-specs)) (set (keys plans)))
        "no cell may be silently dropped from the fleet-wide plan")
    (is (every? #(= :blocked (:status %)) (vals plans)))
    (is (zero? (reduce + 0 (map #(count (:effects %)) (vals plans))))
        "an unattested actor writes nothing anywhere")))

;; ── how an attestation may be presented ─────────────────────────────────────

(deftest gate-value-reads-all-four-attestation-shapes
  (let [g :did-primary-baseline]
    (is (= 1 (m/gate-value {g 1} g))            "keyword-keyed map")
    (is (= 1 (m/gate-value {(name g) 1} g))     "string-keyed map")
    (is (= g (m/gate-value #{g} g))             "set of keywords")
    (is (= (name g) (m/gate-value #{(name g)} g)) "set of strings")
    (is (nil? (m/gate-value {} g))              "absent is nil")
    (is (nil? (m/gate-value nil g))             "no attestations at all is nil")))

(deftest a-plan-is-ready-when-attestations-arrive-as-a-set-of-strings
  (testing "the shape the caller happens to use must not change the verdict"
    (doseq [[label atts] [["keyword map"  all-attested]
                          ["string map"   (zipmap (map name m/common-gates) (repeat true))]
                          ["keyword set"  (set m/common-gates)]
                          ["string set"   (set (map name m/common-gates))]]]
      (let [p (m/cell-plan :health {:attestations atts})]
        (is (= :ready (:status p)) (str label " should satisfy every gate"))
        (is (seq (:effects p)) (str label " should produce effects"))))))

;; ── rkey derivation ─────────────────────────────────────────────────────────

(deftest safe-rkey-strips-the-did-web-prefix-only-at-the-start
  (is (= "oil-trading.etzhayyim.com" (m/safe-rkey "did:web:oil-trading.etzhayyim.com")))
  (is (= "did-web-a" (m/safe-rkey "did:web:did:web:a"))
      "only the leading prefix is a scheme; a second one is data")
  (is (= "x-did-web-a" (m/safe-rkey "x/did:web:a"))
      "a prefix that is not at the start is not stripped"))

(deftest safe-rkey-replaces-every-character-outside-the-safe-set
  (is (= "aZ0._~-" (m/safe-rkey "aZ0._~-")) "the safe set survives untouched")
  (is (= "a-b-c-d" (m/safe-rkey "a/b c:d")) "slash, space and colon are replaced")
  (is (= "-----" (m/safe-rkey "日本語です"))
      "non-ASCII is replaced one character at a time, not collapsed")
  (is (not (re-find #"[^A-Za-z0-9._~-]" (m/safe-rkey "at://oil-trading.etzhayyim.com/x?y=1")))
      "nothing outside the safe set may survive"))

(deftest safe-rkey-falls-back-to-unknown-only-when-nothing-is-left
  (is (= "unknown" (m/safe-rkey nil)))
  (is (= "unknown" (m/safe-rkey "")))
  (is (= "---" (m/safe-rkey "   "))
      "whitespace is unsafe, so it becomes dashes before blank? is ever asked")
  (is (= "did:web:" (str/join (take 8 "did:web:x"))) "guard: the prefix under test")
  (is (= "unknown" (m/safe-rkey "did:web:")) "a bare scheme leaves nothing behind"))

(deftest rkey-is-derived-in-a-fixed-order-of-preference
  (let [plan (fn [input] (first (m/records-for (get m/cell-specs :trade) input)))]
    (is (= "from-rkey"
           (:rkey (plan {:record {:rkey "from-rkey" "rkey" "s" :tid "t"} :request-id "r"})))
        "the keyword :rkey wins")
    (is (= "from-string-rkey"
           (:rkey (plan {:record {"rkey" "from-string-rkey" :tid "t"} :request-id "r"})))
        "then the string \"rkey\"")
    (is (= "from-tid" (:rkey (plan {:record {:tid "from-tid"} :request-id "r"})))
        "then :tid")
    (is (= "from-request" (:rkey (plan {:request-id "from-request"})))
        "then the request id")
    (is (= "com-etzhayyim-apps-oilTrading-trade-0" (:rkey (plan {})))
        "and finally the legacy cell name with the collection index")
    (is (= "a-b" (:rkey (plan {:record {:rkey "a/b"}})))
        "whatever wins is still passed through safe-rkey")))

;; ── record and effect shape ─────────────────────────────────────────────────

(deftest planned-records-carry-the-actor-provenance
  (let [spec (get m/cell-specs :cargo)
        [r]  (m/records-for spec {:computed-at "2026-08-31T00:00:00Z" :request-id "req-9"})
        rec  (:record r)]
    (is (= (first (:collections spec)) (:$type rec)))
    (is (= m/actor-did (:actorDid rec)))
    (is (= (:legacy-cell spec) (:legacyCell rec)))
    (is (= (:phase spec) (:phase rec)))
    (is (= "2026-08-31T00:00:00Z" (:computedAt rec)))
    (is (= "req-9" (:requestId rec)))
    (is (= "cljc-migration-scaffold" (:actorBoundary rec)))
    (is (true? (:scaffold rec)))
    (is (= "attested-plan" (:constitutionalStatus rec)))))

(deftest input-records-may-be-addressed-by-collection-or-by-index
  (let [spec (get m/cell-specs :refinery)
        coll (first (:collections spec))]
    (is (= "by-collection"
           (:mark (:record (first (m/records-for spec {:records {coll {:mark "by-collection"}}}))))))
    (is (= "by-index"
           (:mark (:record (first (m/records-for spec {:records {0 {:mark "by-index"}}}))))))
    (is (= "singular"
           (:mark (:record (first (m/records-for spec {:record {:mark "singular"}}))))))))

(deftest put-record-effect-always-names-the-operation-and-the-actor
  (let [e (m/put-record-effect "c" "k" {:a 1})]
    (is (= :mst/put-record (:op e)))
    (is (= m/actor-did (:actor e)))
    (is (= "c" (:collection e)))
    (is (= "k" (:rkey e)))
    (is (= {:a 1} (:record e)))))

(deftest ready-effects-match-the-records-they-were-planned-from
  (let [p (m/cell-plan :offtakecontract {:attestations all-attested :request-id "r"})]
    (is (= :ready (:status p)))
    (doseq [[rec eff] (map vector (:records p) (:effects p))]
      (is (= :mst/put-record (:op eff)))
      (is (= m/actor-did (:actor eff)))
      (is (= (:collection rec) (:collection eff)))
      (is (= (:rkey rec) (:rkey eff)))
      (is (= (:record rec) (:record eff))))))

;; ── the spec catalogue ──────────────────────────────────────────────────────

(deftest every-spec-requires-the-full-gate-set
  (testing "no cell may carry a shorter gate list than its siblings"
    (doseq [[k spec] m/cell-specs]
      (is (= (vec m/common-gates) (vec (:required-gates spec)))
          (str k " must require every common gate")))))

(deftest every-spec-declares-its-collection-under-the-actor-namespace
  (doseq [[k spec] m/cell-specs]
    (is (= [(str "com.etzhayyim.oil-trading." (name k))] (:collections spec))
        (str k " should own exactly the collection named after it"))))

(deftest collection-names-are-namespaced-to-this-actor
  (is (= "com.etzhayyim.oil-trading.x" (m/collection "x")))
  (is (every? #(str/starts-with? % "com.etzhayyim.oil-trading.")
              (mapcat :collections (vals m/cell-specs)))))

(deftest specs-agree-on-node-and-phase
  (doseq [[k spec] m/cell-specs]
    (is (= "reuben" (:murakumo-node spec)) (str k " runs on reuben"))
    (is (= :event (:phase spec)) (str k " is an event-phase cell"))
    (is (string? (:legacy-cell spec)) (str k " names the cell it replaces"))
    (is (seq (:trigger spec)) (str k " states its trigger"))))

(deftest legacy-cell-names-are-unique
  (let [names (map :legacy-cell (vals m/cell-specs))]
    (is (= (count names) (count (set names)))
        "two cells sharing a legacy name would make the migration ambiguous")))

(deftest the-gate-set-is-not-empty
  (is (seq m/common-gates)
      "an empty gate list would make every plan ready without any attestation")
  (is (= (count m/common-gates) (count (set m/common-gates)))
      "a duplicated gate would let one attestation cover two slots"))

(deftest unknown-cells-are-refused-rather-than-planned
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (m/cell-plan :no-such-cell {:attestations all-attested})))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (m/cell-plan nil {:attestations all-attested}))))
