(ns lambdaphonic.overtone.helpers
  (:use [overtone.music.pitch]
        [overtone.algo.trig]))

(defn scale-fieldk [nkey sname]
  (scale-field (keyword (str (first (rest (str nkey))))) sname))

(defn scale? [scale note]
  (not (nil? (some #{note} scale))))

(defn quantize
  ([scale pitch-in]
   (quantize scale pitch-in 0))

  ([scale pitch-in increment]
    (let [pitch (Math/round (* pitch-in 1.0))]
      (cond (scale? scale (+ pitch increment)) (+ pitch increment)
            (scale? scale (- pitch increment)) (- pitch increment)
            (< increment 7) (quantize scale pitch (+ increment 1))
            true (do (println "no pc value to quantize to") false)))))

(defn qcosr [scale beat r center freq]
  (quantize scale (cosr beat r center freq)))
