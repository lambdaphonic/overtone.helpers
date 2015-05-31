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

(defn spb
  "
  Returns the second per beat for the given metronome

  for example
  (def m (metronome 120)
  (spb m)   --> 0.5
  (spb m 1) --> 0.5
  (spb m 2) --> 1.0
  "
  ([metro] (spb metro 1))
  ([metro beat-offset]
   (let [bpm (metro :bpm)
         bps (/ bpm 60)]
     (* 1.0 (/ beat-offset bps)))))

(defn mspb
  "
  Returns the milliseconds per beat for the given metronome

  for example
  (def m (metronome 120))
  (mspb m)   --> 500.0
  (mspb m 1) --> 500.0
  (mspb m 2) --> 1000.0
  "
  ([metro] (mspb metro 1))
  ([metro beat-offset]
   (* 1000.0 (spb metro beat-offset))))

(defn atbeat [metro beat]
  "returns the time with the given beat offset for the given metronome"
  (metro (+ (metro) beat)))
