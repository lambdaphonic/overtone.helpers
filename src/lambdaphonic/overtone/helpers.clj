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

(defn beat? [beat expected-beat cycle-len]
  "
  checks, if the current beat is the expected beat in a cycle (1-based)

  (beat? 0 1 4) ; true

  Cycle length of 4 beats per cycle:

  (beat? 0 4 4) ; false
  (beat? 1 4 4) ; false
  (beat? 2 4 4) ; false
  (beat? 3 4 4) ; true
  (beat? 4 4 4) ; false
  (beat? 5 4 4) ; false
  (beat? 6 4 4) ; false
  (beat? 7 4 4) ; true
  "
  (== expected-beat (+ 1 (mod beat cycle-len))))


(defn on-beat [beat expected-beat cycle-len fun]
  "evaluates fun, if the current beat is the expected beat in a given cycle"
  (if (beat beat expected-beat cycle-len)
    (if (ifn? fun)
      (fun)
      fun)))


(defn beat-map [base-time beat next-beat step fun]
  "
  maps a range of steps between between 0 and the difference
  of next-beat and beat and runs fun with the given beat offset
  to the base time

  example:
  (beat-map (now) 0 1 0.25 #(println %)) ; prints 0, 0.25, 0.5 and 0.75 in the time of a beat distributed evenly over the beat
  "
  (dorun
    (map #(let [b (+ beat %)
                bt (+ base-time (mspb metro %))]
            (at bt (fun b)))
         (range 0 (- next-beat beat) step))))

