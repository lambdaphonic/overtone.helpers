(ns lambdaphonic.overtone.helpers
  (:use [overtone.music.pitch]
        [overtone.algo.trig]
        [overtone.sc.server]))


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
  (if (beat? beat expected-beat cycle-len)
    (if (ifn? fun)
      (fun)
      fun)))


(defn beat-map [metro base-time beat next-beat step fun]
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


(defn alberti [chord]
  "
  Returns an alberti sequence:
  first element
  third element
  second element
  third element

  the chord has to have at least 3 entries
  "
  (map #(nth (sort chord) %) [0 2 1 2]))

(defn chord-degree-short [degree root tonic]
  "
  Takes the first three notes of a given chord-degree
  "
  (take 3 (sort (chord-degree degree root tonic))))

(defn alberti-degree [degree root tonic]
  "
  makes an alberti chord sequence from the given degree
  "
  (alberti (chord-degree-short degree root tonic)))

(def full-cadence [:i :v :vi :iii :iv :i :iv :v :i])
(def markov-cadence {:i [:v :iv]
                     :iii [:iv]
                     :iv [:i :v]
                     :v [:vi :i]
                     :vi [:iii]})

(defn alberti-cadence
  "
  creates a cadence with alberti chord progressions
  "
  ([root tonic] (alberti-cadence root tonic full-cadence))
  ([root tonic degrees] (map #(alberti-degree % root tonic) degrees)))


(defn- arp-updown [notes]
  (let [n (vec (sort notes))
        r (vec (rest (reverse n)))
        r (take (- (count r) 1) r)]
    (flatten (conj n r))))

(defn- arp-downup [notes]
  (let [n (vec (reverse (sort notes)))
        r (vec (rest (reverse n)))
        r (take (- (count r) 1) r)]
  (flatten (conj n r))))

(defn- arp-alberti [notes]
  (let [n (vec (take 3 (cycle (sort notes))))]
    (alberti n)))

(defn arp [arp-type notes]
  "
  Arpeggiates the given notes
  possible arp-type values:

  :alberti
  :downup
  :updown

  Examples
  (arp :alberti [1 2 3 4]) -> (1 3 2 3)
  (arp :updown [1 3 4 2])  -> (1 2 3 4 3 2)
  (arp :downup [1 3 4 2])  -> (4 3 2 1 2 3)
  "
  (case arp-type
    :alberti (arp-alberti notes)
    :downup (arp-downup notes)
    (arp-updown notes)))

(defn scale-steps [root scale-name step-count]
  (let [cnt (mod step-count 8)
        scl (scale root
                   scale-name
                   (case cnt
                     1 [:vii]
                     2 [:iv, :vii]
                     3 [:iv, :vi,  :vii]
                     4 [:ii, :iv, :vi, :vii]
                     5 [:ii, :iii, :iv, :vi, :vii]
                     6 [:i, :ii, :iii, :iv, :vi, :vii]
                     7 [:i, :ii, :iii, :iv, :v, :vi, :vii]
                     []))]
    (flatten (conj scl (map #(- % 12) (take (- (count scl) 1) scl))))))

(defn calc-offsets [durs]
  "
  calculates the offsets for a list of durs

  Example:
  (calc-offsets [1 1 1 1]) -> (0 1 2 3)
  (calc-offsets [0.25 0.25 0.25 0.25] -> (0 0.25 0.5 0.75)
  "
  (let
      [reduced (reductions + 0 durs)
       l (count reduced)]
    (take (- l 1) reduced)))

(defn next-multiple-of
  "
  calculates the next multiple of mul given a current value

  Example:
  (next-multiple-of 4 1) -> 4
  (next-multiple-of 4 3) -> 4
  (next-multiple-of 4 4) -> 8
  "
  ([mul current-value] (next-multiple-of mul current-value true))
  ([mul current-value inc-value]
   (if
       (= 0 (mod current-value mul))
     (if inc-value (+ current-value mul) current-value)
     (next-multiple-of mul (+ 1 current-value) false))))

(defn next-bar [m bar-length]
  "
  Gets the time of the next bar given a metronome function and a bar-length in beats

  Example:
  (def metro (metronome 120))
  (next-bar metro 4) -> returns the time of the next bar, given the bar length is 4 beats
  "
  (let [current-beat (m)
        mul (* bar-length 4)]
    (next-multiple-of mul current-beat)))

(defn note-pattern [notes durs note-changes]
  "
  calculates a pattern of notes with duration and offset.

  notes: the notes to make the pattern for
  durs: the note durations
  note-changes: the pattern, when the notes change
  "
  (let [note-offsets (map (fn [o n] [o, n]) (calc-offsets note-changes) (cycle notes))
        dur-offsets (calc-offsets durs)]
    (map
      (fn [o d]
        {:offset o :duration d :note ((last (filter (fn [item] (<= (item 0) o)) note-offsets)) 1)})
      dur-offsets
      durs)))

(defn rhythm [idx]
  "
  Generates different rhythm patterns (idx can be from 0 to 16).
  All patterns are exactly 4 beats long

  Example:
  (rhythm 2) -> ({:offset 0 :duration 2} {:offset 2 :duration 2})
  "
  (let [n (mod idx 17)
        durations (case n
                    1 [4]
                    2 [2, 2]
                    3 [1.25, 1.25, 1.5]
                    4 [1, 1, 1, 1]
                    5 [0.75, 0.75, 0.75, 0.75, 1]
                    6 [0.75, 0.75, 0.5, 0.75, 0.75, 0.5]
                    7 [0.5, 0.5, 0.75, 0.5, 0.5, 0.75, 0.5]
                    8 [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
                    9 [0.5, 0.5, 0.5, 0.25, 0.5, 0.5, 0.5, 0.25, 0.5]
                    10 [0.5, 0.25, 0.5, 0.25, 0.5, 0.5, 0.25, 0.5, 0.25, 0.5]
                    11 [0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25]
                    12 [0.25, 0.25, 0.5, 0.25, 0.25, 0.5, 0.25, 0.25, 0.5, 0.25, 0.25, 0.5]
                    13 [0.25, 0.25, 0.25, 0.5, 0.25, 0.25, 0.25, 0.5, 0.25, 0.25, 0.25, 0.5, 0.25]
                    14 [0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.5, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.5]
                    15 [0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.5]
                    16 [0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25]
                    [8])
        offsets (calc-offsets durations)]
    (map
      (fn [o d] {:offset o :duration d})
      offsets
      durations)))


(defn- bjorklund [sequences]
  (let [length-of-first-element (count (first sequences))
        main-seqs (filter
                    (fn [s]
                      ( and
                        (or
                          (not (= 1 (count s)))
                          (= 1 (nth s 0)))
                        (= length-of-first-element (count s))))
                    sequences)
        main-seqs-length (count main-seqs)
        rest-seqs (drop main-seqs-length sequences)
        rest-length (count rest-seqs)
        distribute (take (min main-seqs-length rest-length) rest-seqs)
        remainder (if
                    (>= rest-length main-seqs-length)
                    (drop main-seqs-length rest-seqs)
                    (drop rest-length main-seqs))]
    (if
      (and
        (> length-of-first-element 1)
        (<= rest-length 1))
      [(flatten sequences)]
      (bjorklund
        (concat
          (map
            #(concat (nth main-seqs %) (nth distribute %))
            (range 0 (count distribute)))
          remainder)))))

(defn euclid [pulses steps]
  "Evenly distributes a list of pulses and pauses"
   (let [pauses (- steps pulses)
         pulse-seq (repeat pulses '(1))
         pause-seq (repeat pauses '(0))
         sequences (concat pulse-seq pause-seq)
         result (if
                  (empty? pause-seq)
                  [(flatten sequences)]
                  (bjorklund sequences))]
     (nth result 0)))

(defn euclid-string [pulses steps]
  (let [r (reductions + (euclid pulses steps))]
    (map
      #(count (filter (fn [x] (= x %))
                      r))
      (distinct r))))

(defn euclid-rhythm
  ([pulses] (euclid-rhythm pulses 16))
  ([pulses steps] (euclid-rhythm pulses steps 4))
  ([pulses steps over-beats]
   (let [per-step (/ over-beats steps)
         e (euclid-string pulses steps)]
     (map #(* % per-step) e)))))
