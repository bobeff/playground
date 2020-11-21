; Exercise 32. Most people no longer use desktop computers just to run
; applications but also employ cell phones, tablets, and their cars’ information
; control screen. Soon people will use wearable computers in the form of
; intelligent glasses, clothes, and sports gear. In the somewhat more distant
; future, people may come with built-in bio computers that directly interact
; with body functions. Think of ten different forms of events that software
; applications on such computers will have to deal with.

#lang racket

(require racket/block)

(define EVENTS-LIST
  '("keyboard"
    "mouse"
    "network"
    "photo cell"
    "brain wave"
    "heart beat"
    "hand gesture"
    "walk"
    "gyroscope"
    "sound"))

(define (print-events events)
  (cond
    ((not (empty? events))
      (block
        (let ((event (first events)))
          (println (string-append event " event")))
        (print-events (rest events))))))

(print-events EVENTS-LIST)
