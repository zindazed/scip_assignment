#lang racket

(require data-science-master)
(require plot)
(require math)

;; Simulation of Twitter data collection
(define (generate-ugandan-tweets num-tweets)
  (define ugandan-words 
    '("Uganda" "Kampala" "Ugandan" "pearl" "African" "development" 
      "community" "government" "progress" "challenges" "hope" "future"))
  
  (define sentiment-words 
    '(("good" 1) 
      ("great" 2) 
      ("excellent" 3)
      ("bad" -1) 
      ("terrible" -2) 
      ("horrible" -3)))
  
  (define months 
    '("January" "February" "March" "April" "May" "June" 
      "July" "August" "September" "October" "November" "December"))
  
  (for/list ([_ (in-range num-tweets)])
    (define text 
      (string-append 
       (string-join 
        (append 
         (take ugandan-words (random 3)) 
         (take (map first sentiment-words) (random 2))))
       " " (list-ref months (random 12))))
    
    (list text 
          (list-ref sentiment-words (random (length sentiment-words)))
          (list-ref months (random 12)))))

;; Sentiment analysis for Ugandan tweets
(define (analyze-ugandan-tweet-sentiment tweets #:lexicon [lexicon 'nrc])
  (for/list ([tweet tweets])
    (define text (first tweet))
    (define sentiment-label (second tweet))
    (define month (third tweet))
    
    (define tokens (document->tokens text #:sort? #t))
    (define tweet-sentiment 
      (list->sentiment tokens #:lexicon lexicon))
    
    (list 
     text 
     (if (number? sentiment-label) sentiment-label 0)
     month 
     (if (null? tweet-sentiment) 0 
         (sum (map (λ (x) 
                     (cond 
                       [(number? (second x)) (* (second x) (third x))]
                       [(eq? (second x) 'positive) 1]
                       [(eq? (second x) 'negative) -1]
                       [else 0])) 
                   (cdr tweet-sentiment)))))))

;; Monthly sentiment aggregation
(define (aggregate-monthly-sentiment analyzed-tweets)
  (define month-sentiments
    (for/fold ([month-hash (hash)])
              ([tweet analyzed-tweets])
      (define month (third tweet))
      (define sentiment-score (fourth tweet))
      (hash-update month-hash 
                   month 
                   (λ (current-scores) (cons sentiment-score current-scores))
                   (λ () (list sentiment-score)))))
  
  (for/list ([(month scores) (in-hash month-sentiments)])
    (list month (exact->inexact (/ (apply + scores) (length scores))))))

;; Visualization of monthly sentiment
(define (visualize-monthly-sentiment monthly-sentiments)
  (plot 
   (discrete-histogram 
    monthly-sentiments
    #:color "MediumSlateBlue"
    #:line-color "MediumSlateBlue")
   #:x-label "Month"
   #:y-label "Average Sentiment Score"))

;; Main analysis function
(define (uganda-tweet-mood-analysis 
         #:tweet-count [count 1000] 
         #:lexicon [lexicon 'nrc])
  (define tweets (generate-ugandan-tweets count))
  
  (define analyzed-tweets 
    (analyze-ugandan-tweet-sentiment tweets #:lexicon lexicon))
  
  (define monthly-sentiments 
    (aggregate-monthly-sentiment analyzed-tweets))
  
  (visualize-monthly-sentiment monthly-sentiments)
  
  (list 
   'monthly-sentiments monthly-sentiments
   'lexicon lexicon
   'total-tweets count))

;; Example usage
(uganda-tweet-mood-analysis 
 #:tweet-count 2000 
 #:lexicon 'nrc)