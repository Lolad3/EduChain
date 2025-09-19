;; EduChain: Decentralized Learning and Knowledge Sharing Reward System
;; Version: 1.0.0

;; Constants
(define-constant KNOWLEDGE_POOL_CAPACITY u2000000)
(define-constant BASE_LEARNING_REWARD u26)
(define-constant MASTERY_BONUS u10)
(define-constant MAX_SCHOLAR_LEVEL u14)
(define-constant ERR_INVALID_LEARNING_ACTIVITY u1)
(define-constant ERR_NO_KNOWLEDGE_TOKENS u2)
(define-constant ERR_POOL_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_SEMESTER u1872)
(define-constant SCHOLARSHIP_MULTIPLIER u5)
(define-constant MIN_SCHOLARSHIP_PERIOD u936)
(define-constant EARLY_WITHDRAWAL_PENALTY u16)

;; Data Variables
(define-data-var total-knowledge-tokens-distributed uint u0)
(define-data-var total-learning-sessions uint u0)
(define-data-var education-administrator principal tx-sender)

;; Data Maps
(define-map scholar-courses principal uint)
(define-map scholar-knowledge-tokens principal uint)
(define-map learning-session-start-time principal uint)
(define-map scholar-mastery-level principal uint)
(define-map scholar-last-activity principal uint)
(define-map scholar-scholarship-fund principal uint)
(define-map scholar-fund-start-block principal uint)
(define-map subject-specialty principal uint)
(define-map scholar-certification-count principal uint)
(define-map teaching-expertise principal uint)

;; Public Functions
(define-public (start-learning-session (study-duration uint) (subject-type uint))
  (let
    (
      (scholar tx-sender)
    )
    (asserts! (and (> study-duration u0) (> subject-type u0) (<= subject-type u15)) (err ERR_INVALID_LEARNING_ACTIVITY))
    (map-set learning-session-start-time scholar burn-block-height)
    (map-set subject-specialty scholar subject-type)
    (ok true)
  ))

(define-public (complete-learning-session (study-duration uint) (comprehension-score uint))
  (let
    (
      (scholar tx-sender)
      (start-block (default-to u0 (map-get? learning-session-start-time scholar)))
      (blocks-studying (- burn-block-height start-block))
      (last-activity-block (default-to u0 (map-get? scholar-last-activity scholar)))
      (mastery-level (default-to u0 (map-get? scholar-mastery-level scholar)))
      (capped-mastery (if (<= mastery-level MAX_SCHOLAR_LEVEL) mastery-level MAX_SCHOLAR_LEVEL))
      (teaching-bonus (default-to u0 (map-get? teaching-expertise scholar)))
      (comprehension-bonus (/ (* comprehension-score u8) u100))
      (learning-reward (+ BASE_LEARNING_REWARD (* capped-mastery MASTERY_BONUS) teaching-bonus comprehension-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-studying study-duration) (<= comprehension-score u100)) (err ERR_INVALID_LEARNING_ACTIVITY))
    
    (map-set scholar-courses scholar (+ (default-to u0 (map-get? scholar-courses scholar)) u1))
    (map-set scholar-knowledge-tokens scholar (+ (default-to u0 (map-get? scholar-knowledge-tokens scholar)) learning-reward))
    
    (if (< (- burn-block-height last-activity-block) BLOCKS_PER_SEMESTER)
      (map-set scholar-mastery-level scholar (+ mastery-level u1))
      (map-set scholar-mastery-level scholar u1)
    )
    
    (if (>= comprehension-score u80)
      (begin
        (map-set scholar-certification-count scholar (+ (default-to u0 (map-get? scholar-certification-count scholar)) u1))
        (map-set teaching-expertise scholar (+ teaching-bonus u4))
      )
      true
    )
    
    (map-set scholar-last-activity scholar burn-block-height)
    (var-set total-learning-sessions (+ (var-get total-learning-sessions) u1))
    (var-set total-knowledge-tokens-distributed (+ (var-get total-knowledge-tokens-distributed) learning-reward))
    
    (asserts! (<= (var-get total-knowledge-tokens-distributed) KNOWLEDGE_POOL_CAPACITY) (err ERR_POOL_CAPACITY_EXCEEDED))
    (ok learning-reward)
  ))

(define-public (claim-knowledge-rewards)
  (let
    (
      (scholar tx-sender)
      (token-balance (default-to u0 (map-get? scholar-knowledge-tokens scholar)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_KNOWLEDGE_TOKENS))
    (map-set scholar-knowledge-tokens scholar u0)
    (ok token-balance)
  ))

(define-public (create-scholarship-fund (amount uint))
  (let
    (
      (scholar tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_LEARNING_ACTIVITY))
    (asserts! (>= (var-get total-knowledge-tokens-distributed) amount) (err ERR_POOL_CAPACITY_EXCEEDED))
    
    (map-set scholar-scholarship-fund scholar amount)
    (map-set scholar-fund-start-block scholar burn-block-height)
    (var-set total-knowledge-tokens-distributed (- (var-get total-knowledge-tokens-distributed) amount))
    (ok amount)
  ))

(define-public (withdraw-scholarship-fund)
  (let
    (
      (scholar tx-sender)
      (fund-amount (default-to u0 (map-get? scholar-scholarship-fund scholar)))
      (fund-start-block (default-to u0 (map-get? scholar-fund-start-block scholar)))
      (blocks-invested (- burn-block-height fund-start-block))
      (penalty (if (< blocks-invested MIN_SCHOLARSHIP_PERIOD) (/ (* fund-amount EARLY_WITHDRAWAL_PENALTY) u100) u0))
      (scholarship-bonus (if (>= blocks-invested MIN_SCHOLARSHIP_PERIOD) (/ (* fund-amount SCHOLARSHIP_MULTIPLIER) u100) u0))
      (final-amount (+ (- fund-amount penalty) scholarship-bonus))
    )
    (asserts! (> fund-amount u0) (err ERR_NO_KNOWLEDGE_TOKENS))
    
    (map-set scholar-scholarship-fund scholar u0)
    (map-set scholar-fund-start-block scholar u0)
    (var-set total-knowledge-tokens-distributed (+ (var-get total-knowledge-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (establish-learning-academy (academy-name (string-utf8 64)) (course-count uint))
  (let
    (
      (scholar tx-sender)
      (mastery-level (default-to u0 (map-get? scholar-mastery-level scholar)))
      (certification-count (default-to u0 (map-get? scholar-certification-count scholar)))
      (academy-bonus (+ (* course-count u20) (* certification-count u12) BASE_LEARNING_REWARD))
    )
    (asserts! (and (> (len academy-name) u0) (>= mastery-level u8) (> course-count u0)) (err ERR_INVALID_LEARNING_ACTIVITY))
    
    (map-set scholar-knowledge-tokens scholar (+ (default-to u0 (map-get? scholar-knowledge-tokens scholar)) academy-bonus))
    (var-set total-knowledge-tokens-distributed (+ (var-get total-knowledge-tokens-distributed) academy-bonus))
    
    (ok academy-bonus)
  ))

(define-public (conduct-knowledge-workshop (participant-count uint) (workshop-hours uint))
  (let
    (
      (scholar tx-sender)
      (mastery-level (default-to u0 (map-get? scholar-mastery-level scholar)))
      (teaching-expertise-level (default-to u0 (map-get? teaching-expertise scholar)))
      (workshop-bonus (+ (* participant-count u18) (* workshop-hours u7) (* teaching-expertise-level u2)))
    )
    (asserts! (and (> participant-count u0) (> workshop-hours u0) (>= mastery-level u10)) (err ERR_INVALID_LEARNING_ACTIVITY))
    
    (map-set scholar-knowledge-tokens scholar (+ (default-to u0 (map-get? scholar-knowledge-tokens scholar)) workshop-bonus))
    (var-set total-knowledge-tokens-distributed (+ (var-get total-knowledge-tokens-distributed) workshop-bonus))
    
    (ok workshop-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-course-count (user principal))
  (default-to u0 (map-get? scholar-courses user)))

(define-read-only (get-knowledge-token-balance (user principal))
  (default-to u0 (map-get? scholar-knowledge-tokens user)))

(define-read-only (get-mastery-level (user principal))
  (default-to u0 (map-get? scholar-mastery-level user)))

(define-read-only (get-certification-count (user principal))
  (default-to u0 (map-get? scholar-certification-count user)))

(define-read-only (get-scholarship-fund (user principal))
  (default-to u0 (map-get? scholar-scholarship-fund user)))

(define-read-only (get-teaching-expertise (user principal))
  (default-to u0 (map-get? teaching-expertise user)))

(define-read-only (get-education-stats)
  {
    total-learning-sessions: (var-get total-learning-sessions),
    total-knowledge-tokens-distributed: (var-get total-knowledge-tokens-distributed),
    knowledge-pool-capacity: KNOWLEDGE_POOL_CAPACITY
  })

(define-read-only (calculate-learning-reward (mastery-level uint) (comprehension-score uint) (teaching-bonus uint))
  (let
    (
      (capped-mastery (if (<= mastery-level MAX_SCHOLAR_LEVEL) mastery-level MAX_SCHOLAR_LEVEL))
      (comprehension-bonus (/ (* comprehension-score u8) u100))
    )
    (+ BASE_LEARNING_REWARD (* capped-mastery MASTERY_BONUS) teaching-bonus comprehension-bonus)
  ))

;; Private Functions
(define-private (is-education-administrator)
  (is-eq tx-sender (var-get education-administrator)))

(define-private (validate-learning-parameters (study-duration uint) (comprehension-score uint))
  (and (> study-duration u0) (<= comprehension-score u100)))