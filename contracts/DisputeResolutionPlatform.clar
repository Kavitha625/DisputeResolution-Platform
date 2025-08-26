;; DisputeResolution Platform Smart Contract
;; A decentralized arbitration service with expert mediators and enforceable decisions

;; Define constants for error handling
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-dispute-not-found (err u102))
(define-constant err-dispute-already-resolved (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-insufficient-deposit (err u105))

;; Define dispute status constants
(define-constant status-pending u0)
(define-constant status-resolved u1)
(define-constant status-executed u2)

;; Data structures
(define-map disputes 
  uint 
  {
    complainant: principal,
    respondent: principal,
    mediator: principal,
    amount: uint,
    description: (string-ascii 500),
    status: uint,
    decision: (optional (string-ascii 200)),
    winner: (optional principal),
    created-at: uint
  })

(define-map mediators 
  principal 
  {
    reputation-score: uint,
    total-cases: uint,
    is-active: bool
  })

;; Counter for dispute IDs
(define-data-var dispute-counter uint u0)

;; Minimum deposit required for dispute filing
(define-data-var min-dispute-deposit uint u1000000) ;; 1 STX in microSTX

;; Function 1: Create Dispute
;; Allows users to create a new dispute by depositing STX and providing case details
(define-public (create-dispute 
  (respondent principal) 
  (mediator principal) 
  (amount uint) 
  (description (string-ascii 500)))
  (let 
    (
      (dispute-id (+ (var-get dispute-counter) u1))
      (deposit-amount (var-get min-dispute-deposit))
    )
    (begin
      ;; Validate input parameters
      (asserts! (> amount u0) err-invalid-amount)
      (asserts! (not (is-eq tx-sender respondent)) err-not-authorized)
      
      ;; Check if mediator is active
      (asserts! 
        (match (map-get? mediators mediator)
          mediator-info (get is-active mediator-info)
          false) 
        err-not-authorized)
      
      ;; Transfer deposit from complainant to contract
      (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))
       
      ;; Create dispute record
      (map-set disputes dispute-id
        {
          complainant: tx-sender,
          respondent: respondent,
          mediator: mediator,
          amount: amount,
          description: description,
          status: status-pending,
          decision: none,
          winner: none,
          created-at: stacks-block-height
        })
      
      ;; Increment dispute counter
      (var-set dispute-counter dispute-id)
      
      ;; Emit print statement for indexing
      (print {
        event: "dispute-created",
        dispute-id: dispute-id,
        complainant: tx-sender,
        respondent: respondent,
        mediator: mediator,
        amount: amount
      })
      
      (ok dispute-id))))

;; Function 2: Resolve Dispute
;; Allows assigned mediator to resolve dispute and enforce decision
(define-public (resolve-dispute 
  (dispute-id uint) 
  (winner principal) 
  (decision-text (string-ascii 200)))
  (let 
    (
      (dispute-info (unwrap! (map-get? disputes dispute-id) err-dispute-not-found))
      (deposit-amount (var-get min-dispute-deposit))
    )
    (begin
      ;; Check if sender is the assigned mediator
      (asserts! (is-eq tx-sender (get mediator dispute-info)) err-not-authorized)
      
      ;; Check if dispute is still pending
      (asserts! (is-eq (get status dispute-info) status-pending) err-dispute-already-resolved)
      
      ;; Validate winner is either complainant or respondent
      (asserts! 
        (or 
          (is-eq winner (get complainant dispute-info))
          (is-eq winner (get respondent dispute-info))) 
        err-not-authorized)
      
      ;; Update dispute with resolution
      (map-set disputes dispute-id
        (merge dispute-info {
          status: status-resolved,
          decision: (some decision-text),
          winner: (some winner)
        }))
      
      ;; Transfer dispute deposit to winner as compensation
      (try! (as-contract (stx-transfer? deposit-amount tx-sender winner)))
      
      ;; Update mediator statistics
      (match (map-get? mediators tx-sender)
        mediator-info 
          (map-set mediators tx-sender
            (merge mediator-info {
              total-cases: (+ (get total-cases mediator-info) u1),
              reputation-score: (+ (get reputation-score mediator-info) u10)
            }))
        ;; If mediator doesn't exist, create entry
        (map-set mediators tx-sender {
          reputation-score: u10,
          total-cases: u1,
          is-active: true
        }))
      
      ;; Emit resolution event
      (print {
        event: "dispute-resolved",
        dispute-id: dispute-id,
        mediator: tx-sender,
        winner: winner,
        decision: decision-text
      })
      
      (ok true))))

;; Read-only functions for querying data

;; Get dispute information
(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id))

;; Get mediator information
(define-read-only (get-mediator (mediator principal))
  (map-get? mediators mediator))

;; Get current dispute counter
(define-read-only (get-dispute-count)
  (var-get dispute-counter))

;; Get minimum deposit amount
(define-read-only (get-min-deposit)
  (var-get min-dispute-deposit))

;; Admin function to register/activate mediator (only owner)
(define-public (register-mediator (mediator principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set mediators mediator {
      reputation-score: u0,
      total-cases: u0,
      is-active: true
    })
    (ok true)))

;; Admin function to update minimum deposit (only owner)
(define-public (set-min-deposit (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-amount u0) err-invalid-amount)
    (var-set min-dispute-deposit new-amount)
    (ok true)))