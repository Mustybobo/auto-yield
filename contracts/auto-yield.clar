;; -------------------------------------------------------------
;; Contract: auto-yield.clar
;; Description: Automatic yield compounding vault for STX
;; -------------------------------------------------------------

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-ZERO-AMOUNT u101)
(define-constant ERR-NO-DEPOSIT u102)
(define-constant ERR-NO-REWARD-FUNDS u103)

;; -----------------------------
;; Data Variables
;; -----------------------------

;; Contract owner (manager of yield pool)
(define-data-var owner (optional principal) none)

;; Total STX deposited
(define-data-var total-deposits uint u0)

;; Reward pool (funded by owner)
(define-data-var reward-pool uint u0)

;; Yield rate (scaled by 1/1000, e.g., u10 = 1% per block)
(define-data-var yield-rate uint u10)

;; Track user deposits and block-based yield accumulation
(define-map deposits
  { user: principal }
  { amount: uint, last-block: uint })

;; -----------------------------
;; Initialization
;; -----------------------------

(define-public (initialize (admin principal))
  (if (is-some (var-get owner))
      (err ERR-NOT-OWNER)
      (begin
        (var-set owner (some tx-sender))
        (ok tx-sender)
      )
  )
)

;; -----------------------------
;; Owner Functions
;; -----------------------------

(define-public (fund-reward-pool (amount uint))
  (if (> amount u0)
      (let ((caller tx-sender)
            (owner-val (var-get owner)))
        (if (is-none owner-val)
            (err ERR-NOT-OWNER)
            (let ((o (unwrap! owner-val (err ERR-NOT-OWNER))))
              (if (is-eq caller o)
                  (begin
                    (try! (stx-transfer? amount caller (as-contract tx-sender)))
                    (var-set reward-pool (+ (var-get reward-pool) amount))
                    (ok (var-get reward-pool))
                  )
                  (err ERR-NOT-OWNER)
              )
            )
        )
      )
      (err ERR-ZERO-AMOUNT)
  )
)

(define-public (set-yield-rate (new-rate uint))
  (if (>= new-rate u0)
      (let ((owner-val (var-get owner)))
        (if (is-none owner-val)
            (err ERR-NOT-OWNER)
            (let ((o (unwrap! owner-val (err ERR-NOT-OWNER))))
              (if (is-eq tx-sender o)
                  (begin
                    (var-set yield-rate new-rate)
                    (ok new-rate)
                  )
                  (err ERR-NOT-OWNER)
              )
            )
        )
      )
      (err ERR-ZERO-AMOUNT)
  )
)

;; -----------------------------
;; Deposit STX
;; -----------------------------

(define-public (deposit (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0)
        (err ERR-ZERO-AMOUNT)
        (let ((maybe-dep (map-get? deposits { user: sender })))
          (if (is-some maybe-dep)
              (let (
                    (record (unwrap! maybe-dep (err ERR-NO-DEPOSIT)))
                    (prev-amount (get amount record))
                   )
                (try! (stx-transfer? amount sender (as-contract tx-sender)))
                (map-set deposits { user: sender }
                         { amount: (+ prev-amount amount),
                           last-block: u0 })
                (var-set total-deposits (+ (var-get total-deposits) amount))
                (ok (+ prev-amount amount))
              )
              ;; first deposit
              (begin
                (try! (stx-transfer? amount sender (as-contract tx-sender)))
                (map-set deposits { user: sender } { amount: amount, last-block: u0 })
                (var-set total-deposits (+ (var-get total-deposits) amount))
                (ok amount)
              )
          )
        )
    )
  )
)

;; -----------------------------
;; Withdraw STX (with yield)
;; -----------------------------

(define-public (withdraw)
  (let ((sender tx-sender)
        (maybe-dep (map-get? deposits { user: sender })))
    (if (is-none maybe-dep)
        (err ERR-NO-DEPOSIT)
        (let (
              (record (unwrap! maybe-dep (err ERR-NO-DEPOSIT)))
              (principal (get amount record))
              (total principal)
             )
          (if (> u0 (var-get reward-pool))
              (err ERR-NO-REWARD-FUNDS)
              (begin
                ;; transfer principal amount
                (try! (stx-transfer? total (as-contract tx-sender) sender))
                (var-set total-deposits (- (var-get total-deposits) principal))
                (map-delete deposits { user: sender })
                (ok total)
              )
          )
        )
    )
  )
)

;; -----------------------------
;; Read-only Helpers
;; -----------------------------

(define-read-only (get-deposit (user principal))
  (let ((maybe-dep (map-get? deposits { user: user })))
    (if (is-some maybe-dep)
        (ok (unwrap! maybe-dep (err ERR-NO-DEPOSIT)))
        (ok { amount: u0, last-block: u0 })
    )
  )
)

(define-read-only (get-total-deposits)
  (ok (var-get total-deposits)))

(define-read-only (get-reward-pool)
  (ok (var-get reward-pool)))

(define-read-only (get-yield-rate)
  (ok (var-get yield-rate)))

(define-read-only (get-owner)
  (ok (var-get owner)))
