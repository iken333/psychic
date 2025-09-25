;; Withdrawal Notification Wallet Smart Contract
;; Emits events/logs on withdrawal operations

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))

;; Define data variables
(define-data-var wallet-balance uint u0)

;; Define maps to track user balances (if supporting multiple users)
(define-map user-balances principal uint)

;; Event/Log structure for withdrawals
;; Clarity uses print statements to emit events that can be observed off-chain
(define-private (emit-withdrawal-event (recipient principal) (amount uint))
  (print {
    event: "withdrawal",
    recipient: recipient,
    amount: amount,
    block-height: block-height
  })
)

;; Initialize wallet with some balance (for testing purposes)
(define-public (initialize-wallet (initial-balance uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set wallet-balance initial-balance)
    (ok initial-balance)
  )
)

;; Deposit function to add funds to the wallet
(define-public (deposit (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (let ((current-balance (var-get wallet-balance)))
      (var-set wallet-balance (+ current-balance amount))
      (map-set user-balances tx-sender (+ (default-to u0 (map-get? user-balances tx-sender)) amount))
      (ok (var-get wallet-balance))
    )
  )
)

;; Main withdrawal function that triggers event
(define-public (withdraw (amount uint))
  (let (
    (current-balance (var-get wallet-balance))
    (user-balance (default-to u0 (map-get? user-balances tx-sender)))
  )
    ;; Validate withdrawal conditions
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    (asserts! (>= user-balance amount) err-insufficient-balance)
    
    ;; Update balances
    (var-set wallet-balance (- current-balance amount))
    (map-set user-balances tx-sender (- user-balance amount))
    
    ;; Emit withdrawal event
    (emit-withdrawal-event tx-sender amount)
    
    ;; Return success with remaining balance
    (ok {
      withdrawn-amount: amount,
      remaining-balance: (- current-balance amount),
      recipient: tx-sender
    })
  )
)

;; Owner-only withdrawal function
(define-public (owner-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (var-get wallet-balance) amount) err-insufficient-balance)
    
    ;; Update balance
    (var-set wallet-balance (- (var-get wallet-balance) amount))
    
    ;; Emit withdrawal event
    (emit-withdrawal-event recipient amount)
    
    ;; Return success
    (ok {
      withdrawn-amount: amount,
      remaining-balance: (var-get wallet-balance),
      recipient: recipient
    })
  )
)

;; Read-only functions for querying state
(define-read-only (get-wallet-balance)
  (var-get wallet-balance)
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-contract-owner)
  contract-owner
)

;; Helper function to check if user can withdraw amount
(define-read-only (can-withdraw (user principal) (amount uint))
  (let (
    (user-balance (default-to u0 (map-get? user-balances user)))
    (current-wallet-balance (var-get wallet-balance))
  )
    (and 
      (>= user-balance amount)
      (>= current-wallet-balance amount)
      (> amount u0)
    )
  )
)