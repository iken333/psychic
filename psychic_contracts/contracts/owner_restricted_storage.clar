;; Enhanced Owner-Restricted Storage Contract
;; Advanced features with owner controls, history tracking, and flexible storage

;; Define the contract owner (deployer)
(define-constant contract-owner tx-sender)

;; Define error constants
(define-constant err-owner-only (err u100))
(define-constant err-contract-paused (err u101))
(define-constant err-invalid-value (err u102))
(define-constant err-unauthorized-admin (err u103))

;; Define data storage variables
(define-data-var stored-value uint u0)
(define-data-var contract-paused bool false)
(define-data-var min-value uint u0)
(define-data-var max-value uint u1000000)
(define-data-var update-count uint u0)
(define-data-var last-update-block uint u0)

;; Define storage for authorized admins
(define-map authorized-admins principal bool)

;; Define storage for value history (last 10 updates)
(define-map value-history uint {value: uint, block-height: uint, updater: principal})

;; Define storage for user access logs
(define-map access-logs principal {last-read: uint, read-count: uint})

;; Initialize contract with default admin (owner)
(map-set authorized-admins contract-owner true)

;; Public function to set the value (owner or authorized admin only)
(define-public (set-value (new-value uint))
  (begin
    ;; Check if contract is not paused
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    ;; Check if caller is owner or authorized admin
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (default-to false (map-get? authorized-admins tx-sender))) 
              err-owner-only)
    ;; Validate value range
    (asserts! (and (>= new-value (var-get min-value)) 
                   (<= new-value (var-get max-value))) 
              err-invalid-value)
    
    ;; Store current value in history
    (let ((current-count (var-get update-count)))
      (map-set value-history 
               (mod current-count u10)
               {value: new-value, 
                block-height: block-height, 
                updater: tx-sender})
    )
    
    ;; Update variables
    (var-set stored-value new-value)
    (var-set update-count (+ (var-get update-count) u1))
    (var-set last-update-block block-height)
    
    ;; Return success
    (ok new-value)
  )
)

;; Public function to add/remove authorized admins (owner only)
(define-public (set-admin (admin principal) (authorized bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (map-set authorized-admins admin authorized)
    (ok authorized)
  )
)

;; Public function to pause/unpause contract (owner only)
(define-public (set-pause-state (paused bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused paused)
    (ok paused)
  )
)

;; Public function to set value range limits (owner only)
(define-public (set-value-range (min uint) (max uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (< min max) err-invalid-value)
    (var-set min-value min)
    (var-set max-value max)
    (ok {min: min, max: max})
  )
)

;; Public function to increment value by specified amount
(define-public (increment-value (amount uint))
  (let ((current-value (var-get stored-value)))
    (set-value (+ current-value amount))
  )
)

;; Public function to decrement value by specified amount
(define-public (decrement-value (amount uint))
  (let ((current-value (var-get stored-value)))
    (if (>= current-value amount)
        (set-value (- current-value amount))
        (set-value u0)
    )
  )
)

;; Read-only function to get the current value
(define-read-only (get-value)
  (var-get stored-value)
)

;; Public function to get value with access tracking
(define-public (get-value-tracked)
  (begin
    ;; Update access log for the caller
    (map-set access-logs tx-sender 
             {last-read: block-height,
              read-count: (+ (get read-count 
                                 (default-to {last-read: u0, read-count: u0} 
                                           (map-get? access-logs tx-sender))) u1)})
    ;; Return the stored value
    (ok (var-get stored-value))
  )
)

;; Read-only function to get value with metadata
(define-read-only (get-value-info)
  {
    value: (var-get stored-value),
    last-update-block: (var-get last-update-block),
    update-count: (var-get update-count),
    min-allowed: (var-get min-value),
    max-allowed: (var-get max-value),
    is-paused: (var-get contract-paused)
  }
)

;; Read-only function to get contract owner
(define-read-only (get-owner)
  contract-owner
)

;; Read-only function to check if an address is the owner
(define-read-only (is-owner (address principal))
  (is-eq address contract-owner)
)

;; Read-only function to check if an address is an authorized admin
(define-read-only (is-admin (address principal))
  (default-to false (map-get? authorized-admins address))
)

;; Read-only function to check if caller can modify values
(define-read-only (can-modify (address principal))
  (and (not (var-get contract-paused))
       (or (is-eq address contract-owner) 
           (default-to false (map-get? authorized-admins address))))
)

;; Read-only function to get value history entry
(define-read-only (get-history (index uint))
  (if (< index u10)
      (map-get? value-history index)
      none
  )
)

;; Read-only function to get recent value history (last 5 updates)
(define-read-only (get-recent-history)
  (let ((current-count (var-get update-count)))
    {
      entry-0: (map-get? value-history (mod (- current-count u1) u10)),
      entry-1: (map-get? value-history (mod (- current-count u2) u10)),
      entry-2: (map-get? value-history (mod (- current-count u3) u10)),
      entry-3: (map-get? value-history (mod (- current-count u4) u10)),
      entry-4: (map-get? value-history (mod (- current-count u5) u10))
    }
  )
)

;; Read-only function to get access log for a user
(define-read-only (get-access-log (user principal))
  (map-get? access-logs user)
)

;; Read-only function to get contract statistics
(define-read-only (get-contract-stats)
  {
    total-updates: (var-get update-count),
    current-value: (var-get stored-value),
    last-update-block: (var-get last-update-block),
    blocks-since-update: (- block-height (var-get last-update-block)),
    value-range: {min: (var-get min-value), max: (var-get max-value)},
    is-paused: (var-get contract-paused),
    contract-age: (- block-height u0)
  }
)

;; Read-only function to validate a potential new value
(define-read-only (is-valid-value (value uint))
  (and (>= value (var-get min-value)) 
       (<= value (var-get max-value)))
)