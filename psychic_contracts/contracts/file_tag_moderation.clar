;; FileTagModeration Smart Contract
;; Purpose: Moderate file tags for quality control

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_TAG_NOT_FOUND (err u101))
(define-constant ERR_TAG_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_STATUS (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_USER_NOT_FOUND (err u105))
(define-constant ERR_MODERATOR_NOT_FOUND (err u106))

;; Tag status constants
(define-constant STATUS_PENDING u0)
(define-constant STATUS_APPROVED u1)
(define-constant STATUS_REJECTED u2)

;; Reward amounts (in microSTX)
(define-constant QUALITY_TAG_REWARD u1000000) ;; 1 STX
(define-constant MODERATOR_REWARD u500000)    ;; 0.5 STX

;; Data Variables
(define-data-var next-tag-id uint u1)
(define-data-var contract-balance uint u0)

;; Data Maps
;; Store tag submissions
(define-map tags
  { tag-id: uint }
  {
    file-hash: (string-ascii 64),
    tag-content: (string-utf8 50),
    submitter: principal,
    status: uint,
    timestamp: uint,
    moderator: (optional principal),
    moderation-timestamp: (optional uint)
  }
)

;; Track user statistics
(define-map user-stats
  { user: principal }
  {
    tags-submitted: uint,
    tags-approved: uint,
    tags-rejected: uint,
    rewards-earned: uint,
    reputation-score: uint
  }
)

;; Track moderator permissions
(define-map moderators
  { moderator: principal }
  {
    is-active: bool,
    tags-moderated: uint,
    rewards-earned: uint
  }
)

;; Track tags per file to prevent spam
(define-map file-tag-count
  { file-hash: (string-ascii 64) }
  { count: uint }
)

;; Track user tag submissions per file (anti-spam)
(define-map user-file-tags
  { user: principal, file-hash: (string-ascii 64) }
  { tag-count: uint }
)

;; Public Functions

;; Submit a new tag for moderation
(define-public (submit-tag (file-hash (string-ascii 64)) (tag-content (string-utf8 50)))
  (let (
    (tag-id (var-get next-tag-id))
    (current-user-file-tags (default-to u0 (get tag-count (map-get? user-file-tags { user: tx-sender, file-hash: file-hash }))))
    (current-file-tags (default-to u0 (get count (map-get? file-tag-count { file-hash: file-hash }))))
  )
    ;; Prevent spam: max 3 tags per user per file
    (asserts! (< current-user-file-tags u3) (err u107))
    
    ;; Prevent spam: max 10 tags per file total
    (asserts! (< current-file-tags u10) (err u108))
    
    ;; Create tag entry
    (map-set tags
      { tag-id: tag-id }
      {
        file-hash: file-hash,
        tag-content: tag-content,
        submitter: tx-sender,
        status: STATUS_PENDING,
        timestamp: block-height,
        moderator: none,
        moderation-timestamp: none
      }
    )
    
    ;; Update counters
    (var-set next-tag-id (+ tag-id u1))
    (map-set file-tag-count
      { file-hash: file-hash }
      { count: (+ current-file-tags u1) }
    )
    (map-set user-file-tags
      { user: tx-sender, file-hash: file-hash }
      { tag-count: (+ current-user-file-tags u1) }
    )
    
    ;; Update user stats
    (update-user-submitted-count tx-sender)
    
    (ok tag-id)
  )
)

;; Moderate a tag (approve or reject)
(define-public (moderate-tag (tag-id uint) (approve bool))
  (let (
    (tag-data (unwrap! (map-get? tags { tag-id: tag-id }) ERR_TAG_NOT_FOUND))
    (moderator-data (unwrap! (map-get? moderators { moderator: tx-sender }) ERR_UNAUTHORIZED))
  )
    ;; Check if moderator is active
    (asserts! (get is-active moderator-data) ERR_UNAUTHORIZED)
    
    ;; Check if tag is still pending
    (asserts! (is-eq (get status tag-data) STATUS_PENDING) ERR_INVALID_STATUS)
    
    (let (
      (new-status (if approve STATUS_APPROVED STATUS_REJECTED))
      (submitter (get submitter tag-data))
    )
      ;; Update tag status
      (map-set tags
        { tag-id: tag-id }
        (merge tag-data {
          status: new-status,
          moderator: (some tx-sender),
          moderation-timestamp: (some block-height)
        })
      )
      
      ;; Update user stats
      (if approve
        (update-user-approved-count submitter)
        (update-user-rejected-count submitter)
      )
      
      ;; Update moderator stats
      (update-moderator-stats tx-sender)
      
      ;; Reward quality tags and moderators
      (if approve
        (begin
          (try! (reward-user submitter QUALITY_TAG_REWARD))
          (try! (reward-user tx-sender MODERATOR_REWARD))
        )
        true
      )
      
      (ok new-status)
    )
  )
)

;; Add a new moderator (only contract owner)
(define-public (add-moderator (new-moderator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set moderators
      { moderator: new-moderator }
      {
        is-active: true,
        tags-moderated: u0,
        rewards-earned: u0
      }
    )
    (ok true)
  )
)

;; Remove/deactivate a moderator (only contract owner)
(define-public (deactivate-moderator (moderator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (match (map-get? moderators { moderator: moderator })
      moderator-data (begin
        (map-set moderators
          { moderator: moderator }
          (merge moderator-data { is-active: false })
        )
        (ok true)
      )
      ERR_MODERATOR_NOT_FOUND
    )
  )
)

;; Fund the contract for rewards
(define-public (fund-contract (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok true)
  )
)

;; Read-only Functions

;; Get tag details
(define-read-only (get-tag (tag-id uint))
  (map-get? tags { tag-id: tag-id })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

;; Get moderator info
(define-read-only (get-moderator-info (moderator principal))
  (map-get? moderators { moderator: moderator })
)

;; Get tags for a specific file
(define-read-only (get-file-tags (file-hash (string-ascii 64)))
  (map-get? file-tag-count { file-hash: file-hash })
)

;; Check if user is moderator
(define-read-only (is-moderator (user principal))
  (match (map-get? moderators { moderator: user })
    moderator-data (get is-active moderator-data)
    false
  )
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

;; Get user reputation score
(define-read-only (get-user-reputation (user principal))
  (match (map-get? user-stats { user: user })
    stats (get reputation-score stats)
    u0
  )
)

;; Private Functions

;; Update user submitted count
(define-private (update-user-submitted-count (user principal))
  (let (
    (current-stats (default-to
      { tags-submitted: u0, tags-approved: u0, tags-rejected: u0, rewards-earned: u0, reputation-score: u0 }
      (map-get? user-stats { user: user })
    ))
  )
    (map-set user-stats
      { user: user }
      (merge current-stats {
        tags-submitted: (+ (get tags-submitted current-stats) u1)
      })
    )
  )
)

;; Update user approved count and reputation
(define-private (update-user-approved-count (user principal))
  (let (
    (current-stats (default-to
      { tags-submitted: u0, tags-approved: u0, tags-rejected: u0, rewards-earned: u0, reputation-score: u0 }
      (map-get? user-stats { user: user })
    ))
    (new-approved (+ (get tags-approved current-stats) u1))
    (new-reputation (calculate-reputation new-approved (get tags-rejected current-stats)))
  )
    (map-set user-stats
      { user: user }
      (merge current-stats {
        tags-approved: new-approved,
        reputation-score: new-reputation
      })
    )
  )
)

;; Update user rejected count and reputation
(define-private (update-user-rejected-count (user principal))
  (let (
    (current-stats (default-to
      { tags-submitted: u0, tags-approved: u0, tags-rejected: u0, rewards-earned: u0, reputation-score: u0 }
      (map-get? user-stats { user: user })
    ))
    (new-rejected (+ (get tags-rejected current-stats) u1))
    (new-reputation (calculate-reputation (get tags-approved current-stats) new-rejected))
  )
    (map-set user-stats
      { user: user }
      (merge current-stats {
        tags-rejected: new-rejected,
        reputation-score: new-reputation
      })
    )
  )
)

;; Update moderator statistics
(define-private (update-moderator-stats (moderator principal))
  (match (map-get? moderators { moderator: moderator })
    moderator-data (map-set moderators
      { moderator: moderator }
      (merge moderator-data {
        tags-moderated: (+ (get tags-moderated moderator-data) u1)
      })
    )
    false
  )
)

;; Calculate user reputation score
(define-private (calculate-reputation (approved uint) (rejected uint))
  (let (
    (total (+ approved rejected))
  )
    (if (is-eq total u0)
      u0
      (/ (* approved u100) total) ;; Percentage of approved tags
    )
  )
)

;; Reward a user
(define-private (reward-user (user principal) (amount uint))
  (begin
    (asserts! (>= (var-get contract-balance) amount) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? amount tx-sender user)))
    (var-set contract-balance (- (var-get contract-balance) amount))
    
    ;; Update user reward stats
    (let (
      (current-stats (default-to
        { tags-submitted: u0, tags-approved: u0, tags-rejected: u0, rewards-earned: u0, reputation-score: u0 }
        (map-get? user-stats { user: user })
      ))
    )
      (map-set user-stats
        { user: user }
        (merge current-stats {
          rewards-earned: (+ (get rewards-earned current-stats) amount)
        })
      )
    )
    
    ;; Update moderator reward stats if applicable
    (match (map-get? moderators { moderator: user })
      moderator-data (map-set moderators
        { moderator: user }
        (merge moderator-data {
          rewards-earned: (+ (get rewards-earned moderator-data) amount)
        })
      )
      true
    )
    
    (ok true)
  )
)