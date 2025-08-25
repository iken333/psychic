;; Enhanced Voting with Audit Trail Smart Contract
;; Advanced voting system with comprehensive features and audit capabilities

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_VOTING_CLOSED (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_INVALID_CANDIDATE (err u103))
(define-constant ERR_VOTING_NOT_STARTED (err u104))
(define-constant ERR_INSUFFICIENT_STAKE (err u105))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u106))
(define-constant ERR_DELEGATION_FAILED (err u107))
(define-constant ERR_VOTING_PAUSED (err u108))
(define-constant ERR_INVALID_WEIGHT (err u109))
(define-constant ERR_EMERGENCY_MODE (err u110))

;; Data Variables
(define-data-var voting-active bool false)
(define-data-var voting-start-time uint u0)
(define-data-var voting-end-time uint u0)
(define-data-var total-votes uint u0)
(define-data-var total-weighted-votes uint u0)
(define-data-var voting-paused bool false)
(define-data-var emergency-mode bool false)
(define-data-var min-stake-required uint u1000000) ;; 1 STX minimum stake
(define-data-var proposal-counter uint u0)
(define-data-var delegate-counter uint u0)

;; Enhanced Data Maps

;; Store comprehensive voting records
(define-map voting-records 
    principal 
    {
        candidate: (string-ascii 50),
        timestamp: uint,
        block-height: uint,
        vote-weight: uint,
        delegated-by: (optional principal),
        vote-hash: (buff 32),
        ip-hash: (buff 32) ;; For privacy while maintaining audit
    }
)

;; Enhanced candidate information
(define-map candidates 
    (string-ascii 50) 
    {
        vote-count: uint,
        weighted-vote-count: uint,
        is-active: bool,
        description: (string-ascii 200),
        proposal-link: (string-ascii 100),
        registration-time: uint,
        endorsements: uint
    }
)

;; Voter profiles with enhanced features
(define-map voter-profiles 
    principal 
    {
        registered: bool,
        stake-amount: uint,
        vote-weight: uint,
        reputation-score: uint,
        registration-time: uint,
        votes-cast: uint,
        delegate: (optional principal),
        can-delegate: bool
    }
)

;; Delegation system
(define-map delegations 
    uint 
    {
        delegator: principal,
        delegate: principal,
        delegation-time: uint,
        active: bool,
        vote-weight-delegated: uint
    }
)

;; Proposals system
(define-map proposals 
    uint 
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposer: principal,
        creation-time: uint,
        voting-start: uint,
        voting-end: uint,
        yes-votes: uint,
        no-votes: uint,
        abstain-votes: uint,
        status: (string-ascii 20), ;; "active", "passed", "rejected", "expired"
        quorum-required: uint,
        execution-hash: (optional (buff 32))
    }
)

;; Proposal voting records
(define-map proposal-votes 
    {proposal-id: uint, voter: principal}
    {
        vote-type: (string-ascii 10), ;; "yes", "no", "abstain"
        timestamp: uint,
        vote-weight: uint
    }
)

;; Enhanced audit log with categories
(define-map audit-log 
    uint 
    {
        category: (string-ascii 30), ;; "VOTING", "ADMIN", "DELEGATION", "PROPOSAL"
        action: (string-ascii 50),
        actor: principal,
        timestamp: uint,
        block-height: uint,
        details: (string-ascii 300),
        affected-party: (optional principal),
        transaction-hash: (buff 32)
    }
)

(define-data-var audit-log-counter uint u0)

;; Voting statistics
(define-map voting-statistics 
    (string-ascii 30)
    uint
)

;; Emergency contacts
(define-map emergency-contacts principal bool)

;; Vote verification system
(define-map vote-commitments 
    principal 
    {
        commitment-hash: (buff 32),
        reveal-deadline: uint,
        revealed: bool
    }
)

;; Private Functions

;; Enhanced audit entry with categorization
(define-private (add-audit-entry (category (string-ascii 30)) (action (string-ascii 50)) (details (string-ascii 300)) (affected-party (optional principal)))
    (let 
        (
            (counter (var-get audit-log-counter))
            (new-counter (+ counter u1))
        )
        (map-set audit-log counter {
            category: category,
            action: action,
            actor: tx-sender,
            timestamp: burn-block-height,
            block-height: block-height,
            details: details,
            affected-party: affected-party,
            transaction-hash: (get-txid)
        })
        (var-set audit-log-counter new-counter)
        (ok new-counter)
    )
)

;; Calculate vote weight based on stake and reputation
(define-private (calculate-vote-weight (voter principal))
    (let 
        (
            (profile (unwrap-panic (map-get? voter-profiles voter)))
            (base-weight u1)
            (stake-multiplier (/ (get stake-amount profile) u100000)) ;; 0.1 STX = 1 weight unit
            (reputation-bonus (/ (get reputation-score profile) u10))
        )
        (+ base-weight stake-multiplier reputation-bonus)
    )
)

;; Update voting statistics
(define-private (update-statistics (stat-name (string-ascii 30)) (increment uint))
    (let 
        (
            (current-value (default-to u0 (map-get? voting-statistics stat-name)))
        )
        (map-set voting-statistics stat-name (+ current-value increment))
        (ok true)
    )
)

;; Generate vote hash for verification
(define-private (generate-vote-hash (voter principal) (candidate (string-ascii 50)))
    (sha256 (unwrap-panic (to-consensus-buff? {voter: voter, candidate: candidate})))
)

;; Get transaction ID (simplified placeholder)
(define-private (get-txid)
    (sha256 (unwrap-panic (to-consensus-buff? {block: block-height, burn: burn-block-height})))
)

;; Public Functions - Enhanced Voting System

;; Initialize voting with advanced parameters
(define-public (initialize-voting-advanced (start-time uint) (end-time uint) (min-stake uint) (quorum-percentage uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (not (var-get emergency-mode)) ERR_EMERGENCY_MODE)
        (var-set voting-start-time start-time)
        (var-set voting-end-time end-time)
        (var-set min-stake-required min-stake)
        (unwrap-panic (add-audit-entry "ADMIN" "VOTING_INITIALIZED" 
            (concat "Start: " (uint-to-ascii start-time)) none))
        (ok true)
    )
)

;; Enhanced candidate registration
(define-public (register-candidate-enhanced (candidate-name (string-ascii 50)) (description (string-ascii 200)) (proposal-link (string-ascii 100)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (not (var-get emergency-mode)) ERR_EMERGENCY_MODE)
        (map-set candidates candidate-name {
            vote-count: u0,
            weighted-vote-count: u0,
            is-active: true,
            description: description,
            proposal-link: proposal-link,
            registration-time: burn-block-height,
            endorsements: u0
        })
        (unwrap-panic (add-audit-entry "ADMIN" "CANDIDATE_REGISTERED" 
            (concat "Name: " candidate-name) none))
        (ok true)
    )
)

;; Enhanced voter registration with staking
(define-public (register-voter-with-stake (voter principal) (initial-stake uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (>= initial-stake (var-get min-stake-required)) ERR_INSUFFICIENT_STAKE)
        (map-set voter-profiles voter {
            registered: true,
            stake-amount: initial-stake,
            vote-weight: (+ u1 (/ initial-stake u100000)),
            reputation-score: u100, ;; Starting reputation
            registration-time: burn-block-height,
            votes-cast: u0,
            delegate: none,
            can-delegate: true
        })
        (unwrap-panic (add-audit-entry "ADMIN" "VOTER_REGISTERED" 
            (concat "Stake: " (uint-to-ascii initial-stake)) (some voter)))
        (ok true)
    )
)

;; Delegation system
(define-public (delegate-vote (delegate-to principal))
    (let 
        (
            (delegator tx-sender)
            (delegator-profile (unwrap! (map-get? voter-profiles delegator) ERR_UNAUTHORIZED))
            (delegate-profile (unwrap! (map-get? voter-profiles delegate-to) ERR_UNAUTHORIZED))
            (delegation-id (var-get delegate-counter))
        )
        (asserts! (get registered delegator-profile) ERR_UNAUTHORIZED)
        (asserts! (get can-delegate delegator-profile) ERR_DELEGATION_FAILED)
        (asserts! (get registered delegate-profile) ERR_DELEGATION_FAILED)
        (asserts! (not (var-get voting-paused)) ERR_VOTING_PAUSED)
        
        ;; Create delegation record
        (map-set delegations delegation-id {
            delegator: delegator,
            delegate: delegate-to,
            delegation-time: burn-block-height,
            active: true,
            vote-weight-delegated: (get vote-weight delegator-profile)
        })
        
        ;; Update delegator profile
        (map-set voter-profiles delegator 
            (merge delegator-profile {delegate: (some delegate-to)}))
        
        (var-set delegate-counter (+ delegation-id u1))
        (unwrap-panic (add-audit-entry "DELEGATION" "VOTE_DELEGATED" 
            (concat "To: " (principal-to-string delegate-to)) (some delegate-to)))
        (ok delegation-id)
    )
)

;; Revoke delegation
(define-public (revoke-delegation (delegation-id uint))
    (let 
        (
            (delegation (unwrap! (map-get? delegations delegation-id) ERR_DELEGATION_FAILED))
            (delegator-profile (unwrap-panic (map-get? voter-profiles tx-sender)))
        )
        (asserts! (is-eq tx-sender (get delegator delegation)) ERR_UNAUTHORIZED)
        (asserts! (get active delegation) ERR_DELEGATION_FAILED)
        
        ;; Deactivate delegation
        (map-set delegations delegation-id 
            (merge delegation {active: false}))
        
        ;; Update delegator profile
        (map-set voter-profiles tx-sender 
            (merge delegator-profile {delegate: none}))
        
        (unwrap-panic (add-audit-entry "DELEGATION" "DELEGATION_REVOKED" 
            (concat "ID: " (uint-to-ascii delegation-id)) none))
        (ok true)
    )
)

;; Enhanced voting with commit-reveal scheme
(define-public (commit-vote (commitment-hash (buff 32)) (reveal-deadline uint))
    (let 
        (
            (voter tx-sender)
            (profile (unwrap! (map-get? voter-profiles voter) ERR_UNAUTHORIZED))
        )
        (asserts! (get registered profile) ERR_UNAUTHORIZED)
        (asserts! (var-get voting-active) ERR_VOTING_CLOSED)
        (asserts! (not (var-get voting-paused)) ERR_VOTING_PAUSED)
        
        (map-set vote-commitments voter {
            commitment-hash: commitment-hash,
            reveal-deadline: reveal-deadline,
            revealed: false
        })
        
        (unwrap-panic (add-audit-entry "VOTING" "VOTE_COMMITTED" "Vote commitment submitted" none))
        (ok true)
    )
)

;; Reveal and cast vote
(define-public (reveal-and-cast-vote (candidate-name (string-ascii 50)) (nonce uint))
    (let 
        (
            (voter tx-sender)
            (profile (unwrap! (map-get? voter-profiles voter) ERR_UNAUTHORIZED))
            (commitment (unwrap! (map-get? vote-commitments voter) ERR_UNAUTHORIZED))
            (candidate-info (unwrap! (map-get? candidates candidate-name) ERR_INVALID_CANDIDATE))
            (vote-weight (calculate-vote-weight voter))
            (vote-hash (generate-vote-hash voter candidate-name))
            (expected-hash (sha256 (unwrap-panic (to-consensus-buff? {candidate: candidate-name, nonce: nonce}))))
        )
        ;; Verify commitment
        (asserts! (is-eq (get commitment-hash commitment) expected-hash) ERR_UNAUTHORIZED)
        (asserts! (not (get revealed commitment)) ERR_ALREADY_VOTED)
        (asserts! (<= burn-block-height (get reveal-deadline commitment)) ERR_VOTING_CLOSED)
        
        ;; Record the vote
        (map-set voting-records voter {
            candidate: candidate-name,
            timestamp: burn-block-height,
            block-height: block-height,
            vote-weight: vote-weight,
            delegated-by: (get delegate profile),
            vote-hash: vote-hash,
            ip-hash: (sha256 (unwrap-panic (to-consensus-buff? voter))) ;; Simplified IP hash
        })
        
        ;; Update candidate stats
        (map-set candidates candidate-name {
            vote-count: (+ (get vote-count candidate-info) u1),
            weighted-vote-count: (+ (get weighted-vote-count candidate-info) vote-weight),
            is-active: (get is-active candidate-info),
            description: (get description candidate-info),
            proposal-link: (get proposal-link candidate-info),
            registration-time: (get registration-time candidate-info),
            endorsements: (get endorsements candidate-info)
        })
        
        ;; Update voter profile
        (map-set voter-profiles voter 
            (merge profile {votes-cast: (+ (get votes-cast profile) u1)}))
        
        ;; Mark commitment as revealed
        (map-set vote-commitments voter 
            (merge commitment {revealed: true}))
        
        ;; Update totals
        (var-set total-votes (+ (var-get total-votes) u1))
        (var-set total-weighted-votes (+ (var-get total-weighted-votes) vote-weight))
        
        ;; Update statistics
        (unwrap-panic (update-statistics "TOTAL_VOTES_CAST" u1))
        (unwrap-panic (update-statistics "TOTAL_WEIGHT_CAST" vote-weight))
        
        (unwrap-panic (add-audit-entry "VOTING" "VOTE_REVEALED_AND_CAST" 
            (concat "Candidate: " candidate-name) none))
        (ok true)
    )
)

;; Proposal system
(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (voting-duration uint) (quorum-required uint))
    (let 
        (
            (proposal-id (var-get proposal-counter))
            (proposer tx-sender)
            (profile (unwrap! (map-get? voter-profiles proposer) ERR_UNAUTHORIZED))
        )
        (asserts! (get registered profile) ERR_UNAUTHORIZED)
        (asserts! (>= (get reputation-score profile) u50) ERR_UNAUTHORIZED) ;; Minimum reputation to propose
        
        (map-set proposals proposal-id {
            title: title,
            description: description,
            proposer: proposer,
            creation-time: burn-block-height,
            voting-start: burn-block-height,
            voting-end: (+ burn-block-height voting-duration),
            yes-votes: u0,
            no-votes: u0,
            abstain-votes: u0,
            status: "active",
            quorum-required: quorum-required,
            execution-hash: none
        })
        
        (var-set proposal-counter (+ proposal-id u1))
        (unwrap-panic (add-audit-entry "PROPOSAL" "PROPOSAL_CREATED" 
            (concat "ID: " (uint-to-ascii proposal-id)) none))
        (ok proposal-id)
    )
)

;; Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-type (string-ascii 10)))
    (let 
        (
            (voter tx-sender)
            (profile (unwrap! (map-get? voter-profiles voter) ERR_UNAUTHORIZED))
            (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
            (vote-weight (calculate-vote-weight voter))
            (existing-vote (map-get? proposal-votes {proposal-id: proposal-id, voter: voter}))
        )
        (asserts! (get registered profile) ERR_UNAUTHORIZED)
        (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
        (asserts! (is-eq (get status proposal) "active") ERR_VOTING_CLOSED)
        (asserts! (<= burn-block-height (get voting-end proposal)) ERR_VOTING_CLOSED)
        
        ;; Record vote
        (map-set proposal-votes {proposal-id: proposal-id, voter: voter} {
            vote-type: vote-type,
            timestamp: burn-block-height,
            vote-weight: vote-weight
        })
        
        ;; Update proposal counts
        (map-set proposals proposal-id 
            (if (is-eq vote-type "yes")
                (merge proposal {yes-votes: (+ (get yes-votes proposal) vote-weight)})
                (if (is-eq vote-type "no")
                    (merge proposal {no-votes: (+ (get no-votes proposal) vote-weight)})
                    (merge proposal {abstain-votes: (+ (get abstain-votes proposal) vote-weight)})
                )
            )
        )
        
        (unwrap-panic (add-audit-entry "PROPOSAL" "PROPOSAL_VOTE_CAST" 
            (concat "Proposal: " (uint-to-ascii proposal-id)) none))
        (ok true)
    )
)

;; Emergency functions
(define-public (pause-voting)
    (begin
        (asserts! (or (is-eq tx-sender CONTRACT_OWNER) 
                     (default-to false (map-get? emergency-contacts tx-sender))) ERR_UNAUTHORIZED)
        (var-set voting-paused true)
        (unwrap-panic (add-audit-entry "ADMIN" "VOTING_PAUSED" "Emergency pause activated" none))
        (ok true)
    )
)

(define-public (resume-voting)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set voting-paused false)
        (unwrap-panic (add-audit-entry "ADMIN" "VOTING_RESUMED" "Voting resumed" none))
        (ok true)
    )
)

(define-public (activate-emergency-mode)
    (begin
        (asserts! (or (is-eq tx-sender CONTRACT_OWNER) 
                     (default-to false (map-get? emergency-contacts tx-sender))) ERR_UNAUTHORIZED)
        (var-set emergency-mode true)
        (var-set voting-paused true)
        (unwrap-panic (add-audit-entry "ADMIN" "EMERGENCY_ACTIVATED" "Emergency mode activated" none))
        (ok true)
    )
)

;; Reputation management
(define-public (update-reputation (voter principal) (score-change int))
    (let 
        (
            (profile (unwrap! (map-get? voter-profiles voter) ERR_UNAUTHORIZED))
            (current-score (get reputation-score profile))
            (new-score (if (> score-change 0) 
                         (+ current-score (to-uint score-change))
                         (if (>= current-score (to-uint (- 0 score-change)))
                             (- current-score (to-uint (- 0 score-change)))
                             u0)))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        (map-set voter-profiles voter 
            (merge profile {reputation-score: new-score}))
        
        (unwrap-panic (add-audit-entry "ADMIN" "REPUTATION_UPDATED" 
            (concat "NewScore: " (uint-to-ascii new-score)) (some voter)))
        (ok new-score)
    )
)

;; Enhanced Read-only Functions

;; Get comprehensive voting record
(define-read-only (get-voting-record-detailed (voter principal))
    (let 
        (
            (vote-record (map-get? voting-records voter))
            (profile (map-get? voter-profiles voter))
            (commitment (map-get? vote-commitments voter))
        )
        {
            vote-record: vote-record,
            profile: profile,
            commitment: commitment
        }
    )
)

;; Get enhanced candidate info
(define-read-only (get-candidate-detailed (candidate-name (string-ascii 50)))
    (map-get? candidates candidate-name)
)

;; Get voting statistics
(define-read-only (get-voting-stats)
    {
        total-votes: (var-get total-votes),
        total-weighted-votes: (var-get total-weighted-votes),
        voting-active: (var-get voting-active),
        voting-paused: (var-get voting-paused),
        emergency-mode: (var-get emergency-mode),
        start-time: (var-get voting-start-time),
        end-time: (var-get voting-end-time),
        audit-entries: (var-get audit-log-counter)
    }
)

;; Get delegation info
(define-read-only (get-delegation-info (delegation-id uint))
    (map-get? delegations delegation-id)
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

;; Get proposal vote
(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
    (map-get? proposal-votes {proposal-id: proposal-id, voter: voter})
)

;; Get audit entries by category
(define-read-only (get-audit-by-category (category (string-ascii 30)) (start-id uint) (count uint))
    (let 
        (
            (max-id (var-get audit-log-counter))
            (end-id (if (> (+ start-id count) max-id) max-id (+ start-id count)))
        )
        ;; This would need additional helper functions in a full implementation
        (ok "AUDIT_ENTRIES_BY_CATEGORY") ;; Placeholder
    )
)

;; Get voting leaderboard (most active voters)
(define-read-only (get-voter-leaderboard)
    ;; Would return top voters by votes cast and reputation
    (ok "LEADERBOARD_DATA") ;; Placeholder
)

;; Utility Functions (Enhanced)
(define-private (uint-to-ascii (value uint))
    (if (is-eq value u0) "0"
    (if (is-eq value u1) "1"
    (if (is-eq value u2) "2"
    (if (is-eq value u3) "3"
    (if (is-eq value u4) "4"
    (if (is-eq value u5) "5"
    (if (is-eq value u6) "6"
    (if (is-eq value u7) "7"
    (if (is-eq value u8) "8"
    (if (is-eq value u9) "9"
    "N"))))))))))
)

(define-private (int-to-string (value int))
    (if (>= value 0) 
        (uint-to-ascii (to-uint value))
        (concat "-" (uint-to-ascii (to-uint (- 0 value))))
    )
)

(define-private (principal-to-string (p principal))
    "PRINCIPAL" ;; Simplified implementation
)

(define-private (string-to-buff (s (string-ascii 50)))
    (unwrap-panic (to-consensus-buff? s))
)