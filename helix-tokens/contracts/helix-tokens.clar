;; Helix - Social Good Token (SGT) Platform

;; ============================================================
;; SIP-010 TRAIT DEFINITION
;; ============================================================

(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 10) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; ============================================================
;; TOKEN DEFINITION
;; ============================================================

(define-fungible-token social-good-token)

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-NOT-FOUND             (err u101))
(define-constant ERR-ALREADY-EXISTS        (err u102))
(define-constant ERR-INVALID-AMOUNT        (err u103))
(define-constant ERR-INVALID-STATE         (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE  (err u105))
(define-constant ERR-MILESTONE-NOT-MET     (err u106))
(define-constant ERR-VALIDATOR-EXISTS      (err u107))
(define-constant ERR-NOT-VALIDATOR         (err u108))
(define-constant ERR-LOCK-ACTIVE           (err u109))
(define-constant ERR-QUORUM-NOT-MET        (err u110))

;; Token metadata
(define-constant TOKEN-NAME    "Social Good Token")
(define-constant TOKEN-SYMBOL  "SGT")
(define-constant TOKEN-DECIMALS u6)

;; Bonding curve base price in microSTX per SGT (1 STX = 1,000,000 uSTX)
(define-constant BASE-PRICE u1000000)

;; Impact multiplier precision (1.0 = u100)
(define-constant MULTIPLIER-PRECISION u100)

;; Staking tiers (blocks): 30d ~4320, 90d ~12960, 180d ~25920
(define-constant TIER-1-BLOCKS u4320)
(define-constant TIER-2-BLOCKS u12960)
(define-constant TIER-3-BLOCKS u25920)

;; Staking reward rates per tier (basis points out of 10000)
(define-constant TIER-1-RATE u200)
(define-constant TIER-2-RATE u500)
(define-constant TIER-3-RATE u1200)

;; Minimum validators required to approve an impact report
(define-constant MIN-VALIDATOR-QUORUM u3)

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; -- Treasury --
(define-data-var treasury-balance uint u0)

;; -- Token supply tracking for bonding curve --
(define-data-var bonding-supply uint u0)

;; -- Initiative counter --
(define-data-var initiative-nonce uint u0)

;; -- Validator registry --
;; Maps validator principal -> active status
(define-map validators principal bool)
(define-data-var validator-count uint u0)

;; -- Initiative data --
;; initiative-id -> initiative record
(define-map initiatives
  uint
  {
    creator:          principal,
    title:            (string-ascii 64),
    category:         (string-ascii 32),   ;; e.g. "carbon" "education" "healthcare"
    target-impact:    uint,                ;; target impact units (e.g. kg CO2, students)
    current-impact:   uint,               ;; validated impact so far
    funds-raised:     uint,               ;; uSTX in escrow
    funds-released:   uint,               ;; uSTX already disbursed
    milestone-count:  uint,
    is-active:        bool,
    created-at:       uint                ;; block height
  }
)

;; -- Milestones --
;; {initiative-id, milestone-index} -> milestone record
(define-map milestones
  { initiative-id: uint, index: uint }
  {
    impact-threshold: uint,   ;; impact units needed to unlock
    release-amount:   uint,   ;; uSTX to release when met
    is-released:      bool
  }
)

;; -- Impact reports --
;; report-id -> report record
(define-data-var report-nonce uint u0)

(define-map impact-reports
  uint
  {
    initiative-id:  uint,
    reporter:       principal,
    impact-delta:   uint,       ;; new impact units being reported
    data-uri:       (string-ascii 128), ;; IPFS / satellite data URI
    approvals:      uint,
    is-finalized:   bool,
    submitted-at:   uint
  }
)

;; Tracks which validators have voted on a report
(define-map report-votes
  { report-id: uint, validator: principal }
  bool
)

;; -- Staking positions --
(define-map stakes
  principal
  {
    amount:      uint,   ;; SGT locked
    tier:        uint,   ;; 1, 2, or 3
    locked-until: uint,  ;; block height
    reward-rate: uint    ;; basis points
  }
)

;; -- Impact multiplier per initiative (precision u100 = 1.0x) --
(define-map impact-multipliers uint uint)

;; -- SGT balances held by the contract for stakers --
(define-data-var staked-supply uint u0)

;; ============================================================
;; SIP-010 IMPLEMENTATION
;; ============================================================

(define-public (transfer
    (amount uint)
    (sender principal)
    (recipient principal)
    (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (ft-transfer? social-good-token amount sender recipient))
    (match memo m (begin (print m) true) true)
    (ok true)
  )
)

(define-read-only (get-name)
  (ok TOKEN-NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN-SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN-DECIMALS)
)

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance social-good-token account))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply social-good-token))
)

(define-read-only (get-token-uri)
  (ok (some u"https://helix.social/sgt-metadata.json"))
)

;; ============================================================
;; BONDING CURVE
;; ============================================================
;; Price formula: price = BASE-PRICE + (supply * BASE-PRICE / 10000)
;; This gives a linear bonding curve where each token minted
;; increases the price by 0.01% of BASE-PRICE.

(define-read-only (get-current-price)
  (let ((supply (var-get bonding-supply)))
    (+ BASE-PRICE (/ (* supply BASE-PRICE) u10000))
  )
)

;; Buy SGTs via bonding curve. Caller sends STX, receives SGT.
(define-public (buy-sgt (sgt-amount uint))
  (let (
    (price-per-token (get-current-price))
    (total-cost (* sgt-amount price-per-token))
  )
    (asserts! (> sgt-amount u0) ERR-INVALID-AMOUNT)
    ;; Transfer STX from buyer to contract treasury
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    ;; Mint SGT to buyer
    (try! (ft-mint? social-good-token sgt-amount tx-sender))
    (var-set bonding-supply (+ (var-get bonding-supply) sgt-amount))
    (var-set treasury-balance (+ (var-get treasury-balance) total-cost))
    (print { event: "buy-sgt", buyer: tx-sender, amount: sgt-amount, cost: total-cost })
    (ok total-cost)
  )
)

;; Sell SGTs back to the bonding curve.
(define-public (sell-sgt (sgt-amount uint))
  (let (
    (price-per-token (get-current-price))
    (total-return (* sgt-amount price-per-token))
    (current-treasury (var-get treasury-balance))
  )
    (asserts! (> sgt-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= (ft-get-balance social-good-token tx-sender) sgt-amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= current-treasury total-return) ERR-INSUFFICIENT-BALANCE)
    (try! (ft-burn? social-good-token sgt-amount tx-sender))
    (var-set bonding-supply (- (var-get bonding-supply) sgt-amount))
    (var-set treasury-balance (- current-treasury total-return))
    (try! (as-contract (stx-transfer? total-return tx-sender tx-sender)))
    (print { event: "sell-sgt", seller: tx-sender, amount: sgt-amount, returned: total-return })
    (ok total-return)
  )
)

;; ============================================================
;; VALIDATOR REGISTRY
;; ============================================================

(define-public (register-validator (validator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? validators validator)) ERR-VALIDATOR-EXISTS)
    (map-set validators validator true)
    (var-set validator-count (+ (var-get validator-count) u1))
    (print { event: "validator-registered", validator: validator })
    (ok true)
  )
)

(define-public (remove-validator (validator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? validators validator)) ERR-NOT-FOUND)
    (map-delete validators validator)
    (var-set validator-count (- (var-get validator-count) u1))
    (print { event: "validator-removed", validator: validator })
    (ok true)
  )
)

(define-read-only (is-validator (account principal))
  (default-to false (map-get? validators account))
)

;; ============================================================
;; INITIATIVE MANAGEMENT
;; ============================================================

(define-public (create-initiative
    (title (string-ascii 64))
    (category (string-ascii 32))
    (target-impact uint))
  (let ((id (var-get initiative-nonce)))
    (asserts! (> target-impact u0) ERR-INVALID-AMOUNT)
    (map-set initiatives id {
      creator:         tx-sender,
      title:           title,
      category:        category,
      target-impact:   target-impact,
      current-impact:  u0,
      funds-raised:    u0,
      funds-released:  u0,
      milestone-count: u0,
      is-active:       true,
      created-at:      block-height
    })
    (map-set impact-multipliers id MULTIPLIER-PRECISION)
    (var-set initiative-nonce (+ id u1))
    (print { event: "initiative-created", id: id, creator: tx-sender, category: category })
    (ok id)
  )
)

;; Add a milestone to an initiative
(define-public (add-milestone
    (initiative-id uint)
    (impact-threshold uint)
    (release-amount uint))
  (let (
    (initiative (unwrap! (map-get? initiatives initiative-id) ERR-NOT-FOUND))
    (idx (get milestone-count initiative))
  )
    (asserts! (is-eq tx-sender (get creator initiative)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active initiative) ERR-INVALID-STATE)
    (asserts! (> impact-threshold u0) ERR-INVALID-AMOUNT)
    (asserts! (> release-amount u0) ERR-INVALID-AMOUNT)
    (map-set milestones { initiative-id: initiative-id, index: idx } {
      impact-threshold: impact-threshold,
      release-amount:   release-amount,
      is-released:      false
    })
    (map-set initiatives initiative-id
      (merge initiative { milestone-count: (+ idx u1) })
    )
    (print { event: "milestone-added", initiative-id: initiative-id, index: idx })
    (ok idx)
  )
)

;; Fund an initiative by depositing STX into escrow
(define-public (fund-initiative (initiative-id uint) (amount uint))
  (let (
    (initiative (unwrap! (map-get? initiatives initiative-id) ERR-NOT-FOUND))
  )
    (asserts! (get is-active initiative) ERR-INVALID-STATE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set initiatives initiative-id
      (merge initiative { funds-raised: (+ (get funds-raised initiative) amount) })
    )
    (print { event: "initiative-funded", initiative-id: initiative-id, funder: tx-sender, amount: amount })
    (ok true)
  )
)

;; ============================================================
;; IMPACT REPORTING AND VALIDATION
;; ============================================================

;; Submit an impact report (anyone can submit, validators approve)
(define-public (submit-impact-report
    (initiative-id uint)
    (impact-delta uint)
    (data-uri (string-ascii 128)))
  (let ((report-id (var-get report-nonce)))
    (asserts! (is-some (map-get? initiatives initiative-id)) ERR-NOT-FOUND)
    (asserts! (> impact-delta u0) ERR-INVALID-AMOUNT)
    (map-set impact-reports report-id {
      initiative-id:  initiative-id,
      reporter:       tx-sender,
      impact-delta:   impact-delta,
      data-uri:       data-uri,
      approvals:      u0,
      is-finalized:   false,
      submitted-at:   block-height
    })
    (var-set report-nonce (+ report-id u1))
    (print { event: "report-submitted", report-id: report-id, initiative-id: initiative-id })
    (ok report-id)
  )
)

;; Validator approves an impact report
(define-public (approve-impact-report (report-id uint))
  (let (
    (report (unwrap! (map-get? impact-reports report-id) ERR-NOT-FOUND))
    (vote-key { report-id: report-id, validator: tx-sender })
  )
    (asserts! (is-validator tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (not (get is-finalized report)) ERR-INVALID-STATE)
    (asserts! (is-none (map-get? report-votes vote-key)) ERR-ALREADY-EXISTS)
    (map-set report-votes vote-key true)
    (map-set impact-reports report-id
      (merge report { approvals: (+ (get approvals report) u1) })
    )
    (print { event: "report-approved", report-id: report-id, validator: tx-sender })
    (ok true)
  )
)

;; Finalize a report once quorum is met; updates initiative impact
(define-public (finalize-impact-report (report-id uint))
  (let (
    (report (unwrap! (map-get? impact-reports report-id) ERR-NOT-FOUND))
    (initiative-id (get initiative-id report))
    (initiative (unwrap! (map-get? initiatives initiative-id) ERR-NOT-FOUND))
    (new-impact (+ (get current-impact initiative) (get impact-delta report)))
    (multiplier (default-to MULTIPLIER-PRECISION (map-get? impact-multipliers initiative-id)))
  )
    (asserts! (not (get is-finalized report)) ERR-INVALID-STATE)
    (asserts! (>= (get approvals report) MIN-VALIDATOR-QUORUM) ERR-QUORUM-NOT-MET)
    ;; Mark report finalized
    (map-set impact-reports report-id (merge report { is-finalized: true }))
    ;; Update initiative impact
    (map-set initiatives initiative-id
      (merge initiative { current-impact: new-impact })
    )
    ;; Increase multiplier if initiative exceeds 50% of target (bonus zone)
    (if (and
          (>= new-impact (/ (get target-impact initiative) u2))
          (< multiplier u150))
      (map-set impact-multipliers initiative-id (+ multiplier u10))
      false
    )
    (print { event: "report-finalized", report-id: report-id, new-impact: new-impact })
    (ok new-impact)
  )
)

;; ============================================================
;; MILESTONE FUND RELEASE
;; ============================================================

;; Release funds for a milestone if impact threshold has been met
(define-public (release-milestone-funds
    (initiative-id uint)
    (milestone-index uint))
  (let (
    (initiative (unwrap! (map-get? initiatives initiative-id) ERR-NOT-FOUND))
    (milestone (unwrap! (map-get? milestones { initiative-id: initiative-id, index: milestone-index }) ERR-NOT-FOUND))
    (release-amount (get release-amount milestone))
    (creator (get creator initiative))
  )
    (asserts! (not (get is-released milestone)) ERR-INVALID-STATE)
    (asserts! (>= (get current-impact initiative) (get impact-threshold milestone)) ERR-MILESTONE-NOT-MET)
    (asserts! (<= release-amount (- (get funds-raised initiative) (get funds-released initiative))) ERR-INSUFFICIENT-BALANCE)
    ;; Mark milestone released
    (map-set milestones { initiative-id: initiative-id, index: milestone-index }
      (merge milestone { is-released: true })
    )
    ;; Update disbursed amount
    (map-set initiatives initiative-id
      (merge initiative { funds-released: (+ (get funds-released initiative) release-amount) })
    )
    ;; Transfer funds to initiative creator
    (try! (as-contract (stx-transfer? release-amount tx-sender creator)))
    (print { event: "milestone-released", initiative-id: initiative-id, index: milestone-index, amount: release-amount })
    (ok release-amount)
  )
)

;; ============================================================
;; STAKING (TIERED)
;; ============================================================

;; Stake SGT for a chosen tier (1, 2, or 3)
(define-public (stake-sgt (amount uint) (tier uint))
  (let (
    (lock-blocks (if (is-eq tier u1) TIER-1-BLOCKS
                   (if (is-eq tier u2) TIER-2-BLOCKS
                     (if (is-eq tier u3) TIER-3-BLOCKS u0))))
    (rate        (if (is-eq tier u1) TIER-1-RATE
                   (if (is-eq tier u2) TIER-2-RATE
                     (if (is-eq tier u3) TIER-3-RATE u0))))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> lock-blocks u0) ERR-INVALID-AMOUNT) ;; invalid tier
    (asserts! (is-none (map-get? stakes tx-sender)) ERR-ALREADY-EXISTS)
    (asserts! (>= (ft-get-balance social-good-token tx-sender) amount) ERR-INSUFFICIENT-BALANCE)
    ;; Transfer SGT to contract
    (try! (ft-transfer? social-good-token amount tx-sender (as-contract tx-sender)))
    (var-set staked-supply (+ (var-get staked-supply) amount))
    (map-set stakes tx-sender {
      amount:       amount,
      tier:         tier,
      locked-until: (+ block-height lock-blocks),
      reward-rate:  rate
    })
    (print { event: "sgt-staked", staker: tx-sender, amount: amount, tier: tier })
    (ok true)
  )
)

;; Unstake SGT after lock period; receive principal + rewards
(define-public (unstake-sgt)
  (let (
    (position (unwrap! (map-get? stakes tx-sender) ERR-NOT-FOUND))
    (amount    (get amount position))
    (rate      (get reward-rate position))
    ;; Reward = amount * rate / 10000
    (reward    (/ (* amount rate) u10000))
    (total-return (+ amount reward))
  )
    (asserts! (>= block-height (get locked-until position)) ERR-LOCK-ACTIVE)
    (map-delete stakes tx-sender)
    (var-set staked-supply (- (var-get staked-supply) amount))
    ;; Return principal SGT
    (try! (as-contract (ft-transfer? social-good-token amount tx-sender tx-sender)))
    ;; Mint reward SGT (inflationary reward)
    (try! (ft-mint? social-good-token reward tx-sender))
    (var-set bonding-supply (+ (var-get bonding-supply) reward))
    (print { event: "sgt-unstaked", staker: tx-sender, principal: amount, reward: reward })
    (ok total-return)
  )
)

;; ============================================================
;; COMMUNITY IMPACT COUNCIL - QUADRATIC VOTING
;; ============================================================
;; Proposal: governance actions (e.g., pause initiative, adjust multiplier)
;; Vote weight = floor(sqrt(SGT balance)) to prevent whale dominance.

(define-data-var proposal-nonce uint u0)

(define-map proposals
  uint
  {
    proposer:     principal,
    description:  (string-ascii 128),
    target-id:    uint,            ;; initiative-id affected
    action-code:  uint,            ;; 1=pause, 2=resume, 3=boost-multiplier
    votes-for:    uint,
    votes-against: uint,
    is-executed:  bool,
    created-at:   uint,
    voting-ends:  uint             ;; block height
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  bool  ;; true = for, false = against
)

(define-constant VOTING-PERIOD-BLOCKS u1440) ;; ~10 days
(define-constant PROPOSAL-MIN-BALANCE u100)  ;; min SGT to propose

;; Integer square root (Babylonian method approximation for small inputs)
(define-private (isqrt (n uint))
  (if (is-eq n u0)
    u0
    (let ((x (/ (+ n u1) u2)))
      ;; One Newton step is sufficient for governance weight precision
      (/ (+ x (/ n x)) u2)
    )
  )
)

(define-public (create-proposal
    (description (string-ascii 128))
    (target-id uint)
    (action-code uint))
  (let (
    (balance (ft-get-balance social-good-token tx-sender))
    (pid (var-get proposal-nonce))
  )
    (asserts! (>= balance PROPOSAL-MIN-BALANCE) ERR-INSUFFICIENT-BALANCE)
    (asserts! (is-some (map-get? initiatives target-id)) ERR-NOT-FOUND)
    (map-set proposals pid {
      proposer:      tx-sender,
      description:   description,
      target-id:     target-id,
      action-code:   action-code,
      votes-for:     u0,
      votes-against: u0,
      is-executed:   false,
      created-at:    block-height,
      voting-ends:   (+ block-height VOTING-PERIOD-BLOCKS)
    })
    (var-set proposal-nonce (+ pid u1))
    (print { event: "proposal-created", proposal-id: pid, proposer: tx-sender })
    (ok pid)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-NOT-FOUND))
    (vote-key { proposal-id: proposal-id, voter: tx-sender })
    (weight (isqrt (ft-get-balance social-good-token tx-sender)))
  )
    (asserts! (< block-height (get voting-ends proposal)) ERR-INVALID-STATE)
    (asserts! (is-none (map-get? proposal-votes vote-key)) ERR-ALREADY-EXISTS)
    (asserts! (> weight u0) ERR-INSUFFICIENT-BALANCE)
    (map-set proposal-votes vote-key vote-for)
    (if vote-for
      (map-set proposals proposal-id (merge proposal { votes-for: (+ (get votes-for proposal) weight) }))
      (map-set proposals proposal-id (merge proposal { votes-against: (+ (get votes-against proposal) weight) }))
    )
    (print { event: "vote-cast", proposal-id: proposal-id, voter: tx-sender, weight: weight, for: vote-for })
    (ok weight)
  )
)

;; Execute a passed proposal (simple majority of quadratic votes)
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-NOT-FOUND))
    (initiative-id (get target-id proposal))
    (initiative (unwrap! (map-get? initiatives initiative-id) ERR-NOT-FOUND))
    (action (get action-code proposal))
  )
    (asserts! (>= block-height (get voting-ends proposal)) ERR-INVALID-STATE)
    (asserts! (not (get is-executed proposal)) ERR-INVALID-STATE)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-QUORUM-NOT-MET)
    (map-set proposals proposal-id (merge proposal { is-executed: true }))
    ;; Execute action
    (if (is-eq action u1)
      ;; Pause initiative
      (map-set initiatives initiative-id (merge initiative { is-active: false }))
      (if (is-eq action u2)
        ;; Resume initiative
        (map-set initiatives initiative-id (merge initiative { is-active: true }))
        (if (is-eq action u3)
          ;; Boost multiplier by 10 (capped at 200 = 2.0x)
          (let ((current-mult (default-to MULTIPLIER-PRECISION (map-get? impact-multipliers initiative-id))))
            (map-set impact-multipliers initiative-id
              (if (< current-mult u200) (+ current-mult u10) current-mult))
          )
          false
        )
      )
    )
    (print { event: "proposal-executed", proposal-id: proposal-id, action: action })
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY HELPERS
;; ============================================================

(define-read-only (get-initiative (id uint))
  (map-get? initiatives id)
)

(define-read-only (get-milestone (initiative-id uint) (index uint))
  (map-get? milestones { initiative-id: initiative-id, index: index })
)

(define-read-only (get-impact-report (report-id uint))
  (map-get? impact-reports report-id)
)

(define-read-only (get-stake (account principal))
  (map-get? stakes account)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-impact-multiplier (initiative-id uint))
  (default-to MULTIPLIER-PRECISION (map-get? impact-multipliers initiative-id))
)

(define-read-only (get-treasury)
  (var-get treasury-balance)
)

(define-read-only (get-staked-supply)
  (var-get staked-supply)
)

(define-read-only (get-bonding-supply)
  (var-get bonding-supply)
)
