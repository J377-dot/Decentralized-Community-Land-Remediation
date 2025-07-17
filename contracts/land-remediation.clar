;; Land Remediation Core Contract
;; Handles project management, funding, and volunteer coordination

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-project-not-active (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-already-exists (err u106))
(define-constant err-invalid-status (err u107))
(define-constant err-deadline-passed (err u108))
(define-constant err-invalid-params (err u109))

;; Project status constants
(define-constant status-proposed u0)
(define-constant status-funded u1)
(define-constant status-active u2)
(define-constant status-completed u3)
(define-constant status-cancelled u4)

;; Data Variables
(define-data-var next-project-id uint u0)
(define-data-var next-volunteer-id uint u0)
(define-data-var regulatory-compliance-required bool true)
(define-data-var minimum-funding-threshold uint u1000000) ;; 1 STX minimum

;; Data Maps
(define-map projects
  uint
  {
    name: (string-ascii 100),
    description: (string-utf8 500),
    location: (string-ascii 200),
    site-area: uint, ;; in square meters
    contamination-type: (string-ascii 100),
    funding-goal: uint,
    funding-raised: uint,
    project-owner: principal,
    status: uint,
    created-at: uint,
    deadline: uint,
    estimated-duration: uint, ;; in blocks
    regulatory-permits: (list 10 (string-ascii 50)),
    compliance-verified: bool
  }
)

(define-map project-funders
  { project-id: uint, funder: principal }
  { amount: uint, funded-at: uint }
)

(define-map volunteers
  uint
  {
    volunteer-address: principal,
    name: (string-ascii 100),
    skills: (list 10 (string-ascii 50)),
    availability: uint, ;; hours per week
    registered-at: uint,
    reputation-score: uint,
    projects-completed: uint
  }
)

(define-map project-volunteers
  { project-id: uint, volunteer-id: uint }
  {
    assigned-at: uint,
    role: (string-ascii 50),
    hours-committed: uint,
    hours-completed: uint,
    verified: bool
  }
)

(define-map project-milestones
  { project-id: uint, milestone-id: uint }
  {
    title: (string-ascii 100),
    description: (string-utf8 300),
    target-completion: uint,
    actual-completion: (optional uint),
    funding-release: uint,
    completed: bool,
    verified-by: (optional principal)
  }
)

(define-map user-permissions
  principal
  {
    is-regulatory-authority: bool,
    is-environmental-specialist: bool,
    is-project-manager: bool,
    verified-at: uint,
    verification-level: uint
  }
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-project-funding (project-id uint) (funder principal))
  (map-get? project-funders { project-id: project-id, funder: funder })
)

(define-read-only (get-volunteer (volunteer-id uint))
  (map-get? volunteers volunteer-id)
)

(define-read-only (get-project-volunteer (project-id uint) (volunteer-id uint))
  (map-get? project-volunteers { project-id: project-id, volunteer-id: volunteer-id })
)

(define-read-only (get-project-milestone (project-id uint) (milestone-id uint))
  (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-user-permissions (user principal))
  (map-get? user-permissions user)
)

(define-read-only (get-next-project-id)
  (var-get next-project-id)
)

(define-read-only (get-next-volunteer-id)
  (var-get next-volunteer-id)
)

(define-read-only (is-project-active (project-id uint))
  (match (get-project project-id)
    project (is-eq (get status project) status-active)
    false
  )
)

(define-read-only (calculate-project-progress (project-id uint))
  (match (get-project project-id)
    project
    (let ((funding-progress (/ (* (get funding-raised project) u100) (get funding-goal project))))
      {
        funding-progress: funding-progress,
        days-remaining: (if (> (get deadline project) stacks-block-height)
                          (- (get deadline project) stacks-block-height)
                          u0),
        is-funded: (>= (get funding-raised project) (get funding-goal project)),
        compliance-status: (get compliance-verified project)
      }
    )
    { funding-progress: u0, days-remaining: u0, is-funded: false, compliance-status: false }
  )
)

;; Public functions
(define-public (create-project
  (name (string-ascii 100))
  (description (string-utf8 500))
  (location (string-ascii 200))
  (site-area uint)
  (contamination-type (string-ascii 100))
  (funding-goal uint)
  (duration-blocks uint)
  (regulatory-permits (list 10 (string-ascii 50)))
)
  (let ((project-id (var-get next-project-id)))
    (asserts! (>= funding-goal (var-get minimum-funding-threshold)) err-invalid-amount)
    (asserts! (> duration-blocks u0) err-invalid-params)
    (asserts! (> site-area u0) err-invalid-params)

    (map-set projects project-id {
      name: name,
      description: description,
      location: location,
      site-area: site-area,
      contamination-type: contamination-type,
      funding-goal: funding-goal,
      funding-raised: u0,
      project-owner: tx-sender,
      status: status-proposed,
      created-at: stacks-block-height,
      deadline: (+ stacks-block-height duration-blocks),
      estimated-duration: duration-blocks,
      regulatory-permits: regulatory-permits,
      compliance-verified: false
    })

    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (fund-project (project-id uint) (amount uint))
  (let ((project (unwrap! (get-project project-id) err-not-found)))
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-eq (get status project) status-proposed) err-project-not-active)
    (asserts! (>= (get deadline project) stacks-block-height) err-deadline-passed)

    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update project funding
    (let ((new-funding-raised (+ (get funding-raised project) amount)))
      (map-set projects project-id
        (merge project { funding-raised: new-funding-raised })
      )

      ;; Record funder contribution
      (map-set project-funders
        { project-id: project-id, funder: tx-sender }
        { amount: amount, funded-at: stacks-block-height }
      )

      ;; Auto-activate project if funding goal reached
      (if (>= new-funding-raised (get funding-goal project))
        (map-set projects project-id
          (merge project {
            funding-raised: new-funding-raised,
            status: status-funded
          })
        )
        true
      )
    )

    (ok true)
  )
)

(define-public (register-volunteer
  (name (string-ascii 100))
  (skills (list 10 (string-ascii 50)))
  (availability uint)
)
  (let ((volunteer-id (var-get next-volunteer-id)))
    (asserts! (> availability u0) err-invalid-params)

    (map-set volunteers volunteer-id {
      volunteer-address: tx-sender,
      name: name,
      skills: skills,
      availability: availability,
      registered-at: stacks-block-height,
      reputation-score: u100, ;; Starting reputation
      projects-completed: u0
    })

    (var-set next-volunteer-id (+ volunteer-id u1))
    (ok volunteer-id)
  )
)

(define-public (assign-volunteer-to-project
  (project-id uint)
  (volunteer-id uint)
  (role (string-ascii 50))
  (hours-committed uint)
)
  (let ((project (unwrap! (get-project project-id) err-not-found))
        (volunteer (unwrap! (get-volunteer volunteer-id) err-not-found)))

    (asserts! (is-eq tx-sender (get project-owner project)) err-unauthorized)
    (asserts! (or (is-eq (get status project) status-funded)
                  (is-eq (get status project) status-active)) err-project-not-active)
    (asserts! (> hours-committed u0) err-invalid-params)

    (map-set project-volunteers
      { project-id: project-id, volunteer-id: volunteer-id }
      {
        assigned-at: stacks-block-height,
        role: role,
        hours-committed: hours-committed,
        hours-completed: u0,
        verified: false
      }
    )

    (ok true)
  )
)

(define-public (activate-project (project-id uint))
  (let ((project (unwrap! (get-project project-id) err-not-found)))
    (asserts! (or (is-eq tx-sender (get project-owner project))
                  (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (is-eq (get status project) status-funded) err-invalid-status)
    (asserts! (get compliance-verified project) err-unauthorized)

    (map-set projects project-id
      (merge project { status: status-active })
    )

    (ok true)
  )
)

(define-public (complete-project (project-id uint))
  (let ((project (unwrap! (get-project project-id) err-not-found)))
    (asserts! (is-eq tx-sender (get project-owner project)) err-unauthorized)
    (asserts! (is-eq (get status project) status-active) err-invalid-status)

    (map-set projects project-id
      (merge project { status: status-completed })
    )

    ;; Update volunteer reputation scores
    ;; This would iterate through project volunteers and update their reputation

    (ok true)
  )
)

(define-public (verify-regulatory-compliance (project-id uint))
  (let ((project (unwrap! (get-project project-id) err-not-found))
        (permissions (unwrap! (get-user-permissions tx-sender) err-unauthorized)))

    (asserts! (get is-regulatory-authority permissions) err-unauthorized)

    (map-set projects project-id
      (merge project { compliance-verified: true })
    )

    (ok true)
  )
)

(define-public (create-milestone
  (project-id uint)
  (milestone-id uint)
  (title (string-ascii 100))
  (description (string-utf8 300))
  (target-completion uint)
  (funding-release uint)
)
  (let ((project (unwrap! (get-project project-id) err-not-found)))
    (asserts! (is-eq tx-sender (get project-owner project)) err-unauthorized)
    (asserts! (> target-completion stacks-block-height) err-invalid-params)
    (asserts! (<= funding-release (get funding-raised project)) err-invalid-amount)

    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      {
        title: title,
        description: description,
        target-completion: target-completion,
        actual-completion: none,
        funding-release: funding-release,
        completed: false,
        verified-by: none
      }
    )

    (ok true)
  )
)

(define-public (complete-milestone (project-id uint) (milestone-id uint))
  (let ((project (unwrap! (get-project project-id) err-not-found))
        (milestone (unwrap! (get-project-milestone project-id milestone-id) err-not-found)))

    (asserts! (is-eq tx-sender (get project-owner project)) err-unauthorized)
    (asserts! (not (get completed milestone)) err-already-exists)

    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone {
        completed: true,
        actual-completion: (some stacks-block-height),
        verified-by: (some tx-sender)
      })
    )

    ;; Release milestone funding
    (try! (as-contract (stx-transfer? (get funding-release milestone) tx-sender (get project-owner project))))

    (ok true)
  )
)

(define-public (set-user-permissions
  (user principal)
  (is-regulatory-authority bool)
  (is-environmental-specialist bool)
  (is-project-manager bool)
  (verification-level uint)
)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    (map-set user-permissions user {
      is-regulatory-authority: is-regulatory-authority,
      is-environmental-specialist: is-environmental-specialist,
      is-project-manager: is-project-manager,
      verified-at: stacks-block-height,
      verification-level: verification-level
    })

    (ok true)
  )
)

(define-public (update-volunteer-hours
  (project-id uint)
  (volunteer-id uint)
  (additional-hours uint)
)
  (let ((project (unwrap! (get-project project-id) err-not-found))
        (volunteer-assignment (unwrap! (get-project-volunteer project-id volunteer-id) err-not-found)))

    (asserts! (is-eq tx-sender (get project-owner project)) err-unauthorized)
    (asserts! (> additional-hours u0) err-invalid-params)

    (map-set project-volunteers
      { project-id: project-id, volunteer-id: volunteer-id }
      (merge volunteer-assignment {
        hours-completed: (+ (get hours-completed volunteer-assignment) additional-hours)
      })
    )

    (ok true)
  )
)

;; Emergency functions
(define-public (emergency-cancel-project (project-id uint))
  (let ((project (unwrap! (get-project project-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    (map-set projects project-id
      (merge project { status: status-cancelled })
    )

    (ok true)
  )
)

(define-public (update-minimum-funding-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-threshold u0) err-invalid-amount)

    (var-set minimum-funding-threshold new-threshold)
    (ok true)
  )
)
