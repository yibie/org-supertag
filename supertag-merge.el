;;; supertag-merge.el --- Pure 3-way semantic merge core for supertag-db.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; S3a of .phrase/phases/phase-git-sync-20260713/PLAN.md ("S3 语义 merge
;; driver"): the pure, side-effect-free merge core, plus a thin batch driver
;; entry point.  This file deliberately does NOT implement `supertag-git-setup',
;; `.gitattributes' handling, or `doctor' integration -- that is S3b.
;;
;; ## What this file assumes about the on-disk format (FROZEN, do not
;; reinterpret): the S2 canonical, deterministic, line-per-entity format
;; documented above `supertag--persistence-canonical-format-header' in
;; supertag-core-persistence.el, plus the legacy single-`prin1'-of-a-hash-table
;; format it transparently falls back to.  Both are read via
;; `supertag--persistence--try-read-store', which this file requires rather
;; than re-implementing a parser -- per the plan's explicit instruction to
;; reuse, not duplicate, the S2 reader/writer/equality primitives.
;;
;; ## Decision table (normative; copied here only for cross-reference -- the
;; source of truth is the plan's "S3 语义 merge driver" -> "合并算法" table):
;;
;;   base | ours | theirs | result
;;   -----|------|--------|-------
;;   A    | A    | A      | A                          (unchanged)
;;   A    | B    | A      | B                          (ours changed)
;;   A    | A    | C      | C                          (theirs changed)
;;   A    | B    | B      | B                          (both changed, same)
;;   A    | B    | C      | field-level 3-way on the entity plist
;;   A    | del  | A      | deleted
;;   A    | del  | C      | resurrect C + delete-vs-modify conflict
;;   none | B    | C      | field-level 3-way (no base fields)
;;
;; Root scalars (e.g. `:version') follow the exact same per-key rules, with
;; one addition: a genuine `:version' conflict is resolved by taking the
;; higher of the two versions (`supertag-merge--higher-version', via
;; `version<') rather than an arbitrary side, still with a conflict record.
;;
;; ## Equality
;;
;; "Equal" always means `equal' on values run through
;; `supertag--persistence--canonicalize-value' first (recursively sorts plist
;; keys and freezes nested hash tables into a comparable, re-readable form) --
;; this IS the S2-frozen definition of value equality, reused rather than
;; redefined, so a false conflict can never arise merely from plist key order
;; or hash-table iteration order differing between two independently-written
;; files.
;;
;; A deliberate, narrower design choice specific to this file: at the FIELD
;; level (inside one entity, or inside the root scalar plist), a key that is
;; simply ABSENT from a plist is treated as equivalent to that key being
;; PRESENT with value nil (both read back as nil via `plist-get').  This is
;; what makes "nil-vs-missing" a non-conflict, per the plan's field-type
;; equality matrix.  This collapse is intentionally *not* applied at the
;; entity/collection level: an entity's presence or absence in a collection's
;; id -> data map is a real fact (added/deleted), tracked with a distinct
;; internal sentinel, `supertag-merge--absent', that never leaks into merged
;; output (see `supertag-merge--printable').
;;
;; ## Conflict granularity
;;
;; NOT every entity value is a plist.  `:tag-field-associations' entity
;; values are ORDERED LISTS OF PLISTS (tag-id -> a list of `(:field-id ID
;; :order N)' association plists) -- decomposing that shape with plain
;; `plist-get'/`plist-put' (as an earlier version of this file did
;; unconditionally for every conflicting entity) silently produces
;; interleaved garbage, because a proper, even-length list of PLISTS looks
;; superficially plist-shaped by length alone; it is not, since its
;; elements are not keywords.  Every entity-level conflict therefore goes
;; through `supertag-merge--decompose-conflict', which dispatches on
;; COLLECTION identity and shape rather than assuming a plist:
;;
;;   1. `:tag-field-associations' (dispatched by COLLECTION, not shape):
;;      `supertag-merge--merge-tag-field-associations' merges as an ordered
;;      set keyed by the stable `:field-id' of each association -- a
;;      field-id touched by only one side's diff from base is included as
;;      that side left it; the same field-id changed differently on both
;;      sides is a whole-association (not sub-field) conflict for that
;;      field-id; if the surviving field-ids common to both ours and theirs
;;      are placed in a different relative order by each side, ours' order
;;      wins and one extra `:order' conflict is recorded.
;;   2. Positively plist-shaped entities (per `supertag--persistence--plist-p',
;;      REUSED rather than reinvented -- the exact same conservative
;;      discriminator the canonicalizer already uses to decide this exact
;;      question: a proper, non-empty, even-length list whose element at
;;      EVERY even index, not merely the first, is a keyword; checking only
;;      the first element is not sufficient -- the `:tag-field-associations'
;;      shape above has a plist, not a keyword, at index 0, and a naive
;;      first-element check would misclassify it): "field-level 3-way on
;;      the entity plist" as before -- when an entity's plist keys differ
;;      between ours and theirs, each KEY is decided independently via the
;;      same 3-way rule: two sides agreeing but differing from base -> that
;;      value; both sides disagreeing with each other -> a true, per-key
;;      conflict.  Keys are not recursively decomposed further even if a
;;      key's value happens to itself be a nested plist -- "same key, both
;;      sides changed it differently" already IS the true conflict at that
;;      key, per the plan.  A handful of collections (`:field-values', and
;;      the legacy `:fields') store each entity's data as a raw (frozen)
;;      hash table rather than a plist; those are naturally still covered
;;      by the exact same per-"key" logic once frozen (a frozen hash table
;;      prints as `(:supertag-hash-table ((K . V) ...))', which -- being
;;      itself a genuine 2-element plist with one keyword key -- the
;;      plist-shaped field walk sees as a single pseudo-key) -- such
;;      entities therefore get whole-value-granularity conflict detection
;;      rather than per-nested-field granularity when they conflict (see
;;      merge-test.el's dedicated regression test for this).
;;   3. Anything else -- NEVER guessed to be a plist -- gets whole-value
;;      3-way: a conflicting entity becomes one whole-entity conflict
;;      record (`:kind :field-conflict', `:key' nil), with ours written in
;;      place (the same ours-preference tiebreak used everywhere else in
;;      this file when `:modified-at' does not decide a winner).  This is
;;      the safety net that makes any future, not-yet-invented collection
;;      shape safe by construction instead of by omission.
;;
;; Zero data is ever lost in any of the three paths: both full sides always
;; survive intact in the conflict record, even when the entity/field/slot
;; written into the merged store prefers one side over the other.
;;
;; ## Conflict representation
;;
;; True conflicts are never written as git-style conflict markers inside the
;; data file (the loader would refuse to parse it).  Instead each becomes one
;; entity in a new collection, `:sync-conflicts', with a deterministic id
;; (`supertag-merge--conflict-id') of the shape:
;;   "COLLECTION/ENTITY-ID/KEY"   (a field-level conflict)
;;   "COLLECTION/ENTITY-ID"       (a delete-vs-modify conflict; no single key)
;;   "root/KEY"                   (a root-scalar conflict, e.g. "root/version")
;; Determinism of the id is what makes re-merging idempotent: the same three
;; inputs always produce the same conflict ids (and, since everything else in
;; this file is equally deterministic, byte-identical merged output).
;;
;; Each conflict entity's data is:
;;   (:id ID :collection C :entity-id ID-OR-NIL :key K-OR-NIL
;;    :ours V1 :theirs V2 :base V0 :kind :field-conflict|:delete-vs-modify
;;    :detected-at nil)
;; `:detected-at' is always nil in this file: a merge driver run must be a
;; pure function of its three inputs (see the idempotence test), and a
;; wall-clock timestamp would break that.  A live/interactive caller (S3b or
;; later) is free to backfill `:detected-at' non-destructively; this module
;; never does.
;;
;; The value written into the entity itself at a conflicting key (the
;; "winning" value users see by default) prefers whichever side's entity-level
;; `:modified-at' is strictly newer (`supertag-merge--newer-side', via
;; `time-less-p' on the standard 4-integer Emacs time list); when not
;; comparable (missing, equal, or malformed on either side) it falls back to
;; ours.  This is purely a display/default choice -- BOTH values always
;; remain fully present in the conflict record, so nothing is ever silently
;; lost.
;;
;; ## Require chain (kept minimal on purpose)
;;
;; This file requires only `supertag-core-persistence', whose own chain is:
;; `supertag-core-notify' -> `supertag-core-store' -> `supertag-core-index' ->
;; `supertag-core-transform' (plus `cl-lib'/`ht'/`json'/`parse-time', all
;; either built in or a normal package.el dependency).  Collectively this is
;; ~2800 lines of pure data-structure/persistence code.  It does NOT require
;; or load `org-supertag.el' itself, so no UI, no `org' integration, no
;; automation, and no view code is ever pulled in.
;;
;; Loading this chain has exactly one top-level (load-time) side effect
;; anywhere in it: `supertag-core-persistence.el' calls `(supertag-subscribe
;; :store-changed #'supertag-persistence--handle-store-changed)' at load time.
;; That handler only *fires* when something calls the mutating
;; `supertag-put'/`supertag-update' style API to change `supertag--store' --
;; this file never does (it reads plain hash tables with
;; `supertag--persistence--try-read-store' and builds its own local hash
;; tables directly), so in this process that subscription is registered but
;; never invoked.  No timer is started, no database is loaded, and no
;; `data-directory' file is touched, purely by requiring this chain.
;;
;; `ht' itself is expected to already be installed as a normal ELPA package
;; (as it is for every other org-supertag module); since `-Q' skips init
;; files but does NOT change `package-user-dir', this file calls
;; `(package-initialize)' itself, guarded, before requiring the supertag
;; chain below -- so the driver invocation does not also need `-L' to point
;; at ht's install directory, only at this repository's own directory.
;;
;; Git invocation (S3b wires this up automatically via `supertag-git-setup';
;; documented here so this file is independently testable/usable meanwhile):
;;
;;   emacs -Q --batch -L <org-supertag-dir> -l supertag-merge.el \
;;         -f supertag-merge-driver-main %O %A %B
;;
;; Per the git merge-driver protocol, %O/%A/%B are BASE/OURS/THEIRS; the
;; result must be written back over %A (OURS); exit 0 means "merged
;; successfully" (which, per this file's design, is true even when
;; conflicts were recorded -- those are data for the user to resolve later,
;; not driver failures); exit 1 means "could not merge", and git falls back
;; to its own default text merge.  On any parse or internal error this file
;; leaves OURS byte-for-byte untouched.
;;
;;; Code:

(require 'cl-lib)
(require 'pcase)

(when (require 'package nil t)
  (package-initialize))

(require 'supertag-core-persistence)

;;; --- Internal sentinel ---

(defvar supertag-merge--absent (make-symbol "supertag-merge-absent")
  "Internal sentinel meaning \"this (collection . id) does not exist here\".
Used only inside entity-level (not field-level) merge decisions.  MUST
NEVER be written to a merged store or a conflict record -- see
`supertag-merge--printable', which is the only place this symbol is ever
translated into something serializable.")

(defconst supertag-merge--absent-placeholder :supertag-merge/absent
  "Printable stand-in for `supertag-merge--absent' inside conflict records.
An uninterned symbol (what `supertag-merge--absent' is) cannot be
serialized deterministically, so the deleted side of a delete-vs-modify
conflict record holds this keyword instead.  Field-level conflicts never
use this placeholder even when there was no common-ancestor ENTITY at all
(e.g. the same id added independently on both sides): at field
granularity, a key with no base value simply reads back as nil via
`plist-get', the same nil-vs-missing collapse used for OURS/THEIRS field
values everywhere in this file (see the Commentary section \"Equality\").")

(define-error 'supertag-merge-parse-error
  "supertag-merge: failed to parse a store file")

;;; --- Parsed-store structure ---

(cl-defstruct (supertag-merge--parsed
                (:constructor supertag-merge--make-parsed)
                (:copier nil))
  "One parsed store snapshot (base, ours, or theirs -- or a merge result).
ROOT is a plist of root-level scalar keys (e.g. `:version') with
canonicalized values.  ENTITIES is an `equal'-test hash table mapping
`(COLLECTION . ID)' conses (COLLECTION a keyword, ID whatever type that
collection uses -- almost always a string) to that entity's canonicalized
data."
  root
  entities)

(defun supertag-merge--parse-file (path)
  "Parse the supertag-db file at PATH into a `supertag-merge--parsed' struct.
Understands both the S2 canonical (line-per-entity) format and the legacy
single-`prin1'-of-a-hash-table format, via
`supertag--persistence--try-read-store' -- this function does not
implement its own reader.

Every value (root scalars and entity data alike) is run through
`supertag--persistence--canonicalize-value' at parse time, so all
downstream equality comparisons and the final re-serialization only ever
see the one canonical shape.

Signals `supertag-merge-parse-error' (a distinguishable condition, never a
bare `error') if PATH cannot be read or does not parse into a store hash
table -- callers such as `supertag-merge-driver-main' rely on being able to
tell \"corrupt input\" apart from \"bug in this file\"."
  (unless (and (stringp path) (file-readable-p path))
    (signal 'supertag-merge-parse-error
            (list (format "not a readable file: %s" path))))
  (let (store)
    (condition-case err
        (setq store (supertag--persistence--try-read-store path))
      (error
       (signal 'supertag-merge-parse-error
               (list (format "failed to parse %s: %s"
                             path (error-message-string err))))))
    (unless (hash-table-p store)
      (signal 'supertag-merge-parse-error
              (list (format "%s: parsed content is not a store hash table"
                            path))))
    (let (root (entities (ht-create)))
      (maphash
       (lambda (k v)
         (if (hash-table-p v)
             (maphash (lambda (id data)
                        (puthash (cons k id)
                                 (supertag--persistence--canonicalize-value data)
                                 entities))
                      v)
           (setq root (plist-put root k
                                  (supertag--persistence--canonicalize-value v)))))
       store)
      (supertag-merge--make-parsed :root root :entities entities))))

(defun supertag-merge--to-store (parsed)
  "Build a plain store hash table from PARSED (a `supertag-merge--parsed').
Suitable for `supertag--persistence--write-canonical-store' -- this is the
inverse of the collection-splitting walk in `supertag-merge--parse-file'."
  (let ((store (ht-create)))
    (cl-loop for (k v) on (supertag-merge--parsed-root parsed) by #'cddr
             do (puthash k v store))
    (maphash
     (lambda (ck data)
       (let* ((collection (car ck))
              (id (cdr ck))
              (bucket (or (gethash collection store)
                          (let ((h (ht-create)))
                            (puthash collection h store)
                            h))))
         (puthash id data bucket)))
     (supertag-merge--parsed-entities parsed))
    store))

;;; --- Small helpers ---

(defun supertag-merge--name (x)
  "Return a bare string name for X: a keyword's name without its leading
colon, a symbol's name, or X itself if already a string."
  (cond
   ((keywordp x) (substring (symbol-name x) 1))
   ((symbolp x) (symbol-name x))
   ((stringp x) x)
   (t (format "%s" x))))

(defun supertag-merge--printable (v)
  "Return V, translating the internal absent sentinel to a printable form.
See `supertag-merge--absent-placeholder'."
  (if (eq v supertag-merge--absent) supertag-merge--absent-placeholder v))

(defun supertag-merge--val-equal (a b)
  "Return non-nil if A and B are equal for merge purposes.
The internal absent sentinel is equal only to itself; any other pair is
compared via `equal' after `supertag--persistence--canonicalize-value' --
this is the single, frozen definition of \"same value\" used everywhere in
this file (field values, whole entities, and root scalars alike)."
  (cond
   ((and (eq a supertag-merge--absent) (eq b supertag-merge--absent)) t)
   ((or (eq a supertag-merge--absent) (eq b supertag-merge--absent)) nil)
   (t (equal (supertag--persistence--canonicalize-value a)
             (supertag--persistence--canonicalize-value b)))))

(defun supertag-merge--3way-raw (base ours theirs)
  "Classify a single 3-way comparison of BASE/OURS/THEIRS.
Returns one of:
  (:unchanged . VALUE)  all three equal
  (:single . VALUE)     exactly one side changed; VALUE is that side
  (:same . VALUE)       both sides changed to an equal value
  :conflict             both sides changed, to different values
Any of the three may be `supertag-merge--absent'."
  (let ((base=ours (supertag-merge--val-equal base ours))
        (base=theirs (supertag-merge--val-equal base theirs))
        (ours=theirs (supertag-merge--val-equal ours theirs)))
    (cond
     ((and base=ours base=theirs) (cons :unchanged base))
     (base=ours (cons :single theirs))
     (base=theirs (cons :single ours))
     (ours=theirs (cons :same ours))
     (t :conflict))))

(defun supertag-merge--union-plist-keys (&rest plists)
  "Return the sorted union of keyword keys across all PLISTS.
Each element of PLISTS may be nil (treated as the empty plist)."
  (let ((seen (make-hash-table :test 'eq)) keys)
    (dolist (pl plists)
      (when (consp pl)
        (cl-loop for (k _v) on pl by #'cddr
                 do (unless (gethash k seen)
                      (puthash k t seen)
                      (push k keys)))))
    (sort keys (lambda (a b)
                 (string< (supertag--persistence--sort-key a)
                          (supertag--persistence--sort-key b))))))

(defun supertag-merge--conflict-id (collection entity-id key)
  "Return the deterministic `:sync-conflicts' id for one conflict.
See the Commentary section \"Conflict representation\" above for the three
shapes this can take."
  (cond
   ((and collection entity-id key)
    (format "%s/%s/%s" (supertag-merge--name collection) entity-id
            (supertag-merge--name key)))
   ((and collection entity-id (not key))
    (format "%s/%s" (supertag-merge--name collection) entity-id))
   ((and (not collection) (not entity-id) key)
    (format "root/%s" (supertag-merge--name key)))
   (t (error "supertag-merge: cannot build a conflict id from collection=%S entity-id=%S key=%S"
             collection entity-id key))))

(cl-defun supertag-merge--make-conflict (collection entity-id key &key ours theirs base kind)
  "Return an (ID . DATA) pair describing one true merge conflict.
Suitable for direct insertion into a `:sync-conflicts' collection map."
  (let ((id (supertag-merge--conflict-id collection entity-id key)))
    (cons id
          (list :id id
                :collection collection
                :entity-id entity-id
                :key key
                :ours (supertag-merge--printable ours)
                :theirs (supertag-merge--printable theirs)
                :base (supertag-merge--printable base)
                :kind kind
                :detected-at nil))))

(defun supertag-merge--higher-version (ours-version theirs-version)
  "Return whichever of OURS-VERSION/THEIRS-VERSION (version strings) is higher.
Uses `version<'.  Falls back to OURS-VERSION (the documented
ours-preference tiebreak used throughout this file) when the two are not
both parseable version strings, or are not strictly ordered."
  (if (and (stringp ours-version) (stringp theirs-version)
           (ignore-errors (version-to-list ours-version))
           (ignore-errors (version-to-list theirs-version))
           (version< ours-version theirs-version))
      theirs-version
    ours-version))

(defun supertag-merge--newer-side (ours-entity theirs-entity)
  "Return `:ours' or `:theirs': whichever entity has the newer `:modified-at'.
Falls back to `:ours' when the two `:modified-at' values are missing,
equal, or not both valid Emacs time lists -- the documented ours-preference
tiebreak for the value written into a conflicting key."
  (let ((ot (and (listp ours-entity) (plist-get ours-entity :modified-at)))
        (tt (and (listp theirs-entity) (plist-get theirs-entity :modified-at))))
    (if (and (supertag--validate-time ot)
             (supertag--validate-time tt)
             (not (supertag-time-equal ot tt))
             (time-less-p ot tt))
        :theirs
      :ours)))

;;; --- Shape discriminator ---

(defun supertag-merge--plist-like-p (v)
  "Return non-nil if V is positively identified as plist-shaped.
Used only to decide whether an entity-level (or root-scalar) conflict may
be decomposed field-by-field with `plist-get'/`plist-put'.  `nil' (the
empty plist) and `supertag-merge--absent' (no common-ancestor value at
all) both count as plist-compatible -- neither contributes any keys to
decompose.  Anything else is delegated to
`supertag--persistence--plist-p', the one frozen, conservative
discriminator already used by the canonicalizer to decide this exact
question (a proper, non-empty, even-length list whose element at EVERY
even index -- not merely the first -- is a keyword), reused rather than
reinvented so the two call sites can never drift apart.  This deliberately
returns nil (never guesses \"plist\") for anything that does not positively
match, including an ordered list of nested plists such as
`:tag-field-associations' values -- see the Commentary section \"Conflict
granularity\"."
  (or (eq v supertag-merge--absent)
      (null v)
      (supertag--persistence--plist-p v)))

;;; --- :tag-field-associations ordered-set merge ---

(defun supertag-merge--assoc-field-id (assoc)
  "Return ASSOC's `:field-id', or nil if ASSOC is not a well-formed
tag-field-association plist (defensive only; real entities are always
well-formed by the time they reach this file)."
  (and (supertag-merge--plist-like-p assoc) (consp assoc)
       (plist-get assoc :field-id)))

(defun supertag-merge--assoc-field-ids (list)
  "Return the ordered, deduplicated list of `:field-id's found in LIST.
LIST is one side's whole `:tag-field-associations' entity value (an
ordered list of association plists).  Malformed elements (no `:field-id')
are silently skipped; first occurrence wins the position on a duplicate
`:field-id'."
  (let ((seen (make-hash-table :test 'equal)) order)
    (dolist (a list)
      (let ((fid (supertag-merge--assoc-field-id a)))
        (when (and fid (not (gethash fid seen)))
          (puthash fid t seen)
          (push fid order))))
    (nreverse order)))

(defun supertag-merge--assoc-index (list)
  "Return an `equal'-test hash table mapping `:field-id' -> association
plist for LIST.  Last occurrence wins on a duplicate `:field-id'."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (a list)
      (let ((fid (supertag-merge--assoc-field-id a)))
        (when fid (puthash fid a h))))
    h))

(defun supertag-merge--assoc-restrict (order set)
  "Return the subsequence of ORDER whose elements are `member' of SET.
Preserves ORDER's own relative element order."
  (cl-remove-if (lambda (f) (not (member f set))) order))

(defun supertag-merge--merge-assoc-slot (collection entity-id field-id base ours theirs)
  "Merge one `:field-id' slot inside a `:tag-field-associations' entity.
BASE/OURS/THEIRS are each that FIELD-ID's whole association plist, or
`supertag-merge--absent'.  Whole-association-value granularity only (never
decomposed further into `:order' etc. -- the plan asks for a per-field-id
conflict, not a per-sub-field one).  Self-contained (does not call back
into `supertag-merge--merge-entity'/`supertag-merge--decompose-conflict'):
COLLECTION is still `:tag-field-associations' at this nesting level, and
recursing back through the collection-based dispatch would incorrectly
try to re-apply the ordered-set merge to a single association plist.
Returns (VALUE . CONFLICTS); VALUE may be `supertag-merge--absent' (the
field-id is dropped from the merged association list)."
  (let ((absent supertag-merge--absent))
    (cond
     ((and (not (eq ours absent)) (not (eq theirs absent)))
      (let ((decision (supertag-merge--3way-raw base ours theirs)))
        (if (eq decision :conflict)
            (cons ours
                  (list (supertag-merge--make-conflict
                         collection entity-id field-id
                         :ours ours :theirs theirs :base base
                         :kind :field-conflict)))
          (cons (cdr decision) nil))))
     ((and (eq ours absent) (eq theirs absent)) (cons absent nil))
     ((eq ours absent)
      (cond
       ((eq base absent) (cons theirs nil))
       ((supertag-merge--val-equal base theirs) (cons absent nil))
       (t (cons theirs
                (list (supertag-merge--make-conflict
                       collection entity-id field-id
                       :ours supertag-merge--absent :theirs theirs :base base
                       :kind :delete-vs-modify))))))
     (t
      (cond
       ((eq base absent) (cons ours nil))
       ((supertag-merge--val-equal base ours) (cons absent nil))
       (t (cons ours
                (list (supertag-merge--make-conflict
                       collection entity-id field-id
                       :ours ours :theirs supertag-merge--absent :base base
                       :kind :delete-vs-modify)))))))))

(defun supertag-merge--merge-tag-field-associations (collection entity-id base ours theirs)
  "Ordered-set 3-way merge for one `:tag-field-associations' entity value.
BASE/OURS/THEIRS are each an ordered list of association plists (BASE may
be `supertag-merge--absent').  See the Commentary section \"Conflict
granularity\" for the algorithm.  Returns (MERGED-LIST . CONFLICTS)."
  (let* ((base-list (if (eq base supertag-merge--absent) nil base))
         (base-order (supertag-merge--assoc-field-ids base-list))
         (base-idx (supertag-merge--assoc-index base-list))
         (ours-order (supertag-merge--assoc-field-ids ours))
         (ours-idx (supertag-merge--assoc-index ours))
         (theirs-order (supertag-merge--assoc-field-ids theirs))
         (theirs-idx (supertag-merge--assoc-index theirs))
         (fids (let ((seen (make-hash-table :test 'equal)) all)
                 (dolist (fid (append base-order ours-order theirs-order))
                   (unless (gethash fid seen)
                     (puthash fid t seen)
                     (push fid all)))
                 (nreverse all)))
         merged-alist conflicts)
    (dolist (fid fids)
      (let* ((bv (or (gethash fid base-idx) supertag-merge--absent))
             (ov (or (gethash fid ours-idx) supertag-merge--absent))
             (tv (or (gethash fid theirs-idx) supertag-merge--absent))
             (result (supertag-merge--merge-assoc-slot collection entity-id fid bv ov tv)))
        (setq conflicts (nconc conflicts (copy-sequence (cdr result))))
        (unless (eq (car result) supertag-merge--absent)
          (push (cons fid (car result)) merged-alist))))
    (setq merged-alist (nreverse merged-alist))
    (let* ((survivors (mapcar #'car merged-alist))
           (common (cl-intersection ours-order theirs-order :test #'equal))
           (base-common (supertag-merge--assoc-restrict base-order common))
           (ours-common (supertag-merge--assoc-restrict ours-order common))
           (theirs-common (supertag-merge--assoc-restrict theirs-order common))
           (order-decision (supertag-merge--3way-raw base-common ours-common theirs-common))
           (leftover (cl-set-difference survivors ours-order :test #'equal))
           (final-order (append (supertag-merge--assoc-restrict ours-order survivors)
                                 (supertag-merge--assoc-restrict theirs-order leftover))))
      (when (eq order-decision :conflict)
        (setq conflicts
              (nconc conflicts
                     (list (supertag-merge--make-conflict
                            collection entity-id :order
                            :ours ours-common :theirs theirs-common :base base-common
                            :kind :field-conflict)))))
      (cons (mapcar (lambda (f) (cdr (assoc f merged-alist))) final-order)
            conflicts))))

;;; --- Field-level (and root-scalar) merge ---

(defun supertag-merge--field-level-merge (collection entity-id base ours theirs newer-side)
  "Per-key 3-way merge of one entity's plist (or the root scalar plist).
BASE may be `supertag-merge--absent' (no common-ancestor entity at all);
OURS/THEIRS are real plists that differ overall.  NEWER-SIDE is `:ours' or
`:theirs', from `supertag-merge--newer-side', used to pick the value
written into the merged entity at each conflicting key -- both sides
always additionally survive intact in the conflict record.
Returns (MERGED-PLIST . CONFLICTS)."
  (let* ((base-plist (if (eq base supertag-merge--absent) nil base))
         (keys (supertag-merge--union-plist-keys base-plist ours theirs))
         merged conflicts)
    (dolist (k keys)
      (let* ((bv (plist-get base-plist k))
             (ov (plist-get ours k))
             (tv (plist-get theirs k))
             (decision (supertag-merge--3way-raw bv ov tv)))
        (pcase decision
          (`(:unchanged . ,v) (setq merged (plist-put merged k v)))
          (`(:single . ,v) (setq merged (plist-put merged k v)))
          (`(:same . ,v) (setq merged (plist-put merged k v)))
          (:conflict
           (push (supertag-merge--make-conflict collection entity-id k
                                                 :ours ov :theirs tv :base bv
                                                 :kind :field-conflict)
                 conflicts)
           (setq merged (plist-put merged k (if (eq newer-side :theirs) tv ov)))))))
    (cons merged (nreverse conflicts))))

;;; --- Entity-level merge ---

(defun supertag-merge--decompose-conflict (collection entity-id base ours theirs)
  "Decide HOW to decompose an entity-level conflict for COLLECTION/ENTITY-ID.
BASE/OURS/THEIRS genuinely differ from each other (the caller has already
ruled out unchanged/single-side-change/both-changed-to-the-same-value).
Dispatches:
  1. By COLLECTION identity first: `:tag-field-associations' values are
     ORDERED LISTS OF PLISTS, not plists themselves, so they get their own
     ordered-set-by-`:field-id' merge rather than ever being decomposed
     with `plist-get'/`plist-put'.
  2. Otherwise, only when BASE/OURS/THEIRS are ALL positively plist-shaped
     (`supertag-merge--plist-like-p', never guessed): per-key field-level
     3-way, as before.
  3. Otherwise (anything not positively identified as one of the above --
     the safety net): whole-value 3-way.  One whole-entity conflict record
     is emitted (`:kind :field-conflict', `:key' nil), and the value
     written into the merged store is whichever side has the newer
     `:modified-at' (falling back to ours, the same tiebreak used
     everywhere else in this file) -- both sides always still survive
     intact in the conflict record.
See the Commentary section \"Conflict granularity\" for the full rationale."
  (cond
   ((eq collection :tag-field-associations)
    (supertag-merge--merge-tag-field-associations collection entity-id base ours theirs))
   ((and (supertag-merge--plist-like-p base)
         (supertag-merge--plist-like-p ours)
         (supertag-merge--plist-like-p theirs))
    (supertag-merge--field-level-merge
     collection entity-id base ours theirs
     (supertag-merge--newer-side ours theirs)))
   (t
    (cons (if (eq (supertag-merge--newer-side ours theirs) :theirs) theirs ours)
          (list (supertag-merge--make-conflict
                 collection entity-id nil
                 :ours ours :theirs theirs :base base
                 :kind :field-conflict))))))

(defun supertag-merge--merge-present (collection entity-id base ours theirs)
  "Merge one (collection . entity-id) slot when OURS and THEIRS both exist.
BASE may be `supertag-merge--absent'.  Returns (VALUE . CONFLICTS)."
  (let ((decision (supertag-merge--3way-raw base ours theirs)))
    (pcase decision
      (`(:unchanged . ,v) (cons v nil))
      (`(:single . ,v) (cons v nil))
      (`(:same . ,v) (cons v nil))
      (:conflict
       (supertag-merge--decompose-conflict collection entity-id base ours theirs)))))

(defun supertag-merge--merge-entity (collection entity-id base ours theirs)
  "Merge one (COLLECTION . ENTITY-ID) slot per the plan's decision table.
BASE/OURS/THEIRS are each either that side's entity data, or
`supertag-merge--absent'.  Returns (VALUE . CONFLICTS); VALUE may itself be
`supertag-merge--absent', meaning the entity does not exist in the merged
result."
  (let ((absent supertag-merge--absent))
    (cond
     ;; Both sides have the entity: normal / field-level path (base may or
     ;; may not exist).
     ((and (not (eq ours absent)) (not (eq theirs absent)))
      (supertag-merge--merge-present collection entity-id base ours theirs))
     ;; Neither side has it any more: mutual delete (base must have existed
     ;; for this id to appear in the merge's key union at all).
     ((and (eq ours absent) (eq theirs absent))
      (cons absent nil))
     ;; Ours is gone, theirs has it.
     ((eq ours absent)
      (cond
       ((eq base absent) (cons theirs nil)) ; theirs added it; nothing to delete
       ((supertag-merge--val-equal base theirs) (cons absent nil)) ; ours deleted, theirs unchanged
       (t (cons theirs
                (list (supertag-merge--make-conflict
                       collection entity-id nil
                       :ours supertag-merge--absent :theirs theirs :base base
                       :kind :delete-vs-modify))))))
     ;; Theirs is gone, ours has it (mirror of the above).
     (t
      (cond
       ((eq base absent) (cons ours nil))
       ((supertag-merge--val-equal base ours) (cons absent nil))
       (t (cons ours
                (list (supertag-merge--make-conflict
                       collection entity-id nil
                       :ours ours :theirs supertag-merge--absent :base base
                       :kind :delete-vs-modify)))))))))

(defun supertag-merge--entity-get (parsed collection entity-id)
  "Return (COLLECTION . ENTITY-ID)'s data in PARSED, or `supertag-merge--absent'."
  (gethash (cons collection entity-id)
           (supertag-merge--parsed-entities parsed)
           supertag-merge--absent))

(defun supertag-merge--union-entity-keys (base ours theirs)
  "Return the sorted union of (COLLECTION . ID) keys across the three parses."
  (let ((seen (make-hash-table :test 'equal)) keys)
    (dolist (p (list base ours theirs))
      (maphash (lambda (k _v)
                 (unless (gethash k seen)
                   (puthash k t seen)
                   (push k keys)))
               (supertag-merge--parsed-entities p)))
    (sort keys (lambda (a b)
                 (let ((ca (supertag-merge--name (car a)))
                       (cb (supertag-merge--name (car b))))
                   (if (string= ca cb)
                       (string< (supertag--persistence--sort-key (cdr a))
                                (supertag--persistence--sort-key (cdr b)))
                     (string< ca cb)))))))

;;; --- Root scalar merge ---

(defun supertag-merge--merge-roots (base-root ours-root theirs-root)
  "Merge the three root scalar plists.  Returns (MERGED-PLIST . CONFLICTS).

Audited for the same never-guess-plist rule applied to entity values (see
`supertag-merge--decompose-conflict'): BASE-ROOT/OURS-ROOT/THEIRS-ROOT
themselves are unconditionally real plists here, by construction --
`supertag-merge--parse-file' is the only producer of a `-root' value, and
it builds one exclusively via `plist-put' over root-level scalar keys
only (collection values, e.g. `:tag-field-associations', live in
`entities', never in `root') -- so decomposing the ROOT STRUCTURE itself
with `plist-get'/`plist-put' is safe unconditionally, unlike an entity
value.  Each individual root KEY's VALUE (bv/ov/tv below), by contrast, is
never itself decomposed as a plist: a conflicting value is always taken
whole -- either `supertag-merge--higher-version' for `:version', or the
ours-preference tiebreak for anything else -- so there is no plist-shape
assumption at the value level to guess wrong, even if some future root
scalar's value happened to be a list-of-plists shape."
  (let* ((keys (supertag-merge--union-plist-keys base-root ours-root theirs-root))
         merged conflicts)
    (dolist (k keys)
      (let* ((bv (plist-get base-root k))
             (ov (plist-get ours-root k))
             (tv (plist-get theirs-root k))
             (decision (supertag-merge--3way-raw bv ov tv)))
        (pcase decision
          (`(:unchanged . ,v) (setq merged (plist-put merged k v)))
          (`(:single . ,v) (setq merged (plist-put merged k v)))
          (`(:same . ,v) (setq merged (plist-put merged k v)))
          (:conflict
           (if (eq k :version)
               (let ((winner (supertag-merge--higher-version ov tv)))
                 (push (supertag-merge--make-conflict nil nil :version
                                                       :ours ov :theirs tv :base bv
                                                       :kind :field-conflict)
                       conflicts)
                 (setq merged (plist-put merged k winner)))
             ;; Generic root scalar conflict: the plan only specifies special
             ;; handling for `:version'; anything else falls back to the same
             ;; ours-preference tiebreak used at field level (there is no
             ;; entity-level `:modified-at' at root scope to compare).
             (progn
               (push (supertag-merge--make-conflict nil nil k
                                                     :ours ov :theirs tv :base bv
                                                     :kind :field-conflict)
                     conflicts)
               (setq merged (plist-put merged k ov))))))))
    (cons merged (nreverse conflicts))))

;;; --- Top-level 3-way merge ---

(defun supertag-merge-3way (base ours theirs)
  "3-way merge BASE/OURS/THEIRS (each a `supertag-merge--parsed' struct).
Returns (MERGED . CONFLICTS): MERGED is a `supertag-merge--parsed' struct
(pass it to `supertag-merge--write-merged' or `supertag-merge--to-store');
CONFLICTS is the flat list of (ID . DATA) conflict-entity pairs, which are
also folded into MERGED's `:sync-conflicts' collection."
  (let* ((root-result (supertag-merge--merge-roots
                        (supertag-merge--parsed-root base)
                        (supertag-merge--parsed-root ours)
                        (supertag-merge--parsed-root theirs)))
         (merged-root (car root-result))
         (root-conflicts (cdr root-result))
         (keys (supertag-merge--union-entity-keys base ours theirs))
         (merged-entities (ht-create))
         (entity-conflicts nil))
    (dolist (ck keys)
      (let* ((collection (car ck))
             (entity-id (cdr ck))
             (b (supertag-merge--entity-get base collection entity-id))
             (o (supertag-merge--entity-get ours collection entity-id))
             (th (supertag-merge--entity-get theirs collection entity-id))
             (result (supertag-merge--merge-entity collection entity-id b o th))
             (value (car result)))
        (setq entity-conflicts (nconc entity-conflicts (copy-sequence (cdr result))))
        (unless (eq value supertag-merge--absent)
          (puthash ck value merged-entities))))
    (let ((all-conflicts (append root-conflicts entity-conflicts)))
      (dolist (c all-conflicts)
        (puthash (cons :sync-conflicts (car c)) (cdr c) merged-entities))
      (cons (supertag-merge--make-parsed :root merged-root :entities merged-entities)
            all-conflicts))))

;;; --- Serialization ---

(defun supertag-merge--write-merged (merged path)
  "Write MERGED (a `supertag-merge--parsed') to PATH in S2 canonical format.
Reuses `supertag--persistence--write-canonical-store' -- the S2 canonical
writer -- rather than reimplementing serialization.  Writes via a
same-directory temp file plus `rename-file', so a crash or error mid-write
never leaves PATH partially written."
  (let* ((store (supertag-merge--to-store merged))
         (temp-file (make-temp-file (concat path ".merge-tmp")))
         (success nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-file-coding-system 'utf-8-unix)
            (supertag--persistence--write-canonical-store store (current-buffer))
            (let ((write-region-inhibit-fsync nil))
              (write-region (point-min) (point-max) temp-file nil 'silent)))
          (when (file-exists-p path)
            (ignore-errors (set-file-modes temp-file (file-modes path))))
          (rename-file temp-file path t)
          (setq success t))
      (unless success (ignore-errors (delete-file temp-file))))))

;;; --- Driver entry point ---

(defun supertag-merge-driver-main ()
  "Git merge-driver entry point.

Reads BASE OURS THEIRS from `command-line-args-left' (git substitutes
these for %O %A %B; see the Commentary section above for the exact `git
config merge.*.driver' invocation), 3-way merges them, and writes the
result back over OURS -- which is where git's merge-driver protocol
expects the answer.

Exits 0 on success, including when conflicts were recorded (those are
`:sync-conflicts' data for the user, not a driver failure).  Exits 1, with
a human-readable message on stderr and OURS left completely untouched, on
any parse or internal error."
  (let* ((args command-line-args-left)
         (base-path (nth 0 args))
         (ours-path (nth 1 args))
         (theirs-path (nth 2 args)))
    (condition-case err
        (let* ((base (supertag-merge--parse-file base-path))
               (ours (supertag-merge--parse-file ours-path))
               (theirs (supertag-merge--parse-file theirs-path))
               (result (supertag-merge-3way base ours theirs))
               (merged (car result)))
          (supertag-merge--write-merged merged ours-path)
          (kill-emacs 0))
      (error
       (message "supertag-merge: merge failed: %s" (error-message-string err))
       (kill-emacs 1)))))

(provide 'supertag-merge)

;;; supertag-merge.el ends here
