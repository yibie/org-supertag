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
;;      field-id.  Concurrent ordering constraints are combined when possible;
;;      incompatible orders keep ours as the complete fallback and record one
;;      `:order' conflict containing all three original orders.
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
;;      key's value happens to itself be a nested PLIST -- "same key, both
;;      sides changed it differently" already IS the true conflict at that
;;      key, per the plan.  A handful of collections (`:field-values', and
;;      the legacy `:fields') store each entity's data as a raw (frozen)
;;      hash table rather than a plist; those are naturally still covered
;;      by the exact same per-"key" logic once frozen (a frozen hash table
;;      prints as `(:supertag-hash-table ((K . V) ...))', which -- being
;;      itself a genuine 2-element plist with one keyword key -- the
;;      plist-shaped field walk sees as a single pseudo-key).
;;
;;      UNLIKE a nested PLIST value (never decomposed further, above), the
;;      pseudo-key's VALUE here is not a plist at all -- it is the frozen
;;      hash table's own `(KEY . VALUE)' ALIST, i.e. exactly the map this
;;      collection actually stores.  A conflict at this pseudo-key
;;      therefore recurses one level further, map-style, keyed by each
;;      ALIST entry's own KEY (`supertag-merge--merge-hash-marker-map'):
;;      a key touched by only one side is adopted without conflict -- this
;;      is what makes the flagship dogfood scenario (machine A sets field
;;      "priority", machine B concurrently sets field "deadline" on the
;;      SAME node/tag) merge cleanly instead of colliding as one
;;      whole-entity conflict; the same key changed differently on both
;;      sides recurses AGAIN, exactly the same way, when BOTH sides'
;;      values for it are themselves hash-marker forms -- this is what
;;      makes the legacy `:fields' collection's node -> tag -> field
;;      nesting merge field-by-field even when two machines touch
;;      different fields under the SAME tag; otherwise (a genuine,
;;      non-map leaf disagreement) it is the true, per-key-path conflict,
;;      the recursion's own safety net, one level at a time, all the way
;;      down.  See `supertag-merge--merge-hash-marker-map' and
;;      `supertag-merge--merge-hash-map-entry' below, and merge-test.el's
;;      dedicated tests for this, including the regression test pinning
;;      the now-narrower case where a hash-shaped entity's conflict is
;;      STILL whole-leaf-value granularity (because the disagreeing leaf
;;      itself is not a nested map on both sides).
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
;;   "tag-field-associations/ENTITY-ID/field|meta/KEY"
;;                                  (association slot vs list metadata)
;;   "COLLECTION/ENTITY-ID"       (a delete-vs-modify conflict; no single key)
;;   "root/KEY"                   (a root-scalar conflict, e.g. "root/version")
;;   "COLLECTION/ENTITY-ID/SEG1/SEG2/.../SEGN"
;;                                  (a conflict at a leaf inside a frozen
;;                                  hash-marker map, e.g.
;;                                  "fields/NODE-ID/TAG-ID/FIELD-NAME" --
;;                                  see `supertag-merge--merge-hash-marker-map'
;;                                  and `supertag-merge--make-path-conflict')
;; Determinism of the id is what makes re-merging idempotent: the same three
;; inputs always produce the same conflict ids (and, since everything else in
;; this file is equally deterministic, byte-identical merged output).
;;
;; Each conflict entity's data is:
;;   (:id ID :collection C :entity-id ID-OR-NIL :key K-OR-NIL
;;    :ours V1 :theirs V2 :base V0 :kind :field-conflict|:delete-vs-modify
;;    :detected-at nil)
;; A nested hash-marker-map conflict (the last id shape above) additionally
;; carries `:key-path PATH', a list of the ALIST keys from the entity down to
;; the conflicting leaf (e.g. `("TAG-ID" "FIELD-NAME")'); `:key' itself is
;; still set to PATH's own last (leaf) element, so the record's `:collection'
;; /`:entity-id'/`:key' triple alone already reads exactly like an ordinary,
;; non-nested field-conflict record on this collection -- `:key-path' is
;; purely additive, for anything that wants the full nesting instead of just
;; the leaf.  IMPORTANT, READ BEFORE WIRING UP RESOLUTION: this file's sibling
;; supertag-conflicts.el pre-dates this key-path shape entirely and was
;; audited (not modified -- see that file, which this one deliberately does
;; not `require') against it: its dispatch would send such a record down the
;; ordinary per-key `supertag-conflicts--apply' path (COLLECTION non-nil,
;; KEY non-nil and not the hash marker), which assumes the live entity is a
;; PLIST and calls `plist-put' on it -- but `:fields'/`:field-values'
;; entities are genuinely raw hash tables live, not plists, so resolving one
;; of these new leaf conflicts interactively today fails loudly (a
;; `wrong-type-argument' from `plist-put', caught and reported per-conflict
;; by the bulk resolvers, or signaled back to the interactive caller) rather
;; than corrupting the entity or silently doing nothing.  `:drop' (discard
;; the stale conflict record without touching the target) still works fine.
;; Teaching supertag-conflicts.el to apply `:key-path' against the real
;; nested hash tables is intentionally left as followup work, not done here.
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
See the Commentary section \"Conflict representation\" above for the
shapes this can take."
  (cond
   ((and (eq collection :tag-field-associations) entity-id key)
    (format "%s/%s/%s/%s" (supertag-merge--name collection) entity-id
            (if (keywordp key) "meta" "field")
            (supertag-merge--name key)))
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
  "Return `:field-id' from a prevalidated association plist ASSOC."
  (plist-get assoc :field-id))

(defun supertag-merge--assoc-list-p (value)
  "Return non-nil when VALUE is a safe association list to decompose.
Every element must be a plist with a unique, non-empty string `:field-id'.
The absent sentinel is valid for a missing BASE value."
  (or (eq value supertag-merge--absent)
      (let ((seen (make-hash-table :test 'equal)))
        (and (proper-list-p value)
             (cl-every
              (lambda (assoc)
                (and (supertag--persistence--plist-p assoc)
                     (let ((field-id (plist-get assoc :field-id)))
                       (and (stringp field-id)
                            (not (string= field-id ""))
                            (not (gethash field-id seen))
                            (progn (puthash field-id t seen) t)))))
              value)))))

(defun supertag-merge--assoc-field-ids (list)
  "Return ordered `:field-id's from prevalidated association LIST."
  (mapcar #'supertag-merge--assoc-field-id list))

(defun supertag-merge--assoc-index (list)
  "Return an `equal'-test hash table mapping `:field-id' -> association
plist for prevalidated LIST."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (a list)
      (let ((fid (supertag-merge--assoc-field-id a)))
        (when fid (puthash fid a h))))
    h))

(defun supertag-merge--assoc-restrict (order set)
  "Return the subsequence of ORDER whose elements are `member' of SET.
Preserves ORDER's own relative element order."
  (cl-remove-if (lambda (f) (not (member f set))) order))

(defun supertag-merge--merge-assoc-order (survivors base-order ours-order theirs-order)
  "Return (ORDER . CONFLICT-P) for the surviving association ids.
Single-side order changes win normally.  Concurrent changes are combined as
ordering constraints; a cycle falls back to a complete ours-first order and
sets CONFLICT-P."
  (let* ((base (supertag-merge--assoc-restrict base-order survivors))
         (ours (supertag-merge--assoc-restrict ours-order survivors))
         (theirs (supertag-merge--assoc-restrict theirs-order survivors))
         (decision (supertag-merge--3way-raw base ours theirs))
         (chosen (and (not (eq decision :conflict)) (cdr decision))))
    (if (and chosen (= (length chosen) (length survivors)))
        (cons chosen nil)
      (let ((edges (make-hash-table :test 'equal))
            (indegree (make-hash-table :test 'equal))
            (preference (delete-dups (append ours theirs base (copy-sequence survivors))))
            (remaining (copy-sequence survivors))
            result)
        (dolist (fid survivors)
          (puthash fid nil edges)
          (puthash fid 0 indegree))
        (dolist (side (list ours theirs))
          (cl-loop for from in side
                   for to in (cdr side)
                   do (unless (member to (gethash from edges))
                        (push to (gethash from edges))
                        (puthash to (1+ (gethash to indegree)) indegree))))
        ;; ponytail: O(n²) is deliberate; tag schemas are short.  Use a
        ;; priority queue only if real per-tag association lists grow large.
        (while (and remaining
                    (let ((next
                           (cl-find-if
                            (lambda (fid)
                              (and (member fid remaining)
                                   (zerop (gethash fid indegree))))
                            preference)))
                      (when next
                        (setq remaining (delete next remaining))
                        (push next result)
                        (dolist (to (gethash next edges))
                          (puthash to (1- (gethash to indegree)) indegree))
                        t))))
        (if remaining
            (cons preference t)
          (cons (nreverse result) nil))))))

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
           (order-result (supertag-merge--merge-assoc-order
                          survivors base-order ours-order theirs-order))
           (final-order (car order-result)))
      (when (cdr order-result)
        (setq conflicts
              (nconc conflicts
                     (list (supertag-merge--make-conflict
                            collection entity-id :order
                            :ours ours-order :theirs theirs-order :base base-order
                            :kind :field-conflict)))))
      (cons (mapcar (lambda (f) (cdr (assoc f merged-alist))) final-order)
            conflicts))))

;;; --- Frozen hash-table map recursive merge ---
;;
;; See the top-of-file Commentary section "Conflict granularity", point 2's
;; discussion of the `:supertag-hash-table' pseudo-key, for the rationale.
;; This section implements the recursion that lets a conflict AT that
;; pseudo-key (i.e. a genuine 3-way disagreement between the frozen ALISTs
;; of a `:field-values'/legacy-`:fields' entity) merge map-key-by-map-key
;; instead of unconditionally falling back to one whole-entity conflict --
;; the dogfood-repro fix.  Structurally this mirrors the
;; `:tag-field-associations' ordered-set merge above
;; (`supertag-merge--merge-assoc-slot' / `-merge-tag-field-associations'):
;; same three-way absent-sentinel cases per map key, same
;; resurrect-on-delete-vs-modify semantics -- the one addition is that a
;; genuine both-changed-differently disagreement on one key recurses BACK
;; into this same map merge, one level deeper, whenever both sides' values
;; for that key are themselves frozen hash-marker forms (this is what lets
;; the legacy `:fields' node -> tag -> field nesting merge field-by-field);
;; otherwise it is the true, per-key-path leaf conflict.

(defun supertag-merge--hash-marker-value-p (v)
  "Return non-nil if V is a frozen hash-table marker value.
That is, a proper 2-element list `(:supertag-hash-table ALIST)' as produced
by `supertag--persistence--freeze-hash-table' -- see
`supertag--persistence--thaw-value', which recognizes the exact same shape."
  (and (consp v)
       (eq (car v) supertag--persistence--hash-marker)
       (consp (cdr v))
       (null (cddr v))))

(defun supertag-merge--hash-marker-alist (v)
  "Return the `(KEY . VALUE)' ALIST inside hash-marker value V.
V must satisfy `supertag-merge--hash-marker-value-p'."
  (cadr v))

(defun supertag-merge--hash-map-keys (&rest alists)
  "Return the sorted union of keys across ALISTS.
Each element of ALISTS is an alist of `(KEY . VALUE)' conses, or nil."
  (let ((seen (make-hash-table :test 'equal)) keys)
    (dolist (al alists)
      (dolist (pair al)
        (unless (gethash (car pair) seen)
          (puthash (car pair) t seen)
          (push (car pair) keys))))
    (sort keys (lambda (a b) (string< (supertag--persistence--sort-key a)
                                       (supertag--persistence--sort-key b))))))

(defun supertag-merge--path-conflict-id (collection entity-id path)
  "Return the deterministic conflict id for nested hash-map PATH.
PATH is a non-empty list of ALIST-key segments beneath ENTITY-ID inside a
frozen hash-table entity (see `supertag-merge--merge-hash-marker-map').
Produces \"COLLECTION/ENTITY-ID/SEG1/SEG2/.../SEGN\", e.g.
\"fields/NODE-ID/TAG-ID/FIELD-NAME\"."
  (format "%s/%s/%s" (supertag-merge--name collection) entity-id
          (mapconcat #'supertag-merge--name path "/")))

(cl-defun supertag-merge--make-path-conflict (collection entity-id path &key ours theirs base kind)
  "Return an (ID . DATA) pair for one nested hash-map conflict at PATH.
Shaped as closely as possible to an ordinary field-conflict record (see
`supertag-merge--make-conflict'), per this file's Commentary section
\"Conflict representation\": `:key' is PATH's own final (leaf) segment, so
the record's `:collection'/`:entity-id'/`:key' triple alone already reads
exactly like a non-nested field conflict on this collection; the full PATH
is additionally available under `:key-path' for anything that wants more
than the leaf.  See that Commentary section for the supertag-conflicts.el
compatibility assessment of this shape."
  (let ((id (supertag-merge--path-conflict-id collection entity-id path)))
    (cons id
          (list :id id
                :collection collection
                :entity-id entity-id
                :key (car (last path))
                :key-path (copy-sequence path)
                :ours (supertag-merge--printable ours)
                :theirs (supertag-merge--printable theirs)
                :base (supertag-merge--printable base)
                :kind kind
                :detected-at nil))))

(defun supertag-merge--merge-hash-map-entry (collection entity-id path newer-side key base ours theirs)
  "Merge one KEY of a frozen hash-table map found at PATH (the parent
segments, NOT including KEY itself).  BASE/OURS/THEIRS are that key's raw
values, or `supertag-merge--absent'.  NEWER-SIDE is `:ours' or `:theirs',
threaded down unchanged from the entity-level call (there is no per-nested
-key `:modified-at' to recompute it from -- the same tiebreak decided once
for the whole entity applies to every leaf conflict inside it, exactly like
ordinary field-level conflicts).  Returns (VALUE . CONFLICTS); VALUE may be
`supertag-merge--absent' (the key is dropped from the merged map)."
  (let* ((absent supertag-merge--absent)
         (full-path (append path (list key))))
    (cond
     ((and (not (eq ours absent)) (not (eq theirs absent)))
      (let ((decision (supertag-merge--3way-raw base ours theirs)))
        (pcase decision
          (`(:unchanged . ,v) (cons v nil))
          (`(:single . ,v) (cons v nil))
          (`(:same . ,v) (cons v nil))
          (:conflict
           (if (and (supertag-merge--hash-marker-value-p ours)
                    (supertag-merge--hash-marker-value-p theirs))
               (let ((sub (supertag-merge--merge-hash-marker-map
                           collection entity-id full-path newer-side
                           (and (supertag-merge--hash-marker-value-p base)
                                (supertag-merge--hash-marker-alist base))
                           (supertag-merge--hash-marker-alist ours)
                           (supertag-merge--hash-marker-alist theirs))))
                 ;; Re-wrap the recursed-into ALIST back into marker form --
                 ;; this key's merged VALUE must still look like a frozen
                 ;; hash table (matching what OURS/THEIRS held at this key),
                 ;; not a bare alist, so a further level up (or the final
                 ;; entity re-wrap in `supertag-merge--field-level-merge')
                 ;; sees the shape it expects.
                 (cons (list supertag--persistence--hash-marker (car sub)) (cdr sub)))
             (cons (if (eq newer-side :theirs) theirs ours)
                   (list (supertag-merge--make-path-conflict
                          collection entity-id full-path
                          :ours ours :theirs theirs :base base
                          :kind :field-conflict))))))))
     ((and (eq ours absent) (eq theirs absent)) (cons absent nil))
     ((eq ours absent)
      (cond
       ((eq base absent) (cons theirs nil))
       ((supertag-merge--val-equal base theirs) (cons absent nil))
       (t (cons theirs
                (list (supertag-merge--make-path-conflict
                       collection entity-id full-path
                       :ours supertag-merge--absent :theirs theirs :base base
                       :kind :delete-vs-modify))))))
     (t
      (cond
       ((eq base absent) (cons ours nil))
       ((supertag-merge--val-equal base ours) (cons absent nil))
       (t (cons ours
                (list (supertag-merge--make-path-conflict
                       collection entity-id full-path
                       :ours ours :theirs supertag-merge--absent :base base
                       :kind :delete-vs-modify)))))))))

(defun supertag-merge--merge-hash-marker-map (collection entity-id path newer-side base-alist ours-alist theirs-alist)
  "Recursively 3-way merge one nesting level of a frozen hash-table map.
BASE-ALIST/OURS-ALIST/THEIRS-ALIST are each an alist of `(KEY . VALUE)'
conses (BASE-ALIST may be nil -- \"no common-ancestor map at this level\",
the same `none | B | C' row used elsewhere in this file).  PATH is the
list of ALIST-key segments already traversed above this level (empty at
the entity's own top level).  Returns (MERGED-ALIST . CONFLICTS);
MERGED-ALIST is sorted by `supertag--persistence--sort-key' on each key, so
re-serializing it never needs to re-sort (see the on-disk format's
sorted-alist invariant for the `:supertag-hash-table' marker)."
  (let ((keys (supertag-merge--hash-map-keys base-alist ours-alist theirs-alist))
        merged-alist conflicts)
    (dolist (k keys)
      (let* ((bc (assoc k base-alist))
             (oc (assoc k ours-alist))
             (tc (assoc k theirs-alist))
             (bv (if bc (cdr bc) supertag-merge--absent))
             (ov (if oc (cdr oc) supertag-merge--absent))
             (tv (if tc (cdr tc) supertag-merge--absent))
             (result (supertag-merge--merge-hash-map-entry
                      collection entity-id path newer-side k bv ov tv)))
        (setq conflicts (nconc conflicts (copy-sequence (cdr result))))
        (unless (eq (car result) supertag-merge--absent)
          (push (cons k (car result)) merged-alist))))
    (setq merged-alist
          (sort merged-alist (lambda (a b)
                                (string< (supertag--persistence--sort-key (car a))
                                         (supertag--persistence--sort-key (car b))))))
    (cons merged-alist conflicts)))

;;; --- Field-level (and root-scalar) merge ---

(defun supertag-merge--field-level-merge (collection entity-id base ours theirs newer-side)
  "Per-key 3-way merge of one entity's plist (or the root scalar plist).
BASE may be `supertag-merge--absent' (no common-ancestor entity at all);
OURS/THEIRS are real plists that differ overall.  NEWER-SIDE is `:ours' or
`:theirs', from `supertag-merge--newer-side', used to pick the value
written into the merged entity at each conflicting key -- both sides
always additionally survive intact in the conflict record.

Special-cased for the single `:supertag-hash-table' pseudo-key produced
when the whole entity is a frozen hash-table map (`:field-values', legacy
`:fields'; see this file's Commentary \"Conflict granularity\"): a
conflict AT that key does not become one whole-entity conflict record the
way an ordinary plist key's conflict would -- OV/TV at that key are
already the frozen map's own ALIST (not re-wrapped), so it recurses,
map-key-by-map-key, via `supertag-merge--merge-hash-marker-map', and the
merged ALIST is re-wrapped back into marker form for the entity's merged
value.  Any other key's conflict (including a non-map value inside a
hash-marker map recursing down to a genuine leaf) still becomes a plain
field-conflict record, unchanged from before.

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
           (if (and (eq k supertag--persistence--hash-marker) (listp ov) (listp tv))
               (let* ((base-alist (and (listp bv) bv))
                      (result (supertag-merge--merge-hash-marker-map
                               collection entity-id nil newer-side base-alist ov tv)))
                 (setq conflicts (nconc conflicts (copy-sequence (cdr result))))
                 ;; NOTE: unlike a recursed-into NESTED key (see
                 ;; `supertag-merge--merge-hash-map-entry', which re-wraps
                 ;; its own recursion result), K here already IS the marker
                 ;; keyword itself -- `(plist-put merged k (car result))'
                 ;; below produces `(:supertag-hash-table MERGED-ALIST)'
                 ;; directly, matching the entity's own frozen shape.
                 ;; Wrapping `(car result)' in ANOTHER marker layer here
                 ;; would double-wrap the entity (caught by this file's
                 ;; regression test).
                 (setq merged (plist-put merged k (car result))))
             (progn
               (setq conflicts
                     (nconc conflicts
                            (list (supertag-merge--make-conflict collection entity-id k
                                                                  :ours ov :theirs tv :base bv
                                                                  :kind :field-conflict))))
               (setq merged (plist-put merged k (if (eq newer-side :theirs) tv ov)))))))))
    (cons merged conflicts)))

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
   ((and (eq collection :tag-field-associations)
         (supertag-merge--assoc-list-p base)
         (supertag-merge--assoc-list-p ours)
         (supertag-merge--assoc-list-p theirs))
    (supertag-merge--merge-tag-field-associations collection entity-id base ours theirs))
   ((and (not (eq collection :tag-field-associations))
         (supertag-merge--plist-like-p base)
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
